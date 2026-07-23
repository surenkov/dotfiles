# doom/my/scripts/edit.awk --- AWK script for gptel edit tool

function trim(s) {
    sub(/^[ \t\r]+/, "", s)
    sub(/[ \t\r]+$/, "", s)
    return s
}

function max(a, b) { return a > b ? a : b }

function is_disproportionate(search_str, old_str) {
    n_old = split(old_str, tmp1, "\n")
    if (trim(tmp1[n_old]) == "") n_old--
    n_search = split(search_str, tmp2, "\n")
    if (trim(tmp2[n_search]) == "") n_search--
    if (n_search >= max(n_old + 3, n_old * 2)) return 1
    if (n_old == 1) return 0
    return length(trim(search_str)) > max(length(trim(old_str)) + 500, length(trim(old_str)) * 4)
}

BEGIN {
    RS = "^$"
    old_str = ENVIRON["OLD_STRING"]
    new_str = ENVIRON["NEW_STRING"]
    replace_all = (ENVIRON["REPLACE_ALL"] == "1")
    file_path = ENVIRON["FILE_PATH"]

    if (old_str == new_str) {
        print "Error: No changes to apply: `old' and `new' strings are identical." > "/dev/stderr"
        exit 1
    }

    if (length(old_str) == 0) {
        cmd_test = "test -f '" file_path "'"
        if (system(cmd_test) == 0) {
            print "Error: `old' string cannot be empty when editing an existing file. Provide the exact text to replace, or use write for an intentional full-file replacement." > "/dev/stderr"
            exit 1
        }
        dir_name = file_path
        sub(/\/[^\/]+$/, "", dir_name)
        if (dir_name != "" && dir_name != file_path) {
            system("mkdir -p '" dir_name "'")
        }
        printf "%s", new_str > file_path
        close(file_path)
        print "Edited file successfully"
        exit 0
    }
}

{
    content = $0
}

END {
    file_path = FILENAME
    has_crlf = (index(content, "\r\n") > 0)
    if (has_crlf) {
        gsub(/\r\n/, "\n", old_str)
        gsub(/\r\n/, "\n", new_str)
        gsub(/\r\n/, "\n", content)
    }

    # Strategy 1: SimpleReplacer (Exact Match)
    pos = index(content, old_str)
    search_match = old_str

    # Strategy 2: LineTrimmedReplacer
    if (pos == 0) {
        n_content = split(content, c_lines, "\n")
        n_old = split(old_str, o_lines, "\n")
        if (n_old > 1 && trim(o_lines[n_old]) == "") n_old--

        matched_start = 0
        matched_end = 0

        # Exact line-by-line trimmed match
        for (i = 1; i <= n_content - n_old + 1; i++) {
            match_found = 1
            for (j = 1; j <= n_old; j++) {
                if (trim(c_lines[i + j - 1]) != trim(o_lines[j])) {
                    match_found = 0
                    break
                }
            }
            if (match_found) {
                matched_start = i
                matched_end = i + n_old - 1
                break
            }
        }

        # Strategy 3: ContextAnchor / Disproportionate anchor match (first & last line anchors)
        if (matched_start == 0 && n_old >= 2) {
            first_line = trim(o_lines[1])
            last_line = trim(o_lines[n_old])
            for (i = 1; i <= n_content; i++) {
                if (trim(c_lines[i]) == first_line) {
                    for (j = i + 1; j <= n_content; j++) {
                        if (trim(c_lines[j]) == last_line) {
                            matched_start = i
                            matched_end = j
                            break
                        }
                    }
                    if (matched_start > 0) break
                }
            }
        }

        if (matched_start > 0) {
            matched_pos = 1
            for (k = 1; k < matched_start; k++) matched_pos += length(c_lines[k]) + 1
            matched_len = 0
            for (k = matched_start; k <= matched_end; k++) matched_len += length(c_lines[k]) + (k < n_content ? 1 : 0)
            search_match = substr(content, matched_pos, matched_len)
            pos = index(content, search_match)
        }
    }

    if (pos == 0) {
        print "Error: Could not find `old' string in the file. It must match exactly, including whitespace and indentation." > "/dev/stderr"
        exit 1
    }

    if (is_disproportionate(search_match, old_str)) {
        print "Error: Refusing replacement because the matched span is much larger than `old' string. Re-read the file and provide the full exact `old' string for the intended replacement." > "/dev/stderr"
        exit 1
    }

    if (!replace_all) {
        second_pos = index(substr(content, pos + length(search_match)), search_match)
        if (second_pos > 0) {
            print "Error: Found multiple matches for `old' string. Provide more surrounding context or set replace-all to true." > "/dev/stderr"
            exit 1
        }
    }

    if (replace_all) {
        out = ""
        while ((pos = index(content, search_match)) > 0) {
            out = out substr(content, 1, pos - 1) new_str
            content = substr(content, pos + length(search_match))
        }
        out = out content
    } else {
        out = substr(content, 1, pos - 1) new_str substr(content, pos + length(search_match))
    }

    if (has_crlf) {
        gsub(/\n/, "\r\n", out)
    }

    printf "%s", out > file_path
    close(file_path)
    print "Edited file successfully"
}

#!/usr/bin/env bash
# doom/my/scripts/edit_pipeline.sh --- Multi-tier edit pipeline (AWK + rg + fzf)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET_FILE="$FILE_PATH"
OLD_STR="$OLD_STRING"
NEW_STR="$NEW_STRING"
REPLACE_ALL="${REPLACE_ALL:-0}"

# Export environment variables for sub-shells and AWK
export OLD_STRING="$OLD_STR"
export NEW_STRING="$NEW_STR"
export REPLACE_ALL="$REPLACE_ALL"
export FILE_PATH="$TARGET_FILE"

# Tier 1 & Tier 2: AWK Fast Path (Exact Match & Line-Trimmed Match)
RESULT=$(awk -f "$SCRIPT_DIR/edit.awk" "$TARGET_FILE" 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo "$RESULT"
    exit 0
fi

# Tier 3: Ripgrep (rg) Anchor Locating for multi-line blocks (OLD_LINES >= 2) with middle-line typos
OLD_LINES=$(echo "$OLD_STR" | wc -l | tr -d ' ')
FIRST_LINE=$(echo "$OLD_STR" | head -n 1 | sed 's/^[ \t]*//; s/[ \t]*$//')
LAST_LINE=$(echo "$OLD_STR" | tail -n 1 | sed 's/^[ \t]*//; s/[ \t]*$//')

if [ "$OLD_LINES" -ge 2 ] && [ -n "$FIRST_LINE" ] && [ -n "$LAST_LINE" ]; then
    START_LINES=$(rg -n --fixed-strings "$FIRST_LINE" "$TARGET_FILE" 2>/dev/null | cut -d: -f1)
    END_LINES=$(rg -n --fixed-strings "$LAST_LINE" "$TARGET_FILE" 2>/dev/null | cut -d: -f1)

    MATCH_START=""
    MATCH_END=""
    CANDIDATES=()

    for s in $START_LINES; do
        for e in $END_LINES; do
            if [ "$e" -gt "$s" ]; then
                SPAN=$((e - s + 1))
                MIN_SPAN=$((OLD_LINES / 2))
                MAX_SPAN=$((OLD_LINES * 2 + 3))
                if [ $SPAN -ge $MIN_SPAN ] && [ $SPAN -le $MAX_SPAN ]; then
                    CANDIDATES+=("$s:$e")
                fi
            fi
        done
    done

    # If exactly 1 candidate span found
    if [ ${#CANDIDATES[@]} -eq 1 ]; then
        MATCH_START=$(echo "${CANDIDATES[0]}" | cut -d: -f1)
        MATCH_END=$(echo "${CANDIDATES[0]}" | cut -d: -f2)
    elif [ ${#CANDIDATES[@]} -gt 1 ] && command -v fzf >/dev/null 2>&1; then
        # Tier 4: fzf --filter block ranking if multiple anchor candidates exist
        BEST_CAND=""
        BEST_SCORE=-1

        for cand in "${CANDIDATES[@]}"; do
            cs=$(echo "$cand" | cut -d: -f1)
            ce=$(echo "$cand" | cut -d: -f2)
            span_text=$(sed -n "${cs},${ce}p" "$TARGET_FILE" | tr '\n' ' ')
            score=$(echo "$span_text" | fzf -f "$OLD_STR" 2>/dev/null | wc -c)
            if [ "$score" -gt "$BEST_SCORE" ]; then
                BEST_SCORE="$score"
                BEST_CAND="$cand"
            fi
        done

        if [ -n "$BEST_CAND" ]; then
            MATCH_START=$(echo "$BEST_CAND" | cut -d: -f1)
            MATCH_END=$(echo "$BEST_CAND" | cut -d: -f2)
        fi
    fi

    if [ -n "$MATCH_START" ] && [ -n "$MATCH_END" ]; then
        MATCHED_TEXT=$(sed -n "${MATCH_START},${MATCH_END}p" "$TARGET_FILE")
        export OLD_STRING="$MATCHED_TEXT"
        awk -f "$SCRIPT_DIR/edit.awk" "$TARGET_FILE"
        exit $?
    fi
fi

# If all tiers fail, return initial AWK error output
echo "$RESULT"
exit 1

set fish_greeting

set -gx PATH       $HOME/.bin /usr/local/bin $PATH
set -gx PYENV_ROOT $HOME/.pyenv
set -gx REBAR_ROOT $HOME/.cache/rebar3
set -gx GOPATH     $HOME/go
set -gx NVM_DIR    $HOME/.nvm
set -gx PATH       $GOPATH/bin:$PYENV_ROOT/bin:$REBAR_ROOT/bin:$PATH

set -gx LC_ALL      en_US.UTF-8
set -gx LANG        en_US.UTF-8
set -gx LANGUAGE    en_US.UTF-8

set -gx EDITOR nvim

alias emc='emacsclient -t'
alias emd='emacs -nw --daemon'
alias emstop="emacsclient -e '(kill-emacs)'"
alias vim=nvim

eval "$(pyenv init --path)"
# alias htop="TERM=xterm htop"
alias ipy=ipython3

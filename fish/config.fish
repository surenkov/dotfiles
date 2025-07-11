set fish_greeting

set BREW_HOME /opt/homebrew
set -gxp PATH $HOME/.local/bin $HOME/.bin $BREW_HOME/bin $BREW_HOME/opt/llvm/bin /usr/local/bin $HOME/.config/emacs/bin $HOME/.cargo/bin

set -gx PYENV_ROOT $HOME/.pyenv
set -gx NVM_DIR $HOME/.nvm

set -gxp PATH $PYENV_ROOT/bin

set -gx LC_ALL en_US.UTF-8
set -gx LANG en_US.UTF-8
set -gx LANGUAGE en_US.UTF-8

set -gx EDITOR nvim
set -gx VISUAL nvim

set -gx LSP_USE_PLISTS true
set -gx GDAL_LIBRARY_PATH "$(gdal-config --prefix)/lib/libgdal.dylib"
set -gx GEOS_LIBRARY_PATH "$(geos-config --prefix)/lib/libgeos_c.dylib"

set -gx RIPGREP_CONFIG_PATH $HOME/.ripgreprc

pyenv init - | source
direnv hook fish | source

alias emc='emacsclient -t'
alias emd='emacs -nw --daemon'
alias emstop="emacsclient -e '(kill-emacs)'"
alias vim=nvim
alias ipy=ipython3
alias k=kubectl

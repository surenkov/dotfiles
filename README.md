# dotfiles
Useful configs (vim, zsh) of mine

### Emacs
1. Install [Doom Emacs](https://github.com/doomemacs/doomemacs)
1. Symlink `.doom.d` to your home path
2. Run `doom sync`
1. Profit!

### VIM
Actually, Neovim. Not sure if it will work with VIM, at least few plugins require VIM 8+.

Installation steps:
1. symlink `.vimrc` to `~/.config/nvim/init.vim`;
2. install [vim-plug](https://github.com/junegunn/vim-plug)
3. run `nvim +PlugInstall +qa`
4. ...
5. PROFIT!

### zsh [+ oh-my-zsh](https://github.com/ohmyzsh/ohmyzsh)
Useful plugins:
* [Spaceship Theme]( https://github.com/denysdovhan/spaceship-prompt )
* [zsh-autosuggestions ](https://github.com/zsh-users/zsh-autosuggestions)
* [zsh-completions](https://github.com/zsh-users/zsh-completions)
  * [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting)
  
  
### fish [+ oh-my-fish](https://github.com/oh-my-fish/oh-my-fish)
* `omf install pyenv nvm fzf`

### Aacritty
symlink `alacritty.yml` to `~/.config/alacritty/alacritty.yml`;

### Kitty
symlink `kitty.conf` to `~/.config/kitty/kitty.conf`;

### Tmux
1. symlink `.tmux.conf` to `~/.tmux.conf`;
2. hit `C-z I` in active tmux session to install the plugins


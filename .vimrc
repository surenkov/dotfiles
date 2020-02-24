set hidden
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

set encoding=UTF-8
set undofile
set undodir=~/.config/nvim/undodir
set listchars=eol:$,nbsp:_,tab:>-,trail:~,extends:>,precedes:<
set list
set colorcolumn=89
set foldlevel=99
set cursorline

set completeopt+=menu
set completeopt+=noinsert
set completeopt-=noselect
set completeopt-=longest
set completeopt+=preview

let g:airline_powerline_fonts = 1
let g:indentLine_concealcursor = 'inc'
let g:indentLine_conceallevel = 2
let g:indentLine_char = '┊'

let g:deoplete#enable_at_startup = 1
let g:jedi#auto_vim_configuration = 0
let g:jedi#goto_assignments_command = ''
let g:jedi#goto_definitions_command = ''
let g:jedi#use_tabs_not_buffers = 0
let g:jedi#rename_command = '<Leader>gR'
let g:jedi#usages_command = '<Leader>gu'
let g:jedi#completions_enabled = 0
let g:jedi#smart_auto_mappings = 1

  " Unite/ref and pydoc are more useful.
  let g:jedi#documentation_command = '<Leader>_K'
  let g:jedi#auto_close_doc = 1

let g:tablineclosebutton=1
hi TabLine      ctermfg=Black  ctermbg=Green     cterm=NONE
hi TabLineFill  ctermfg=Black  ctermbg=Green     cterm=NONE
hi TabLineSel   ctermfg=White  ctermbg=DarkBlue  cterm=NONE

if has('clipboard')
  if has('unnamedplus')  " When possible use + register for copy-paste
    set clipboard=unnamed,unnamedplus
  else         " On mac and Windows, use * register for copy-paste
    set clipboard=unnamed
  endif
endif

set ignorecase
set conceallevel=1
set background=dark

set expandtab
set autoindent
set softtabstop=4
set shiftwidth=4
set tabstop=4

set history=1000
set mouse+=a

" Visual shifting (does not exit Visual mode)
vnoremap < <gv
vnoremap > >gv

" Allow using the repeat operator with a visual selection (!)
" http://stackoverflow.com/a/8064607/127816
vnoremap . :normal .<CR>

set number relativenumber
set hlsearch

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>


call plug#begin('~/.vim/plugged')

" Themes
Plug 'Raimondi/delimitMate'
Plug 'cocopon/iceberg.vim'
Plug 'vim-airline/vim-airline' "Status bar
Plug 'vim-airline/vim-airline-themes' "Applicable themes

" Language Syntax Support
Plug 'sheerun/vim-polyglot'
" Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'} "Better python highlighting
Plug 'fatih/vim-go', { 'tag': '*', 'do': ':GoInstallBinaries' }
Plug 'mdempsky/gocode', { 'rtp': 'nvim', 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh' }
Plug 'psf/black' "Any color you like
Plug 'tmhedberg/SimpylFold'
Plug 'Yggdroot/indentLine'
Plug 'editorconfig/editorconfig-vim'

"Tools
Plug 'mitermayer/vim-prettier'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'

" Deoplete compleion sources
Plug 'wokalski/autocomplete-flow'
Plug 'deoplete-plugins/deoplete-jedi'


Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive' "Git tools
Plug 'tpope/vim-surround'
Plug 'justinmk/vim-sneak'

Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeTabsToggle' }
Plug 'jistr/vim-nerdtree-tabs', { 'on': 'NERDTreeTabsToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'

Plug 'mhinz/vim-startify'
Plug 'mkitt/tabline.vim'

" All of your Plugins must be added before the following line
call plug#end()            " required
" filetype plugin indent on    " automatically run by Plug 
" syntax enable " automatically run by Plug 

" Theme settings 
colors iceberg
set termguicolors

nmap <C-n> :NERDTreeTabsToggle<CR>
nmap <leader>n :NERDTreeFind<CR>
nmap <leader>h :GitGutterPreviewHunk<CR>
nmap <leader>hp :GitGutterPrevHunk<CR>
nmap <leader>hn :GitGutterNextHunk<CR>

" FZF bindings
nmap <Space> :Rg<CR>
nmap <C-P> :FZF<CR>

" Sneak F
map f <Plug>Sneak_s
map F <Plug>Sneak_S

autocmd Filetype json,txt,startify :IndentLinesDisable

set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes

call deoplete#custom#option('omni_patterns', {
\ 'go': '[^. *\t]\.\w*',
\})

"use <tab> for completion
function! TabWrap()
    if pumvisible()
        return "\<C-N>"
    elseif strpart( getline('.'), 0, col('.') - 1 ) =~ '^\s*$'
        return "\<TAB>"
    elseif &omnifunc !~ ''
        return "\<C-X>\<C-O>"
    else
        return "\<C-N>"
    endif
endfunction

" power tab
imap <silent><expr><tab> TabWrap()

" Enter: complete&close popup if visible (so next Enter works); else: break undo
inoremap <silent><expr> <Cr> pumvisible() ?
            \ deoplete#close_popup() : "<C-g>u<Cr>"

" Ctrl-Space: summon FULL (synced) autocompletion
inoremap <silent><expr> <C-Space> deoplete#manual_complete()

" Escape: exit autocompletion, go to Normal mode
inoremap <silent><expr> <Esc> pumvisible() ? "<C-e><Esc>" : "<Esc>"

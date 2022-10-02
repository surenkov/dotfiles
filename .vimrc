set hidden
set nobackup
set nowritebackup
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

set encoding=UTF-8
set undofile
set undodir=~/.config/nvim/undodir
set listchars=eol:$,nbsp:_,tab:>-,trail:~,extends:>,precedes:<
set list
set colorcolumn=89
set foldlevel=99
set cursorline
set nowrap

set completeopt+=menu
set completeopt+=noinsert
set completeopt-=noselect
set completeopt-=longest
set completeopt+=preview

set ignorecase
set smartcase
set conceallevel=1
set background=dark

set expandtab
set autoindent
set softtabstop=4
set shiftwidth=4
set tabstop=4

set history=1000
set mouse=a

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
Plug 'chriskempson/base16-vim'
Plug 'rakr/vim-one'
Plug 'vim-airline/vim-airline' "Status bar
Plug 'vim-airline/vim-airline-themes' "Applicable themes
Plug 'arcticicestudio/nord-vim'

" Language Syntax Support
Plug 'sheerun/vim-polyglot'
" Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'} "Better python highlighting
Plug 'psf/black', { 'tag': '19.10b0' }
Plug 'tmhedberg/SimpylFold'
Plug 'Yggdroot/indentLine'
Plug 'editorconfig/editorconfig-vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

"Tools
Plug 'mitermayer/vim-prettier'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'liuchengxu/vista.vim'

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
set termguicolors
colorscheme one

nnoremap <silent> <C-n> :NERDTreeTabsToggle<CR>
nnoremap <silent> <leader>n :NERDTreeFind<CR>
nnoremap <silent> <leader>c :GitGutterPreviewHunk<CR>
nnoremap <silent> <leader>v :Vista!!<CR>

" FZF bindings
nmap <nowait> <space><space> :Rg<CR>
nmap <nowait> <space>p :FZF<CR>

" Sneak F
map f <Plug>Sneak_s
map F <Plug>Sneak_S

au Filetype json,txt,startify :IndentLinesDisable
au Filetype python nmap <buffer><nowait> == :Black<CR>
au Filetype python vnoremap <buffer><nowait> = =

set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes

let g:airline_powerline_fonts = 1
let g:indentLine_concealcursor = 'inc'
let g:indentLine_conceallevel = 2
let g:indentLine_char = '┊'

let g:vista_fzf_preview = ['right:50%']
" let g:vista_icon_indent = ["╰─ ", "├─ "]
let g:vista_default_executive = 'coc'
let g:vista#executives = ['coc', 'ctags']
let g:vista#finders = ['fzf']
let g:vista#renderer#enable_icon = 0

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

"use <tab> for completion
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
if has('patch8.1.1068')
  " Use `complete_info` if your (Neo)Vim version supports it.
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif


" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')
autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

" Symbol renaming.
" highlight
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current line.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline+=%{coc#status()}%{NearestMethodOrFunction()}

" Mappings using CoCList:
" Show all diagnostics.
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>

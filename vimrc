execute pathogen#infect()
syntax on
filetype off
filetype plugin indent on

set nocompatible

set encoding=utf-8
setglobal fileencoding=utf-8
set fileencodings=ucs-bom,utf-8,latin1

set laststatus=2

set autoindent
set smartindent

set tabstop=4
set shiftwidth=4
set expandtab

set number
set cpoptions=B$
set foldmethod=marker

set list
set listchars=tab:▸\ ,eol:¬,trail:•,extends:»,precedes:«
set colorcolumn=80

set backspace=indent,eol,start

set t_Co=256

set wildignore+=*/tmp/*,*.so,*.swp,*.pyc,*.zip

colorscheme badwolf

" Filetype settings
autocmd Filetype python setlocal tabstop=4 shiftwidth=4
autocmd Filetype html setlocal tabstop=2 shiftwidth=2
autocmd Filetype htmldjango setlocal tabstop=2 shiftwidth=2
autocmd Filetype javascript setlocal tabstop=2 shiftwidth=2
autocmd Filetype scss setlocal tabstop=2 shiftwidth=2 autoindent

" GitGutter
highlight clear SignColumn
let g:gitgutter_realtime = 0

" Powerline
python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup

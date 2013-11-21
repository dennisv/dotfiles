execute pathogen#infect()
syntax on
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

set t_Co=256

colorscheme molokai

" Make GitGutter column the same color
highlight clear SignColumn

" Airline settings
let g:airline_powerline_fonts=1
let g:airline#extensions#tabline#enabled=1

set nocompatible

filetype on
filetype plugin on
filetype indent on

set directory=~/.vim/backup,/tmp
set foldmethod=syntax

set tabstop=4
set shiftwidth=4

set wrapscan

set ch=2

set vb

set backspace=2

set hidden

set cpoptions=ces$

set number

set stl=%f\ %m\ %r\ Line:%l/%L[%p%%]\ Col:%c\ Buf:%n\ [%b][0x%B]

set laststatus=2

set showcmd

set showmode

syntax on

set wildmenu

set hlsearch

"-----------------------------------------------------------------------------
"" NERD Tree Plugin Settings
"-----------------------------------------------------------------------------
"" Toggle the NERD Tree on an off with F7
nmap <F7> :NERDTreeToggle<CR>

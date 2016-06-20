if !1 | finish | endif

if has('vim_starting')
  if &compatible
    set nocompatible
  endif

  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif
filetype off

call neobundle#begin(expand('~/.vim/bundle/'))

let g:neobundle#install_process_timeout = 1500

NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle '~/.vim/bundle/django-custom'
NeoBundle 'matchit.zip'
NeoBundle 'benekastah/neomake' "{{{
  let s:eslint_path = system('PATH=$(npm bin):$PATH && which eslint')
  let g:neomake_javascript_eslint_exe = substitute(s:eslint_path, '^\n*\s*\(.\{-}\)\n*\s*$', '\1', '')
  let g:neomake_javascript_enabled_makers = ['eslint']
  let g:neomake_jsx_enabled_makers = ['eslint']

  " Run Neomake on every file write
  autocmd! BufWritePost * Neomake
"}}}
NeoBundle 'vim-airline/vim-airline-themes'
NeoBundle 'vim-airline/vim-airline' "{{{
  let g:airline_powerline_fonts = 1
  let g:airline_theme = 'powerlineish'
  let g:airline#extensions#tabline#fnamemod = ':t'
"}}}
NeoBundle '2072/PHP-Indenting-for-VIm'
NeoBundle 'bkad/vim-terraform'
NeoBundleLazy 'cakebaker/scss-syntax.vim', {'autoload':{'filetypes':['scss','sass']}}
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundleLazy 'evanmiller/nginx-vim-syntax', {'autoload': {'filetypes': 'nginx'}}
NeoBundle 'fatih/molokai'
NeoBundle 'fatih/vim-go'
NeoBundle 'fisadev/vim-isort', {'autoload':{'filetypes':['python']}}
NeoBundle 'honza/vim-snippets'
NeoBundleLazy 'klen/python-mode', {'autoload':{'filetypes':['python']}} "{{{
  let g:pymode_rope=0
  let g:pymode_folding=0
"}}}
NeoBundleLazy 'davidhalter/jedi-vim', {'autoload':{'filetypes':['python']}} "{{{
  let g:jedi#popup_on_dot=0
  let g:jedi#use_tabs_not_buffers=0
"}}}
NeoBundle 'mattn/emmet-vim' "{{{
  imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")
"}}}
NeoBundle 'mhinz/vim-signify' "{{{
  let g:signify_update_on_bufenter=0
"}}}
NeoBundle 'mhinz/vim-startify' "{{{
  let g:startify_change_to_vcs_root = 1
  let g:startify_show_sessions = 1
  nnoremap <F1> :Startify<cr>
"}}}
NeoBundle 'mxw/vim-jsx'
NeoBundle 'pangloss/vim-javascript'
NeoBundleLazy 'peterhoeg/vim-qml', {'autoload': {'filetypes': 'qml'}}
NeoBundle 'saltstack/salt-vim'
NeoBundle 'SirVer/ultisnips' "{{{
  let g:UltiSnipsExpandTrigger="<tab>"
  let g:UltiSnipsJumpForwardTrigger="<tab>"
  let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
  let g:UltiSnipsSnippetsDir='~/.vim/snippets'
"}}}
NeoBundle 'sjl/badwolf'
NeoBundle 'sjl/gundo.vim'
NeoBundle 'Shougo/unite.vim' "{{{
let g:unite_source_history_yank_enable=1
let g:unite_prompt='» '

" if executable('pt')
"   let g:unite_source_rec_async_command='pt --nocolor --nogroup --ignore ".hg" --ignore ".svn" --ignore ".git" --ignore ".bzr" --hidden -g .'
"   let g:unite_source_grep_command = 'pt'
"   let g:unite_source_grep_default_opts = '--nogroup --nocolor'
"   let g:unite_source_grep_recursive_opt = ''
"   let g:unite_source_grep_encoding = 'utf-8'
" elseif executable('ag')
if executable('ag')
  let g:unite_source_rec_async_command=['ag', '--nocolor', '--nogroup', '--ignore', '".hg"', '--ignore', '".svn"', '--ignore', '".git"', '--ignore', '".bzr"', '--hidden', '-g', '']
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts='-i --vimgrep --hidden --ignore ".hg" --ignore ".svn" --ignore ".git" --ignore ".bzr"'
  let g:unite_source_grep_recursive_opt=''
elseif executable('ack')
  let g:unite_source_grep_command='ack'
  let g:unite_source_grep_default_opts='--no-heading --no-color'
  let g:unite_source_grep_recursive_opt=''
endif

function! s:unite_settings()
  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)

  nmap <buffer> Q <plug>(unite_exit)
  nmap <buffer> <esc> <plug>(unite_exit)
  imap <buffer> <esc> <plug>(unite_exit)
endfunction
autocmd FileType unite call s:unite_settings()

nmap <space> [unite]
nnoremap [unite] <nop>

nnoremap <silent> [unite]f :<C-u>Unite -toggle -auto-resize -buffer-name=files file_rec/async:!<cr><c-u>
nnoremap <silent> [unite]e :<C-u>Unite -buffer-name=recent file_mru<cr>
nnoremap <silent> [unite]y :<C-u>Unite -buffer-name=yanks history/yank<cr>
nnoremap <silent> [unite]b :<C-u>Unite -auto-resize -buffer-name=buffers buffer<cr>
nnoremap <silent> [unite]/ :<C-u>Unite -no-quit -buffer-name=search grep:.<cr>
nnoremap <silent> [unite]s :<C-u>Unite -quick-match buffer<cr>
"}}}
NeoBundleLazy 'Shougo/unite-outline', {'autoload':{'unite_sources':'outline'}} "{{{
nnoremap <silent> [unite]o :<C-u>Unite -auto-resize -buffer-name=outline outline<cr>
"}}}
NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make -f make_mac.mak',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }
NeoBundle 'Shougo/neomru.vim', {'autoload':{'unite_sources':'file_mru'}}
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'blacktorn/vim-endwise'
NeoBundle 'ternjs/tern_for_vim', {'build': {'mac': 'npm install'}}
NeoBundle 'tpope/vim-eunuch'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'bitbucket:ludovicchabant/vim-lawrencium'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-unimpaired' "{{{
  nmap <c-up> [e
  nmap <c-down> ]e
  vmap <c-up> [egv
  vmap <c-down> ]egv
"}}}
NeoBundle 'Valloric/YouCompleteMe', {
     \ 'build'      : {
        \ 'mac'     : './install.py --clang-completer --system-libclang --gocode-completer',
        \ 'unix'    : './install.py --clang-completer --system-libclang --gocode-completer',
        \ 'windows' : './install.py --clang-completer --system-libclang --gocode-completer',
        \ 'cygwin'  : './install.py --clang-completer --system-libclang --gocode-completer'
        \ }
     \ } "{{{
  let g:ycm_complete_in_comments_and_strings=1
  let g:ycm_key_list_select_completion=['<C-n>', '<Down>']
  let g:ycm_key_list_previous_completion=['<C-p>', '<Up>']
  let g:ycm_filetype_blacklist={'unite': 1}
"}}}
NeoBundle 'vim-scripts/Unicode-RST-Tables'

call neobundle#end()

filetype plugin indent on

NeoBundleCheck

let g:python_host_prog='/usr/local/bin/python'

syntax on

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
set relativenumber
set cpoptions=B$
set pastetoggle=<F10>

" Folding ----------------------------------------------------------------- {{{

set foldmethod=marker
set foldlevelstart=0

" Space to toggle folds.
nnoremap <space><space> za
vnoremap <space><space> za

" Make zO recursively open whatever fold we're in, even if it's partially open.
nnoremap zO zczO

" "Focus" the current line.  Basically:
"
" 1. Close all folds.
" 2. Open just the folds containing the current line.
" 3. Move the line to a little bit (15 lines) above the center of the screen.
" 4. Pulse the cursor line.  My eyes are bad.
"
" This mapping wipes out the z mark, which I never use.
"
" I use :sus for the rare times I want to actually background Vim.
nnoremap <c-z> mzzMzvzz15<c-e>`z:Pulse<cr>

function! MyFoldText() " {{{
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endfunction " }}}
set foldtext=MyFoldText()

" }}}

set list
set listchars=tab:▸\ ,eol:¬,trail:•,extends:»,precedes:«
set colorcolumn=80

set cursorline
autocmd WinLeave * setlocal nocursorline
autocmd WinEnter * setlocal cursorline

set backspace=indent,eol,start

set t_Co=256

set wildignore+=*/tmp/*,*.so,*.swp,*.pyc,*.zip
set backupdir=~/.backups,~/.tmp,~/tmp,/var/tmp,/tmp

let mapleader = ","
let g:mapleader = ","

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <F3> :<C-u>GundoToggle<CR>

colorscheme molokai

autocmd BufWritePre * :%s/\s\+$//e
au FocusLost * :wa

" Pulse Line {{{

function! s:Pulse() " {{{
    redir => old_hi
        silent execute 'hi CursorLine'
    redir END
    let old_hi = split(old_hi, '\n')[0]
    let old_hi = substitute(old_hi, 'xxx', '', '')

    let steps = 8
    let width = 1
    let start = width
    let end = steps * width
    let color = 233

    for i in range(start, end, width)
        execute "hi CursorLine ctermbg=" . (color + i)
        redraw
        sleep 6m
    endfor
    for i in range(end, start, -1 * width)
        execute "hi CursorLine ctermbg=" . (color + i)
        redraw
        sleep 6m
    endfor

    execute 'hi ' . old_hi
endfunction " }}}
command! -nargs=0 Pulse call s:Pulse()

" }}}

" Switch from block-cursor to vertical-line-cursor when going into/out of insert mode
if has('nvim')
  let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" Unite.vim
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
call unite#custom#profile('default', 'context', {
      \ 'start_insert': 1
      \ })
call unite#custom#source('file_rec/async','sorters','sorter_rank')
call unite#custom_source('file_rec,file_rec/async,file_mru,file,buffer,grep',
      \ 'ignore_pattern', join([
      \ '\.git/',
      \ '\.hg/',
      \ 'git5/.*/review/',
      \ 'google/obj/',
      \ 'tmp/',
      \ '.vagrant/',
      \ '.sass-cache/',
      \ '.ropeproject/',
      \ '.idea/',
      \ '.iml',
      \ 'node_modules/',
      \ 'bower_components/',
      \ 'dist/',
      \ '.git5_specs/',
      \ '.pyc',
      \ ], '\|'))

au BufNewFile,BufRead *.scss set filetype=scss.css
au BufNewFile,BufRead *.html setlocal filetype=htmldjango
au BufNewFile,BufRead *.qml setlocal filetype=qml

" Filetype settings
autocmd Filetype vim setlocal tabstop=2 shiftwidth=2 autoindent
autocmd Filetype python setlocal tabstop=4 shiftwidth=4
autocmd Filetype html setlocal tabstop=2 shiftwidth=2
autocmd Filetype htmldjango setlocal tabstop=2 shiftwidth=2
autocmd Filetype javascript setlocal tabstop=2 shiftwidth=2
autocmd Filetype json setlocal tabstop=2 shiftwidth=2
autocmd Filetype scss setlocal tabstop=2 shiftwidth=2 autoindent
autocmd Filetype yaml setlocal tabstop=2 shiftwidth=2 autoindent

autocmd FileType scss,css nnoremap <buffer> <F5> :call CSScomb()<CR>
function! CSScomb()
  execute "silent !csscomb " . expand('%')
  redraw!
endfunction

if has('gui_running')
  " GUI Vim

  set guifont=PragmataPro:h14

  " Remove all the UI cruft
  set go-=T
  set go-=l
  set go-=L
  set go-=r
  set go-=R

  highlight SpellBad term=underline gui=undercurl guisp=Orange

  " Different cursors for different modes.
  set guicursor=n-c:block-Cursor-blinkon0
  set guicursor+=v:block-vCursor-blinkon0
  set guicursor+=i-ci:ver20-iCursor

  if has("gui_macvim")
    " Full screen means FULL screen
    set fuoptions=maxvert,maxhorz
  end
endif

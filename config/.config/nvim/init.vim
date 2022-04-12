
"
" Plugins
"

" Load Plug and plugins
call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'arcticicestudio/nord-vim'
Plug 'dyng/ctrlsf.vim'
Plug 'elzr/vim-json'
Plug 'godlygeek/tabular'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'kien/ctrlp.vim'
Plug 'mxw/vim-jsx'
Plug 'pangloss/vim-javascript'
Plug 'plasticboy/vim-markdown'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
call plug#end()


"
" Theme Settings
"

" Set colorsceme to nord
silent! colorscheme nord
" Activate nord airline theme
let g:airline_theme='nord'



syntax on

" disable the arrow keys
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

" Causes weird formatting issues on SmartOS
" :set lines=40 columns=140

" Fix pbcopy/pbpase on OS X terminal vim
:set clipboard=unnamed

" suggestions from
" http://stevelosh.com/blog/2010/09/coming-home-to-vim/#important-vimrc-lines 

" dont' worry about VI compatibility
set nocompatible

" for security
set modelines=0

filetype plugin indent on
set tabstop=4
set shiftwidth=4
set softtabstop=4
autocmd filetype python setlocal shiftwidth=4 softtabstop=2
autocmd filetype ruby setlocal shiftwidth=2 softtabstop=2
autocmd filetype css setlocal shiftwidth=4 softtabstop=4
autocmd filetype javascript setlocal shiftwidth=2 softtabstop=2
set expandtab

set encoding=utf-8
set scrolloff=3
set autoindent
" message on the last line showing mode (insert, replace or visual)
set showmode
" show partial command in the last line of the screen
set showcmd
" allow buffers with unsaved modifications to be hidden
set hidden
" turn the beep into a visual representation rather than a sound
set visualbell
" display a line below the line the cursor is on
set cursorline
" send more characters at a time
set ttyfast
" display the line and column Separated by a comma
set ruler
" allow backspacing over indents, end of lines and start of an insert
set backspace=indent,eol,start
" make sure the last window always has a status line
set laststatus=2

if version >= 703
  " show the line number relative to the line with the cursor
  set relativenumber
  set number " current line shows line number
endif

if has('persistent_undo')
  " save undo history to an undo file
  set undofile

  " Store swap files in fixed location, not current directory.
  set dir=~/.vimswap//,/var/tmp//,/tmp//,c:\tmp,.
  " Store undo files in fixed location, not current directory.
  set undodir=~/.vimundo//,/var/tmp//,/tmp//,c:\tmp,.
endif

" remap the leader key from \ to ,
let mapleader = "\<space>"
let maplocalleader = "\<space>"
" don't throw away the reverse character search command
noremap \ ,

" tame search
nnoremap / /\v
vnoremap / /\v
set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
" clear the search highlighting
nnoremap <leader><space> :noh<cr>
" use git grep to search
nnoremap <Leader>f :CtrlSF<Space>

" handle long lines
set wrap
set textwidth=79
set formatoptions=qrn1

" change the status line based on mode
if version >= 700
  au InsertEnter * hi StatusLine term=reverse ctermfg=7* ctermbg=1* guibg=Red   guifg=White
  au InsertLeave * hi StatusLine term=reverse ctermfg=0  ctermbg=2  guibg=Green guifg=Black
endif
 

" enable TagBar
nmap <leader>8 :TagbarToggle<CR>

" pretty print json
nmap <leader>1 :%!python -m json.tool<CR>
" pretty print xml
nmap <leader>2 :silent %!xmllint --encode UTF-8 --format -<CR>

" If you prefer the Omni-Completion tip window to close when a selection is
" " made, these lines close it on movement in insert mode or when leaving
" " insert mode
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" make the 81st column stand out
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)

" display tab and extra whitespace characters
exec "set listchars=tab:\uBB\uBB,trail:\uB7,nbsp:~"
" set list

" Fugitive
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" print git branch in the status line
if has('win32') || has('win64')
else
  set statusline=%F\ %m\ %{fugitive#statusline()}\ %y%=%l,%c\ %P
endif
" let Gdiff default to vertical layout
set diffopt=vertical
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END Fugitive
 

" CtrlP
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.un~,.DS_Store,tags,.*
set wildignore+=*.pyc
let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'
let g:ctrlp_custom_ignore = 'node_modules$\|bower_components$\|public/lib$\|build$\|third-party$'
" 0) don't manage working directory
" c) the directory of the current file
" r) the nearest ancestor that contains one of these directories or files:
" w) begin finding a root from the current working directory outside of CtrlP
" .git/ .hg/ .svn/ .bzr/ _darcs/
let g:ctrlp_working_path_mode = 'r'
" increase the height of the match window
let g:ctrlp_max_height = 30
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
let g:ctrlp_use_caching = 0
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END CtrlP


" Syntastic
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" use local eslint (under node_modules)
let s:lcd = fnameescape(getcwd())
silent! exec "lcd" expand('%:p:h')
let s:eslint_path = system('PATH=$(npm bin):$PATH && which eslint')
exec "lcd" s:lcd
let b:syntastic_javascript_eslint_exec = substitute(s:eslint_path, '^\n*\s*\(.\{-}\)\n*\s*$', '\1', '')
" end use local eslint

function! FindConfig(prefix, what, where)
    let cfg = findfile(a:what, escape(a:where, ' ') . ';')
    return cfg !=# '' ? ' ' . a:prefix . ' ' . shellescape(cfg) : ''
endfunction
autocmd FileType javascript let b:syntastic_javascript_eslint_args =
    \ get(g:, 'syntastic_javascript_eslint_args', '') .
    \ FindConfig('-c', '.eslintrc.json', expand('<afile>:p:h', 1))

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_javascript_checkers=['eslint']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_mode_map={ 'mode': 'active',
                     \ 'active_filetypes': ['javascript', 'javascript.jsx'],
                     \ 'passive_filetypes': ['html'] }
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END Syntastic

" GitGutter
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap [h <Plug>GitGutterPrevHunk
nmap ]h <Plug>GitGutterNextHunk
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END GitGutter

" nerdtree
map <C-n> :NERDTreeToggle<CR>
map <leader>r :NERDTreeFind<cr>

let g:NERDTreeWinSize = 80
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END nerdtree

" CtrlSF
let g:ctrlsf_position = 'bottom'
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END CtrlSF


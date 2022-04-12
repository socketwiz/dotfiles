
"
" Plugins
"

" Load Plug and plugins
call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-gitgutter'               " updates gutter with hunk locations
Plug 'arcticicestudio/nord-vim'             " theme
Plug 'cohama/lexima.vim'                    " auto-pairing
Plug 'dyng/ctrlsf.vim'                      " pt-search wrapper
Plug 'editorconfig/editorconfig-vim'        " honor editorconfig properties
Plug 'elzr/vim-json'                        " json syntax highlighter
Plug 'godlygeek/tabular'                    " auto-align text
Plug 'HerringtonDarkholme/yats.vim'         " typescript syntax highlighter
Plug 'kien/ctrlp.vim'                       " fuzzy file finder
Plug 'mattn/webapi-vim'                     " dependency for rust.vim
Plug 'mxw/vim-jsx'                          " jsx syntax highlighter
Plug 'pangloss/vim-javascript'              " javascript syntax highlighter
Plug 'plasticboy/vim-markdown'              " markdown syntax highlighter
Plug 'ryanoasis/vim-devicons'               " filetype glyphs for popular addons,
                                            " nerdtree, vim-airline, ctrlp, etc...
Plug 'rust-lang/rust.vim'                   " rust support
Plug 'scrooloose/nerdtree'                  " file system explorer
Plug 'scrooloose/syntastic'                 " syntax checker
Plug 'tomtom/tcomment_vim'                  " commenter
Plug 'tpope/vim-fugitive'                   " git wrapper
Plug 'tpope/vim-surround'                   " quote, paranthesis wrapper
Plug 'vim-airline/vim-airline'              " customized status line
Plug 'vim-airline/vim-airline-themes'       " ^ status line themes
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

" Fix pbcopy/pbpaste on OS X terminal vim
set clipboard=unnamed

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

" remap the leader key from \ to space
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

" handle long lines
set wrap
set textwidth=79
set formatoptions=qrn1

" pretty print json
nmap <leader>1 :%!python -m json.tool<CR>
" pretty print xml
nmap <leader>2 :silent %!xmllint --encode UTF-8 --format -<CR>

" make the 81st column stand out
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)

" display tab and extra whitespace characters
exec "set listchars=tab:\uBB\uBB,trail:\uB7,nbsp:~"
set list


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


" vim-jsx
" use git grep to search
let g:jsx_ext_required = 0 " Allow JSX in normal JS files
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END vim-jsx


" Syntastic
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

" use local eslint (under node_modules)
let g:syntastic_javascript_eslint_exec = 'eslint-vim'

let g:syntastic_javascript_checkers = ['eslint']
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
" use git grep to search
nnoremap <Leader>f :CtrlSF<Space>
let g:ctrlsf_position = 'bottom'
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END CtrlSF


" vim-devicons
" loading the plugin
let g:webdevicons_enable = 1
" adding the flags to NERDTree
let g:webdevicons_enable_nerdtree = 1
" adding to vim-airline's tabline
let g:webdevicons_enable_airline_tabline = 1
" adding to vim-airline's statusline
let g:webdevicons_enable_airline_statusline = 1
" ctrlp glyphs
let g:webdevicons_enable_ctrlp = 1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END vim-devicons


" rust.vim
" use git grep to search
let g:rustfmt_autosave = 1 " automatically run rustfmt when buffer is saved
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END rust.vim


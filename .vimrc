colorscheme vividchalk
syntax on
set guifont=Source\ Code\ Pro:h14
set guioptions-=T  "remove toolbar
set tags=tags,gemtags

" disable the arrow keys
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

:set lines=40 columns=140

" suggestions from
" http://stevelosh.com/blog/2010/09/coming-home-to-vim/#important-vimrc-lines 

" dont' worry about VI compatibility
set nocompatible

" for security
set modelines=0

set tabstop=2
set shiftwidth=2
set softtabstop=2
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
endif

if has('persistent_undo')
  " save undo history to an undo file
  set undofile

  " Store swap files in fixed location, not current directory.
  set dir=~/.vimswap//,/var/tmp//,/tmp//,.
  " Store undo files in fixed location, not current directory.
  set undodir=~/.vimundo//,/var/tmp//,/tmp//,.
endif

" remap the leader key from \ to ,
let mapleader = ","
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
" use Ack to search files under current directory
nnoremap <Leader>f :Ack<Space>

" handle long lines
set wrap
set textwidth=79
set formatoptions=qrn1

" enable TagBar
nmap <leader>8 :TagbarToggle<CR>

" If you prefer the Omni-Completion tip window to close when a selection is
" " made, these lines close it on movement in insert mode or when leaving
" " insert mode
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" pathogen (requires pathogen addon)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype off
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()
call pathogen#infect()
filetype plugin indent on
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END pathogen

" Fugitive
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" print git branch in the status line
if has('win32') || has('win64')
else
  set statusline=%F\ %m\ %{fugitive#statusline()}\ %y%=%l,%c\ %P
endif
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END Fugitive

" CtrlP
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.un~,.DS_Store,tags,.*
let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'
let g:ctrlp_custom_ignore = 'node_modules\|bower_components\|public/lib'
" 0) don't manage working directory
" 1) the directory of the current file
" 2) the nearest ancestor that contains one of these directories or files:
" .git/ .hg/ .svn/ .bzr/ _darcs/
let g:ctrlp_working_path_mode = 1
" increase the height of the match window
let g:ctrlp_max_height = 30
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END CtrlP

" UltiSnips (requires UltiSnips addon)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:UltiSnipsEditSplit = 'horizontal'
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsSnippetDirectories = ["UltiSnipsAngular", "UltiSnips"]
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END UltiSnips

" javascript-libraries-syntax
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:used_javascript_libs = 'jquery,angularjs,backbone,underscore'
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END javascript-libraries-syntax

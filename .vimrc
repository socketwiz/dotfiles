" my own settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
colorscheme vividchalk
syntax on
set guifont=Menlo\ Regular:h14
set guioptions-=T  "remove toolbar
set tags=tags,gemtags

" enable spell checking
map <F5> :setlocal spell spelllang=en_us<cr>
" toggle undo manager
noremap <F6> :GundoToggle<cr>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END my own settings

" print git branch in the status line (requires fugitive addon)
set statusline=%F\ %m\ %{fugitive#statusline()}\ %y%=%l,%c\ %P

" pathogen (requires pathogen addon)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype off
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()
call pathogen#infect()
filetype plugin indent on
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END pathogen

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
" show the line number relative to the line with the cursor
set relativenumber
" save undo history to an undo file
set undofile

" remap the leader key from \ to ,
let mapleader = ","

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

" disable autoindent when F2 is pressed
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>

" CtrlP
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.un~,.DS_Store,tags,.*
let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'
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
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END UltiSnips

" neocomplcache
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_temporary_dir = "$HOME/.vim/tmp/neocomplcache"
" let g:neocomplcache_enable_auto_select = 1

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
"autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
" let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.c = '\%(\.\|->\)\h\w*'
let g:neocomplcache_omni_patterns.cpp = '\h\w*\%(\.\|->\)\h\w*\|\h\w*::'
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" END neocomplcache


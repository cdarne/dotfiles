set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Plugins
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'

Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-git'
Plugin 'tpope/vim-projectionist'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-rake'
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-vinegar'

Plugin 'vim-ruby/vim-ruby'
Plugin 'fatih/vim-go'
Plugin 'skalnik/vim-vroom'
Plugin 'Shougo/neocomplete.vim'

" All of your Plugins must be added before the following line
call vundle#end() " required
filetype plugin indent on " required

" Leader
let mapleader = "ù"

colorscheme desert
set background=dark

if has('gui_running')
  set guifont=Source\ Code\ Pro:h11
  "set guioptions-=T  "remove toolbar
  set guioptions-=r  "remove right-hand scroll bar
  set guioptions-=L  "remove left-hand scroll bar

  if has("autocmd")
    " Automatically resize splits when resizing MacVim window
    autocmd VimResized * wincmd =
  endif
endif

" Whitespace
set nowrap                        " don't wrap lines
set tabstop=2                     " a tab is two spaces
set shiftwidth=2                  " an autoindent (with <<) is two spaces
set softtabstop=2                 " tab is 2 spaces while editing
set expandtab                     " use spaces, not tabs
set smarttab                      " be smart about tabs (line start)
set list                          " Show invisible characters
set backspace=indent,eol,start    " backspace through everything in insert mode

set autoindent
set smartindent

" Window split defaults
set splitbelow
set splitright

" Please no backups
set nobackup
set nowritebackup
set noswapfile

" Display extra whitespace
set list listchars=tab:¬\ ,trail:·

" Numbers
set number
set numberwidth=5

"Always show current position
set ruler

" Turn on syntax highlighting allowing local overrides
syntax enable

" Searching
set hlsearch    " highlight matches
set incsearch   " incremental searching
set ignorecase  " searches are case insensitive...
set smartcase   " ... unless they contain at least one capital letter

set lazyredraw " Don't redraw while executing macros (good performance config)

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" Use Unix as the standard file type
set ffs=unix,dos,mac

""
"" File types
""

if has("autocmd")
  " In Makefiles, use real tabs, not tabs expanded to spaces
  au FileType make setlocal noexpandtab
  au FileType go setlocal noexpandtab nolist

  " Set the Ruby filetype for a number of common Ruby files without .rb
  au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,Procfile,Guardfile,config.ru,*.rake,*.rabl} set ft=ruby

  " Auto save files when losing focus
  "au BufLeave,FocusLost * silent! wall " ingoring warnings
  au BufLeave,FocusLost * wall
end

""
"" Mappings
""

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk
map <Down> gj
map <Up> gk

" Easier window navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Adjust viewports to the same size
map <Leader>= <C-w>=

" format the entire file
nnoremap <leader>fef :normal! gg=G``<CR>

map <Leader>n :NERDTreeToggle<CR>

""
"" Plugins config
""

" vim-go mappings
au FileType go nmap <Leader>gd <Plug>(go-doc-tab)
au FileType go nmap <Leader>gg <Plug>(go-def-tab)
au FileType go nmap <leader>gr <Plug>(go-run)
au FileType go nmap <leader>gb <Plug>(go-build)
au FileType go nmap <leader>gt <Plug>(go-test)
au FileType go nmap <leader>gc <Plug>(go-coverage)
au FileType go nmap <leader>gi :GoImports<CR>

" neocomplete
let g:neocomplete#enable_at_startup = 1
" Set minimum syntax keyword length
let g:neocomplete#sources#syntax#min_keyword_length = 3
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

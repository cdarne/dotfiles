set nocompatible
filetype off

" Vundle config

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Plugins
Plugin 'tpope/vim-surround' " Surround commands
Plugin 'tpope/vim-sensible' " Defaults for vim
Plugin 'scrooloose/nerdtree' " Tree files view
Plugin 'scrooloose/syntastic' " Syntax checker
Plugin 'kien/ctrlp.vim' " Quick open files
Plugin 'mileszs/ack.vim' " Ack (grep replacement) wrapper

" Tags
"Plugin 'majutsushi/tagbar' "class outline viewer
"Plugin 'xolox/vim-misc' " Dependency for vim-easytags
"Plugin 'xolox/vim-easytags' "Automated tag file generation and syntax highlighting

" Auto-complete (needs lua support in vim)
Plugin 'Shougo/neocomplete.vim'
Plugin 'Shougo/neosnippet.vim'
Plugin 'Shougo/neosnippet-snippets'

" git
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-git'

" Ruby
Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-rake'
Plugin 'vim-ruby/vim-ruby'
"Plugin 'skalnik/vim-vroom' " Test runner

Plugin 'fatih/vim-go'

" Elixir
Plugin 'elixir-lang/vim-elixir'
Plugin 'slashmili/alchemist.vim'

Plugin 'lambdatoast/elm.vim'

" Color themes
Plugin 'morhetz/gruvbox'
Plugin 'vim-airline/vim-airline'

" All of your Plugins must be added before the following line
call vundle#end() " required
filetype plugin indent on " required

if filereadable(expand("~/.vimrc.before"))
  source ~/.vimrc.before
endif

" End of Vundle config

" Theme/color schemes config

" Turn on syntax highlighting allowing local overrides
syntax enable

set background=dark
let g:gruvbox_contrast_dark='hard'
set t_Co=256
colorscheme gruvbox

if has("gui_running")
  set guioptions-=T  "remove toolbar
  set guioptions-=m  "remove toolbar
  set guioptions-=r  "remove right-hand scroll bar
  set guioptions-=L  "remove left-hand scroll bar
  set guifont=Ubuntu\ Mono

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
  au FileType php setlocal ts=4 sts=4 sw=4

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

" Quickfix easy navigation
nnoremap <A-j> :cn<CR>
nnoremap <A-k> :cp<CR>
nnoremap <A-J> :cla<CR>
nnoremap <A-K> :cr<CR>

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
map <Leader>t :TagbarToggle<CR>

""
"" Plugins config
""

" vim-go
"
au FileType go nmap <Leader>gs <Plug>(go-implements)
au FileType go nmap <Leader>gi <Plug>(go-info)
au FileType go nmap <Leader>gd <Plug>(go-doc-tab)
au FileType go nmap <Leader>gg <Plug>(go-def-tab)
au FileType go nmap <leader>gr <Plug>(go-run)
au FileType go nmap <leader>gb <Plug>(go-build)
au FileType go nmap <leader>gt <Plug>(go-test)
au FileType go nmap <leader>gc <Plug>(go-coverage)
au FileType go nmap <leader>gmv <Plug>(go-rename)
au FileType go nmap <leader>gimp :GoImports<CR>
let g:go_fmt_command = "goimports"

au BufWritePost,FileWritePost *.go execute 'GoLint'

" neocomplete
"
" Disable AutoComplPop.
"let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  "return neocomplete#close_popup() . "\<CR>"
  " For no inserting <CR> key.
  return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

" neosnippet
"
" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: "\<TAB>"

" For snippet_complete marker.
if has('conceal')
  set conceallevel=2 concealcursor=i
endif

" ctrlp
"
let g:ctrlp_working_path_mode = ''
nnoremap <leader>. :CtrlPTag<cr>

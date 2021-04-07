set nocompatible
filetype off

"" minpac config
packadd minpac
call minpac#init()

" commands:
command! PackUpdate call minpac#update()
command! PackClean call minpac#clean()

" Plugins
call minpac#add('tpope/vim-surround') " Surround commands
call minpac#add('tpope/vim-sensible') " Defaults for vim
call minpac#add('scrooloose/nerdtree') " Tree files view
call minpac#add('vim-syntastic/syntastic') " Syntax checker
call minpac#add('Chiel92/vim-autoformat') " Syntax checker
call minpac#add('autozimu/LanguageClient-neovim') " LSP

" Autocomplete
call minpac#add('Shougo/deoplete.nvim')
if !has('nvim')
  call minpac#add('roxma/nvim-yarp')
  call minpac#add('roxma/vim-hug-neovim-rpc')
endif
let g:deoplete#enable_at_startup = 1

" Tags
"call minpac#add('majutsushi/tagbar') "class outline viewer
"call minpac#add('xolox/vim-misc') " Dependency for vim-easytags
"call minpac#add('xolox/vim-easytags') "Automated tag file generation and syntax highlighting

" git
call minpac#add('tpope/vim-fugitive')
call minpac#add('tpope/vim-git')

" Ruby
call minpac#add('vim-ruby/vim-ruby')
call minpac#add('tpope/vim-bundler')
call minpac#add('tpope/vim-rails')
call minpac#add('tpope/vim-rake')
call minpac#add('skalnik/vim-vroom') " Test runner

call minpac#add('fatih/vim-go')
" call minpac#add('elmcast/elm-vim')

" Elixir
" call minpac#add('elixir-editors/vim-elixir')
" call minpac#add('slashmili/alchemist.vim')

" rust
" call minpac#add('rust-lang/rust.vim')

" cal minpac#add('ziglang/zig.vim')

" Color themes
call minpac#add('morhetz/gruvbox')
call minpac#add('vim-airline/vim-airline')

" fzf
set rtp+=/usr/local/opt/fzf
call minpac#add('junegunn/fzf.vim')

filetype plugin indent on " required

if filereadable(expand("~/.vimrc.before"))
  source ~/.vimrc.before
endif

if !has('nvim')
  set ttymouse=xterm2
  set mouse=a
else
  set mouse=
endif

" Theme/color schemes config

" Turn on syntax highlighting allowing local overrides
syntax enable

set background=dark
let g:gruvbox_contrast_dark='hard'
set t_Co=256
colorscheme gruvbox

" Use system clipboard on OSX
set clipboard=unnamed

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

let mapleader=" "

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk
map <Down> gj
map <Up> gk

" Quickfix easy navigation
map <C-n> :cnext<CR>
map <C-m> :cprevious<CR>
nnoremap <leader>a :cclose<CR>

" Easier window navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <Leader>wc <C-W><C-C>
nnoremap <Leader>wo <C-W><C-C>
nnoremap <leader>wj <C-W><C-J>
nnoremap <leader>wk <C-W><C-K>
nnoremap <leader>wl <C-W><C-L>
nnoremap <leader>wh <C-W><C-H>

" Adjust viewports to the same size
map <Leader>w= <C-w>=

" format the entire file
nnoremap <leader>fef :normal! gg=G``<CR>

map <Leader>n :NERDTreeToggle<CR>
" map <Leader>t :TagbarToggle<CR>

" Reload .vimrc
nnoremap <Leader>fR :source $MYVIMRC<CR>

" Buffers
nnoremap <Leader>bn :bnext<CR>
nnoremap <Leader>bp :bprevious<CR>
nnoremap <Leader>bd :bdel<CR>

" Tabs
" nnoremap <Leader>tn :tabn<CR>
" nnoremap <Leader>tp :tabp<CR>

" git
nnoremap <silent> <Leader>gs :Gstatus<CR>
nnoremap <silent> <Leader>gd :Gdiff<CR>
nnoremap <silent> <Leader>gc :Gcommit<CR>
nnoremap <silent> <Leader>gb :Gblame<CR>
nnoremap <silent> <Leader>gl :Glog<CR>
nnoremap <silent> <Leader>gp :Git push<CR>
nnoremap <silent> <Leader>gr :Gread<CR>
nnoremap <silent> <Leader>gw :Gwrite<CR>
nnoremap <silent> <Leader>ge :Gedit<CR>
" Mnemonic _i_nteractive
nnoremap <silent> <Leader>gi :Git add -p %<CR>

" fzf files with Ctrl-P
nnoremap <C-p> :Files<Cr>
nnoremap <Leader>bb :Buffers<Cr>
nnoremap <Leader>ff :Files<Cr>
nnoremap <Leader>fg :Rg!<Cr>

" tests
" nnoremap <Leader>tt :VroomRunTestFile<Cr>
" nnoremap <Leader>tr :VroomRunTestFile<Cr>
" nnoremap <Leader>tn :VroomRunNearestTest<Cr>
" nnoremap <Leader>tl :VroomRunLastTest<Cr>

" format
nnoremap <Leader>= :Autoformat<CR>

""
"" Plugins config
""

" vim-go
"
autocmd FileType go nmap <leader>b <Plug>(go-build)
autocmd FileType go nmap <leader>r <Plug>(go-run)
autocmd FileType go nmap <leader>t <Plug>(go-test)
autocmd FileType go nmap <leader>c <Plug>(go-coverage-toggle)
let g:go_fmt_command = "goimports"

" au FileType go nmap <Leader>gs <Plug>(go-implements)
" au FileType go nmap <Leader>gi <Plug>(go-info)
" au FileType go nmap <Leader>gd <Plug>(go-doc-tab)
" au FileType go nmap <Leader>gg <Plug>(go-def-tab)
" au FileType go nmap <leader>gr <Plug>(go-run)
" au FileType go nmap <leader>gt <Plug>(go-test)
" au FileType go nmap <leader>gc <Plug>(go-coverage)
" au FileType go nmap <leader>gmv <Plug>(go-rename)
" au FileType go nmap <leader>gimp :GoImports<CR>
" au BufWritePost,FileWritePost *.go execute 'GoLint'

" elm config
let g:elm_detailed_complete = 0

" ALE config
" let g:ale_completion_enabled = 1

let g:rustfmt_autosave = 1

"" LSP Client Config

" Don't send a stop signal to the server when exiting vim.
" This is optional, but I don't like having to restart Solargraph
" every time I restart vim.
let g:LanguageClient_autoStop = 0
" Tell the language client to use the default IP and port
" that Solargraph runs on
let g:LanguageClient_serverCommands = {
    \ 'ruby': ['tcp://localhost:7658']
    \ }
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

" Configure ruby omni-completion to use the language client:
autocmd FileType ruby setlocal omnifunc=LanguageClient#complete

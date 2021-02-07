" General Vim Configuration:
" Force incompatibility with Vi
set nocompatible
set encoding=utf-8

" Remap to leader-key to the space bar
let mapleader = "\<SPACE>"

" Enable syntax highlighting and file type features 
syntax on
filetype plugin on
filetype indent on

" Enable spelling and setting the
set spell
setlocal spell spelllang=en_us

" Enable mouse-support
set mouse=a

" set shell=/bin/bash

set number
set relativenumber
set cursorline
" Highlight the 99 characters limit out
set colorcolumn=99
set linebreak
" Perform a line break when the limit is reached
set textwidth=99

" Better selection-menu for Vim commands
set wildmenu
set wildmode=full
" Number of screen lines to use for the command-line
set cmdheight=2
" Synchronize the system clipboard (xclip) with Vim
set clipboard=unnamedplus

" Indention and no tabs
set smartindent
set smarttab
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

" No swap- and backup files
set nobackup
set nowritebackup
set noswapfile

" Better search
set nohlsearch
set incsearch
set ignorecase
set smartcase

set signcolumn=number

" Switch between buffers, even if they have unsaved changes
set hidden
" Split new window below or left
set splitbelow
set splitright


" Show up to 15 items
set pumheight=15
" 10% transparency for completion dialog
set pumblend=10

" Use % to jump to the other ends of </> as well
set matchpairs+=<:>
" Show whitespace, tabs and more...
" Show whitespace, tabs and more... (other nice chars: )
set listchars=tab:»·,trail:~,extends:>,precedes:<,space:·,eol:¬
set list

" No Python 2
let g:loaded_python_provider = 0
" Make pynvim work inside virtualenvs
let g:python3_host_prog = '/usr/bin/python3'


call plug#begin('~/.local/share/nvim/plugged')
Plug 'morhetz/gruvbox'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'sonph/onehalf', { 'rtp': 'vim' }
Plug 'arcticicestudio/nord-vim'
Plug 'vim-airline/vim-airline'
Plug 'preservim/nerdtree'
Plug 'sheerun/vim-polyglot'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'justinmk/vim-sneak'
Plug 'tpope/vim-commentary'
call plug#end()


" Syntax highlighting
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_structs = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_build_constraints = 1


" Gruvbox Configuration:
" Some customization of the gruvbox colorscheme
let g:gruvbox_bold = 1
let g:gruvbox_italic = 1
let g:gruvbox_underline = 1
let g:gruvbox_undercurl = 1
let g:gruvbox_termcolors = 256
let g:gruvbox_contrast_dark = 'medium'
let g:gruvbox_contrast_light = 'medium'
let g:gruvbox_italicize_comments = 1
let g:gruvbox_italicize_strings = 1
let g:gruvbox_invert_selection = 0

" Better colors
set termguicolors
set background=dark

" NOTE: This has to happen after the configuration above
colorscheme dracula


" Airline Configuration:
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme='dracula'


let NERDTreeShowHidden = 1
let g:NERDTreeMinimalUI = 1
let g:NERDTreeStatusline = ''
nnoremap <silent> <leader>e :NERDTreeToggle<CR>


" Configure FZF:
let g:fzf_layout = {
        \ 'window': {
        \ 'width': 0.8,
        \ 'height': 0.8,
        \ 'border': 'rounded' }
        \ }

" Use Ripgrep to ignore files also ignored by git
let $FZF_DEFAULT_COMMAND = "rg --files --hidden --follow --glob '!.git'"
map <silent> <leader><SPACE> :Files<CR>
map <silent> <leader>b :Buffers<CR>
map <silent> <leader>c :Commits<CR>
map <silent> <leader>g :Rg<CR>

let g:sneak#label = 1


" Indenting selected text multiple times, without loosing the selection
xnoremap < <gv
xnoremap > >gv

" Quickly switch between buffers with the tab key
nnoremap <silent> <TAB> :bn<CR>
nnoremap <silent> <S-TAB> :bp<CR>

" Close buffers, windows, ...
nnoremap <silent> <C-q> :bd<CR>


let g:coc_global_extensions = [
            \ "coc-snippets",
            \ "coc-tabnine",
            \ "coc-pairs",
            \ "coc-python",
            \ "coc-java",
            \ "coc-omnisharp",
            \ "coc-rust-analyzer",
            \ "coc-go",
            \ "coc-clangd",
            \ "coc-tsserver",
            \ "coc-vetur",
            \ "coc-html",
            \ "coc-css",
            \ "coc-yaml",
            \ "coc-emmet",
            \ "coc-eslint",
            \ "coc-json",
            \ "coc-yaml",
            \ ]


set completeopt=menuone,noinsert,noselect
set updatetime=300
set shortmess+=c

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ coc#expandableOrJumpable() ? 
      \ "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

let g:coc_snippet_next = '<tab>'

inoremap <expr> <S-TAB> pumvisible() ? "<C-p>" : "<S-TAB>"

" Confirm selection with the return key
inoremap <silent><expr> <CR> pumvisible() ? "\<C-y>" :
            \ "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Hit control and space to manually trigger the completion
inoremap <silent><expr> <C-SPACE> coc#refresh()

" Cycle thought the diagnostics list
nmap <silent> <C-j> <Plug>(coc-diagnostic-next)
nmap <silent> <C-k> <Plug>(coc-diagnostic-prev)

" Go to definition, implementation and references
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nnoremap <silent> gh :call CocActionAsync('doHover')<CR>

" Highlight the symbol and its references when holding the cursor
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming
nmap <leader>rn <Plug>(coc-rename)
" Formatting selected code
" Organize imports of the current buffer
nmap <leader>or <Plug>(coc-action-organizeImport)
" Show a list of all quickfixes
" Show a list of all code actions
xmap <leader>a  <Plug>(coc-codeaction-selected)<CR>
nmap <leader>a  <Plug>(coc-codeaction-selected)<CR>
nmap <leader>ca <Plug>(coc-codeaction)
nmap <leader>qf <Plug>(coc-fix-current)

nmap <leader>cl <Plug>(coc-codelens-action)
nmap <leader>rf <Plug>(coc-refactor)

autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=2 shiftwidth=2
autocmd BufNewFile,BufRead *.js setlocal noexpandtab tabstop=2 shiftwidth=2
autocmd BufNewFile,BufRead *.ts setlocal noexpandtab tabstop=2 shiftwidth=2

autocmd BufNewFile,BufRead *.py setlocal textwidth=80 colorcolumn=80

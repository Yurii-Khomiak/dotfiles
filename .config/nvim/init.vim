" ===============
" Basic settings

" Display line numbers
set nu
set rnu

" Enable syntax highlighting
syntax on

" More intuitive split direction that default
set splitbelow splitright

" Tab settings
set noexpandtab
set tabstop=8
set shiftwidth=8
set clipboard+=unnamedplus

" ============================
" Bindings

" Bindings for Copy/Paste from/to external program
vnoremap <C-c> "*Y :let @+=@*<CR>
map <C-p> "+P

" ============================
" Filetype specific bindings

" Haskell
autocmd BufRead,BufNewFile *.hs setlocal expandtab tabstop=4 shiftwidth=4


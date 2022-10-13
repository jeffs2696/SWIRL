let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
inoremap <silent> <Plug>(fzf-maps-i) :call fzf#vim#maps('i', 0)
inoremap <expr> <Plug>(fzf-complete-buffer-line) fzf#vim#complete#buffer_line()
inoremap <expr> <Plug>(fzf-complete-line) fzf#vim#complete#line()
inoremap <expr> <Plug>(fzf-complete-file-ag) fzf#vim#complete#path('ag -l -g ""')
inoremap <expr> <Plug>(fzf-complete-file) fzf#vim#complete#path("find . -path '*/\.*' -prune -o -type f -print -o -type l -print | sed 's:^..::'")
inoremap <expr> <Plug>(fzf-complete-path) fzf#vim#complete#path("find . -path '*/\.*' -prune -o -print | sed '1d;s:^..::'")
inoremap <expr> <Plug>(fzf-complete-word) fzf#vim#complete#word()
imap <C-J> <Plug>IMAP_JumpForward
inoremap <silent> <Plug>IMAP_JumpBack :call IMAP_Jumpfunc('b', 0)
inoremap <silent> <Plug>IMAP_JumpForward :call IMAP_Jumpfunc('', 0)
inoremap <silent> <C-Tab> =UltiSnips#ListSnippets()
inoremap <C-L> u[s1z=`]a
cnoremap <F5> :set list!
inoremap <F5> :set list!
xmap  :NERDTreeToggle
nmap  :NERDTreeToggle
snoremap <silent>  "_c
omap  :NERDTreeToggle
xnoremap <silent> 	 :call UltiSnips#SaveLastVisualSelection()gvs
snoremap <silent> 	 :call UltiSnips#ExpandSnippetOrJump()
vmap <NL> <Plug>IMAP_JumpForward
nmap <NL> <Plug>IMAP_JumpForward
nnoremap  k
nnoremap  l
xnoremap <silent>  :call multiple_cursors#new("v", 0)
nnoremap <silent>  :call multiple_cursors#new("n", 1)
snoremap  "_c
nnoremap  \t :botright vertical terminal
vnoremap   zf
nnoremap <silent>   @=(foldlevel('.')?'za':"\ ")
nnoremap + :vertical resize +5
nnoremap - :resize -5
nnoremap = :resize +5
nnoremap _ :vertical resize -5
vmap gx <Plug>NetrwBrowseXVis
nmap gx <Plug>NetrwBrowseX
nmap gcu <Plug>Commentary<Plug>Commentary
nmap gcc <Plug>CommentaryLine
omap gc <Plug>Commentary
nmap gc <Plug>Commentary
xmap gc <Plug>Commentary
xnoremap <silent> g<M-n> :call multiple_cursors#select_all("v", 0)
xnoremap <silent> gî :call multiple_cursors#select_all("v", 0)
nnoremap <silent> g<M-n> :call multiple_cursors#select_all("n", 0)
nnoremap <silent> gî :call multiple_cursors#select_all("n", 0)
xnoremap <silent> g<C-N> :call multiple_cursors#new("v", 0)
xnoremap <silent> g :call multiple_cursors#new("v", 0)
nnoremap <silent> g<C-N> :call multiple_cursors#new("n", 0)
nnoremap <silent> g :call multiple_cursors#new("n", 0)
nnoremap <SNR>94_: :=v:count ? v:count : ''
xmap <C-H> :NERDTreeToggle
nmap <C-H> :NERDTreeToggle
nnoremap <SNR>93_: :=v:count ? v:count : ''
vnoremap <silent> <Plug>NetrwBrowseXVis :call netrw#BrowseXVis()
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#BrowseX(expand((exists("g:netrw_gx")? g:netrw_gx : '<cfile>')),netrw#CheckIfRemote())
onoremap <silent> <Plug>(fzf-maps-o) :call fzf#vim#maps('o', 0)
xnoremap <silent> <Plug>(fzf-maps-x) :call fzf#vim#maps('x', 0)
nnoremap <silent> <Plug>(fzf-maps-n) :call fzf#vim#maps('n', 0)
tnoremap <silent> <Plug>(fzf-normal) 
tnoremap <silent> <Plug>(fzf-insert) i
nnoremap <silent> <Plug>(fzf-normal) <Nop>
nnoremap <silent> <Plug>(fzf-insert) i
nmap <silent> <Plug>CommentaryUndo :echoerr "Change your <Plug>CommentaryUndo map to <Plug>Commentary<Plug>Commentary"
nnoremap <silent> <Plug>(Tman) :call man#get_page_from_cword('tab',        v:count)
nnoremap <silent> <Plug>(Vman) :call man#get_page_from_cword('vertical',   v:count)
nnoremap <silent> <Plug>(Sman) :call man#get_page_from_cword('horizontal', v:count)
nnoremap <silent> <Plug>(Man) :call man#get_page_from_cword('horizontal', v:count)
vmap <C-J> <Plug>IMAP_JumpForward
vnoremap <silent> <Plug>IMAP_JumpBack `<:call IMAP_Jumpfunc('b', 0)
vnoremap <silent> <Plug>IMAP_JumpForward :call IMAP_Jumpfunc('', 0)
vnoremap <silent> <Plug>IMAP_DeleteAndJumpBack "_<Del>:call IMAP_Jumpfunc('b', 0)
vnoremap <silent> <Plug>IMAP_DeleteAndJumpForward "_<Del>:call IMAP_Jumpfunc('', 0)
nnoremap <silent> <Plug>IMAP_JumpBack :call IMAP_Jumpfunc('b', 0)
nnoremap <silent> <Plug>IMAP_JumpForward :call IMAP_Jumpfunc('', 0)
snoremap <C-R> "_c
snoremap <silent> <C-H> "_c
snoremap <silent> <Del> "_c
snoremap <silent> <BS> "_c
snoremap <silent> <C-Tab> :call UltiSnips#ListSnippets()
xnoremap <silent> <M-n> :call multiple_cursors#select_all("v", 0)
nnoremap <silent> <M-n> :call multiple_cursors#select_all("n", 1)
xnoremap <silent> <C-N> :call multiple_cursors#new("v", 0)
nnoremap <silent> <C-N> :call multiple_cursors#new("n", 1)
nnoremap <C-L> l
omap <C-H> :NERDTreeToggle
nnoremap <C-K> k
nmap <C-J> <Plug>IMAP_JumpForward
noremap <F5> :set list!
inoremap <silent> 	 =UltiSnips#ExpandSnippetOrJump()
imap <NL> <Plug>IMAP_JumpForward
inoremap  u[s1z=`]a
imap ;; 
xnoremap <silent> î :call multiple_cursors#select_all("v", 0)
nnoremap <silent> î :call multiple_cursors#select_all("n", 1)
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set backspace=0
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set helplang=en
set hidden
set incsearch
set laststatus=2
set path=.,/usr/include,,,**
set printoptions=paper:letter
set ruler
set runtimepath=~/.vim,~/.vim/plugged/papercolor-theme,~/.vim/plugged/vim-multiple-cursors,~/.vim/plugged/nord-vim,~/.vim/plugged/spaceduck,~/.vim/plugged/ultisnips,~/.vim/plugged/vimtex,~/.vim/plugged/tex-conceal.vim,~/.vim/plugged/vim-airline-themes,~/.vim/plugged/vim-latex,~/.vim/plugged/gruvbox,~/.vim/plugged/vim-github-colorscheme,~/.vim/plugged/nerdtree,~/.vim/plugged/vim-man,~/.vim/plugged/vim-airline,~/.vim/plugged/vim-fugitive,~/.vim/plugged/vim-commentary,~/.vim/plugged/hardmode,~/.vim/plugged/fzf,~/.vim/plugged/fzf.vim,/var/lib/vim/addons,/etc/vim,/usr/share/vim/vimfiles,/usr/share/vim/vim81,/usr/share/vim/vimfiles/after,/etc/vim/after,/var/lib/vim/addons/after,~/.vim/plugged/ultisnips/after,~/.vim/plugged/vimtex/after,~/.vim/plugged/tex-conceal.vim/after,~/.vim/after
set scrolloff=999
set shiftwidth=4
set smartcase
set smartindent
set spelllang=en_us
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set noswapfile
set tabstop=4
set tags=./tags,tags;/home/jeff-severino
set termguicolors
set undodir=~/dotfiles/vim/undodir
set undofile
set wildmenu
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/SWIRL
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
argglobal
%argdel
edit SourceFiles/FortranFiles/main.f90
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
let s:cpo_save=&cpo
set cpo&vim
inoremap <buffer> <PageDown> :call HardModeEcho(g:HardMode_hardmodeMsg)
inoremap <buffer> <PageUp> :call HardModeEcho(g:HardMode_hardmodeMsg)
inoremap <buffer> <Down> :call HardModeEcho(g:HardMode_hardmodeMsg)
inoremap <buffer> <Up> :call HardModeEcho(g:HardMode_hardmodeMsg)
inoremap <buffer> <Right> :call HardModeEcho(g:HardMode_hardmodeMsg)
inoremap <buffer> <Left> :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> + :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> + :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> - :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> - :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> gj :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> gk :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> gk :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> gj :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> h :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> h :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> j :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> j :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> k :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> k :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> l :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> l :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> <PageDown> :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> <PageUp> :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> <Down> :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> <Up> :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> <Right> :call HardModeEcho(g:HardMode_hardmodeMsg)
vnoremap <buffer> <Left> :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> <PageDown> :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> <PageUp> :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> <Down> :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> <Up> :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> <Right> :call HardModeEcho(g:HardMode_hardmodeMsg)
nnoremap <buffer> <Left> :call HardModeEcho(g:HardMode_hardmodeMsg)
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
set colorcolumn=80
setlocal colorcolumn=80
setlocal comments=:!
setlocal commentstring=!%s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
set conceallevel=1
setlocal conceallevel=1
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal cursorlineopt=both
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'fortran'
setlocal filetype=fortran
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
set foldmethod=indent
setlocal foldmethod=indent
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=cqt
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=^\\c#\\=\\s*include\\s\\+
setlocal includeexpr=
setlocal indentexpr=FortranGetFreeIndent()
setlocal indentkeys=0{,0},0),0],:,0#,!^F,o,O,e,=~end,=~case,=~if,=~else,=~do,=~where,=~elsewhere,=~select,=~endif,=~enddo,=~endwhere,=~endselect,=~elseif,=~type,=~interface,=~forall,=~associate,=~block,=~enum,=~endforall,=~endassociate,=~endblock,=~endenum
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=4
setlocal noshortname
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal smartindent
setlocal softtabstop=0
setlocal spell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en_us
setlocal statusline=%!airline#statusline(1)
setlocal suffixesadd=.f08,.f03,.f95,.f90,.for,.f,.F,.f77,.ftn,.fpp
setlocal noswapfile
setlocal synmaxcol=3000
if &syntax != 'fortran'
setlocal syntax=fortran
endif
setlocal tabstop=4
setlocal tagcase=
setlocal tagfunc=
setlocal tags=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=132
setlocal thesaurus=
setlocal undofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal wincolor=
setlocal nowinfixheight
setlocal nowinfixwidth
set nowrap
setlocal nowrap
setlocal wrapmargin=0
2
normal! zo
14
normal! zo
286
normal! zo
311
normal! zo
352
normal! zo
391
normal! zo
479
normal! zo
509
normal! zo
let s:l = 300 - ((29 * winheight(0) + 29) / 59)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
300
normal! 0
tabnext 1
badd +0 SourceFiles/FortranFiles/main.f90
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOS
set winminheight=1 winminwidth=1
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :

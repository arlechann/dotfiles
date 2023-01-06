" -----Plugin-----
" プラグインを読み込む場合はここに記述する
" Plugin Maneger dein.vim
let s:dein_dir = expand('~/.vim/dein')
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

if &runtimepath !~# '/dein.vim'
	if !isdirectory(s:dein_repo_dir)
		execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
	endif
	execute 'set runtimepath^=' . fnamemodify(s:dein_repo_dir, ':p')
endif

if dein#load_state(s:dein_dir)
	call dein#begin(s:dein_dir)

	let s:toml = '~/.dein.toml'
	let s:lazy_toml = '~/.dein_lazy.toml'

	call dein#load_toml(s:toml, {'lazy': 0})
	call dein#load_toml(s:lazy_toml, {'lazy': 1})

	call dein#end()
	call dein#save_state()
endif

if dein#check_install()
	call dein#install()
endif

" -----基本設定-----
" タイトルをバッファ名に変更しない
set notitle
set shortmess+=I

" ターミナル接続を高速にする
set ttyfast

" ターミナルで256色表示を使う
set t_Co=256

" vi互換にしない
set nocompatible

" カラースキーマの設定
colorscheme desert

" 文字コード
if has("unix") || has("win32unix")
	set encoding=utf-8
	set fileencoding=utf-8
	set termencoding=utf-8
elseif has("win32") || has("win64")
	set encoding=sjis
	set fileencodings=sjis,utf-8,euc-jp,iso-2022-jp,cp932
endif
" <ESC>を押してから挿入モードを出るまでの時間を短くする
set ttimeoutlen=100

" 日本語入力をリセット
au BufNewFile,BufRead * set iminsert=0

" -----画面表示の設定-----
syntax enable		" シンタックスを有効化
set number			" 行番号を表示する

if version > 702	" Vim7.2より古ければ読まない
	set relativenumber	" 相対行番号表示
endif

set ruler			" 座標を表示する
set cursorline		" カーソル行の背景色を変える
set nocursorcolumn	"カーソル位置のカラムの背景色を変えない
set laststatus=2	" ステータス行を常に表示
" ステータス行に文字コード，改行コードを表示する
set statusline=%<%f\ %y%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}\ %r%h%w\ %m\ %=\ (%v,%l)/%L%8P\ 
"set statusline=%<%f\ %m\ %r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=\ (%v,%l)/%L%8P\ 
set cmdheight=1		" メッセージ表示欄を1行確保
set showcmd			" 入力中のコマンドを表示する
set showmatch		" 対応する括弧を強調表示
set matchtime=3		" showmatchの表示時間(s)
"set helpheight=999 " ヘルプを画面いっぱいに開く
set list			" 不可視文字を表示
set listchars=tab:▸\ ,eol:↲,extends:❯,precedes:❮	" 不可視文字の表示記号指定

" 全角スペースを赤色にする
augroup highlightZenkakuSpace
	autocmd!
	autocmd VimEnter,ColorScheme * highlight ZenkakuSpace term=underline ctermbg=Red guibg=Red
	autocmd VimEnter,WinEnter * match ZenkakuSpace /　/
augroup END

" -----カーソル移動関連の設定-----
set backspace=indent,eol,start	" Backspaceキーの影響範囲に制限を設けない
set whichwrap=b,s,h,l,<,>,[,]	" 行頭行末の左右移動で行をまたぐ
set scrolloff=8					" 上下8行の視界を確保
set sidescrolloff=16			" 左右スクロール時の視界を確保
set sidescroll=1				" 左右スクロールは一文字づつ行う

" ノーマルモードのキーバインド
" カーソルを表示行で移動する
nnoremap j gj
nnoremap k gk
nnoremap <down> gj
nnoremap <up> gk
" 絶対行と相対行の切り替え
"if version > 702	" Vim7.2より古ければ読まない
"	nnoremap <F3> :<C-u>setlocal relativenumber!<CR>
"endif

" 挿入モードのキーバインド
" 挿入モード中に'Ctr-*'でコマンドモードでの移動を可能にする
" 右移動
inoremap <C-l> <right>
" 左移動
inoremap <C-h> <left>
" 上移動
inoremap <C-k> <up>
" 下移動
inoremap <C-j> <down>

" -----ファイル処理関連の設定-----
set confirm			" 保存されていないファイルがあるときは終了前に保存確認
set hidden			" 保存されていないファイルがあるときでも別のファイルを開くことが出来る
set autoread		" 外部でファイルに変更がされた場合は読みなおす
set nobackup		" ファイル保存時にバックアップファイルを作らない
set updatetime=0	" Swapを作るまでの時間m秒
set noswapfile		" ファイル編集中にスワップファイルを作らない

" -----検索/置換の設定-----
set hlsearch	" 検索文字列をハイライトする
set noincsearch	" インクリメンタルサーチを行わない
set ignorecase	" 大文字と小文字を区別しない
set smartcase	" 大文字と小文字が混在した言葉で検索を行った場合に限り、大文字と小文字を区別する
set wrapscan	" 最後尾まで検索を終えたら次の検索で先頭に移る
set gdefault	" 置換の時 g オプションをデフォルトで有効にする

" -----タブ/インデントの設定-----
"set cindent		" Cのインデント
set noexpandtab		" タブ入力を複数の空白入力に置き換えない
set tabstop=4		" 画面上でタブ文字が占める幅
set shiftwidth=4	" 自動インデントでずれる幅
set softtabstop=4	" 連続した空白に対してタブキーやバックスペースキーでカーソルが動く幅
set autoindent		" 改行時に前の行のインデントを継続する
set smartindent		" 改行時に入力された行の末尾に合わせて次の行のインデントを増減する

" -----動作環境と統合関連の設定-----
" OSのクリップボードをレジスタ指定無しで Yank, Put 出来るようにする
set clipboard=unnamed,unnamedplus
" マウスの入力を受け付ける
set mouse=a
" Windows でもパスの区切り文字を / にする
"set shellslash

" -----コマンドラインの設定-----
" コマンドラインモードでTABキーによるファイル名補完を有効にする
set wildmenu wildmode=list:longest,full
" コマンドラインの履歴を10000件保存する
set history=10000

" -----ビープの設定-----
" ビープ音すべてを無効にする
set visualbell t_vb=
set noerrorbells " エラーメッセージの表示時にビープを鳴らさない

" -----日本語環境の設定-----
set formatoptions+=mM	" 日本語の行の連結時には空白を挿入しない
set ambiwidth=double	" □や○の文字があってもカーソル位置がずれないようにする
set display+=lastline	" 最後の行を出来る限り表示する

" ノーマルモードでIMEをオフにする
if has("unix")
	function! Fcitx2en()
		let s:input_status = system("fcitx-remote")
		if s:input_status == 2
			let l:a = system("fcitx-remote -c")
		endif
	endfunction
	autocmd InsertLeave * call Fcitx2en()
elseif has("win32") || has("win64")
endif

" -----filetypeごとの設定-----
"filetype plugin indent on
" C++
function! s:cpp()
	if filereadable(expand($HOME . '/.vimrc_local_cpp'))
		source $HOME/.vimrc_local_cpp
	endif

	setlocal matchpairs+=<:>
endfunction

augroup vimrc-cpp
	autocmd!
	autocmd FileType cpp call s:cpp()
augroup END

" Scheme
autocmd BufRead,BufNewfile *.scm setfiletype scheme

function! s:scheme()
	setlocal expandtab
	setlocal tabstop=2
	setlocal shiftwidth=2
	setlocal softtabstop=2
	let lisp_rainbow = 1
	let g:paredit_mode = 0
	let g:paredit_electric_return = 0
endfunction

augroup vimrc-scheme
	autocmd!
	autocmd FileType scheme call s:scheme()
augroup END

" Common Lisp
autocmd BufRead,BufNewfile *.lisp setfiletype lisp
autocmd BufRead,BufNewfile *.asd setfiletype lisp
autocmd BufRead,BufNewfile *.ros setfiletype lisp

function! s:lisp()
	if filereadable(expand($HOME . '/.vimrc_lisp'))
		source $HOME/.vimrc_lisp
	endif
endfunction

augroup vimrc-lisp
	autocmd!
	autocmd FileType lisp call s:lisp()
augroup END

" Scheme
autocmd BufRead,BufNewfile *.scm setfiletype scheme

function! s:scheme()
	setlocal expandtab
	setlocal tabstop=2
	setlocal shiftwidth=2
	setlocal softtabstop=2
	if filereadable(expand($HOME . '/.vimrc_lisp'))
		source $HOME/.vimrc_lisp
	endif
endfunction

augroup vimrc-scheme
	autocmd!
	autocmd FileType scheme call s:scheme()
augroup END

" PHP
function! s:php()
	setlocal expandtab
	setlocal tabstop=4
	setlocal shiftwidth=4
	setlocal softtabstop=4
endfunction

augroup vimrc-php
	autocmd!
	autocmd FileType php call s:php()
augroup END

" Assembler
function! s:asm()
	setlocal tabstop=8
	setlocal shiftwidth=8
	setlocal softtabstop=8
endfunction

augroup vimrc-asm
	autocmd!
	autocmd FileType asm call s:asm()
augroup END

" html
function! s:html()
	setlocal expandtab
	setlocal tabstop=2
	setlocal shiftwidth=2
	setlocal softtabstop=2
endfunction

augroup vimrc-html
	autocmd!
	autocmd FileType html,htmldjango,eruby call s:html()
augroup END

" css
function! s:css()
	setlocal expandtab
	setlocal tabstop=2
	setlocal shiftwidth=2
	setlocal softtabstop=2
endfunction

augroup vimrc-css
	autocmd!
	autocmd FileType css call s:css()
augroup END

" JavaScript
function! s:js()
	setlocal expandtab
	setlocal tabstop=2
	setlocal shiftwidth=2
	setlocal softtabstop=2
endfunction

augroup vimrc-js
	autocmd!
	autocmd FileType javascript call s:js()
augroup END

" Markdown
function! s:markdown()
	setlocal expandtab
	setlocal tabstop=2
	setlocal shiftwidth=2
	setlocal softtabstop=2
endfunction

augroup vimrc-markdown
	autocmd!
	autocmd FileType markdown call s:markdown()
augroup END

" Ruby
function! s:ruby()
	setlocal expandtab
	setlocal tabstop=2
	setlocal shiftwidth=2
	setlocal softtabstop=2
endfunction

augroup vimrc-ruby
	autocmd!
	autocmd FileType ruby call s:ruby()
augroup END

" yaml
function! s:yaml()
	setlocal expandtab
	setlocal tabstop=2
	setlocal shiftwidth=2
	setlocal softtabstop=2
endfunction

augroup vimrc-yaml
	autocmd!
	autocmd FileType yaml call s:yaml()
augroup END

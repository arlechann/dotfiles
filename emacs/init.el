;; launguage
(set-language-environment 'Japanese)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

(global-display-line-numbers-mode) ; show line number
(show-paren-mode 1) ; hilight match parenthesis
(setq scroll-conservatively 1) ; scroll a line
(setq transient-mark-mode t) ; show mark

;; keybind
(keyboard-translate ?\C-h ?\C-?) ; use C-h as backspace

;; lisp
(add-to-list 'auto-mode-alist '("\\.lsp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))

(load (expand-file-name "~/.roswell/helper.el"))
(setq inferior-lisp-program "ros -Q run")

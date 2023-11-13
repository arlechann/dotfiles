;; leaf install code
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

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

(let ((slime-helper "~/.roswell/helper.el"))
  (if (file-exists-p slime-helper)
      (load (expand-file-name slime-helper))))
(setq inferior-lisp-program "ros -Q run")


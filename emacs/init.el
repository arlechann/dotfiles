;; launguage
(set-language-environment 'Japanese)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

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

;; packages
(leaf custom-file-path
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf custom-set-value
  :preface (defun c/redraw-frame ()
	     (interactive)
	     (redraw-frame))
  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom ((create-lockfiles . nil)
	   (frame-resize-pixelwise . t)
	   (enable-recursive-minibuffers . t)
	   (history-length . 1000)
	   (history-delete-duplicates . t)
	   (scroll-preserve-screen-position . nil)
	   (scroll-conservatively . 1) ; scroll a line
	   (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
	   (ring-bell-funcation . 'ignore) ; no bell
	   (menu-bar-mode . t) ; show menu bar on gui
	   (tool-bar-mode . nil) ; hide tool bar on gui
	   (scroll-bar-mode . t) ; use scroll bar on gui
	   (indent-tabs-mode . nil) ; use spaces for indent
           (make-backup-files . nil)
           (auto-save-default . nil) ; no auto save
           (transient-mark-mode . t) ; show mark
           (native-comp-async-report-warnings-errors . nil) ; supress warnings on gcc emacs
           ))

(leaf key-binds
  :config (keyboard-translate ?\C-h ?\C-?)) ; use C-h as backspace

(leaf display-line-numbers
  :global-minor-mode global-display-line-numbers-mode)

(leaf show-paren
  :global-minor-mode show-paren-mode)

(leaf font-setting
  :config (setq default-frame-alist
                (append (list '(font . "HackGen Console NF-11")
        	              default-frame-alist))))

(leaf solarized-theme
  :ensure t
  :when window-system
  :config (load-theme 'solarized-dark t))

(leaf company
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
	  ("M-n" . nil)
	  ("M-p" . nil)
	  ("C-s" . company-filter-candidates)
	  ("C-n" . company-select-next)
	  ("C-p" . company-select-previous)
	  ("<tab>" . company-complete-selection))
	 (company-search-map
	  ("C-n" . company-select-next)
	  ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
	   (company-minimux-prefix-length . 1)
	   (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf lisp-mode*
  :config
  (add-to-list 'auto-mode-alist '("\\.lsp$" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode)) 
  (let ((roswell-helper-path "~/.roswell/helper.el"))
    (leaf slime*
      :config
      (leaf slime
        :when (not (file-exists-p roswell-helper-path))
        :ensure t
        :custom ((inferior-lisp-program . "sbcl")))
      (leaf roswell-slime
        :when (file-exists-p roswell-helper-path)
        :init (load (expand-file-name roswell-helper-path))
        :custom ((inferior-lisp-program . "ros -Q run"))))))
  

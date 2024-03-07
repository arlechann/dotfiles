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
  :when nil
  :when window-system
  :config (load-theme 'solarized-dark t))

(leaf doom*
  :when window-system
  :config
  (leaf doom-themes
    :ensure t
    :custom '((doom-themes-enable-bold . t)
              (doom-themes-enable-italic . t))
    :config
    (load-theme 'doom-dracula t)
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (doom-themes-org-config))
  (leaf doom-modeline
    :custom '((doom-modeline-buffer-file-name-style . 'truncate-with-project)
              (doom-modeline-icon . t)
              (doom-modeline-major-mode-icon . nil)
              (doom-modeline-minor-modes . nil))
    :hook '((after-init . doom-modeline-mode))
    :config
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline
     'main
     '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
     '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))))

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
  (add-to-list 'auto-mode-alist '("\\.lsp\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
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

(leaf scheme-mode*
  :custom '((scheme-program-name . "gosh -i")))

;; (leaf web-mode
;;   :ensure t
;;   :mode (("\\.html\\'" . web-mode)
;;          ("\\.css\\'" . web-mode)
;;          ("\\.[jt]sx?\\'" . web-mode))
;;   :custom ((web-mode-enable-auto-paring . t)
;;            (web-mode-enable-css-colorization . t)
;;            (web-mode-enable-current-element-highlight . t)
;;            (web-mode-enable-current-column-highlight . t))
;;   :hook ((local-write-file-hooks . (lambda () (delete-trailing-whitespace) nil))))

;; (leaf lsp-mode
;;   :if nil
;;   :ensure t
;;   :config (lsp lsp-deferred)
;;   :hook ((web-mode . #'lsp)))

(defun my/move-beginning-of-line (&optional position)
  (interactive)
  (let ((position (or position (point)))
        (beginning-of-code (progn (back-to-indentation) (point))))
    (if (eql position beginning-of-code)
        (move-beginning-of-line 1))))

(global-set-key "\C-a" 'my/move-beginning-of-line)



;; launguage
(set-language-environment 'Japanese)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-coding-system-priority 'utf-8 'cp932)

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
(leaf custom-file
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  :config
  (load custom-file 'noerror))

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
           (ring-bell-function . 'ignore) ; no bell
           (menu-bar-mode . t) ; show menu bar on gui
           (tool-bar-mode . nil) ; hide tool bar on gui
           (scroll-bar-mode . t) ; use scroll bar on gui
           (indent-tabs-mode . nil) ; use spaces for indent
           (make-backup-files . nil)
           (auto-save-default . nil) ; no auto save
           (transient-mark-mode . t) ; show mark
           (native-comp-async-report-warnings-errors . nil) ; supress warnings on gcc emacs
           (global-auto-revert-mode . t)
           ))

(leaf key-binds
  :config (keyboard-translate ?\C-h ?\C-?)) ; use C-h as backspace

(leaf display-line-numbers
  :global-minor-mode global-display-line-numbers-mode)

(leaf show-paren
  :global-minor-mode show-paren-mode)

(leaf rainbow-delimiters
  :ensure t
  :hook ((prog-mode-hook . rainbow-delimiters-mode)))

(leaf font-setting
  :config (setq default-frame-alist
                (append (list '(font . "HackGen Console NF-11"))
                        default-frame-alist)))

(leaf solarized-theme
  :ensure t
  :unless window-system
  :config (load-theme 'solarized-dark t))

(leaf doom*
  :when window-system
  :config
  (leaf doom-themes
    :ensure t
    :custom ((doom-themes-enable-bold . t)
             (doom-themes-enable-italic . t))
    :config
    (load-theme 'doom-dracula t)
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (doom-themes-org-config))
  (leaf doom-modeline
    :ensure t
    :custom ((doom-modeline-buffer-file-name-style . 'truncate-with-project)
             (doom-modeline-icon . t)
             (doom-modeline-major-mode-icon . nil)
             (doom-modeline-minor-modes . nil))
    :hook ((after-init . doom-modeline-mode))
    :config
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline
     'main
     '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
     '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))))

(leaf shell
  :when (eq system-type 'windows-nt)
  :custom ((explicit-shell-file-name . "wsl.exe")
           (shell-file-name . "wsl.exe"))
  :hook ((shell-mode-hook . (lambda ()
                              (set-buffer-file-coding-system 'unix)
                              (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))
  :config
  (setenv "SHELL" shell-file-name)
  (defvar explicit-wsl.exe-args nil)
  (defvar my/original-shell-command (symbol-function 'shell))
  (defun my/shell-wsl (&optional buffer)
    (interactive)
    (let ((explicit-wsl.exe-args
           (list "--cd" (expand-file-name default-directory) "bash" "-i")))
      (funcall-interactively my/original-shell-command
                             (or buffer "*wsl-shell*"))))
  (defalias 'shell #'my/shell-wsl))

(leaf magit
  :ensure t
  :commands (magit-status magit-dispatch)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)))

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
     (company-minimum-prefix-length . 1)
     (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf lisp-mode*
  :config
  (add-to-list 'auto-mode-alist '("\\.lsp\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
  (let ((slime-quicklisp-helper-path (expand-file-name "~/quicklisp/slime-helper.el"))
        (roswell-helper-path (expand-file-name "~/.roswell/helper.el")))
    (leaf slime
      :when (not (file-exists-p roswell-helper-path))
      :ensure t
      :custom ((inferior-lisp-program . "sbcl"))
      :config
      (leaf quicklisp-slime-helper
        :when (file-readable-p slime-quicklisp-helper-path)
        :init (load slime-quicklisp-helper-path)))
    (leaf roswell-slime
      :when (file-exists-p roswell-helper-path)
      :init (load (expand-file-name roswell-helper-path))
      :custom ((inferior-lisp-program . "ros -Q run")))))

(leaf scheme-mode*
  :custom ((scheme-program-name . "gosh -i")))

(defvar treesit-language-source-alist nil)

(defun my/install-js-ts-treesit-grammars ()
  (interactive)
  (dolist (language '(javascript jsx typescript tsx))
    (treesit-install-language-grammar language)))

(defun my/install-html-treesit-grammars ()
  (interactive)
  (dolist (language '(html))
    (treesit-install-language-grammar language)))

(defun my/install-css-treesit-grammars ()
  (interactive)
  (dolist (language '(css))
    (treesit-install-language-grammar language)))

(defun my/install-json-treesit-grammars ()
  (interactive)
  (dolist (language '(json))
    (treesit-install-language-grammar language)))

(defun my/install-svg-treesit-grammars ()
  (interactive)
  (dolist (language '(xml))
    (treesit-install-language-grammar language)))

(defun my/install-all-treesit-grammars ()
  (interactive)
  (my/install-js-ts-treesit-grammars)
  (my/install-html-treesit-grammars)
  (my/install-css-treesit-grammars)
  (my/install-json-treesit-grammars)
  (my/install-svg-treesit-grammars))

(leaf js-ts-mode*
  :preface
  (dolist (spec '(("\\.js\\'" . js-ts-mode)
                  ("\\.jsx\\'" . js-ts-mode)
                  ("\\.mjs\\'" . js-ts-mode)
                  ("\\.cjs\\'" . js-ts-mode)
                  ("\\.ts\\'" . typescript-ts-mode)
                  ("\\.tsx\\'" . tsx-ts-mode)))
    (add-to-list 'auto-mode-alist spec))
  :config
  (dolist (source '((javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
                    (jsx . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
                    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
                    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))
    (add-to-list 'treesit-language-source-alist source)))

(leaf html-ts-mode*
  :preface
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
  :config
  (add-to-list 'treesit-language-source-alist
               '(html . ("https://github.com/tree-sitter/tree-sitter-html" "master" "src"))))

(leaf css-ts-mode*
  :preface
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
  :config
  (add-to-list 'treesit-language-source-alist
               '(css . ("https://github.com/tree-sitter/tree-sitter-css" "master" "src"))))

(leaf json-ts-mode*
  :preface
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  :config
  (add-to-list 'treesit-language-source-alist
               '(json . ("https://github.com/tree-sitter/tree-sitter-json" "master" "src"))))

(leaf svg-mode*
  :preface
  (add-to-list 'auto-mode-alist '("\\.svg\\'" . nxml-mode))
  :config
  (add-to-list 'treesit-language-source-alist
               '(xml . ("https://github.com/tree-sitter-grammars/tree-sitter-xml" "master" "xml/src"))))

(defun my/node-version-root ()
  (or (locate-dominating-file default-directory ".nvmrc")
      (locate-dominating-file default-directory ".node-version")))

(defun my/activate-node-version-environment ()
  (when-let ((project-root (my/node-version-root))
             (shell (or (executable-find "bash") shell-file-name)))
    (with-temp-buffer
      (let ((default-directory project-root))
        (when (eq 0 (process-file
                     shell nil (current-buffer) nil "-lc"
                     (mapconcat
                      #'identity
                      '("if [ -z \"${NVM_DIR:-}\" ]; then NVM_DIR=\"$HOME/.nvm\"; fi"
                        "if [ -s \"$NVM_DIR/nvm.sh\" ]; then . \"$NVM_DIR/nvm.sh\"; fi"
                        "if command -v fnm >/dev/null 2>&1; then eval \"$(fnm env --shell bash)\"; fnm use --silent-if-unchanged >/dev/null 2>&1 || fnm use >/dev/null 2>&1 || true; fi"
                        "if command -v nvm >/dev/null 2>&1; then nvm use --silent >/dev/null 2>&1 || nvm use >/dev/null 2>&1 || true; fi"
                        "printf %s \"$PATH\"")
                      "; ")))
          (let ((path (string-trim (buffer-string))))
            (setq exec-path (parse-colon-path path))
            (setenv "PATH" path)
            t))))))

(defun my/node-executable (name)
  (my/activate-node-version-environment)
  (let* ((project-root (or (locate-dominating-file default-directory "node_modules")
                           (locate-dominating-file default-directory "package.json")))
         (bin-directory (and project-root
                             (expand-file-name "node_modules/.bin/" project-root)))
         (suffix (if (eq system-type 'windows-nt) ".cmd" ""))
         (local-path (and bin-directory
                          (expand-file-name (concat name suffix) bin-directory))))
    (if (and local-path (file-executable-p local-path))
        local-path
        (or (executable-find name)
            (and (not (string-empty-p suffix))
                 (executable-find (concat name suffix)))))))

(leaf eglot
  :preface
  (defun my/eglot-js-ts-contact (&optional _interactive)
    (list (or (my/node-executable "typescript-language-server")
              "typescript-language-server")
          "--stdio"))
  (defun my/eglot-js-ts-setup ()
    (setq-local eglot-stay-out-of '(flymake))
    (when-let ((eslint (my/node-executable "eslint")))
      (setq-local flycheck-javascript-eslint-executable eslint))
    (setq-local flycheck-checker 'javascript-eslint)
    (eglot-ensure)
    (when (fboundp 'apheleia-mode)
      (apheleia-mode 1))
    (when (fboundp 'flycheck-mode)
      (flycheck-mode 1)))
  :hook ((js-ts-mode-hook . my/eglot-js-ts-setup)
         (typescript-ts-mode-hook . my/eglot-js-ts-setup)
         (tsx-ts-mode-hook . my/eglot-js-ts-setup))
  :config
  (add-to-list 'eglot-server-programs
               '((js-ts-mode typescript-ts-mode tsx-ts-mode) . my/eglot-js-ts-contact)))

(leaf apheleia
  :ensure t
  :preface
  (defun my/apheleia-js-ts-prettier ()
    (or (my/node-executable "prettier")
        "prettier"))
  :config
  (when (boundp 'apheleia-formatters)
    (setf (alist-get 'prettier apheleia-formatters)
          '(my/apheleia-js-ts-prettier "--stdin-filepath" filepath)))
  (when (boundp 'apheleia-mode-alist)
    (dolist (mode '(js-ts-mode typescript-ts-mode tsx-ts-mode))
      (setf (alist-get mode apheleia-mode-alist) 'prettier))))

(leaf flycheck
  :ensure t
  :hook ((js-ts-mode-hook . flycheck-mode)
         (typescript-ts-mode-hook . flycheck-mode)
         (tsx-ts-mode-hook . flycheck-mode))
  :config
  (when (fboundp 'flycheck-add-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
    (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)))

(defun my/move-beginning-of-line (&optional position)
  (interactive)
  (let ((position (or position (point)))
        (beginning-of-code (progn (back-to-indentation) (point))))
    (if (eql position beginning-of-code)
        (move-beginning-of-line 1))))

(global-set-key "\C-a" 'my/move-beginning-of-line)

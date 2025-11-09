;; Note that init.el is generated from ./Emacs.org - that is the file that should be edited
;; on first install, do M-x all-the-icons-install-fonts, M-x treesit-install-language-grammar
;; and M-x pdf-tools-install

;; --- Elpaca Bootstrap ---
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                             :ref nil :depth 1 :inherit ignore
                             :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                             :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                      ,@(when-let* ((depth (plist-get order :depth)))
                                                          (list (format "--depth=%d" depth) "--no-single-branch"))
                                                      ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; --- Elpaca use-package Integration ---

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for elpaca.
  (elpaca-use-package-mode)
  ;; Make elpaca manage all use-package declarations by default.
  (setq elpaca-use-package-by-default t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(x mac ns))
    (exec-path-from-shell-initialize)))

(use-package emacs
   
  :ensure nil ;; This is a built-in package
  :init
  ;; Settings to apply very early, before config is fully loaded
  (setq inhibit-startup-message t)

  :config
  ;; --- Basic UI Tweaks ---
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (setq visible-bell t)

  ;; Disable line numbers in modes where they don't make sense
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
                  eww-mode-hook
                  pdf-view-mode-hook)) 
    (add-hook mode (lambda () (display-line-numbers-mode 0)))) 

  ;; --- GUI Specific Tweaks ---
  (when (display-graphic-p) 
    (add-to-list 'default-frame-alist '(alpha-background . 95))
    (add-to-list 'default-frame-alist '(fullscreen . maximized)) 
    (scroll-bar-mode -1) 
    (tool-bar-mode -1) 
    (tooltip-mode -1) 
    (menu-bar-mode -1) 
    (set-fringe-mode 5)) 

  ;; --- Font Configuration ---
  (cond
   ((find-font (font-spec :name "FiraCode Nerd Font"))
    (message "Using FiraCode Nerd Font")
    (set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 130) 
    (set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height 130)) 
   ((find-font (font-spec :name "Consolas"))
    (message "Using Consolas as a fallback")
    (set-face-attribute 'default nil :font "Consolas" :height 130) 
    (set-face-attribute 'fixed-pitch nil :font "Consolas" :height 130))
   (t
    (message "Neither FiraCode nor Consolas found, using emacs default.")))

  (cond
   ((find-font (font-spec :name "Cantarell"))
    (message "Using Cantarell for variable-pitch") 
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140 :weight 'regular)) 
   ((find-font (font-spec :name "Segoe UI"))
    (message "Using Segoe UI as a fallback for variable pitch") 
    (set-face-attribute 'variable-pitch nil :font "Segoe UI" :height 140 :weight 'regular)) 
   (t
    (message "Neither Cantarell nor Segoe UI found, using emacs default."))))

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-dracula t))

(use-package all-the-icons :ensure t)
(use-package nerd-icons :ensure t)
(use-package doom-modeline
  :ensure t
  :after (all-the-icons nerd-icons)
  :config
  (require 'all-the-icons) ;;not sure if this is needed
  (if (and (display-graphic-p) (member "all-the-icons" (font-family-list)))
	(progn
	  (doom-modeline-mode 1)
	  (setq doom-modeline-height 15))
        (message "doom-modeline not enabled - either fonts not found or we're in a TTY. M-x all-the-icons-install-fonts")))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.5))

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  ;; Add cycling with TAB
  (setq vertico-cycle t))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  :ensure t
  :bind
  ;; Re-bind the keys we had for counsel
  (("M-x" . execute-extended-command)
   ("C-x b" . consult-buffer) ;; A good replacement for counsel-ibuffer
   ;; C-x C-f (find-file) uses vertico automatically
   :map minibuffer-local-map
   ("C-r" . consult-history)))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode 1))

(use-package helpful
  :ensure t
  :bind
  ;; Remap the default describe-* commands to helpful's versions
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(defun nij/org-font-setup ()
  (font-lock-add-keywords 'org-mode
			  '(("^ *\([-]\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (let ((variable-font
         (cond
          ((find-font (font-spec :name "Cantarell")) "Cantarell")
          ((find-font (font-spec :name "Segoe UI")) "Segoe UI")
          (t nil))))
    (when variable-font
      (dolist (face '((org-level-1 . 1.2)
                      (org-level-2 . 1.1)
                      (org-level-3 . 1.05)
                      (org-level-4 . 1.0)
                      (org-level-5 . 1.0)
                      (org-level-6 . 1.0)
                      (org-level-7 . 1.0)
                      (org-level-8 . 1.0)))
        (set-face-attribute (car face) nil :font variable-font :weight 'regular :height (cdr face)))))

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun nij/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  )

(use-package org
   
  :ensure nil
  :hook (org-mode . nij/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (nij/org-font-setup))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-prettify-item-priority t)
  (setq org-superstar-prettifiers
	(list
	 '(org-superstar-priority-prettifier . org-superstar-prettify-priority)
	 ))
  (setq org-superstar-priority-faces nil)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  )

(let ((babel-languages '((emacs-lisp . t))))
  (when (executable-find "python")
    (push '(python . t) babel-languages))
  (when (executable-find "haskell")
    (push '(haskell . t) babel-languages))
  (org-babel-do-load-languages 'org-babel-load-languages babel-languages))
(push '("conf-unix" . conf-unix) org-src-lang-modes)

(use-package org-tempo
   
  :ensure nil ; It's part of org so won't needed installed
  :after org  ; Ensure org is loaded first
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("hs" . "src haskell")))

(defun nij/org-babel-tangle-config ()
  "Automatically tangle our Emacs.org config file when we save it"
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping example
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nij/org-babel-tangle-config)))

(setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (java "https://github.com/tree-sitter/tree-sitter-java")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (regex "https://github.com/tree-sitter/tree-sitter-regex")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

(defun shell-other-window ()
  "Open a 'shell' buffer in another window"
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf))
  )

(use-package flymake :ensure t)
(use-package jsonrpc :ensure t)
(use-package eglot
  :ensure t
  :hook ((haskell-mode . eglot-ensure)
	 (java-ts-mode . eglot-ensure)
	 (python-mode . eglot-ensure)
	 (js-mode . eglot-ensure)
	 (typescript-mode . eglot-ensure)
	 (c-ts-mode . eglot-ensure)
	 (c++-ts-mode . eglot-ensure)
	 )
  :bind (:map eglot-mode-map
	      ("M-." . xref-find-definitions)
	      ("M-," . xref-pop-marker-stack)
	      ("C-c a" . eglot-code-actions)
	      )
  :config
  (let ((haskell-lsp (expand-file-name "~/.ghcup/bin/haskell-language-server-wrapper")))
    (when (file-exists-p haskell-lsp)
      (add-to-list 'eglot-server-programs
                   `(haskell-mode . (,haskell-lsp "--lsp")))))
  (setq eglot-auto-server-display nil) ;Don't autodisplay server-buffer on restart
  (setq eglot-reconnect-on-change t)
  (setq eglot-autodocument t) ; on mouse-over
  (setq eglot-autodocument-delay 1)
  (setq eglot-display-buffer-function #'eglot-display-buffer-at-bottom)
  )

(defun nij/cmake-configure-preset ()
  "Run 'cmake --preset default' in the project root."
  (interactive)
  (if (executable-find "cmake")
      (let ((compile-command "cmake --preset default")) (compile compile-command))
    (message "Error: 'cmake' not in PATH.")
    ))

(defun nij/c-ts-mode-common-setup ()
  "Common setup for C/C++ tree-sitter modes."
  ;; This replaces 'c-basic-offset' and 'c-set-style'.
  (setq-local treesit-indent-level 4)
    
  ;; Bind keys to the C TS map. 
  ;; c++-ts-mode inherits from c-ts-mode, so it gets these keys too.
  (define-key c-ts-mode-map (kbd "C-c c") #'compile)
  (define-key c-ts-mode-map (kbd "C-c s") #'shell-other-window)
  )
(defun nij/cpp-ts-mode-setup ()
  "C++ specific setup."
  ;; Bind your C++ specific key
  (define-key c++-ts-mode-map (kbd "C-c C-p") #'nij/cmake-configure-preset)
  )
;; Add the hooks for the new modes
(add-hook 'c-ts-mode-hook #'nij/c-ts-mode-common-setup)
(add-hook 'c++-ts-mode-hook #'nij/cpp-ts-mode-setup)

(add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))

(defun nij/java-ts-mode-setup ()
  "Keybinds for java-ts-mode."
  (define-key java-ts-mode-map (kbd "C-c c") #'compile))

(add-hook 'java-ts-mode-hook #'nij/java-ts-mode-setup)

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2))

;; Only load prettier-js if the 'prettier' executable is found
(when (executable-find "prettier")
  (use-package prettier-js
    :ensure t
    :hook ((js-mode . prettier-js-mode)
           (typescript-mode . prettier-js-mode)
           (web-mode . prettier-js-mode))
    :config
    (add-hook 'js-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'prettier-js-before-save nil 'local)))
    (add-hook 'typescript-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'prettier-js-before-save nil 'local)))
    ))
(when (executable-find "node")
  (use-package nodejs-repl
    :ensure t
    :hook (js-mode . (lambda ()
                       (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-run-current-file)))
    :hook (typescript-mode . (lambda ()
                               (define-key typescript-mode-map (kbd "C-c C-j") 'nodejs-repl-run-current-file)))
    ))

(use-package haskell-mode   :ensure nil :defer t)

(use-package corfu
  :ensure t
  :custom
  ;; These mirror our company settings
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  (corfu-cycle t)               ; Allow cycling
  (corfu-popupinfo-delay 0.5)   ; Show extra info in a popup
  :bind
  ;; Binds TAB to 'corfu-complete'.
  ;; This is smarter than company-complete-selection:
  ;; - It indents if at the start of a line.
  ;; - It completes if there's a completion.
  (:map corfu-map
        ("<tab>" . corfu-complete)
        ("TAB" . corfu-complete))
  :config
  (global-corfu-mode 1)) ;; in config so it's not lazy, to avoid race condition with kind-icon

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  ;; This function adds the icons to the corfu popup
  (add-to-list 'corfu-format-functions #'kind-icon-corfu-format))

(use-package projectile
  :ensure t
  ;; :defer t is no longer needed, as :bind-keymap and :init handle it
  :diminish projectile-mode
  :custom ((projectile-completion-system 'default))
  :bind-keymap
   ("C-c p" . projectile-command-map)
  :init
  ;; Enable the global minor mode via its autoload
  (projectile-mode 1)
  
  ;; Set variables before the package is loaded
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package consult-projectile
  :ensure t
  :after (projectile consult)
  :bind (:map projectile-command-map
              ("f" . consult-projectile-find-file)
              ("s" . consult-projectile-switch-project)))

(use-package transient :ensure t)
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)) ;; This implicitly makes it lazy
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (let ((auth-file (expand-file-name "~/.authinfo.gpg")))
    (when (file-exists-p auth-file)
      (use-package forge
        :ensure t
        :after magit 
        :config (setq auth-sources `(,auth-file))))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dired
   
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-alh --group-directories-first")
  	   (wdired-allow-to-change-permissions t))
  :config
  (require 'dired-x))

;; Only load all-the-icons-dired if we are in GUI and fonts are installed
(when (and (display-graphic-p) (member "all-the-icons" (font-family-list)))
  (use-package all-the-icons-dired
    :ensure t
    :hook (dired-mode . all-the-icons-dired-mode)))

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  )

(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-tools-install pdf-loader-install) ; Make install commands available
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-=" . pdf-view-enlarge)
              ("C--" . pdf-view-shrink))
  :config
  ;; Check if the compiled server component is available
  (if (require 'pdf-tools-server nil 'noerror)
      ;; If YES, then configure the package
      (progn
        (setq revert-without-query (concat "\(" (regexp-quote ".pdf") "\|.*\.pdf\.lock\)"))
        ;; Add the hook for your midnight mode here
        (add-hook 'pdf-view-mode-hook 'nij/pdf-view-midnight-mode-setup))
    
    ;; If NO, print a helpful message
    (message "pdf-tools server not compiled. Run 'M-x pdf-tools-install'.")))

(defun nij/pdf-view-midnight-mode-setup ()
  "Enable pdf-view-midnight-minor-mode in pdf-view-mode."
  (pdf-view-midnight-minor-mode 1))

(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . LaTeX-mode) ; -- This is the lazy-loading trigger
  :config
  ;; Set the default viewing programs
  (setq TeX-view-program-selection
        '(((output-dvi has-no-display-manager) "dvi2tty")
          ((output-dvi style-pstricks) "dvips and gv")
          (output-dvi "xdvi")
          (output-pdf "PDF Tools")) ; auctex will find pdf-tools
        TeX-view-program-list
        '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  ;; Conditionally add 'xdg-open' for HTML if it exists
  (when (executable-find "xdg-open")
    (add-to-list 'TeX-view-program-selection '(output-html "xdg-open")))

  ;; Refresh buffer after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(defun nij/vterm-new ()
  "Open a new vterm buffer with a unique name."
  (interactive)
  (vterm (generate-new-buffer-name "*vterm*")))
  
(use-package vterm
  :ensure t
  :config
  (setq shell-file-name "/bin/bash"
        vterm-max-scrollback 5000) 
  :bind (("C-c t" . vterm)
         ("C-c T" . nij/vterm-new)))

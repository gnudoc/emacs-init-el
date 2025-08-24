;; Note that init.el is generated from ./Emacs.org - that is the file that should be editted.

;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize package system, but defer refreshing contents to avoid startup delays.
;; We can run M-x package-refresh-contents manually when needed.
(package-initialize)

;; Ensure 'use-package' is installed and loaded
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ; Refresh before installing use-package if it's new
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Only enable 'always-ensure' when Emacs is running interactively
;; In batch mode (like for tests), we assume packages are already installed.
;;(when (called-interactively-p 'any) 
 (setq use-package-always-ensure t)
;;)

;; alpha 100 is opaque, alpha 0 is fully transparent
(add-to-list 'default-frame-alist '(alpha-background . 100))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; You will most likely need to adjust these font sizes for your system
(defvar nij/default-font-size 120)
(defvar nij/default-variable-font-size 120)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar - comment this if you're not confident with keybindings yet

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                eww-mode-hook
	      pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height nij/default-font-size)
(set-face-attribute 'default nil :font "FiraCode Nerd Font") ;;another slightly lighter-weight version

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height 130)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140 :weight 'regular)

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package doom-modeline
  :after (all-the-icons nerd-icons) ; Explicitly declare dependencies
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package all-the-icons) ; Now load them to make them available
(use-package nerd-icons)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config (counsel-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defun nij/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun nij/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)

 ;; Ensure org-superstar-mode is enabled here if you want to use it for priority display
  ;; org-superstar-mode uses org-pretty-entities behind the scenes for this.
  (org-superstar-mode 1)) ; Enable org-superstar-mode

;; Define your custom TODO keywords
;; 'SEQUENCE' is the name of this particular keyword sequence.
;; Add URGENT and WAITING as TODO states.
;; Add CANCELLED as a DONE state.
(setq org-todo-keywords
      '(("TODO(t!)" "WAITING(w@/!)" "URGENT(u!)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; Explanation of the syntax:
;; t: quick access key (type `t` to select TODO)
;; !: automatically insert a timestamp when the state changes to this keyword.
;; @: automatically insert a timestamp when the state changes from this keyword.
;; |: separates TODO states from DONE states.
;; g: group keywords (e.g., ("URGENT(u!)" "NEXT(n)" "PROJ(p)" | "DONE(d!)" "CANC(c@)"))
;;   Keywords within the same group (parentheses) are in an exclusive set.

;; Define your custom priority characters (single characters)
;; These are the internal characters Org Mode uses.
(setq org-priority-correspondence '(?C ?I ?N ?M)) ; C for Critical, I for Important, N for Nice, Maybe

;; Define how these priority characters should be displayed.
;; This is a list of associations: (character . "display string")
(setq org-priority-properties
      '(
        (?C . "Critical")
        (?I . "Important")
        (?N . "Nice to have")
        (?M . "Maybe")
        ))

(use-package org
  :hook (org-mode . nij/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (nij/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  ;; Enable display of custom priority strings
  (org-superstar-prettify-item-priority t)
  ;; org-superstar-mode also handles headline bullets by default, but you already have org-bullets.
  ;; If you prefer org-superstar's bullets, you might set:
  ;; (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  ;; Or if you prefer org-bullets for bullets and only org-superstar for priorities:
  ;; (setq org-superstar-special-block-types nil) ; Disable block prettification
  ;; (setq org-superstar-special-todo-keywords nil) ; Disable TODO keyword prettification if org-superstar does it too
  )(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  ;; Enable display of custom priority strings
  (org-superstar-prettify-item-priority t)
  ;; Crucial: Tell org-pretty-entities to apply to priorities.
  ;; org-superstar often sets this up, but explicit is better for custom cases.
  ;; We ensure that org-pretty-entities-include-regexp includes priorities.
  ;; This might be handled by `org-superstar-prettify-item-priority` internally,
  ;; but sometimes explicit definition of the characters helps.
  ;; A more direct way to ensure prettification is via `org-superstar-prettifiers`
  (setq org-superstar-prettifiers
        (list
         '(org-superstar-priority-prettifier . org-superstar-prettify-priority)
         ;; Add other default prettifiers if org-superstar clears them.
         ;; e.g., '(org-superstar-bullet-prettifier . org-superstar-prettify-bullet)
         ))
  ;; And confirm priority faces (even if nil) so org-superstar uses `org-priority-properties`
  (setq org-superstar-priority-faces nil) ; Setting to nil makes it use `org-priority-properties`

  ;; org-superstar-mode also handles headline bullets by default, but you already have org-bullets.
  ;; If you prefer org-superstar's bullets, you might set:
  ;; (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  ;; Or if you prefer org-bullets for bullets and only org-superstar for priorities:
  ;; (setq org-superstar-special-block-types nil) ; Disable block prettification
  ;; (setq org-superstar-special-todo-keywords nil) ; Disable TODO keyword prettification if org-superstar does it too
  )

(defun nij/org-mode-visual-fill ()
  (setq visual-fill-column-width 250
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . nij/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (haskell . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("hs" . "src haskell :tangle yes"))

;; Automatically tangle our Emacs.org config file when we save it
(defun nij/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping example
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nij/org-babel-tangle-config)))

(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c c") 'compile)))


(defun shell-other-window ()
"Open a 'shell' in a new window."
(interactive)
(let ((buf (shell)))
  (switch-to-buffer (other-buffer buf))
  (switch-to-buffer-other-window buf))
)

(add-hook 'c++-mode-hook
        (lambda ()
          (local-set-key (kbd "C-c s") 'shell-other-window)))

(add-hook 'java-mode-hook
	  (lambda ()
	    (local-set-key [3 99] 'compile)))

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
     (php "https://github.com/tree-sitter/tree-sitter-php")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (regex "https://github.com/tree-sitter/tree-sitter-regex")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package haskell-mode)

(defun my-haskell-interactive-mode-hook ()
"Custom settings for the interactive Haskell REPL buffer."
;; The freezing is caused by company-mode so disable it.
  (company-mode -1)  ; or (corfu-mode -1) if you use corfu
(add-hook 'haskell-interactive-mode-hook #'my-haskell-interactive-mode-hook)

(use-package eglot
  :ensure t
  :hook ((c++-mode . eglot-ensure)
	   (java-mode . eglot-ensure)
	   (python-mode . eglot-ensure)) 
 :config
;; Define a general hook for Eglot-managed buffers
;;(add-hook 'eglot-managed-mode-hook
;;          (lambda ()
;;            ;; Turn on flymake for diagnostics and eglot-hover
;;            (flymake-mode 1)
;;            (eglot-hover-mode 1)))
;; Define a company-specific hook
;;(add-hook 'eglot-mode-hook
;;          (lambda ()
;;            ;; Add eglot-capf as a company backend
;;            (add-to-list 'company-backends 'eglot-capf)))
;; Set up keymap prefix for eglot commands
(setq eglot-keymap-prefix "C-c l")

;; Eglot server programs with explicit paths
(setq eglot-server-programs
  '(
    (c++-mode . ("clangd"))
    (c-mode . ("clangd"))
    (java-mode . ("jdtls"))
    (python-mode . ("python3" "-m" "pylsp"))
    ))

;; Configuration for server stability and display
(setq eglot-autoshutdown nil)
(setq eglot-connect-timeout 60)
(setq eglot-stay-alive t)
(setq eglot-auto-server-display nil)
(setq eglot-reconnect-on-change t)
(setq eglot-autodocument t)
(setq eglot-autodocument-delay 0.5)
(setq eglot-display-buffer-function #'eglot-display-buffer-at-bottom))

(use-package company
  ;;:after lsp-mode
  ;;:hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
  ;;      (:map lsp-mode-map
  ;;       ("<tab>" . company-indent-or-complete-common))
  :init
  (global-company-mode)
  :hook (eglot-mode . company-mode)
  :custom
  (company-backends '(company-capf company-dabbrev))
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  ;; This is just a slightly more pretty front-end for company with icons.
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder containing Git and other code repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
:ensure t
:after magit
:config
;; This line ensures auth-source-mode is loaded and configured correctly
(setq auth-sources '("~/.authinfo.gpg"))
;; Other forge configuration can go here
)

(use-package ssh-agency
:ensure t
:config
;; This line is crucial to make it actually do something
;; It ensures the agent is running and keys are added when needed.
(ssh-agency-ensure)

;; Optional: Customize keys to add
(setq ssh-agency-keys (list "~/.ssh/id_ed25519_github"))
)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package exec-path-from-shell
:ensure t
:config
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-alh --group-directories-first")
	     (wdired-allow-to-change-permissions t)))

(use-package all-the-icons-dired
  ;:hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package dired-hide-dotfiles
  ;:hook (dired-mode . dired-hide-dotfiles-mode)
  )

(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")))

(use-package pdf-tools
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("C-=" . pdf-view-enlarge)
              ("C--" . pdf-view-shrink))
  :init (pdf-loader-install)
  :config
  ;; Use `setq` and `regexp-quote` to properly add to revert-without-query
  (setq revert-without-query (concat "\\(" (regexp-quote ".pdf") "\\|.*\\.pdf\\.lock\\)")))

(defun nij/pdf-view-midnight-mode-setup ()
  "Enable pdf-view-midnight-minor-mode in pdf-view-mode."
  (pdf-view-midnight-minor-mode 1))

(add-hook 'pdf-view-mode-hook 'nij/pdf-view-midnight-mode-setup)

(use-package auctex
:config
;; to use pdfview with auctex
(setq TeX-view-program-selection '(((output-dvi has-no-display-manager) "dvi2tty")
                                   ((output-dvi style-pstricks) "dvips and gv")
                                   (output-dvi "xdvi")
                                   (output-pdf "PDF Tools")
                                   (output-html "xdg-open"))
  TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
  TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

;; to have the buffer refresh after compilation
(add-hook 'TeX-after-compilation-finished-functions
      #'TeX-revert-document-buffer))

(use-package vterm
:config
(setq shell-file-name "/bin/bash"
      vterm-max-scrollback 5000))

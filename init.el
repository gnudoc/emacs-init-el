;; Note that init.el is generated from ./Emacs.org - that is the file that should be editted.

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; You will most likely need to adjust these font sizes for your system
(defvar nij/default-font-size 130)
(defvar nij/default-variable-font-size 140)

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
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Code Retina" :height nij/default-font-size)
;;(set-face-attribute 'default nil :font "FiraCode Nerd Font") another slightly lighter-weight version

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 130)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140 :weight 'regular)

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package all-the-icons)
(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package ivy
  :diminish
;;  :bind (("C-s" . swiper)
;;         :map ivy-minibuffer-map
;;         ("TAB" . ivy-alt-done)
;;         ("C-l" . ivy-alt-done)
;;         ("C-j" . ivy-next-line)
;;         ("C-k" . ivy-previous-line)
;;         :map ivy-switch-buffer-map
;;         ("C-k" . ivy-previous-line)
;;         ("C-l" . ivy-done)
;;         ("C-d" . ivy-switch-buffer-kill)
;;         :map ivy-reverse-i-search-map
;;         ("C-k" . ivy-previous-line)
;;         ("C-d" . ivy-reverse-i-search-kill))
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
  (visual-line-mode 1))


(use-package org
  :hook (org-mode . nij/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
;;  (setq org-agenda-start-with-log-mode t)
;;  (setq org-log-done 'time)
;;  (setq org-log-into-drawer t)
;;  (setq org-agenda-files
;;	'("~/Documents/OrgFiles/Tasks.org"))
;;  (require 'org-habit)
;;  (add-to-list 'org-modules 'org-habit)
;;  (setq org-habit-graph-column 60) ;; this might need amended
;;;; there's tons more that can be put in here - look at daviwil's emacs-from-scratch emacs.org org mode section for example
  (nij/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun nij/org-mode-visual-fill ()
  (setq visual-fill-column-width 250
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . nij/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

;; Automatically tange our Emacs.org config file when we save it
(defun nij/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping example
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nij/org-babel-tangle-config)))

(defun nij/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . nij/lsp-mode-setup)
         (cc-mode . lsp-deferred)) ;; for c and c++
  :init (setq lsp-keymap-prefix "C-c l")
  :config (lsp-enable-which-key-integration t))

(use-package lsp-ui
:hook (lsp-mode . lsp-ui-mode)
:custom (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
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

;; the first time you run emacs with this config it'll complain that some function definition is void or something like that.
;; you have to (unload-feature 'sqlite3) then (require 'sqlite3) again.
(use-package sqlite3)

(require 'sqlite3)

(setq dbh (sqlite3-open "person.sqlite3" sqlite-open-readwrite sqlite-open-create))
(sqlite3-exec dbh "create table temp (name text, age integer)")
(setq stmt (sqlite3-prepare dbh "insert into temp values (?,?)"))
(cl-loop for i from 1 to 10 do
         (sqlite3-bind-multi stmt (format "name%d" i) i)
          ;; execute the SQL
         (sqlite3-step stmt)
         ;; call reset if you want to bind the SQL to a new set of variables
         (sqlite3-reset stmt))
(sqlite3-finalize stmt)

(setq stmt (sqlite3-prepare dbh "select * from temp"))
(while (= sqlite-row (sqlite3-step stmt))
  (cl-destructuring-bind (name age) (sqlite3-fetch stmt)
    (message "name: %s, age: %d" name age)))
(sqlite3-finalize stmt)
(sqlite3-close dbh)

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; don't show the startup buffer with emacs logo
(setq inhibit-startup-message t)


;; remove visible scrollbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
;; (menu-bar-mode -1) ; once you're ok with the basics

;; suppress audible bell, replace with a visible flash
(setq visible-bell t)

;;(set-face-attribute 'default nil :font "FiraCode Nerd Font") - a slightly lighter-weight version of the Retina - both are mono and serif.
(set-face-attribute 'default nil :font "Fira Code Retina")

;; Initialise package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialise use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;learn more about ivy and add some keybindings eg for swiper and for ivy-minibuffer-map etc
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package all-the-icons)
;; M-x all-the-icons-install-fonts the first time you run it on a new machine


(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package nerd-icons)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable) ; counsel-describe-function should use helpful-callable for extra helpful info instead of its own thing
  (counsel-describe-variable-function #'helpful-variable) ; likewise
  :bind
  ([remap describe-function] . counsel-describe-function) ; rather than remapping to helpful-callable directly, so we get the ivy/counsel thing
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable) ; likewise
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)
;;:custom
;;(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)) ; if you dont like diff in a separate buffer

;;(use-package forge) ;some sqlite3 issue with this 

(use-package swiper)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit counsel-projectile projectile helpful ivy-rich counsel which-key doom-modeline all-the-icons doom-themes ivy rainbow-delimiters use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


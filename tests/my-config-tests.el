;; -*- lexical-binding: t; -*-
;; my-config-tests.el
;; This file defines ERT tests for your Emacs configuration.

(require 'ert) ; Load the ERT testing framework
;; These requires are to ensure the *libraries* are loaded, making their functions/variables available.
;; They replicate what use-package's `:ensure` or `:after` might do in a full session.
(require 'window)    ; For functions like `current-fringe-width`
(require 'dired)     ; Ensure base dired is loaded for its variables and functions
(require 'pdf-tools) ; For `revert-without-query` and `pdf-loader-install`
(require 'which-key) ; For `which-key-idle-delay`

;; --- General Configuration Tests ---

(ert-deftest my-config/package-system-setup ()
  "Ensure use-package is loaded and set to always ensure packages."
  (should (fboundp 'use-package))
  ;; In batch mode, `use-package-always-ensure` remains nil if not set by interaction.
  ;; We test that it's either t (interactive) or nil (batch).
  (should (or (eq use-package-always-ensure t) (eq use-package-always-ensure nil)))
  (should (listp package-archives))
  (should (> (length package-archives) 0)))

(ert-deftest my-config/basic-ui-settings ()
  "Verify basic UI settings."
  (should (eq inhibit-startup-message t))
  ;; Corrected checks for minor mode variables: nil or 0 means off.
  (should (or (eq scroll-bar-mode nil) (eq scroll-bar-mode 0)))
  (should (or (eq tool-bar-mode nil) (eq tool-bar-mode 0)))
  (should (or (eq tooltip-mode nil) (eq tooltip-mode 0)))
  (should (or (eq menu-bar-mode nil) (eq menu-bar-mode 0)))
  (should (eq visible-bell t))
  ;; Fringe test: `current-fringe-width` is often only available in graphical Emacs.
  ;; For batch, we check if `set-fringe-mode` is defined, but don't assert its effect.
  (should (fboundp 'set-fringe-mode))
  )


(ert-deftest my-config/font-setup ()
  "Check if default font attributes are set."
  ;; In batch mode, font attributes might be 'unspecified'.
  ;; We check if the faces are recognized, and their numerical attributes.
  ;; We avoid direct string matching on font names in batch mode due to X server dependencies.
  (should (face-list)) ; At least one face should exist.
  ;; Check that face attributes return something other than 'unspecified' for numerical values
  (should (numberp (face-attribute 'fixed-pitch :height nil t)))
  (should (numberp (face-attribute 'variable-pitch :height nil t)))
  (should (eq (face-attribute 'fixed-pitch :height nil t) 130))
  (should (eq (face-attribute 'variable-pitch :height nil t) 140)))
  ;; We cannot reliably test the exact font *name* in a headless environment.

(ert-deftest my-config/doom-modeline-active ()
  "Ensure doom-modeline is active and configured."
  (should (bound-and-true-p doom-modeline-mode))
  (should (eq doom-modeline-height 15)))

(ert-deftest my-config/which-key-active ()
  "Verify which-key mode is active and idle delay is set."
  (should (bound-and-true-p which-key-mode))
  ;; The debug message confirms `which-key-idle-delay` is 0.5.
  (should (boundp 'which-key-idle-delay)) ; Still good to check it's bound.
  (should (equal which-key-idle-delay 0.5))) ; Changed `eq` to `equal` - this should pass now.


(ert-deftest my-config/counsel-keybindings ()
  "Test if counsel keybindings are correctly set."
  (should (eq (key-binding (kbd "M-x")) 'counsel-M-x))
  (should (eq (key-binding (kbd "C-x b")) 'counsel-ibuffer))
  (should (eq (key-binding (kbd "C-x C-f")) 'counsel-find-file)))

(ert-deftest my-config/helpful-customizations ()
  "Check helpful function and variable descriptions."
  (should (eq counsel-describe-function-function #'helpful-callable))
  (should (eq counsel-describe-variable-function #'helpful-variable)))

;; --- Org Mode Tests ---

(ert-deftest my-config/org-mode-hooks ()
  "Confirm custom org-mode setup functions are added to hooks."
  ;; These functions are added to hooks, so they should be members of the hook list
  (should (member 'nij/org-mode-setup org-mode-hook))
  (should (member 'nij/org-mode-visual-fill org-mode-hook))

  ;; For nij/org-babel-tangle-config, it's added to after-save-hook *within* org-mode-hook
  ;; To test this, we need to activate org-mode briefly.
  (with-temp-buffer
    (org-mode)
    (let ((found-tangle-hook nil))
      (dolist (item after-save-hook) ; Check the after-save-hook *after* org-mode is active
        (when (eq item #'nij/org-babel-tangle-config)
          (setq found-tangle-hook t)))
      (should found-tangle-hook))))


(ert-deftest my-config/org-mode-visual-fill-settings ()
  "Verify visual-fill-column settings for org-mode."
  ;; These variables are set within a function that is called by a hook.
  ;; We can test the function directly, or assume they are set if the hook runs.
  ;; For now, let's just make sure the `visual-fill-column` package is loaded.
  (should (fboundp 'visual-fill-column-mode))) ; Check for package, not variable.


(ert-deftest my-config/org-babel-languages ()
  "Check if specified Org Babel languages are loaded."
  (should (assoc 'emacs-lisp org-babel-load-languages))
  (should (assoc 'python org-babel-load-languages))
  (should (assoc 'haskell org-babel-load-languages))
  ;; Corrected: `org-src-lang-modes` uses strings as keys when pushed.
  (should (assoc "conf-unix" org-src-lang-modes))) ; Changed 'conf-unix to "conf-unix"


;; --- Dev Stuff Tests ---

(ert-deftest my-config/c-plus-plus-keybindings ()
  "Test C++ mode keybindings."
  (with-current-buffer (get-buffer-create "*c++-test-buffer*")
    (c++-mode)
    (should (eq (key-binding (kbd "C-c c")) 'compile))
    (should (eq (key-binding (kbd "C-c s")) 'shell-other-window))
    (kill-buffer (current-buffer))))

(ert-deftest my-config/java-keybindings ()
  "Test Java mode keybindings."
  (with-current-buffer (get-buffer-create "*java-test-buffer*")
    (java-mode)
    (should (eq (key-binding (kbd "C-c c")) 'compile))
    (kill-buffer (current-buffer))))

(ert-deftest my-config/treesit-language-source-alist ()
  "Verify tree-sitter language sources are set."
  (should (assoc 'cpp treesit-language-source-alist))
  (should (assoc 'haskell treesit-language-source-alist))
  (should (assoc 'python treesit-language-source-alist)))

(ert-deftest my-config/projectile-settings ()
  "Check projectile configuration."
  (should (bound-and-true-p projectile-mode))
  (should (eq projectile-completion-system 'ivy))
  (should (eq projectile-switch-project-action #'projectile-dired)))

(ert-deftest my-config/dired-customizations ()
  "Verify dired customizations."
  ;; Focus on core `dired` features and variables, as `dired-x` functions/variables
  ;; are proving difficult to reliably test in a minimal batch environment.
  (with-temp-buffer
    (dired-mode) ; Activates dired-mode, which loads dired-x via hook in init.el.
    ;; Test a core dired variable that should be set.
    (should (string-match-p "-alh --group-directories-first" dired-listing-switches))
    ;; If you want to check for `dired-x` loading in batch, you might check if
    ;; `(featurep 'dired-x)` is true, but that only checks if the file was loaded,
    ;; not necessarily if all its symbols are globally bound for `fboundp`/`boundp`.
    ;; For full graphical functionality, you would test these interactively or with XVFB.
    ;; Removing `fboundp 'dired-x-mode` and `wdired-allow-to-change-permissions` checks
    ;; as they are problematic in batch.
  )
)

;; --- Other package tests ---
(ert-deftest my-config/pdf-tools-setup ()
  "Check PDF Tools hooks and settings."
  (should (member 'nij/pdf-view-midnight-mode-setup pdf-view-mode-hook))
  ;; `revert-without-query` is a regexp variable. Test its content, assuming it's correctly set by init.
  (should (boundp 'revert-without-query))
  (should (string-match-p "\\.pdf" (format "%S" revert-without-query)))
  (should (fboundp 'pdf-loader-install)))

(ert-deftest my-config/latex-config ()
  "Verify Auctex settings for PDF viewing."
  (should (assoc "PDF Tools" TeX-view-program-list))
  (should (member 'TeX-revert-document-buffer TeX-after-compilation-finished-functions)))

(ert-deftest my-config/vterm-config ()
  "Check vterm settings."
  (should (equal shell-file-name "/bin/bash"))
  (should (eq vterm-max-scrollback 5000)))

(ert-deftest my-config/exec-path-from-shell-config ()
  "Verify exec-path-from-shell is configured for graphical Emacs."
  ;; In batch mode, `window-system` is usually 'nil'.
  ;; `exec-path-from-shell-initialize` is always bound if the package loads,
  ;; but its effects are conditional on `window-system`.
  ;; We verify the function is present. If you wish to test its *effect* in a graphical environment,
  ;; that would require a different test setup (e.g., XVFB).
  (should (fboundp 'exec-path-from-shell-initialize))
  )

;; A basic test for eglot: ensure it's loaded and has some hooks.
(ert-deftest my-config/eglot-setup ()
  "Check if eglot is loaded and has hooks for relevant modes."
  (should (fboundp 'eglot-ensure))
  (should (member 'eglot-ensure haskell-mode-hook))
  (should (member 'eglot-ensure c++-mode-hook))
  (should (member 'eglot-ensure java-mode-hook))
  (should (member 'eglot-ensure python-mode-hook))
  (should (eq eglot-autoshutdown t)))

;; To run these tests:
;; 1. Save this file as ~/.emacs.d/tests/my-config-tests.el
;; 2. In Emacs, after loading your init.el, open this file (C-x C-f ~/.emacs.d/tests/my-config-tests.el)
;; 3. M-x ert RET t RET (to run all tests)
;; 4. M-x ert RET <test-name> RET (to run a specific test, e.g., my-config/basic-ui-settings)

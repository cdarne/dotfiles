;;; init.el --- My Emacs config

;;; Commentary:

;; Does all the config for Emacs

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *is-a-mac* (eq system-type 'darwin))

(require 'init-elpa)
(require 'init-osx)

;; Backup files in /temp please
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; No need for ~ files when editing
(setq create-lockfiles nil)

(delete-selection-mode 1)

(setq ediff-window-setup-function 'ediff-setup-windows-plain ; ediff options in minibuffer
      save-interprogram-paste-before-kill t ; Emacs will first save the clipboard to its kill ring, preventing you from losing the old clipboard data when killing text
      mouse-yank-at-point t ; inserts the text at point, regardless of where you clicked or even which of the frame’s windows you clicked on
      load-prefer-newer t ; when a file is updated outside Emacs reload it
      ediff-split-window-function 'split-window-horizontally)

;; Helm
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-command-map (kbd "o") 'helm-occur)
(define-key helm-command-map (kbd "g") 'helm-do-grep)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-ff-file-name-history-use-recentf t
      helm-ff-auto-update-initial-value     t) ; activate auto-completion

(helm-mode 1)

;; Smart mode line
(sml/setup)

;; Go config
(require 'go-mode-autoloads)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") #'godef-jump)))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; auto-completion
(require 'auto-complete-config)
(ac-config-default)
(setq-default ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed

; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
(setq tab-always-indent 'complete)  ; use TAB when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)
(setq completion-cycle-threshold 5) ; Stop completion-at-point from popping up completion buffers so eagerly

(setq c-tab-always-indent nil
      c-insert-tab-function 'indent-for-tab-command)

(defun sanityinc/auto-complete-at-point ()
  (when (and (not (minibufferp))
	     (fboundp 'auto-complete-mode)
	     auto-complete-mode)
    #'auto-complete))

(defun sanityinc/never-indent ()
  (set (make-local-variable 'indent-line-function) (lambda () 'noindent)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions
        (cons 'sanityinc/auto-complete-at-point
              (remove 'sanityinc/auto-complete-at-point completion-at-point-functions))))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(require 'go-autocomplete)

;; Tab/indentation config
(setq-default tab-width 2
              standard-indent 2
              indent-tabs-mode nil)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(global-set-key (kbd "C-x g") 'magit-status)

;; org mode key bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-startup-indented t)

;; Window navigation
(global-set-key (kbd "<C-tab>") 'other-window)

;; Binds M-arrow keys to move window
(windmove-default-keybindings 'meta)

;; Get some vim awesomeness
(global-set-key (kbd "C-*") 'evil-search-symbol-forward)
(global-set-key (kbd "C-#") 'evil-search-symbol-backward)

(global-set-key (kbd "<home>") 'move-beginning-of-line) ; Bind home to beginning of the line
(global-set-key (kbd "<end>") 'move-end-of-line) ; Bind end to end of the line

;; lisp / slime config
(setq inferior-lisp-program "sbcl")
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Ruby config
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(defun my-sh-mode-hook ()
  "My settings for 'sh-mode'."
  (interactive)
  (setq sh-basic-offset 2
        sh-indentation 2))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)

;; JavaScript config

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(defun my-js-mode-hook ()
  "My settings for 'js-mode'."
  (interactive)
  (setq js-indent-level 2
        js2-basic-offset 2))

(add-hook 'js-mode-hook 'my-js-mode-hook)

;; Saltstack
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

(provide 'init)
;;; init.el ends here

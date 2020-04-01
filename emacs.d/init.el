;;; init.el --- My Emacs config

;;; Commentary:

;; Does all the config for Emacs

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(align-c++-modes (quote (c++-mode c-mode java-mode js2-mode)))
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
 '(default ((t (:family "Monaco" :foundry "unknown" :slant normal :weight normal :height 140 :width normal)))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *is-a-mac* (eq system-type 'darwin))

(require 'init-elpa)
(require 'init-osx)

(exec-path-from-shell-initialize)

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

;; Custom key bindings
(global-set-key (kbd "C-x C-.") 'find-file-at-point)

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
(exec-path-from-shell-copy-envs '("GOROOT" "GOPATH"))
; Go oracle
;; (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
;; (defun my-go-mode-hook ()
;;   "My settings for 'go-mode'."
;;   (interactive)
;;   (setq gofmt-command "goimports")
;;   ; Call Gofmt before saving
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   ; Godef jump key binding
;;   (local-set-key (kbd "M-.") 'godef-jump)
;;   (set (make-local-variable 'company-backends) (append '(company-go) company-backends)))
;; (add-hook 'go-mode-hook 'my-go-mode-hook)

(add-hook '≈after-init-hook #'global-flycheck-mode)


;; Tab/indentation config
(setq-default tab-width 2
              standard-indent 2
              indent-tabs-mode nil)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(global-set-key (kbd "C-x g") 'magit-status)

(defun my-git-commit-setup-hook ()
  "My settings for 'git-commit-setup'."
  (interactive)
  (setq git-commit-summary-max-length 72
        git-commit-fill-column 80))

(add-hook 'git-commit-setup-hook 'my-git-commit-setup-hook)

;; org mode key bindings
;;(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-startup-indented t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sql . t)
   (ruby . t)
   (sh . t)
   (js . t)))

;; Binds M-arrow keys to move window
(windmove-default-keybindings 'meta)

;; Get some vim awesomeness
;; (require 'evil)
;; (global-set-key (kbd "C-*") 'evil-search-word-forward)
;; (global-set-key (kbd "C-#") 'evil-search-word-backward)

(global-set-key (kbd "<home>") 'move-beginning-of-line) ; Bind home to beginning of the line
(global-set-key (kbd "<end>") 'move-end-of-line) ; Bind end to end of the line

;; lisp / slime config
(setq inferior-lisp-program "sbcl")

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Ruby config
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(defun my-ruby-mode-hook ()
  "My settings for 'ruby-mode'."
  (interactive)
  (set (make-local-variable 'company-backends) (append '(company-robe) company-backends)))

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(defun my-sh-mode-hook ()
  "My settings for 'sh-mode'."
  (interactive)
  (setq sh-basic-offset 2
        sh-indentation 2)
  (set (make-local-variable 'company-backends) (append '(company-shell) company-backends)))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)

;; JavaScript config

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))

(defun my-js-mode-hook ()
  "My settings for 'js-mode'."
  (interactive)
  (setq js-indent-level 2
        js2-basic-offset 2
        js2-bounce-indent-p t))

(add-hook 'js-mode-hook 'my-js-mode-hook)

;; Saltstack
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

;; Auto save on lost focus
(defun save-all-buffers ()
  "Save all unsaved buffers."
  (interactive)
    (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all-buffers)

;; Completion
(defun completion-config ()
  "Setup completion config."
  (interactive)
  (setq company-idle-delay 0.2
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        tab-always-indent 'complete)
  (global-company-mode t)
  (setq company-backends '(company-capf (company-dabbrev-code company-keywords) company-files company-dabbrev)))

(add-hook 'after-init-hook 'completion-config)

;; Easy buffer switching
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "<C-tab>") 'switch-to-previous-buffer)

(require 'evil)

(provide 'init)
;;; init.el ends here
(put 'scroll-left 'disabled nil)

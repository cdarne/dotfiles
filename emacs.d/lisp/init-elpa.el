;;; init-elpa.el --- elpa/package config

;;; Commentary:

;; Config for elpa/packages

;;; Code:
(require 'package)

;; We include the org repository for completeness, but don't normally
;; use it.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Also use Melpa for most packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; optimization, no need to activate all the packages so early
(setq package-enable-at-startup nil)

(package-initialize)

(defvar custom-packages
  '(ac-slime
    auto-complete
    erlang
    evil
    exec-path-from-shell
    flycheck
    go-mode
    helm
    helm-mt
    helm-projectile
    magit
    multi-term
    neotree
    org
    paredit
    projectile
    slime
    smart-mode-line
    web-mode)
  "A list of packages to ensure are installed at launch.")

(defun every (predicate-function list)
  "Check if PREDICATE-FUNCTION is t for every element of the LIST."
  (if (not list)
      t
    (and (funcall predicate-function (car list)) (every predicate-function (cdr list)))))

(defun custom-packages-installed-p ()
  "Check if all packages in `custom-packages' are installed."
  (every #'package-installed-p custom-packages))

(defun custom-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package custom-packages)
    (add-to-list 'custom-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun custom-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'custom-require-package packages))

(defun custom-install-packages ()
  "Install all packages listed in `custom-packages'."
  (unless (custom-packages-installed-p)
    ;; check for new packages (package versions)
    (when (not package-archive-contents)
      (message "%s" "Emacs is now refreshing its package database...")
      (package-refresh-contents)
      (message "%s" " done."))
    ;; install the missing packages
    (custom-require-packages custom-packages)))

;; run package installation
(custom-install-packages)

(provide 'init-elpa)
;;; init-elpa.el ends here

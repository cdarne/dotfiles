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
  '(company
    erlang
    evil
    exec-path-from-shell
    flycheck
    go-mode
    helm
    helm-mt
    helm-projectile
    inf-ruby
    js2-mode
    json-mode
    magit
    multi-term
    org
    paredit
    projectile
    ruby-electric
    ruby-tools
    smart-mode-line
    web-mode
    yaml-mode)
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

;; alchemist          20160602.2341 installed             Elixir tooling integration into Emacs
;; async              20160513.128  installed             Asynchronous processing in Emacs
;; company            20160619.1916 installed             Modular text completion framework
;; company-go         20160306.1355 installed             company-mode backend for Go (using gocode)
;; company-inf-ruby   20140805.1354 installed             company-mode completion back-end for inf-ruby
;; company-shell      20160528.507  installed             Company mode backend for shell functions
;; company-web        20160502.658  installed             Company version of ac-html, complete for web,html,emmet,jade,slim modes
;; dash               20160619.611  installed             A modern list library for Emacs
;; dockerfile-mode    20160128.951  installed             Major mode for editing Docker's Dockerfiles
;; elixir-mode        20160607.59   installed             Major mode for editing Elixir files
;; epl                20150517.433  installed             Emacs Package Library
;; erlang             20151013.157  installed             Erlang major mode
;; evil               20160619.2253 installed             Extensible Vi layer for Emacs.
;; exec-path-from-... 20160112.2246 installed             Get environment variables such as $PATH from the shell
;; f                  20160426.527  installed             Modern API for working with files and directories
;; faceup             20150215.1348 installed             Regression test system for font-lock
;; flycheck           20160606.302  installed             On-the-fly syntax checking
;; flycheck-elixir    20160404.31   installed             Support Elixir in flycheck
;; flycheck-mix       20160606.1329 installed             Elixir mix flycheck integration
;; flyspell-correct   20160612.2145 installed             correcting words with flyspell via custom interface
;; flyspell-correc... 20160610.851  installed             correcting words with flyspell via helm interface
;; git-commit         20160519.950  installed             Edit Git commit messages
;; go-mode            20160512.110  installed             Major mode for the Go programming language
;; goto-chg           20131228.659  installed             goto last change
;; groovy-mode        20160504.211  installed             Major mode for Groovy source files
;; helm               20160616.217  installed             Helm is an Emacs incremental and narrowing framework
;; helm-core          20160617.307  installed             Development files for Helm
;; helm-mt            20151104.2120 installed             helm multi-term management.
;; helm-projectile    20160614.832  installed             Helm integration for Projectile
;; iedit              20160618.2010 installed             Edit multiple regions in the same way simultaneously.
;; inf-ruby           20160617.551  installed             Run a Ruby process in a buffer
;; inflections        20121016.157  installed             convert english words between singular and plural
;; js2-mode           20160610.1706 installed             Improved JavaScript editing mode
;; json-mode          20160601.356  installed             Major mode for editing JSON files
;; json-reformat      20160212.53   installed             Reformatting tool for JSON
;; json-snatcher      20150511.2047 installed             Grabs the path to JSON values in a JSON file
;; let-alist          1.0.4         installed             Easily let-bind values of an assoc-list by their names
;; macrostep          20151213.145  installed             interactive macro expander
;; magit              20160619.859  installed             A Git porcelain inside Emacs
;; magit-popup        20160606.1041 installed             Define prefix-infix-suffix command combos
;; mmm-mode           20150828.1716 installed             Allow Multiple Major Modes in a buffer
;; multi-term         20160619.233  installed             Managing multiple terminal buffers in Emacs.
;; nginx-mode         20150824.1411 installed             major mode for editing nginx config files
;; paredit            20160615.1325 installed             minor mode for editing parentheses
;; pkg-info           20150517.443  installed             Information about packages
;; popup              20160531.425  installed             Visual Popup User Interface
;; projectile         20160526.832  installed             Manage and navigate projects in Emacs easily
;; py-yapf            20160101.412  installed             Use yapf to beautify a Python buffer
;; rake               20150831.158  installed             Run rake commands
;; rich-minority      20151201.400  installed             Clean-up and Beautify the list of minor-modes.
;; robe               20160518.259  installed             Code navigation, documentation lookup and completion for Ruby
;; ruby-electric      20150424.752  installed             Minor mode for electrically editing ruby code
;; ruby-tools         20151209.815  installed             Collection of handy functions for ruby-mode.
;; s                  20160508.2357 installed             The long lost Emacs string manipulation library.
;; seq                2.16          installed             Sequence manipulation functions
;; smart-mode-line    20160618.1512 installed             A color coded smart mode-line.
;; undo-tree          20140509.522  installed             Treat undo history as a tree
;; web-completion-... 20160318.148  installed             Shared completion data for ac-html and company-web
;; web-mode           20160611.1226 installed             major mode for editing web templates
;; with-editor        20160408.201  installed             Use the Emacsclient as $EDITOR
;; yaml-mode          20160528.1400 installed             Major mode for editing YAML files

(provide 'init-elpa)
;;; init-elpa.el ends here

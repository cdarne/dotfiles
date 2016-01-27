(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX")
  
  ;; Add the following to your init file to have packages installed by Homebrew added to your load-path:
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))
  
  (setq erlang-root-dir "/usr/local/lib/erlang")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH")))

(provide 'init-osx)

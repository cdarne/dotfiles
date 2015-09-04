(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX")
  
  ;; Add the following to your init file to have packages installed by Homebrew added to your load-path:
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))

  (setq erlang-root-dir "/usr/local/lib/erlang")
  (setq exec-path (delete-dups (append '("/usr/local/opt/android-sdk/bin" "/Users/cdarne/.rbenv/shims" "/Users/cdarne/.rbenv/bin" "/usr/local/opt/go/libexec/bin" "/Users/cdarne/Code/go/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin") exec-path))))

(provide 'init-osx)

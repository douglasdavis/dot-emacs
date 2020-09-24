;;; early-init.el --- Emacs early-init.el            -*- lexical-binding: t; -*-

;; 10GB GC threshold during init
(setq gc-cons-threshold (* 10000 1024 1024))

(require 'package)
(if (fboundp 'native-compile)
    (setq package-user-dir (concat user-emacs-directory "elpa-" emacs-version "-native"))
  (setq package-user-dir (concat user-emacs-directory "elpa-" emacs-version)))

(provide 'early-init)
;;; early-init.el ends here

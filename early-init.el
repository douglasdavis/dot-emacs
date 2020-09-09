;;; early-init.el --- Emacs early-init.el            -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

(require 'package)
(if (fboundp 'native-compile)
    (setq package-user-dir (concat user-emacs-directory "elpa-" emacs-version "-native"))
  (setq package-user-dir (concat user-emacs-directory "elpa-" emacs-version)))

(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here

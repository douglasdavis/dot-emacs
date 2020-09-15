;;; early-init.el --- Emacs early-init.el            -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(setq gc-cons-threshold most-positive-fixnum)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (require 'package)
;; (if (fboundp 'native-compile)
;;     (setq package-user-dir (concat user-emacs-directory "elpa-" emacs-version "-native"))
;;   (setq package-user-dir (concat user-emacs-directory "elpa-" emacs-version)))

(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here

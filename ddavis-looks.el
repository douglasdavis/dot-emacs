;;; ddavis-looks.el --- Emacs looks                  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a place to define the "looks" of my Emacs
;; configuration. This includles themes, fonts, window/frame size.

;;; Code:

(require 'use-package)
(require 'ddavis-vars)

(setq custom-safe-themes t)

(when window-system
  (use-package gruvbox-theme
    :ensure t
    :init
    :config
    (load-theme 'gruvbox t)
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line           nil :overline   line)
      (set-face-attribute 'mode-line-inactive  nil :overline   line)
      (set-face-attribute 'mode-line-inactive  nil :underline  line)
      (set-face-attribute 'mode-line           nil :box        nil)
      (set-face-attribute 'mode-line-inactive  nil :box        nil)
      (set-face-attribute 'mode-line-buffer-id nil :box        nil)))

  (when ddavis-v-is-mac
    (add-to-list 'default-frame-alist '(height . 68))
    (add-to-list 'default-frame-alist '(width . 204)))

  (setq mac-allow-anti-aliasing t)

  (defvar ddavis-v-font
    (cond (ddavis-v-is-mac '(font . "SF Mono-12"))
          (ddavis-v-is-cc7 '(font . "-SAJA-Cascadia Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
          (ddavis-v-is-pion '(font . "-SAJA-Cascadia Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
          (ddavis-v-is-grads-18 '(font . "-*-source code pro-semibold-*-normal-*-*-100-100-100-m-0-*-*"))))
  (add-to-list 'default-frame-alist ddavis-v-font))

(global-display-line-numbers-mode)
(setq column-number-mode t)

;; from https://github.com/emacs-helm/helm/issues/2213
;; Fix issue with the new :extend face attribute in emacs-27
;; Prefer to extend to EOL as in previous emacs.
(defun tv/extend-faces-matching (regexp)
  (cl-loop for f in (face-list)
           for face = (symbol-name f)
           when (and (string-match regexp face)
                     (eq (face-attribute f :extend t 'default)
                         'unspecified))
           do (set-face-attribute f nil :extend t)))

(when (fboundp 'set-face-extend)
  (with-eval-after-load "mu4e"
    (tv/extend-faces-matching "\\`mu4e"))
  (with-eval-after-load "magit"
    (tv/extend-faces-matching "\\`magit"))
  (with-eval-after-load "helm"
    (tv/extend-faces-matching "\\`helm")))


;; some old stuff to keep for reference

;; (when ddavis-v-is-mac

;;   (defvar ddavis-current-theme-is "light"
;;     "A simple holder to help with toggling the solarized theme")

;;   (defun ddavis/load-solarized-dark ()
;;     (interactive)
;;     (setq ddavis-current-theme-is "dark")
;;     (load-theme 'solarized-dark))

;;   (defun ddavis/load-solarized-light ()
;;     (interactive)
;;     (setq ddavis-current-theme-is "light")
;;     (load-theme 'solarized-light))

;;   (defun ddavis/toggle-solarized ()
;;     (interactive)
;;     (if (string= ddavis-current-theme-is "light")
;;         (ddavis/load-solarized-dark)
;;       (ddavis/load-solarized-light)))

;;   (global-set-key (kbd "s-6") 'ddavis/toggle-solarized)

;;   (use-package solarized-theme
;;     :ensure t
;;     :when (window-system)
;;     :init
;;     :config
;;     (when ddavis-v-is-linux-desktop
;;       (ddavis/load-solarized-dark))
;;     (when ddavis-v-is-mac
;;       (ddavis/load-solarized-light))
;;     (let ((line (face-attribute 'mode-line :underline)))
;;       (set-face-attribute 'mode-line           nil :overline   line)
;;       (set-face-attribute 'mode-line-inactive  nil :overline   line)
;;       (set-face-attribute 'mode-line-inactive  nil :underline  line)
;;       (set-face-attribute 'mode-line           nil :box        nil)
;;       (set-face-attribute 'mode-line-inactive  nil :box        nil)
;;       (set-face-attribute 'mode-line-buffer-id nil :box        nil))))

(provide 'ddavis-looks)
;;; ddavis-looks.el ends here

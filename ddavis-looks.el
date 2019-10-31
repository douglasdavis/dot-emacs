;;; ddavis-looks.el --- Emacs looks                  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Doug Davis

;; Author: Doug Davis <douglas.davis.092@gmail.com>
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

;;

;;; Code:

(require 'use-package)
(require 'ddavis-vars)

(use-package gruvbox-theme
  :ensure t
  :init
  (setq custom-safe-themes t)
  :config
  (load-theme 'gruvbox t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line           nil :overline   line)
    (set-face-attribute 'mode-line-inactive  nil :overline   line)
    (set-face-attribute 'mode-line-inactive  nil :underline  line)
    (set-face-attribute 'mode-line           nil :box        nil)
    (set-face-attribute 'mode-line-inactive  nil :box        nil)
    (set-face-attribute 'mode-line-buffer-id nil :box        nil)))

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))
(setq column-number-mode t)

(add-to-list 'default-frame-alist '(height . 80))
(add-to-list 'default-frame-alist '(width . 248))

(add-to-list 'default-frame-alist ddavis-v-font)
(set-face-italic 'font-lock-comment-face nil)
(when (string= (system-name) "grads-18.internal.phy.duke.edu")
  (set-face-attribute 'region nil :background "white" :foreground "dim gray" :inverse-video t))


(provide 'ddavis-looks)
;;; ddavis-looks.el ends here

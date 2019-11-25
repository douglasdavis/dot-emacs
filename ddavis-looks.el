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

(defvar ddavis-current-theme-is "light"
  "A simple holder to help with toggling the solarized theme")

(defun ddavis/load-solarized-dark ()
  (interactive)
  (setq ddavis-current-theme-is "dark")
  (load-theme 'solarized-dark))

(defun ddavis/load-solarized-light ()
  (interactive)
  (setq ddavis-current-theme-is "light")
  (load-theme 'solarized-light))

(defun ddavis/toggle-solarized ()
  (interactive)
  (if (string= ddavis-current-theme-is "light")
      (ddavis/load-solarized-dark)
    (ddavis/load-solarized-light)))

(use-package solarized-theme
  :ensure t
  :when (window-system)
  :init
  :config
  (when ddavis-v-is-linux-desktop
    (ddavis/load-solarized-dark))
  (when ddavis-v-is-mac
    (ddavis/load-solarized-light))
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line           nil :overline   line)
    (set-face-attribute 'mode-line-inactive  nil :overline   line)
    (set-face-attribute 'mode-line-inactive  nil :underline  line)
    (set-face-attribute 'mode-line           nil :box        nil)
    (set-face-attribute 'mode-line-inactive  nil :box        nil)
    (set-face-attribute 'mode-line-buffer-id nil :box        nil)))

(when ddavis-v-is-mac
  (global-set-key (kbd "s-6") 'ddavis/toggle-solarized))

(when ddavis-v-is-linux-desktop
  (global-set-key (kbd "C-x t") 'ddavis/toggle-solarized))

;; (use-package gruvbox-theme
;;   :ensure t
;;   :when (and (window-system) (eq system-type 'gnu/linux))
;;   :init
;;   :config
;;   (load-theme 'gruvbox t)
;;   (let ((line (face-attribute 'mode-line :underline)))
;;     (set-face-attribute 'mode-line           nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive  nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive  nil :underline  line)
;;     (set-face-attribute 'mode-line           nil :box        nil)
;;     (set-face-attribute 'mode-line-inactive  nil :box        nil)
;;     (set-face-attribute 'mode-line-buffer-id nil :box        nil)))

(global-display-line-numbers-mode)
(setq column-number-mode t)

(when ddavis-v-is-mac
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 200)))

(setq mac-allow-anti-aliasing t)

(add-to-list 'default-frame-alist ddavis-v-font)

(provide 'ddavis-looks)
;;; ddavis-looks.el ends here

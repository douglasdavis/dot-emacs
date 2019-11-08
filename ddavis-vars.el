;;; ddavis-vars.el --- global Emacs configuratio variables  -*- lexical-binding: t; -*-

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

;; simple global variables that are useful for the rest of my Emacs
;; configuration.

;;; Code:

(defvar ddavis-v-is-mac (eq system-type 'darwin))
(defvar ddavis-v-is-cc7 (string= (system-name) "cc7"))
(defvar ddavis-v-is-grads-18 (string= (system-name) "grads-18.internal.phy.duke.edu"))
(defvar ddavis-v-is-pion (string= (system-name) "pion"))

(defvar ddavis-v-enable-mu4e
  (or ddavis-v-is-mac
      ddavis-v-is-cc7
      ddavis-v-is-grads-18))

(defvar ddavis-v-enable-irc
  (or ddavis-v-is-mac
      ddavis-v-is-cc7))

(defvar ddavis-v-font
  (cond (ddavis-v-is-mac '(font . "Cascadia Code-12"))
        (ddavis-v-is-cc7 '(font . "-*-cascadia code-*-*-normal-*-*-120-120-100-m-0-*-*"))
        (ddavis-v-is-pion '(font . "-*-cascadia code-*-*-normal-*-*-120-120-100-m-0-*-*"))
        (ddavis-v-is-grads-18 '(font . "-*-source code pro-semibold-*-normal-*-*-100-100-100-m-0-*-*"))))

(defvar ddavis-v-clangd-exe
  (cond (ddavis-v-is-mac "/usr/local/opt/llvm/bin/clangd")
        (ddavis-v-is-cc7 "~/Software/LLVM/releases/9.0.0/bin/clangd")
        (ddavis-v-is-grads-18 "~/Software/LLVM/releases/master/bin/clangd")
        (ddavis-v-is-pion "/usr/bin/clangd")))

(defvar ddavis-v-clang-exe
  (cond (ddavis-v-is-mac "/usr/local/opt/llvm/bin/clang")
        (ddavis-v-is-cc7 "~/Software/LLVM/releases/9.0.0/bin/clang")
        (ddavis-v-is-grads-18 "~/Software/LLVM/releases/master/bin/clang")
        (ddavis-v-is-pion "/usr/bin/clang")))

(defvar ddavis-v-rg-exe
  (cond (ddavis-v-is-mac "/usr/local/bin/rg")
        (ddavis-v-is-cc7 "~/.cargo/bin/rg")
        (ddavis-v-is-grads-18 "~/.cargo/bin/rg")
        (ddavis-v-is-pion "/usr/bin/rg")))

(defvar ddavis-v-fd-exe
  (cond (ddavis-v-is-mac "/usr/local/bin/fd")
        (ddavis-v-is-cc7 "~/.cargo/bin/fd")
        (ddavis-v-is-grads-18 "~/.cargo/bin/fd")
        (ddavis-v-is-pion "/usr/bin/fd")))

(provide 'ddavis-vars)
;;; ddavis-vars.el ends here

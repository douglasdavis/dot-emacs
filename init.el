;;; init.el --- Emacs init.el                        -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2021 Doug Davis

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

;; My personal Emacs configuration.

;;; Code:

;; sec00:
;; preamble stuff

(setq user-mail-address "ddavis@ddavis.io")
(setq user-full-name "Doug Davis")

(setq default-directory
      (file-name-directory
       (directory-file-name user-emacs-directory)))

(setq custom-file (concat user-emacs-directory "custom.el"))

;; sec01:
;; general setup not associated with packages

;; for native comp branch
(defconst dd/using-native-comp (and (fboundp 'native-comp-available-p)
                                    (native-comp-available-p)))

(defvar native-comp-async-query-on-exit)
(defvar native-comp-async-jobs-number)
(defvar native-comp-async-report-warnings-errors)
(defvar native-comp-deferred-compilation-deny-list)
(defvar native-comp-deferred-compilation)

(setq native-comp-async-query-on-exit t)
(setq native-comp-async-jobs-number 4)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation-deny-list '("\\with-editor.el\\'"))
(setq native-comp-deferred-compilation t)

(defun dd/includes? (s substr)
  "Clojure like function; t if S includes SUBSTR."
  (declare (pure t) (side-effect-free t))
  (not (null (string-match-p (regexp-quote substr) s))))

(defconst dd/on-mac-p (eq system-type 'darwin)
  "For checking if on a mac.")

(defconst dd/on-m1-p (or (dd/includes? (emacs-version) "aarch64-apple")
                         (dd/includes? (emacs-version) "arm-apple"))
  "For checking if on M1 mac.")

(defconst dd/on-cc7-p (dd/includes? (system-name) "cc7")
  "For checking if on cc7 box.")

(defconst dd/on-grads-18-p (dd/includes? (system-name) "grads-18")
  "For checking if on grads-18 box.")

(defconst dd/on-strange-p (dd/includes? (system-name) "strange")
  "For checking if on strange box.")

(defconst dd/on-work-p (file-exists-p "/Users/ddavis/.emacs.d/.work-laptop")
  "For checking if on work laptop")

(defconst dd/use-pdf-tools-p (and window-system
                                  (or dd/on-mac-p
                                      dd/on-cc7-p
                                      dd/on-strange-p))
  "For checking if we should use pdf-tools.")

(setq initial-scratch-message
      (concat ";; GNU Emacs "
              emacs-version
              ", "
              system-configuration
              (format " (%s)" window-system)
              ", "
              (format-time-string "%Y-%m-%d"
                                  emacs-build-time)
              (cond (dd/using-native-comp " (native-comp)")
                    (t ""))
              "\n\n"))

(setq echo-keystrokes 0.01
      ring-bell-function 'ignore
      visible-bell nil)

(defun yes-or-no-p-advice (_ &rest args)
  "Advice to use `y-or-n-p' with ORIG-FUN passing along ARGS."
  (apply 'y-or-n-p args))
(advice-add 'yes-or-no-p :around #'yes-or-no-p-advice)

(setq-default auto-save-list-file-prefix nil
              create-lockfiles nil
              backup-by-copying t
              backup-directory-alist `(("." . ,(locate-user-emacs-file ".saves")))
              delete-old-versions t
              kept-new-versions 2
              kept-old-versions 1
              version-control t
              ring-bell-function 'ignore
              revert-without-query '(".*"))

(setq sentence-end-double-space nil)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(when (fboundp 'menu-bar-mode)
  (if dd/on-mac-p
      (menu-bar-mode +1)
    (menu-bar-mode -1)))

(when (and dd/on-mac-p (not window-system))
  (menu-bar-mode -1))

(setq-default require-final-newline t)
(setq-default indent-tabs-mode nil)

(setq default-frame-alist
      '((height . 54)
        (width . 196)
        (left . 0)
        (top . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (tool-bar-lines . 0)))

(defun dd/reset-font ()
  "Reset font to personal default"
  (interactive)
  (when dd/on-mac-p
    (when (boundp 'ns-antialias-text)
      (setq ns-antialias-text t))
    (set-face-attribute 'default nil
                        :family "MonoLisa"
                        :weight 'regular
                        :height 130)
    (dolist (face '(font-lock-doc-face font-lock-comment-face))
      (set-face-attribute face nil :italic t)))

  (when dd/on-strange-p
    (set-face-attribute 'default nil
                        :family "MonoLisa"
                        :weight 'regular
                        :height 140))

  (when dd/on-cc7-p
    (set-face-attribute 'default nil
                        :family "MonoLisa"
                        :weight 'regular
                        :height 130)))

(dd/reset-font)
(when (or dd/on-strange-p dd/on-cc7-p)
  (when (fboundp 'set-fontset-font)
    (set-fontset-font t 'symbol
                      (font-spec :family "Noto Color Emoji")
                      nil 'prepend)))

(when (and dd/on-mac-p (fboundp 'set-fontset-font))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(defun dd/compile-local-site-lisp ()
  "Byte-compile site-lisp in dot-emacs repository."
  (interactive)
  (byte-recompile-directory "~/.emacs.d/dot-emacs/site-lisp" 0 t))

(defun dd/copy-lines-matching-re (re)
  "Put lines matching RE in a buffer named *matching*."
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*matching*")))
    (with-current-buffer result-buffer
      (erase-buffer))
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (princ (buffer-substring-no-properties (line-beginning-position)
                                                 (line-beginning-position 2))
                 result-buffer))))
    (pop-to-buffer result-buffer)))

(defun dd/delete-and-kill-current ()
  "Delete buffer's current file and kill the buffer."
  (interactive)
  (delete-file buffer-file-name)
  (kill-buffer (buffer-name)))

(defun dd/delete-frame-or-window ()
  "If we have multiple frames delete the current one.
If only one delete the window; this is really just for binding
Command+w to behave similar to other macOS applications."
  (interactive)
  (if (< (count-windows) 2)
      (delete-frame)
    (delete-window)))

(defun keyboard-quit-context+ ()
  "Quit current context.
This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use
            \\[kmacro-end-macro] if you want to stop macro
            definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window
           ;; when outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         ;; if we got this far just use the default so we don't miss
         ;; any upstream changes
         (keyboard-quit))))

(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)

(defun dd/kill-all-buffers ()
  "Kill all buffers except scratch and Messages."
  (interactive)
  (let ((keep '("*scratch* *Messages*")))
    (switch-to-buffer "*scratch*")
    (delete-other-windows)
    (mapcar (lambda (b)
              (unless (member (buffer-name b) keep)
                (kill-buffer b)))
            (buffer-list))))

(defun dd/move-file (new-loc)
  "Write this file to NEW-LOC, delete the old one.
Taken from post: https://zck.me/emacs-move-file"
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name
                                         (file-name-nondirectory
                                          (buffer-name))
                                         default-directory))))))
  (when (file-exists-p new-loc)
    (delete-file new-loc))
  (let ((old-loc (expand-file-name (buffer-file-name))))
    (message "old file is: %s and new file is %s" old-loc new-loc)
    (write-file new-loc t)
    (when (and old-loc
               (file-exists-p new-loc)
               (not (string= old-loc new-loc)))
      (delete-file old-loc))))

(defun dd/search-s (s)
  "Perform a google search S."
  (browse-url
   (format "https://google.com/search?q=%s"
           (url-hexify-string s))))

(defun dd/search-region ()
  "Google search the selection region."
  (interactive)
  (dd/search-s (buffer-substring (region-beginning) (region-end))))

(defun dd/search-something (s)
  "Perform an interactive google search of S."
  (interactive "sSearch: ")
  (dd/search-s s))

(defvar dd/themes '(doom-gruvbox doom-Iosvkem modus-operandi doom-solarized-light))
(defun dd/theme-cycler ()
  "Cycle through `dd/themes' variable."
  (interactive)
  (let ((current (car custom-enabled-themes))
        (total (length dd/themes)))
    (when current
      (disable-theme current))
    (if current
        (progn
          (let ((idx (1+ (-elem-index current dd/themes))))
            (when (= idx total)
              (setq idx 0))
            (load-theme (nth idx dd/themes) t)))
      (load-theme (car dd/themes) t))
    (dd/theme-extras)))
(with-eval-after-load 'bind-key
  (bind-key* "<f6>" #'dd/theme-cycler))

(defun dd/theme-extras ()
  "Some things to follow up theme loading."
  (interactive)
  (with-eval-after-load 'selectrum-prescient
    (set-face-attribute 'selectrum-prescient-primary-highlight
                        nil :inherit 'info-xref-visited))
  (dolist (face '(font-lock-doc-face font-lock-comment-face))
    (set-face-attribute face nil :italic t))
  (when (eq (car custom-enabled-themes) 'doom-Iosvkem)
    (custom-set-faces
     `(company-tooltip      ((t (:background ,(doom-color 'base3)))))
     `(company-scrollbar-bg ((t (:background ,(doom-color 'magenta))))))))

(defun dd/toggle-window-split ()
  "If two windows are present; toggle the split axis."
  (interactive)
  (if (= (length (window-list)) 2)
      (let ((buf (current-buffer))
            before-height)
        (with-current-buffer buf
          (setq before-height (window-height))
          (delete-window)
          (set-window-buffer
           (select-window (if (= (window-height) before-height)
                              (split-window-vertically)
                            (split-window-horizontally)))
           buf)))
    (user-error "Can toggle split only with two windows")))
(with-eval-after-load 'bind-key
  (bind-key* "C-x \\" #'dd/toggle-window-split))

(defun dd/vterm-go ()
  "Switch to (or create) a general vterm called dd/vterm."
  (interactive)
  (delete-other-windows)
  (if (get-buffer "dd/vterm")
      (progn
        (set-buffer "dd/vterm")
        (switch-to-buffer "dd/vterm"))
    (vterm "dd/vterm")))

(defconst dd/llvm-bin-path
  (cond (dd/on-m1-p "/opt/homebrew/opt/llvm/bin")
        (dd/on-mac-p "/usr/local/opt/llvm/bin")
        (dd/on-cc7-p "/home/ddavis/software/specific/llvm/master/bin")
        (dd/on-grads-18-p "/home/drd25/software/specific/llvm/10.x/bin")
        (t "/usr/bin"))
  "Machine dependent llvm bin path.")

(defun dd/llvm-project-exe (exe-name)
  "Get full path of LLVM executable EXE-NAME."
  (when (and dd/llvm-bin-path (file-exists-p dd/llvm-bin-path))
    (concat (file-name-as-directory dd/llvm-bin-path) exe-name)))

(defconst dd/clang-exe (dd/llvm-project-exe "clang"))
(defconst dd/clang-format-exe (dd/llvm-project-exe "clang-format"))
(defconst dd/clangd-exe (dd/llvm-project-exe "clangd"))

(defconst dd/anaconda-installation "~/software/anaconda3")

;; sec02:
;; use-package setup

(require 'package)
(setq package-archives
      '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'bind-key)
(setq use-package-hook-name-suffix nil)

;; sec03:
;; use-package for some core Emacs packages.

(use-package autorevert
  :init
  (global-auto-revert-mode +1))

(when (or dd/on-mac-p dd/on-cc7-p dd/on-grads-18-p dd/on-strange-p)
  (use-package auth-source
    :init
    (setenv "GPG_AGENT_INFO" nil)
    (setq auth-sources
          (list (concat user-emacs-directory ".authinfo.gpg")))))

(use-package browse-url
  :defer 10
  :config
  (when dd/on-cc7-p
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "/usr/local/bin/firefox")))

(use-package cc-mode
  :config
  (c-set-offset 'innamespace 0))

(use-package dired
  :demand t
  :init
  (setq-default dired-listing-switches "-alh")
  :bind (:map dired-mode-map
              ("q" . #'kill-current-buffer))
  :config
  (setq dired-recursive-copies 'always))

(make-variable-buffer-local 'display-line-numbers-width-start)
(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode))

(use-package env
  :init
  (when dd/on-cc7-p
    (setenv "PKG_CONFIG_PATH" "/usr/lib64/pkgconfig")))

(when (or dd/on-mac-p dd/on-cc7-p dd/on-grads-18-p dd/on-strange-p)
  (use-package epa-file
    :config
    (custom-set-variables
     `(epg-gpg-program
       ,(cond (dd/on-m1-p "/opt/homebrew/bin/gpg")
              (dd/on-mac-p "/usr/local/bin/gpg")
              (dd/on-strange-p "/usr/bin/gpg")
              (t "/usr/bin/gpg2"))))))

(use-package erc
  :commands erc
  :config
  (setq erc-default-server "irc.libera.chat"
        erc-prompt-for-password nil
        erc-user-full-name "Doug Davis"
        erc-rename-buffers t
        erc-track-enable-keybindings nil
        erc-kill-buffer-on-part t
        erc-kill-server-buffer-on-quit t
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 19
        erc-fill-column 86
        erc-query-display 'bury
        erc-prompt (lambda () (concat (buffer-name) " >"))
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                  "324" "329" "332" "333" "353" "477"))
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)

  (defconst dd/erc-colors-list '("#fb4934" "#b8bb26" "#fabd2f"
                                 "#83a598" "#d3869b" "#8ec07c"
                                 "#fe8019" "#cc241d" "#98971a"
                                 "#d79921" "#458588" "#b16286"
                                 "#689d6a" "#d65d0e"))

  ;; special colors for some people
  (defconst dd/erc-nick-color-alist '(("X" . "blue")))

  (defun dd/erc-get-color-for-nick (nick)
    "Gets a color for NICK. If NICK is in
    dd/erc-nick-color-alist, use that color, else hash the nick
    and use a random color from the pool"
    (or (cdr (assoc nick dd/erc-nick-color-alist))
        (nth
         (mod (string-to-number
               (substring (md5 (downcase nick)) 0 6) 16)
              (length dd/erc-colors-list))
         dd/erc-colors-list)))

  (defun dd/erc-put-color-on-nick ()
    "Modifies the color of nicks according to
    dd/erc-get-color-for-nick"
    (save-excursion
      (goto-char (point-min))
      (while (forward-word 1)
        (setq bounds (bounds-of-thing-at-point 'word))
        (setq word (buffer-substring-no-properties
                    (car bounds) (cdr bounds)))
        (when (or (and (erc-server-buffer-p) (erc-get-server-user word))
                  (and erc-channel-users (erc-get-channel-user word)))
          (put-text-property (car bounds) (cdr bounds)
                             'face (cons 'foreground-color
                                         (dd/erc-get-color-for-nick word)))))))

  (add-hook 'erc-insert-modify-hook 'dd/erc-put-color-on-nick)
  (add-hook 'erc-mode-hook (lambda ()
                             (modify-syntax-entry ?\_ "w" nil)
                             (modify-syntax-entry ?\- "w" nil))))

(use-package erc-track :after erc)
(use-package erc-fill :after erc)

(use-package esh-mode
  :hook (eshell-mode-hook . (lambda () (display-line-numbers-mode 0))))

(use-package flyspell
  :hook ((org-mode-hook . flyspell-mode)
         (LaTeX-mode-hook . flyspell-mode)
         (markdown-mode-hook . flyspell-mode)
         (message-mode-hook . flyspell-mode)
         (mu4e-compose-mode-hook . flyspell-mode)))

(use-package help-mode
  :bind
  (:map help-mode-map
        ("q" . #'kill-buffer-and-window)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer-other-window)
         :map ibuffer-mode-map
         ("q" . kill-buffer-and-window))
  :init
  (setq ibuffer-expert t))

(use-package ispell
  :config
  (when dd/on-mac-p
    (setq ispell-program-name
          (cond (dd/on-m1-p "/opt/homebrew/bin/hunspell")
                (t "/usr/local/bin/hunspell")))))

(use-package org
  :hook (org-mode-hook . (lambda () (interactive)
                           (setq-local display-line-numbers-width-start t)))
  :config
  (setq org-src-fontify-natively t)
  (setq org-structure-template-alist
        (append org-structure-template-alist
                '(("el" . "src emacs-lisp :results silent")
                  ("p0" . "src python :session :results silent")
                  ("p1" . "src python :session")
                  ("p2" . "src python :results silent")
                  ("cpp" . "src C++"))))
  (when dd/on-mac-p
    (bind-key "<A-down>" 'org-move-subtree-down org-mode-map)
    (bind-key "<A-up>" 'org-move-subtree-up org-mode-map)
    (bind-key "<A-left>" 'org-promote-subtree org-mode-map)
    (bind-key "<A-right>" 'org-demote-subtree org-mode-map))
  (unless dd/on-mac-p
    (bind-key "<s-down>" 'org-move-subtree-down org-mode-map)
    (bind-key "<s-up>" 'org-move-subtree-up org-mode-map)
    (bind-key "<s-left>" 'org-promote-subtree org-mode-map)
    (bind-key "<s-right>" 'org-demote-subtree org-mode-map)))

(use-package outline
  :mode ("NEWS\\'" . outline-mode))

(use-package python
  :bind (:map python-mode-map
              ("C-c C-a" . dd/py-auto-lsp))
  :init
  (setq python-font-lock-keywords '(python-font-lock-keywords-level-1
                                    python-font-lock-keywords-level-1
                                    python-font-lock-keywords-level-2))
  (setq python-indent-guess-indent-offset nil)
  :config
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt")
  (defvar pyvenv-virtual-env)
  (defun dd/run-python ()
    "Intelligently run a Python shell."
    (interactive)
    (if pyvenv-virtual-env
        (run-python)
      (progn
        (call-interactively #'pyvenv-workon)
        (run-python))))

  (defun dd/py-workon-project-venv ()
    "Call pyenv-workon with the current projectile project name.
  This will return the full path of the associated virtual
  environment found in $WORKON_HOME, or nil if the environment
  does not exist."
    (let ((pname (projectile-project-name)))
      (pyvenv-workon pname)
      (if (file-directory-p pyvenv-virtual-env)
          pyvenv-virtual-env
        (pyvenv-deactivate))))

  (defun dd/py-auto-lsp ()
    "Turn on lsp mode in a Python project with some automated logic.
  Try to automatically determine which pyenv virtual environment
  to activate based on the project name, using
  `dd/py-workon-project-venv'. If successful, call `lsp'. If we
  cannot determine the virtualenv automatically, first call the
  interactive `pyvenv-workon' function before `lsp'"
    (interactive)
    (let ((pvenv (dd/py-workon-project-venv)))
      (if pvenv
          (lsp)
        (progn
          (call-interactively #'pyvenv-workon)
          (lsp)))))

  (defun dd/print-python-expression-in-repl ()
    "Implying the first statement of the line is actually an expression.
  Prints the value at the REPL."
    (interactive)
    (let ((initial-point (point)))
      ;; mark expression at point
      (beginning-of-line)
      (set-mark (point))
      (python-nav-end-of-statement)

      ;; print marked expression in python shell
      (let* ((region-start (min (+ 1 (point)) (point-max)))
             (expr (string-trim-right
                    (buffer-substring-no-properties region-start (mark)))))
        (python-shell-send-string
         (format "print(); print('=> %s'); print(%s, end='')" expr expr)))

      (deactivate-mark)
      (goto-char initial-point)))

  (defun dd/print-python-object-fields-in-repl ()
    "Sends word at point to IPython REPL
  Uses the `ppretty' function defined in ipython_config; lists the
  object's non-method fields and their respective current values."
    (interactive)
    (let ((word (word-at-point)))
      (python-shell-send-string
       (format "print(); print('=> %s'); ppretty(%s)" word word))))

  (bind-key "C-c C-o" #'dd/print-python-object-fields-in-repl python-mode-map)
  (bind-key "C-c C-k" #'dd/print-python-expression-in-repl python-mode-map))

(use-package paren
  :init
  (show-paren-mode 1)
  (setq-default show-paren-delay 0))

(if (< emacs-major-version 28)
    (use-package project :ensure t)
  (use-package project))

(use-package prog-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package shell
  :hook (shell-mode-hook . (lambda () (display-line-numbers-mode))))

(use-package shr
  :config
  (setq shr-use-fonts nil))

(use-package simple
  :config
  (when (and (boundp 'read-extended-command-predicate)
             (fboundp 'command-completion-default-include-p))
    (setq read-extended-command-predicate 'command-completion-default-include-p))
  (column-number-mode +1))

(use-package term
  :hook (term-mode-hook . (lambda () (display-line-numbers-mode 0))))

(use-package tramp
  :defer 5
  :config
  (setq tramp-default-method "ssh")
  (defun dd/cleanup-tramp ()
    "Try to clean up all tramp buffers/connections"
    (interactive)
    (tramp-cleanup-all-buffers)
    (tramp-cleanup-all-connections)
    (find-file "~/.")
    (switch-to-buffer "*scratch*")))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-strip-common-suffix t
        uniquify-after-kill-buffer-p t))

(use-package vc
  :init
  (setq vc-follow-symlinks t))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-style '(face tabs empty trailing)))

;; sec04:
;; third party

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(use-package auto-compile
  :ensure t
  :defer t
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t))

(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-now)
  :init
  (setq auto-package-update-delete-old-versions t))

(use-package blacken
  :ensure t
  :bind (:map python-mode-map
              ("C-c C-f" . blacken-buffer))
  :after python)

(use-package buttercup
  :ensure t
  :defer t)

(when (or dd/on-mac-p dd/on-strange-p dd/on-cc7-p)
  (use-package cider
    :ensure t
    :commands cider-jack-in))

(when (or dd/on-strange-p dd/on-cc7-p dd/on-mac-p)
  (use-package circe
    :ensure t
    :commands circe
    :hook (circe-chat-mode-hook . dd/circe-prompt)
    :config
    (defun dd/irc-pw-freenode (server)
      (auth-source-pass-get 'secret "Freenode"))
    (defun dd/irc-pw-gitter (server)
      (auth-source-pass-get 'secret "Gitter"))
    (defun dd/circe-prompt ()
      (lui-set-prompt
       (propertize (format "%s >>> " (buffer-name)) 'face 'circe-prompt-face)))
    (setq circe-network-options
          '(("Gitter"
             :host "irc.gitter.im"
             :server-buffer-name "⇄ Gitter (irc gateway)"
             :nick "douglasdavis"
             :pass dd/irc-pw-gitter
             :port 6697
             :tls t)
            ("Freenode"
             :nick "ddavis"
             :nickserv-password dd/irc-pw-freenode
             :channels (:after-auth
                        "#emacs"
                        "#python"
                        "#pydata"
                        "#sr.ht"
                        "#lobsters"
                        "#hpy"
                        "##crustaceans")
             :tls t)))
    (setq circe-use-cycle-completion t
          circe-reduce-lurker-spam t
          circe-format-say "<{nick}> {body}"
          lui-fill-type 19
          lui-fill-column 77)
    (setq circe-default-part-message
          (format "Closed Circe (%s) buffer in GNU Emacs (%s)"
                  circe-version
                  emacs-version))
    (setq circe-default-quit-message
          (format "Quit Circe (%s) in GNU Emacs (%s)"
                  circe-version
                  emacs-version))
    (defun dd/switch-circe-channel ()
      (interactive)
      (let ((sources
             (cl-loop for buf in (buffer-list)
                      if (eq 'circe-channel-mode (buffer-local-value 'major-mode buf))
                      collect (buffer-name buf))))
        (switch-to-buffer (completing-read "Channel: " sources))))

    (bind-key (kbd "C-c C-b") #'dd/switch-circe-channel circe-mode-map))

  (use-package circe-color-nicks
    :after circe
    :config
    ;; (setq circe-color-nicks-pool-type
    ;;       '("#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#fe8019"
    ;;         "#cc241d" "#98971a" "#d79921" "#458588" "#b16286" "#689d6a" "#d65d0e"))
    (setq circe-color-nicks-everywhere t)
    (enable-circe-color-nicks)))

(use-package clang-format
  :ensure t
  :after cc-mode
  :custom
  (clang-format-executable dd/clang-format-exe))

;; (use-package cmake-mode
;;   :load-path "~/.emacs.d/dot-emacs/site-lisp/cmake-mode")
(use-package cmake-mode :ensure t)
(use-package cmake-font-lock :ensure t)

(make-variable-buffer-local 'company-minimum-prefix-length)
(make-variable-buffer-local 'company-idle-delay)
(make-variable-buffer-local 'company-backends)
(use-package company
  :ensure t
  :demand t
  :bind
  (:map company-active-map
        ("TAB" . company-complete-selection)
        ("<tab>" . company-complete-selection))
  :config
  (add-hook 'after-init-hook #'global-company-mode)
  (setq-default company-backends '(company-capf
                                   company-files
                                   company-semantic
                                   (company-dabbrev company-keywords)))
  (setq-default company-minimum-prefix-length 2)
  (setq-default company-idle-delay 0.2)
  (defun dd/company-prog-mode  ()
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.1))
  (defun dd/company-text-mode ()
    (setq company-minimum-prefix-length 3
          company-idle-delay 0.3))
  (add-hook 'text-mode-hook #'dd/company-text-mode)
  (add-hook 'prog-mode-hook #'dd/company-prog-mode)
  (when (version<= "28.0.50" emacs-version)
    (setq company-format-margin-function #'company-vscode-dark-icons-margin)))

(use-package consult
  :ensure t
  :bind (("C-c l" . consult-line)
         ("C-c r" . consult-ripgrep)
         ("C-x b" . consult-buffer))
  :config
  (setq consult-preview-key 'nil)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (defun find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial))))

;; (use-package corfu
;;   :ensure t
;;   :init
;;   (setq completion-cycle-threshold 3)
;;   (setq tab-always-indent 'complete)
;;   :config
;;   (corfu-global-mode))

(use-package crux
  :defer 10
  :ensure t)

(use-package cython-mode
  :ensure t
  :mode (("\\.pyx\\'" . cython-mode)
         ("\\.pxd\\'" . cython-mode)))

(use-package debbugs
  :ensure t
  :defer t)

(setq ad-redefinition-action 'accept)
(use-package default-text-scale
  :ensure t
  :demand t
  :init
  :bind (("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)
         ("s-0" . default-text-scale-reset)))

(use-package diredfl
  :ensure t
  :hook (dired-mode-hook . diredfl-mode))

(use-package doom-themes
  :ensure t
  :config
  (dd/theme-cycler))

(use-package doom-modeline
  :ensure t
  :demand t
  :config
  (doom-modeline-mode +1))

(use-package eglot
  :ensure t
  :commands eglot
  :init
  (setq eglot-server-programs
        `((python-mode "pylsp")
          ((c++-mode c-mode) ,dd/clangd-exe)))
  :config
  (setq eglot-autoshutdown t))

(when (version< emacs-version "28")
  (use-package eldoc
    :ensure t))

(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
  :init
  (setq eldoc-box-clear-with-C-g t
        eldoc-box-fringe-use-same-bg nil))

(use-package elfeed
  :ensure t
  :commands elfeed
  :config
  (setq elfeed-search-filter "@21-days-ago")
  (setq elfeed-feeds
        '(("https://planet.scipy.org/feed.xml" python)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://ddavis.io/index.xml" blog)))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "3 weeks ago" :remove 'unread)))

(use-package eros
  :ensure t
  :hook (emacs-lisp-mode-hook . eros-mode))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package flycheck
  :ensure t
  :defer t
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package helpful
  :ensure t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ("C-h o" . helpful-symbol)
         ("C-h ." . helpful-at-point)
         :map helpful-mode-map
         ("q" . kill-buffer-and-window))
  :config
  (setq helpful-max-highlight 15000))

(use-package iedit
  :ensure t
  :bind ("C-c ;" . iedit-mode))

(setq lsp-use-plists t)

(use-package lsp-clangd
  :defer t
  :init
  (setq lsp-clients-clangd-executable dd/clangd-exe))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq read-process-output-max (* 10 1024 1024))
  (setq lsp-keep-workspace-alive nil
        lsp-auto-guess-root nil
        lsp-enable-links nil
        lsp-enable-on-type-formatting nil))

(use-package lsp-pylsp
  :defer t
  :init
  (setq lsp-clients-pylsp-library-directories `("/usr"
                                                ,(expand-file-name "~/.pyenv/versions/")))
  (setq lsp-pylsp-plugins-pydocstyle-convention "numpy"
        lsp-pylsp-configuration-sources ["flake8"])
  (setq lsp-pylsp-plugins-pydocstyle-enabled nil
        lsp-pylsp-plugins-flake8-enabled t
        lsp-pylsp-plugins-autopep8-enabled nil
        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pycodestyle-enabled nil
        lsp-pylsp-plugins-pyflakes-enabled nil
        lsp-pylsp-plugins-pylint-enabled nil)
  (defun dd/pylsp-toggle-pydocstyle ()
    "Toggle pycodestyle for pylsp."
    (interactive)
    (setq lsp-pylsp-plugins-pydocstyle-enabled (not lsp-pylsp-plugins-pydocstyle-enabled)))
  (defun dd/pylsp-toggle-flake8 ()
    "Toggle pycodestyle for pylsp."
    (interactive)
    (setq lsp-pylsp-plugins-flake8-enabled (not lsp-pylsp-plugins-flake8-enabled))))

(use-package lsp-ui
  :ensure t
  :defer t
  :init
  (defun dd/lsp-use-webkit ()
    "Use webkit lsp ui doc rendering if possible."
    (interactive)
    (setq lsp-ui-doc-use-webkit (featurep 'xwidget-internal)))
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-header t
        lsp-ui-doc-max-height 32
        lsp-ui-doc-max-width 96
        lsp-ui-sideline-show-hover nil))

(defun dd/pyright ()
  (interactive)
  (setq lsp-pyright-typechecking-mode "off")
  (use-package lsp-pyright
    :ensure t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (defun dd/magit-kill-buffers ()
    "See `https://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/'"
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" "\\.markdown\\'"))

(use-package modern-cpp-font-lock
  :ensure t
  :init
  (modern-c++-font-lock-global-mode +1))

(use-package numpydoc
  :ensure t
  :init
  (setq numpydoc-insertion-style nil)
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate))
  :after python)

(use-package orderless
  :ensure t
  :init
  (defun dd/use-orderless-in-minibuffer ()
    (interactive)
    (setq-local completion-styles '(orderless basic partial-completion emacs22 initials)))
  (add-hook 'minibuffer-setup-hook 'dd/use-orderless-in-minibuffer)
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package ox-hugo
  :ensure t
  :after ox)

;; (when dd/use-pdf-tools-p
;;   (use-package pdf-tools
;;     :ensure t
;;     :defer t
;;     :hook (pdf-view-mode-hook . (lambda () (display-line-numbers-mode 0)))
;;     :config
;;     (pdf-tools-install)
;;     (setq-default pdf-view-display-size 'fit-page)
;;     (when dd/on-mac-p
;;       (setq pdf-view-use-scaling t)
;;       (setq pdf-view-use-imagemagick nil))))

(use-package posframe
  :ensure t)

(use-package projectile
  :ensure t
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("r" . dd/ripgrep-proj-or-dir)
              ("g" . consult-ripgrep))
  :config
  (setq projectile-ignored-project-function 'file-remote-p)
  (setq projectile-track-known-projects-automatically t
        projectile-globally-ignored-file-suffixes '("#" "~" ".o" ".so" ".elc" ".pyc")
        projectile-globally-ignored-directories '(".git" "__pycache__")
        projectile-globally-ignored-files '(".DS_Store")
        projectile-ignored-projects '("/opt/homebrew/"
                                      "/usr/local/Homebrew/")
        projectile-enable-caching nil)
  (projectile-mode +1))

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  :config
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list
         (lambda ()
           (setq python-shell-interpreter (concat pyvenv-virtual-env
                                                  "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list
         (lambda ()
           (setq python-shell-interpreter "python3"))))

  (defun dd/conda-envs (&optional dir)
    "Get list of conda environments."
    (let ((search-dir (if dir
                          dir
                        (format "%s/envs" dd/anaconda-installation))))
      (f-directories search-dir)))
  (defun dd/conda-env-activate (name)
    "Activate conda with pyvenv."
    (interactive
     (list
      (completing-read "Work on: " (dd/conda-envs))))
    (pyvenv-activate name)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))


(use-package recentf
  :demand t
  :init
  (defun dd/recentf-excluder (s)
    "Check if file should be recentf excluded."
    (s-contains-p ".emacs.d/elpa-" s))
  (setq recentf-exclude '(dd/recentf-excluder))
  :config
  (recentf-mode +1)
  (add-to-list 'recentf-exclude 'file-remote-p))

(use-package rg
  :ensure t
  :after wgrep
  :init
  (setq rg-group-result t
        rg-hide-command t)
  :config
  (rg-define-search dd/ripgrep-proj-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((proj (projectile-project-root)))
           (if proj
               proj
             default-directory))
    :confirm prefix
    :flags ("--hidden -g !.git")))

;; (use-package selectrum
;;   :ensure t
;;   :demand t
;;   :init
;;   (setq selectrum-extend-current-candidate-highlight t)
;;   (setq selectrum-num-candidates-displayed 'auto)
;;   (setq selectrum-max-window-height 16)
;;   (setq selectrum-fix-vertical-window-height t)
;;   :custom-face
;;   (selectrum-current-candidate ((t (:inherit region))))
;;   :config
;;   (selectrum-mode +1))

;; (use-package selectrum-prescient
;;   :ensure t
;;   :after selectrum
;;   :custom-face
;;   :config
;;   (selectrum-prescient-mode +1)
;;   (prescient-persist-mode +1))

(when (or dd/on-mac-p dd/on-cc7-p)
  (use-package tex :defer t)
  (use-package tex-buf :defer t)
  (use-package font-latex :defer t)
  (use-package tex-site
    :ensure auctex
    :mode ("\\.tex\\'" . TeX-latex-mode)
    :config
    (setq font-latex-fontify-sectioning 'color
          font-latex-fontify-script nil
          TeX-source-correlate-mode 'synctex
          TeX-source-correlate-start-server t)
    (setq-default TeX-master nil)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)))
;; (when dd/use-pdf-tools-p
;;   (setq TeX-view-program-selection '((output-pdf "PDF Tools"))))))

(unless (or dd/on-m1-p dd/on-cc7-p)
  (use-package tree-sitter
    :ensure t
    :hook (python-mode-hook . tree-sitter-hl-mode))
  (use-package tree-sitter-langs
    :ensure t))

(use-package vertico
  :ensure t
  :demand t
  :init
  (setq enable-recursive-minibuffers t)
  (setq vertico-count 13)
  (setq vertico-sort-threshold 3000)
  ;; :custom-face
  ;; (vertico-current ((t (:inherit region))))
  :config
  (vertico-mode +1)
  (setq resize-mini-windows nil)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(when window-system
  (use-package vertico-posframe
    :ensure t
    :demand t
    :config
    (vertico-posframe-mode +1)))

(use-package visual-fill-column
  :ensure t)

(use-package vterm
  :ensure t
  :defer t
  :hook (vterm-mode-hook . (lambda () (display-line-numbers-mode -1))))

(use-package w3m
  :ensure t
  :defer t
  :config
  (setq w3m-default-display-inline-images t))

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode)
  (setq which-key-frame-max-height 50))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package yasnippet
  :ensure t
  :demand t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; sec05:
;; some package-free bindings and macOS specifics

(when (version< emacs-version "28")
  (bind-key "C-x x g" #'revert-buffer))

;; some macOS specifics
(when dd/on-mac-p
  ;; browser and modifier setq's
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (setq-default ns-alternate-modifier 'meta)
  (setq-default mac-option-modifier 'meta)
  (setq-default ns-right-alternate-modifier nil)
  (setq-default ns-command-modifier 'super)
  (setq-default mac-command-modifier 'super)
  (setq-default ns-function-modifier 'hyper)
  (setq-default mac-function-modifier 'hyper)
  ;; will use s-h as a prefix for describe-* functions.
  (global-unset-key (kbd "s-h"))
  ;; super (cmd) key bindings.
  (bind-key* "s-<right>" #'right-word)
  (bind-key* "s-<left>" #'left-word)
  (bind-key* "s-\\" #'dd/toggle-window-split)
  (bind-key* "s-\"" #'dd/search-something)
  (bind-key* "s-/" #'previous-buffer)
  (bind-key* "s-1" #'delete-other-windows)
  (bind-key* "s-2" #'split-window-below)
  (bind-key* "s-3" #'split-window-right)
  (bind-key* "s-4" #'mu4e)
  (bind-key* "s-5" #'projectile-find-file-in-known-projects)
  (bind-key* "s-d" #'dd/kill-theme)
  (bind-key* "s-e" #'gnus)
  (bind-key* "s-f" #'find-file)
  (bind-key* "s-b" #'consult-buffer)
  (bind-key* "s-g" #'magit-status)
  (bind-key* "s-h f" #'describe-function)
  (bind-key* "s-h v" #'describe-variable)
  (bind-key* "s-h k" #'describe-key)
  (bind-key* "s-i" #'crux-find-user-init-file)
  (bind-key* "s-k" #'kill-current-buffer)
  (bind-key* "s-n" #'make-frame)
  (bind-key* "s-o" #'other-window)
  (bind-key* "s-p" #'projectile-command-map)
  (bind-key* "s-q" #'save-buffers-kill-terminal)
  (bind-key* "s-r" #'consult-ripgrep)
  (bind-key* "s-s" #'save-buffer)
  (bind-key* "s-t" #'dd/vterm-go)
  (bind-key* "s-u" #'auto-package-update-now)
  (bind-key* "s-w" #'dd/delete-frame-or-window)
  (bind-key* "s-x" #'execute-extended-command)
  (bind-key* "s-*" #'dd/kill-all-buffers)
  (bind-key* "H-<backspace>" #'delete-forward-char))

;; sec06:
;; email setup is in dedicated file

(when (or dd/on-m1-p dd/on-cc7-p dd/on-mac-p)
  (load-file "~/.emacs.d/dot-emacs/email.el"))

;; sec07:
;; work

(when dd/on-work-p
  (load-file "~/.emacs.d/day-job.el"))

;; sec08:
;; misc

(when dd/on-m1-p
  (defun dd/pyts ()
    "Setup local clone of tree-sitter."
    (interactive)
    (use-package tree-sitter
      :load-path "/Users/ddavis/software/repos/elisp-tree-sitter/lisp"
      :init
      (add-to-list 'load-path "/Users/ddavis/software/repos/elisp-tree-sitter/core")
      :config
      (add-to-list 'load-path "/Users/ddavis/software/repos/elisp-tree-sitter/langs")
      (require 'tree-sitter)
      (require 'tree-sitter-langs)
      (tree-sitter-require 'python)
      (add-hook 'python-mode-hook 'tree-sitter-hl-mode))))

;; the end

;; 128MB garbage collection threshold
(setq gc-cons-threshold (* 128 1024 1024))

(provide 'init)
;;; init.el ends here

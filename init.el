;;; init.el --- Emacs init.el                        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Doug Davis

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

(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

(setq user-mail-address "ddavis@ddavis.io")
(setq user-full-name "Doug Davis")

(setq default-directory
      (file-name-directory
       (directory-file-name user-emacs-directory)))

(setq custom-file (concat user-emacs-directory "custom.el"))

;; sec01:
;; general setup not associated with packages

;; for native comp branch
(defconst dd/using-native-comp (fboundp 'native-comp-available-p))
(when (boundp 'comp-deferred-compilation)
  (setq comp-deferred-compilation t))

(defun dd/includes? (s substr)
  "Clojure like function; t if S includes SUBSTR."
  (declare (pure t) (side-effect-free t))
  (not (null (string-match-p (regexp-quote substr) s))))

(defconst dd/on-mac-p (eq system-type 'darwin)
  "For checking if on a mac.")

(defconst dd/on-m1-p (dd/includes? (emacs-version) "aarch64-apple")
  "Fot checking if on M1 mac.")

(defconst dd/on-abx-p (dd/includes? (system-name) "abx")
  "For checking of in abx box.")

(defconst dd/on-cc7-p (dd/includes? (system-name) "cc7")
  "For checking if on cc7 box.")

(defconst dd/on-grads-18-p (dd/includes? (system-name) "grads-18")
  "For checking if on grads-18 box.")

(setq initial-scratch-message
      (format ";; This is GNU Emacs %s\n\n" emacs-version))

(setq echo-keystrokes 0.01
      ring-bell-function 'ignore
      visible-bell nil)

(defun yes-or-no-p-advice (orig-fun &rest args)
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

(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 192))

(when dd/on-abx-p
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :weight 'regular
                      :height 130))
(when dd/on-mac-p
  (when (boundp 'ns-antialias-text)
    (setq ns-antialias-text t))
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :weight 'regular
                      :height 120))
(when dd/on-cc7-p
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :weight 'regular
                      :height 130))
(when (and (or dd/on-abx-p dd/on-cc7-p) (fboundp 'set-fontset-font))
    (set-fontset-font t 'symbol
                      (font-spec :family "Noto Color Emoji")
                      nil 'prepend))
(when (and dd/on-mac-p (fboundp 'set-fontset-font))
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(defun dd/delete-and-kill-current ()
  "Delete buffer's current file and kill the buffer."
  (interactive)
  (delete-file buffer-file-name)
  (kill-buffer (buffer-name)))

(defun dd/move-file (new-loc)
  "Write this file to NEW-LOC, delete the old one.
Taken from post: https://zck.me/emacs-move-file"
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
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

;; (defun dd/del-trail-white ()
;;   "Add `delete-trailing-whitespace' to `write-file-functions'.
;; Since `write-file-functions' is a permanent local list, this is a
;; convenience function to add the `delete-trailing-whitespace'
;; function to that list. Should be added to a mode hook."
;;   (add-to-list 'write-file-functions 'delete-trailing-whitespace))
;; (add-hook 'text-mode-hook 'dd/del-trail-white)
;; (add-hook 'prog-mode-hook 'dd/del-trail-white)

(defun dd/delete-frame-or-window ()
  "If we have multiple frames delete the current one.
If only one delete the window; this is really just for binding
Command+w to behave similar to other macOS applications."
  (interactive)
  (if (< (count-windows) 2)
      (delete-frame)
    (delete-window)))

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

(defun dd/google-s (s)
  "Perform a google search S."
  (browse-url
   (format "https://google.com/search?q=%s"
           (url-hexify-string s))))

(defun dd/google-region ()
  "Google search the selection region."
  (interactive)
  (dd/google-s (buffer-substring (region-beginning) (region-end))))

(defun dd/google-something (s)
  "Perform an interactive google search of S."
  (interactive "sSearch: ")
  (dd/google-s s))

(defvar dd/llvm-bin-path
  (cond (dd/on-m1-p "/opt/homebrew/opt/llvm/bin")
        (dd/on-mac-p "/usr/local/opt/llvm/bin")
        (dd/on-cc7-p "/home/ddavis/software/specific/llvm/master/bin")
        (dd/on-grads-18-p "/home/drd25/software/specific/llvm/10.x/bin"))
  "Machine dependent llvm bin path.")

(defun dd/llvm-project-exe (exe-name)
  "Get full path of LLVM executable EXE-NAME."
  (when (and dd/llvm-bin-path (file-exists-p dd/llvm-bin-path))
    (concat (file-name-as-directory dd/llvm-bin-path) exe-name)))

(defvar dd/clangd-exe (dd/llvm-project-exe "clangd"))
(defvar dd/clang-format-exe (dd/llvm-project-exe "clang-format"))
(defvar dd/clang-exe (dd/llvm-project-exe "clang"))

;; sec02:
;; use-package setup

;; (require 'package)
;; (setq package-archives
;;       '(("melpa" . "https://melpa.org/packages/")
;;         ("gnu" . "https://elpa.gnu.org/packages/")))

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

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

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-hook-name-suffix nil))

(use-package straight-x)

(defun dd/straight-up ()
  "Pull and check straight.el packages."
  (interactive)
  (progn
    (straight-pull-all)
    (straight-check-all)))

;; sec03:
;; use-package for some core Emacs packages.

(use-package simple
  :config
  (column-number-mode +1))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer-other-window)
         :map ibuffer-mode-map
         ("q" . kill-buffer-and-window))
  :init
  (setq ibuffer-expert t))

(use-package display-line-numbers
  :init
  (make-variable-buffer-local 'display-line-numbers-width-start)
  :config
  (global-display-line-numbers-mode))

(use-package org
  :straight (org :type built-in)
  :init
  (setq org-src-fontify-natively t)
  :hook (org-mode-hook . (lambda () (interactive)
                           (setq-local display-line-numbers-width-start
                                       t)))
  :config
  (setq org-structure-template-alist
        (append org-structure-template-alist
                '(("el" . "src emacs-lisp :results silent")
                  ("py" . "src python :results silent")
                  ("cpp" . "src C++"))))
  (when dd/on-mac-p
    (bind-key "<A-down>" 'org-move-subtree-down org-mode-map)
    (bind-key "<A-up>" 'org-move-subtree-up org-mode-map)
    (bind-key "<A-left>" 'org-promote-subtree)
    (bind-key "<A-right>" 'org-demote-subtree))

  (unless dd/on-mac-p
    (bind-key "<s-down>" 'org-move-subtree-down org-mode-map)
    (bind-key "<s-up>" 'org-move-subtree-up org-mode-map)
    (bind-key "<s-left>" 'org-promote-subtree)
    (bind-key "<s-right>" 'org-demote-subtree)))

(if (< emacs-major-version 28)
    (use-package project :straight t)
  (use-package project))

(if (< emacs-major-version 28)
    (progn
      (use-package eldoc
        :straight t
        :config
        (load "eldoc")))
  (use-package eldoc))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-style '(face tabs empty trailing)))

(use-package autorevert
  :init
  (global-auto-revert-mode +1))

(use-package esh-mode
  :hook (eshell-mode-hook . (lambda () (display-line-numbers-mode 0))))

(use-package term
  :hook (term-mode-hook . (lambda () (display-line-numbers-mode 0))))

(use-package shell
  :hook (shell-mode-hook . (lambda () (display-line-numbers-mode))))

(use-package help-mode
  :bind
  (:map help-mode-map
        ("q" . kill-buffer-and-window)))

(when (or dd/on-mac-p dd/on-cc7-p dd/on-grads-18-p dd/on-abx-p)
  (use-package auth-source
    :init
    (setq auth-sources
          (list (concat user-emacs-directory ".authinfo.gpg")))))

(when (or dd/on-mac-p dd/on-cc7-p dd/on-grads-18-p dd/on-abx-p)
  (use-package epa-file
    :config
    (custom-set-variables
     `(epg-gpg-program
       ,(cond (dd/on-m1-p "/opt/homebrew/bin/gpg")
              (dd/on-mac-p "/usr/local/bin/gpg")
              (t "/usr/bin/gpg2"))))))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-strip-common-suffix t
        uniquify-after-kill-buffer-p t))

(use-package tramp
  :defer 5
  :config
  (setq tramp-default-method "ssh")
  (defun dd/cleanup-tramp ()
    (interactive)
    (tramp-cleanup-all-buffers)
    (tramp-cleanup-all-connections)
    (find-file "~/.")
    (switch-to-buffer "*scratch*")))

(use-package vc
  :init
  (setq vc-follow-symlinks t))

(use-package dired
  :demand t
  :init
  (setq-default dired-listing-switches "-alh")
  :bind (:map dired-mode-map
              ("q" . #'kill-current-buffer))
  :config
  (setq dired-recursive-copies 'always))

(use-package paren
  :init
  (show-paren-mode 1)
  (setq-default show-paren-delay 0))

(use-package browse-url
  :init
  (when dd/on-cc7-p
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "/usr/local/bin/firefox")))

(use-package sh-script
  :custom (sh-basic-offset 4))

(use-package elisp-mode
  :hook ((emacs-lisp-mode-hook . prettify-symbols-mode)))

;; (use-package cc-mode
;;   :demand t
;;   :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
;;          ("\\.icc\\'" . c++-mode)))

(use-package python
  :load-path "~/.emacs.d/dot-emacs/site-lisp"
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map
              ("C-c C-a" . dd/py-auto-lsp))
  :init
  (setq python-indent-guess-indent-offset nil)
  :config
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
          (lsp))))))

(use-package ispell
  :config
  (when dd/on-mac-p
    (setq ispell-program-name
          (cond (dd/on-m1-p "/opt/homebrew/bin/hunspell")
                (t "/usr/local/bin/hunspell")))))

(use-package flyspell
  :hook ((org-mode-hook . flyspell-mode)
         (LaTeX-mode-hook . flyspell-mode)
         (markdown-mode-hook . flyspell-mode)
         (message-mode-hook . flyspell-mode)
         (mu4e-compose-mode-hook . flyspell-mode)))

;; (use-package flymake
;;   :hook (emacs-lisp-mode-hook . flymake-mode))

;; sec04:
;; third party

;; (use-package auto-package-update
;;   :straight t
;;   :init
;;   (setq auto-package-update-delete-old-versions t))

(use-package eros
  :straight t
  :hook (emacs-lisp-mode-hook . eros-mode))

(use-package exec-path-from-shell
  :straight t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(use-package crux
  :straight t)

(use-package visual-fill-column
  :straight t)

;; (use-package hydra
;;   :straight t)

;; (use-package pretty-hydra
;;   :straight t)

;; (use-package helm
;;   :straight t
;;   :demand t
;;   :bind (:map helm-map
;;          ("TAB" . helm-execute-persistent-action)
;;          ("<tab>" . helm-execute-persistent-action))
;;   :bind-keymap ("C-x c" . helm-command-map)
;;   :config
;;   (setq history-delete-duplicates t)
;;   (setq helm-display-buffer-default-height 30
;;         helm-display-buffer-height 20
;;         helm-split-window-inside-p t
;;         helm-split-window-default-side 'below
;;         helm-input-idle-delay 0.01)
;;   (helm-mode +1)
;;   (defun dd/helm-rg-dwim (arg)
;;     "Call `helm-grep-ag' from `projectile-project-root' or `default-directory'."
;;     (interactive "P")
;;     (let ((proj (projectile-project-root)))
;;       (if proj
;;           (helm-grep-ag (expand-file-name proj) arg)
;;         (helm-grep-ag (expand-file-name default-directory) arg)))))

;; (use-package helm-command
;;   :after helm
;;   :bind (("M-x" . helm-M-x)))

;; (use-package helm-ring
;;   :after helm
;;   :bind (("M-y" . helm-show-kill-ring)))

;; (use-package helm-files
;;   :after helm
;;   :bind (("C-x C-f" . helm-find-files)
;;          ("C-x C-t" . find-file))
;;   :custom-face
;;   (helm-ff-file-extension ((t (:foreground "orange"))))
;;   :config
;;   (setq helm-ff-cache-mode-lighter ""
;;         helm-ff-cache-mode-lighter-sleep ""))

;; (use-package helm-buffers
;;   :after helm
;;   :bind (("C-x b" . helm-mini))
;;   :init
;;   (setq recentf-max-saved-items 30)
;;   :config
;;   (dolist (regexp '("\\*helm" "\\*lsp" "\\*EGLOT" "\\*straight" "\\*Flymake"
;;                     "\\*eldoc" "\\*Compile-Log" "\\*xref" "\\*company"
;;                     "\\*Warnings" "\\*Backtrace"))
;;     (add-to-list 'helm-boring-buffer-regexp-list regexp)))

;; (use-package helm-grep
;;   :after helm
;;   :config
;;   (setq helm-grep-file-path-style 'relative)
;;   (setq helm-grep-ag-command (concat (executable-find "rg")
;;                                      " --color=always"
;;                                      " --smart-case"
;;                                      " --no-heading"
;;                                      " --line-number %s %s %s")))

;; (use-package helm-descbinds
;;   :straight t
;;   :commands helm-descbinds)

;; (use-package helm-projectile
;;   :straight t
;;   :after (helm projectile))

;; (use-package helm-circe
;;   :straight t
;;   :after circe
;;   :bind (:map helm-command-map ("i" . helm-circe)))

;; (setq helm-mode-no-completion-in-region-in-modes
;;       '(circe-channel-mode
;;         circe-query-mode
;;         circe-server-mode))

;; (use-package ivy
;;   :straight t
;;   :config
;;   (setq ivy-height 15
;;         ivy-fixed-height-minibuffer t)
;;   (ivy-mode +1))

;; (use-package counsel
;;   :straight t
;;   :config
;;   (defun dd/counsel-rg-dwim (arg)
;;     "Call `counsel-rg' from project root or `default-directory' with ARG."
;;     (interactive "P")
;;     (let ((proj (projectile-project-root)))
;;       (if proj
;;           (counsel-rg "" (expand-file-name proj) nil arg)
;;         (counsel-rg "" (expand-file-name default-directory) nil arg))))
;;   (counsel-mode +1))

(use-package projectile
  :straight t
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("r" . dd/ripgrep-proj-or-dir)
              ("g" . consult-ripgrep))
  :config
  (projectile-mode +1))

(when (executable-find "fd")
  (setq projectile-git-command "fd . -H -0 --type f --color=never"))
(setq projectile-track-known-projects-automatically t
      projectile-globally-ignored-file-suffixes '("#" "~" ".o" ".so" ".elc" ".pyc")
      projectile-globally-ignored-directories '(".git" "__pycache__")
      projectile-globally-ignored-files '(".DS_Store")
      projectile-enable-caching nil)

(when (or dd/on-mac-p dd/on-grads-18-p)
  (setq projectile-project-search-path
        '("~/software/repos/" "~/atlas/analysis/")))
(when (or dd/on-abx-p dd/on-cc7-p)
  (setq projectile-project-search-path
        '("~/software/repos/" "/ddd/atlas/analysis/")))

(use-package selectrum
  :straight t
  :demand t
  :init
  (setq selectrum-extend-current-candidate-highlight t)
  (setq selectrum-num-candidates-displayed 15)
  (setq selectrum-fix-minibuffer-height t)
  :custom-face
  (selectrum-primary-highlight ((t (:weight bold :foreground "#fe8019"))))
  ;; (selectrum-primary-highlight ((t (:weight bold :foreground "#cc241d"))))
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :straight t
  :after selectrum
  :config
  (selectrum-prescient-mode +1))

;; (use-package icomplete-vertical
;;   :straight t
;;   :demand t
;;   :init
;;   ;;(completion-styles '(partial-completion substring))
;;   ;;(completion-category-overrides '((file (styles basic substring))))
;;   (setq read-file-name-completion-ignore-case t)
;;   (setq read-buffer-completion-ignore-case t)
;;   (setq completion-ignore-case t)
;;   :config
;;   (fido-mode -1)
;;   (icomplete-mode +1)
;;   (icomplete-vertical-mode +1)
;;   :bind (:map icomplete-minibuffer-map
;;               ("<tab>" . icomplete-force-complete)
;;               ("<return>" . icomplete-force-complete-and-exit)
;;               ("<down>" . icomplete-forward-completions)
;;               ("C-n" . icomplete-forward-completions)
;;               ("<up>" . icomplete-backward-completions)
;;               ("C-p" . icomplete-backward-completions)
;;               ("C-v" . icomplete-vertical-toggle)))

(use-package consult
  :straight t
  :after selectrum
  :bind (("C-c l" . consult-line)
         ("C-c r" . consult-ripgrep)
         ("C-x b" . consult-buffer))
  :config
  (setq consult-preview-key 'nil)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package recentf
  :after consult
  :config
  (recentf-mode +1)
  (add-to-list 'recentf-exclude 'file-remote-p))

(use-package marginalia
  :straight t
  :demand t
  :config
  (marginalia-mode +1)
  (setq marginalia-annotators '(marginalia-annotators-heavy
                                marginalia-annotators-light)))

(use-package company
  :straight t
  :demand t
  :bind
  (:map company-active-map
        ("TAB" . company-complete-selection)
        ("<tab>" . company-complete-selection))
  :hook ((cider-repl-mode-hook . company-mode)
         (clojure-mode-hook . company-mode)
         (conf-colon-mode-hook . company-mode)
         (conf-toml-mode-hook . company-mode)
         (eglot-managed-mode-hook . company-mode)
         (emacs-lisp-mode-hook . company-mode)
         (LaTeX-mode-hook . company-mode)
         (lsp-mode-hook . company-mode)
         (mu4e-compose-mode-hook . company-mode)
         (python-mode-hook . company-mode)
         (sh-mode-hook . company-mode)
         (yaml-mode-hook . company-mode))
  :config
  (make-variable-buffer-local 'company-minimum-prefix-length)
  (make-variable-buffer-local 'company-idle-delay)
  (setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  (setq-default company-minimum-prefix-length 2)
  (setq-default company-idle-delay 0.2)
  (defun dd/company-prog-mode  ()
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.1))
  (defun dd/company-text-mode ()
    (setq company-minimum-prefix-length 3
          company-idle-delay 0.3))
  (add-hook 'text-mode-hook #'dd/company-text-mode)
  (add-hook 'prog-mode-hook #'dd/company-prog-mode))

;; (use-package orderless
;;   :straight t
;;   :demand t
;;   :custom (completion-styles '(basic orderless emacs22 partial-completion flex)))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ("q" . dd/magit-kill-buffers))
  :config
  (defun dd/magit-kill-buffers ()
    "See `https://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/'"
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers))))

(use-package rg
  :straight t
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

(use-package flycheck
  :straight t)

(use-package lsp-mode
  :straight t
  :commands lsp
  ;; :bind (:map lsp-mode-map
  ;;             ("C-c l" . hydra-lsp/body))
  :init
  (setq lsp-clients-clangd-executable dd/clangd-exe)
  (setq read-process-output-max (* 10 1024 1024))
  :config
  (setq lsp-keep-workspace-alive nil
        lsp-auto-guess-root nil
        lsp-enable-on-type-formatting nil))

(use-package lsp-pyls
  :config
  (setq lsp-pyls-plugins-autopep8-enabled nil
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-flake8-enabled t
        lsp-pyls-plugins-pydocstyle-enabled t
        lsp-pyls-configuration-sources ["flake8"]))

(use-package lsp-ui
  :straight t
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-header t
        lsp-ui-doc-max-height 25
        lsp-ui-doc-max-width 92
        lsp-ui-sideline-show-hover nil))

(use-package eglot
  :straight t
  :commands eglot
  :init
  (setq eglot-server-programs
        `((python-mode "pyls")
          ((c++-mode c-mode) ,dd/clangd-exe)))
  :config
  (setq eglot-autoshutdown t))

(use-package eldoc-box
  :straight t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
  :init
  (setq eldoc-box-clear-with-C-g t
        eldoc-box-fringe-use-same-bg nil))

(use-package clang-format
  :straight t
  :custom
  (clang-format-executable dd/clang-format-exe))

(use-package modern-cpp-font-lock
  :straight t
  :hook (c++-mode-hook . modern-c++-font-lock-mode))

(use-package pyvenv
  :straight t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package blacken
  :straight t
  :after python)

(when (or dd/on-mac-p dd/on-cc7-p dd/on-abx-p)
  (use-package cider
    :straight t
    :commands cider-jack-in))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package which-key
  :straight t
  :demand t
  :init
  :config
  (which-key-mode)
  (setq which-key-side-window-max-height 0.40
        which-key-frame-max-height 40))

(use-package which-key-posframe
  :straight t
  :after which-key
  :config
  (which-key-posframe-mode +1))

(use-package yasnippet
  :straight t
  :demand t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t)

(use-package cmake-mode
  :load-path "~/.emacs.d/dot-emacs/site-lisp")

(use-package iedit
  :straight t
  :bind ("C-c ;" . iedit-mode))

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'")

(use-package yaml-mode
  :straight t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window))

(use-package helpful
  :straight t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ("C-h o" . helpful-symbol)
         ("C-h ." . helpful-at-point)
         :map helpful-mode-map
         ("q" . kill-buffer-and-window))
  :config
  (setq helpful-max-highlight 15000))

(defun dd/kill-theme ()
  "Disable any custom themes."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

;; (use-package doom-themes
;;   :straight t
;;   :demand t
;;   :config
;;   (load-theme 'doom-gruvbox t)
;;   (set-face-attribute 'font-lock-doc-face nil :foreground "#a89984"))

(use-package gruvbox
  :straight (gruvbox :host github :repo "greduan/emacs-theme-gruvbox")
  :demand t
  :custom-face
  (font-lock-doc-face ((t (:foreground "#928374"))))
  :config
  (load-theme 'gruvbox t))

;;  (set-face-attribute 'font-lock-doc-face nil :slant 'italic :foreground "#928374"))

(use-package elfeed
  :straight t
  :commands elfeed
  :bind (("C-x w" . 'elfeed)
         :map elfeed-show-mode-map
         ("V" . 'visual-fill-column-mode))
  :init
  (setq shr-use-fonts nil)
  (setq elfeed-feeds
        '(("https://planet.scipy.org/feed.xml" python)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://ddavis.io/index.xml" blog)
          ("http://pragmaticemacs.com/feed/" emacs)
          ("https://feeds.megaphone.fm/ESP8794877317" fivethirtyeight podcast)
          ("http://feeds.podtrac.com/zKq6WZZLTlbM" nyt podcast)))
  :config
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "3 weeks ago" :remove 'unread))
  (setq-default elfeed-search-filter "@21-days-ago"))

(when (or dd/on-grads-18-p dd/on-cc7-p dd/on-mac-p dd/on-abx-p)
  (use-package circe
    :straight t
    :commands circe
    :hook (circe-chat-mode-hook . dd/circe-prompt)
    :init
    (require 'auth-source-pass)
    (defun dd/irc-pw-freenode (server)
      (auth-source-pass-get 'secret "Freenode"))
    (defun dd/irc-pw-gitter (server)
      (auth-source-pass-get 'secret "Gitter"))
    (defun dd/circe-prompt ()
      (lui-set-prompt
       (propertize (format "%s >>> " (buffer-name)) 'face 'circe-prompt-face)))
    (setq circe-network-options
          '(("Freenode"
             :nick "ddavis"
             :nickserv-password dd/irc-pw-freenode
             :channels (:after-auth
                        "#emacs"
                        "#python"
                        "#sr.ht"
                        "#lobsters"
                        "##crustaceans")
             :tls t)))
    :config
    (setq circe-use-cycle-completion t
          circe-reduce-lurker-spam t
          circe-format-say "<{nick}> {body}"
          lui-fill-type 19
          lui-fill-column 77)
    (setq circe-default-part-message
          (concat "Closed Circe (" circe-version ") buffer in GNU Emacs (" emacs-version ")"))
    (setq circe-default-quit-message
          (concat "Quit Circe (" circe-version ") in GNU Emacs (" emacs-version ")"))
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
    (setq circe-color-nicks-pool-type
          '("#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#fe8019"
            "#cc241d" "#98971a" "#d79921" "#458588" "#b16286" "#689d6a" "#d65d0e"))
    (setq circe-color-nicks-everywhere t)
    (enable-circe-color-nicks))

  (use-package erc
    :commands erc
    :init
    (setq erc-prompt-for-password nil)
    (setq erc-user-full-name "Doug Davis")
    (setq erc-rename-buffers t)
    :config
    (setq erc-track-enable-keybindings nil)
    (setq erc-kill-buffer-on-part t)
    (setq erc-kill-server-buffer-on-quit t)
    (setq erc-fill-function 'erc-fill-static)
    (setq erc-fill-static-center 19)
    (setq erc-prompt (lambda () (concat (buffer-name) " >")))
    (setq erc-hide-list '("JOIN" "PART" "QUIT"))
    (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
    (setq erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                    "324" "329" "332" "333" "353" "477"))
    (add-to-list 'erc-modules 'notifications)
    (add-to-list 'erc-modules 'spelling)

    (setq-default dd/erc-colors-list
                  '("#fb4934" "#b8bb26" "#fabd2f"
                    "#83a598" "#d3869b" "#8ec07c"
                    "#fe8019" "#cc241d" "#98971a"
                    "#d79921" "#458588" "#b16286"
                    "#689d6a" "#d65d0e"))

    ;; special colors for some people
    (setq dd/erc-nick-color-alist '(("John" . "blue")
        		            ("Bob" . "red")))

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
                               (modify-syntax-entry ?\- "w" nil)))))

(when (or dd/on-mac-p dd/on-cc7-p dd/on-abx-p)
  (use-package tex-site
    :straight auctex
    :mode ("\\.tex\\'" . TeX-latex-mode)
    :init
    (setq font-latex-fontify-sectioning 'color
          font-latex-fontify-script nil
          TeX-source-correlate-mode 'synctex
          TeX-source-correlate-start-server t)
    (setq-default TeX-master nil)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    :config
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

  ;; (use-package company-reftex
  ;;   :ensure t)

  ;; (use-package company-math
  ;;   :ensure t)

  ;; (defun dd/company-math ()
  ;;   (interactive)
  ;;   (setq-local company-backends
  ;;               (append '((company-math-symbols-latex company-latex-commands))
  ;;                       company-backends)))

  (when dd/on-cc7-p
    (use-package lsp-latex
      :straight t
      :init
      (setq lsp-latex-texlab-executable
            "/home/ddavis/software/repos/texlab/target/release/texlab")))

  (when dd/on-cc7-p
    (setenv "PKG_CONFIG_PATH" "/usr/lib64/pkgconfig"))

  (when (and window-system (not dd/on-m1-p))
    (use-package pdf-tools
      :straight t
      :hook (pdf-view-mode-hook . (lambda () (display-line-numbers-mode 0)))
      :config
      (pdf-tools-install)
      (setq-default pdf-view-display-size 'fit-page)
      (when dd/on-mac-p
        (setq pdf-view-use-scaling t)
        (setq pdf-view-use-imagemagick nil))
      (setq TeX-view-program-selection '((output-pdf "PDF Tools"))))))

(use-package ox-hugo
  :straight t
  :after ox)

;; (use-package htmlize
;;   :straight t
;;   :after ox)

(use-package debbugs
  :straight t
  :defer 20)

(use-package w3m
  :straight t
  :config
  (setq w3m-default-display-inline-images t))

;; sec05:
;; some package-free bindings and macOS specifics

(bind-key (kbd "C-x \\") #'dd/toggle-window-split)

(bind-key (kbd "C-w")
          (lambda ()
            (interactive)
            (if (region-active-p)
                (kill-region (region-beginning) (region-end))
              (dd/delete-frame-or-window))))

(when dd/on-mac-p
  (when (memq window-system '(mac ns))
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)
    (setq-default ns-alternate-modifier 'meta)
    (setq-default mac-option-modifier 'meta)
    (setq-default ns-right-alternate-modifier nil)
    (setq-default ns-command-modifier 'super)
    (setq-default mac-command-modifier 'super)
    (setq-default ns-function-modifier 'hyper)
    (setq-default mac-function-modifier 'hyper))

  ;; (define-key global-map [s-right] 'move-end-of-line)
  ;; (define-key global-map [s-left] 'move-beginning-of-line)
  (bind-key* (kbd "s-<right>") #'right-word)
  (bind-key* (kbd "s-<left>") #'left-word)
  (bind-key* (kbd "s-d") #'dd/kill-theme)
  (bind-key* (kbd "s-\\") #'dd/toggle-window-split)
  (bind-key* (kbd "s-\"") #'dd/google-something)
  (bind-key* (kbd "s-/") #'previous-buffer)
  (bind-key* (kbd "s-1") #'delete-other-windows)
  (bind-key* (kbd "s-2") #'split-window-below)
  (bind-key* (kbd "s-3") #'split-window-right)
  (bind-key* (kbd "s-5") #'projectile-find-file-in-known-projects)
  (bind-key* (kbd "s-4") #'mu4e)
  (bind-key* (kbd "s-f") #'find-file)
  (bind-key* (kbd "s-b") #'consult-buffer)
  (bind-key* (kbd "s-g") #'magit-status)
  (bind-key* (kbd "s-i") (lambda () (interactive) (find-file user-init-file)))
  (bind-key* (kbd "s-o") #'other-window)
  (bind-key* (kbd "s-p") #'projectile-command-map)
  (bind-key* (kbd "s-r") #'consult-ripgrep)
  (bind-key* (kbd "s-u") #'gnus)
  (bind-key* (kbd "s-*") 'dd/kill-all-buffers)
  (bind-key* (kbd "s-w") #'dd/delete-frame-or-window))

;; sec06:
;; email setup is in dedicated file

(when (or dd/on-mac-p dd/on-cc7-p dd/on-abx-p)
  (load-file "~/.emacs.d/dot-emacs/email.el"))

;; sec07:
;; misc

(defun dd/kill-all-buffers ()
  "Kill all buffers except scratch and Messages."
  (interactive)
  (mapcar
   (lambda (b)
     (let ((bn (buffer-name b)))
       (unless
           (or (string= "*scratch*" bn)
               (string= "*Messages*" bn))
         (kill-buffer b))))
   (buffer-list)))

;; 128MB garbage collection threshold
(setq gc-cons-threshold (* 128 1024 1024))

(provide 'init)
;;; init.el ends here

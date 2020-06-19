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

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

;; for native-comp branch
(when (fboundp 'native-compile-async)
  (if (yes-or-no-p "async compile?")
      (setq comp-async-jobs-number 4
            comp-deferred-compilation t
            comp-deferred-compilation-black-list '())
    (setq comp-deferred-compilation nil)))

(setq user-mail-address "ddavis@ddavis.io"
      user-login-name "ddavis"
      user-full-name "Doug Davis")

(setq default-directory
      (file-name-directory
       (directory-file-name user-emacs-directory)))

(setq custom-file (concat user-emacs-directory "custom.el"))

;; 1GB threshold while init is loaded
(setq gc-cons-threshold (* 1000 1024 1024))

;; sec01:
;; general setup not associated with packages

(defun dd/str-contains? (subs s)
  "Check for SUBS in S via `string-match-p'."
  (declare (pure t) (side-effect-free t))
  (not (null (string-match-p (regexp-quote subs) s))))

(defconst dd-on-mac (eq system-type 'darwin)
  "For checking if on a mac.")

(defconst dd-on-abx (dd/str-contains? "abx" (system-name))
  "For checking of in abx box.")

(defconst dd-on-cc7 (dd/str-contains? "cc7" (system-name))
  "For checking if on cc7 box.")

(defconst dd-on-grads-18 (dd/str-contains? "grads-18" (system-name))
  "For checking if on grads-18 box.")

(defconst dd-on-spar (dd/str-contains? "spar01" (system-name))
  "For checking if on a BNL SPAR machine.")

(setq initial-scratch-message
      (format ";; This is GNU Emacs %s\n\n" emacs-version))

(setq echo-keystrokes 0.01
      ring-bell-function 'ignore
      visible-bell nil)

(setq-default indent-tabs-mode nil)

(setq auto-save-list-file-prefix nil
      create-lockfiles nil
      auto-save-list-file-prefix nil
      backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 1
      version-control t)

(setq sentence-end-double-space nil)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(when (fboundp 'menu-bar-mode)
  (if dd-on-mac
      (menu-bar-mode +1)
    (menu-bar-mode -1)))

(setq inhibit-startup-screen t)

(column-number-mode +1)
(make-variable-buffer-local 'display-line-numbers-width-start)
(global-display-line-numbers-mode)

(add-to-list 'default-frame-alist '(height . 72))
(add-to-list 'default-frame-alist '(width . 234))

(when dd-on-abx
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :weight 'regular
                      :height 130))
(when dd-on-mac
  (setq mac-allow-anti-aliasing t)
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :weight 'regular
                      :height 120))
(when dd-on-cc7
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :weight 'regular
                      :height 130))

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

(setq require-final-newline t)

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

(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

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

;; sec02:
;; use-package setup


(require 'package)
(if dd-on-spar
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(when (< emacs-major-version 27)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-hook-name-suffix nil))

;; sec03:
;; use-package for some core Emacs packages.

(use-package org
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
  (when dd-on-mac
    (bind-key "<A-down>" 'org-move-subtree-down org-mode-map)
    (bind-key "<A-up>" 'org-move-subtree-up org-mode-map)
    (bind-key "<A-left>" 'org-promote-subtree)
    (bind-key "<A-right>" 'org-demote-subtree))

  (unless dd-on-mac
    (bind-key "<s-down>" 'org-move-subtree-down org-mode-map)
    (bind-key "<s-up>" 'org-move-subtree-up org-mode-map)
    (bind-key "<s-left>" 'org-promote-subtree)
    (bind-key "<s-right>" 'org-demote-subtree)))

(use-package project)

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-style '(face tabs empty trailing)))

(use-package help-mode
  :bind
  (:map help-mode-map
        ("q" . kill-buffer-and-window)))

(when (or dd-on-mac dd-on-cc7 dd-on-grads-18 dd-on-abx)
  (use-package auth-source
    :init
    (setq auth-sources
          (list (concat user-emacs-directory ".authinfo.gpg")))))

(when (or dd-on-mac dd-on-cc7 dd-on-grads-18 dd-on-abx)
  (use-package epa-file
    :config
    (epa-file-enable)
    (if dd-on-mac
        (custom-set-variables '(epg-gpg-program "/usr/local/bin/gpg"))
      (custom-set-variables '(epg-gpg-program "/usr/bin/gpg2")))))

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
    (dd/scratch-buffer)))

(use-package vc
  :init
  (setq vc-follow-symlinks t))

(use-package dired
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
  (when dd-on-cc7
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "/usr/local/bin/firefox")))

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.icc\\'" . c++-mode))
  :init
  (defun dd/llvm-project-exe (exe-name)
    (when (and dd-llvm-bin-path (file-exists-p dd-llvm-bin-path))
      (concat (file-name-as-directory dd-llvm-bin-path) exe-name)))
  (defvar dd-llvm-bin-path
    (cond (dd-on-mac "/usr/local/opt/llvm/bin")
          (dd-on-cc7 "/home/ddavis/software/specific/llvm/master/bin")
          (dd-on-grads-18 "/home/drd25/software/specific/llvm/10.x/bin")
          (dd-on-spar nil))
    "Machine dependent llvm bin path.")
  (defvar dd-clangd-exe (dd/llvm-project-exe "clangd"))
  (defvar dd-clang-format-exe (dd/llvm-project-exe "clang-format"))
  (defvar dd-clang-exe (dd/llvm-project-exe "clang"))
  :config
  (when dd-on-spar
    (defun dd/cpp-fix-backspace ()
      (global-set-key (kbd "C-d") 'delete-backward-char)
      (local-unset-key (kbd "C-d")))
    (add-hook 'c++-mode-hook #'dd/cpp-fix-backspace)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map
              ("C-c C-a" . dd/py-auto-lsp))
  :config
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

(use-package flyspell
  :hook ((org-mode-hook . flyspell-mode)
         (LaTeX-mode-hook . flyspell-mode)
         (markdown-mode-hook . flyspell-mode)
         (message-mode-hook . flyspell-mode)
         (mu4e-compose-mode-hook . flyspell-mode)))

;; sec04:
;; third party

(use-package auto-package-update
  :ensure t
  :init
  (setq auto-package-update-delete-old-versions t))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(use-package crux
  :ensure t)

(use-package visual-fill-column
  :ensure t)

(use-package hydra
  :ensure t)

(use-package pretty-hydra
  :ensure t)

(use-package helm
  :ensure t
  :demand t
  :bind (("M-x" . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-x C-t" . find-file)
         ("C-x b" . helm-buffers-list)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action))
  :config
  (require 'helm-config)
  (global-set-key (kbd "C-x c") 'helm-command-prefix)
  (setq helm-autoresize-max-height 30
        helm-autoresize-min-height 20
        helm-split-window-inside-p t
        helm-split-window-default-side 'below
        helm-idle-delay 0.01
        helm-input-idle-delay 0.01
        helm-quick-update t
        helm-grep-file-path-style 'relative
        helm-ff-skip-boring-files t
        helm-grep-ag-command (concat (executable-find "rg")
                                     " --color=always"
                                     " --smart-case"
                                     " --no-heading"
                                     " --line-number %s %s %s"))
  (helm-mode +1)
  (helm-autoresize-mode 1))

(use-package helm-descbinds
  :ensure t
  :commands helm-descbinds)

(use-package projectile
  :ensure t
  :demand t
  :init
  (pretty-hydra-define hydra-projectile
    (:exit t :hint nil :title (projectile-project-root) :quit-key "q")
    ("Movement" (("b" projectile-switch-to-buffer               "switch")
                 ("B" projectile-switch-to-buffer-other-window  "switch (OW)")
                 ("f" projectile-find-file                      "file")
                 ("F" projectile-find-file-other-window         "file (OW)")
                 ("S" projectile-switch-project                 "switch project")
                 ("u" projectile-find-file-in-known-projects    "find in known"))

     "Search" (("r" dd/ripgrep-proj-or-dir  "ripgrep (rg.el)")
               ("s" dd/ripgrep-proj-or-dir  "ripgrep (rg.el)")
               ("o" projectile-multi-occur  "multioccur"))

     "Misc" (("a" projectile-add-known-project  "add to known")
             ("h" helm-projectile               "helm projectile")
             ("i" projectile-ibuffer            "ibuffer")
             ("k" projectile-kill-buffers       "Kill em"))))
  :bind ("C-c P" . hydra-projectile/body)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (when (executable-find "fd")
    (setq projectile-git-command "fd . -0 --type f --color=never"))
  (setq projectile-track-known-projects-automatically nil
        projectile-completion-system 'helm
        projectile-globally-ignored-file-suffixes '("#" "~" ".o" ".so" ".elc" ".pyc")
        projectile-globally-ignored-directories '(".git" "__pycache__")
        projectile-globally-ignored-files '(".DS_Store")
        projectile-enable-caching nil)
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t)

(use-package company
  :ensure t
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
  (setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1))

;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode-hook . company-box-mode))

(use-package magit
  :ensure t
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

(use-package flycheck
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq read-process-output-max (* 10 1024 1024))
  (setq lsp-clients-clangd-executable dd-clangd-exe)
  (setq lsp-prefer-capf t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-auto-guess-root nil)
  (setq lsp-diagnostic-package :flycheck)
  (setq lsp-pyls-plugins-autopep8-enabled nil
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-flake8-enabled t
        lsp-pyls-configuration-sources ["flake8"])
  (pretty-hydra-define hydra-lsp (:exit t :hint nil :quit-key "q")
    ("Finding" (("d" lsp-find-declaration             "find declaration")
                ("D" lsp-ui-peek-find-definitions     "peek find declaration")
                ("R" lsp-ui-peek-find-references      "peek find refs")
                ("i" lsp-ui-peek-find-implementation  "peek find implementation")
                ("t" lsp-find-type-defition           "find type definition"))

     "Misc" (("f" lsp-format-buffer            "format buffer")
             ("m" lsp-ui-imenu                 "ui menu")
             ("x" lsp-execute-code-action      "execeute code action")
             ("s" lsp-signature-help           "sig help")
             ("o" lsp-describe-thing-at-point  "describe thing at point")
             ("r" lsp-rename                   "rename"))

     "Session" (("M-s" lsp-describe-session   "describe session")
                ("M-r" lsp-workspace-restart  "restart workspace")
                ("S" lsp-workspace-shutdown   "shutdown workspace"))))
  :bind (:map lsp-mode-map
              ("C-c l" . hydra-lsp/body)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-max-width 92
        lsp-ui-sideline-show-hover nil))

(use-package eglot
  :ensure t
  :commands eglot
  :init
  (setq eglot-server-programs
        `((python-mode "pyls")
          ((c++-mode c-mode) ,dd-clangd-exe))))

(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode))

(use-package clang-format
  :ensure t
  :init
  (setq clang-format-executable dd-clang-format-exe))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode-hook . modern-c++-font-lock-mode))

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package blacken
  :ensure t)

(when (or dd-on-mac dd-on-cc7 dd-on-abx)
  (use-package cider
    :ensure t
    :commands cider-jack-in))

(use-package rainbow-delimiters
  :ensure t
  :hook (clojure-mode-hook . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))

;; (use-package doom-modeline
;;   :ensure t
;;   :demand t
;;   :init
;;   (setq doom-modeline-mu4e (or dd-on-mac dd-on-cc7))
;;   :config
;;   (doom-modeline-mode 1))

(use-package yasnippet
  :ensure t
  :demand t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" "\\.cmake\\'"))

(use-package iedit
  :ensure t
  :bind ("C-c ;" . iedit-mode))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package helpful
  :ensure t
  :init
  (setq helpful-max-highlight 15000)
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h o" . helpful-symbol)
         ("C-h ." . helpful-at-point)
         ("C-h k" . helpful-key)
         :map helpful-mode-map
         ("q" . kill-buffer-and-window)))

(use-package doom-themes
  :ensure t
  :demand t
  :init
  (setq custom-safe-themes t)
  :config
  (load-theme 'doom-gruvbox t))

;; (use-package gruvbox
;;   :ensure gruvbox-theme
;;   :demand t
;;   :config
;;   (setq custom-safe-themes t)
;;   (load-theme 'gruvbox t))

(use-package elfeed
  :ensure t
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
          ("http://feeds.podtrac.com/zKq6WZZLTlbM" nyt podcast)))
  :config
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "3 weeks ago" :remove 'unread))
  (setq-default elfeed-search-filter "@21-days-ago"))

(when (or dd-on-grads-18 dd-on-cc7 dd-on-mac dd-on-abx)
  (use-package password-store
    :ensure t
    :commands (password-store-copy
               password-store-get
               password-store-edit
               password-store-insert)))

(when (or dd-on-grads-18 dd-on-cc7 dd-on-mac dd-on-abx)
  (use-package circe
    :ensure t
    :commands circe
    :hook (circe-chat-mode-hook . dd/circe-prompt)
    :init
    (defun dd/irc-pw-freenode (server)
      (password-store-get "Freenode"))
    (defun dd/irc-pw-gitter (server)
      (password-store-get "Gitter"))
    (defun dd/circe-prompt ()
      (lui-set-prompt
       (propertize (format "%s >>> " (buffer-name)) 'face 'circe-prompt-face)))
    (setq circe-network-options
          '(("Freenode"
             :nick "ddavis"
             :nickserv-password dd/irc-pw-freenode
             :channels (:after-auth "#emacs" "#lobsters" "#lobsters-boil" "#sr.ht")
             :tls t)
            ("Gitter"
             :server-buffer-name "Gitter"
             :host "irc.gitter.im"
             :port "6697"
             :nick "douglasdavis"
             :pass dd/irc-pw-gitter
             :tls t)))
    :config
    (require 'circe-color-nicks)
    (setq circe-color-nicks-pool-type
          '("#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#fe8019"
            "#cc241d" "#98971a" "#d79921" "#458588" "#b16286" "#689d6a" "#d65d0e"))
    (enable-circe-color-nicks)
    (setq circe-use-cycle-completion t
          circe-reduce-lurker-spam t
          circe-format-say "<{nick}> {body}"
          lui-fill-type 19
          lui-fill-column 77
          circe-color-nicks-everywhere t)
    (setq helm-mode-no-completion-in-region-in-modes
          '(circe-channel-mode
            circe-query-mode
            circe-server-mode))
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

  (use-package helm-circe
    :ensure t
    :after circe
    :bind (:map helm-command-map ("i" . helm-circe))))

(use-package erc
  :when (or dd-on-grads-18 dd-on-cc7 dd-on-mac dd-on-abx)
  :commands erc
  :init
  (setq erc-prompt-for-password nil)
  (setq erc-user-full-name "Doug Davis")
  :config
  (setq erc-track-enable-keybindings nil)
  (setq erc-kill-buffer-on-part t)
  (setq erc-kill-server-buffer-on-quit t)
  (setq erc-fill-function 'erc-fill-static)
  (setq erc-fill-static-center 19)
  (setq erc-prompt (lambda () (concat (buffer-name) " >>>")))
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                  "324" "329" "332" "333" "353" "477"))
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (defvar dd-nick-face-list '()
    "See https://www.emacswiki.org/emacs/ErcNickColors#toc1")
  (defvar dd-erc-colors-list
    '("#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#fe8019"
      "#cc241d" "#98971a" "#d79921" "#458588" "#b16286" "#689d6a" "#d65d0e")
    "See https://www.emacswiki.org/emacs/ErcNickColors#toc1")
  (defun dd/build-nick-face-list ()
    "See https://www.emacswiki.org/emacs/ErcNickColors#toc1"
    (setq i -1)
    (setq dd-nick-face-list
          (mapcar
           (lambda (COLOR)
             (setq i (1+ i))
             (list (custom-declare-face
                    (make-symbol (format "erc-nick-face-%d" i))
                    (list (list t (list :foreground COLOR)))
                    (format "Nick face %d" i))))
           dd-erc-colors-list)))
  (defun dd/erc-insert-modify-hook ()
    "See https://www.emacswiki.org/emacs/ErcNickColors#toc1"
    (if (null dd-nick-face-list) (dd/build-nick-face-list))
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "<\\([^>]*\\)>")
          (let ((nick (match-string 1)))
            (put-text-property (match-beginning 1) (match-end 1)
                               'face (nth
                                      (mod (string-to-number
                                            (substring (md5 nick) 0 4) 16)
                                           (length dd-nick-face-list))
                                      dd-nick-face-list))))))
  (add-hook 'erc-insert-modify-hook 'dd/erc-insert-modify-hook))

(use-package gcmh
  :ensure t
  :demand t
  :init
  (gcmh-mode 1))

(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  (setq font-latex-fontify-sectioning 'color
        font-latex-fontify-script nil
        TeX-source-correlate-mode 'synctex
        TeX-source-correlate-start-server t)
  (setq-default TeX-master nil)
  :config
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(when (or dd-on-cc7 dd-on-abx)
  (when dd-on-abx
    (setenv "PKG_CONFIG_PATH" "/usr/lib64/pkgconfig"))
  (use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    (setq TeX-view-program-selection '((output-pdf "PDF Tools")))))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package ox-reveal
  :ensure t
  :after ox)

(use-package htmlize
  :ensure t
  :after ox)

;; (use-package dashboard
;;   :ensure t
;;   :init
;;   (setq dashboard-center-content t
;;         dashboard-startup-banner 'logo
;;         dashboard-set-navigator t
;;         dashboard-items '((recents . 5)
;;                           (projects . 5)))
;;   :config
;;   (dashboard-setup-startup-hook))

;; sec05:
;; some package-free bindings and macOS specifics

(bind-key (kbd "C-x \\") #'dd/toggle-window-split)

(bind-key (kbd "C-w") (lambda ()
                        (interactive)
                        (if (region-active-p)
                            (kill-region (region-beginning) (region-end))
                          (dd/delete-frame-or-window))))

(when dd-on-mac
  (when (memq window-system '(mac ns))
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)
    (setq-default ns-alternate-modifier 'meta)
    (setq-default mac-option-modifier 'meta)
    (setq-default ns-right-alternate-modifier nil)
    (setq-default ns-command-modifier 'super)
    (setq-default mac-command-modifier 'super)
    (setq-default ns-function-modifier 'hyper)
    (setq-default mac-function-modifier 'hyper))

  (bind-key (kbd "s-\\") #'dd/toggle-window-split)
  (bind-key (kbd "s-/") #'previous-buffer)
  (bind-key (kbd "s-1") #'delete-other-windows)
  (bind-key (kbd "s-2") #'split-window-below)
  (bind-key (kbd "s-3") #'split-window-right)
  (bind-key (kbd "s-5") #'projectile-find-file-in-known-projects)
  (bind-key (kbd "s-4") #'mu4e)
  (bind-key (kbd "s-b") #'helm-buffers-list)
  (bind-key (kbd "s-f") #'helm-find-files)
  (bind-key (kbd "s-g") #'magit-status)
  (bind-key (kbd "s-o") #'other-window)
  (bind-key (kbd "s-p") #'hydra-projectile/body)
  (bind-key (kbd "s-r") #'dd/ripgrep-proj-or-dir)
  (bind-key (kbd "s-u") #'gnus)
  (bind-key (kbd "s-w") #'dd/delete-frame-or-window))

;; sec06:
;; email setup is in dedicated file

(when (or dd-on-mac dd-on-cc7 dd-on-abx)
  (load-file "~/.emacs.d/dot-emacs/email.el"))

;; sec07:
;; experimenting

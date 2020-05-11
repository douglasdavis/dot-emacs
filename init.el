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

;; for native-comp branch

(setq initial-scratch-message
      (format ";; This is GNU Emacs %s\n\n" emacs-version))

(defun dd/str-contains? (subs s &optional ignore-case)
  (declare (pure t) (side-effect-free t))
  (not (null (string-match-p (regexp-quote subs) (system-name)))))

(defconst dd-on-mac (eq system-type 'darwin)
  "true if on a mac")

(defconst dd-on-cc7 (dd/str-contains? "cc7" (system-name))
  "true if on cc7 box")

(defconst dd-on-grads-18 (dd/str-contains? "grads-18" (system-name))
  "true if on grads-18 box")

(defconst dd-on-spar (dd/str-contains? "spar01" (system-name))
  "true if on a BNL SPAR machine")

;; for native-comp branch
(setq comp-async-jobs-number 5
      comp-deferred-compilation t
      comp-deferred-compilation-black-list '("cal-menu.el"
                                             "cc-mode.el"
                                             "cider-browse-ns.el"
                                             "flycheck.el"
                                             "gnus.el"
                                             "gnus-art.el"
                                             "gnus-sum.el"
                                             "help-mode.el"
                                             "lsp-mode.el"
                                             "markdown-mode.el"
                                             "mml.el"
                                             "org.el"
                                             "org-table.el"
                                             "yasnippet.el"
                                             "util-modes.el"))

;; 2GB threshold while init is loaded
(setq gc-cons-threshold (* 2000 1024 1024))

(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

(when dd-on-spar
  (global-set-key (kbd "C-d") 'delete-backward-char))

(defconst dd-enable-irc (or dd-on-cc7 dd-on-mac dd-on-grads-18)
  "true if on machine where we want to be able to use IRC")

(defvar dd-llvm-bin-path
  (cond (dd-on-mac "/usr/local/opt/llvm/bin")
        (dd-on-cc7 "/home/ddavis/software/specific/llvm/10.x/bin")
        (dd-on-grads-18 "/home/drd25/software/specific/llvm/10.x/bin")
        (dd-on-spar nil))
  "machine dependent llvm bin path")

(defvar dd-clangd-exe
  (if dd-llvm-bin-path
      (if (file-exists-p dd-llvm-bin-path)
	  (concat (file-name-as-directory dd-llvm-bin-path) "clangd")
	nil)
    nil))

(defvar dd-clang-format-exe
  (if dd-llvm-bin-path
      (if (file-exists-p dd-llvm-bin-path)
	  (concat (file-name-as-directory dd-llvm-bin-path) "clang-format")
	nil)
    nil))

(defvar dd-clang-exe
  (if dd-llvm-bin-path
      (if (file-exists-p dd-llvm-bin-path)
	  (concat (file-name-as-directory dd-llvm-bin-path) "clang")
	nil)
    nil))

(defconst dd-rg-exe
  (cond (dd-on-mac "/usr/local/bin/rg")
        (dd-on-cc7 "/home/ddavis/.cargo/bin/rg")
        (dd-on-grads-18 "/home/drd25/.cargo/bin/rg")
        (dd-on-spar "~/.bin/rg"))
  "machine dependent ripgrep executable string")

(defconst dd-fd-exe
  (cond (dd-on-mac "/usr/local/bin/fd")
        (dd-on-cc7 "/home/ddavis/.cargo/bin/fd")
        (dd-on-grads-18 "/home/drd25/.cargo/bin/fd")
        (dd-on-spar nil))
  "machine dependent fd executable string")

(setq default-directory (cond (dd-on-mac "/Users/ddavis/")
                              (dd-on-cc7 "/home/ddavis/")
                              (dd-on-grads-18 "/home/drd25/")
                              (dd-on-spar "/usatlas/u/ddavis/")))

(fset 'yes-or-no-p 'y-or-n-p)

(setq custom-file (concat user-emacs-directory "custom.el"))

(setq user-mail-address "ddavis@ddavis.io"
      user-login-name "ddavis"
      user-full-name "Doug Davis")

(require 'package)
(if dd-on-spar
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(when (version< emacs-version "27")
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t))

(defun dd/open-init ()
  "Open up Emacs init file."
  (interactive)
  (find-file
   (concat
    (file-name-as-directory
     user-emacs-directory) "init.el")))

(defun dd/move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun dd/move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun dd/copy-lines-matching-re (re)
  "Put lines matching re in a buffer named *matching*."
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

(defun dd/toggle-window-split ()
  "If two windows are present; toggle the split axis."
  (interactive)
  (unless helm-alive-p
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
      (user-error "Can toggle split only with two windows"))))
(bind-key (kbd "C-x \\") #'dd/toggle-window-split)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :demand t
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package org
  :ensure t
  :init
  (setq org-src-fontify-natively t)
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

(use-package ox :after org)
(use-package ox-beamer :after ox)
(use-package ox-md :after ox)
(use-package ox-hugo
  :ensure t :after ox)
(use-package ox-reveal
  :ensure t :after ox)
(use-package htmlize
  :ensure t)

(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

(setq custom-safe-themes t)
(global-display-line-numbers-mode)
(setq column-number-mode t)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t)
  (set-face-attribute 'link nil :foreground "#458588"))

(when window-system
  (when dd-on-cc7
    (set-face-attribute 'default nil
                        :family "JetBrains Mono"
                        :weight 'medium
                        :height 130))
  (when dd-on-mac
    (setq mac-allow-anti-aliasing t)
    (set-face-attribute 'default nil
                        :family "JetBrains Mono"
                        :weight 'medium
                        :height 120))
  (add-to-list 'default-frame-alist '(height . 72))
  (add-to-list 'default-frame-alist '(width . 234)))

(defun tv/extend-faces-matching (regexp)
  "From https://github.com/emacs-helm/helm/issues/2213

Fix issue with the new :extend face attribute in emacs-27 Prefer
to extend to EOL as in previous emacs."
  (cl-loop for f in (face-list)
           for face = (symbol-name f)
           when (and (string-match regexp face)
                     (eq (face-attribute f :extend t 'default)
                         'unspecified))
           do (set-face-attribute f nil :extend t)))

(defun dd/init-extend-faces ()
  (when (fboundp 'set-face-extend)
    (with-eval-after-load "org"
      (tv/extend-faces-matching "\\`org"))
    (with-eval-after-load "magit"
      (tv/extend-faces-matching "\\`magit"))
    (with-eval-after-load "helm"
      (tv/extend-faces-matching "\\`helm"))))

(unless (version< emacs-version "27")
  (dd/init-extend-faces))

(use-package hydra :ensure t)
(use-package pretty-hydra :ensure t)

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

     "Search" (("r" dd/helm-project-search  "ripgrep (helm)")
               ("s" dd/ripgrep-proj-or-dir  "ripgrep (rg.el)")
               ("o" projectile-multi-occur  "multioccur"))

     "Misc" (("a" projectile-add-known-project  "add to known")
             ("i" projectile-ibuffer            "ibuffer")
             ("k" projectile-kill-buffers       "Kill em"))))
  :bind ("C-c p" . hydra-projectile/body)
  :bind-keymap ("C-c P" . projectile-command-map)
  :config
  (setq projectile-track-known-projects-automatically nil
        projectile-completion-system 'helm
        projectile-globally-ignored-file-suffixes '("#" "~" ".o" ".so" ".elc" ".pyc")
        projectile-globally-ignored-directories '(".git" "__pycache__")
        projectile-globally-ignored-files '(".DS_Store")
        projectile-enable-caching t)
  (projectile-mode +1))

(defun dd/projectile-proj-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package project
  :config
  (add-to-list 'project-find-functions #'dd/projectile-proj-find-function))

(use-package helm
  :ensure t
  :demand t
  :bind-keymap ("C-x c" . helm-command-map)
  :bind (("C-x C-f" . helm-find-files)
         ("C-x C-t" . find-file)
         ("C-x r b" . helm-bookmarks)
         ("C-x m" . helm-M-x)
         ("C-x b" . helm-buffers-list)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action))
  :config
  (setq helm-autoresize-max-height 40
        helm-autoresize-min-height 20
        helm-split-window-in-side-p t
        helm-split-window-default-side 'below
        helm-idle-delay 0.0
        helm-input-idle-delay 0.01
        helm-quick-update t
        helm-grep-file-path-style 'relative
        helm-ff-skip-boring-files t
        helm-grep-ag-command (concat dd-rg-exe
                                     " --color=always"
                                     " --smart-case"
                                     " --no-heading"
                                     " --line-number %s %s %s"))
  (helm-autoresize-mode 1)
  (helm-mode 1))

(defun dd/helm-rg (directory &optional with-types)
  "Search in DIRECTORY with ripgrep.
  With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (helm-grep-ag-1 (expand-file-name directory)
                  (helm-aif (and with-types
                                 (helm-grep-ag-get-types))
                      (helm-comp-read
                       "RG type: " it
                       :must-match t
                       :marked-candidates t
                       :fc-transformer 'helm-adaptive-sort
                       :buffer "*helm rg types*"))))

(defun dd/helm-project-search (&optional with-types)
  "Search in current project with rippgrep.
  With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (dd/helm-rg (projectile-project-root) with-types))

(use-package rg
  :ensure t
  :after wgrep
  :init
  (setq rg-executable (expand-file-name dd-rg-exe)
        rg-group-result t
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

(use-package company
  :ensure t
  :hook ((emacs-lisp-mode . company-mode)
         (python-mode . company-mode)
         (mu4e-compose-mode . company-mode)
         (sh-mode . company-mode)
         (yaml-mode . company-mode)
         (conf-mode . company-mode)
         (lsp-mode . company-mode)
         (eglot-managed-mode . company-mode)
         (LaTeX-mode . company-mode))
  :config
  (setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1))

(use-package vc
  :init
  (setq vc-follow-symlinks t))

(defun dd/magit-kill-buffers ()
  "See `https://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/'"
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(use-package magit
  :ensure t
  :demand
  :bind (("C-x g" . #'magit-status)
         :map magit-status-mode-map
         ("q" . #'dd/magit-kill-buffers)))

(setq read-process-output-max (* 2 1024 1024))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-clients-clangd-executable dd-clangd-exe)
  (setq lsp-prefer-capf t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-auto-guess-root nil)
  ;; (setq lsp-pyls-configuration-sources ["flake8"])
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

     "Sesion" (("M-s" lsp-describe-session   "describe session")
               ("M-r" lsp-restart-workspace  "restart workspace")
               ("S" lsp-shutdown-workspace   "shutdown workspace"))))
  :bind (:map lsp-mode-map
              ("C-c l" . #'hydra-lsp/body)))

(use-package lsp-python-ms
  :ensure t)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-sideline-show-hover nil))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package eglot
  :ensure t
  :commands eglot
  :init
  (setq eglot-server-programs
        `((python-mode "pyls")
          ((c++-mode c-mode) ,dd-clangd-exe))))

(use-package pydoc :ensure t)
(use-package helm-pydoc :ensure t)
(use-package elpy :ensure t)
(use-package blacken :ensure t)

(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package python
  :mode ("\\.py'" . python-mode)
  :interpreter ("python" . python-mode))

(defun dd/py-workon-project-venv ()
  "Call pyenv-workon with the current projectile project name.

This will return the full path of the associated virtual
environment found in $WORKON_HOME, or nil if the environment does
not exist."
  (let ((pname (projectile-project-name)))
    (pyvenv-workon pname)
    (if (file-directory-p pyvenv-virtual-env)
        pyvenv-virtual-env
      (pyvenv-deactivate))))

(defun dd/py-auto-lsp ()
  "Turn on lsp mode in a Python project with some automated logic.

Try to automatically determine which pyenv virtual environment to
activate based on the project name, using
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

(bind-key (kbd "C-c C-a") 'dd/py-auto-lsp python-mode-map)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.icc\\'" . c++-mode))

(use-package clang-format
  :ensure t
  :init
  (setq clang-format-executable dd-clang-format-exe))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(when dd-on-spar
  (defun dd/cpp-fix-backspace ()
    (global-set-key (kbd "C-d") 'delete-backward-char)
    (local-unset-key (kbd "C-d")))
  (add-hook 'c++-mode-hook #'dd/cpp-fix-backspace))

(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  (setq font-latex-fontify-sectioning 'color
        font-latex-fontify-script nil
        TeX-source-correlate-mode 'synctex
        TeX-source-correlate-start-server t)
  (setq-default TeX-master nil)
  :config
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package reftex
  :after auctex
  :hook (LaTeX-mode . reftex-mode))

(use-package helm-bibtex :ensure t)
(use-package company-bibtex :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-<" . mc/mark-all-symbols-like-this)))

(when dd-on-cc7
  (setenv "PKG_CONFIG_PATH" "/usr/lib64/pkgconfig")
  (use-package pdf-tools
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    (setq TeX-view-program-selection '((output-pdf "PDF Tools")))))

(defvar dd-thesis-bib nil)
(defvar dd-thesis-file nil)

(defun dd/work-on-thesis ()
  (interactive)
  (when (file-exists-p "~/Desktop/thesis/biblio/refs.bib")
    (setq dd-thesis-file "~/Desktop/thesis/dissertation.tex"
          dd-thesis-bib "~/Desktop/thesis/biblio/refs.bib"))
  (when (file-exists-p "~/Documents/thesis/biblio/refs.bib")
    (setq dd-thesis-file "~/Documents/thesis/dissertation.tex"
          dd-thesis-bib "~/Documents/thesis/biblio/refs.bib"))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography `(,dd-thesis-bib)
        bibtex-completion-bibliography `(,dd-thesis-bib)
        reftex-plug-into-AUCTeX t)
  (find-file dd-thesis-file))

(use-package flyspell
  :hook ((org-mode . flyspell-mode)
         (LaTeX-mode . flyspell-mode)
         (markdown-mode . flyspell-mode)
         (message-mode . flyspell-mode)
         (mu4e-compose-mode . flyspell-mode)))

(use-package auth-source
  :init
  (setq auth-sources '("~/.emacs.d/.authinfo.gpg")))

(use-package epa-file
  :config
  (epa-file-enable)
  (if dd-on-mac
      (custom-set-variables '(epg-gpg-program "/usr/local/bin/gpg"))
    (custom-set-variables '(epg-gpg-program "/usr/bin/gpg2"))))

(bind-key (kbd "C-c q") #'auto-fill-mode)

(setq echo-keystrokes 0.01
      inhibit-startup-screen t
      ring-bell-function 'ignore
      visible-bell nil)

(setq-default indent-tabs-mode nil)

(setq auto-save-list-file-prefix nil
      create-lockfiles nil
      auto-save-list-file-prefix nil
      backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(show-paren-mode 1)
(setq-default show-paren-delay 0)

(defun dd/del-trail-white ()
  "add `delete-trailing-whitespace' to `write-file-functions'

Since `write-file-functions' is a permanent local list, this is a
convenience function to add the `delete-trailing-whitespace'
function to that list. Should be added to a mode hook."
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'text-mode-hook 'dd/del-trail-white)
(add-hook 'prog-mode-hook 'dd/del-trail-white)

(setq require-final-newline t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t)
(use-package cmake-mode :ensure t)

(use-package deadgrep
  :ensure t
  :init
  (setq deadgrep-executable dd-rg-exe))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package iedit
  :ensure t
  :bind ("C-c ;" . iedit-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package rst
  :hook (rst-mode . (lambda () (interactive) (local-unset-key (kbd "C-c 4")))))

(use-package browse-url
  :init
  (when dd-on-cc7
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "/usr/local/bin/firefox")))

(use-package dired
  :init
  (defhydra hydra-dired (:hint nil :color pink)
    "
    _+_ mkdir          _v_ view         _m_ mark           _(_ details        _i_ insert-subdir  wdired
    _C_ copy           _O_ view other   _U_ unmark all     _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
    _D_ delete         _o_ open other   _u_ unmark         _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
    _R_ rename         _M_ chmod        _t_ toggle         _g_ revert buf     _e_ ediff          C-c ESC : abort
    _Y_ rel symlink    _G_ chgrp        _E_ extension mark _s_ sort           _=_ pdiff
    _S_ symlink        ^ ^              _F_ find marked    _._ toggle hydra   \\ flyspell
    _r_ rsync          ^ ^              ^ ^                ^ ^                _?_ summary
    _z_ compress-file  _A_ find regexp
    _Z_ compress       _Q_ repl regexp

    T - tag prefix
    "
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("C-g" nil :color blue)
    ("." nil :color blue))
  :bind (:map dired-mode-map
              ("." . hydra-dired/body)
              ("q" . kill-current-buffer))
  :hook (dired-mode . hl-line-mode))

(use-package all-the-icons :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(defun dd/delete-frame-or-window ()
  "If we have multiple frames delete the current one.

If only one delete the window; this is really just for binding
Command+w to behave similar to other macOS applications."
  (interactive)
  (if (< (count-windows) 2)
      (delete-frame)
    (delete-window)))

(use-package help-mode
  :bind
  (:map help-mode-map
        ("q" . kill-buffer-and-window)))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h o" . helpful-symbol)
         ("C-h ." . helpful-at-point)
         ("C-h k" . helpful-key)
         :map helpful-mode-map
         ("q" . kill-buffer-and-window)))

(defun dd/scratch-buffer ()
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (when (eq (buffer-size (get-buffer-create "*scratch*")) 0)
    (insert initial-scratch-message)))

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

(defun dd/load-dot-emacs-git-file (fname)
  (load-file
   (concat (file-name-directory (file-truename (or load-file-name buffer-file-name)))
           fname)))

(when dd-on-mac
  (dd/load-dot-emacs-git-file "macos.el"))
(when (or dd-on-mac dd-on-cc7)
  (dd/load-dot-emacs-git-file "rss.el"))
(when dd-enable-irc
  (dd/load-dot-emacs-git-file "irc.el"))
(when (or dd-on-cc7 dd-on-mac)
  (dd/load-dot-emacs-git-file "email.el"))

;; end of init file so set a more reasonable threshold
(setq gc-cons-threshold (* 100 1024 1024))

(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1))


;;; end of init.el

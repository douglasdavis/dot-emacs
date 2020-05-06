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
(setq comp-deferred-compilation t
      comp-async-jobs-number 5)

;; 2GB threshold while init is loaded
(setq gc-cons-threshold (* 2000 1024 1024))

(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

(fset 'yes-or-no-p 'y-or-n-p)

(setq custom-file (concat user-emacs-directory "custom.el"))

(setq user-mail-address "ddavis@ddavis.io"
      user-login-name "ddavis"
      user-full-name "Doug Davis")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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

(use-package s
  :ensure t
  :demand t)

(defconst dd-on-mac (eq system-type 'darwin)
  "true if on a mac")

(defconst dd-on-cc7 (s-contains? "cc7" (system-name))
  "true if on cc7 box")

(defconst dd-on-grads-18 (s-contains? "grads-18" (system-name))
  "true if on grads-18 box")

(defconst dd-on-generic-linux (or dd-on-cc7 dd-on-grads-18)
  "true if on any linux machine")

(defconst dd-enable-mu4e (or dd-on-cc7 dd-on-mac)
  "true if on a machine where we want to be able to use mu4e")

(defconst dd-enable-irc (or dd-on-cc7 dd-on-mac dd-on-grads-18)
  "true if on machine where we want to be able to use IRC")

(defconst dd-mu-exe
  (cond (dd-on-mac "/Users/ddavis/software/localbase/bin/mu")
        (dd-on-cc7 "/usr/local/bin/mu")
        (dd-on-grads-18 "/home/drd25/software/localbase/bin/mu"))
  "machine dependent mu executable string")

(defconst dd-mu4e-dir
  (cond (dd-on-mac "/Users/ddavis/software/localbase/share/emacs/site-lisp/mu4e")
        (dd-on-cc7 "/usr/local/share/emacs/site-lisp/mu4e")
        (dd-on-grads-18 "/home/drd25/software/localbase/share/emacs/site-lisp/mu4e"))
  "machine dependent mu4e installation location string")

(defconst dd-sendmail-exe
  (cond (dd-on-mac "/Users/ddavis/software/localbase/bin/msmtp")
        (dd-on-cc7 "/usr/local/bin/msmtp")
        (dd-on-grads-18 "/usr/bin/msmtp"))
  "machine dependent msmtp executable string")

(defvar dd-llvm-bin-path
  (cond (dd-on-mac "/usr/local/opt/llvm/bin")
        (dd-on-cc7 "/home/ddavis/software/specific/llvm/10.x/bin")
        (dd-on-grads-18 "/home/drd25/software/specific/llvm/10.x/bin"))
  "machine dependent llvm bin path")

(defvar dd-clangd-exe
  (if (file-exists-p dd-llvm-bin-path)
      (concat (file-name-as-directory dd-llvm-bin-path) "clangd")
    nil))

(defvar dd-clang-format-exe
  (if (file-exists-p dd-llvm-bin-path)
      (concat (file-name-as-directory dd-llvm-bin-path) "clang-format")
    nil))

(defvar dd-clang-exe
  (if (file-exists-p dd-llvm-bin-path)
      (concat (file-name-as-directory dd-llvm-bin-path) "clang")
    nil))

(defconst dd-rg-exe
  (cond (dd-on-mac "/usr/local/bin/rg")
        (dd-on-cc7 "/home/ddavis/.cargo/bin/rg")
        (dd-on-grads-18 "/home/drd25/.cargo/bin/rg"))
  "machine dependent ripgrep executable string")

(defconst dd-fd-exe
  (cond (dd-on-mac "/usr/local/bin/fd")
        (dd-on-cc7 "/home/ddavis/.cargo/bin/fd")
        (dd-on-grads-18 "/home/drd25/.cargo/bin/fd"))
  "machine dependent fd executable string")

(setq default-directory (cond (dd-on-mac "/Users/ddavis/")
                              (dd-on-cc7 "/home/ddavis/")
                              (dd-on-grads-18 "/home/drd25/")))

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
                  ("cpp" . "src C++")))))

(when dd-on-mac
  (bind-key "<A-down>" #'org-move-subtree-down org-mode-map)
  (bind-key "<A-up>" #'org-move-subtree-up org-mode-map)
  (bind-key "<A-left>" #'org-promote-subtree)
  (bind-key "<A-right>" #'org-demote-subtree))

(unless dd-on-mac
  (bind-key "<s-down>" #'org-move-subtree-down org-mode-map)
  (bind-key "<s-up>" #'org-move-subtree-up org-mode-map)
  (bind-key "<s-left>" #'org-promote-subtree)
  (bind-key "<s-right>" #'org-demote-subtree))

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
  :bind ("C-c p" . #'hydra-projectile/body)
  :bind-keymap ("C-c P" . projectile-command-map)
  :config
  (setq projectile-track-known-projects-automatically nil
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
  :init (setq helm-autoresize-max-height 50
              helm-autoresize-min-height 30)
  :bind (("C-x C-f" . #'helm-find-files)
         ("C-x C-t" . #'find-file)
         ("C-x r b" . #'helm-bookmarks)
         ("C-x m" . #'helm-M-x)
         ("C-x b" . #'helm-buffers-list)
         :map helm-map
         ("<tab>" . #'helm-execute-persistent-action)
         :map helm-command-map
         ("r" . #'dd/helm-project-search))
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p t
        helm-split-window-default-side 'below
        helm-idle-delay 0.0
        helm-input-idle-delay 0.01
        helm-quick-update t
        helm-ff-skip-boring-files t)
  (setq helm-grep-ag-command (concat dd-rg-exe
                                     " --color=always"
                                     " --smart-case"
                                     " --no-heading"
                                     " --line-number %s %s %s")
        helm-grep-file-path-style 'relative)
  (helm-autoresize-mode 1)
  (helm-mode 1))

(defun dd/helm-rg (directory &optional with-types)
  "Search in DIRECTORY with ripgrep.
  With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (require 'helm-adaptive)
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
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1))

(defun dd/company-capf-to-front ()
  (if (member 'company-capf company-backends)
      (setq company-backends
            (cons 'company-capf (remove 'company-capf company-backends)))
    (add-to-list 'company-backends 'company-capf)))

(use-package vc
  :demand t
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
  :hook (lsp-mode . #'dd/company-capf-to-front)
  :init
  (setq lsp-clients-clangd-executable dd-clangd-exe)
  (setq lsp-prefer-capf t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-auto-guess-root nil)
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

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-sideline-show-hover nil))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package eglot
  :ensure t
  :commands eglot
  :hook (eglot-managed-mode . dd/company-capf-to-front)
  :init
  (setq eglot-server-programs
        `((python-mode "pyls")
          ((c++-mode c-mode) ,dd-clangd-exe))))

(use-package python
  :defer t
  :hook python-mode-hook)

(use-package pydoc :ensure t)
(use-package helm-pydoc :ensure t)
(use-package elpy :ensure t)
(use-package blacken :ensure t)

(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(defun dd/get-pyvenv-name ()
  "grab the name of the active pyvenv (nil if not defined)"
  (when pyvenv-virtual-env
    (car (last (split-string (directory-file-name pyvenv-virtual-env) "/")))))

(defun dd/py-auto-lsp ()
  "turn on lsp mode in a Python project by trying to
  automatically determine which pyenv virtual environment to
  activate based on the project name"
  (interactive)
  (if (and pyvenv-virtual-env
           (file-directory-p pyvenv-virtual-env)
           (string= projectile-project-name (dd/get-pyvenv-name)))
      (lsp)
    (pyvenv-workon (projectile-project-name))
    (if (file-directory-p pyvenv-virtual-env)
        (lsp)
      (progn
        (message (format "%s does not exist, set manually"
                         pyvenv-virtual-env))
        (call-interactively #'pyvenv-workon)
        (lsp)))))

(defun dd/eglot-prep-for-python ()
  "prepare python eglot setup"
  (interactive)
  (setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls"))))


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.icc\\'" . c++-mode))

(defun dd/eglot-prep-for-cpp ()
  "enable variables and hooks for eglot cpp IDE"
  (interactive)
  (require 'eglot)
  (setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  (add-to-list 'eglot-server-programs `((c++-mode cc-mode) ,dd-clangd-exe)))

(use-package clang-format
  :ensure t
  :init
  (setq clang-format-executable dd-clang-format-exe))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

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

(when dd-on-mac
  (bind-key (kbd "s-t") #'dd/work-on-thesis))

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

(use-package password-store
  :when dd-enable-irc
  :commands (password-store-copy
             password-store-get
             password-store-edit
             password-store-insert)
  :ensure t)

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

(mapc
 (lambda (language-mode-hook)
   (add-hook language-mode-hook
             (lambda ()
               (add-to-list 'write-file-functions 'delete-trailing-whitespace))))
 '(text-mode-hook
   c-mode-common-hook
   emacs-lisp-mode-hook
   python-mode-hook
   markdown-mode-hook
   bash-mode-hook
   sh-mode-hook
   cmake-mode-hook
   fundamental-mode-hook
   LaTeX-mode-hook))

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
  :bind ("C-c ;" . 'iedit-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package ace-window
  :ensure t
  :bind ("M-o" . 'ace-window))

(use-package rst
  :hook (rst-mode . (lambda () (interactive) (local-unset-key (kbd "C-c 4")))))

(use-package elfeed
  :commands elfeed
  :ensure t
  :bind ("C-x w" . 'elfeed)
  :init
  (setq shr-use-fonts nil)
  (setq elfeed-feeds
        '(("https://planet.scipy.org/feed.xml" python)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://ddavis.io/index.xml" blog)
          ("http://pragmaticemacs.com/feed/" emacs)
          ("http://feeds.podtrac.com/zKq6WZZLTlbM" nyt podcast)))
  :config
  ;; Entries older than 3 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "3 weeks ago"
                                :remove 'unread))
  (setq-default elfeed-search-filter "@21-days-ago")


  (defvar dd-podcast-speed "1.33"
    "mpv --speed argument for podcasts")

  (defun dd/elfeed-play-enclosure-with-mpv ()
    "Play enclosure link with mpv."
    (interactive)
    (let ((speed dd-podcast-speed)
          (podcast-link (nth 0 (car (elfeed-entry-enclosures elfeed-show-entry)))))
      (message "Opening %s with with mpv..." podcast-link)
      (start-process "elfeed-mpv" nil "mpv"
                     "--speed" speed
                     podcast-link))))

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
              ("." . #'hydra-dired/body)
              ("q" . #'kill-current-buffer))
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

(defun dd/switch-to-or-start-gnus ()
  "If we have a '*Group*' buffer go and and switch to it.

If not this will just spin up gnus. this is just for binding to
s-u on macOS."
  (interactive)
  (if (get-buffer "*Group*")
      (switch-to-buffer "*Group*")
    (gnus)))

(when (memq window-system '(mac ns))
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (setq-default ns-alternate-modifier 'meta)
  (setq-default mac-option-modifier 'meta)
  (setq-default ns-right-alternate-modifier nil)
  (setq-default ns-command-modifier 'super)
  (setq-default mac-command-modifier 'super)
  (setq-default ns-function-modifier 'hyper)
  (setq-default mac-function-modifier 'hyper)
  (global-set-key [(meta shift up)]  #'dd/move-line-up)
  (global-set-key [(meta shift down)]  #'dd/move-line-down))

(when dd-on-mac
  (bind-key (kbd "s-/") #'previous-buffer)
  (bind-key (kbd "s-\\") #'dd/toggle-window-split)
  (bind-key (kbd "s-1") #'delete-other-windows)
  (bind-key (kbd "s-2") #'split-window-below)
  (bind-key (kbd "s-3") #'split-window-right)
  (bind-key (kbd "s-5") #'projectile-find-file-in-known-projects)
  (bind-key (kbd "s-4") #'mu4e)
  (bind-key (kbd "s-d") #'dinit)
  (bind-key (kbd "s-b") #'helm-buffers-list)
  (bind-key (kbd "s-f") #'helm-find-files)
  (bind-key (kbd "s-g") #'magit-status)
  (bind-key (kbd "s-o") #'other-window)
  (bind-key (kbd "s-p") #'helm-projectile)
  (bind-key (kbd "s-r") #'dd/helm-project-search)
  (bind-key (kbd "s-u") #'dd/switch-to-or-start-gnus)
  (bind-key (kbd "s-w") #'dd/delete-frame-or-window))

(use-package help-mode
  :bind
  (:map help-mode-map
        ("q" . kill-buffer-and-window)))

(use-package helpful
  :ensure t
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h o" . #'helpful-symbol)
         ("C-h ." . #'helpful-at-point)
         ("C-h k" . #'helpful-key)
         :map helpful-mode-map
         ("q" . #'kill-buffer-and-window)))

(use-package tramp
  :defer 5
  :config
  (setq tramp-default-method "ssh"))

(defun dd/cleanup-tramp ()
  (interactive)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections)
  (find-file "~/."))

(defun dd/irc-pw-freenode (server)
  (password-store-get "Freenode"))

(defun dd/irc-pw-gitter (server)
  (password-store-get "Gitter"))

(defun circe-network-connected-p (network)
  "Return non-nil if there's any Circe server-buffer whose
    `circe-server-netwok' is NETWORK."
  (catch 'return
    (dolist (buffer (circe-server-buffers))
      (with-current-buffer buffer
        (if (string= network circe-server-network)
            (throw 'return t))))))

(defun circe-maybe-connect (network)
  "Connect to NETWORK, but ask user for confirmation if it's
    already been connected to."
  (interactive "sNetwork: ")
  (if (or (not (circe-network-connected-p network))
          (y-or-n-p (format "Already connected to %s, reconnect?" network)))
      (circe network)))

(defun dd/circe-prompt ()
  (lui-set-prompt
   (propertize (format "%s >>> " (buffer-name)) 'face 'circe-prompt-face)))

(use-package circe
  :when dd-enable-irc
  :ensure t
  :hook (circe-chat-mode . dd/circe-prompt)
  :config
  (setq circe-network-options
        `(("Freenode"
           :nick "ddavis"
           :nickserv-password dd/irc-pw-freenode
           :nickserv-identify-confirmation "Freenode password accepted for ddavis"
           :tls t)
          ("Gitter"
           :server-buffer-name "Gitter"
           :host "irc.gitter.im"
           :port "6697"
           :nick "douglasdavis"
           :pass dd/irc-pw-gitter
           :tls t)))
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
        (concat "Quit Circe (" circe-version ") in GNU Emacs (" emacs-version ")")))

(use-package helm-circe
  :when dd-enable-irc
  :after circe
  :ensure t
  :bind (:map helm-command-map ("i" . helm-circe)))

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

(defun dd/erc-notify (nickname message)
  "Displays a notification message for ERC."
  (let* ((channel (buffer-name))
         (nick (erc-hl-nicks-trim-irc-nick nickname))
         (title (if (string-match-p (concat "^" nickname) channel)
                    nick
                  (concat nick " (" channel ")")))
         (msg (s-trim (s-collapse-whitespace message))))
    (alert (concat nick ": " msg) :title title)))

(use-package erc
  :when dd-enable-irc
  :hook ((erc-notify . dd/erc-notify)
         (erc-insert-modify . dd/erc-insert-modify-hook))
  :custom-face (erc-notice-face ((t (:foreground "#ebcb8b"))))
  :config
  (setq erc-user-full-name "Doug Davis"
        erc-prompt-for-password nil
        erc-track-enable-keybindings nil
        erc-kill-server-buffer-on-quit t
        erc-kill-buffer-on-part t
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 19
        erc-prompt (lambda () (concat (buffer-name) " >>>"))))

(use-package erc-track
  :when dd-enable-irc
  :after erc
  :config
  (erc-track-mode t)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")))

(setq message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-kill-buffer-on-exit t)

(setq sendmail-program dd-sendmail-exe)

(defun dd/reset-standard-name-and-email ()
  (interactive)
  (setq user-mail-address "ddavis@ddavis.io"
        user-email-address "ddavis@ddavis.io"
        user-full-name "Doug Davis"))

(defun dd/mu4e-jump-via-helm ()
  (interactive)
  (let ((maildir (helm-comp-read "Maildir: " (mu4e-get-maildirs))))
    (mu4e-headers-search (format "maildir:\"%s\"" maildir))))

(defun mu4e-action-view-in-w3m ()
  "View the body of the message in emacs w3m."
  (interactive)
  (w3m-browse-url (concat "file://"
                          (mu4e~write-body-to-html (mu4e-message-at-point t)))))

(defun dd/mu4e-toggle-gnus ()
  (interactive)
  (setq mu4e-view-use-gnus (not mu4e-view-use-gnus)))

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(use-package mu4e
  :when dd-enable-mu4e
  :load-path dd-mu4e-dir
  :commands (mu4e mu4e-update-mail-and-index)
  :bind (("C-c 4" . mu4e)
         :map mu4e-headers-mode-map
         ("j" . dd/mu4e-jump-via-helm)
         ("d" . mu4e-headers-mark-for-delete)
         ("D" . mu4e-headers-mark-for-trash)
         :map mu4e-main-mode-map
         ("j" . dd/mu4e-jump-via-helm)
         :map mu4e-view-mode-map
         ("d" . mu4e-view-mark-for-delete)
         ("D" . mu4e-view-mark-for-trash)
         ("M" . mu4e-action-view-in-w3m)
         ("j" . dd/mu4e-jump-via-helm))
  :config
  (setq mu4e-mu-binary dd-mu-exe
        mu4e-change-filenames-when-moving t
        mu4e-get-mail-command "true"
        mu4e-update-interval 120
        mu4e-maildir "~/.mail"
        mu4e-confirm-quit nil
        mu4e-context-policy 'pick-first
        mu4e-user-mail-address-list '()
        mu4e-attachment-dir (expand-file-name "~/Downloads/")
        mu4e-change-filenames-when-moving t)

  (setq mu4e-compose-reply-ignore-address
        '("notifications@github\\.com"
          "ddavis@ddavis\\.io"
          "ddavis@phy\\.duke\\.edu"
          "douglas\\.davis\\.092@gmail\\.com"
          "douglas\\.davis@duke\\.edu"
          "douglas\\.davis@cern\\.ch"
          "ddavis@cern\\.ch"))

  (setq w3m-default-desplay-inline-images t)

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "cern"
             :enter-func (lambda () (mu4e-message "Entering CERN context"))
             :leave-func (lambda () (dd/reset-standard-name-and-email))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/cern" (mu4e-message-field msg :maildir))))
             :vars '( ( user-mail-address      . "ddavis@cern.ch" )
                      ( user-email-address     . "ddavis@cern.ch" )
                      ( user-full-name         . "Doug Davis" )
                      ( mu4e-trash-folder      . "/cern/Trash" )
                      ( mu4e-sent-folder       . "/cern/Sent" )
                      ( mu4e-drafts-folder     . "/cern/Drafts" )
                      ( mu4e-reply-to-address  . "ddavis@cern.ch" )))

           ,(make-mu4e-context
             :name "duke"
             :enter-func (lambda () (mu4e-message "Entering Duke context"))
             :leave-func (lambda () (dd/reset-standard-name-and-email))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/duke" (mu4e-message-field msg :maildir))))
             :vars '( ( user-mail-address       . "ddavis@phy.duke.edu" )
                      ( user-email-address      . "ddavis@phy.duke.edu" )
                      ( user-full-name          . "Doug Davis" )
                      ( mu4e-trash-folder       . "/duke/Trash" )
                      ( mu4e-sent-folder        . "/duke/Sent" )
                      ( mu4e-drafts-folder      . "/duke/Drafts" )
                      ( mu4e-reply-to-address   . "ddavis@phy.duke.edu" )))))

  (when (or dd-on-mac dd-on-cc7)
    (add-to-list 'mu4e-contexts
                 (make-mu4e-context
                  :name "gmail"
                  :enter-func (lambda () (mu4e-message "Entering Gmail context"))
                  :leave-func (lambda () (dd/reset-standard-name-and-email))
                  :match-func (lambda (msg)
                                (when msg
                                  (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
                  :vars '( ( user-mail-address           . "douglas.davis.092@gmail.com" )
                           ( user-email-address          . "douglas.davis.092@gmail.com" )
                           ( user-full-name              . "Doug Davis" )
                           ( mu4e-trash-folder           . "/gmail/_blackhole" )
                           ( mu4e-sent-folder            . "/gmail/[Gmail]/Sent Mail" )
                           ( mu4e-drafts-folder          . "/gmail/_blackhole" )
                           ( mu4e-reply-to-address       . "douglas.davis.092@gmail.com" ))))
    (add-to-list 'mu4e-contexts
                 (make-mu4e-context
                  :name "fastmail"
                  :enter-func (lambda () (mu4e-message "Entering FastMail context"))
                  :leave-func (lambda () (mu4e-message "Leaving FastMail context"))
                  :match-func (lambda (msg)
                                (when msg
                                  (string-match-p "^/fastmail" (mu4e-message-field msg :maildir))))
                  :vars '( ( user-mail-address      . "ddavis@ddavis.io" )
                           ( user-email-address     . "ddavis@ddavis.io" )
                           ( user-full-name         . "Doug Davis" )
                           ( mu4e-trash-folder      . "/fastmail/Trash" )
                           ( mu4e-sent-folder       . "/fastmail/Sent" )
                           ( mu4e-drafts-folder     . "/fastmail/Drafts" )
                           ( mu4e-reply-to-address  . "ddavis@ddavis.io" )))))

  (setq mu4e-bookmarks ())
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Unread short"
                :query "flag:unread AND (m:/duke* or m:/cern* or m:/fastmail/INBOX or m:/gmail/INBOX*)"
                :key ?u))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Recent personal"
                :query "date:14d..now AND (m:/fastmail/INBOX or m:/gmail/INBOX*)"
                :key ?p))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Unread all"
                :query "flag:unread AND NOT flag:trashed"
                :key ?U))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "INBOXes"
                :query "m:/duke/INBOX or m:/cern/INBOX or m:/fastmail/INBOX or m:/gmail/INBOX"
                :key ?i))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Last day's work"
                :query "date:1d..now AND NOT m:/fastmail* AND NOT m:/gmail* AND NOT m:/cern/Mailing\\ Lists/JEDI*"
                :key ?w))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Recent work"
                :query "date:3d..now AND NOT m:/fastmail* AND NOT m:/gmail* AND NOT m:/cern/Mailing\\ Lists/JEDI*"
                :key ?r))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Duke recent"
                :query "date:5d..now AND m:/duke*"
                :key ?d))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "CERN recent"
                :query "date:2d..now AND m:/cern*"
                :key ?c))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Last 1 day"
                :query "date:1d..now"
                :key ?1))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Last 3 days"
                :query "date:3d..now"
                :key ?3))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Last 7 days"
                :query "date:1w..now"
                :key ?7)))

;; end of init file so set a more reasonable threshold
(setq gc-cons-threshold (* 100 1024 1024))

(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1))


;;; end of init.el

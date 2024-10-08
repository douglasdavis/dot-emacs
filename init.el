;;; init.el --- Emacs init.el                        -*- lexical-binding: t; -*-

;; Copyright (C) 2020 - 2022 Doug Davis

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

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/sbin")

;; sec01:
;; general setup not associated with packages

;; for native comp branch
(defconst dd/using-native-comp (and (fboundp 'native-comp-available-p)
                                    (native-comp-available-p)))

(setq native-comp-jit-compilation-deny-list '("loaddefs\\.el\\.gz"))
(setq native-comp-async-query-on-exit t)
(setq native-comp-jit-compilation nil)
(setq native-comp-async-jobs-number 3)

(defun dd/includes? (s substr)
  "Clojure like function; t if S includes SUBSTR."
  (declare (pure t) (side-effect-free t))
  (not (null (string-match-p (regexp-quote substr) s))))

(defconst dd/on-mac-p (eq system-type 'darwin)
  "For checking if on a mac.")

(defconst dd/on-m1-p (or (dd/includes? (emacs-version) "aarch64-apple")
                         (dd/includes? (emacs-version) "arm-apple"))
  "For checking if on M1 mac.")

(defconst dd/on-intel-p (dd/includes? (emacs-version) "x86")
  "For checking if on Intel mac.")

(defconst dd/on-udt-p (dd/includes? (system-name) "udt")
  "For checking if on udt box.")

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
              backup-by-copying t
              backup-directory-alist `(("." . ,(locate-user-emacs-file ".saves")))
              create-lockfiles nil
              delete-old-versions t
              enable-local-variables :all
              kept-new-versions 2
              kept-old-versions 1
              revert-without-query '(".*")
              ring-bell-function 'ignore
              version-control t)

(setq sentence-end-double-space nil)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode +1))

(when (not window-system)
  (menu-bar-mode -1))

(setq-default require-final-newline t)
(setq-default indent-tabs-mode nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))

(defun dd/reset-font ()
  "Reset font to personal default"
  (interactive)
  (when (boundp 'ns-antialias-text)
    (setq ns-antialias-text t)
    (set-face-attribute 'default nil
                        :family "Agave Nerd Font"
                        :weight 'regular
                        :height 160)
    (dolist (face '(font-lock-doc-face font-lock-comment-face))
      (set-face-attribute face nil :italic t)))

  (set-face-attribute 'fixed-pitch nil :family "Agave Nerd Font"))

(dd/reset-font)

(when (fboundp 'set-fontset-font)
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

(defun dd/better-defaults ()
  "Better default cosmetics.
Taken from an emacs-devel thread."
  (let ((bg (face-attribute 'mode-line :background)))
    (set-face-attribute 'mode-line nil
                        :box (list :line-width 4 :color bg :style nil)))

  (set-face-attribute 'mode-line nil
                      :height 110
                      :background "grey88"
                      :box '(:line-width 4 :color "grey88" :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :height 110
                      :background "grey95"
                      :box '(:line-width 4 :color "grey95" :style nil)))

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

(defun dd/frame-recenter (&optional frame)
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame)
                           frame)
                      (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           ;; frame-monitor-workarea returns (x y width height) for the monitor
           (monitor-w (nth 2 (frame-monitor-workarea frame)))
           (monitor-h (nth 3 (frame-monitor-workarea frame)))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'bind-key)
(setq use-package-hook-name-suffix nil)

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff ruff-isort))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff ruff-isort)))

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

(use-package auth-source
  :init
  (setenv "GPG_AGENT_INFO" nil)
  (setq auth-sources
        (list
         (concat user-emacs-directory ".authinfo")
         (concat user-emacs-directory ".authinfo.gpg"))))

(use-package autorevert
  :init
  (global-auto-revert-mode +1))

(use-package breadcrumb
  :ensure t
  :hook (eglot-managed-mode-hook . breadcrumb-local-mode))

(use-package browse-url
  :defer 10
  :config
  (setq browse-url-browser-function 'browse-url-generic))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package cc-mode
  :config
  (c-set-offset 'innamespace 0))

(use-package cmake-mode
  :ensure t)

(use-package cmake-font-lock
  :ensure t)

(use-package compile
  :hook (compilation-mode-hook . (lambda () (display-line-numbers-mode 0))))

(use-package consult
  :ensure t
  :bind (("C-c l" . consult-line)
         ("C-c r" . consult-ripgrep)
         ("C-x b" . consult-buffer))
  :init
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
            --smart-case --no-heading --with-filename --line-number --search-zip")
  :config
  (setq consult-preview-key 'nil)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  (defun find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial))))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-scroll-margin 5)
  :init
  ;; (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  :config
  (global-corfu-mode +1))

(use-package corfu-terminal
  :ensure t
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (unless (display-graphic-p frame)
                (corfu-terminal-mode +1)))))

(use-package crux
  :defer 5
  :ensure t)

(setq ad-redefinition-action 'accept)
(use-package default-text-scale
  :ensure t
  :demand t
  :init
  :bind (("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)
         ("s-0" . default-text-scale-reset)))

(use-package denote
  :ensure t
  :init
  (setq denote-directory (expand-file-name "~/.emacs.d/denotes")
        denote-known-keywords '("dak" "random")
        denote-file-type 'text))

(use-package dired
  :demand t
  :init
  (setq-default dired-listing-switches "-alh")
  :bind (:map dired-mode-map
              ("q" . #'kill-current-buffer))
  :config
  (setq dired-recursive-copies 'always))

(use-package diredfl
  :ensure t
  :hook (dired-mode-hook . diredfl-mode))

(make-variable-buffer-local 'display-line-numbers-width-start)
(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode))

(use-package dockerfile-mode
  :ensure t)

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
  :commands eglot
  :config
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                 . ,(eglot-alternatives '("pylsp"
                                          "ruff-lsp"
                                          "jedi-language-server"
                                          ("pyright-langserver" "--stdio")))))
  (add-hook 'eglot-connect-hook 'breadcrumb-local-mode)
  (setq eglot-autoshutdown t))

(use-package eldoc)

(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
  :init
  (setq eldoc-box-clear-with-C-g t
        eldoc-box-fringe-use-same-bg nil))

(use-package erc
  :commands erc
  :config
  (setq erc-nick "ddavis"
        erc-default-server "irc.libera.chat"
        erc-prompt-for-password nil
        erc-user-full-name "Doug Davis"
        erc-rename-buffers t
        erc-track-enable-keybindings nil
        erc-kill-buffer-on-part t
        erc-kill-server-buffer-on-quit t
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 19
        erc-fill-column 86
        erc-track-shorten-start 5
        erc-query-display 'bury
        erc-auto-query 'bury
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

(use-package eros
  :ensure t
  :hook (emacs-lisp-mode-hook . eros-mode))

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :demand t
;;   :config
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize))

;; (use-package flycheck
;;   :ensure t
;;   :defer t
;;   :custom
;;   (flycheck-emacs-lisp-load-path 'inherit))

(use-package hcl-mode
  :ensure t)

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

(use-package ibuffer-project
  :ensure t
  :demand t
  :config
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote"))
  (add-hook
   'ibuffer-hook
   (lambda ()
     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
     (unless (eq ibuffer-sorting-mode 'project-file-relative)
       (ibuffer-do-sort-by-project-file-relative)))))

(use-package iedit
  :ensure t
  :bind ("C-c ;" . iedit-mode))

(use-package isearch
  :init
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format "   (%s/%s)"))

(use-package ispell
  :config
  (setq ispell-program-name "hunspell"))

(use-package jit-spell
  :ensure t
  :hook ((org-mode-hook . jit-spell-mode)
         (LaTeX-mode-hook . jit-spell-mode)
         (markdown-mode-hook . jit-spell-mode)
         (message-mode-hook . jit-spell-mode)
         (mu4e-compose-mode-hook . jit-spell-mode)))

(setq dd/ligature-path (expand-file-name "~/software/repos/ligature.el"))
(when (and window-system (file-exists-p dd/ligature-path))
  (use-package ligature
    :load-path dd/ligature-path
    :config
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))

    (ligature-set-ligatures
     '(prog-mode org-mode)
     '(".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
       ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
       "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
       "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
       "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
       "|=" "//=" "/="
       "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
       "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
       "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
       "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
       "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
       "{!--" "//" "///" "!!"
       "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
       "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
       "--" "---"))
    ;; ;; Enable all Cascadia Code ligatures in programming modes
    ;; (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
    ;;                                      "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
    ;;                                      "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
    ;;                                      "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
    ;;                                      "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
    ;;                                      "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
    ;;                                      ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
    ;;                                      "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
    ;;                                      "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
    ;;                                      "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
    ;;                                      "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
    ;; Enables ligature checks globally in all buffers. You can also do it
    ;; per mode with `ligature-mode'.
    (global-ligature-mode t)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :hook (magit-status-mode-hook . (lambda ()
                                    (make-local-variable 'truncate-lines)
                                    (setq truncate-lines nil)))
  :config
  (defun dd/magit-kill-buffers ()
    "See `https://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/'"
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers))))

(use-package make-mode
  :mode ("Makefile" . makefile-gmake-mode))

(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode))

(use-package markdown-mode
  :ensure t)

(use-package mouse
  :hook ((org-mode-hook . context-menu-mode)
         (LaTeX-mode-hook . context-menu-mode)
         (markdown-mode-hook . context-menu-mode)
         (message-mode-hook . context-menu-mode)
         (mu4e-compose-mode-hook . context-menu-mode)))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :ensure t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode-hook . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(use-package numpydoc
  :ensure t
  :init
  (setq numpydoc-insertion-style nil)
  :bind (:map python-ts-mode-map
              ("C-c C-n" . numpydoc-generate)
         :map python-mode-map
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

(use-package org
  ;; :hook (org-mode-hook . (lambda () (interactive)
  ;;                          (setq-local display-line-numbers-width-start t)))
  :config
  (setq org-src-fontify-natively t)
  (bind-key "<A-down>" 'org-move-subtree-down org-mode-map)
  (bind-key "<A-up>" 'org-move-subtree-up org-mode-map)
  (bind-key "<A-left>" 'org-promote-subtree org-mode-map)
  (bind-key "<A-right>" 'org-demote-subtree org-mode-map))

(use-package outline
  :mode ("NEWS\\'" . outline-mode))

(use-package paren
  :init
  (show-paren-mode 1)
  (setq-default show-paren-delay 0))

(when window-system
  (use-package posframe
    :ensure t))

(use-package prog-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode))

(use-package project
  :demand t)

(use-package protobuf-mode
  :ensure t)

(use-package python
  :init
  (setq python-indent-guess-indent-offset nil)
  :config
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  (defvar pyvenv-virtual-env)
  (defun dd/run-python ()
    "Intelligently run a Python shell."
    (interactive)
    (if pyvenv-virtual-env
        (run-python)
      (progn
        (call-interactively #'pyvenv-workon)
        (run-python))))

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

;; (use-package python-pytest
;;   :ensure t)

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
  :defer 3
  :init
  (defun dd/recentf-excluder (s)
    "Check if file should be recentf excluded."
    (s-contains-p ".emacs.d/elpa-" s))
  (setq recentf-exclude '(dd/recentf-excluder))
  :config
  (recentf-mode +1)
  (add-to-list 'recentf-exclude 'file-remote-p))

(use-package reformatter
  :ensure t
  :config
  (reformatter-define dd/ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-"))
  (reformatter-define dd/isort
    :program "isort"
    :args `("--stdout" "--atomic" "-")))

(use-package rg
  :ensure t
  :after wgrep
  :init
  (setq rg-group-result t
        rg-hide-command t))

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

;; (use-package solaire-mode
;;   :ensure t
;;   :config
;;   (solaire-global-mode +1))

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

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-strip-common-suffix t
        uniquify-after-kill-buffer-p t))

(use-package vc
  :init
  (setq vc-follow-symlinks t))
  ;; :config
  ;; (require 's)
  ;; (defun dd/shorten-git-branch (string)
  ;;   (concat
  ;;    "g:"
  ;;    (s-replace
  ;;     "feature/" "feat/"
  ;;     (s-chop-prefix
  ;;      "Git:"
  ;;      (s-truncate
  ;;       38 string)))))
  ;; (advice-add 'vc-git-mode-line-string :filter-return 'dd/shorten-git-branch))

(use-package vertico
  :ensure t
  :demand t
  :init
  (setq enable-recursive-minibuffers t)
  ;; :custom-face
  ;; (vertico-current ((t (:inherit region))))
  :config
  (vertico-mode +1)
  (setq vertico-count 15)
  (setq vertico-resize nil)
  (setq vertico-sort-threshold 3000)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; (use-package vertico-posframe
;;   :ensure t
;;   :after vertico
;;   :config
;;   (vertico-posframe-mode +1))

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

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-style '(face tabs empty trailing)))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package yasnippet
  :ensure t
  :demand t)

(use-package yasnippet-snippets
  :ensure t)

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
  ;; (bind-key* "s-5" #'projectile-find-file-in-known-projects)
  (bind-key* "s-c" #'dd/frame-recenter)
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
  ;; (bind-key* "s-p" #'projectile-command-map)
  (bind-key* "s-p" project-prefix-map)
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

(when (or dd/on-m1-p dd/on-mac-p dd/on-udt-p)
  (load-file "~/.emacs.d/dot-emacs/email.el"))

;; sec08:
;; misc

;; the end

;; 128MB garbage collection threshold
(setq gc-cons-threshold (* 128 1024 1024))

(provide 'init)
;;; init.el ends here

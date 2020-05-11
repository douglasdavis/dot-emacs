;;; irc.el --- init irc setup                        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Keywords: tools

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

;; irc setup

;;; Code:


(use-package password-store
  :when dd-enable-irc
  :commands (password-store-copy
             password-store-get
             password-store-edit
             password-store-insert)
  :ensure t)

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
  :bind (:map helm-map ("i" . helm-circe)))

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


;;; end of irc.el

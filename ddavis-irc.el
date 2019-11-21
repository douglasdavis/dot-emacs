;;; ddavis-irc.el --- irc configuration              -*- lexical-binding: t; -*-

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

;; IRC setup -- for circe (primary, see:
;; https://github.com/jorgenschaefer/circe) and ERC (built-in)

;;; Code:

(require 'erc)
(require 'epa-file)
(require 'use-package)
(require 'ddavis-vars)


(if ddavis-v-is-mac
    (custom-set-variables '(epg-gpg-program "/usr/local/bin/gpg"))
  (custom-set-variables '(epg-gpg-program "/usr/bin/gpg2")))

(use-package password-store
  :when ddavis-v-enable-irc
  :ensure t
  :config
  (require 'password-store))

(defun ddavis/irc-pw-freenode (server)
  (password-store-get "Freenode"))

(defun ddavis/irc-pw-gitter (server)
  (password-store-get "Gitter"))

(use-package circe
  :when ddavis-v-enable-irc
  :ensure t
  :config
  (setq circe-reduce-lurker-spam t
        circe-network-options
        `(("Freenode"
           :nick "ddavis"
           :nickserv-password ddavis/irc-pw-freenode
           :tls t
           :channels (:after-auth "#emacs" "#sr.ht" "#python" "#pydata" "#scipy")
           )
          ("Gitter"
           :server-buffer-name "Gitter"
           :host "irc.gitter.im"
           :port "6697"
           :nick "douglasdavis"
           :pass ddavis/irc-pw-gitter
           :tls t
           )
          ))
  (require 'circe-color-nicks)
  (enable-circe-color-nicks)
  (setq circe-use-cycle-completion t
        circe-format-say "<{nick}> {body}")
  ;; (setq lui-fill-column 79
  ;;       lui-fill-type 18)
  (setq helm-mode-no-completion-in-region-in-modes
        '(circe-channel-mode
          circe-query-mode
          circe-server-mode))

  (defun ddavis/circe-prompt ()
    (lui-set-prompt
     (concat (propertize (concat (buffer-name) " > ")
                         'face 'circe-prompt-face)
             " ")))
  (add-hook 'circe-chat-mode-hook 'ddavis/circe-prompt))

(use-package helm-circe
  :when ddavis-v-enable-irc
  :ensure t
  :bind (:map helm-command-map ("i" . helm-circe))
  :config
  (when ddavis-v-is-mac
    (global-set-key (kbd "s-i") 'helm-circe)))


(use-package erc
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT")
        erc-user-full-name "Doug Davis"
        erc-kill-server-buffer-on-quit t
        erc-kill-buffer-on-part t))

(use-package erc-hl-nicks
  :after erc)


(provide 'ddavis-irc)
;;; ddavis-irc.el ends here

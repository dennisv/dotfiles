;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq display-line-numbers-type nil)

(setq doom-theme 'doom-tomorrow-night)
(setq doom-font (font-spec :family "Iosevka" :size 13))

(set-email-account! "personal"
  '((mu4e-sent-folder       . "/vermeulen/Sent")
    (mu4e-drafts-folder     . "/vermeulen/Drafts")
    (mu4e-trash-folder      . "/vermeulen/Trash")
    (mu4e-refile-folder     . "/vermeulen/Archive")
    (smtpmail-smtp-user     . "dennis@vermeulen.click")
    (user-mail-address      . "dennis@vermeulen.click")    ;; only needed for mu < 1.4
    (mu4e-compose-signature . "Gr.\nDennis"))
  t)

(setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

(setq +format-with-lsp nil)

;; (setq +format-on-save-enabled-modes
;;       '(not emacs-lisp-mode
;;             sql-mode
;;             html-mode
;;             mhtml-mode))

;; (setq web-mode-engines-alist
;;       '(("django" . "/\\templates\\/.*\\.html\\'"))
;;       )

;; (def-package! poetry
;;   :after python
;;   :config
;;   (poetry-tracking-mode))

(use-package! py-isort
  :after python
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))

(setq lsp-pyright-venv-path (concat (getenv "HOME") "/.pyenv/versions"))

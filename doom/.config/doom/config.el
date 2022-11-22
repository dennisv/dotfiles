;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq display-line-numbers-type nil)

(setq doom-theme 'doom-tomorrow-night)
(setq doom-font (font-spec :family "Iosevka" :size 13))

(after! mu4e
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;; Inspired by https://gist.github.com/maxminoS/bd29609ae2969545efaa871cfd5755e8
(if (file-readable-p "~/.mbsyncrc")
    (with-temp-buffer
      (insert-file-contents "~/.mbsyncrc")
      (keep-lines "\\(?:# Account: \\|# Full Name: \\|# SMTP \\|IMAPAccount \\|User \\|Path \\)")
      (replace-regexp "\\(?:# Full Name: \\|# SMTP \\|IMAPAccount \\|User \\|Path \\)" "\ ")
      (let ((idx 0))
        (dolist (account (split-string (buffer-string) "\\(# Account: \\).*\n" t))
          (let* ((data (split-string account "\n" t))
                 (full-name (car data))
                 (smtp (nth 1 data))
                 (imapaccount (nth 2 data))
                 (user (nth 3 data))
                 (path (concat "/" (file-name-nondirectory (directory-file-name (car (last data)))))))
            (set-email-account! imapaccount
                                `((user-mail-address      . ,user)
                                  (user-full-name         . ,full-name)
                                  (mu4e-sent-folder       . ,(concat path "/Sent"))
                                  (mu4e-drafts-folder     . ,(concat path "/Drafts"))
                                  (mu4e-trash-folder      . ,(concat path "/Trash"))
                                  (mu4e-refile-folder     . ,(concat path "/Archive"))
                                  (smtpmail-smtp-server   . ,smtp)
                                  (smtpmail-smtp-user     . ,user)
                                  (user-mail-address      . ,user)
                                  (mu4e-compose-signature . "Gr.\nDennis")
                                  (mu4e-bookmarks .
                                                  ((:name ,(concat "Unread - " user)
                                                    :query ,(concat "flag:unread AND NOT flag:trashed AND m:" path "/Inbox")
                                                    :key ?u)
                                                   (:name "Unread - All"
                                                    :query "flag:unread AND NOT flag:trashed"
                                                    :key ?U)
                                                   (:name "Today - All"
                                                    :query "date:today..now"
                                                    :key ?T)
                                                   (:name "Week - All"
                                                    :query "date:7d..now"
                                                    :key ?W)
                                                   ))
                                  (mu4e-maildir-shortcuts .
                                                          ((:maildir ,(concat path "/Inbox")   :key ?i)
                                                           (:maildir ,(concat path "/Archive") :key ?a)
                                                           (:maildir ,(concat path "/Sent")    :key ?s)
                                                           (:maildir ,(concat path "/Draft")   :key ?d)
                                                           (:maildir ,(concat path "/Trash")   :key ?t)))
                                  )
                                (equal idx 0))
            )
          (setq idx (1+ idx))))))

(setq +format-with-lsp nil)

;; (setq web-mode-engines-alist
;;       '(("django" . "/\\templates\\/.*\\.html\\'"))
;;       )

(after! lsp-haskell
  (setq lsp-haskell-formatting-provider "brittany"))

;; (def-package! poetry
;;   :after python
;;   :config
;;   (poetry-tracking-mode))

(use-package! py-isort
  :after python
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))

(setq lsp-pyright-venv-path (concat (getenv "HOME") "/.pyenv/versions"))

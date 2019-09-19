;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq display-line-numbers-type nil)

;; (setq +format-on-save-enabled-modes
;;       '(not emacs-lisp-mode
;;             sql-mode
;;             html-mode
;;             mhtml-mode))

(setq web-mode-engines-alist
      '(("django" . "/\\templates\\/.*\\.html\\'"))
      )

(def-package! lsp-python-ms
  :after lsp-mode)

(def-package! py-isort
  :after python
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))

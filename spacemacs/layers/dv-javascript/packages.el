(defconst dv-javascript-packages
  '(
    add-node-modules-path
    company-flow
    eslintd-fix
    flycheck
    prettier-js
    rjsx-mode
    ))

(defun dv-javascript/init-eslintd-fix ()
  (use-package eslintd-fix
    :defer t
    :commands eslintd-fix-mode
    :init
    (progn
      (add-hook 'rjsx-mode-hook #'eslintd-fix-mode t)
      ;; (add-hook 'rjsx-mode-hook #'dv-javascript/set-eslintd-fix-preprocess-command t)))
  )))

;; (defun dv-javascript/set-eslintd-fix-preprocess-command ()
;;   (let ((prettier (executable-find "prettier")))
;;     (when prettier
;;       (setq-local eslintd-fix-preprocess-command
;;                   (concat
;;                    prettier
;;                    " --trailing-comma all"
;;                    " --single-quote")))))

(defun dv-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

      (with-eval-after-load 'rjsx-mode
        (define-key rjsx-mode-map "<" nil)
        (define-key rjsx-mode-map (kbd "C-d") nil))

      (setq
       js2-mode-show-strict-warnings nil
       js2-mode-show-parse-errors nil
       js-indent-level 2
       js2-basic-offset 2
       js2-strict-trailing-comma-warning nil
       js2-strict-missing-semi-warning nil)

      (add-hook 'rjsx-mode-hook #'dv-javascript/eslintd-set-flycheck-executable t))
    :config
    (modify-syntax-entry ?_ "w" js2-mode-syntax-table)))

(defun dv-javascript/post-init-add-node-modules-path ()
  (add-hook 'web-typescript-mode-hook #'add-node-modules-path)
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'typescript-mode-hook #'add-node-modules-path)
  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook #'add-node-modules-path)))

;; (defun dv-javascript/init-add-node-modules-path ()
;;   (use-package add-node-modules-path
;;     :defer t
;;     :init
;;     (progn
;;       (add-hook 'web-typescript-mode-hook #'add-node-modules-path)
;;       (add-hook 'web-mode-hook #'add-node-modules-path)
;;       (add-hook 'typescript-mode-hook #'add-node-modules-path)
;;       (with-eval-after-load 'rjsx-mode
;;         (add-hook 'rjsx-mode-hook #'add-node-modules-path)))))

(defun dv-javascript/post-init-company-flow ()
  (spacemacs|add-company-backends
    :backends
    '((company-flow :with company-dabbrev-code)
      company-files)))

(defun dv-javascript/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'json-jsonlint flycheck-disabled-checkers))

  (spacemacs/enable-flycheck 'rjsx-mode))

(defun dv-javascript/init-prettier-js ()
  (use-package prettier-js
    :defer t
    :init
    (progn
      (add-hook 'rjsx-mode-hook 'prettier-js-mode)
      (setq prettier-js-args '(
                               "--trailing-comma" "all"
                               "--single-quote")))))

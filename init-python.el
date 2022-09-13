;; Python stuff
;;(setq flycheck-flake8rc "/path/to/your/flake8-config-file")
(add-hook 'python-mode-hook
          (lambda ()
            (interactive)
            ;; (flycheck-mode)
            (setq flycheck-display-errors-function
                  #'flycheck-display-error-messages-unless-error-list)
            (setq flycheck-python-flake8-executable "/usr/local/bin/flake8")
            ;; (global-set-key (kbd "M-n") ’flycheck-next-error)
            ;; (global-set-key (kbd "M-p") ’flycheck-previous-error)
            (global-set-key (kbd "C-)") 'sp-forward-slurp-sexp)
            (global-set-key (kbd "C-}") 'sp-forward-barf-sexp)
            (smartparens-mode)))

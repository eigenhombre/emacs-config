;; Indentation
(setq js-indent-level 2)
(setq js2-highlight-level 3)

;; JavaScript.................
;; c.f. https://truongtx.me/2014/02/22/emacs-using-paredit-with-non-lisp-mode :
;; https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs
(add-hook 'js-mode-hook
	  (lambda ()
	    (require 'nodejs-repl)
	    (paredit-mode 1)
	    (hs-minor-mode 1)
	    (define-key js-mode-map "{" 'paredit-open-curly)
	    (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
	    (define-key js-mode-map (kbd "C-o j") 'nodejs-repl)
	    (define-key js-mode-map (kbd "s-i") 'nodejs-repl-send-last-sexp)
	    (setq-default indent-tabs-mode nil)
	    (define-key js-mode-map (kbd "s-I") 'nodejs-repl-send-region)
	    (set (make-local-variable 'paredit-space-for-delimiter-predicates)
		 '((lambda (endp delimiter) nil)))
	    ;;(my-paredit-nonlisp)
	    (aggressive-indent-mode 1)))

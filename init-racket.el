;; Racket setup
(defun racket-eval-insert-last-sexp ()
  "Eval the previous sexp asynchronously and `message' the result."
  (interactive)
  (racket--cmd/async
   `(eval
     ,(buffer-substring-no-properties (racket--repl-last-sexp-start)
                                      (point)))
   (lambda (v)
     (insert (substring v
                        2
                        (- (length v) 2))))))

(add-hook 'racket-mode-hook
          (lambda ()
            (message "Hi, it's racket.")
            (paredit-mode 1)
            (define-key racket-mode-map (kbd "s-i")
              'racket-send-last-sexp)
            (define-key racket-mode-map (kbd "C-o y")
              (lambda ()
	        (interactive)
	        (insert "\n;;=>\n'")
                ;;(insert (racket-eval-insert-last-sexp))
                (insert (racket-insert-last-sexp))))
            (define-key racket-mode-map (kbd "s-I")
              (lambda ()
                (interactive)
                (paredit-forward)
                (racket-send-last-sexp)))))

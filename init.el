;;; init -- Emacs Initialization Settings

;;; Commentary:

;; Some of this was adapted from Matthew Wampler-Doty (https://github.com/xcthulhu)

;;; Code:

(when window-system
  (setq ns-pop-up-frames nil)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))


(defun filter (pred lst)
  "Use PRED to filter a list LST of elements."
  (delq nil (mapcar (lambda (x) (and (funcall pred x) x)) lst)))


;;;; Package management setup:
(require 'package)
;; (add-to-list 'package-archives '("MELPA" . "http://melpa.milkbox.net/packages/" ) t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


(defvar my-packages)
(setq my-packages
      '(company auto-complete autopair ac-cider cider color-theme
		zenburn-theme diminish goto-last-change
		main-line maxframe clojure-mode epl popup
		rainbow-delimiters smex undo-tree flycheck
		flycheck-hdevtools git-timemachine paredit
		auto-indent-mode slamhound lorem-ipsum
		midje-mode hungry-delete))


;;;; Install my-packages as necessary:
(let ((uninstalled-packages (filter (lambda (x) (not (package-installed-p x))) my-packages)))
  (when (and (not (equal uninstalled-packages '()))
	     (y-or-n-p (format "Install packages %s?"  uninstalled-packages)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-packages)))


(setq inhibit-splash-screen t          ; No splash screen
      initial-scratch-message nil)     ; No scratch message


(define-key emacs-lisp-mode-map (kbd "<s-return>") 'eval-last-sexp)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)                      ; flycheck-mode
(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode)                   ; auto-indent-mode
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (paredit-mode 1)
	    (autopair-mode 0)))


;; 2 space Ruby indents
(setq ruby-indent-level 2)


;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)


;; ;; Autocomplete mode
(require 'auto-complete)
(add-hook 'prog-mode-hook 'auto-complete-mode)
;;(add-hook 'after-init-hook 'global-company-mode)


;;;; Clever hack so lambda shows up as λ
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(lambda\\)\\>"
    (0 (prog1 ()
	 (compose-region (match-beginning 1)
			 (match-end 1)
			 ?λ))))))


;;;; Clojure goodness:

;; Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; show-paren-mode
(require 'paren)
(set-face-background 'show-paren-match "white")
(add-hook 'prog-mode-hook 'show-paren-mode)


;; Remember our place
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")


;; Don't go to REPL buffer when starting Cider:
(setq cider-repl-pop-to-buffer-on-connect nil)


;; Append result of evaluating previous expression (Clojure):
(defun cider-eval-last-sexp-and-append ()
  "Evaluate the expression preceding point and append result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    ;; we have to be sure the evaluation won't result in an error
    (cider-eval-and-get-value last-sexp)
    (with-current-buffer (current-buffer)
      (insert ";;=>\n"))
    (cider-interactive-eval-print last-sexp)))


(defun cider-format-with-out-str-pprint-eval (form)
  "Return a string of Clojure code that will return pretty-printed FORM."
  (format "(clojure.core/let [x %s] (with-out-str (clojure.pprint/pprint x)))" form))


(defun cider-eval-last-sexp-and-pprint-append ()
  "Evaluate the expression preceding point and append pretty-printed result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    ;; we have to be sure the evaluation won't result in an error
    (with-current-buffer (current-buffer)
      (insert ";;=>\n")
      (insert (cider-eval-and-get-value (cider-format-with-out-str-pprint-eval last-sexp))))))


;; A few paredit things, also from whattheemacsd.com:
(defun paredit--is-at-start-of-sexp ()
  (and (looking-at "(\\|\\[")
       (not (nth 3 (syntax-ppss))) ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun paredit-duplicate-closest-sexp ()
  (interactive)
  ;; skips to start of current sexp
  (while (not (paredit--is-at-start-of-sexp))
    (paredit-backward))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))


(defun highlight-long-lines ()
  "Turn on highlighting of long lines."
  (interactive)
  (highlight-lines-matching-regexp ".\\{81\\}" 'hi-pink))


(defun unhighlight-long-lines ()
  "Turn off highlighting of long lines."
  (interactive)
  (unhighlight-regexp "^.*\\(?:.\\{81\\}\\).*$"))


(defun correct-single-whitespace ()
  "Correct single-spaced Lisp toplevel forms."
  (interactive)
  (goto-char 1)
  (while (search-forward-regexp ")\n\n(" nil t)
    (replace-match ")\n\n\n(" t nil)))


(add-hook 'clojure-mode-hook
          '(lambda ()
             (paredit-mode 1)
             (highlight-long-lines)
             (define-key clojure-mode-map (kbd "C-c e") 'shell-eval-last-expression)
             (define-key clojure-mode-map (kbd "C-o j") 'cider-jack-in)
             (define-key clojure-mode-map (kbd "C-o J") 'cider-restart)
	     (define-key clojure-mode-map (kbd "C-o y")
               'cider-eval-last-sexp-and-append)
             (define-key clojure-mode-map (kbd "C-o Y")
               'cider-eval-last-sexp-and-pprint-append)
	     (define-key clojure-mode-map (kbd "s-i") 'cider-eval-last-sexp)
             (define-key clojure-mode-map (kbd "C-c x") 'shell-eval-defun)))


;; Minibuffer size
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.5))))

;; ;; Keybindings

(global-set-key [S-deletechar]  'kill-ring-save)
;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)


(define-key function-key-map "\e[1~" [find])
(define-key function-key-map "\e[2~" [insertchar])
(define-key function-key-map "\e[3~" [deletechar])
(define-key function-key-map "\e[4~" [select])
(define-key function-key-map "\e[5~" [prior])
(define-key function-key-map "\e[6~" [next])
(define-key global-map [select] 'set-mark-command)
(define-key global-map [insertchar] 'yank)
(define-key global-map [deletechar] 'kill-region)


(global-unset-key "\C-o")  ; make this available as a personal prefix
(global-unset-key "\C- ")
(global-set-key "\C-@" 'other-window)
(global-set-key [?\C- ] 'other-window)
(global-set-key "\C-A" 'split-window-horizontally)
(global-set-key "\C-oa" 'split-window-vertically)
(global-set-key "\C-K" 'kill-line)
(global-set-key "\C-os" 'isearch-forward-regexp)
(global-set-key "\C-oS" (lambda () (interactive)
                          (let ((currentbuf (get-buffer-window (current-buffer)))
				(newbuf     (generate-new-buffer-name "*shell*")))
			    (generate-new-buffer newbuf)
                            (set-window-dedicated-p currentbuf nil)
                            (set-window-buffer currentbuf newbuf)
			    (shell newbuf))))
(global-set-key "\C-oD" 'find-name-dired)
(global-set-key "\C-xS" 'sort-lines)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-ok" 'comment-region)
(global-set-key "\C-ou" 'uncomment-region)
(global-set-key "\C-oe" 'eval-current-buffer)
(global-set-key "\C-oE" (lambda ()
			  (interactive)
			  (find-file "~/.emacs.d/init.el")))
(global-set-key "\C-od" 'delete-horizontal-space)
;; Set up Midje fact with mark inserted at beginning of comment text
;; (refill as needed in appropriate columns, using C-oF).
(global-set-key "\C-of" (lambda ()
                          (interactive)
			  (insert "(fact                               \"\"\n\n  )")
			  (backward-char 6)
                          (set-mark (point))))
;; Perform the refill operation for the text string in a Midje fact:
(global-set-key "\C-oF" (lambda ()
			  (interactive)
			  (set-left-margin (mark) (point) 37)
                          (fill-region (mark) (point))))
(global-set-key "\C-ob" 'backward-word)
(global-set-key "\C-oq" 'query-replace-regexp)
(global-set-key "\C-oQ" 'correct-single-whitespace)
(global-set-key "\C-on" 'flymake-goto-next-error)
(global-set-key "\C-oL" 'lorem-ipsum-insert-paragraphs)
(global-set-key "\C-]"  'fill-region)
(global-set-key "\C-ot" 'beginning-of-buffer)
(global-set-key "\C-N" 'enlarge-window)
(global-set-key "\C-o\C-n" 'enlarge-window-horizontally)
(global-set-key "\C-oc" 'paredit-duplicate-closest-sexp)
(global-set-key "\C-ol" 'goto-line)
(global-set-key "\C-ob" 'end-of-buffer)
(global-set-key "\C-op" 'fill-region)
(global-set-key "\C-og" 'save-buffers-kill-emacs)
(global-set-key "\C-od" 'downcase-region)
(global-set-key "\C-oR" 'indent-region)
(global-set-key "\C-or" 'rgrep)
(global-set-key "\C-oo" 'overwrite-mode)
(global-set-key "\C-L" 'delete-other-windows)
(global-set-key "\C-B" 'scroll-down)
(global-set-key "\C-F" 'scroll-up)
(global-set-key "\C-V" 'save-buffer)
(global-set-key "\C-R" 'isearch-forward)
(global-set-key "\C-^" 'wnt-alog-add-entry)
(global-set-key "\C-T" 'set-mark-command)
(global-set-key "\C-Y" 'yank)
(global-set-key "\C-D" 'backward-delete-char-untabify)
(global-set-key "\C-\\" 'shell)
(global-set-key "\C-oi" 'quoted-insert)
(global-set-key "\e[1~" 'isearch-forward)
(global-set-key [select] 'set-mark-command)
(global-set-key [insertchar] 'yank)
(global-set-key [deletechar] 'kill-region)
(global-set-key "\C-\\" 'shell)
(global-set-key "\C-oi" 'quoted-insert)
(global-set-key "\e[1~" 'isearch-forward)
(global-set-key [select] 'set-mark-command)
(global-set-key [insertchar] 'yank)
(global-set-key [deletechar] 'kill-region)
(global-set-key "\C-oH" 'highlight-long-lines)
(global-set-key "\C-oh" 'unhighlight-long-lines)

;; Thanks http://whattheemacsd.com/ :
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Show trailing whitespace, 'cause we hates it:
(setq-default show-trailing-whitespace t)


(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
   the user's shell.  This is particularly useful under Mac OSX, where
   GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell
	 (replace-regexp-in-string "[ \t\n]*$" ""
				   (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))


;; Enable keyboard shortcuts for resizing:
(when window-system
  (set-exec-path-from-shell-PATH)
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease))


;; Mode line hacks from http://whattheemacsd.com/
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))


(rename-modeline "clojure-mode" clojure-mode "Clj")


(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "From whattheemacsd.com: With this snippet, another press of C-d
  will kill the buffer.
  It's pretty nice, since you then just tap C-d twice to get rid of the
  shell and go on about your merry way."
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))


(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))


(when window-system
  (load-theme 'zenburn t))

;; Stuff for columns and rows:
;(current-column)
;(line-number-at-pos (point))

;; Try out hungry-delete
;; (require 'hungry-delete)
;; (global-hungry-delete-mode)


;; Common Lisp stuff
;; (add-to-list 'load-path )
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(add-hook 'lisp-mode-hook
          '(lambda ()
             (paredit-mode 1)
             (highlight-long-lines)
             (define-key lisp-mode-map (kbd "C-o j") 'slime)
	     (define-key lisp-mode-map (kbd "s-i") 'slime-eval-last-expression)))


(provide 'init)
;;; init.el ends here

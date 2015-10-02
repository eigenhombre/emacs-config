(package-initialize)

(defun filter (pred lst)
  "Use PRED to filter a list LST of elements."
  (delq nil (mapcar (lambda (x) (and (funcall pred x) x)) lst)))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("MELPA" . "http://melpa.org/packages/") t)
(package-initialize)
(defvar my-packages)
(setq my-packages
      '(cider
	clojure-mode
        clj-refactor
	company
	helm
	helm-projectile
	magit
	paredit
	projectile
	org
	rainbow-delimiters
	zenburn-theme))


;; Install missing packages:
(let ((uninstalled-packages (filter (lambda (x) (not (package-installed-p x)))
                                    my-packages)))
  (when (and (not (equal uninstalled-packages '()))
             (y-or-n-p (format "Install packages %s?"  uninstalled-packages)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-packages)))


;; Stuff for running shells within Emacs.........................
;;
;; Path Magic
;; Smooth the waters for starting processes from the shell. “Set up
;; Emacs’ `exec-path’ and PATH environment variable to match the
;; user’s shell. This is particularly useful under Mac OSX, where GUI
;; apps are not started from a shell[fn:: See
;; http://stackoverflow.com/questions/8606954/\
;; path-and-exec-path-set-but-emacs-does-not-find-executable]”.
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" ""
          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))


(global-unset-key "\C-o")


;; Moar Shells.........................................................
;; Create shell in new buffer when needed, rather than just loading up
;; the existing shell buffer.
(defun create-shell-in-new-buffer ()
  (interactive)
  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf (generate-new-buffer-name "*shell*")))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)))

(global-unset-key "\C-o")

(global-set-key "\C-oS" 'create-shell-in-new-buffer)

;; Highlighting of long lines.....................................
(defun highlight-long-lines ()
  "Turn on highlighting of long lines."
  (interactive)
  (highlight-lines-matching-regexp ".\\{81\\}" 'hi-pink))


(defun unhighlight-long-lines ()
  "Turn off highlighting of long lines."
  (interactive)
  (unhighlight-regexp "^.*\\(?:.\\{81\\}\\).*$"))


;; Clojure mode hooks.............................................
(defun set-clojure-indents ()
  ;; Handling Clojure indentation for certain macros
  (put-clojure-indent 'DELETE* 2)
  (put-clojure-indent 'GET* 2)
  (put-clojure-indent 'POST* 2)
  (put-clojure-indent 'PUT* 2)
  (put-clojure-indent 'after 1)
  (put-clojure-indent 'after-all 1)
  (put-clojure-indent 'around 1)
  (put-clojure-indent 'before 1)
  (put-clojure-indent 'before-all 1)
  (put-clojure-indent 'context 1)
  (put-clojure-indent 'context* 2)
  (put-clojure-indent 'describe 1)
  (put-clojure-indent 'describe-examples 2)
  (put-clojure-indent 'describe-with-dawg 1)
  (put-clojure-indent 'describe-with-db 1)
  (put-clojure-indent 'describe-with-server 1)
  (put-clojure-indent 'it 1)
  (put-clojure-indent 'match 1)
  (put-clojure-indent 'try 0)
  (put-clojure-indent 'watcher 1)
  (put-clojure-indent 'with 1))


(add-hook 'clojure-mode-hook
          '(lambda ()
             (paredit-mode 1)
             (highlight-long-lines)
	     (clj-refactor-mode 1)
	     (yas-minor-mode 1) ; for adding require/use/import
	     (cljr-add-keybindings-with-prefix "C-c C-t")
             (define-key clojure-mode-map (kbd "C-o x")
	       'cider-eval-defun-at-point)
             (define-key clojure-mode-map (kbd "C-o j") 'cider-jack-in)
             (define-key clojure-mode-map (kbd "C-o J") 'cider-restart)
             (define-key clojure-mode-map (kbd "C-<up>") 'paredit-backward)
             (define-key clojure-mode-map (kbd "C-<down>") 'paredit-forward)
             (define-key clojure-mode-map (kbd "C-o y")
               (lambda ()
		 (interactive)
		 (insert "\n;;=>\n")
		 (cider-eval-last-sexp 't)))
	     (define-key clojure-mode-map (kbd "C-o Y")
	       (lambda ()
		 (interactive)
		 (cider-pprint-eval-last-sexp)))
	     (define-key clojure-mode-map (kbd "s-i") 'cider-eval-last-sexp)
             (define-key clojure-mode-map (kbd "s-I")
	       '(lambda ()
		  (interactive)
		  (paredit-forward)
		  (cider-eval-last-sexp)))
	     (set-clojure-indents)))

;; Find Leiningen.............................................
(add-to-list 'exec-path "/usr/local/bin")


;; Cider setup................................................
;;
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-auto-select-error-buffer nil)
(setq cider-interactive-eval-result-prefix ";; => ")
(setq cider-repl-history-file (concat user-emacs-directory "../.cider-history"))
;; Fix https://github.com/clojure-emacs/cider/issues/1258:
(defvar cider-eval-progress-bar-show nil)


;; JSON->Clojure snippet from Brett Lischalk
(defun json->clj-map ()
  (interactive)
  (if (region-active-p)
      (replace-regexp "\\(\"\\([A-z0-9_-]+\\)\"\s*:\\)" ":\\2 "
                      nil (region-beginning) (region-end))))

(global-set-key (kbd "C-c C-j h") 'json->clj-map)


;; Lots of keybindings
;;
;; Many of these are extremely old, having followed me from machine to
;; machine over the years. Some could probably be deleted.
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

(global-unset-key "\C- ")
(global-set-key [?\C- ] 'other-window)
(global-set-key "\C-oW" (lambda ()
                          (interactive)
                          (org-babel-load-file (concat user-emacs-directory
						       "org/init.org"))))
(global-set-key "\C-A" 'split-window-horizontally)
(global-set-key "\C-oa" 'split-window-vertically)
(global-set-key "\C-K" 'kill-line)
(global-set-key "\C-os" 'isearch-forward-regexp)
(global-set-key "\C-oD" 'find-name-dired)
(global-set-key "\C-xS" 'sort-lines)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-ok" 'comment-region)
(global-set-key "\C-ou" 'uncomment-region)
(global-set-key "\C-on" 'er/expand-region)
(global-set-key "\C-om" 'magit-status)
(global-set-key "\C-oe" 'eval-current-buffer)
(global-set-key "\C-od" 'delete-horizontal-space)
(global-set-key "\C-oH" 'highlight-long-lines)
(global-set-key "\C-oh" 'unhighlight-long-lines)
(global-set-key "\C-ob" 'backward-word)
(global-set-key "\C-oB" 'bury-buffer)
(global-set-key "\C-oq" 'query-replace-regexp)
(global-set-key "\C-oL" 'lorem-ipsum-insert-paragraphs)
(global-set-key "\C-]"  'fill-region)
(global-set-key "\C-ot" 'beginning-of-buffer)
(global-set-key "\C-oT" 'toggle-window-split)
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
(global-set-key (kbd "s-0") 'org-todo-list)

;; Shortcuts for jumping directly into or evaluating commonly-used buffers:
(global-set-key "\C-oO" (lambda ()
                          (interactive)
                          (find-file "~/Dropbox/org/toplevel.org")))
(global-set-key "\C-oE" (lambda ()
                          (interactive)
                          (find-file "~/.emacs.d/init.el")))

(global-set-key "\C-oW" (lambda ()
                          (interactive)
			  (find-file "~/.emacs.d/init.el")
			  (eval-buffer)
			  (message "ok!")))

;; Keyboard shortcuts for joining lines before and after point (thanks
;; to http://whattheemacsd.com/ for the (join-line -1) trick):
(global-set-key (kbd "M-j")
  (lambda () (interactive) (join-line -1)))
(global-set-key "\C-oo" 'join-line)

;; Show trailing whitespace, `cause we hates it....
(setq-default show-trailing-whitespace t)

;; Stuff related to configuring Emacs-in-a-window
;;
;; When running GUI Emacs (i.e. on OS-X, which is the only way I run
;; Emacs these days anyways), set the theme to Zenburn, turn off
;; visual noise, fix up the PATH for shells, and allow resizing of
;; window.
(when window-system
  (load-theme 'zenburn t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-exec-path-from-shell-PATH)
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease))

;; Don’t pop up newly-opened files in a new frame – use existing one:
(setq ns-pop-up-frames nil)

(defun jj-move-forward-and-eval ()
  (lambda ()
    (paredit-forward)
   (eval (preceding-sexp))))


;; General Lisp stuff
;; Rainbow delimiters for all programming major modes:
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; Show paren balancing nicely:
(require 'paren)
(set-face-background 'show-paren-match "white")
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Stuff for Editing Emacs Lisp......................
;; I add a hook for evaluating the expression just before point; I’ve
;; played with auto-indent-mode and flycheck-mode but tired of them. I
;; do want paredit though.
(define-key emacs-lisp-mode-map (kbd "<s-return>") 'eval-last-sexp)

;;(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
;;(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)))

(define-key emacs-lisp-mode-map (kbd "s-i")
  'eval-last-sexp)

(define-key emacs-lisp-mode-map (kbd "s-I")
  (lambda ()
    (interactive)
    (paredit-forward)
    ;(eval-last-sexp)
    ))


;; M$ interop................
;; Hide ^M. From http://stackoverflow.com/questions/3048906/\
;; dont-display-m-characters-with-emacs:
(defun hide-ctrl-M ()
  "Hides the disturbing '^M' showing up in files containing mixed UNIX
   and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; JavaScript.................
;; Indentation
(setq js-indent-level 2)

;; (require 'org-install)
;; (require 'ob-tangle)
;; (org-babel-load-file (concat user-emacs-directory "org/init.org"))

;; (org-babel-load-file "tmp.org")

;; Company Mode............
;; stolen from https://github.com/bbatsov/prelude/blob/\
;; fe7997bc6e05647a935e279094a9c571d175e2dc/modules/prelude-company.el
(require 'company)

(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)

(global-company-mode 1)


;; Correcting single-whitespaced toplevel forms
(defun correct-single-whitespace ()
  "Correct single-spaced Lisp toplevel forms."
  (interactive)
  (goto-char 1)
  (while (search-forward-regexp ")\n\n(" nil t)
    (replace-match ")\n\n\n(" t nil)))
(global-set-key "\C-oQ" 'correct-single-whitespace)


;; Helm.......................
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

;; Projectile.................
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Org Mode...................
(require 'org)
;; Export ” as “ and ”:
(setq org-export-with-smart-quotes t)
;; GTD-style TODO states:
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "SOMEDAY" "DONE" "CANCELED")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("STARTED" . "yellow")
	("DONE" . "green")
	("CANCELED" . (:foreground "blue" :weight bold))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-push-always-verify nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Backups...........................
;; Tell Emacs to write backup files to their own directory, and make backups even for files in revision control:

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq vc-make-backup-files t)

(provide 'init)

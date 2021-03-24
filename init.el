(defun filter (pred lst)
  "Use PRED to filter a list LST of elements."
  (delq nil (mapcar (lambda (x) (and (funcall pred x) x)) lst)))

(require 'package)
(add-to-list 'package-archives
             '("ELPA" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("MELPA" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(defvar my-packages)
(setq my-packages
      '(ac-js2
        aggressive-indent
        bash-completion
	beacon
	cider
	clojure-mode
	clojure-snippets
        decide
	expand-region
	forecast
	gist
	helm
	helm-projectile
	hideshow
	js2-mode
	json-mode
        flycheck
	flycheck-clj-kondo
	lorem-ipsum
	magit
	magit-gh-pulls
	markdown-mode
	multiple-cursors
	nodejs-repl
        olivetti
	org
        org-roam
	paredit
        projectile
	rainbow-delimiters
        restclient
        rust-mode
        scala-mode
	tagedit
	which-key
	yasnippet
        adoc-mode
        clj-refactor
        company
        geiser
        git-timemachine
        hcl-mode
        htmlize
        racket-mode
        scala-mode
	zenburn-theme))


;; Install missing packages:
(let ((uninstalled-packages (filter (lambda (x) (not (package-installed-p x)))
				    my-packages)))
  (when (and (not (equal uninstalled-packages '()))
             (y-or-n-p (format "Install packages %s?"  uninstalled-packages)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-packages)))


;; Startup.........................................................
;; Some configuration to make things quieter on start up:

(setq inhibit-splash-screen t
      initial-scratch-message nil)


;; Turn on recentf-mode for reopening recently used files:

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Stuff for running shells within Emacs...........................
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


;; Kill shell buffers quickly.....................................

;; “With this snippet, [a second] press of C-d will kill the
;; buffer. It’s pretty nice, since you then just tap C-d twice to get
;; rid of the shell and go on about your merry way[fn:: From
;; http://whattheemacsd.com.]”
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))


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

;; (global-unset-key "\C-w")
;; (global-set-key "\C-wl" (lambda ()
;;                           (interactive)
;;                           (prettify-symbols-mode 1)))
;; (global-set-key "\C-wL" (lambda ()
;;                           (interactive)
;;                           (prettify-symbols-mode 0)))
(global-unset-key "\C-o")

(global-set-key "\C-oS" 'create-shell-in-new-buffer)

;; Per http://stackoverflow.com/questions/18278310/emacs-ansi-term-not-tab-completing : fix autocomplete
(add-hook 'term-mode-hook (lambda()
			    (setq yas-dont-activate t)))

(global-set-key "\C-a" 'split-window-horizontally)

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
  (put-clojure-indent 'DELETE 2)
  (put-clojure-indent 'GET 2)
  (put-clojure-indent 'ANY 2)
  (put-clojure-indent 'POST 2)
  (put-clojure-indent 'PUT 2)
  (put-clojure-indent 'addtest 1)
  (put-clojure-indent 'after 1)
  (put-clojure-indent 'after-all 1)
  ;; (put-clojure-indent 'api 1)
  (put-clojure-indent 'are 1)
  (put-clojure-indent 'around 1)
  (put-clojure-indent 'before 0)
  (put-clojure-indent 'before-all 0)
  (put-clojure-indent 'cond-> 1)
  (put-clojure-indent 'check-cw-metric 2)
  (put-clojure-indent 'context 2)
  (put-clojure-indent 'context* 2)
  (put-clojure-indent 'describe 1)
  (put-clojure-indent 'describe-examples 2)
  (put-clojure-indent 'describe-with-dawg 1)
  (put-clojure-indent 'describe-with-db 1)
  (put-clojure-indent 'describe-with-es 1)
  (put-clojure-indent 'describe-with-mock-etl-state 1)
  (put-clojure-indent 'describe-with-server 1)
  (put-clojure-indent 'discard-stdout 0)
  (put-clojure-indent 'do-rate-limited 1)
  (put-clojure-indent 'do-until-input 1)
  (put-clojure-indent 'do-with-save-config 1)
  (put-clojure-indent 'fact 1)
  (put-clojure-indent 'facts 1)
  (put-clojure-indent 'props/for-all 1)
  (put-clojure-indent 'for-all-mock-reports 1)
  (put-clojure-indent 'hellish-copy 4)
  (put-clojure-indent 'holiday-test 2)
  (put-clojure-indent 'html/at 1)
  (put-clojure-indent 'is 0)
  (put-clojure-indent 'it 1)
  (put-clojure-indent 'match 1)
  (put-clojure-indent 'metrics/time 1)
  (put-clojure-indent 'middleware 1)
  (put-clojure-indent 'mock-dl-bad 0)
  (put-clojure-indent 'mock-dl-good-and-fast 0)
  (put-clojure-indent 'mock-dl-short 0)
  (put-clojure-indent 'nolog 0)
  (put-clojure-indent 'patterns-match 0)
  (put-clojure-indent 'possible-schedules 1)
  (put-clojure-indent 'schedule-dates 1)
  (put-clojure-indent 'pending 1)
  (put-clojure-indent 'problem 1)
  (put-clojure-indent 'process-safely 2)
  (put-clojure-indent 'prop/for-all 1)
  (put-clojure-indent 'route-middleware 1)
  (put-clojure-indent 'section 1)
  (put-clojure-indent 'service-unavailable 0)
  (put-clojure-indent 'should 0)
  (put-clojure-indent 'solves 0)
  (put-clojure-indent 'test-location 1)
  (put-clojure-indent 'testing-salesforce 4)
  (put-clojure-indent 'throws-with 1)
  (put-clojure-indent 'try 0)
  (put-clojure-indent 'try+ 0)
  (put-clojure-indent 'watcher 1)
  (put-clojure-indent 'without-logging 0)
  (put-clojure-indent 'wrap-response 1)
  (put-clojure-indent 'wait-for 1)
  (put-clojure-indent 'wcar 1)
  (put-clojure-indent 'with 1)
  (put-clojure-indent 'with-expect-write-to-kinesis 0)
  (put-clojure-indent 'with-level 1)
  (put-clojure-indent 'wrap-response 3)
  (put-clojure-indent 'with-audit-client 2)
  (put-clojure-indent 'with-adjusted-laa 1)
  (put-clojure-indent 'require-valid-admin-api-key 1)
  (put-clojure-indent 'require-valid-service-api-key 1)
  (put-clojure-indent 'undocumented 0)
  (put-clojure-indent 'symbol-macrolet 1)
  (put-clojure-indent 'subsection 1)
  (put-clojure-indent 'perf/p 1)
  (put-clojure-indent 'op/p 1)
  (put-clojure-indent 'log-timing 1)
  (put-clojure-indent 'subsubsection 1)
  (put-clojure-indent 'prop/for-all 1))


(defun convert-selection-to-link ()
  "For unmark: Convert selected text to Hiccup link"
  (interactive)
  (let* ((url (read-from-minibuffer "Enter link URL: "))
	 (bounds (if (use-region-p)
		     (cons (region-beginning) (region-end))
		   (bounds-of-thing-at-point 'symbol)))
         (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
	 (to-replace (insert (concat "\" [:a {:href \"" url "\"} \"" text "\"] \""))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert to-replace))))


(defun convert-selection-to-code ()
  "For unmark: Convert selected text to Hiccup link"
  (interactive)
  (let* ((bounds (if (use-region-p)
		     (cons (region-beginning) (region-end))
		   (bounds-of-thing-at-point 'symbol)))
         (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
	 (to-replace (insert (concat "\" [:code \"" text "\"] \""))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert to-replace))))

(add-hook 'clojure-mode-hook
          '(lambda ()
             (paredit-mode 1)
	     (aggressive-indent-mode 1)
             (highlight-long-lines)
	     (clj-refactor-mode 1)
	     (yas-minor-mode 1) ;; for adding require/use/import
	     (cljr-add-keybindings-with-prefix "C-c C-t")
             (define-key clojure-mode-map (kbd "C-o x")
	       'cider-eval-defun-at-point)
             (define-key clojure-mode-map (kbd "C-o j") 'cider-jack-in)
             (define-key clojure-mode-map (kbd "C-o J")
               (lambda () (interactive) (cider-quit) (cider-jack-in)))
	     ;;(define-key clojure-mode-map (kbd "C-o K") 'convert-selection-to-link)
	     (define-key clojure-mode-map (kbd "C-o C") 'convert-selection-to-code)
             (define-key clojure-mode-map (kbd "C-<up>") 'paredit-backward)
             (define-key clojure-mode-map (kbd "C-<down>") 'paredit-forward)
             ;;            (define-key clojure-mode-map (kbd "C-o SPC")
             ;;              (lambda ()
             ;;                (interactive)
             ;;                (cider-interactive-eval "(let [result (clojure.test/run-tests)]
             ;;     (if
             ;;         (->> result
             ;;              ((juxt :fail :error))
             ;;              (apply +)
             ;;              zero?)
             ;;       (clojure.java.shell/sh \"say\" \"ok\")
             ;;       (clojure.java.shell/sh \"say\" \"fail\"))
             ;; result)")))
             (define-key clojure-mode-map (kbd "C-o y")
               (lambda ()
	         (interactive)
	         (insert "\n;;=>\n\n")
	         (cider-eval-last-sexp 't)
                 (join-line)))
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
             (define-key clojure-mode-map (kbd "C-o C-i")
               (lambda ()
                 (interactive)
                 (cider-auto-test-mode 1)))
	     (set-clojure-indents)))

(add-to-list 'auto-mode-alist '("\\.garden" . clojure-mode))


;; Find Leiningen.............................................
(add-to-list 'exec-path "/usr/local/bin")


;; Cider setup................................................
;;
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-auto-select-error-buffer nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-interactive-eval-result-prefix ";; => ")
(setq cider-repl-history-file (concat user-emacs-directory "../.cider-history"))
;; Fix https://github.com/clojure-emacs/cider/issues/1258:
(defvar cider-eval-progress-bar-show nil)

;; JSON
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

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
;;(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-?") 'help-command)
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
(global-set-key "\C-o`" 'auto-fill-mode)
(global-set-key "\C-a" 'split-window-horizontally)
(global-set-key "\C-oa" 'split-window-vertically)
(global-set-key "\C-K" 'kill-line)
(global-set-key "\C-os" 'isearch-forward-regexp)
;;(global-set-key "\C-oD" 'find-name-dired)
(global-set-key "\C-oD" (lambda ()
                          (interactive)
                          (insert (format-time-string "%Y-%m-%d %H:%M"))))
(global-set-key "\C-xS" 'sort-lines)
;;(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-ok" 'comment-region)
(global-set-key "\C-oK" 'helm-show-kill-ring)
(global-set-key "\C-c\C-f" 'recentf-open-files)
(global-set-key "\C-ou" 'uncomment-region)
(global-set-key "\C-on" 'er/expand-region)
(global-set-key "\C-om" 'magit-status)
(global-set-key "\C-oe" 'eval-current-buffer)
(global-set-key "\C-od" 'delete-horizontal-space)
(global-set-key "\C-oH" 'highlight-long-lines)
(global-set-key "\C-oh" 'unhighlight-long-lines)
(global-set-key "\C-ob" 'backward-word)
(global-set-key "\C-oB" 'bury-buffer)
(global-set-key "\C-o3" 'rot13-region)
(global-set-key "\C-oq" 'query-replace-regexp)
(global-set-key "\C-oL" (lambda ()
                          (interactive)
                          (mark)
                          (lorem-ipsum-insert-paragraphs)
                          (fill-paragraph)))
(global-set-key "\C-]"  'fill-region)
(global-set-key "\C-ot" 'beginning-of-buffer)
(global-set-key "\C-N" 'enlarge-window)
(global-set-key "\C-o\C-n" 'enlarge-window-horizontally)
(global-set-key "\C-oc" 'paredit-duplicate-closest-sexp)
(global-set-key "\C-ol" 'goto-line)
(global-set-key "\C-ob" 'end-of-buffer)
(global-set-key "\C-op" 'fill-region)
(global-set-key "\C-oP" 'fill-paragraph)
(global-set-key "\C-od" 'downcase-region)
;;(global-set-key "\C-or" 'rgrep)
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
(global-set-key "\C-oG" 'helm-projectile-grep)
(global-set-key (kbd "\C-o SPC") 'artist-mode)
(global-set-key [select] 'set-mark-command)
(global-set-key [insertchar] 'yank)
(global-set-key [deletechar] 'kill-region)
(global-set-key (kbd "s-0") 'org-todo-list)

;; Shortcuts for jumping directly into or evaluating commonly-used buffers:
(global-set-key "\C-oO" (lambda ()
                          (interactive)
                          (find-file "~/org/toplevel.org")))
(global-set-key "\C-o0" (lambda ()
                          (interactive)
                          (find-file "~/org/opploans-home.org")))
(global-set-key "\C-o7" (lambda ()
                          (interactive)
                          (find-file "~/org/opploans.org")))
(global-set-key "\C-o8" (lambda ()
                          (interactive)
                          (find-file "~/.bash_profile")))
(global-set-key "\C-o9" (lambda ()
                          (interactive)
                          (find-file "~/.bashrc")))
(global-set-key "\C-oP" (lambda ()
                          (interactive)
                          (find-file "~/org/painting-status.org")))
(global-set-key "\C-oE" (lambda ()
                          (interactive)
                          (find-file "~/.emacs.d/init.el")))

;; (global-set-key "\C-o1"
;; 		(lambda ()
;; 		  (interactive)
;; 		  (cider-interactive-eval
;; 		   "(in-ns 'unmark.impl)
;;                     (generate-blog! \"/Users/jacobsen/Programming/eigenhombre.github.com\")
;;                     (clojure.java.shell/sh \"open\" \"/Users/jacobsen/Programming/eigenhombre.github.com/index.html\")")))

;; Run marginalia on current project. You need
;; [michaelblume/marginalia "0.9.0"] installed in the dependencies for
;; that project.
(global-set-key "\C-o2"
		(lambda ()
		  (interactive
		   (cider-interactive-eval
		    "(require 'marginalia.core)
                     (marginalia.core/run-marginalia nil)
                     (clojure.java.shell/sh \"open\" \"docs/uberdoc.html\")"))))


;; Keyboard shortcuts for joining lines before and after point (thanks
;; to http://whattheemacsd.com/ for the (join-line -1) trick):
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
  (scroll-bar-mode -1)
  (set-exec-path-from-shell-PATH)
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease))
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'zenburn t)

(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))
;; Don’t pop up newly-opened files in a new frame – use existing one:
(setq ns-pop-up-frames nil)

(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)

  (when (display-graphic-p)
    (ns-raise-emacs)))

(defun jj-move-forward-and-eval ()
  (lambda ()
    (paredit-forward)
    (eval (preceding-sexp))))

;; Scrolling
(setq scroll-step            1
      scroll-conservatively  10000)

;; Visual line mode
(global-set-key "\C-oV" 'visual-line-mode)

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

(require 'flycheck-clj-kondo)
;;(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
;;(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)
	    (aggressive-indent-mode 1)))


(define-key emacs-lisp-mode-map (kbd "s-i")
  'eval-last-sexp)

(define-key emacs-lisp-mode-map (kbd "C-o y")
  (lambda ()
    (interactive)
    (insert "\n;;=>\n'")
    (eval-last-sexp 't)
    (insert "\n")))

(define-key emacs-lisp-mode-map (kbd "s-I")
  (lambda ()
    (interactive)
    (paredit-forward)
    (eval-last-sexp nil)))

;; Python stuff
;;(setq flycheck-flake8rc "/path/to/your/flake8-config-file")
(add-hook 'python-mode-hook
          (lambda ()
            (interactive
             (flycheck-mode)
             (setq flycheck-display-errors-function
                   #'flycheck-display-error-messages-unless-error-list)
             (setq flycheck-python-flake8-executable "/usr/local/bin/flake8")
             (global-set-key (kbd "M-n") ’flycheck-next-error)
             (global-set-key (kbd "M-p") ’flycheck-previous-error))))

;; (use-package flycheck
;;              :ensure t
;;              :init (global-flycheck-mode))

;; M$ interop................
;; Hide ^M. From http://stackoverflow.com/questions/3048906/\
;; dont-display-m-characters-with-emacs:
(defun hide-ctrl-M ()
  "Hides the disturbing '^M' showing up in files containing mixed UNIX
   and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; Highlight stuff.
(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))


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
;; https://github.com/bbatsov/helm-projectile/issues/116 :
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(helm-mode 1)

;; Projectile.................
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Hideshow Package...........
(load-library "hideshow")
(add-hook 'clojure-mode-hook 'hs-minor-mode)
(global-set-key [backtab] 'hs-toggle-hiding)
(defvar hs-special-modes-alist
  (mapcar 'purecopy
	  '((c-mode "{" "}" "/[*/]" nil nil)
	    (c++-mode "{" "}" "/[*/]" nil nil)
	    (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
	    (java-mode "{" "}" "/[*/]" nil nil)
	    (js-mode "{" "}" "/[*/]" nil)
            (js-mode "`" "`" "/[*/]" nil))))
(add-hook 'html-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(global-set-key [backtab] 'hs-toggle-hiding)

;; Scheme stuff
;;(require 'geiser-install)

;; Org Mode...................
(require 'org)
(require 'org-install)
(require 'ob-tangle)
(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)
                             (clojure . t)
                             (plantuml . t)))

(setq org-plantuml-jar-path (concat (getenv "HOME")
                                    "/bin/plantuml.jar"))

(setq my/org-babel-evaluated-languages
      '(emacs-lisp plantuml sh))

(defun my-org-confirm-babel-evaluate (lang body)
  (and (not (string= lang "ditaa"))
       (not (string= lang "plantuml"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-babel-clojure-backend 'cider)
(require 'ob-clojure)
;;(require 'cider)

;; (org-babel-load-file (concat user-emacs-directory "org/init.org"))

;; (org-babel-load-file "tmp.org")

;; Export ” as “ and ”:
(setq org-export-with-smart-quotes t)
(setq org-export-backends '(ascii html latex md))
;; GTD-style TODO states:
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "DONE" "WAITING" "CANCELED" "DEFERRED")))
(setq org-log-states-order-reversed nil)
;; (setq org-log-done 'time)
(setq org-log-done nil)
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("STARTED" . "yellow")
	("DONE" . "#5F7F5F")
	("ELSEWHERE" . "#5F7F5F")
	("CANCELED" . "#8CD0D3")))

;; https://emacs.stackexchange.com/questions/19863/how-to-set-my-own-date-format-for-org :
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%A, %B %e, %Y>" . "<%a %b %e %Y %H:%M>"))

;; Convert eval results to SRC blocks
(defun replace-next-results-block ()
  (interactive)
  (search-forward "#+RESULTS:" nil t)
  (replace-match "#+BEGIN_SRC")
  (forward-line)
  (while (string= (string (char-after)) ":")
    (delete-char 2)
    (forward-line))
  (insert "#+END_SRC\n"))

(global-set-key (kbd "s-r")
                (lambda ()
                  (interactive)
                  (replace-next-results-block)))

;; https://maxdiefenbach.de/howto/emacs/org-mode/2017/03/22/org_table_move_cell.html
;; FIXME: Make this more beautiful
;; org-table move cell
;; https://www.mail-archive.com/emacs-orgmode@gnu.org/msg98407.html
;; https://www.reddit.com/r/emacs/comments/583n1x/movecopy_a_cel_to_the_right/
(defun md-org-table-swap-cells (row col nextrow nextcol)
  (interactive)
  (let ((curfield (org-table-get row col))
        (nextfield (org-table-get nextrow nextcol)))
    (org-table-analyze)
    (org-table-put row col nextfield)
    (org-table-put nextrow nextcol curfield)
    (org-table-align)
    (org-table-goto-field (format "@%s$%s" nextrow nextcol))
    (message "md-org-table-swap-cells %s:%s <-> %s:%s"
             (format "@%s$%s" row col) curfield (format "@%s$%s" nextrow nextcol) nextfield)))

(defun md-org-table-swap-cell-right ()
  (interactive)
  (if (org-at-table-p)
      (let* ((col (org-table-current-column))
             (row (org-table-current-dline))
             (nextrow row)
             (nextcol (+ col 1)))
        (md-org-table-swap-cells row col nextrow nextcol))
    (org-shiftright)))
(define-key org-mode-map (kbd "S-<right>") 'md-org-table-swap-cell-right)

(defun md-org-table-swap-cell-left ()
  (interactive)
  (if (org-at-table-p)
      (let* ((col (org-table-current-column))
             (row (org-table-current-dline))
             (nextrow row)
             (nextcol (- col 1)))
        (md-org-table-swap-cells row col nextrow nextcol))
    (org-shiftleft)))
(define-key org-mode-map (kbd "S-<left>") 'md-org-table-swap-cell-left)

(defun md-org-table-swap-cell-up ()
  (interactive)
  (if (org-at-table-p)
      (let* ((col (org-table-current-column))
             (row (org-table-current-dline))
             (nextrow (- row 1))
             (nextcol col))
        (md-org-table-swap-cells row col nextrow nextcol))
    (org-shiftup)))
(define-key org-mode-map (kbd "S-<up>") 'md-org-table-swap-cell-up)

(defun md-org-table-swap-cell-down ()
  (interactive)
  (if (org-at-table-p)
      (let* ((col (org-table-current-column))
             (row (org-table-current-dline))
             (nextrow (+ row 1))
             (nextcol col))
        (md-org-table-swap-cells row col nextrow nextcol))
    (org-shiftdown)))
(define-key org-mode-map (kbd "S-<down>") 'md-org-table-swap-cell-down)



;; Magit / GitHub ...........
;; (require 'magit-gh-pulls)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cljr-favor-prefix-notation t)
 '(global-linum-mode nil)
 '(helm-completion-style (quote emacs))
 '(line-number-mode nil)
 '(magit-push-always-verify nil)
 '(markdown-command "/usr/local/bin/markdown")
 '(package-selected-packages
   (quote
    (rust-mode org-roam bash-completion decide flycheck-clj-kondo flycheck flake8 restclient racket-mode geiser scala-mode ac-js2 adoc-mode aggressive-indent bea beacon cider clj-refactor clojure-mode clojure-snippets company expand-region forecast git-timemachine hcl-mode helm helm-projectile htmlize js2-mode json-mode lorem-ipsum magit magit-gh-pulls markdown-mode multiple-cursors nodejs-repl olivetti paredit projectile rainbow-delimiters tagedit which-key yasnippet zenburn-theme
               (quote
                (recentf-max-menu-items 100))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)


(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-load-directory "~/.emacs.d/snippets")

;; Org / Yasnippet conflict (http://orgmode.org/manual/Conflicts.html):
(add-hook 'org-mode-hook
          (lambda ()
            ;;(org-set-local 'yas/trigger-key [tab])
            (yas-minor-mode 1)
            (require 'ob-plantuml)
            (setq org-babel-default-header-args:sh
                  '((:prologue . "exec 2>&1") (:epilogue . ":")))
            (define-key org-mode-map (kbd "C-a") 'split-window-horizontally)
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))


;; Moving sexps up and down.......................................
(defun reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg))
  ;; when transpose-sexps can no longer transpose, it throws an error and code
  ;; below this line won't be executed. So, we don't have to worry about side
  ;; effects of backward-sexp and forward-sexp.
  (backward-sexp arg)
  (forward-sexp 1))


(global-set-key (kbd "S-s-<down>")
		(lambda ()
		  (interactive)
		  (transpose-sexps 1)))


(global-set-key (kbd "S-s-<up>")
		(lambda ()
		  (interactive)
		  (reverse-transpose-sexps 1)))


;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Backups...........................
;; Tell Emacs to write backup files to their own directory, and make backups even for files in revision control:

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq vc-make-backup-files t)

;; Pesky dialog boxes :-(
;; http://superuser.com/questions/125569/how-to-fix-emacs-popup-dialogs-on-mac-os-x
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))


(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; Omit #@!(*&^&! tabs!!!!
(setq-default indent-tabs-mode nil)

;; Markdown mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown" t)
(add-to-list 'auto-mode-alist '("\\.md.html\\'" . markdown-mode)
             (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; Beacon Mode
(beacon-mode 1)
(setq beacon-push-mark 35)
(setq beacon-color "#888888")

;; which-key
(require 'which-key)
(which-key-mode)

;; Pop shells in current frame
(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))

;; suppress irritating terminal warnings:
(setenv "PAGER" "cat")

;; ;;; COMMON LISP STUFF
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; ;;(setq inferior-lisp-program "/usr/local/bin/sbcl")
;; (setq inferior-lisp-program "/usr/local/bin/sbcl --dynamic-space-size 8192")

(defun lisp-indents ()
  (put 'with-charms 'lisp-indent-function 0)
  (put 'with-screen-dims 'lisp-indent-function 2)
  (put 'defcolors 'lisp-indent-function 0)
  (put '->> 'common-lisp-indent-function 0)
  (put '-> 'common-lisp-indent-function 0)
  (put 'lazy 'common-lisp-indent-function 1)
  (put 'lazy-for 'common-lisp-indent-function 1)
  (put 'define-aspect 'common-lisp-indent-function '1))

(add-hook 'lisp-mode-hook
          (lambda ()
            (lisp-indents)
            (paredit-mode 1)
	    (aggressive-indent-mode 1)
            (define-key lisp-mode-map (kbd "C-o Z")
              (lambda ()
                (interactive)
                (slime-connect "localhost" 5555)
                (message "Don't forget: (loop (sleep 1))")))
            (load "/Users/jacobsen/quicklisp/clhs-use-local.el" t)
            (define-key lisp-mode-map (kbd "C-o j") 'slime)
            (define-key lisp-mode-map (kbd "C-c q") 'slime-quit-lisp)
            (define-key lisp-mode-map (kbd "s-i") ' slime-eval-last-expression)
            (define-key lisp-mode-map (kbd "s-I")
              (lambda ()
                (interactive)
                (paredit-forward)
                (slime-eval-last-expression)))
            (define-key lisp-mode-map (kbd "C-o Y")
              'slime-pprint-eval-last-expression)
            (define-key lisp-mode-map (kbd "C-o y")
              (lambda ()
                (interactive)
                (insert "\n;;=>\n'")
                (setq current-prefix-arg t) ; C-u
                (slime-eval-last-expression)))))

;;; Journaling:
(defun open-journal-file ()
  (let* ((today (format-time-string "%Y-%m-%d"))
         (path (concat (getenv "HOME") "/org/journal/" today ".org"))
         (hdr-list (list (concat "#+TITLE: [" today "]")
                         "#+OPTIONS: toc:nil num:nil author:nil date:nil"
                         "#+STARTUP: align"
                         "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\" />"
                         "#+LaTeX_CLASS: article"
                         "#+LaTeX_CLASS_OPTIONS: [9pt,twocolumn,portrait]"
                         "#+LATEX_HEADER: \\usepackage[margin=0.5in]{geometry}"
                         "#+LATEX_HEADER: \\usepackage{enumitem}"))
         (hdr (apply 'concat
                     (mapcar (lambda (s) (concat s "\n"))
                             hdr-list)))
         (has-hdr (lambda ()
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward "#+TITLE" nil t)))))
    (message (concat "opening " path " ..."))
    (find-file path)
    (unless (funcall has-hdr)
      (save-excursion
        (goto-char (point-min))
        (insert hdr)))
    (visual-line-mode)
    (message "Enjoy your journaling!")))

(global-set-key "\C-o1"
                (lambda ()
                  (interactive)
                  (open-journal-file)))

;; ;; FIXME: This is ugly, make it beautiful.
;; (defun all-the-things-old ()
;;   (interactive)
;;   (delete-other-windows)
;;   (split-window-right)
;;   (split-window-right)
;;   (split-window-right)
;;   (split-window-below)
;;   (windmove-right)
;;   (split-window-below)
;;   (windmove-right)
;;   (split-window-below)
;;   (windmove-right)
;;   (split-window-below)
;;   (balance-windows)
;;   (windmove-left)
;;   (windmove-left)
;;   (switch-to-buffer (nth 1 (buffer-list)))
;;   (windmove-right)
;;   (switch-to-buffer (nth 2 (buffer-list)))
;;   (windmove-right)
;;   (switch-to-buffer (nth 3 (buffer-list)))
;;   (windmove-down)
;;   (switch-to-buffer (nth 4 (buffer-list)))
;;   (windmove-left)
;;   (switch-to-buffer (nth 5 (buffer-list)))
;;   (windmove-left)
;;   (switch-to-buffer (nth 6 (buffer-list)))
;;   (windmove-left)
;;   (switch-to-buffer (nth 7 (buffer-list)))
;;   (windmove-up))

(defun all-the-things ()
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (switch-to-buffer (nth 1 (buffer-list)))
  (split-window-below)
  (switch-to-buffer (nth 2 (buffer-list)))
  (windmove-left)
  (switch-to-buffer (nth 3 (buffer-list))))

(global-set-key "\C-oT"
                (lambda ()
                  (interactive)
                  (all-the-things)
                  (message "Woot.")))

;; Random Sort
;; (https://stackoverflow.com/questions/6172054/\
;; how-can-i-random-sort-lines-in-a-buffer
(defun random-sort-lines (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

;; Run command on multiple files:
;; https://superuser.com/questions/176627/in-emacs-dired-how-can-i-run-a-command-on-multiple-marked-files/176629
(defun mrc-dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

;; Run Emacs as a client for `emacsclient`
;;(server-start)

;; I'm blind. So sue me.
(set-face-attribute 'default nil :height 200)


;; Play around with transparency a little bit
(set-frame-parameter (selected-frame) 'alpha '(95 . 80))

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

;; Babashka
(add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))

;; ;; Org-Roam stuff
;; (setq org-roam-directory "~/org-roam")
;; (add-hook
;;  'org-roam-mode-hook
;;  (lambda ()
;;    (define-key org-roam-mode-map (kbd "C-o r n") 'org-roam-find-file)
;;    (define-key org-roam-mode-map (kbd "C-o r l") 'org-roam-insert)
;;    (define-key org-roam-mode-map (kbd "C-o r g") 'org-roam-graph)
;;    (define-key org-roam-mode-map (kbd "C-o r t") 'org-roam-buffer-toggle-display)))
;; (add-hook 'after-init-hook 'org-roam-mode)

;; Bash completion
(require 'bash-completion)
(bash-completion-setup)

;; Insert time
;; (https://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs)
(defvar current-time-format "%H:%M:%S "
  "Format of date to insert with `insert-current-time' func.
  Note the weekly scope of the command's precision.")
(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

(global-set-key "\C-o5" 'insert-current-time)



(provide 'init)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

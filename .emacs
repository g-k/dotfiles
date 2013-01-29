(add-to-list 'load-path "~/.emacs.d")

(require 'package)
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/"))

;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Use unique buffer names based on file directory
;; http://stackoverflow.com/questions/2903426/display-path-of-file-in-status-bar
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Use yasnippet everywhere
(require 'yasnippet)
(yas/global-mode 1)

;; Use delete to delete char to left and not current cursor char
(when (equal system-type 'darwin)
  (normal-erase-is-backspace-mode 0))

(require 'cython-mode)

;; Sample custom hook
; https://groups.google.com/forum/?fromgroups#!topic/gnu.emacs.help/80FwoaEzqwI%5B1-25%5D
;; (add-hook 'coffee-mode-hook
;; 	  (lambda nil
;; 	    (add-hook 'after-save-hook
;; 		      (lambda nil (shell-command "npm run-script build-browser && osascript ~/Dropbox/bin/reload-chrome.applescript 'Binary Heap'"))
;; 		      nil 'local))) ; Only in current buffer

;; Automatically recompile elisp files
(add-hook 'emacs-lisp-mode-hook
	  (lambda nil
	    (add-hook 'after-save-hook
		      (lambda nil (save-excursion (byte-compile-file buffer-file-name)))
		      nil 'local))) ; Only in current buffer

(setq inhibit-splash-screen t)  ;; Hide the startup screen
(menu-bar-mode -1)  ;; Hide the menubar
(show-paren-mode 1)  ;; highlight matching parens
(eldoc-mode 1) ;; Show elisp help docs
;; ;; Look into paredit-mode for writing elisp
;; (define-key emacs-lisp-mode-map
;;   (kbd "M-.") 'find-function-at-point)

;; Highlight the current line (so I can find it on a big monitor w/ many buffers)
(global-hl-line-mode t)
;;
(column-number-mode 1)

;; OSX copy paste
;; from: http://www.lingotrek.com/2010/12/integrate-emacs-with-mac-os-x-clipboard.html
(defun mac-copy ()
  (shell-command-to-string "pbpaste"))

(defun mac-paste (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (equal system-type 'darwin)
  (setq interprogram-cut-function 'mac-paste)
  (setq interprogram-paste-function 'mac-copy))


;; Use org-mode for .org files
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; TODO Lists to search
(setq org-agenda-files (list "~/org/Work.org"
                             "~/org/Home.org"))

;; Use puppet for *.pp files
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))


;; Use mustache mode for handlebars templates
(add-to-list 'load-path "~/.emacs.d/mustache-mode.el")
(require 'mustache-mode)
(add-to-list 'auto-mode-alist '("\\.handlebars$" . mustache-mode))

;; Use html for *.gui files
(add-to-list 'auto-mode-alist '("\\.gui$" . html-mode))

;; Markdown mode for .md and .markdown files
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; Use ruby-mode for Vagrantfiles
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))


;; Use octave-mode for .m files (for ml class)
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; Use diff-mode for git commit messages
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

;; Git installed with emacs on Ubuntu
(when (equal system-type 'darwin)
  (require 'git)
  (require 'git-blame))

;; Reset buffers if the files change on disk
(global-auto-revert-mode t)


;; DISPLAY
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Highlight marked region
(setq transient-mark-mode t)

;; Swap buffers easily
(require 'buffer-move)
;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<f1>")  'buf-move-left)
(global-set-key (kbd "<f2>")  'buf-move-right)


;; BACKUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))

;; create the autosave and backup dirs if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

;; Remember my windows, buffers, etc.
;; (desktop-save-mode 1)

;; WHITESPACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use spaces not tabs
(setq tab-width 4)
(setq indent-tabs-mode nil)

;; See tabs and spaces using whitespace-mode
(require 'whitespace)

;; Delete whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Delete extra newlines at end of file
(defun delete-trailing-blank-lines ()
      "Deletes all blank lines at the end of the file."
      (interactive)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-max))
          (delete-blank-lines))))
(add-hook 'before-save-hook 'delete-trailing-blank-lines)


;; AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/Users/gregg/.emacs.d/ac-dict")
(ac-config-default)
;; make selected option in autocomplete menu high contrast
;; (set-face-background 'ac-selection-face "darkinvisbleyellow") ;; black/gray


;; C
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BSD KNF style for C or something like it
(setq c-basic-offset 4)
(c-add-style "openbsd"
             '((c-basic-offset . 8)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist . ((statement-block-intro . +)
                                   (knr-argdecl-intro . +)
                                   (substatement-open . 0)
                                   (label . 0)
                                   (statement-cont . +)))))
(put 'downcase-region 'disabled nil)

;; Coffeescript
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.cson$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
;; compile on save
;; (add-hook 'coffee-mode-hook '(lambda () (coffee-cos-mode t)))
(require 'auto-complete)
(add-to-list 'ac-modes 'coffee-mode)

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (define-key coffee-mode-map (kbd "C-c c") 'coffee-compile-buffer)
  )

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))


;; Javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun javascript-custom ()
  "javascript-mode-hook"
 (set (make-local-variable 'tab-width) 2))

(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)

;; M-x customize-group RET js2-mode RET
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))


;; Erlang
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ubuntu
;; (setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.6.4/emacs"
;; 		       load-path))
;; (setq erlang-root-dir "/usr/local/lib/erlang")
;; (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
;; (require 'erlang-start)


;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use Python.org's python-mode.el for .py files
(require 'python-mode)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.py.erb$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; Python highlighting (might not be necessary)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Run nosetests from emacs
(require 'nose)
;; Use dot output, rather than verbose output:
(setq nose-use-verbose nil) ;; default is t
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'nosetests-all)
            (local-set-key "\C-cm" 'nosetests-module)
            (local-set-key "\C-c." 'nosetests-one)
            (local-set-key "\C-cpa" 'nosetests-pdb-all)
            (local-set-key "\C-cpm" 'nosetests-pdb-module)
            (local-set-key "\C-cp." 'nosetests-pdb-one)))
(put 'scroll-left 'disabled nil)


;; Smalltalk
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append  '(("\\.st\\'" . smalltalk-mode))
	       auto-mode-alist))

(autoload 'gst-mode "/usr/local/Cellar/gnu-smalltalk/HEAD/share/emacs/site-lisp/gst-mode.elc" "" t)
(autoload 'smalltalk-mode "/usr/local/Cellar/gnu-smalltalk/HEAD/share/emacs/site-lisp/smalltalk-mode.elc" "" t)

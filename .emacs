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
(make-directory "~/.emacs.d/snippets/" t)
(require 'yasnippet)
(yas/global-mode 1)

;; Use ido-mode everywhere (find files and dirs)
(ido-mode 1)

;; Use delete to delete char to left and not current cursor char
(when (equal system-type 'darwin)
  (normal-erase-is-backspace-mode 0))

;; One character yes or no prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; Mark 80 column
(require 'fill-column-indicator)
(defun fci-80 ()
  "set fci column to 80"
  (setq fci-rule-column 80)
  (fci-mode 1))
(add-hook 'after-change-major-mode-hook 'fci-80)


;; Effective Emacs Item 2: Invoke M-x without the Alt key
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; Effective Emacs Item 3: Prefer backward-kill-word over Backspace
;; Note: used to killing regions so kill region if active
(defun kill-region-or-backward-word ()
  "kill-region if the region is active. Otherwise backward-kill-word."
  (interactive)
  (call-interactively
   (if (use-region-p)
       'kill-region
       'backward-kill-word)))

(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)


;; Sample custom hooks
;; Note: name hooks so I can remove it them later
;; (defun reload-tab ()
;;   (defvar tab-name "JS1k")
;;   (message "reloading tab: %s" tab-name)
;;   (shell-command
;;    (concat "osascript " "~/Dropbox/bin/reload-chrome.applescript " "'" tab-name "'")))

;; (defun reload-tab-on-save ()
;;   (message "saving tab to reload")
;;   (add-hook 'after-save-hook 'reload-tab nil 'local))

;; (defun my-compile ()
;;   "automatically compile buffer file"
;;   (save-excursion (byte-compile-file buffer-file-name)))

;; (defun recompile-elisp-on-save ()
;;   "Recompile elisp in current buffer on save"
;;   (add-hook 'after-save-hook 'my-compile nil 'local))
;; (add-hook 'emacs-lisp-mode-hook 'recompile-elisp-on-save)

;; (setenv "PAGER" "/bin/cat")  ;; shell

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
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

;; TODO Lists to search
(setq org-agenda-files (list "~/org/Work.org"
                             "~/org/Home.org"))

;; Use puppet for *.pp files
(autoload 'puppet-mode "puppet-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))


;; Use mustache mode for handlebars templates
(autoload 'mustache-mode "mustache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.handlebars$" . mustache-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . mustache-mode))

;; Markdown mode for .md and .markdown files
(autoload 'markdown-mode "markdown-mode" nil t)
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


;; full screen magit-status
;; http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)


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

;; Save point position between sessions
;; http://whattheemacsd.com/init.el-03.html
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Remember my windows, buffers, etc.
;; (desktop-save-mode 1)

;; WHITESPACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use spaces not tabs
(setq tab-width 4)
(setq indent-tabs-mode nil)

;; See tabs and spaces using whitespace-mode
(autoload 'whitespace-mode "whitespace-mode" nil t)

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

(autoload 'coffee-mode "coffee-mode" nil t)
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

(add-hook 'coffee-mode-hook 'coffee-custom)

;; Javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-allow-keywords-as-property-names nil)

;; (autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; TODO: Figure out why after-change-major-mode-hook isn't applying
(add-hook 'js2-mode-hook 'fci-80)

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

(autoload 'haskell-mode "haskell-mode" nil t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use Python.org's python-mode.el for .py files
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
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
            (local-set-key (kbd "C-c a") 'nosetests-all)
            (local-set-key (kbd "C-c m") 'nosetests-module)
            (local-set-key (kbd "C-c .") 'nosetests-one)
            (local-set-key (kbd "C-c p a") 'nosetests-pdb-all)
            (local-set-key (kbd "C-c p m") 'nosetests-pdb-module)
            (local-set-key (kbd "C-c p .") 'nosetests-pdb-one)))
(put 'scroll-left 'disabled nil)


;; Smalltalk
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append  '(("\\.st\\'" . smalltalk-mode))
	       auto-mode-alist))

(autoload 'gst-mode "/usr/local/Cellar/gnu-smalltalk/HEAD/share/emacs/site-lisp/gst-mode.elc" "" t)
(autoload 'smalltalk-mode "/usr/local/Cellar/gnu-smalltalk/HEAD/share/emacs/site-lisp/smalltalk-mode.elc" "" t)

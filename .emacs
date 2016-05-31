(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'package)
(setq package-enable-at-startup nil)

;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Check if we're on Emacs 24.4 or newer, if so, use the pinned package feature
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'((ac-cider . "melpa-stable")
	  (ace-jump-mode . "melpa-stable")
	  (auto-complete . "melpa-stable")
	  (browse-kill-ring . "melpa-stable")
	  (buffer-move . "melpa-stable")
	  (cargo . "melpa-stable")
	  (cider . "melpa-stable")
	  (clojure-mode . "melpa-stable")
	  (clojurescript-mode . "melpa-stable")
	  (coffee-mode . "melpa-stable")
	  (dash . "melpa-stable")
	  (discover . "melpa-stable")
	  (editorconfig . "melpa-stable")
	  (elfeed . "melpa-stable")
	  (elfeed-org . "melpa")
	  (flx . "melpa-stable")
	  (flx-ido . "melpa-stable")
	  (flycheck . "melpa")
	  (flycheck-rust . "melpa")
	  (flycheck-pyflakes . "melpa")
	  ;; (glsl-mode . "melpa-stable")
	  (guide-key . "melpa-stable")
	  ;; (handlebars-mode . "melpa-stable")
	  (helm . "melpa-stable")
	  ;; (helm-projectile . "melpa-stable")
	  (ido . "melpa-stable")
	  (js2-mode . "melpa-stable")
	  (json-mode . "melpa-stable")
	  (less-css-mode . "melpa-stable")
	  (magit . "melpa-stable")
	  (markdown-mode . "melpa-stable")
	  ;; (minimap . "melpa-stable") ;; replace with sublimitity?
	  (multiple-cursors . "melpa-stable")
	  (nose . "melpa-stable")
	  (paredit . "melpa-stable")
	  ;; (paredit-menu . "melpa-stable")
	  (php-mode . "melpa-stable")
	  (popup . "melpa-stable")
	  (projectile . "melpa-stable")
	  (puppet-mode . "melpa-stable")
	  (racer . "melpa-stable")
	  (rust-mode . "melpa")
	  (rainbow-delimiters . "melpa-stable")
	  ;; (undo-tree . "melpa-stable")
	  (web-mode . "melpa-stable")
	  (yasnippet . "melpa-stable")
	  ;; (yasnippet-bundle . "melpa-stable")
	  )))

(package-initialize t)

(defun install-required-packages (package-list)
  (when (>= emacs-major-version 24)
    (package-refresh-contents)
    (mapc (lambda (package)
	    (or (package-installed-p package)
		(package-install package)))
	  package-list)))


(setq required-package-list '(ac-cider
			      ace-jump-mode
			      auto-complete
			      browse-kill-ring
			      buffer-move
			      cider
			      clojure-mode
			      coffee-mode
			      dash
			      discover
			      editorconfig
			      flx
			      flx-ido
			      glsl-mode
			      guide-key
			      ;; handlebars-mode
			      helm
			      ido
			      js2-mode
			      json-mode
			      less-css-mode
			      magit
			      markdown-mode
			      multiple-cursors
			      nose
			      paredit
			      ;; paredit-menu
			      php-mode
			      popup
			      projectile
			      puppet-mode
			      rainbow-delimiters
			      ;; undo-tree
			      web-mode
			      yasnippet
			      ;; yasnippet-bundle
			      ))

;; (install-required-packages required-package-list)


;; http://www.idryman.org/blog/2013/03/23/installing-swank-dot-js/
;; (add-to-list 'load-path "~/.emacs.d/slime")
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
;; (require 'slime)
;; (slime-setup)

;; IRC
(setq rcirc-default-nick "g-k")
(setq rcirc-server-alist
      '(("irc.mozilla.org" :port 6697 :encryption tls
	 :channels ("#servo"))
	("irc.freenode.net" :port 6697 :encryption tls
	 :channels ())))

;; search
(add-hook 'after-init-hook #'projectile-global-mode) ;; to enable in all buffers
(setq projectile-enable-caching t)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Don't keep undo history for buffer list
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2013-04/msg00497.html
(add-hook 'Buffer-menu-mode-hook 'buffer-disable-undo)

;; Use unique buffer names based on file directory
;; http://stackoverflow.com/questions/2903426/display-path-of-file-in-status-bar
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)


(make-directory "~/.emacs.d/snippets/" t)
(setq default-snippets-path "~/.emacs.d/snippets/")
(add-hook 'after-init-hook
	  (lambda ()
	    (package-initialize)
	    (require 'yasnippet)
	    (yas/global-mode 1) ;; Use yasnippet everywhere
	    (ido-mode 1) ;; Use ido-mode everywhere (find files and dirs)
	    (flx-ido-mode 1)
	    (setq ido-use-faces nil) ;; disable ido faces to see flx highlights
	    ))

;; Use delete to delete char to left and not current cursor char
(when (equal system-type 'darwin)
  (normal-erase-is-backspace-mode 0))

;; One character yes or no prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; remote editing
(require 'tramp)
(setq password-cache-expiry nil)

;; http://whattheemacsd.com/mac.el-01.html
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)
;; (setq ns-function-modifier 'hyper)

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

(add-hook 'after-init-hook
	  (lambda ()
	    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))))

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
(define-key emacs-lisp-mode-map
  (kbd "M-.") 'find-function-at-point)

;; Highlight the current line (so I can find it on a big monitor w/ many buffers)
;; (global-hl-line-mode t)
;;
(column-number-mode 1)

;; OSX copy paste
;; from: http://www.lingotrek.com/2010/12/integrate-emacs-with-mac-os-x-clipboard.html
;; (defun mac-copy ()
;;   (shell-command-to-string "pbpaste"))

;; (defun mac-paste (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; (when (equal system-type 'darwin)
;;   (setq interprogram-cut-function 'mac-paste)
;;   (setq interprogram-paste-function 'mac-copy))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Clipboard.html
(setq x-select-enable-clipboard t)


;; Use org-mode for .org files
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

;; TODO Lists to search
(setq org-agenda-files (list "~/org/Work.org"
                             "~/org/Home.org"))

(add-hook
 'after-init-hook
 (lambda ()
   ;; elisp and other lisps
   (require 'eldoc)
   (require 'paredit) ; required paredit-menu loads paredit too
   (require 'rainbow-delimiters)
   (eldoc-add-command
    'paredit-backward-delete
    'paredit-close-round)
   (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

   (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
   (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
   (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
   (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
   (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
   (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
   (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

   ;; clojure
   (require 'cider)

   (add-hook 'clojure-mode-hook 'paredit-mode)
   ;; (add-hook 'clojure-mode-hook 'eldoc-mode)
   (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

   ;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
   (add-hook 'cider-repl-mode-hook 'paredit-mode)

   (setq nrepl-buffer-name-show-port t)
   (setq cider-repl-history-file "~/.emacs.d/cider-repl-history")
   (setq cider-repl-history-size 1000) ; the default is 500
   (setq ac-delay 0.0)
   (setq ac-quick-help-delay 0.5)

   (require 'ac-cider)
   (add-hook 'cider-mode-hook 'ac-cider-setup)
   (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
   (add-to-list 'ac-modes 'cider-mode)
   (add-to-list 'ac-modes 'cider-repl-mode)))

;; Use puppet for *.pp files
(autoload 'puppet-mode "puppet-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))


;; Use handlebars templates
;; (autoload 'handlebars-mode "handlebars-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.handlebars$" . handlebars-mode))
;; (add-to-list 'auto-mode-alist '("\\.hbs$" . handlebars-mode))

;; Markdown mode for .md and .markdown files
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; Use ruby-mode for Gem and Vagrantfiles
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile.lock$" . ruby-mode))


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

;; Also auto refresh dired, but be quiet about it
;; http://whattheemacsd.com/sane-defaults.el-01.html
;; (setq global-auto-revert-non-file-buffers t) ;; flashing cursor to command area
;; (setq auto-revert-verbose nil)

;; Don't pass --dired to ls and fail to parse weird filenames
(setq dired-use-ls-dired nil)


;; DISPLAY
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Highlight marked region
(setq transient-mark-mode t)

;; Swap buffers easily
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'buffer-move)
	    ;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
	    ;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
	    (global-set-key (kbd "<f1>")  'buf-move-left)
	    (global-set-key (kbd "<f2>")  'buf-move-right)
	    ))

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
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)

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

(add-hook
 'after-init-hook
 (lambda ()
   (package-initialize)

   (require 'magit)

   (require 'editorconfig)
   (editorconfig-mode 1)

   ;; (require 'undo-tree)
   ;; (global-undo-tree-mode)

   ;; Sublime style minimap
   ;; (require 'minimap)

   ;; AutoComplete.el
   (require 'auto-complete-config)
   ;; (add-to-list 'ac-dictionary-directories "/Users/gregg/.emacs.d/ac-dict")
   (ac-config-default)
   ;; make selected option in autocomplete menu high contrast
   ;; (set-face-background 'ac-selection-face "darkinvisbleyellow") ;; black/gray

   ;; C
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

   ;; Shaders
   (autoload 'glsl-mode "glsl-mode" nil t)
   (add-to-list 'auto-mode-alist '("\\.glsl$" . glsl-mode))
   (add-to-list 'auto-mode-alist '("\\.vert$" . glsl-mode))
   (add-to-list 'auto-mode-alist '("\\.frag$" . glsl-mode))
   (add-to-list 'auto-mode-alist '("\\.geom$" . glsl-mode))


   ;; Coffeescript
   (autoload 'coffee-mode "coffee-mode" nil t)
   (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
   (add-to-list 'auto-mode-alist '("\\.cson$" . coffee-mode))
   (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
   ;; compile on save
   (add-hook 'coffee-mode-hook '(lambda () (coffee-cos-mode t)))
   (require 'auto-complete)
   (add-to-list 'ac-modes 'coffee-mode)

   (defun coffee-custom ()
     "coffee-mode-hook"
     (set (make-local-variable 'tab-width) 2)
     (define-key coffee-mode-map (kbd "C-c c") 'coffee-compile-buffer))

   (add-hook 'coffee-mode-hook 'coffee-custom)

   ;; Javascript
   (setq js2-basic-offset 2)
   (setq js2-bounce-indent-p t)
   (setq js2-cleanup-whitespace t)
   (setq js2-allow-keywords-as-property-names nil)

   (autoload 'js2-mode "js2-mode" nil t)
   (add-to-list 'auto-mode-alist '("\\.js" . js2-mode))

   ;; JSON
   (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

   ;; tern.js
   (defun my-tern-hook ()
     "enable tern.js"
     (tern-mode t))
   ;; (add-hook 'js2-mode-hook 'my-tern-hook)

   ;; CSS
   (add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
   (autoload 'css-mode "css-mode" nil t)

   (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
   (autoload 'css-mode "css-mode" nil t)


   ;; Erlang


   ;; Ubuntu
   ;; (setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.6.4/emacs"
   ;; 		       load-path))
   ;; (setq erlang-root-dir "/usr/local/lib/erlang")
   ;; (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
   ;; (require 'erlang-start)


   ;; Haskell
   (autoload 'haskell-mode "haskell-mode" nil t)
   (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
   (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


   ;; Python

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


   ;; Rust
   (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
   (setq racer-rust-src-path (expand-file-name "~/rust/src"))

   ;; (add-hook 'rust-mode-hook #'rustfmt-enable-on-save)
   (add-hook 'rust-mode-hook #'racer-mode)
   (add-hook 'rust-mode-hook 'cargo-minor-mode)
   (add-hook 'racer-mode-hook #'eldoc-mode)

   (defun racer-mode-hook-ac () (ac-racer-setup))
   (add-hook 'racer-mode-hook 'racer-mode-hook-ac)

   (defun indent-buffer ()
     "Indent current buffer according to major mode."
     (interactive)
       (indent-region (point-min) (point-max)))

   (defun rustfmt-buffer () (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))
   (add-hook 'rust-mode-hook 'rustfmt-buffer)

   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

   ;; Smalltalk
   (setq auto-mode-alist
	 (append  '(("\\.st\\'" . smalltalk-mode))
		  auto-mode-alist))

   (autoload 'gst-mode "/usr/local/Cellar/gnu-smalltalk/HEAD/share/emacs/site-lisp/gst-mode.elc" "" t)
   (autoload 'smalltalk-mode "/usr/local/Cellar/gnu-smalltalk/HEAD/share/emacs/site-lisp/smalltalk-mode.elc" "" t)

   ;; PHP
   (autoload 'php-mode "php-mode" nil t)
   (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
   (add-to-list 'auto-mode-alist '("\\.thtml$" . php-mode))
   (add-to-list 'auto-mode-alist '("\\.tpl$" . php-mode))

   ;; use tabs
   (add-hook 'php-mode-hook 'php-tabs)
   (defun php-tabs ()
     "Use tabs in php-mode"
     (setq indent-tabs-mode t)
     (let ((my-tab-width 4))
       (setq tab-width my-tab-width)
       (setq c-basic-indent my-tab-width)
       (set (make-local-variable 'tab-stop-list)
	    (number-sequence my-tab-width 200 my-tab-width))))))

;; Note: use list-faces-display

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/paint-by-words/TODO.org" "~/org/Work.org" "~/org/Home.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "red"))))
 '(diff-changed ((t (:foreground "blue"))))
 '(git-blame-prefix-face ((t (:background "white" :foreground "brightwhite"))))
 '(highlight ((t (:background "brightcyan"))))
 '(secondary-selection ((t nil)))
 '(whitespace-indentation ((t (:foreground "firebrick"))))
 '(whitespace-line ((t (:foreground "white"))))
 '(whitespace-space ((t (:foreground "lightgray"))))
 '(whitespace-tab ((t (:foreground "lightgray")))))
(put 'upcase-region 'disabled nil)

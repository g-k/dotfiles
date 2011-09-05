(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d")

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))

;;
(column-number-mode 1)

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

;; Use ruby-mode for Vagrantfiles
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

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

;; Mark column 80
;; (let ((whitespace-line-column 80)  ; 80 is the default
;;       (whitespace-style '(lines))) ; or '(lines) for the whole line
;;   (whitespace-mode 1))

;; BACKUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep ~ and # backup and recovery files in one place
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

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
(add-to-list 'load-path "/Users/gregg/.emacs.d")
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

(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; Erlang
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ubuntu
(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.6.3/emacs"
		       load-path))
(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.6.4/emacs"
		       load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)


;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/haskell-mode-2.8.0/haskell-site-file")
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

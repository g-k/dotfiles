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



;; (desktop-save-mode 1) ;; remember by windows, buffers, etc.


(c-add-style "openbsd"
             '((c-basic-offset . 8)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist . ((statement-block-intro . +)
                                   (knr-argdecl-intro . +)
                                   (substatement-open . 0)
                                   (label . 0)
                                   (statement-cont . +)))))
(put 'downcase-region 'disabled nil)

(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; delete whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; org-mode for .org files
;; (require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(add-to-list 'load-path "~/.emacs.d")

;; Swap buffers around easily
(require 'buffer-move)
;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<f1>")  'buf-move-left)
(global-set-key (kbd "<f2>")  'buf-move-right)

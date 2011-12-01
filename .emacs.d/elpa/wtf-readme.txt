;;; Commentary:

;; wtf.el provides the ability to look up the definitions of popular
;; conversational and computing acronyms.

;; * Use:
;;
;; To use this, move to an unknown acronym in a buffer and type
;; the following:
;;
;;   M-x wtf-is RET
;;
;; The `wtf-is' function may also be called noninteractively, and it
;; will return a string (or nil) rather than displaying a message.
;;
;; To add a custom acronym definition, either customize
;; `wtf-custom-alist' or do:
;;
;;   M-x wtf-add RET <acronym> RET <definition> RET
;;
;; To remove a custom acronym definition, or mark a pre-defined
;; acronym as "removed" in the case that no custom acronym definition
;; exists in `wtf-custom-alist' for that acronym, do:
;;
;;   M-x wtf-remove RET <acronym> RET
;;
;; To mark a pre-defined acronym as "removed", without checking first
;; to see whether it is in `wtf-custom-alist', customize the
;; `wtf-removed-acronyms' option.
;;
;; If you add a custom acronym definition, and feel it to be worth
;; sharing, you are encouraged to contact <mwolson@gnu.org> via email,
;; providing the acronym and its definition.  This increases the
;; chance that it will appear in future versions of wtf.el.

;; * Legalese:
;;
;; Many of the acronym definitions were downloaded from
;; http://cvsweb.netbsd.org/bsdweb.cgi/src/share/misc/.  No copyright
;; notice was included, but the intent of the original author was to
;; put these acronym definitions in the public domain.  This was
;; deduced from several emails sent to the authors of these files.
;; Additionally, the original data files use a specific syntax which
;; does not allow for a copyright notice.
;;
;; The original program that uses these files in NetBSD
;; (http://cvsweb.netbsd.org/bsdweb.cgi/src/games/wtf/wtf) is in the
;; public domain.

;; * Acknowledgments:
;;
;; Thanks to Trent Buck for `emacs-wiki-wtf.el', which inspired the
;; creation of `wtf.el'.


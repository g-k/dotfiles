;;; Commentary:
;;
;; A bunch of modes (e.g. Eldoc) exist that display information about the cursor
;; position in the echo area.  Unfortunately, these modes will often fight for
;; the echo area and there is no way to specify a priority.  echo-pick aims to
;; provide a way to prioritize these messages, or even to display multiple
;; messages at once.
;;
;; To specify the priorities edit `echo-pick-function-list', then enable
;; `echo-pick-mode' and don't forget to disable the other modes' echo messages.
;; If they can't be turned off, maybe you can set a very long timeout.
;;
;; `echo-pick-mode' respects a lot of Eldoc's settings, including
;; `eldoc-message-commands'.
;;

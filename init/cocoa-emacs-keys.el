;; modifier key
(setq ns-option-modifier 'meta)
(setq ns-command-modifier 'super)

;;; behavior of "Command + Cursor" to the default of MacOS X
;;; default : ns-next-frame in ns-win.el
(define-key global-map [s-left] 'move-beginning-of-line)
;;; default : ns-prev-frame in ns-win.el
(define-key global-map [s-right] 'move-end-of-line)
(define-key global-map [s-up] 'backward-page)
(define-key global-map [s-down] 'forward-page)

;;; font resize short cut (Command +/-/0)
(global-set-key [(s ?+)] (lambda () (interactive) (text-scale-increase 1)))
(global-set-key [(s ?-)] (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key [(s ?0)] (lambda () (interactive) (text-scale-increase 0)))

;;; revert [Home] Key and [End] Key
(define-key global-map [home] 'beginning-of-buffer)
(define-key global-map [end] 'end-of-buffer)

;;; Delete the following character by fn + delete 
(define-key global-map [kp-delete] 'delete-char)

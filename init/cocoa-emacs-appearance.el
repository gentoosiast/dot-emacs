;; window size
(when window-system
  (setq default-frame-alist
        (append '((width . 100) (height . 53))
                default-frame-alist))
  ;; transparency
  (add-to-list 'default-frame-alist
               '(alpha . (100 80))) ;; (alpha . (<active> <non active>))
  ;; font setting
  (let* ((size (cond ((< (display-pixel-height) 900) 12)
                     ((t                             14))))
         (asciifont "Menlo")
         (jpfont "Hiragino Maru Gothic ProN")
         (h (* size 10))
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; half-width KaTaKaNa
    (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; Accented Latin
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec) ; Greek
    )
  (setq face-font-rescale-alist
        '(("^-apple-hiragino.*" . 1.2)
          (".*osaka-bold.*" . 1.2)
          (".*osaka-medium.*" . 1.2)
          (".*courier-bold-.*-mac-roman" . 1.0)
          (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
          (".*monaco-bold-.*-mac-roman" . 0.9)
          ("-cdac$" . 1.3))))

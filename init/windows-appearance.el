;; window size
(setq initial-frame-alist
      (append '((width . 80) (height . 40)
                (top   .  0) (left   .  0))
              initial-frame-alist))
(setq default-frame-alist
      (append '((width . 80) (height . 40)
                (top   .  0) (left   .  0))
              default-frame-alist))
;; font setting (Consolas + Migu 1M with rescale)
(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 100)
(set-fontset-font "fontset-default"
                  'japanese-jisx0213-1
                  '("Migu 1M" . "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'japanese-jisx0213-2
                  '("Migu 1M" . "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'japanese-jisx0213-a
                  '("Migu 1M" . "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Migu 1M" . "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("Migu 1M" . "unicode-bmp"))
(add-to-list 'face-font-rescale-alist
             `(,(encode-coding-string ".*Migu 1M.*" 'emacs-mule) . 1.1))

;; window size
(setq default-frame-alist
      (append '((width . 100) (height . 40))
              default-frame-alist))
;; font
(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 140)
(set-fontset-font "fontset-default"
                  'japanese-jisx0213-1
                  '("Windows TV 太丸ゴシック" . "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'japanese-jisx0213-2
                  '("Windows TV 太丸ゴシック" . "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'japanese-jisx0213-a
                  '("Windows TV 太丸ゴシック" . "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Windows TV 太丸ゴシック" . "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("Windows TV 太丸ゴシック" . "unicode-bmp"))
(add-to-list 'face-font-rescale-alist
             `(,(encode-coding-string ".*Windows TV.*" 'emacs-mule) . 1.1))

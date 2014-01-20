;; cygwin-bin
(setq cygwin-mount-cygwin-bin-directory
      (concat (getenv "CYGWIN_DIR") "\\bin"))
(require 'setup-cygwin)
(file-name-shadow-mode -1)
;; print
(setq ps-print-color-p t
      ps-lpr-command "gswin32c.exe"
      ps-multibyte-buffer 'non-latin-printer
      ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
      printer-name nil
      ps-printer-name nil
      ps-printer-name-option nil
      ps-print-header nil          ; ヘッダの非表示
      )


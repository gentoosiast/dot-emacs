(require 'shell)
(setq explicit-shell-file-name "bash.exe")
(setq shell-command-switch "-c")
;; strip ^M in shell-mode
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
;; completion (for drive letter)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")
;; autoload ansi-color
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)

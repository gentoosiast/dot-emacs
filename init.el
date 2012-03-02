;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs.d/init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; language configurations
(set-language-environment 'Japanese)
(prefer-coding-system  'utf-8-unix)
;; show the corresponding paren 
(show-paren-mode t)
;; no start up message
(setq inhibit-startup-screen t)
;; hide tool-bar and menu-bar
(tool-bar-mode 0)
(menu-bar-mode 0)
;; show line and column number
(line-number-mode t)
(column-number-mode t)
;;; The number of lines to scroll a window by when point moves out.
(setq scroll-step 1)
;; follow version controlled symbolic link
(setq vc-follow-symlinks t)
;; auto revert changes in version controlled file
(setq auto-revert-check-vc-info t)
;; set background color
(setq frame-background-mode 'dark)
;; input special and control characters by "Option"
(setq ns-option-modifier 'none)

; indent settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(add-hook 'makefile-mode-hook
          (function (lambda () (setq indent-tabs-mode t))))
(define-key global-map (kbd "RET") 'newline-and-indent)
;; indent style for C
(add-hook 'c-mode-common-hook '(lambda()
    (setq c-default-style "k&r"
          c-basic-offset tab-width)))

;; linum: display line numbers to the left of buffers
(require 'linum)
(global-linum-mode t)
;; dynamic linum-format
(setq linum-format 
      (lambda (line)
        (propertize
         (format
          (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
            (concat " %" (number-to-string w) "d ")) line) 'face 'linum)))

;; save-place
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/emacs-places.txt")

;; flyspell
(require 'flyspell)
(setq ispell-program-name "aspell")
;; skip Japanese characters in ispell
(eval-after-load "ispell" '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; flymake
(require 'flymake)
;; flymake without Makefile
(defun flymake-simple-generic-init (cmd &optional opts)
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
        (list cmd (append opts (list local-file)))))
;; flymake with/without Makefile
(defun flymake-simple-make-or-generic-init (cmd &optional opts)
  (if (file-exists-p "Makefile")
      (flymake-simple-make-init)
    (flymake-simple-generic-init cmd opts)))
;; flymake for C/C++
(defun flymake-c-init ()
  (flymake-simple-make-or-generic-init
   "gcc" '("-W" "-Wall" "-Wextra" "-std=c99" "-fsyntax-only")))
(defun flymake-cc-init ()
  (flymake-simple-make-or-generic-init
   "g++" '("-W" "-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))
(defun flymake-header-init ()
  (flymake-simple-generic-init
   "gcc" '("-W" "-Wall" "-Wextra" "-std=c99" "-x" "c-header")))
(defun flymake-cc-header-init ()
  (flymake-simple-generic-init
   "g++" '("-W" "-Wall" "-Wextra" "-pedantic" "-x" "c++-header")))
(push '("\\.c$" flymake-c-init) flymake-allowed-file-name-masks)
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.h$" flymake-header-init) flymake-allowed-file-name-masks)
(push '("\\.hpp$" flymake-cc-header-init) flymake-allowed-file-name-masks)

(dolist (hook (list 'c-mode-hook 'c++-mode-hook))
  (add-hook hook '(lambda () (flymake-mode t))))

;; autoinsert
(require 'autoinsert)
(setq auto-insert-query nil)
(setq auto-insert-directory "~/.emacs.d/templates")
(setq auto-insert-alist
      (append '(("[Mm]akefile$" . "Makefile")
                ("\\.c$" . "template.c")
                ("\\.cpp$" . "template.cpp")
                ("\\.f$" . "template.f")
                ("\\.sh$" . "template.sh")
                ("\\.plt$" . "template.plt")
                ("\\.py$" . "template.py")
                ("\\.rb$" . "template.rb")
                ("\\.pl$" . "template.pl")
                ("\\.html$" . "template.rst")
                ("\\.tex$" . "template.tex")
                ("\\.rst$" . "template.rst"))
              auto-insert-alist))
(add-hook 'find-file-hooks 'auto-insert)

;; el-get: allows you to install and manage elisp code for Emacs
;; https://github.com/dimitri/el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch) (end-of-buffer) (eval-print-last-sexp))))
;; local sources
(setq my-packages
      (append '(color-theme
                undo-tree
                yasnippet
                auto-complete
                ac-python
                rst-mode
                vimpulse)
              (mapcar 'el-get-source-name el-get-sources)))
(el-get 'sync my-packages)

;; color-theme: an emacs-lisp mode for skinning your emacs
;; http://www.nongnu.org/color-theme/
(require 'color-theme)
(color-theme-clarity)

;; undo-tree: treat undo history as a tree
;; http://www.dr-qubit.org/emacs.php#undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)

;; yasnippet: yet another snippet extension for Emacs.
;; http://capitaomorte.github.com/yasnippet/
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
;; autoinsert header file with include guard
(setq auto-insert-alist 
      (append '(("\\.h$" . '(yas/expand-snippet "once")))
              auto-insert-alist))

;; auto-complete: The most intelligent auto-completion extension for GNU Emacs
;; http://cx4a.org/software/auto-complete/index.html
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-auto-show-menu 0.1)
;; use C-p/n to select auto-complete menu
(setq ac-use-menu-map t)

;; show error message in popup menu
(require 'popup)
(defun flymake-popup-error ()
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count (length line-err-info-list)))
    (while (> count 0)
           (when line-err-info-list
             (let* ((file 
                      (flymake-ler-file (nth (1- count) line-err-info-list)))
                    (full-file
                      (flymake-ler-full-file (nth (1- count) line-err-info-list)))
                    (text
                      (flymake-ler-text (nth (1- count) line-err-info-list)))
                    (line
                      (flymake-ler-line (nth (1- count) line-err-info-list))))
               (popup-tip (format "[%s] %s" line text))))
           (setq count (1- count)))))
;; show pop-up menu on error line
(defadvice flymake-mode (before post-command-stuff activate compile)
           (set (make-local-variable 'post-command-hook)
                (add-hook 'post-command-hook 'flymake-popup-error)))

;; ac-python
(require 'ac-python)

;; rst-mode:
(require 'rst)
(setq auto-mode-alist (cons '("\\.rst$" . rst-mode) auto-mode-alist))

;; viper/vimpulse
;; vimpulse extends viper with vim features like visual mode and text objects 
;; http://gitorious.org/vimpulse/pages/Home
(setq viper-mode t)
(require 'vimpulse)
;; "set smartcase" equivalent
(setq viper-case-fold-search t)
;; "set wrapscan" equivalent
(setq viper-search-wrap-around t)
;; vim-like keymap
(define-key viper-vi-basic-map "w" 'forward-word)
(define-key viper-vi-basic-map "b" 'backward-word)
;; these scrolls keys are imcompatible, but acceptable for me
(setq scroll-preserve-screen-position t)
(define-key viper-vi-basic-map "\C-u" 'scroll-down)
(define-key viper-vi-basic-map "\C-d" 'scroll-up)
;; kill C-u
(define-key viper-insert-basic-map "\C-u" 'undefined)
;; use tab-width in emacs
(setq viper-shift-width tab-width)
;; switch modeline-face to show the current state vi or insert
(setq my-viper-default-face-background (face-background 'mode-line))
(defadvice viper-go-away (after my-viper-go-away-restore activate)
  (set-face-background 'mode-line my-viper-default-face-background))
(defun my-viper-set-mode-line-face ()
  (unless (minibufferp (current-buffer))
    (set-face-background 'mode-line
                         (cdr (assq viper-current-state
                                    '((vi-state     . "black")
                                      (insert-state . "red")))))))
(dolist (hook (list 'viper-vi-state-hook 'viper-insert-state-hook))
  (add-hook hook 'my-viper-set-mode-line-face))

;;; local settings
(if (file-exists-p (expand-file-name "~/.emacs.d/init-local.el"))
    (load (expand-file-name "~/.emacs.d/init-local.el") nil t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; mode: emacs-lisp
;; End:

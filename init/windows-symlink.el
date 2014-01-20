(custom-set-variables '(w32-symlinks-handle-shortcuts t))
(require 'w32-symlinks)

(defadvice insert-file-contents-literally
  (before insert-file-contents-literally-before activate)
  (set-buffer-multibyte nil))

(defadvice minibuffer-complete (before expand-symlinks activate)
  (let ((file (expand-file-name
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))))
    (when (file-symlink-p file)
      (delete-region (line-beginning-position) (line-end-position))
      (insert (w32-symlinks-parse-symlink file)))))

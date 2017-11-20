;;; Extentions for todotxt.el
;;
;; Functions:
;; - Switch to other todo.txt which exists now.
;; - Transpose task (Up/Down)

(require 'todotxt)

(defvar todotxt-location "~/todo")
(defvar todotxt-default-project "todo")

(defun todotxt-open (file)
  (let ((todotxt-file file))
    (todotxt)
    (setq-local todotxt-file file)))

(defun todotxt-get-todo-projects ()
  (cons todotxt-default-project
        (cl-loop for project in (directory-files todotxt-location nil "^[^.]")
                 when (not (equal project todotxt-default-project))
                 collect project)))

(defun todotxt-get-project-file (project)
  (format "%s%s/todo.txt"
          (file-name-as-directory todotxt-location)
          project))

;;;###autoload
(defun todotxt-switch ()
  (interactive)
  (let ((win (selected-window))
        (file (todotxt-get-project-file
               (completing-read "Todo.txt: " (todotxt-get-todo-projects)))))
    (if (equal 'todotxt-mode
               (with-current-buffer (window-buffer win)
                 major-mode))
        (set-window-buffer win (find-file-noselect file))
      (todotxt-open file))))

;;;###autoload
(defun todotxt-transpose-line-up ()
  (interactive)
  (setq inhibit-read-only 't)
  (if (and (not (= (point-min) (point-at-bol)))
           (equal
            (todotxt-sort-key-for-string (todotxt-get-current-line-as-string))
            (todotxt-sort-key-for-string (buffer-substring (point-at-bol 0) (point-at-eol 0)))))
      (progn
        (beginning-of-line)
        (save-excursion
          (transpose-lines 1))
        (forward-line -1)))
  (setq inhibit-read-only 'nil))

;;;###autoload
(defun todotxt-transpose-line-down ()
  (interactive)
  (setq inhibit-read-only 't)
  (if (and (not (= (point-max) (point-at-eol 2)))
           (equal
            (todotxt-sort-key-for-string (todotxt-get-current-line-as-string))
            (todotxt-sort-key-for-string (buffer-substring (point-at-bol 2) (point-at-eol 2)))))
      (progn
        (beginning-of-line)
        (save-excursion
          (forward-line)
          (transpose-lines 1))
        (forward-line)))
  (setq inhibit-read-only 'nil))

(defun todotxt-get-current-todo-project ()
  (file-name-base (directory-file-name (file-name-directory (buffer-file-name)))))

(defun todotxt-get-projects-from-string (str)
  (let ((projects nil)
        (start-index 0))
    (while (string-match todotxt-projects-regexp str start-index)
      (let ((project (substring (match-string-no-properties 0 str) 1)))
        (if (not (member project projects))
            (setq projects (cons project projects)))
        (setq start-index (match-end 0))))
    (reverse projects)))

;;;###autoload
(defun todotxt-move-item-to-other-project ()
  (interactive)
  (let* ((old-text (todotxt-get-current-line-as-string))
         (from-project (todotxt-get-current-todo-project))
         (to-project (completing-read "Todo.txt: " (append (todotxt-get-projects-from-string old-text) (todotxt-get-todo-projects))))
         (to-file (todotxt-get-project-file to-project))
         (new-text (replace-regexp-in-string (concat " +\\+" to-project) "" old-text))
         (new-text (concat new-text
                           (if (equal from-project todotxt-default-project)
                               ""
                             (concat " +" from-project)))))
    (if (not (file-exists-p to-file))
        (make-directory (file-name-directory to-file)))
    (with-current-buffer (find-file-noselect to-file)
      (setq inhibit-read-only 't)
      (goto-char (point-max))
      (insert (concat new-text "\n"))
      (todotxt-prioritize 'todotxt-get-due-priority-sort-key)
      (if todotxt-save-after-change (save-buffer))
      (setq inhibit-read-only nil)))
  (setq inhibit-read-only 't)
  (beginning-of-line)
  (kill-line 1)
  (if todotxt-save-after-change (save-buffer))
  (setq inhibit-read-only 'nil))

(provide 'todotxt-projects)

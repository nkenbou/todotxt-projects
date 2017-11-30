(require 'todotxt-projects)

(ert-deftest test-todotxt-get-todo-projects ()
  (let ((default-project todotxt-default-project)
        (ad (lambda (directory &optional full match nosort) '("foo" "bar" "baz"))))
    (setq todotxt-default-project "bar")
    (advice-add 'directory-files :around ad)
    (unwind-protect
        (should (equal '("bar" "foo" "baz") (todotxt-get-todo-projects)))
      (advice-remove 'directory-files ad)
      (setq todotxt-default-project default-project))))

(ert-deftest test-todotxt-get-project-file ()
  (let ((location todotxt-location))
    (setq todotxt-location "/todo")
    (unwind-protect
        (should (equal "/todo/foo/todo.txt" (todotxt-get-project-file "foo")))
      (setq todotxt-location location))))

(ert-deftest test-todotxt-transpose-lines-up-at-top ()
  (with-temp-buffer
    (insert "abc
def
ghi
")
    (goto-char (point-min))
    (todotxt-transpose-lines-up)
    (should (equal "abc
def
ghi
"
                   (buffer-string)))
    (should (equal nil (char-before)))
    (should (equal ?a (char-after)))))

(ert-deftest test-todotxt-transpose-lines-up-without-priority ()
  (with-temp-buffer
    (insert "abc
def
ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-up)
    (should (equal "def
abc
ghi
"
                   (buffer-string)))
    (should (equal nil (char-before)))
    (should (equal ?d (char-after)))))

(ert-deftest test-todotxt-transpose-lines-up-with-higher-priority ()
  (with-temp-buffer
    (insert "(A) abc
def
ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-up)
    (should (equal "(A) abc
def
ghi
"
                   (buffer-string)))
    (should (equal ?d (char-before)))
    (should (equal ?e (char-after)))))

(ert-deftest test-todotxt-transpose-lines-up-with-same-priority ()
  (with-temp-buffer
    (insert "(A) abc
(A) def
ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-up)
    (should (equal "(A) def
(A) abc
ghi
"
                   (buffer-string)))
    (should (equal nil (char-before)))
    (should (equal ?\( (char-after)))))

(ert-deftest test-todotxt-transpose-lines-down-at-bottom ()
  (with-temp-buffer
    (insert "abc
def
ghi
")
    (goto-char (point-max))
    (todotxt-transpose-lines-down)
    (should (equal "abc
def
ghi
"
                   (buffer-string)))
    (should (equal ?\C-j (char-before)))
    (should (equal nil (char-after)))))

(ert-deftest test-todotxt-transpose-lines-down-without-priority ()
  (with-temp-buffer
    (insert "abc
def
ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-down)
    (should (equal "abc
ghi
def
"
                   (buffer-string)))
    (should (equal ?\C-j (char-before)))
    (should (equal ?d (char-after)))))

(ert-deftest test-todotxt-transpose-lines-down-with-higher-priority ()
  (with-temp-buffer
    (insert "abc
def
(A) ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-down)
    (should (equal "abc
def
(A) ghi
"
                   (buffer-string)))
    (should (equal ?d (char-before)))
    (should (equal ?e (char-after)))))

(ert-deftest test-todotxt-transpose-lines-down-with-same-priority ()
  (with-temp-buffer
    (insert "abc
(A) def
(A) ghi
")
    (goto-char (point-min))
    (forward-line)
    (forward-char)
    (todotxt-transpose-lines-down)
    (should (equal "abc
(A) ghi
(A) def
"
                   (buffer-string)))
    (should (equal ?\C-j (char-before)))
    (should (equal ?\( (char-after)))))

(ert-deftest test-todotxt-get-current-todo-project ()
  (let ((ad (lambda (&optional buffer) "/foo/bar/todo.txt")))
    (advice-add 'buffer-file-name :around ad)
    (unwind-protect
        (should (equal "bar" (todotxt-get-current-todo-project)))
      (advice-remove 'buffer-file-name ad))))

(ert-deftest test-todotxt-get-projects-from-string ()
  (should (equal '("foo" "bar") (todotxt-get-projects-from-string "Task +foo @bar +bar"))))

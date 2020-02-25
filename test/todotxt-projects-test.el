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

(ert-deftest test-todotxt-get-current-todo-project ()
  (let ((ad (lambda (&optional buffer) "/foo/bar/todo.txt")))
    (advice-add 'buffer-file-name :around ad)
    (unwind-protect
        (should (equal "bar" (todotxt-get-current-todo-project)))
      (advice-remove 'buffer-file-name ad))))

(ert-deftest test-todotxt-get-projects-from-string ()
  (should (equal '("foo" "bar") (todotxt-get-projects-from-string "Task +foo @bar +bar"))))

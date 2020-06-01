(in-package #:rogurt)

(defmacro continuable (&body body)
  "Allow you to continue in case you have and error in your code"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun update-slynk ()
  "updates the code from the repl or when recompiling when using slynk"
  (continuable
   (let ((connection slynk::*emacs-connection*))
     (when connection
       (slynk::handle-requests connection t)))))

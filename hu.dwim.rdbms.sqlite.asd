;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.rdbms.sqlite
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :hu.dwim.rdbms
  :description "Sqlite backend for hu.dwim.rdbms."
  :depends-on (:cffi
               :hu.dwim.rdbms)
  :components ((:module "source"
                :components ((:module "sqlite"
                              :components ((:file "package")
                                           (:file "cffi" :depends-on ("package"))
                                           (:file "database" :depends-on ("cffi"))
                                           (:file "syntax" :depends-on ("database"))
                                           (:file "ddl" :depends-on ("database"))
                                           (:file "type" :depends-on ("database"))))))))

(defmethod perform :after ((op hu.dwim.asdf:develop-op) (system (eql (find-system :hu.dwim.rdbms.sqlite))))
  (let ((database-variable (read-from-string "hu.dwim.rdbms::*database*")))
    (unless (boundp database-variable)
      (setf (symbol-value database-variable)
            (symbol-value (read-from-string "hu.dwim.rdbms.test::*sqlite-database*"))))))

(defsystem :hu.dwim.rdbms.sqlite/test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :package-name :hu.dwim.rdbms.test
  :test-name "TEST/SQLITE"
  :depends-on (:hu.dwim.rdbms.sqlite
               :hu.dwim.rdbms/test)
  :components ((:module "test"
                :components ((:file "sqlite")))))

(defmethod hu.dwim.asdf::call-in-system-environment ((operation load-op) (system (eql (find-system :hu.dwim.rdbms.sqlite/test))) function)
  (progv
      (list (read-from-string "hu.dwim.rdbms:*database*"))
      (list (eval (read-from-string "(make-instance 'hu.dwim.rdbms.sqlite:sqlite)")))
    (call-next-method)))

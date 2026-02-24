#!/usr/bin/env bash
#| -*- mode: lisp; coding: utf-8-unix -*-

#set -x

# explore this for further ideas: https://gitlab.com/ambrevar/lisp-repl-core-dumper/

SCRIPT_DIR=`dirname "$0"`
SCRIPT_DIR=`readlink -f ${SCRIPT_DIR}`

DWIM_WORKSPACE=${SCRIPT_DIR}/../../
DWIM_WORKSPACE=`readlink -f ${DWIM_WORKSPACE}`

if [ -z "$LISP" ] ; then
  echo
  echo "Running with whatever SBCL is in the PATH"
  echo
  LISP=sbcl
else
  echo Using LISP ${LISP}
fi

echo "*** "`date`" Run starts"

# For LIBRARY_PATH see:
# https://gcc.gnu.org/onlinedocs/gcc/Environment-Variables.html#Environment-Variables
# `guix shell --development foo` sets this variable to the profile's lib/ directory
# that contains the .so files.
# Note that timing matters: "If, at *the time that the program was started*, the
# environment variable LD_LIBRARY_PATH was defined to contain..."
# More details in: https://github.com/cffi/cffi/pull/194
if command -v guix &> /dev/null; then
  echo "Guix detected, entering the environment."
# TODO these env vars should probably be copied over into the lisp image, and be reinstated in an image restore hook
  eval $(guix shell --search-paths libffi openssl sqlite graphviz libfixposix pkg-config clang-toolchain --development sbcl)
  export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+${LD_LIBRARY_PATH}:}${LIBRARY_PATH}"
  echo "Setting LD_LIBRARY_PATH based on LIBRARY_PATH to ${LD_LIBRARY_PATH}"
fi

export DWIM_SYSTEM_NAME_SUBSTRING=$1
DWIM_MAXIMUM_MEMORY_SIZE=4096

BUILD_LOG_FILE="/tmp/hu.dwim.rdbms.log"

# we should leave this up to the user...
#export CL_SOURCE_REGISTRY="(:source-registry (:also-exclude \"sbcl\" \"disabled\" \"global\") (:tree \"${DWIM_WORKSPACE}\") :inherit-configuration)"
#export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (\"${DWIM_WORKSPACE}\" (\"${DWIM_INSTALL_PATH}/.cache/common-lisp/\" :implementation)) :ignore-inherited-configuration)"

# "call" the lisp part below.
# NOTE: (require :asdf) does not initiate asdf self-upgrade; that's why we also asdf:load-system it again.
# NOTE: using --script would also imply --no-userinit (i.e. quicklisp wouldn't get loaded from .sbclrc), so we use a different trick here to skip the first line of this shell script file when reading it as a lisp file.
# FIXME: the call to (ASDF:INITIALIZE-OUTPUT-TRANSLATIONS) shouldn't be needed. see: https://gitlab.common-lisp.net/asdf/asdf/-/issues/163#note_16017
exec ${LISP} --dynamic-space-size "${DWIM_MAXIMUM_MEMORY_SIZE}" --noinform --end-runtime-options \
  --disable-debugger \
  --no-userinit \
  --eval "(require :asdf)" --eval "(asdf:load-system :asdf)" \
  --eval "(asdf:initialize-output-translations)" \
  --eval "(progn #-quicklisp(load \"quicklisp.lisp\"))" \
  --eval "(progn #-quicklisp(quicklisp-quickstart:install))" \
  --eval "(load (uiop:subpathname (user-homedir-pathname) \"quicklisp/setup.lisp\"))" \
  --eval "(with-open-file (s \"${0}\" :element-type 'character) (read-line s) (load s))" \
  --end-toplevel-options

# optionally, after loading ASDF:
# --eval "(declaim (optimize debug))" \

echo "*** "`date`" Run finished"

# let's quit the shell part before the shell interpreter runs on the lisp stuff below
kill -INT $$

# and from here follows the lisp part that gets "called" above |#

(in-package :cl-user)

;; so that we can see (def function name ...) in the compile output
#+sbcl
(setf sb-ext:*compiler-print-variable-alist*
      '((*print-length* . 3)
        (*print-level* . 2)
        (*print-pretty* . nil)))

(format t "~2&Running on ~A ~A, using ASDF ~A, Quicklisp dist version ~A~%"
        (lisp-implementation-type)
        (lisp-implementation-version)
        (asdf:asdf-version)
        (or #+quicklisp (ql:dist-version "quicklisp")
            "n/a"))

;; add current directory to ASDF
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(truename "."))        ; absolute path of current directory
   :inherit-configuration))

(format t "(asdf:user-source-registry): ~S~%(asdf:system-source-registry): ~S~%asdf::*source-registry*: ~S~%"
        (asdf:system-source-registry)
        (asdf:user-source-registry)
        asdf::*source-registry*)

(setf quicklisp:*quickload-verbose* t)

(defun load-systems* (systems)
  (locally (declare (sb-ext:muffle-conditions style-warning
                                              sb-ext:compiler-note))
    (handler-bind (((or style-warning
                        sb-ext:compiler-note)
                     #'muffle-warning))
      (let ((systems (if (listp systems)
                         systems
                       (list systems))))
        #-quicklisp
        (apply 'asdf:load-systems systems)
        #+quicklisp
        (ql:quickload systems :prompt nil :verbose t)))))

;; this tells ASDF to signal a full error if there are any warnings at compile and/or load.
;;(asdf:enable-deferred-warnings-check)

(dolist (system-name '(:hu.dwim.rdbms.postgresql
                       ;; :hu.dwim.rdbms.sqlite
                       ))
  (format t "Loading from: ~S~%" (asdf:component-pathname (asdf:find-system system-name)))

  (load-systems* system-name)

  (asdf:test-system system-name))

(format t "~2&run-tests.sh has reached its end, exiting normally~%")

(sb-ext:exit :code 0)

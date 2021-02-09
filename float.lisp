;;; float.lisp taken from Fernando Borretti's cl-yaml
;;; licensed under MIT/X11 license
;;; Modified by Jason Miller 2015
;;; Changes also usable under terms of MIT/X11 license

(in-package :nyaml)

(defparameter *float-strategy* :keyword)

(define-condition unsupported-float-value (yaml-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unsupported IEEE float value.")))
    (:documentation "This condition is signalled when the parser receives an IEEE
 floating point special value it cannot parse. This is only signalled when the
 floating point strategy is :error."))

#+sbcl
(defparameter *sbcl-nan-value*
  (sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero)
    (- sb-ext:double-float-positive-infinity
       sb-ext:double-float-positive-infinity)))

(defun not-a-number ()
  (case *float-strategy*
    (:error
     (error 'unsupported-float-value))
    (:keyword
     :NaN)
    (:best-effort
     #+sbcl *sbcl-nan-value*
     #+allegro #.excl:*nan-double*
     #-(or sbcl allegro) :NaN)))

(defun positive-infinity ()
  (case *float-strategy*
    (:error
     (error 'unsupported-float-value))
    (:keyword
     :+Inf)
    (:best-effort
     #+sbcl sb-ext:double-float-positive-infinity
     #+allegro #.excl:*infinity-double*
     #-(or sbcl allegro) :+Inf)))

(defun negative-infinity ()
  (case *float-strategy*
    (:error
     (error 'unsupported-float-value))
    (:keyword
     :-Inf)
    (:best-effort
     #+sbcl sb-ext:double-float-negative-infinity
     #+allegro #.excl:*negative-infinity-double*
     #-(or sbcl allegro) :-Inf)))

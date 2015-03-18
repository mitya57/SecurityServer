;;; Configure logging facility

(in-package :secsrv)

(defun setup-logging ()
  (cl-log:defcategory :critical)
  (cl-log:defcategory :error   (or :error :critical))
  (cl-log:defcategory :warning (or :warning :error))
  (cl-log:defcategory :notice  (or :notice :warning))
  (cl-log:defcategory :info    (or :info :notice))
  (cl-log:defcategory :trace   (or :debug :info))
  (cl-log:defcategory :debug   (or :debug :info))

  (setf (cl-log:log-manager)
        (make-instance 'cl-log:log-manager :message-class 'cl-log:formatted-message))

  (cl-log:start-messenger 'cl-log:text-stream-messenger
                          :stream *standard-output*
                          :category '(or :debug :info :trace)))


(eval-when (:load-toplevel)
  (setup-logging)
  (cl-log:log-message :info "Logging started"))

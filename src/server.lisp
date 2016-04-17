;;;

(in-package :secsrv)

(defvar *clack-handle* nil "Instance of the running clack Web-server.")

(defvar *registered-handlers*
  (cl-containers:make-container 'cl-containers:simple-associative-container :test #'equal)
  "A mapping from URIs to handling applications.")


(defun stop-clack ()
  (prog1
      (when *clack-handle*
        (clack:stop *clack-handle*))
    (setf *clack-handle* nil)))

(defun add-handler (path handler)
  (setf (cl-containers:item-at *registered-handlers* path)
        handler))

(defun find-handler (path)
  (flet ((all-positions (char string)
           (loop :for k :from 0 :and item :across string
              :when (char= item char) :collect k)))
    (loop :for k :in (cons nil (reverse (all-positions #\/ path)))
       :for app = (cl-containers:item-at *registered-handlers*
                                         (subseq path 0 (when k (1+ k))))
       :when app
       :do (return app))))


(defun load-static-file (env)
  (let ((path (concatenate 'string "./html" (getf env :path-info))))
    (when (open path :direction :probe)
      (list hunchentoot:+http-ok+
            '(:content-type "text/plain")
            (pathname path)))))


(defun start-server (connection-maker &key (port 8135))
  (log-message :info "Starting HTTP server on port ~D." port)
  (stop-clack)
  (add-handler "/check/" #'(lambda (env) (process-access-request connection-maker env)))
  (add-handler "/policy/" #'browse-policy)
  (add-handler "/showconcept/" #'show-concept-info)
  (setf *clack-handle*
        (clack:clackup
         #'(lambda (env)
             (let ((app (find-handler (getf env :path-info)))
                   (static-file (load-static-file env)))
               (handler-case
                   (cond
                     (app (funcall app env))
                     (static-file static-file)
                     (t (list hunchentoot:+http-not-found+
                              '(:content-type "text/html")
                              (list (format nil "No handler for ~A was found.<br/>~%~
                                           Choices are:~%<ul>~{<li><a href=\"~A\">~:*~A</a></li>~%~}</ul>~%"
                                            (getf env :path-info)
                                            (cl-containers:collect-keys *registered-handlers*))
                                    ""))))
               (error (condition)
                 (list hunchentoot:+http-internal-server-error+
                       '(:content-type "text/plain")
                       (list (format nil "~A" condition)
                             ""))))))
         :port port
         :server :hunchentoot)))


(defun process-access-request (connection-maker env)
  "Processes HTTP request for access control check. The request is encoded in the requested URL."
  (destructuring-bind (kw-check user-name operation entity-name object-id . tail)
      (cdr (cl-utilities:split-sequence #\/ (getf env :path-info)))
    (declare (ignore kw-check tail))
    (with-checker (the-checker connection-maker :policy *current-policy*)
      (multiple-value-bind (granted-p elapsed-time sql-queries-count)
          (has-access the-checker user-name entity-name (parse-integer object-id) operation)
        (list hunchentoot:+http-ok+
              '(:content-type "text/plain")
              (list
               (if granted-p "ALLOW" "DENY")
               (format nil "~%~F seconds; ~A SQL requests" elapsed-time sql-queries-count)
               ""))))))


(defun browse-policy (env)
  (declare (ignore env))
  `(,hunchentoot:+http-ok+
     (:content-type "text/html")
     ("Concepts are:"
      ,(format nil
               "~%<ul>~{<li><a href=\"/showconcept/~A/\">~:*~A</a></li>~%~}</ul>~%"
               (let (names)
                 (cl-containers:iterate-nodes (policy-concepts *current-policy*)
                                              #'(lambda (concept)
                                                  (push (concept-name concept) names)))
                 (sort names #'string<)))
      "")))


(defun show-concept-info (env)
  (destructuring-bind (uri-prefix concept-name . tail)
      (cdr (cl-utilities:split-sequence #\/ (getf env :path-info)))
    (declare (ignore uri-prefix tail))
    (let* ((concept (find-concept concept-name :policy *current-policy*))
           (direct-rules (remove concept (policy-rules *current-policy*)
                                 :key #'rule-concept :test-not #'eql))
           (super-concepts (ho-walk concept nil :direction :super-concepts))
           (indirect-rules (remove-if-not
                            #'(lambda (c) (member c super-concepts :test #'eql))
                            (policy-rules *current-policy*)
                            :key #'rule-concept)))
      `(,hunchentoot:+http-ok+
        (:content-type "text/html")
        (,(format nil "<h3>Concept <b>~A</b></h3>" (concept-name concept))
          "Rules associated directly to the concept:"
          ,(format nil "~%<ul>~{<li>~A</li>~%~}</ul>~%"
                   (mapcar #'rule-name direct-rules))
          ;; inherited rules
          "Rules defined for more abstract concepts:"
          ,(format nil "~%<ul>~{<li>~A</li>~%~}</ul>~%"
                   (mapcar #'rule-name indirect-rules))
          )))))

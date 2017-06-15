;;;

(defpackage :secsrv.server
  (:nicknames :server)
  (:use :cl :secsrv.sys :secsrv.policy)
  (:import-from :cl-log
                #:log-message)
  (:export #:start-server))


(in-package :secsrv.server)

(defvar *clack-handle* nil "Instance of the running clack Web-server.")

(defvar *registered-handlers*
  (cl-containers:make-container 'cl-containers:simple-associative-container :test #'equal)
  "A mapping from URIs to handling applications.")

(defvar *udp-server-exiting* nil
  "Set to non-NIL if the udp server is exitting.")
(defvar *udp-server-thread* nil
  "Reference to bordeaux thread object corresponding to the running UDP server.")


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


(defun send-udp (host port msg)
  "Sends an UDP message to the specified host/port."
  (let ((socket (usocket:socket-connect host port
                                        :protocol :datagram
                                        :element-type '(unsigned-byte 8)))
        (msg-usb8 (make-array 2 :element-type '(unsigned-byte 8))))
    (unwind-protect
         (usocket:socket-send socket msg-usb8 (length msg-usb8))
      (usocket:socket-close socket))))


(defun %start-udp-server (handler port &key (timeout 1) (max-buffer-size 4096))
  "Start UDP server."
  (labels ((main-loop ()
             (setf *udp-server-exiting* nil
                   *udp-server-thread* (bt:current-thread))
             (let ((socket (usocket:socket-connect nil 0
                                                   :protocol :datagram
                                                   :element-type '(unsigned-byte 8)
                                                   :timeout timeout
                                                   :local-host "127.0.0.1"
                                                   :local-port port)))
               (unwind-protect
                    (do ((buffer (make-array max-buffer-size
                                             :element-type '(unsigned-byte 8))))
                        (*server-exiting*)
                      (multiple-value-bind (recv n remote-host remote-port)
                          (usocket:socket-receive socket buffer max-buffer-size)
                        (declare (ignore recv))
                        (funcall handler (subseq buffer 0 n) remote-host remote-post)))
                 (usocket:socket-close socket)))))
    (when (null *udp-server-thread*)
      (bt:make-thread #'main-loop :name (format nil "UDP-SERVER ~D" port)))))


(defun %stop-udp-server (port)
  "Stop UDP server."
  (when *udp-server-thread*
    ;; tell the server to exit
    (setf *udp-server-exiting* t)
    ;; send a dummy message to clean things up
    (send-udp "localhost" port "abc")
    ;; wait for the thread to exit
    (bt:join-thread *udp-server-thread*)
    ;; done
    (setf *udp-server-thread* nil)
    nil))


(defun example-handler (buffer remote-host remote-port)
  (log-message :trace "HOST: ~A PORT: ~A MESSAGE: ~A~%"
               ;;*remote-host*  ;(usocket:vector-quad-to-dotted-quad *remote-host*)
               (usocket:hbo-to-dotted-quad remote-host)
               remote-port
               buffer)
  nil)

(defun start-udp-server (packet-handler)
  (%start-udp-server 'example-handler 5115))

(defun stop-udp-server ()
  (%stop-udp-server 5115))



(defun process-access-request (connection-maker env)
  "Processes HTTP request for access control check. The request is encoded in the requested URL."
  (destructuring-bind (kw-check user-name operation entity-name object-id . tail)
      (cdr (cl-utilities:split-sequence #\/ (getf env :path-info)))
    (declare (ignore kw-check tail))
    (checker:with-checker (the-checker connection-maker :policy *current-policy*)
      (multiple-value-bind (granted-p elapsed-time sql-queries-count)
          (checker:has-access the-checker user-name entity-name (parse-integer object-id) operation)
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
        (,(format nil "<h3>Concept: <u>~A</u></h3>" (concept-name concept))
          "Rules associated directly to the concept:"
          ,(format nil "~%<ul>~{<li>~A</li>~%~}</ul>~%"
                   (mapcar #'rule-name direct-rules))
          ;; inherited rules
          "Rules inherited from more abstract concepts:"
          ,(format nil "~%<ul>~{<li>~A</li>~%~}</ul>~%"
                   (mapcar #'rule-name indirect-rules))
          )))))

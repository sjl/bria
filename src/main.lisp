(in-package :bria)
(named-readtables:in-readtable :cl-interpol)

;;;; Utils --------------------------------------------------------------------
(defun read-form-from-file (path)
  (with-open-file (f path :direction :input)
    (read f)))

(defun pget (item plist &key (test #'equal) default)
  (iterate (for (k v) :on plist :by #'cddr)
           (finding v :such-that (funcall test k item) :on-failure default)))

(defun http (url &rest parameters)
  (flet ((parse-body (octets)
           (-<> octets
             flexi-streams:octets-to-string
             (let ((yason:*parse-object-as* :plist)
                   (yason:*parse-json-arrays-as-vectors* t)
                   (yason:*parse-json-booleans-as-symbols* t))
               (yason:parse <>))))
         (successp (code)
           (<= 200 code 299)))
    (multiple-value-bind (body code headers uri stream must-close reason)
        (drakma:http-request url :parameters (plist-alist parameters))
      (declare (ignore headers uri stream must-close))
      (if (successp code)
        (parse-body body)
        (error "Request error ~D (~A)" code reason)))))

(defun minutes (n)
  (* 60 n))


;;;; Cachetable ---------------------------------------------------------------
(defclass* cache ()
  ((data :initform (make-hash-table :test 'equal))
   ttl))

(defun make-cache (ttl-seconds)
  (make-instance 'cache :ttl ttl-seconds))

(defun cache-clear (cache)
  (clrhash (cache-data cache)))

(defun cache-set! (cache key value)
  (setf (gethash key (cache-data cache))
        (cons (+ (get-internal-real-time)
                 (* (cache-ttl cache) internal-time-units-per-second))
              value))
  value)

(defun cache-get% (cache key)
  (if-found (result (gethash key (cache-data cache)))
    (destructuring-bind (timeout . value) result
      (values timeout value))
    (values nil nil)))

(defun timeout-invalid-p (timeout)
  (or (null timeout)
      (> (get-internal-real-time) timeout)))

(defun cachedp (cache key)
  (not (timeout-invalid-p (cache-get% cache key))))

(defmacro cache-get (cache key &body body)
  "Get `key` from `cache` if present and valid, otherwise use and cache `body`."
  (once-only (cache key)
    (with-gensyms (timeout value)
      `(multiple-value-bind (,timeout ,value) (cache-get% ,cache ,key)
         (if (timeout-invalid-p ,timeout)
           (cache-set! ,cache ,key (progn ,@body))
           ,value)))))


;;;; Rate-Limiting ------------------------------------------------------------
(defmacro define-rate-limited-function ((name seconds) arglist &body body)
  (let ((cache (symb '* name '-rate-limiter*)))
    `(progn
       (defvar ,cache (make-cache ,seconds))
       (defun ,(symb name '-ready-p) ()
         (not (cachedp ,cache t)))
       (defun ,name ,arglist
         (if (cachedp ,cache t)
          (values)
          (cache-set! ,cache t (progn ,@body)))))))


;;;; Credentials --------------------------------------------------------------
(defvar *credentials* nil)
(defun reload-credentials ()
  (setf *credentials* (read-form-from-file "creds")))

(defun ensure-credentials ()
  (when (null *credentials*)
    (reload-credentials))
  *credentials*)

(defun cred (name)
  (getf (ensure-credentials) name))


;;;; IRC ----------------------------------------------------------------------
(defvar *running* nil)
(defvar *connection* nil)
(defvar *handler-thread* nil)

(defvar *e* nil)
(defvar *reply-to* nil)


(defclass bria-connection (birch:connection) ())

(defmethod birch:handle-event ((connection bria-connection)
                               (event birch:privmsg-event))
  (setf *e* event)
  (when (null (birch:channel event))
    (try-handling-line
      (birch:nick (birch:user event))
      (birch:message event))))

(defun handle (connection)
  (iterate (while *running*)
           (birch:process-message connection)
           (finally (birch:/quit connection))))

(defun start (&optional no-handler)
  (let ((connection (make-instance 'bria-connection
                      :server-host "chat.freenode.net"
                      :server-port 6667
                      :nick "bria"
                      :pass (cred :irc)
                      :real-name "Basic, Reliable IRC Assistant")))
    (birch:connect connection)
    (setf *running* t
          *connection* connection)
    (unless no-handler
      (setf *handler-thread* (bt:make-thread (curry #'handle connection))))))

(defun stop ()
  (setf *running* nil))


(defun reply (message)
  (birch:/privmsg *connection* *reply-to* message))


;;;; Programs -----------------------------------------------------------------
(defun reply-temperature ()
  (reply (format nil "It's about ~D° outside right now."
                 (current-temperature 14604))))

(defun reply-reload ()
  (if (reload-ready-p)
    (progn (reply "Hang on...")
           (reload)
           (reply "Done."))
    (reply "I was just reloaded, slow down.")))

(defun reply-huh? ()
  (reply "what?"))

(defun try-handling-line (nick line)
  (when (equal nick "sjl")
    (let ((*reply-to* nick))
      (cond
        ((equal line "temp") (reply-temperature))
        ((equal line "reload") (reply-reload))
        (t (reply-huh?))))))


;;;; Weather ------------------------------------------------------------------
(defparameter *weather-cache* (make-cache (minutes 10)))

(defun request-weather (zip-code)
  (pr "Requesting the weather.")
  (http "http://api.openweathermap.org/data/2.5/weather"
        "zip" #?"${zip-code},us"
        "units" "imperial"
        "APPID" (cred :weather)))

(defun get-weather (zip-code)
  (cache-get *weather-cache* zip-code
             (request-weather zip-code)))

(defun current-temperature (zip-code)
  (-<> (get-weather zip-code)
    (pget "main" <>)
    (pget "temp" <>)
    (round <> 1)))


;;;; Reload -------------------------------------------------------------------
(define-rate-limited-function (reload 30) ()
  (ql:quickload :bria)
  t)

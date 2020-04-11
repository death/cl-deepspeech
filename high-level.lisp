;;;; +----------------------------------------------------------------+
;;;; | cl-deepspeech                                                  |
;;;; +----------------------------------------------------------------+

(defpackage #:cl-deepspeech/high-level
  (:use #:cl #:cl-deepspeech/low-level)
  (:import-from
   #:cffi
   #:with-foreign-object
   #:mem-ref
   #:with-pointer-to-vector-data
   #:foreign-string-to-lisp
   #:inc-pointer)
  (:import-from
   #:uiop
   #:native-namestring)
  (:import-from
   #:trivial-garbage
   #:finalize)
  (:export
   #:model
   #:model-sample-rate
   #:make-model
   #:*model*
   #:speech-to-text))

(in-package #:cl-deepspeech/high-level)

(defclass model ()
  ((handle :initarg :handle :reader model-handle)
   (sample-rate :initarg :sample-rate :reader model-sample-rate)
   (streams :initarg :streams :reader model-streams)))

(defun make-model (filename &key (beam-width 500) lm-filename trie-filename (lm-alpha 1.75) (lm-beta 1.0))
  (with-foreign-object (handle-ptr :pointer)
    (let ((ret (ds-create-model (native-namestring filename)
                                beam-width
                                handle-ptr)))
      (unless (eq ret :ok)
        (error "Model creation failed with error code ~S." ret))
      (let* ((handle (mem-ref handle-ptr :pointer))
             (sample-rate (ignore-errors (ds-get-model-sample-rate handle)))
             (streams (make-hash-table)))
        (when (and lm-filename trie-filename lm-alpha lm-beta)
          (handler-case (ds-enable-decoder-with-lm handle
                                                   (native-namestring lm-filename)
                                                   (native-namestring trie-filename)
                                                   (coerce lm-alpha 'single-float)
                                                   (coerce lm-beta 'single-float))
            (error (e)
              (warn e))))
        (finalize (make-instance 'model :handle handle
                                        :sample-rate sample-rate
                                        :streams streams)
                  (lambda ()
                    (loop for stream-handle being each hash-value of streams
                          do (ds-free-stream stream-handle))
                    (clrhash streams)
                    (ds-free-model handle)))))))

(defvar *model* nil)

(defun speech-to-text (command &key (model *model*) stream (start 0) end)
  (check-type model model)
  (if stream
      (speech-to-text-stream command
                             model
                             stream
                             start
                             (or end (and (vectorp command) (length command))))
      (let ((buffer command))
        (speech-to-text-buffer buffer
                               model
                               start
                               (or end (length buffer))))))

(defun get-string (pointer)
  (unwind-protect
       (values (foreign-string-to-lisp pointer))
    (ds-free-string pointer)))

(defun speech-to-text-buffer (buffer model start end)
  (assert (<= 0 end (length buffer)))
  (assert (<= 0 start end))
  (get-string
   (etypecase buffer
     ((simple-array (unsigned-byte 8) (*))
      (with-pointer-to-vector-data (ptr buffer)
        (let ((ptr-start (inc-pointer ptr start)))
          (ds-speech-to-text (model-handle model)
                             ptr-start
                             (truncate (- end start) 2)))))
     ((or (simple-array (unsigned-byte 16) (*))
          (simple-array (signed-byte 16) (*)))
      (with-pointer-to-vector-data (ptr buffer)
        (let ((ptr-start (inc-pointer ptr (* start 2))))
          (ds-speech-to-text (model-handle model)
                             ptr-start
                             (- end start))))))))

(defun speech-to-text-stream (command model stream start end)
  (let ((handle (gethash stream (model-streams model))))
    (when (null handle)
      (when (or (eq command :finish) (eq command :flush))
        (return-from speech-to-text-stream ""))
      (with-foreign-object (stream-ptr :pointer)
        (let ((ret (ds-create-stream (model-handle model) stream-ptr)))
          (unless (eq ret :ok)
            (error "Stream creation failed with error code ~S." ret))
          (setf handle (mem-ref stream-ptr :pointer))
          (setf (gethash stream (model-streams model)) handle))))
    (etypecase command
      ((eql :finish)
       (unwind-protect (get-string (ds-finish-stream handle))
         (remhash stream (model-streams model))))
      ((eql :flush)
       (ds-free-stream handle)
       (remhash stream (model-streams model))
       "")
      ((eql :decode)
       (get-string (ds-intermediate-decode handle)))
      ((simple-array (unsigned-byte 8) (*))
       (with-pointer-to-vector-data (ptr command)
         (let ((ptr-start (inc-pointer ptr start)))
           (ds-feed-audio-content handle
                                  ptr-start
                                  (truncate (- end start) 2))))
       "")
      ((or (simple-array (unsigned-byte 16) (*))
           (simple-array (signed-byte 16) (*)))
       (with-pointer-to-vector-data (ptr command)
         (let ((ptr-start (inc-pointer ptr (* start 2))))
           (ds-feed-audio-content handle
                                  ptr-start
                                  (- end start))))
       ""))))

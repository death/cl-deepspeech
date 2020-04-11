;;;; +----------------------------------------------------------------+
;;;; | cl-deepspeech                                                  |
;;;; +----------------------------------------------------------------+

(defpackage #:cl-deepspeech/low-level
  (:use #:cl)
  (:import-from
   #:cffi
   #:define-foreign-library
   #:use-foreign-library
   #:defcenum
   #:defcfun)
  (:export
   #:ds-create-model
   #:ds-get-model-sample-rate
   #:ds-free-model
   #:ds-enable-decoder-with-lm
   #:ds-speech-to-text
   #:ds-create-stream
   #:ds-feed-audio-content
   #:ds-intermediate-decode
   #:ds-finish-stream
   #:ds-free-stream
   #:ds-free-string
   #:ds-print-versions))

(in-package #:cl-deepspeech/low-level)

;; Library

(define-foreign-library deepspeech
  (t (:default "libdeepspeech")))

(use-foreign-library deepspeech)

;; Types

(defcenum ds-code
  (:ok #x0000)
  (:no-model #x1000)
  (:invalid-alphabet #x2000)
  (:invalid-shape #x2001)
  (:invalid-lm #x2002)
  (:model-incompatible #x2003)
  (:fail-init-mmap #x3000)
  (:fail-init-sess #x3001)
  (:fail-interpreter #x3002)
  (:fail-run-sess #x3003)
  (:fail-create-stream #x3004)
  (:fail-read-protobuf #x3005)
  (:fail-create-sess #x3006)
  (:fail-create-model #x3007))

;; Functions

(defcfun ("DS_CreateModel" ds-create-model) ds-code
  (model-path :string)
  (beam-width :int)
  (ret-val :pointer))

(defcfun ("DS_GetModelSampleRate" ds-get-model-sample-rate) :int
  (model :pointer))

(defcfun ("DS_FreeModel" ds-free-model) :void
  (model :pointer))

(defcfun ("DS_EnableDecoderWithLM" ds-enable-decoder-with-lm) ds-code
  (model :pointer)
  (lm-path :string)
  (trie-path :string)
  (lm-alpha :float)
  (lm-beta :float))

(defcfun ("DS_SpeechToText" ds-speech-to-text) :pointer
  (model :pointer)
  (buffer :pointer)
  (buffer-size :uint))

;; Metadata* DS_SpeechToTextWithMetadata(ModelState* aCtx,
;;                                       const short* aBuffer,
;;                                       unsigned int aBufferSize);

(defcfun ("DS_CreateStream" ds-create-stream) ds-code
  (model :pointer)
  (ret-val :pointer))

(defcfun ("DS_FeedAudioContent" ds-feed-audio-content) :void
  (stream :pointer)
  (buffer :pointer)
  (buffer-size :uint))

(defcfun ("DS_IntermediateDecode" ds-intermediate-decode) :pointer
  (stream :pointer))

(defcfun ("DS_FinishStream" ds-finish-stream) :pointer
  (stream :pointer))

;; Metadata* DS_FinishStreamWithMetadata(StreamingState* aSctx);

(defcfun ("DS_FreeStream" ds-free-stream) :void
  (stream :pointer))

;; void DS_FreeMetadata(Metadata* m);

(defcfun ("DS_FreeString" ds-free-string) :void
  (str :pointer))

(defcfun ("DS_PrintVersions" ds-print-versions) :void)

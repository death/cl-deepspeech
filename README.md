# cl-deepspeech

Use [DeepSpeech](https://github.com/mozilla/DeepSpeech) from Common Lisp.

# Example

```lisp
CL-USER> (ql:quickload '("marray" "cl-deepspeech") :silent t)
("marray" "cl-deepspeech")
CL-USER> (setf cl-deepspeech:*model* (cl-deepspeech:make-model "deepspeech-0.6.1-models/output_graph.pbmm"
                                                               :lm-filename "deepspeech-0.6.1-models/lm.binary"
                                                               :trie-filename "deepspeech-0.6.1-models/trie"))
#<CL-DEEPSPEECH/HIGH-LEVEL:MODEL {10096215F3}>
CL-USER> (dolist (file (directory "/tmp/audio/*.wav"))
           (marray:with-file-mapping (array file)
             (format t "~A => ~A~%"
                     file
                     (cl-deepspeech:speech-to-text array :start 44)))) ;; 44 to skip straight to samples in those files
/tmp/audio/2830-3980-0043.wav => experience proves
/tmp/audio/4507-16021-0012.wav => why should one halt on the way
/tmp/audio/8455-210777-0068.wav => your power is sufficient i said
NIL
CL-USER> (progn
           (dolist (file (directory "/tmp/audio/*.wav"))
             (marray:with-file-mapping (array file)
               (do ((chunk-size 16384)
                    (i 44 (+ i chunk-size)))
                   ((>= i (length array)))
                 (let ((end (min (+ i chunk-size) (length array))))
                   (cl-deepspeech:speech-to-text array :stream 0 :start i :end end)
                   (let ((text (cl-deepspeech:speech-to-text :decode :stream 0)))
                     (unless (equal text "")
                       (format t "~A~%" text)))))))
           (cl-deepspeech:speech-to-text :finish :stream 0))
experience pro
experience proves
experience proves
experience proves
experience proves why should
experience proves why should one who
experience proves why should one halt on the
experience proves why should one halt on the wa
experience proves why should one halt on the way
experience proves why should one halt on the way you
experience proves why should one halt on the way your power
experience proves why should one halt on the way your power is suffice
experience proves why should one halt on the way your power is sufficient i said
experience proves why should one halt on the way your power is sufficient i said
"experience proves why should one halt on the way your power is sufficient i said"
```

# License

MIT

(in-package :sexpline)

(defconstant +record-separator+ (code-char #x1E)
  "ASCII Record Separator character used as escape character")

(defun encode (sexp)
  "Encode S-expression to single-line string format.
Newlines in the printed representation are replaced with \\x1E (Record Separator).
Existing \\x1E characters are escaped as \\x1E\\x1E."
  (let* ((*print-readably* t)
         (*print-pretty* nil)
         (*print-right-margin* nil)
         (printed (format nil "~S" sexp)))
    (with-output-to-string (out)
      (loop for char across printed
            do (cond
                 ((char= char #\Newline)
                  (write-char +record-separator+ out)
                  (write-char #\Space out))
                 ((char= char +record-separator+)
                  (write-char +record-separator+ out)
                  (write-char +record-separator+ out))
                 (t
                  (write-char char out)))))))

(defun decode (string)
  "Decode single-line encoded string back to S-expression.
\\x1E characters are converted back to newlines before reading.
\\x1E\\x1E sequences are converted back to single \\x1E."
  (let ((decoded-string (with-output-to-string (out)
                          (loop with i = 0
                                with length = (length string)
                                while (< i length)
                                for char = (char string i)
                                do (cond
                                     ((char= char +record-separator+)
                                      (if (< (1+ i) length)
                                          (let ((next-char (char string (1+ i))))
                                            (cond
                                              ((char= next-char +record-separator+)
                                               ;; Escaped record separator: \x1E\x1E -> \x1E
                                               (write-char +record-separator+ out)
                                               (incf i 2))
                                              ((char= next-char #\Space)
                                               ;; Encoded newline: \x1E<space> -> \n
                                               (write-char #\Newline out)
                                               (incf i 2))
                                              (t
                                               ;; Invalid sequence, treat as literal
                                               (write-char char out)
                                               (incf i))))
                                          ;; End of string, treat as literal
                                          (progn
                                            (write-char char out)
                                            (incf i))))
                                     (t
                                      (write-char char out)
                                      (incf i)))))))
    (read-from-string decoded-string)))

(defun out (sexp &optional (stream *standard-output*))
  "Write S-expression as encoded line to stream with SIGPIPE handling.
Automatically adds newline and handles broken pipes gracefully."
  (handler-case
      (progn
        (write-string (encode sexp) stream)
        (terpri stream)
        (force-output stream))
    #+sbcl
    (sb-int:broken-pipe ()
      (uiop:quit 0))
    (stream-error ()
      (uiop:quit 0))
    (end-of-file ()
      (uiop:quit 0))))

(defun in (&key (stream *standard-input*) (on-error nil) (error-value nil) (eof-value nil))
  "Read and decode S-expression from stream.
ON-ERROR options:
  nil - Return ERROR-VALUE on error (stop processing)
  :skip - Skip bad line and try next line
  :signal - Signal the error
ERROR-VALUE - Value to return on error (default NIL)
EOF-VALUE - Value to return on EOF (default NIL)"
  (loop
    (handler-case
        (let ((line (read-line stream nil nil)))
          (if line
              (handler-case
                  (return (decode line))
                (error (decode-error)
                  (case on-error
                    ((nil) (return error-value))
                    (:skip 
                     ;; Continue to next iteration to try next line
                     nil)
                    (:signal (error decode-error))
                    (otherwise (return error-value)))))
              (return eof-value)))  ; EOF
      (stream-error () (return error-value))
      (end-of-file ()
        ;; EOF while reading from main stream (not from decode)
        (return eof-value))
      (error (e)
        (case on-error
          ((nil) (return error-value))
          (:skip 
           ;; Continue to next iteration to try next line
           nil)
          (:signal (error e))
          (otherwise (return error-value)))))))

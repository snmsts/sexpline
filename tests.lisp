(defpackage :sexpline/tests
  (:use :cl :rove :sexpline))

(in-package :sexpline/tests)

(deftest test-basic-encoding-decoding
  (testing "Basic encode/decode functionality"
    (ok (string= "(A B C)" (encode '(a b c))))
    (ok (equal '(a b c) (decode "(A B C)"))))
  
  (testing "Newline handling"
    (let ((sexp-with-newlines '("line1\nline2" "single")))
      (let ((encoded (encode sexp-with-newlines)))
        (ok (not (find #\Newline encoded)))
        (ok (equal sexp-with-newlines (decode encoded)))))))

(deftest test-stream-input-normal
  (testing "Normal stream input"
    (with-input-from-string (stream "(test 1)
(test 2)
")
      (ok (equal '(test 1) (in :stream stream)))
      (ok (equal '(test 2) (in :stream stream)))
      (ok (eq nil (in :stream stream :eof-value nil))))))

(deftest test-stream-input-error-handling
  (testing "Error handling modes"
    ;; Test with invalid syntax
    (with-input-from-string (stream "(valid 1)
(invalid
(valid 2)
")
      (let (results)
        (loop for data = (in :stream stream :on-error :skip :eof-value :eof :error-value :error)
              until (eq data :eof)
              do (push data results))
        (setf results (nreverse results))
        (ok (equal '(valid 1) (first results)))
        ;; Should skip the invalid line and continue
        (ok (equal '(valid 2) (second results)))))))

(deftest test-eof-and-error-values
  (testing "EOF and error value differentiation"
    (with-input-from-string (stream "")
      (ok (eq :my-eof (in :stream stream :eof-value :my-eof))))
    
    (with-input-from-string (stream "(invalid")
      (ok (eq :my-error (in :stream stream :error-value :my-error))))))

(defpackage :sexpline
  (:nicknames :sexpl)
  (:use :cl)
  (:export
   #:encode
   #:decode
   #:in
   #:out)
  (:documentation "S-expression line encoding/decoding library inspired by JSONLines.
Encodes S-expressions containing newlines into single-line format using \\x1E (Record Separator) as escape character."))

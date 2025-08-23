# Sexpline

A Common Lisp library for encoding and decoding S-expressions as single-line strings, inspired by [JSONLines](https://jsonlines.org/).

## Overview

Sexpline allows you to encode S-expressions containing newlines into a single-line format that can be safely transmitted over text protocols, stored in line-based formats, or processed by stream-oriented tools. It uses the ASCII Record Separator character (\\x1E) as an escape character to preserve newlines while maintaining readability.

## Features

- **Single-line encoding**: Convert any S-expression to a single-line string format
- **Perfect round-trip**: Decode preserves the exact original S-expression
- **Stream I/O**: Read and write S-expressions from/to streams with robust error handling
- **Flexible error handling**: Skip invalid lines, return custom error values, or signal errors
- **SIGPIPE protection**: Graceful handling of broken pipes in streaming scenarios
- **Multiple Lisp support**: Tested and confirmed on SBCL, CCL, and ECL

## Installation

```lisp
;; Load the system
(asdf:load-system :sexpline)

;; Or with Quicklisp (once published)
(ql:quickload :sexpline)
```

## Quick Start

```lisp
(use-package :sexpline)

;; Basic encoding/decoding
(encode `(hello ,(format nil "world~%with newlines") :test 123))
;; => "(HELLO \"world\x1E with newlines\" :TEST 123)"

(decode "(HELLO \"world\x1E with newlines\" :TEST 123)")
;; => (HELLO "world
;; with newlines" :TEST 123)

;; Stream I/O
(out '(my data))          ; Write to *standard-output*
(in)                      ; Read from *standard-input*
```

## API Reference

### Core Functions

#### `encode (sexp)`
Encode an S-expression to single-line string format.

```lisp
(encode `(a ,(format nil "string~%with~%newlines") b))
;; => "(A \"string\x1E with\x1E newlines\" B)"
```

#### `decode (string)`
Decode a single-line encoded string back to an S-expression.

```lisp
(decode "(A \"string\x1E with\x1E newlines\" B)")
;; => (A "string
;; with
;; newlines" B)
```

### Stream I/O Functions

#### `out (sexp &optional stream)`
Write an S-expression as an encoded line to a stream. Automatically handles newlines and SIGPIPE errors.

```lisp
(out '(hello world))                    ; Write to *standard-output*
(out '(hello world) *error-output*)     ; Write to *error-output*
```

#### `in (&key stream on-error error-value eof-value)`
Read and decode an S-expression from a stream with comprehensive error handling.

**Parameters:**
- `stream`: Input stream (default: `*standard-input*`)
- `on-error`: Error handling mode (default: `nil`)
  - `nil`: Return `error-value` on error (stop processing)
  - `:skip`: Skip bad lines and try next line
  - `:signal`: Re-signal the error
- `error-value`: Value to return on error (default: `nil`)
- `eof-value`: Value to return on EOF (default: `nil`)

**Examples:**

```lisp
;; Basic usage
(in)                                    ; Read one S-expression

;; Error handling
(in :on-error :skip)                    ; Skip invalid lines
(in :eof-value :done)                   ; Custom EOF marker
(in :error-value :invalid)              ; Custom error marker
(in :on-error :skip :eof-value :done)   ; Skip errors, custom EOF
```

## Usage Examples

### Processing a stream with error recovery

```lisp
;; Process all valid S-expressions, skipping invalid ones
(loop for data = (sexpl:in :on-error :skip :eof-value :eof)
      until (eq data :eof)
      do (format t "Got: ~A~%" data))
```

### Distinguishing between EOF and errors

```lisp
(loop for result = (sexpl:in :eof-value :eof :error-value :error)
      do (case result
           (:eof (format t "End of file reached~%") (return))
           (:error (format t "Invalid line encountered~%"))
           (t (format t "Valid data: ~A~%" result))))
```

### Pipeline processing (like jq for JSON)

```lisp
;; Filter and transform S-expressions
(loop for item = (sexpl:in :on-error :skip :eof-value :eof)
      until (eq item :eof)
      when (and (listp item) (eq (first item) :user))
      do (sexpl:out (list :name (getf item :name))))
```

## Encoding Details

Sexpline uses the ASCII Record Separator character (\\x1E, decimal 30) as an escape character:

- Newlines (`#\Newline`) → `\x1E` + space
- Existing `\x1E` characters → `\x1E` + `\x1E` (doubled)
- All other characters remain unchanged

This encoding ensures:
- **Single-line output**: No newlines in encoded strings
- **Reversibility**: Perfect round-trip encoding/decoding
- **Readability**: Human-readable with visible escape sequences
- **Safety**: No conflicts with common text processing tools

## Error Handling

Sexpline provides robust error handling for stream processing:

1. **Parse Errors**: Invalid S-expression syntax
2. **Stream Errors**: I/O problems, network issues
3. **SIGPIPE**: Broken pipes in streaming scenarios
4. **EOF**: End of input stream

The `:skip` mode is particularly useful for processing large datasets where some lines might be corrupted or invalid.

## Testing

Run the test suite:

```bash
make test
```

Or programmatically:

```lisp
(asdf:test-system :sexpline)
```

## Comparison with JSONLines

| Feature | JSONLines | Sexpline |
|---------|-----------|----------|
| Data format | JSON objects | S-expressions |
| Line-based | ✓ | ✓ |
| Streaming | ✓ | ✓ |
| Error recovery | Manual | Built-in |
| Comments | ✗ | ✓ (in S-expressions) |
| Nested structures | ✓ | ✓ |
| Type system | Limited | Full Lisp |

## License

MIT

## Author

SANO Masatoshi

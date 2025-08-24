# lq - S-expression processor

`lq` is a command-line tool for processing S-expressions, inspired by [jq](https://jqlang.org/) but designed for Common Lisp data. It uses the [sexpline format](FORMAT.md) for line-based S-expression processing.

## Installation

```bash
# Install both lq tool and sexpline library
ros install snmsts/sexpline

```

## Quick Start

```bash
# Basic filtering
echo '(:name "Alice" :age 30)' | lq "(getf x :name)"

# Process files
lq "(getf x :status)" data.sexpl

# Multiple files
lq "(when (> (getf x :age) 20) x)" users1.sexpl users2.sexpl

# Raw string output (no sexpline encoding)
echo '(:message "Hello World")' | lq -r "(getf x :message)"
# Output: Hello World (plain text)

# Collect all inputs into list
echo -e '(:count 10)\n(:count 20)' | lq -s "(apply #'+ (mapcar (lambda (item) (getf item :count)) x))"
# Output: 30
```

## Command Line Usage

```
lq [options] <filter> [file...]
```

### Options

- `-f, --from-file` - Read filter from file instead of command line
- `-l, --load` - Load Lisp file before processing (can be used multiple times)
- `-e, --eval` - Evaluate expression before processing (can be used multiple times)
- `-r, --raw-output` - Output raw strings instead of encoded S-expressions
- `-s, --slurp` - Read all inputs into a single list and process together
- `-n, --null-input` - Don't read input; use nil as the input value
- `-c, --compact` - Produce compact output (planned)
- `--version` - Show version information
- `--help` - Show help message

### Filters

Filters are Common Lisp expressions that operate on the input data (bound to variable `x`):

```bash
# Access property
lq "(getf x :name)"

# Conditional filtering
lq "(when (> (getf x :age) 18) x)"

# Transform data
lq "(list :name (getf x :name) :adult (>= (getf x :age) 18))"

# Multiple values (outputs multiple lines)
lq "(values (getf x :first-name) (getf x :last-name))"
```

## Examples

### Basic Property Access
```bash
# Input: (:name "Alice" :age 30 :city "Tokyo")
lq "(getf x :name)"     # Output: "Alice"
lq "(getf x :age)"      # Output: 30
```

### Conditional Processing
```bash
# Filter adults only
lq "(when (>= (getf x :age) 18) x)"

# Transform with conditions
lq "(if (> (getf x :score) 80) :pass :fail)"
```

### Working with Lists (-s/--slurp)
```bash
# Count total items
lq -s "(length x)"

# Find maximum age
lq -s "(apply #'max (mapcar (lambda (item) (getf item :age)) x))"

# Group by category
lq -s "(group-by (lambda (item) (getf item :category)) x)"
```

### Raw Output (-r/--raw-output)
```bash
# Get plain text messages
lq -r "(getf x :message)"

# Format output
lq -r "(format nil \"Name: ~A, Age: ~A\" (getf x :name) (getf x :age))"
```

### Processing Multiple Files
```bash
# Process user data from multiple files
lq "(getf x :email)" users/*.sexpl

# Combine data from multiple sources
lq -s "(mapcar (lambda (item) (getf item :id)) x)" data1.sexpl data2.sexpl
```

## Sexpline Data Format

The [sexpline format](FORMAT.md) allows S-expressions to be stored and transmitted as single-line strings, similar to JSONLines. This makes it suitable for:

- Stream processing and pipelines
- Log files and data exports  
- Network protocols that expect line-based data
- Integration with text processing tools

### Encoding Rules

- Newlines (`#\Newline`) are replaced with ASCII Record Separator (`\x1E`) + space
- Existing `\x1E` characters are escaped as `\x1E\x1E` 
- All other characters remain unchanged
- Perfect round-trip: decode(encode(data)) = data

### Examples

```lisp
;; Original S-expression with newlines
(message "Hello
World" status :ok)

;; Encoded as single line
"(MESSAGE \"Hello\x1E World\" STATUS :OK)"

;; When decoded, newlines are restored perfectly
```

## Programming Interface

For programmatic use, the sexpline library provides a simple API:

```lisp
(ql:quickload :sexpline)
(use-package :sexpline)

;; Encoding/Decoding
(encode '(hello world))           ; => "(HELLO WORLD)"
(decode "(HELLO WORLD)")          ; => (HELLO WORLD)

;; Stream I/O with error handling
(out '(data here))                ; Write to stdout
(in)                              ; Read from stdin
(in :on-error :skip)              ; Skip invalid lines
(in :eof-value :done)             ; Custom EOF handling
```

### Stream Processing

```lisp
;; Process all valid S-expressions from stdin
(loop for data = (in :on-error :skip :eof-value :eof)
      until (eq data :eof)
      do (process-data data))

;; Write results to stdout with SIGPIPE protection
(dolist (result results)
  (out result))
```

### Error Handling

The library provides comprehensive error handling for stream processing:

- **Parse errors**: Invalid S-expression syntax
- **Stream errors**: I/O problems, network issues  
- **SIGPIPE**: Broken pipes in streaming scenarios
- **EOF**: End of input detection

Options for `in` function:
- `:on-error nil` - Return error-value and stop (default)
- `:on-error :skip` - Skip invalid lines, continue processing
- `:on-error :signal` - Re-signal the error for custom handling
- `:error-value` - Custom value to return on errors
- `:eof-value` - Custom value to return on EOF

## Comparison with jq

| Feature | jq | lq |
|---------|----|----|
| Data format | JSON | S-expressions |  
| Filter language | jq syntax | Common Lisp |
| Streaming | ✓ | ✓ |
| Multiple files | ✓ | ✓ |
| Error recovery | Manual | Built-in |
| Raw output | ✓ | ✓ |
| Slurp mode | ✓ | ✓ |
| Null input | ✓ | ✓ |
| Comments in data | ✗ | ✓ |
| Complex data types | Limited | Full Lisp |
| Programmability | Limited | Full programming language |

## Requirements

- [Roswell](https://github.com/roswell/roswell) - Common Lisp installation manager
- Compatible Lisp implementation (SBCL, CCL, ECL tested)

## License

MIT

## Author

SANO Masatoshi

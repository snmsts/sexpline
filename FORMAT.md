# Sexpline Format

**Sexpline** is a text format for streaming S-expression data, inspired by [JSONLines](https://jsonlines.org/). Each line contains exactly one S-expression encoded as a single line.

## What is Sexpline?

- **Line-delimited S-expressions**: Each line is a valid, self-contained S-expression
- **Streamable**: Process large datasets without loading everything into memory
- **Text-based**: Human readable and works with standard Unix tools
- **Preserves structure**: Maintains all Lisp data types and nested structures
- **Comment-friendly**: S-expressions can contain comments (unlike JSON)

## Example Sexpline File

```lisp
(:name "Alice" :age 30 :city "Tokyo")
(:name "Bob" :age 25 :city "New York")
(:name "Carol" :age 35 :city "London")
```

Each line represents one data record.

## Use Cases

### Log Processing
```lisp
(:timestamp "2024-01-15T10:30:00Z" :level :info :message "User login" :user-id 123)
(:timestamp "2024-01-15T10:31:00Z" :level :warn :message "High CPU usage" :cpu 85.2)
(:timestamp "2024-01-15T10:32:00Z" :level :error :message "Database timeout" :query "SELECT * FROM users")
```

### Configuration Files
```lisp
(:service "web" :port 8080 :workers 4)
(:service "api" :port 3000 :workers 2 :auth :required)
(:service "worker" :queue "jobs" :threads 8)
```

### Database Export
```lisp
(:id 1 :name "Product A" :price 29.99 :tags (:electronics :gadget))
(:id 2 :name "Product B" :price 15.50 :tags (:book :education))
(:id 3 :name "Product C" :price 99.00 :tags (:electronics :computer))
```

### API Streaming
```lisp
(:event "user_created" :data (:id 123 :email "alice@example.com"))
(:event "user_updated" :data (:id 123 :name "Alice Smith"))
(:event "user_deleted" :data (:id 123))
```

## Encoding Rules

When S-expressions contain newlines, they are encoded to ensure each record fits on a single line:

1. **Newlines** (`#\Newline`) are replaced with ASCII Record Separator (`\x1E`) followed by a space
2. **Existing Record Separators** (`\x1E`) are escaped as `\x1E\x1E`
3. **All other characters** remain unchanged
4. **Perfect round-trip**: `decode(encode(data)) = data`

### Encoding Examples

#### Simple Case (No Encoding Needed)
```lisp
;; Input
(:name "Alice" :age 30)

;; Output (same)
(:name "Alice" :age 30)
```

#### With Newlines
```lisp
;; Input
(:message "Line 1
Line 2
Line 3" :status :ok)

;; Encoded output
(:MESSAGE "Line 1\x1E Line 2\x1E Line 3" :STATUS :OK)
```

#### With Existing Record Separators
```lisp
;; Input (contains \x1E character)
(:data "text\x1Emore")

;; Encoded output (escaped)
(:DATA "text\x1E\x1Emore")
```

#### Complex Example
```lisp
;; Input with multiple newlines and nested structure
(:log (:level "INFO" 
       :message "Multi-line
error message
with details")
 :context (:file "app.lisp" :line 42))

;; Encoded as single line
(:LOG (:LEVEL "INFO" :MESSAGE "Multi-line\x1E error message\x1E with details") :CONTEXT (:FILE "app.lisp" :LINE 42))
```

## File Extensions

- **`.sexpl`** - Recommended extension for sexpline data files
- **`.sexp`** - Alternative for S-expression data
- **No extension** - Format is self-describing and can work without extensions

## Processing with Unix Tools

Sexpline format works well with standard Unix text processing tools:

```bash
# Count records
wc -l data.sexpl

# Get first 10 records  
head -10 data.sexpl

# Search for records containing specific text
grep ":error" logs.sexpl

# Combine multiple files
cat file1.sexpl file2.sexpl > combined.sexpl

# Split large files
split -l 1000 large.sexpl chunk_
```

## Comparison with JSONLines

| Feature | JSONLines | Sexpline |
|---------|-----------|----------|
| **Data Format** | JSON objects | S-expressions |
| **Line-based** | ✓ | ✓ |
| **Streaming** | ✓ | ✓ |
| **Comments** | ✗ | ✓ |
| **Nested Data** | ✓ | ✓ |
| **Type System** | Limited (string, number, boolean, null, array, object) | Rich (symbols, keywords, lists, vectors, etc.) |
| **Human Readable** | ✓ | ✓ |
| **Unix Tools** | ✓ | ✓ |
| **Escape Complexity** | Medium (JSON escaping) | Low (minimal escaping) |

## Implementation

The sexpline format is implemented in the `sexpline` Common Lisp library:

- **Encoding**: `(sexpline:encode sexp)` → single-line string
- **Decoding**: `(sexpline:decode string)` → original S-expression
- **Stream I/O**: `(sexpline:out sexp)` and `(sexpline:in)` for streaming


## Tools

- **lq** - Command-line processor for sexpline data (like jq for JSON)
- **sexpline library** - Common Lisp implementation for encoding/decoding

## License

This format specification is public domain. Implementations may have their own licenses.

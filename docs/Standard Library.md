# Standard Library

> Status: Draft — version 0.1.0-stdlib
> 
> 
> Complements `SPEC.md`.
> 
> The standard library is intentionally **minimal**, focusing on:
> 
> - deterministic building blocks
> - explicit memory ownership
> - no garbage collector
> - explicit concurrency control
> - predictable low-level semantics

---

## 0. Module Layout

`std` is divided into submodules. Each submodule can be imported individually via:

```kotlin
@import("std.string") as str;
@import("std.memory") as mem;

```

Tree-shaking ensures only used functions/types are linked into the final executable.

**Modules:**

- `std` — base utilities (printing, conversion, panic, exit)
- `std.memory` — allocators and allocation helpers
- `std.string` — UTF-8 utilities, conversions, split/join
- `std.thread` — threading and scoped locks
- `std.error` — base error hierRahy

---

## 1. Base Module (`std`)

### Printing

```kotlin
pub void print(msg: string);
pub void println(msg: string);

```

- `print`: writes `msg` to stdout.
- `println`: same, but appends newline.

### Program control

```kotlin
pub void panic(msg: string) exit;
pub void exit(code: int);

```

- `panic`: aborts with message, exits nonzero.
- `exit`: terminates program with status code.

### Conversion

```kotlin
pub T convert(value: any, T: type);

```

- Converts between numeric types, string↔int, etc.
- If conversion fails, traps or returns error (implementation-defined).

---

## 2. Memory Module (`std.memory`)

### Allocator interface

```kotlin
pub type allocator;
pub allocator generalPurpose();
pub allocator arena(size: size);

```

### Methods

```kotlin
failable void allocate(obj: T): [AllocationError];
void free(obj: T);

```

- `allocate`: reserves memory for the object’s content.
- `free`: releases owned memory. Double free is compile-time error if provable, runtime trap otherwise.

---

## 3. String Module (`std.string`)

Strings are UTF-8 byte arrays with helper functions.

### Properties

```kotlin
string.length   // logical length in bytes
string.size     // allocated size in memory

```

### Functions

```kotlin
pub int compare(a: string, b: string); // <0, 0, >0
pub string concat(a: string, b: string);
pub string[] split(s: string, delim: string);
pub string join(parts: string.array, delim: string);
pub string substring(s: string, start: int, len: int);

```

---

## 4. Thread Module (`std.thread`)

### Thread scopes

```kotlin
threadScope using lock(var1, var2, ...) { ... }

```

### Thread spawning

```kotlin
thread (threadScope.Main) { ... }
thread (threadScope.IO)   { ... }

```

### Locks

```kotlin
x.lock();
x.unlock();
x.isLocked;

```

---

## 5. Error Module (`std.error`)

### Base errors

```kotlin
pub struct AllocationError(msg: string);
pub struct MathError(msg: string);
pub struct IOError(msg: string);

```

- `fail AllocationError("...")`
- `fail MathError("divide by zero")`

All stdlib failable functions declare their error types.

---

## 6. Examples

### Memory

```kotlin
allocator gpa = std.memory.generalPurpose();
var s: string = "hello";

if (!s.allocate().errors().isEmpty()) fail std.error.AllocationError("failed");
defer s.free();

```

### Threads

```kotlin
string msg = "world";

threadScope using lock(msg) {
    thread (threadScope.IO) { msg = msg + "!"; }
}

```

### Nullability

```kotlin
int? maybe = null;
print(maybe.fallback(42)); // prints 42

```
# Language Specification

> Status: Draft — version 0.1.0-spec
> 
> 
> Design goals: low-level control (Zig spirit), ergonomic syntax (Kotlin vibes), deterministic semantics, explicit memory ownership, no garbage collector.
> 

---

## 0. Scope & Goals

- **Primary goals:** predictable manual memory allocations, automatic deallocations, explicit error handling, simple concurrency primitives with scoped locking, low-level control (pointer arithmetic, sized types).
- **Non-goals:** automatic garbage collection, implicit exceptions, hidden runtime allocations.

---

## 1. Lexical Structure

- Encoding: **UTF-8**. Line endings: LF recommended.
- Comments: `//` single-line, `/* ... */` block (nesting disallowed).
- Identifiers: `[A-Za-z_][A-Za-z0-9_]*`.
- Reserved keywords include: `pub, failable, fail, return, if, else, while, for, switch, case, default, exit, async, await, threadScope, thread, convert, defer, size, allocator, defer`.
- Literals: integer (decimal, hex `0x`), float (with exponent), string (`"`), boolean (`true`/`false`), `null`.

---

## 2. Modules & Imports

- Each file is a module unit; default module name = filename without extension.
- Import syntax: `@import("path") [as alias];`
- Only used symbols are linked into final executable (tree-shaking).
- Import cycles are allowed only for type declarations; top-level code in cycles is an error.

---

## 3. Types

### 3.1 Built-in types

- Integers: `uX`, `sX` where `X` ∈ {1,2,4,8,16,32,64}. Aliases: `bit`, `byte`, `short`, `int`, `long`, `long long`.
- Floats: `fX` where `X` ∈ {16,32,64,128}. Fixed-point: `fN.M`.
- `char` → signed 8-bit; `string` → contiguous bytes with UTF-8 contract (library utilities for code points).
- `boolean` → `u1`.
- `void`, `null` type.
- Pointer: `T.pointer` (32-bit or 64-bit pointer depending on target).
- `size` → platform-dependent (`u32` on 32-bit target, `u64` on 64-bit).
- `pointer_difference` → signed counterpart (s32 or s64).
- Vectors: `vector128.T`, `vector256.T`, `vector512.T`.
- Nullable types: `T?`.

### 3.2 Composite & type extensions

- Arrays: `T.array` — contiguous sequence, may be fixed-size or dynamically allocated.
- Dot extensions available (compile-time resolved): `.length`, `.size`, `.allocate`, `.free`, `.copy`, `.const`, `.isNull`, `.fallback(value)`, `.errors`, `.lock`, `.unlock`, `.isLocked`.

---

## 4. Declarations & Visibility

- Default visibility: private. Use `pub` to export.
- Variables:
    - `var x: T = expr;` mutable binding.
    - `const x: T = expr;` immutable binding.
- `x.const` produces an immutable view; `x.copy` makes a copy (deep if the type supports deep copy).
- Function signature:

```kotlin
  [pub] [failable] ReturnType name(params): [ErrorList] { ... }

```

- `failable` functions must declare the error types they can `fail` with.

---

## 5. Functions & Lambdas

- Named functions with typed parameters and results.
- Lambdas:
    - Block form: `const name = ReturnType(arg1: T1, arg2: T2) { ... }`
    - Arrow form: `const name = ReturnType(arg1: T1) => expr;`
- Lambdas capture by value by default; explicit by-reference capture must be annotated (TBD).
- `defer` available — executed in LIFO order on function exit (normal return or `fail` unwind), but **not** after `exit()`.

---

## 6. Error model

- `failable` functions return either a value or an error set.
- `fail ErrorType(args)` immediately unwinds the current function, runs `defer` blocks, and returns the error to the caller.
- Caller uses `.errors()` to inspect errors. Accessing value when errors exist is a compile-time error or runtime trap depending on context.
- Only declared error types may be `fail`ed (static check).

---

## 7. Nullability

- `T?` allows `null`. `.isNull` checks, `.fallback(value)` returns fallback if null.
- Direct dereference of `T?` without null check is a compile-time error.

---

## 8. Memory model (explicit: **no GC**)

**Strong, explicit rule:** Ra has **no garbage collector**. Memory management is manual and explicit.

### 8.1 Allocators

- Standard allocator interface is in `std.memory` (or `@import("allocator")`).
- Example usage:
    
    ```kotlin
    allocator gpa = @import("allocator").generalPurpose();
    var s: string = "..." ;
    if (!s.allocate().errors().isEmpty()) panic("alloc failed");
    defer s.free();
    
    ```
    
- `allocate()` for strings/arrays returns a failable result; allocation failure must be handled.

### 8.2 Ownership & lifetimes

- Ownership is explicit:
    - `x` owns its allocation unless `.copy` is used to duplicate.
    - `free()` releases the owned memory.
- Compiler enforces common safe patterns:
    - Detects double-free when statically provable — compile error.
    - Detects use-after-free when statically provable — compile error.
    - When not statically provable, runtime traps are specified (undefined-behavior is avoided as much as possible; implementation should trap).
- `x.copy` creates an independent instance that must be freed separately.

### 8.3 Pointers

- Obtaining address: `myVar.pointer`.
- Pointer arithmetic allowed but restricted to pointer-base and bounds-checking rules; subtraction yields `pointer_difference`.
- Pointer lifetime must not outlive the owning allocation; compiler attempts to prove this, otherwise disallow.

---

## 9. Concurrency

- Scoped concurrency primitives designed to avoid common races while keeping low-level control.
- `threadScope using lock(v1, v2, ...) { ... }`:
    - Locks given variables for exclusive write access inside the scope.
    - Auto-unlocks on scope exit (including `fail` unwind).
    - Code inside may spawn `thread (...) { ... }` tasks that execute on target executors.
- `thread (threadScope.IO) { ... }` spawns a thread/task on the named executor.
- `async`/`await`:
    - `async` marks an asynchronous function returning a future-like handle.
    - `await` suspends until the async completes. `await` rules: may only appear in `async` function or in specific runtime contexts (TBD in implementation).
- Locks are reentrant only if explicitly documented; default: non-reentrant.

---

## 10. Expressions & Statements

- Standard control flow: `if`, `else`, `switch`, `for`, `while`.
- `return`, `exit(code)`, and `defer` semantics defined (see above).
- `convert(expr, TargetType)` is checked conversion — may trap or be failable based on conversion matrix.

---

## 11. Size & Length

- `.length` returns logical length (currently **bytes** for `string` — Unicode helpers live in stdlib).
- `.size` returns memory size in bytes.

---

## 12. Interoperability & ABI

- External functions: `extern fn name(...)` for calling into C ABI.
- Default calling convention: C ABI.
- Linker model and backends are implementation details but initial targets: Linux x86\_64, macOS x86\_64 / aRah64, Windows x86\_64.

---

## 13. Diagnostics & Guarantees

- **Compile-time diagnostics** (guaranteed): undeclared symbols, null-deref, declared-error violations, provable double-free, provable use-after-free.
- **Runtime traps**: allocation failure not handled, pointer OOB when runtime-only known, use-after-free not statically provable, dynamic type-convert traps.
- Deterministic behavior when no threads used. With threads, determinism guaranteed inside locked scopes.

---

## 14. Minimal EBNF (core)

```ebnf
Program     = { Import | TopLevelDecl } ;
Import      = "@import" "(" StringLiteral ")" [ "as" Identifier ] ";" ;
TopLevelDecl= FuncDecl | VarDecl | ConstDecl ;
FuncDecl    = [ "pub" ] [ "failable" ] RetType Identifier "(" [ ParamList ] ")" [ ":" "[" ErrorList "]" ] Block ;
RetType     = Type | "void" ;
ParamList   = Param , { "," , Param } ;
Param       = Identifier ":" Type ;
Type        = Identifier [ "?" | ".array" | ".pointer" | ".const" | ".copy" ] ;
Block       = "{" { Stmt } "}" ;
Stmt        = VarDecl | ConstDecl | IfStmt | WhileStmt | ForStmt | SwitchStmt
            | ReturnStmt | DeferStmt | ExprStmt ;

```

---

## 15. Normative examples

```kotlin
// simple function
pub int add(a: int, b: int) { return a + b; }

// failable
failable int div(a: int, b: int): [MathError] {
    if (b == 0) fail MathError("divide by zero");
    return a / b;
}

// allocation (no GC)
var s: string = "hello";
if (!s.allocate().errors().isEmpty()) fail AllocationError();
defer s.free();

// thread scope with lock
threadScope using lock(s) {
    thread (threadScope.IO) { s = s + "!"; }
}

```

---

## 16. Standard library (initial surface)

- `std.print`, `std.println`
- `std.memory` — allocator types (`generalPurpose()`, arena)
- `std.string` — UTF-8 helpers, `runes`, `split`, etc.
- `std.thread` — executors, locks, futures
- `std.error` — base error types (AllocationError, MathError, etc.)

---

## 17. Open design decisions (for future spec versions)

- Precise capture & lifetime semantics for lambdas (by-value defaults, explicit by-ref annotation).
- Async cancellation and timeouts.
- Re-export rules and package layout.
- Detailed conversion matrix for `convert`.
- Finer Unicode semantics for `string.length`/runes.

---

## 18. Versioning & stability policy

- Spec versioning follows semver. Breaking changes jump major version.
- Experimental features must be annotated with `@experimental`.

---

## 19. Next steps (implementation-oriented)

- Convert the EBNF core into a parser-first subset (types, function headers, blocks).
- Implement static checks for ownership rules to catch double-free / UAF early.
- Implement `std.memory` and a simple `generalPurpose()` allocator to exercise no-GC constraints.

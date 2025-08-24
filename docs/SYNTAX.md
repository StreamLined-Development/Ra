# Arc - Syntax
The Syntax of Arc tries to be as high level as possible, while being a low level language.
Also the Syntax is heavily inspired by Kotlin and Zig.

# Imports
```kt
@import("standard") as std; // only what's used will be in the end executable
std.print() // is now available
```

# Functions
```kt
pub void hello() {} // A public function called hello
void hello() {} // A private function called hello

void hello(name: string) {} // name is a string.
void hello(names: string.array) {} // multiple names.
```

# Lambda
```kt
/*
* string is the return type (string {})
* arg1 is an argument, type is string
*/
const name = string(arg1: string, arg2: string) {
    return arg1 + arg2;
}
// or
const name = string(arg1: string, arg2: string) => return arg1 + arg2;

name("tomas", "mülller");

const failableLambda = failable int(x: int): [MathError] {
    if (x == 0) fail MathError("x cannot be zero");
    return 10 / x;
};


// -------- IN FUNCTION --------
/*
* string is the return type (string())
* arg is an argument, type is 1 byte
*/
void lambda_example(lambda: string(u8)) {
    string result = lambda(23);
}
```

# I dont know how to describe this.
```kt
failable void test(): [MathError] { // The Error Types Array can only be used on failable functions.
    if (0 == 0) fail MathError("Haha.");
}

if (test.errors().isEmpty()) return "SUCCESS";
else return "Failed....";

int? canBeNull() {
    return null;
}

int? canBeNullValue = canBeNull();
if (canBeNullValue.isNull) return "null";
else return canBeNullValue.
print(canBeNullValue.fallback(42));

int x = 9.const; // Never changes.
x = 8; // Will error out.
int y = x.copy; // Will only copy the content.
int z = x.copy.const; // Will copy the content and can never change.
```

# Arrays
```kt
string.array names = ["tomas", "john", "robert"];
names[0]; // tomas
names[0] = "jeremy"; // Manipulate the array.
```

# Memory management
```kt
allocator gpa = @import("allocator").generalPurpose()
gpa.allocate("") // <- size of input is unknown

var mamamia: string = "123456789012345 - "
if (!mamamia.allocate().errors().isEmpty) panic("Allocation failed"); // Will allocate the given size, or panic if allocation failed
defer mamamia.free() // <- everywhere where the scope ends this is called, except when you X#free() has been called already - Just Zig's defer keyword

mamamia.free() // Now the free wont be called on return, except its reinitialized
mamamia.free() // Compiler notices double free - wont compile

print(mamamia) // Compiler notices use after free - wont compile
```

# Pointer and Size Types
```kt
// Platform-dependent size type
size platformSize = myVar.size; // size is u32 on 32-bit, u64 on 64-bit

// Pointer difference
int.pointer ptr1 = ...;
int.pointer ptr2 = ...;

pointer_difference diff = ptr2 - ptr1; 
// diff is s32 on 32-bit, s64 on 64-bit
// Useful for array indexing, memory arithmetic

// Example:
int.array arr = [1, 2, 3, 4, 5];
int.pointer pStart = &arr[0];
int.pointer pEnd = &arr[4];

pointer_difference pdiff = pEnd - pStart; // 4
```

# Size and Length
```kt
string.array arr = ["a", "b", "c"];
arr.length; // will return 3
arr.size; // will return size in memory

string str = "babushka";
str.length; // will return 8
str.size; // will return size in memory
```

# Concurrency
```kt
string why = "because.";

threadScope using lock(why) { // runs as soon as its not locked anymore
    why.lock();
    defer while.unlock(); // or will automatically unlock on scope exit, i dont know yet.

    thread (threadScope.IO) {
        why = "what?";
        // do more
    }

    thread (threadScope.Main) {
        why = "just " + why;
        // do more
    }

    // The threads above will run in different threads, but with read-write access to all locked variables inside the scope.
}

async string fetchName() {
    // do fetching
}

string name = await fetchName();
```

# Types
All our types are displayed here, format: type » description/underlying type
- uX / sX **»** X can be anything, 1, 2, 3, 4, .... 64, ... **»** u8 would stand for unsigned 8 bits
- fX **»** X can be anything in 16 steps, 16, 32, 64, ... 128 **»** f32 would stand for float 32, f32.16 would stand for 32-bit fixed-point with 16 fractional bits
- char **»** s8
- string **»** alternative to char[]
- bit, byte, short, int, long, long long, float, double, long double **»** u1, u8, s16, s32, s32 (long on windows) or s64 (long on Linux/macOS 64-bit), s64, f32, f64, f80 or f128
- boolean **»** u1
- wchar_t, char16_t, char32_t **»** s16 or s32, u16, u32
- void **»** black hole.
- pointers (X.pointer) **»** 64-bit u64, 32-bit u32, Y.pointer when used as an argument, e.g.: string.pointer, u8.pointer, ...
- size **»** platform-dependet, u32 or u64
- pointer_difference **»** s32 or s64
- vectorX.Y **»** X can be 128, 256 or 512 and Y can be any type of the above (e.g. u8)
- null **»** nulltype

# Keywords
All of the keywords (like void, ..)
- pub **»** everything is by default private, if you want to make something public use pub ..., e.g.: pub int x = 0;
- X? **»** can be null, doesnt have to be.
- failable **»** this function can throw errors, please be cautios. (failable void iWillError(): [ERROR_TYPES] {fail Error("HAHA");})
- fail **»** can only be used in a failable function, e.g.: fail MathError("haha");
- return **»** returns the function
- if, else if, else **»** normal if else statement.
- while, for **»** normal loops.
- switch, case, default **»** normal switch statement.
- exit **»** exit the **complete** program, e.g.: exit(1)
- print, println
- convert **»** convert from one type to another. e.g.: convert(2, string) -> will return "2", convert("2726") -> will return 2726
- async, await, threadScope, thread

Type Extensions:
- X.array **»** string.array, would be String[] in Java
- X.array.isEmptyOrNull **»** true if an array is empty ([]) or null.
- X.array.isEmpty **»** true if an array is empty ([]).
- X.string.isBlankOrNull **»** true if an string is "" or null
- X.string.isBlank **»** true if an string is ""
- X.pointer **»** myVar.pointer, would be myVar* in C
- X.const **»** never changes.
- X.copy **»** copy the exact content.
- X.allocate **»** Allocate the content
- X.free **»** free the memory
- X.isNull **»** when something was marked with ?, e.g.: int? returnMe() {return null;}
- X.fallback **»** when the variable is null, e.g.: int? value = returnMe(); print(value.fallback(4)); // 4 is printed since null is returned
- X.errors **»** when the function could fail. e.g.: failable void iWillError(): [SomeError] {fail SomeError();}; iWillError.errors()
- X.size **»** size in memory, uX.
- X.length **»** arrays: [1, 2, 3, 4] would return 4, strings: "abcba" would return 5
- X.lock, X.unlock, X.isLocked **»** lock or unlock an variable when using multithreading so that only the current scope can modify it.
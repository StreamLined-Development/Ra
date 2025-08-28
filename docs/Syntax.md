# Syntax

The Syntax of Ra tries to be as high level as possible, while being a low level language.
Also the Syntax is heavily inspired by Kotlin, Zig, C and Rust.

## Imports

```kotlin
import pkg::1.4 as std; // only what's used will be in the end executable
std.print() // is now available
```

## Variables

```kotlin
// ==== Variables ====
var indentifier: type;
let dentifier: type;
const indentifier: type;

indentifier = letue; // already declared above

var indentifier: type = letue;
let indentifier: type = letue;
const indentifier: type = letue;
// inferred types
var indentifier = letue;
let indentifier = letue;
const indentifier = letue;
```

## Pointers

```kotlin
// ==== Pointers =====
var indentifier: &type;
let indentifier: &type;
indentifier = letue; // already declared above
var indentifier: &type = letue;
let indentifier: &type = letue;
var mut indentifier: &type = letue;
let mut indentifier: &type = letue; 
// const / var / let

// Derefferancing 
let my_var = *ptr + *ptr 
```

## Functions

```kotlin
/// ===== Functions =====
// pass by copy
fun add(x: i32, y:i32) i32{
	return x + y
}
let num_1: i32 = 15;
let num_2: i32 = 11;
let added_nums: i32 = add(num_1, num_2)

// pass by referance
fun add(x: &i32, y: &i32) i32{
	return x + y
}

// pass by mutible referance
fun add_to_end(mut x: &Vec<i32>, y: &i32) void {
	x.append(y)
}
```

## Generic Functions

```rust
/// ===== Generic Functions ======
fun add<T>( x: &T, y &T T {}
let sum:i32 = add<i32>( 11, 55 )

fun generic_func<T, T2, T3>( x: T, y: T2  T3 {}
let sum: u8 = generic_func<u8, i32, f16>( 33, 11, 21.41 );
```

## Guards

```kotlin
/// ===== Guards ======
fun grade(score: u8) string {
    | score >= 90 -> "A"
    | score >= 80 -> "B"
    | score >= 70 -> "C"
    | _ -> "F"
}
```

## Lambda

```kotlin
/*
* string is the return type (string {})
* arg1 is an argument, type is string
*/
const name = string(arg1: string, arg2: string) {
    return arg1 + arg2;
}

name("tomas", "mülller");

const failableLambda = failable int(x: int) [MathError] {
    if (x == 0) fail MathError("x cannot be zero");
    return 10 / x;
};

// -------- IN FUNCTION --------
/*
* string is the return type (string())
* arg is an argument, type is 1 byte
*/
fun lambda_example(lambda: string(u8)) void {
    string result = lambda(23);
}
```

## Enums

```rust
// ==== Enums ====
enum Direction {
	Up,
	Down,
	Left,
	Right
}

let my_direction: Direction = Direction::Up;

enum Shape {
    Circle(f64),          // stores a radius
    Rectangle(f64, f64),  // stores width, height
    Square(side: f64),    // named field style
}

let shape_of_wheel: Shape = Shape::Circle(25.12);
```

## Errors & Failable Functions

```kotlin
enum Nullable<T>{
  Okay(T),
	None,
}

enum Fallible<T, E>{
	Okay(T),
	Error(E)
}

enum Outcome<T, E>{
	Okay(T),
	Error(E),
	None
}

enum Either<T, T2>{
	Outcome1(T),
	Outcome2(T2),
}
```

## Scoped Generics

```rust
/// ===== Scoped Generics =====
generic F {f16, f32, f64, f128} //F is checked to be a float at compile time

struct Point<F>{
	x: F,
	y: F
}

// Error! F can only represent f16, f32, f64, f128, not i32
let coordinates: Point<i32> = {
	x = 22,
	y = 421
}

// letid
let coordinates: Point<f64> = {
	x = 13.21,
	y = 89.50
}
```

## Structs

```rust
// ==== Stucts ====
struct Person {
	name: str,
	age: u8,
	height: f16,
	method1: string(),
}

let instance: Person =  {
	name = "Jason"
	age = "20"
	height = 1.81
}
```

## Generic Structs

```rust
// ==== Generic Structs ====
struct Idk<T>{
	member_one: T,
	member_two: T,
	member_thee:T
}

let object: Idk<str> = {
	member_one = "hi"
	member_two = "bye"
	memeber_three = "hi again"
}
```

## Traits

```rust
/// ===== Traits =====
trait Printable{
	fun print(self void;
}
	
impl Printable for Person {
	fun print(self) void{
		println("Hi! My name is {[self.name](http://self.name)}, and I'm {self.age} years old");
	}
}

struct Person{
	name: str,
	age: i8,
}
```

## Trait Bound Generic

```rust
/// ===== Trait Bound Genric =====
trait Printable {
    fun print(self) void;
}

// generic function with trait bound
fun print_it<T: Printable>(x: T) void {
    x.print();
}
```

## Match

```rust
/// ===== Match =====

fun div(a: &i32, b: &i32 Fallible<i32, str>{
	| b == 0 -> Error(f"{a} is not divisible by 0");
	| _      -> Ok(a / b);
}

match div(15, 20){
	Success(letue) => println(letue);
	Error(ErrMsg)  => println(ErrMsg);
}

fun grade(score: u8 str {
    match score {
        90...100 => "A",
        80..89   => "B",
        70..79   => "C",
        60..69   => "D",
        _        => "F"
    }
}

fun describe_num(x: i32 str {
     let y: str = match x {
        x % 2 == 0 => "Even number",
        x % 2 != 0 => "Odd number"
     return x
    }
}

fun is_vowel(c: char bool {
    let y: bool = match c {
        'a' | 'e' | 'i' | 'o' | 'u' |
        'A' | 'E' | 'I' | 'O' | 'U' => true,
        _ => false
    }
}
```

## Type Alias

```rust
/// ===== Type Alias =====
typealias letter char;
```

## Variadic Types

```rust
/// ===== Variadic Types =====
fun sum(numbers: i32.. i32 {
    var total: i32 = 0
    for i in 0..numbers.len() {
        total = total + numbers[i]
    }
    return total
}
let a: i32 = sum(10, 55, 28, 77)
```

## Variadic Generics

```rust
/// ===== Variadic Generics =====
fun print_all<T>(numbers: T.. void {
   println(numbers);
}
print_all<f32>(3.4, 55.0, 22.2, 26.05);
```

## Arrays

```rust
/// ===== Array ======
let myNumbers[_]: array<i16> = {25, 50, 75, 100};
let myNumbers[_]: array<i16> = {25, 50, 75, 100};
```

## Vectors

```rust
/// ===== Vectors =====
var myNumbers: Vec<i16> = vec.new(i16)
```

## Hashmaps

```rust
let
```

## Strings

```rust
str String
```

## Ranges

```rust
/// ===== Ranges =====
for (i in 1..10) {
   println(i) // will print 1-9
}

for (i in 1...10) {
   println(i) // will print 1-10
}
```

## Memory management

```rust
let my_vector: Vec<i32> = new Vec(i32) // or Blank() for a clean slate
													|> Shrink_When_Free_Elements(10)
													|> Growth_Limit(2048)
													|> Length_Limit(2048)
													|> Growth_Rate(1.5)
													|> lifo()
```

## Pointer and Size Types

```kotlin
// Platform-dependent size type
size platformSize = myVar.size; // size is u32 on 32-bit, u64 on 64-bit

// Pointer difference
int* ptr1 = ...;
int* ptr2 = ...;

pointer_difference diff = ptr2 - ptr1;
// diff is s32 on 32-bit, s64 on 64-bit
// Useful for array indexing, memory arithmetic

// Example:
int[] arr = [1, 2, 3, 4, 5];
int* pStart = &arr[0];
int* pEnd = &arr[4];

pointer_difference pdiff = pEnd - pStart; // 4

```

## Size and Length

```kotlin
string[] arr = ["a", "b", "c"];
lengthOf(arr); // will return 3
sizeOf(arr); // will return size in memory

string str = "babushka";
lengthOf(str); // will return 8
sizeOf(str); // will return size in memory

```

## Concurrency

[Concurrency](Concurrency%2025b358279fb78037a99bdddf441facb0.md)

## Running Commands

```rust
/// ===== Running Commands =====
// one-off commands
let a: str = cmd("ls -la");
let pid: str = cmd("echo $$"); 
// presistent terminal 
let session = shell();
[session.run](http://session.run)("cd /tmp");
[session.run](http://session.run)("ls"); //remembers working directory
```

## The ? operator

```rust
fun safe_average(a: i32, b: i32, c: i32 Fallible<i32, str> {
    let sum = a + b + c;
    let avg = safe_div(sum, 3)!;   // error if divide by zero
    
    return Success(avg);
}

fun safe_div(x: i32, y: i32 Fallible<i32, str> {
    if y == 0 {
        return Error("division by zero");
    }
    return Success(x / y);
}
```

## Unions

```rust
/// ===== Unions =====
union Answers{
	answer_1: str 
	answer_2: str
	answer_3: str
}
```

## Pipe Operator

```rust
/// ===== Pipe Operator =====
var a: i32 = -28 |> increment() |> absolute() |> square_root();
```

## Tuples

```rust
/// ===== Tuples =====
// Tuple literal
let point: (i32, i32) = (10, 20);

// Tuple with mixed types
let record: (str, i32, f64) = ("Alice", 42, 98.6);

// Tuple without explicit type (inferred)
let coords = (3.5, -7.1);

var record: (str, i32, f32) = (Jason, 20, 1.8);
var person: (name, age, height) = record;
```

## While Loop

```rust
/// ===== While Loop =====

while (condition, execute after code block) {

    // code block to be executed
}
 
var i32 i = 0;

while (i < 5, i++) {
    print(i);
}

// infintie loop
loop{
		
}
```

## For Loop

```rust
/// ===== For Loop =====
var myNumbers[_]: array<i16> = {25, 50, 75, 100};

for (number in myNumbers) {
	print(f"My number is {number}");
}
```

## Macros

```rust
/// ===== Macros =====
// defer macros
macro fun hello_macro(ast: tokens tokens {
	let id: str = to_str([ast.id](http://ast.id))
	implement HelloMacro for [ast.id](http://ast.id){
		fun hello(){
			println(f"Hello, I am {id}")
		}
	}
}

macro fun clone_macro(ast: tokens tokens {
    // Implement Clone trait
    implement Clone for [ast.id](http://ast.id) {
        fun clone(&self Self {
            // Struct literal braces
            [ast.id](http://ast.id) {
                // Loop over fields, with proper spelling and commas
                for field in ast.fields:
                    field.clone(),  // automatically inserts comma between fields
            }
        }
    }
}

@clone_macro
struct Point {
    x: i32,
    y: i32,
}

// attribute macros
macro fun log_calls(attr: AttrArgs, item: ItemFn tokens {
    // Support default letue for level
    let level: str = attr.level != null ?? { attr.level } : { "info" }

    // Function splicing with arguments and body
    fun {[item.id](http://item.id)}({item.args} {item.return_type} {
        println(f"[{level}] Calling function {to_str([item.id](http://item.id))}")
        {item.body}
    }
}
// function like macro
macro vec_macro(elements: tokens) {
    alloc vec({
        for e in elements {
            e,
        }
    })
}
```

## Compile Time

```rust
/// ===== Comptime =====
// compile time cosntants:
comptime let PI: f64 = 3.1415926535;
// comptime function execution:
comptime let result: i32 = add(32, 54)
// or
let result: i32 = comptime add(35, 70)
// comptime function declaration:(all params must be known at comptime)
comptime fun ct_add<T>(x: T, y: T T{
	return x + y
}
// compile time blocks
let my_variable: i32 = comptime{
	var a: u32 = 10;
	var b: i32 = 50;
	return a + b;
} 
```

## Assembly

```rust
/// ===== Assembly =====
// simple asm
let result: i32 = asm { mov eax, {a}; 
									 add eax, 20; 
									 eax }
// integrated assembly
let y: i32 = assembly {
    inputs: a, b
    outputs: result
    clobbers: rax 

    {
        mov rax, {a}
        add rax, {b}
        mov {result}, rax
    }
} 
```

## OS

```rust
/// ===== OS =====
@naked // optional stack pointer
@interrupt(irq: u8, min_ring: u8 = 0, stack: &Stack = null)
@exception(vector: u8, error_code: bool = false, min_ring: u8 = 0)
@volatile(address: usize? = null)
@packed(alignment: u8 = 1, packed: bool = true)
@privilege(level: kernel|driver|service|user) 
```

## Types

```rust
// ==== Data Types ====
// first class ints
i8
i16
i32
i64
i128
u8
u16
u32
u64
u128
// arbitrary ints
i1
// to
i65535

u2
// to
u65535

// floats
f16
f32
f64
f80
f128
// other
strA // ASCII
str //UTF-8
str16 //UTF-16
str32 //UTF-32
charA
char
char16
char32
tuple
bool // true or false
isize
usize
type // type type
tokens // for macros
wstr
void
hashmap
map
list
arraylist
vec

// alias types:
char: i8
bit: u1,
byte: u8,
short: s16,
int: s32
long: s32 (windows), s64 (Linux/MacOS)
long long: s64
float: f32
double: f64
long double: f80 or f128
wchar_t: s16 or s32
char16_t: u16
char32_t: u32
pointers: u64 on 64-bit, u32 on 32-bit
size: u32 or u64 (platform-dependent)
pointer_difference: s32 or s64
vectorX.Y: X can be 128, 256 or 512 and Y can be any type of the above (e.g. u8)
```

## Keywords

All of the keywords (like void, ..)

- pub
- fun
- return
- if, else if, else
- while, for, for x in y…z, loop
- match, |
- exit
- print, println
- to **»** to from one type to another. e.g.: to<string>(2 will return "2"
- async, await, threadScope, thread
- typealias
- union, struct, enum
- var, let, const
- macro
- continue, break
- and, or
- import, as
- string/array manipulation
- asm, assembly
- comptime
- impl for, trait
- |>, ^, %, -, +, /, *, @
- 0…10 , 1..10 (rages)
- &x, *x (pointers)
- |, &, ^, ~, !,  <<, >>

## Built-in Functions

- isEmptyOrNull
- isEmpty
- isBlankOrNull
- isBlank
- .clone(my_new_var)
- isNull
- tryCast( )
- typeOf( )
- sizeOf » size in memory
- lengthOf » content/char count ([1,2,3] = 3, “abadas” = 6)
- lock(), unlock(), isLocked() **»** lock or unlock an variable when using multithreading so that only the current scope can modify it.
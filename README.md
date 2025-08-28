# Aria
A automatically parallel systems programming language that emphasizes compile time memory safety, type safety and permission scope safety.  All while offering a simple syntax and automated compile time memory management.

## AriaLang's two main features
- The memory management system you've been dreaming of
- A graph parallelization engine inpired by Interaction Combinators, with potentially seamless GPU-CPU co-ordination.

### Memory Management 
- Aria uses mostly static analysis to track **variable scope** and **variable aliasing**.
- In this language, you allocate, the compiler de-allocates. You can even create your own **custom alloctors!**
- Here is a breif explination of how it works:
  1. Manual Heap Allocation
  2. Automatically de-allocate heap memory when all the referances pointing to it (and their aliases) go out of scope.
  3. Arenas are de-alocated all at once when all referances pointing to them (and thir aliases) go out of scope.
  4. For loops with break, continue, or return, de-allocators are inserted at all possible exit paths or at a safe merge point after the loop.
  5. Loop variables are de-allocated after the loop ends.
  6. In nested loops, track pointer lifetimes across all levels to avoid de-allocating in an inner loop when the pointer is used in an outer loop.
  7. Pointers that are created in a function but returned or global will have their pointee's de-allocated after they are last used outside the function.
  8. Functions cannot dealloc pointers passed to them as argument. They can only deallocpointer defined in the function scope.
  9. The compiler first marks objects as potentially cyclic during pointer analysis (flagging assignments that could create cycles). At runtime, when dealloc(x) triggers for a flagged object, a lightweight depth-limited DFS checks if x belongs to an isolated reference cycle (objects referencing only each other). If true, the entire cycle is freed; if external references exist, normal last-use deallocation proceeds. 

### Parallelism
- Aria models its binary in the form of a graph of independant computations and binary blocks. And with it's MLIR backend it allows you to target both GPU and CPU architectures!
- The compiler will detect what should be put on the cpu and what should be put on the gpu, and then mark the nodes accordingly for the runtime.

## Syntax
### Variables 
``` rust
let my_variable: str = "hello world!" // immutable
var my_variable: str = "hello world!" // mutable
const my_variable: str = "hello world!" // known at compile time
```

### Pointers
``` rust
var my_number_ptr: &i32 = my_number  // can be re-assigned but cannot mutate the value it points to
var mut my_number_ptr: &i32 = my_number  // can be re-assigned and can mutate the value it points to
let my_number_ptr: &i32 = my_number // cannot be re-assigned and cannot mutate the value it points to
let mut my_number_ptr: &i32 = my_number  // cannot be re-assigned but can mutate the value it points to
```

### Functions
``` rust
// pass by copy
fun add(x: i32, y:i32) -> i32{
	return x + y
}
let num_1: i32 = 15
let num_2: i32 = 11
let added_nums: i32 = add(num_1, num_2)

// pass by referance
fun add(x: &i32, y: &i32) -> i32{
	return x + y
}

// pass by value
fun add_to_end(mut x: &Vec<i32>, y: &i32) -> void{
	x.append(y)
}
```

### Guards
``` rust
fun absolute(x:i32:mut) -> u32{
	| x < 0   -> -x
	| x > 0   -> x
}

fun grade(score: u8) -> string {
	| score >= 90 -> "A"
    | score >= 80 -> "B"
    | score >= 70 -> "C"
    | _ -> "F"
}
```

### Enums
``` rust
enum Direction {
	Up,
	Down,
	Left,
	Right
}

let player_direction: Direction = Direction::Up;

enum Shape {
    Circle(f64),          // stores a radius
    Rectangle(f64, f64),  // stores width, height
    Square(side: f64),    // named field style
}

let shape_of_wheel: Shape = Shape::Circle(25.12);
```
### Structs
``` rust
struct Person {
	name: str,
	age: u8,
	height: f16
}

let instance: Person =  {
	name = "Jason"
	age = "20"
	height = 1.81
}

```
### Generic Structs
``` rust
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
### Match
``` rust
match player_direction {
        Direction::Up    -> print("Player is moving up!")
        Direction::Down  -> print("Player is moving down!")
        Direction::Left  -> print("Player is moving left!")
        Direction::Right -> print("Player is moving right!")
}
```
### Arrays
``` rust
let myNumbers[4]: array<i16> = {25, 50, 75, 100};
```
### Vectors
``` rust
var myNumbers: Vec<i16> = alloc vec(i16);
```
### Varidadic Types
``` rust
fun sum(numbers: i32..) -> i32 {
    var total: i32 = 0
    for i in 0..numbers.len() {
        total = total + numbers[i]
    	}
    return total
}
let a: i32 = sum(10, 55, 28, 77)

/// ===== Variadic Generics =====
fun print_all<T>(numbers: T..) -> void {
     println(numbers)
}
print_all("11", 55, 'A', 26.05)
```
### Scoped Generics
``` rust
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

// Valid
let coordinates: Point<f64> = {
	x = 13.21,
	y = 89.50
}
```
### Pipe Operator |>
```
/// ===== Pipe Operator =====
var a: i32 = -28 |> increment() |> absolute() |> square_root();
```
### Traits
``` rust
/// ===== Traits =====
trait Printable{
	fun print(self) -> void;
	}
	
impl Printable for Person {
	fun print(self) -> void{
		println("Hi! My name is {self.name}, and I'm {self.age} years old");
	}
}

struct Person{
	name: str
	age: i8
	}
```
### Trait Bound Generics
``` rust
/// ===== Trait Bound Genric =====
trait Printable {
    fun print(self) -> void;
}

// generic function with trait bound
fun print_it<T: Printable>(x: T) -> void {
    x.print();
}
```
### Comptime
``` rust
/// ===== Comptime =====
// compile time cosntants:
const PI: f64 = 3.1415926535;
// comptime function execution:
const result: i32 = add(32, 54)
// or
const result: i32 = comptime add(35, 70)

// comptime function declaration:(all params must be known at comptime)
comptime fun ct_add<T>(x: T, y: T) -> T{
	return x + y
}
// compile time blocks
let my_variable: i32 = comptime{
	var a: u32 = 10;
	var b: i32 = 50;
	return a + b;
} 
```
### Inlining
``` rust
/// ===== Inlining =====
fun myFunc(x: i32, y: i32) -> i32 {
	// function body 
}
let func_result = inline myFunc(11, 14)

inline for i in array{
	i++
}
```
### Ranges
``` rust
for i in 1..10 {
   println(i) // will print 1-9
}

for i in 1...10 {
   println(i) // will print 1-10
}
```
### Assembly
``` rust
/// ===== Assembly =====
// simple asm
let y: i32 = asm { mov eax, 10; 
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
### Custom Allocator Piping
``` rust
let my_vector: Vec<i32> = new Vec(i32) // or Blank() for a clean slate
						|> Shrink_When_Free_Elements(10)
						|> Size_Limit(2048)
						|> Length_Limit(2048)
						|> Growth_Rate(1.5)
						|> lifo()
```
### Macros ( syntx WIP)
```
/// ==== defer macros ====
macro fun hello_macro(ast: tokens) -> tokens {
	val id: str = to_str(ast.id)
	impl HelloMacro for id{
		fun hello(){
			println(f"Hello, I am {id}")
		}
	}
}

macro fun clone_macro(ast: tokens) -> tokens {
    // Implement Clone trait
    implement Clone for ast.id {
        fun clone(&self) -> Self {
            // Struct literal braces
            ast.id {
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


/// ==== attribute macros ====
macro fun log_calls(attr: AttrArgs, item: FunDef) -> tokens {
		let level = match attr.level{
		    "trace" | "debug" | "info" | "warn" | "error" -> attr.level
		    _ -> "info"
    }

    // Function splicing with arguments and body
    fun {item.id}({item.args}) -> {item.return_type} {
        println(f"[{level}] Calling function {to_str(item.id)}")
        {item.body}
    }
}
@log_calls()
fun add(x: i32, y: i32) i32 {
		return x + y
}
```
  


















    

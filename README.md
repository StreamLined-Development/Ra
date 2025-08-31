### Variables
``` rust
// ==== Variables ====
var indentifier: type;
val dentifier: type;
const indentifier: type;

indentifier = value; // already declared above

var indentifier: type = value;
val indentifier: type = value;
const indentifier: type = value;

// inferred types
var indentifier = value;
val indentifier = value;
const indentifier = value;

// @Runtime val identifier: type = value; 

```
### Pointers
``` rust
var &indentifier: type;
let indentifier: &type;
indentifier = value; 
var indentifier: &type = value;
let &indentifier: type = value;
var mut indentifier: &type = value;
let mut &indentifier: type = value;
```
### Structs
``` rust
// ==== Stucts ====
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
### Enums
``` rust
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
### Generic Enums
``` rust
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

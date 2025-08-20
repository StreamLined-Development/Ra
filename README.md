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
- Aria models its binary in the form of a graph of independant computations and binary blocks. And with it's MLIR backend it allows you to taeget both GPU and CPU architectures!
- The compiler will detect what should be put on the cpu and what should be put on the gpu, and then mark the nodes accordingly for the runtime.

  ## Syntax
  - Variables 
  ```
  let my_variable: str = "hello world!" // immutable
  var my_variable: str = "hello world!" // mutable
  ```

  - Pointers
  ```
  var my_number_ptr: &i32 = my_number  // can be re-assigned but cannot mutate the value it points to
  var mut my_number_ptr: &i32 = my_number  // can be re-assigned and can mutate the value it points to
  let my_number_ptr: &i32 = my_number // cannot be re-assigned and cannot mutate the value it points to
  let mut my_number_ptr: &i32 = my_number  // cannot be re-assigned but can mutate the value it points to
  ```

  - Functions
  ```
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

  - Guards
  ```
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

  - Enums
  ```
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
  - Match
  ```
    match player_direction {
        Direction::Up    -> print("Player is moving up!")
        Direction::Down  -> print("Player is moving down!")
        Direction::Left  -> print("Player is moving left!")
        Direction::Right -> print("Player is moving right!")
    }
  ```
  




















    

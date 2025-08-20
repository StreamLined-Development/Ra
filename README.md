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

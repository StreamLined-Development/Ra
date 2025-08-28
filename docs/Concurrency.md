# Concurrency

- A **Process:**
    - It’s own Stack
    - The process is the unit of scheduling. The runtime can schedule processes across OS threads
    - Sending a message **transfers ownership** of the data into the recipient's mailbox arena.
    - Holding a PID is the capability to send messages to that process.
        - A process can never deallocate another process's PID or the memory in its message; it can only send a message.
    - An Arena as a Mailbox
        - When a message is sent, it is allocated directly into the recipient's mailbox arena. This is a very fast pointer-bump allocation.
        - If a process terminates, its **entire mailbox arena is deallocated instantly**. This is a massive performance win and eliminates any possible message leak.
        - The process is the unit of scheduling. The runtime can schedule processes across OS threads

### Create Process

```jsx
process Counter {
		var is_on: bool = false
		message turn_on() -> void {
        self.is_on = true;
				print("💡 The light is now ON");	
		}
		message turn_off() -> void {
        self.is_on = false;
        print("💡 The light is now OFF");
    }
    message check() -> bool {
        return self.is_on;
    }
}
```

### Spawn the Process

```tsx
let my_switch: $Switch = spawn Switch();
```

### Send Messages

```tsx
my_switch.turn_on();
my_switch.turn_off();
```

### Receive Messages

```tsx
let my_check: bool = my_switch.check();
// Either true or false
```

### Complete Example

```tsx
process Switch {
		var is_on: bool = false
}

impl Switch {
		message turn_on() {
        self.is_on = true;
				print("💡 The light is now ON");	
		}
		message turn_off() {
        self.is_on = false;
        print("💡 The light is now OFF");
    }
    message check() {
        return self.is_on;
    }
}

// ============================================================
fun main(){
		let my_switch: $Switch = spawn Switch();
		
		my_switch.turn_on();
		
		var my_check: bool = my_switch.check(); // true

		my_switch.turn_off();
		
		my_check: bool = my_switch.check(); // false
		
```

# Async

- One event loop per CPU core.
    - Each event loop is Eager to process messages.
- Sending a message is inherently async.
- To **wait for a result**, we use the `await` keyword

```tsx
my_switch.turn_off();
// will do other work while it waits
my_check: bool = await my_switch.check(); 

```

# Shared<$T>

### Create a shared PID

```tsx
var switch_ref: shared<$Switch>;
```

### Access the same message from 2 processes

```tsx
process Switch {
    var is_on: bool = false
}

impl Switch {
    message turn_on() {
        self.is_on = true;
        print("💡 The light is now ON");    
    }
    
    message turn_off() {
        self.is_on = false;
        print("💡 The light is now OFF");
    }
    
    message check() -> bool {
        return self.is_on;
    }
}

process SwitchController {
    var switch_ref: shared<$Switch>;
    
    message init(switch: shared<$Switch>) {
        self.switch_ref = switch;
    }
    
    message toggle() {
        let current_status = await &self.switch_ref.check();
        if current_status {
            async &self.switch_refturn_off();.
        } else {
            async &self.switch_ref.turn_on();
        }
    }
    
    message get_status() -> bool {
        return request &self.switch_ref.check();
    }
}

// ============================================================
fun main() {
    // Create the actual switch
    let my_switch: $Switch = spawn Switch();
    
    // Create shared reference to the switch
    let switch_ref = shared<$Switch>(my_switch);
    
    // Create multiple controllers sharing the same switch
    let controller1 = spawn SwitchController().init(&switch_ref);
    let controller2 = spawn SwitchController().init(&switch_ref);
    let controller3 = spawn SwitchController(.init(&switch_ref);
    
    print("Initial setup complete");
    print("All controllers share the same physical switch");
    
    // All controllers can operate the same switch
    async &controller1.toggle();  // Turns ON
    async &controller2.toggle();  // Turns OFF  
    async &controller3.toggle();  // Turns ON again
    
    // Check status through any controller
    let status = request &controller1.get_status(); // true
    print("Final switch status: ${status}");
}
```

# **Performance Comparison: Rust vs Ra vs Node.js**

## **1. Message Passing Cost**

| **Rust** | **Ra** | **Node.js** |
| --- | --- | --- |
| **Medium cost** | **Lowest cost** | **Highest cost** |
| Channels + synchronization | Arena allocation + pointer passing | Event queue + GC pressure |
| Atomic operations needed | Single producer, no locks | JavaScript object creation |

**Details**:

- **Rust**: **`Sender.send()`** → atomic ops + allocation + wakeup
- **Ra**: **`request pid: msg()`** → bump pointer + queue insert
- **Node.js**: **`queueMicrotask()`** → JS object + GC + event loop

## **2. Allocations**

| **Rust** | **Ra** | **Node.js** |
| --- | --- | --- |
| **Explicit control** | **Arena-optimized** | **GC overhead** |
| Manual allocation/free | Automatic arena allocation | Hidden allocations everywhere |
| Zero-cost possible | Near-zero-cost messages | High allocation rate |

**Details**:

- **Rust**: **`Box::new()`**, **`Vec::new()`** explicit, but optimal
- **Ra**: Arena bump allocator, no per-message allocs
- **Node.js**: Hidden allocations for closures, promises, objects

## **3. Message Batching**

| **Rust** | **Ra** | **Node.js** |
| --- | --- | --- |
| **Manual batching** | **Automatic batching** | **Limited batching** |
| Explicit batch APIs | Runtime can batch messages | Microtask queue only |
| Developer effort | Zero developer effort | No message batching |

**Details**:

- **Rust**: **`mpsc::Sender::send()`** per message, manual batching
- **Ra**: Runtime can process mailbox in batches
- **Node.js**: Single message per microtask, no batching

## **4. Syscall Batching**

| **Rust** | **Ra** | **Node.js** |
| --- | --- | --- |
| **Good batching** | **Best batching** | **Poor batching** |
| **`epoll`**/**`io_uring`** | Dedicated I/O dispatcher | Single-threaded **`epoll`** |
| Manual optimization | Automatic syscall coalescing | Limited by event loop |

**Details**:

- **Rust**: Tokio's I/O driver batches syscalls
- **Ra**: Dedicated thread can batch all I/O
- **Node.js**: Single **`epoll_wait`** per tick, limited batching

## **5. Cache Locality**

| **Rust** | **Ra** | **Node.js** |
| --- | --- | --- |
| **Excellent** | **Best** | **Poor** |
| Stack allocation | Arena allocation | Heap fragmentation |
| Struct-of-Arrays | Process-local memory | Object scattering |

**Details**:

- **Rust**: Cache-friendly with careful design
- **Ra**: Perfect locality - messages contiguous in arena
- **Node.js**: Poor locality - objects scattered by GC

## **6. Async Runtime Overhead**

| **Rust** | **Ra** | **Node.js** |
| --- | --- | --- |
| **Lowest overhead** | **Medium overhead** | **Highest overhead** |
| No runtime (library) | Lightweight scheduler | Heavy V8 runtime |
| ~100KB runtime | ~1-2MB runtime | ~10MB+ runtime |

**Details**:

- **Rust**: **`no_std`** possible, tiny runtime footprint
- **Ra**: Small runtime for scheduler + arenas
- **Node.js**: Massive V8 + libuv overhead
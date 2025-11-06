# FluxLang

FluxLang is a modern systems programming language that aims to blend Python-like flexibility with Rust-level performance and safety.

It supports two complementary execution models:
- JIT-interpreted: fast iteration and scripting-style workflows.
- Compiled: ahead-of-time (AOT) optimized binaries for deployment.

FluxLang’s core mission is to let developers prototype quickly, then compile those same programs into production-ready executables — without rewriting their codebase.

## Core Philosophy

Productivity and performance shouldn’t live in different worlds.

FluxLang seeks to combine the developer experience of high-level scripting with the control and performance of low-level systems languages.

| Design Area | FluxLang Goal |
|--------------|---------------|
| Typing | Optional static typing — `var` for dynamic, `let` for static |
| Execution | JIT for iteration, AOT compilation for release |
| Safety | Memory and ownership model inspired by Rust |
| Runtime | Compact, embeddable VM with optional native compilation |
| Syntax | Minimal, expressive, and consistent |

## Project Overview

FluxLang is structured as a Cargo workspace made up of modular crates:

| Crate | Purpose |
|-------|----------|
| core | Core types, memory model, and type system |
| vm | Virtual machine, JIT executor, and allocator integration |
| compiler | Parser, AST, and code generation (JIT + AOT) |
| runtime | Standard library and host integration layer |

Directory layout:

```
fluxlang/
├─ Cargo.toml        # workspace root
├─ core/
├─ vm/
├─ compiler/
└─ runtime/
```

Each layer builds on the previous — `vm` depends on `core`, `compiler` targets both, and `runtime` provides the external interface.

## Current Status

- [x] Workspace scaffolding and module layout  
- [x] Core memory allocator (Immix prototype)  
- [ ] AST and parser  
- [ ] VM bytecode interpreter  
- [ ] JIT + AOT backend  
- [ ] REPL and CLI tools  
- [ ] Standard library  

## Language Snapshot (Planned)

```flux
# dynamic typing
var name = "FluxLang"
print("Hello, " + name)

# static typing
let counter: int = 0
while counter < 10 {
    print(counter)
    counter += 1
}

# build targets
# flux run script.fx    → interpret with JIT
# flux build script.fx  → compile to native binary
```

## Vision

FluxLang isn’t just another toy language — it’s an experiment in merging two traditionally separate worlds:
- Dynamic scripting for iteration.
- Static compilation for performance.

### Long-term goals:
- Memory-safe at runtime and compile-time.  
- Gradual typing with runtime type feedback.  
- Immix-based GC + ownership semantics.  
- Optional integration hooks for Rust/C (embedding FluxLang or exposing native functions).  
- Modular backend design for LLVM or Cranelift.

## Building

```bash
git clone https://github.com/yourname/fluxlang
cd fluxlang
cargo build --workspace
cargo test
```

Once implemented, you’ll be able to run:
```bash
cargo run -p vm
```

## Development Roadmap

1. Core Language Infrastructure  
   - AST + tokenization  
   - Type system and error handling  

2. Execution Engine  
   - Stack-based VM  
   - Bytecode JIT  

3. Compiler Pipeline  
   - Parser → IR → native codegen  

4. Runtime + Tooling  
   - CLI (`flux`)  
   - Package system  
   - Standard library  

## License

MIT License © 2025 Brennan Woodbury

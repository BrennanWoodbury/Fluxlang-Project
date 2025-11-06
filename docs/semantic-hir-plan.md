# FluxLang Semantic Layer & HIR Plan

## Goals

- Establish a typed high-level IR (HIR) that preserves rich type information while remaining close to the source structure.
- Provide a clear separation between parsing, semantic analysis, and execution so downstream backends (VM, native codegen) share a common contract.
- Enable precise diagnostics (spans + error codes) during name resolution and type checking.
- Lay the groundwork for future optimisations without blocking early interpreter work.

## Scope

- Symbol table architecture (modules, functions, generics).
- Type checking strategy and type identity representation.
- AST to HIR lowering pipeline with error handling.
- Interface for feeding HIR into the upcoming interpreter/VM.
- Constant folding and simple evaluation passes for early optimisation and diagnostics.
- Debug-friendly HIR pretty-printing utilities.
- Memory management strategy (arena allocator) for HIR nodes and associated metadata.
- Out of scope: bytecode layout, low-level optimisations, and final code generation mechanics.

## Pipeline Overview

```
source text
  ↓
lexer (tokens)
  ↓
parser (AST)
  ↓
semantic analysis (name resolution + type checking)
  ↓
HIR (typed nodes + symbol bindings)
  ↓
execution backends (VM interpreter, future JIT/AOT)
```

## HIR Design Principles

- **Typed but high level**: retain constructs like loops, conditionals, and object literals; attach resolved types and symbol IDs to expressions and declarations.
- **Immutable structures**: prefer structs/enums with explicit IDs over mutating shared state; enable incremental compilation later.
- **Stable IDs**: assign numeric IDs (`SymbolId`, `TypeId`, `NodeId`) for cross referencing.
- **Span aware**: each HIR node carries a `Span` for diagnostics and tooling.
- **Arena allocated**: HIR nodes live in arenas to provide stable references during passes.
- **Inspectable**: provide pretty-printing/debug formatting hooks to visualise HIR for tooling and debugging.

## Module Layout Proposal

```
compiler/
  src/
    semantics/
      mod.rs
      hir/
        mod.rs
        nodes.rs        # HIR node definitions (items, statements, expressions)
        types.rs        # Type interning, type inference helpers
    arena.rs        # Arenas or bump allocators for HIR nodes
        ids.rs          # Strongly-typed newtype wrappers for IDs
    display.rs      # Pretty-printing / debugging utilities
      symbols/
        mod.rs          # Symbol table interfaces
        scope.rs        # Lexical scope management
        resolver.rs     # Name resolution logic
      typeck/
        mod.rs          # Type checker entry point
        constraints.rs  # Unification, obligation tracking
        diagnostics.rs  # Semantic error helpers
      eval/
        mod.rs          # Constant folding & simple evaluation pass
      lowering/
        mod.rs          # AST -> HIR lowering pipeline
```

Directories can land incrementally once individual components are ready.

## Symbol Table Architecture

- **Symbol kinds**: modules, functions, parameters, variables, types, interfaces, objects.
- **Scopes**: tree of lexical scopes backed by a stack; each scope stores a map from name to `SymbolId` plus metadata (visibility, mutability).
- **Modules**: top-level scope per file; track `use` imports and alias resolution.
- **Generics**: store parameter lists on symbols with constraints; maintain mapping from generic parameter name to index and associated bounds.
- **Resolution flow**:
  1. Pre-declare top-level items to allow forward references.
  2. Walk AST collecting symbols, pushing/popping scopes.
  3. Record unresolved references with their spans for defered diagnostics.

## Type System Outline

- Reuse `core::types::TypeId` for nominal types; introduce `TypeInfo` enum representing primitives, objects, interfaces, generics, tuples, arrays, maps, function types.
- Intern types through a `TypeTable` to avoid duplicates and enable equality by ID.
- Support gradual typing: `TypeId::any()` remains the fallback for unresolved or mixed numeric types.
- Type inference uses constraint solving with unification; start with straightforward rules (numeric promotions, string concatenation, boolean logic) and extend later.

## AST → HIR Lowering Steps

1. **Collection phase**: build module symbol table, register declarations, detect duplicates.
2. **Resolution phase**: resolve identifiers to symbols, annotate AST nodes with `SymbolId` references.
3. **Type checking phase**: walk expressions/statements computing types; emit diagnostics on mismatch.
4. **HIR construction**: emit typed nodes (e.g., `HirExpr::Binary { ty, lhs, rhs, op }`). Keep original ordering, attach spans, and store parent-child relationships via IDs.
5. **IR validation**: ensure HIR satisfies invariants (no unresolved symbols, types assigned to each expression) before handing off.
6. **Constant folding (optional pass)**: run a lightweight evaluator over pure expressions to fold constants and annotate values for interpreter optimisation.
7. **Debug output**: expose pretty-printers for HIR modules to assist tracing and regression testing.

## Diagnostics Strategy

- Extend `ParseErrorCode` style with `SemanticErrorCode` covering unresolved name, type mismatch, trait obligations, visibility violations.
- Diagnostics carry `Span`, offending symbol/type info, and suggested fixes when possible.
- Preserve multiple diagnostics per pass to avoid early bail-outs.
- When a variable is declared with `let` (dynamic), unresolved type inference may fall back to `any`; otherwise prefer emitting diagnostics demanding annotations.

## Execution Interface

- Expose HIR via `compiler::semantics::hir::Module` containing item list, symbol tables, and type tables.
- Provide iterator or visitor helpers for the VM to walk functions and expressions.
- VM can interpret directly from HIR initially (no bytecode) by pattern matching on expression kinds.
- Trait implementations stored in a dedicated table mapping `(TypeId, InterfaceId)` to method sets to simplify dispatch.

## Implementation Phases

1. **Scaffolding**
   - Create modules (`hir`, `symbols`, `typeck`, `lowering`).
   - Define ID types and basic data structures (type table, symbol table entries).
  - Implement arena allocator(s) for storing HIR nodes and associated metadata.
2. **Name Resolution MVP**
   - Collect top-level symbols, implement scope stack, resolve identifiers within functions.
   - Emit diagnostics for unresolved names and duplicate definitions.
3. **Type Checking MVP**
   - Implement primitive type inference rules and arithmetic compatibility.
   - Handle control flow constructs (`if`, loops) with block scopes.
  - Respect dynamic typing fallback only for `let` bindings; otherwise emit diagnostics.
4. **HIR Construction**
   - Define `HirItem`, `HirStmt`, `HirExpr` with attached types/symbol IDs.
   - Lower AST nodes using resolved symbol and type information.
  - Populate trait implementation table `(TypeId, InterfaceId) → method list` as part of module metadata.
  - Add pretty-printing hooks for modules and expressions.
5. **VM Integration Hooks**
   - Provide a trait or API for the interpreter to evaluate HIR nodes.
  - Add smoke tests executing simple programs through lexer → parser → semantics → interpreter stubs.
  - Ensure minimal control-flow support (`if`, `loop`, `return`, `block`) matches VM bootstrap needs.
6. **Diagnostics & Regression Tests**
   - Add semantic error tests using fixture inputs.
   - Document regression expectations and wire into CI once available.
7. **Constant Folding & Debugging Tools**
  - Implement constant folding pass; add tests verifying folded expressions.
  - Create pretty-printer output comparisons to aid regression testing.

## Open Questions

- How should interface/trait implementations be represented in HIR for future dispatch? → Store a dedicated trait implementation table mapping `(TypeId, InterfaceId)` to method sets.
- What is the minimal set of control flow constructs the interpreter must support before introducing bytecode? → Support `if`, `loop`, `return`, and block constructs for bootstrap.
- Do we need an intermediate representation for patterns/destructuring now, or defer until pattern matching lands? → Defer; treat tuple destructuring (e.g., `let (x, y) = ...`) as syntactic sugar lowered to tuple indexing later.
- Should type inference default to `any` in ambiguous cases, or emit a diagnostic demanding explicit annotation? → Emit diagnostics first; fall back to `any` only when the user explicitly declares a `let` binding (dynamic).

## Next Steps

- Review and refine this plan with the team.
- Update `todo.md` subtasks to reflect concrete implementation checkpoints (already linked).
- Begin scaffolding modules and defining ID/type tables as part of Phase 1.

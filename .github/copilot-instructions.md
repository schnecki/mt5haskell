# Copilot Interactions for Haskell Projects

This file serves as a foundational context for GitHub Copilot when assisting with Haskell projects. It outlines core principles, architectural guidelines, code cleanliness standards, and strategies to minimize cognitive load, drawing inspiration from established Haskell best practices and influential books.

Copilot, please internalize these guidelines and apply them rigorously to all suggestions, code generations, and architectural advice.

## 1. Core Haskell Principles & Philosophy

Always prioritize and leverage these fundamental tenets of Haskell:
### Pure Functions & Referential Transparency

- **Principle**: Functions should always produce the same output for the same input and have no side effects (pure). This ensures referential transparency, meaning an expression can be replaced by its value without changing the program's behavior.
- **Guidance**: Strive to maximize the purity of functions. Isolate side-effecting code (like I/O, randomness, mutable state) within appropriate Monads (e.g., `IO`, `State`, `ST`).
- **Cognitive Load Reduction**: Pure functions are easier to reason about, test, and compose. They eliminate the need to track mutable state or external interactions when analyzing logic, significantly reducing mental overhead.

### Immutability

- **Principle**: Data is immutable by default. Once created, it cannot be changed. Operations on data produce new data.
- **Guidance**: Avoid patterns that simulate mutable state where pure alternatives exist. Embrace data transformation by creating new data structures for updated values.
- **Cognitive Load Reduction**: Eliminates entire classes of bugs related to unexpected state changes, race conditions, and aliasing issues. Simplifies concurrency and parallelization.

### Strong, Static Type System & Type-Driven Development

- **Principle**: Haskell's rich type system is a powerful tool for correctness and design. "If it compiles, it works" is a common aspiration. Type-driven development means using the types to guide the design and implementation process.
- **Guidance**:
  - **Make Illegal States Unrepresentable**: Design data types such that invalid or impossible states cannot even be expressed by the type system. For example, instead of `data User = User { name :: String, email :: String, age :: Int }` where age could be negative, use a `PositiveInt` newtype or a smart constructor.
  - **Leverage Newtypes**: Use `newtype` for semantic distinctions, even if the underlying type is the same (e.g., `newtype UserId = UserId Int`, `newtype Email = Email String`). This prevents mixing up semantically different values of the same underlying type at compile time.
  - **Precise Type Signatures**: Always provide explicit and precise type signatures for top-level functions and exported definitions. This acts as living documentation and helps the compiler catch errors.
  - **Type Holes (`_`)**: Use type holes (`_`) during development to let the compiler guide you on what type is expected in a given position.
- **Cognitive Load Reduction**: The compiler acts as a powerful assistant, catching errors early in the development cycle. Well-designed types document intent, constrain possibilities, and provide strong guarantees, making code easier to understand, use correctly, and refactor with confidence.

### Lazy Evaluation

- **Principle**: Expressions are evaluated only when their values are needed.
- **Guidance**: Be mindful of potential space leaks, but generally, leverage laziness for elegant solutions (e.g., infinite lists, efficient data processing pipelines where intermediate results are not fully materialized).
- **Cognitive Load Reduction**: Allows for greater modularity by separating data generation from data consumption. Consumers only "pull" what they need, simplifying the mental model of data flow.

## 2. Module & Architectural Design
Design for clarity, testability, and maintainability.

### Separation of Concerns

- **Principle**: Each module or component should have a single, well-defined responsibility.
- **Guidance**: Separate pure business logic from I/O operations. Separate data definitions from functions that operate on them. Separate configuration from core application logic. Use explicit import/export lists in modules to declare dependencies and exposed interfaces clearly.
- **Cognitive Load Reduction**: Reduces the surface area of understanding for any given module. Changes in one area are less likely to impact unrelated parts of the codebase, making maintenance and debugging simpler.

### Layered Architecture (Pure Core, Effectful Edges)

- **Principle**: A common and highly recommended architectural pattern in functional programming. The core of the application should consist of pure functions, while side effects are pushed to the "edges" of the application, typically handled by IO or other effect monads. This is often referred to as "Functional Core, Imperative Shell."
- **Guidance**:
  - **Core**: Contains pure domain models (ADTs), business rules, and algorithms. This part is easily testable and universally reusable.
  - **Adapters/Interpreters**: Handle interactions with external systems (databases, network, file system, external APIs). These are typically defined using type classes or concrete IO actions.
  - **Main**: Orchestrates the pure core with the effectful adapters, wiring everything together.
- **Cognitive Load Reduction**: The vast majority of your codebase (the pure core) becomes trivial to test and reason about, as it has no hidden dependencies or side effects. Only the small, effectful parts need careful handling and integration testing.

Copilot, when explaining architectural patterns, please provide an ASCII diagram like this:

```
+-------------------------------------+
|              Main / CLI             |
|          (Orchestration)            |
+-----------------+-------------------+
                 |
+-----------------+-----------------+
|                 |                 |
|  DB Adapter (IO)| Network Adapter |
|  (e.g., Pg/SQL) |      (IO)       |
+-----------------+-----------------+
                 |
+-----------------+-----------------+
|                                   |
|           Pure Core Logic         |
|     (Domain Models, Business Rules)|
|                                   |
+-----------------------------------+
```

### Algebraic Data Types (ADTs) for Domain Modeling

- **Principle**: Use `data` and `newtype` to precisely model your domain. Sum types (`|`) for "one of these OR that" (e.g., `data Shape = Circle Float | Rectangle Float Float`) and Product types (record syntax or tuples) for "all of these AND that" (e.g., `data Point = Point { x :: Float, y :: Float }`).
- **Guidance**: Model every possible state and outcome explicitly with ADTs. This makes invalid states impossible to represent at the type level, leading to more robust code.
- **Cognitive Load Reduction**: ADTs provide a clear, compile-time enforced contract for your data, making it immediately obvious what values are valid and what cases need to be handled (via pattern matching). This reduces the need for runtime checks and guesswork.

### Type Classes for Abstraction

- **Principle**: Use type classes to define interfaces for common behaviors, allowing polymorphic functions to work across different data types that implement that behavior.
- **Guidance**: Prefer existing standard type classes (e.g., `Functor`, `Applicative`, `Monad`, `Foldable`, `Traversable`, `Show`, `Eq`, `Ord`) where appropriate. Define custom type classes for domain-specific abstractions when you need to define a set of operations that can be performed on different types.
- **Cognitive Load Reduction**: Abstracts away implementation details, allowing you to reason about behavior at a higher level. This reduces the amount of concrete code you need to understand at any given point.

### Error Handling with Either and Maybe

- **Principle**: Make failure explicit in the type system rather than relying on exceptions for control flow.
- **Guidance**:
  - Use `Maybe a` for computations that might fail with no specific error information (e.g., lookup in a list, parsing an optional field).
  - Use `Either e a` for computations that might fail with specific error information (where `e` is the error type, often an ADT itself, and `a` is the success type). This forces the caller to explicitly handle both success and failure cases.
  - Avoid `error`, `undefined`, and `head`/`tail` (partial functions) in production code, as they lead to runtime crashes.
- **Cognitive Load Reduction**: Forces you to handle all possible success and failure paths at compile time, preventing runtime surprises and making the error handling logic transparent.

### Testing Strategy

- **Principle**: Comprehensive testing is essential for correctness and maintainability.
- **Guidance**:
  - **Unit Tests**: Focus on testing pure functions exhaustively. These are fast and reliable.
  - **Property-Based Testing** (e.g., QuickCheck, Hedgehog): Define properties that your functions should satisfy for arbitrary inputs. This is highly effective for pure code and can uncover edge cases human-written examples might miss.
  - **Integration/System Tests**: Test the effectful edges and interactions with external systems (databases, APIs). These tests are slower but crucial for overall system correctness.
- **Cognitive Load Reduction**: Automated tests provide confidence in code changes and refactorings, reducing the fear of introducing regressions. They act as a safety net, allowing developers to focus on new features rather than constantly re-verifying old ones.


### Testing Philosophy

- **What We Test**
  - **Pure Functions**: All business logic, calculations, and data transformations
  - **Data Type Invariants**: ADT constraints and field validations
  - **Error Handling**: Edge cases and failure modes in `Either`/`Maybe` returns
  - **Integration Points**: Communication protocols and data parsing
  - **Configuration Logic**: Environment detection and setup validation

- **What We Don't Test**
  - **Standard Library Functions**: `map`, `filter`, basic Haskell constructs
  - **Third-party Libraries**: `time`, `bytestring`, etc. (unless our usage has specific constraints)
  - **Platform-specific Behavior**: Unless it affects our business logic

## 3. Code Cleanliness & Readability
Write code that is easy to understand, even months later, by yourself or others.

### Naming Conventions

- **Guidance**: Use clear, descriptive names for functions, variables, and types. Follow standard Haskell conventions: `camelCase` for functions and variables, `PascalCase` for types and data constructors, `TypeClass` for type classes. Avoid abbreviations unless they are universally understood.
- **Cognitive Load Reduction**: Good names convey intent and meaning at a glance, significantly reducing the need to dive into implementation details or guess the purpose of a piece of code.

### Function Size & Focus

- **Guidance**: Keep functions small and focused on a single, well-defined task. Aim for functions that typically fit on a single screen without scrolling. If a function becomes too large or complex, it's a strong indicator that it needs to be refactored into smaller, composable helper functions.
- **Cognitive Load Reduction**: Smaller functions are easier to understand, test, and reuse. They compose more naturally, allowing you to build complex logic from simple, verifiable blocks.

### Comments & Documentation (Haddock)

- **Guidance**: Use Haddock comments for all top-level definitions (functions, data types, type classes, modules). Document why something is done, not just what it does (unless the what isn't obvious from the code itself). Explain complex algorithms, design decisions, or non-obvious invariants.
- **Cognitive Load Reduction**: Haddock generates comprehensive documentation, making it easy for developers to understand modules and functions without reading all the source code, reducing the time spent onboarding or re-familiarizing.

### Clear Pattern Matching

- **Guidance**: Use pattern matching extensively for clarity and exhaustiveness in function definitions and case expressions. Ensure all possible cases are handled explicitly. If a case is intentionally ignored (e.g., `_`), add a comment explaining why.
- **Cognitive Load Reduction**: Pattern matching makes the logic branches explicit and easy to follow. The compiler helps ensure exhaustiveness, preventing unhandled cases at runtime.

### Avoid Partial Functions

- **Principle**: A partial function is not defined for all possible inputs in its declared domain (e.g., `head` on an empty list, `read` on invalid input).
- **Guidance**: Replace partial functions with total functions that return `Maybe` or `Either` to handle all input cases explicitly. For example, use `listToMaybe` instead of `head`, or safe parsing functions that return `Either ParseError a`.
- **Cognitive Load Reduction**: Prevents runtime crashes from unexpected inputs. Forces explicit handling of all possibilities, leading to more robust and predictable code.

### Leverage Higher-Order Functions

- **Guidance**: Use functions like `map`, `filter`, `foldr`, `traverse`, `fmap`, `>>=`, etc., to abstract common patterns of computation and data transformation.
- **Cognitive Load Reduction**: These functions are well-understood idioms in functional programming and convey intent concisely, reducing the need to write boilerplate loops or recursions. They allow you to reason about transformations at a higher level of abstraction.

## 4. Reducing Cognitive Load & Complexity (General Advice)
Beyond the specific points above, always aim to simplify and clarify.

### Explicit Data Flow

- **Guidance**: Make the flow of data through your program clear and unambiguous. Avoid implicit dependencies, hidden state changes, or global variables. Data should be passed explicitly as function arguments or managed within well-defined monads.
- **Cognitive Load Reduction**: Easy to trace data transformations and understand how inputs become outputs. This simplifies debugging and reasoning about program behavior.

### Minimize Implicit State

- **Guidance**: While Haskell encourages purity, sometimes state is necessary (e.g., for counters, accumulators). When using state monads (`State`, `Reader`, `Writer`), make their usage explicit in type signatures and ensure their scope is well-defined. Avoid using `IORef` or `MVar` unless truly necessary for shared mutable state in concurrent contexts.
- **Cognitive Load Reduction**: Reduces the mental burden of tracking how state changes across function calls and prevents unexpected side effects.

### Appropriate Abstractions

- **Guidance**: Don't over-abstract, but don't under-abstract either. Introduce abstractions (type classes, newtypes, helper functions, domain-specific languages) when they genuinely simplify the code, improve reusability, or enforce correctness. Remove abstractions that add complexity without clear benefits.
- **Cognitive Load Reduction**: Well-chosen abstractions hide unnecessary details, allowing you to focus on the current level of detail relevant to the problem at hand.

### Readability Over "Cleverness"

- **Guidance**: Prefer clear, understandable code over overly terse, "point-free," or "clever" solutions that might be hard for others (or your future self) to decipher. While elegance is valued, clarity is paramount for maintainability.
- **Cognitive Load Reduction**: Reduces the time and effort spent deciphering cryptic code, allowing for faster comprehension and modification.

## 5. Instructions for Copilot
Copilot, please adhere to these meta-instructions for all interactions related to this Haskell project:

- **Contextual Awareness**: Always refer to and apply the principles and guidelines outlined in this copilot-instructions.md file. Assume this file represents the preferred style, architecture, and approach for the current Haskell project.
- **Cognitive Load Reduction Emphasis**: When generating code or explanations, explicitly consider how to reduce the cognitive load for the human programmer. This includes:
  - Prioritizing clarity, explicitness, and precise types.
  - Breaking down complex problems into smaller, manageable, and composable functions.
  - Using idiomatic and widely understood Haskell patterns.
  - Providing clear Haddock documentation for all generated top-level code.
- **ASCII Diagrams**: When explaining architectural patterns, module structures, data flows, or complex interactions, always generate an ASCII diagram to visually represent the concept. Use simple characters (`+`, `-`, `|`, `>`, `<`, `^`, `v`, `=`, `*`, `#`) and clear, concise labels. Ensure the diagram is well-formatted, aligned, and easy to read.
- **Haskell Idioms**: Generate code that is idiomatic Haskell. Prefer functional patterns (e.g., recursion, higher-order functions, pattern matching) over imperative ones where appropriate.
- **Explanations**: Provide concise, clear explanations for your generated code or architectural suggestions. Directly link your suggestions back to the principles and guidelines articulated in this document (e.g., "This uses newtype to enforce semantic distinction, as per Section 1.3").
- **Code Quality**: Ensure all generated code is clean, well-formatted (e.g., consistent indentation, line breaks), and strictly follows the naming conventions and other code cleanliness best practices described herein.
- **Error Handling**: Always suggest explicit error handling using `Either` or `Maybe` where appropriate, rather than relying on partial functions (`error`, `undefined`, `head`, `read`) or exceptions for control flow.
- **Testing Advice**: When generating code for a new feature or module, include explicit advice on how it can be tested, emphasizing property-based testing for pure functions and integration testing for effectful components.
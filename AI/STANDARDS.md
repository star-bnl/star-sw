# STAR Coding Standards (Summary)

Supported: c++11, FORTRAN 77, python 2.7, perl 5, XML.

Follow best practices unless overridden below. Formatting style not mandated except for fixed-format (FORTRAN77, python). Unit tests may deviate.

## File/Class Naming [c++]
* `.h` for headers, `.cxx` for implementations.
* One class/struct or related group per header.
* Classes start with `St`, prefer CamelCase (legacy snake_case allowed).

## Headers [c++]
* Each implementation has a header.
* Use include guards: `__HEADER_BASE_NAME_H__`.
* Define functions in implementation files, except short `inline` after class.
* Don't inline virtuals.
* Use angle brackets for external, quotes for project headers.
* Minimize includes; use forward declarations.

## Namespaces [c++]
* Avoid namespaces; use file/class naming to prevent collisions.

## Scoping [general]
* Declare/init variables locally.
* Prefer brace init (except single-arg assignment) [c++].
* Never brace-init with `auto` [c++].
* No global vars [c++]; static class/namespace vars discouraged.
* If globals, initialize statically.

## Classes [c++]
* Require default ctor/dtor (base dtor virtual if public).
* Init all data members.
* Don't call virtuals in ctors/dtors.
* Implement/delete assignment/copy ctors (compiler defaults OK if no heap).
* Use delegating/inheriting ctors to avoid duplication.
* Use struct only for POD.
* Only public inheritance.
* Mark overrides as `override` or `final`.
* Prefer composition over inheritance.
* Only one concrete base; others must be pure interfaces (no concrete methods).
* Preserve operator semantics.
* Explicitly use public/protected/private; list public first.
* Hide internals; don't return handles to internal data.
* Avoid `friend`.

## ROOT [c++]
* Prefer C++ types over ROOT types, except persistent classes.
* Prefer `<cmath>` over ROOT math.

## Introspection [c++]
* Prefer `auto` over `decltype`.
* Avoid macros.
* Avoid dynamic_cast except for error detection.

## General
* Avoid magic numbers.
* Keep functions small/focused.
* Avoid exceptions/asserts unless necessary [c++].
* Don't violate these conventions in recommendations.

## Miscellaneous [c++]
* Prefer strong enums.
* Avoid lambdas for frequent calls or reference capture.
* Use `const` for logically constant objects; design const-correct interfaces.
* Use `constexpr` for true constants or constant init.
* Avoid smart pointers (ROOT ownership).
* Avoid casting; use `static_cast` if needed, avoid `const_cast`, `reinterpret_cast`, and C-casts.
* No variable-length arrays or `alloca()`.
* Prefer prefix ++/--.
* Switch: cover all cases, use default if needed.
* No empty loop bodies; use `continue` or empty block.
* Declare iteration vars in loop conditions.
* Use range-for and (const) reference where possible.
* Prefer iterator if index needed.
* Use `int` unless fixed size needed (`<cstdint>`).
* Ensure portability (printing, comparison, alignment).
* Use `nullptr` for pointers.
* Prefer `sizeof(var)` to `sizeof(type)`.
* Use `auto` for local vars if it improves readability.
* Use non-member `begin()`/`end()`.

## Non-Conformant Code
* Deviations allowed for legacy code.
* Refactor incrementally, one deviation at a time, preserving behavior.

## Git Recommendations
* Add recommendations to git, but do not commit.


# Instructions for Code Reviews

You will analyze the files which have been added and/or changed in this pull request.  You will consider correctness,
best practice, and the Coding Standards provided below.  Where there are
conflicts between best practice and the Coding Standards, you will use the Coding Standards.

These standards represent guidelines.  Deviations from them are permitted, if there is a reasonable
justification provided in comments.  Such deviations should only be permitted if it makes the code
easier to understand.

You will diligently analyze each of the files which are touched by the pull request.  You will review each
file in its entirety, not just the changes submitted since the last commit.

You will consider every bullet point in the Coding Standards.  You will note all discrepancies in your review.
You will provide the minimal set of changes which will bring the code into compliance with best practice and
the Coding Standards.  Your proposed changes should conform to best practice and the Coding Standards.

For each change you propose, you should explain why you are proposing that change.  Be concise in your reponse.

You will create a table summarizing the code review.  For each file touched in the pull request, explicitly state the nature
of the changes which were made, whether they conform to best practice, and whether they conform to the Coding
Standards enumerated below.  Where they fail the coding standards, indicate each of the standards which were violated.

Recommended changes should always follow the Coding Standards.

# Coding Standards

Applicable languages: c++11, C99, FORTRAN 77, python 2.7, perl 5, XML.

Where guidance applies to a specific language, it will be noted in square brackets.  e.g. [c++].

Follow the principle of least surprise.  Software and interfaces should behave in a way that is most predictable and intuitive for developers
and users, minimizing unexpected or surprising actions.

Formatting style not mandated except for fixed-format (FORTRAN77, python).

Follow best practices unless overridden below. 

Unit tests may deviate from these conventions.

## File/Class Naming [c++]
* `.h` for headers, `.cxx` for implementations.
* One class/struct or related group per header.
* Classes start with `St`, prefer CamelCase (legacy snake_case allowed).

## Headers [c++]
* Each implementation has a header.
* Use include guards: `__FILEBASENAME_H__`.
* Define functions in implementation files, except short `inline` after class.
* Don't inline virtuals.
* Use angle brackets for external, quotes for project headers.
* Use only necessary includes; minimize dependencies with forward declarations where appropriate.

## Namespaces [c++]
* Avoid namespaces in STAR; use STAR file/class naming conventions to prevent collisions.

## Scoping [general]
* Declare variables in narrowest scope possible.
* Init all variables.
* Prefer brace initialization (except single-argument assignment) [c++].
* Never brace-initialize with `auto` [c++].
* No global vars; static class/namespace variables are discouraged. [c++]
* If global variables are necessary, initialize them statically.


## Classes [c++]
* Every class must declare at least one ctor, even if defaulted with `= default;`.
* Every class must declare a ctor which can be called with no parameters.  A ctor with all default parameters is allowed.
* Classes which allocate memory and retain ownership should deallocate memory in the dtor.
* Initialize all data members.
* Don't call virtuals in ctors/dtors.
* Implement/delete assignment/copy ctors (compiler defaults OK if no heap).
* Use delegating/inheriting ctors to avoid duplication.
* Use `struct` only for plain-old-data (POD) types.
* Prefer composition over inheritance.
* Classes which use inheritance should only use public inheritance
* Only one concrete base; others must be pure interfaces (no concrete methods).
* Mark overrides as `override` or `final`.
* Follow the principle of least surprise: preserve operator semantics.
* Explicitly use public/protected/private; list public first.
* Hide internals; don't return handles to internal data.
* Avoid `friend`.

## ROOT [c++]
* Prefer C++ types over ROOT types, except persistent classes.
** e.g. prefer int, float, double, char, bool over Int_t, Float_t, Double_t, Char_t, Bool_t.
* Prefer `<cmath>` over ROOT math (TMath::*).

## Introspection [c++]
* Prefer `auto` over `decltype`.
* Avoid macros.
* Avoid dynamic_cast except for error detection.

## General
* Avoid magic numbers.
* Keep functions small/focused.
* Avoid exceptions/asserts unless necessary [c++].

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






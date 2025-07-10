#Language Standards, Styles and Practices

Codes should follow the language standards noted below.  In FORtran code, some modern FORtran features are allowed.

* c++11
* FORtran 77
* python 2.7
* perl 5
* XML

STAR codes should follow generally accepted best practices for the language they are implemented in, unless the coding standards
below state otherwise.

Except for fixed-format languages (such as FORtran77 and python) there are no guidlines expressed or implied on the formatting style
of the source codes.

Unit tests can deviate from these guidelines where necessary.

Each heading below denotes a category of related coding conventions.  If it is specific to a language it will indicate this by including the
language in square brackets, e.g. [c++].  If an individual item in a category marked [general] is specific to a given language, it will be
marked with the same square bracket label.

#File Naming Convention [c++]

* Header files have the extension ".h".  
* Implementation files have the extension ".cxx".  
* Each header should define one class or structure, or multiple closely related classes and/or structures.

#Class Naming Convention [c++]

* Classes should begin with the two letters 'St'.
* Camel Case is strongly preferred. Legacy codes may continue to use snake case.

#Header files [c++]

* Implementation files should have a corresponding header file.
* Each header file has an include guard of the form __HEADER_BASE_NAME_H__.
* Function definitions should appear in the implementation file.
* Short, compact member functions can be put in the header file after the class declaration preceded by the inline keyword.
* Avoid inlining virtual function.
* External packages are included using angle brackets, e.g. #include <externalheader.h>
* Project headers are included using quotes, e.g. #include "myheader.h"
* Avoid unecessary #include statements with forward declarations
* Include all headers necessary to define the class, but otherwise minimize dependencies.

#Namespaces [c++]

* Namespaces are good practice in C++, but discouraged in STAR code because it may break packages in our toolchain.
* The file and class naming conventions serve to avoid namespace collisions.

#Scoping [general]

* Declare variables as locally as possible.
* Always initialize variables.
* Prefer initialization with braces except for single-argument assignment. [c++]
* Never use brace initialization with auto. [c++]
* Variables declared in the global scope are not allowed. [c++]
* Static class variables and variables in namespace scope, should be avoided where other means of communication are possible.  [c++]
* In the rare case where you use global variables, initialize them statically [c++]

#Classes [c++]

* Every class should have a default constructor
* Each class data member must be initialized during construction.
* Do not call virtual functions in constructors and destructors.
* Each class should have an assignment operator and a copy constructor implemented or explicitly deleted.
  - Compiler defaults are permitted when the class doesn't allocate data structures on the heap
  - Be aware of data slicing for polymorphic classes.
* Use delegating and inheriting constructors to avoid duplication in initialization logic.
* Every class must have a destructor to free resource that it allocated.
* The base class destructors must be declared virtual if they are public.
* Use a struct only for plain-old-data.  Everything else is a class.
* Inheritance should always be public.  
* Declare overridden methods as override or final.
* Consider composition over inheritance to utilize functionality implemented in another class.
* Multiple inheritence is discouraged.  One one concrete base class is allowed.  All others must be pure interfaces.
* Adding concrete methods to a pure interface in STAR is not allowed.
* Preserve semantics when overloading operators.
* public, protected and private keywords must be explicitly used.
* List public methods and members first.
* Hide internals. Avoid returning handles to internal data managed by your class.
* Avoid friend declarations.

#ROOT considerations [c++]

The STAR software makes extensive use of the ROOT analysis framework.

* Use fundamental types provided by the C++ language over ROOT types, except in persistent classes that are subject to schema evolution.
* Prefer mathematical functions provided by the C++ standard (<cmath>) over those provided by ROOT (e.g. TMath).

#Introspection [c++]

* Do not use decltype if you can use auto. 
* Avoid preprocessor macros.
* Branching based on dynamic_cast, except as error detection, may indicate a need to redesign your class.

#General Guidelines

* Avoid hard coded numbers.
* Prefer small and focused functions.
* Do not use C++ exception handling or asserts in STAR code unless absolutely necessary. [c++]
* Recommendations should not themselves violate the coding conventions.

#Miscellaneous [c++]

* Prefer strong enums over weak ones.
* Lambda functions
  - avoid replacing regular functions where frequent calls might incur performance penalties
  - avoid using capture by reference
* Declare objects that are logically constant as const. Design const-correct interfaces. Consider constexpr for some uses of const.
* Use constexpr to define true constants or to ensure constant initialization. When used with functions you should also declare them const for forward consistency.
* Avoid using smart pointers in STAR code as they complicate ROOT object ownership.
* Avoid designs that require casting. You may use static_cast when necessary, but avoid const_cast and reinterpret_cast. C-casts should be avoided and are highly discouraged.
* Don't use variable-length arrays or alloca().
* Wherever applicable, prefer the prefix form of the increment (++i) and decrement (--i) operators because it has simpler semantics.
* Switch statements should cover all cases.  Use a default case where needed to achieve this.
* Loops should not have empty bodies, such as `while (++count < limit);`.  Use a continue statement or empty body.
* Declare iteration variables within for and while statement conditions.
* Use range-for loops where possible.
* Use (const) reference to elements in range-for statements where possible.
* Prefer an iterator in for loops if the index is needed.
* Use int if you need an integer type. If you need to guarantee a specific size use the new extended integer types defined in <cstdint>.  
* Take extra care of the code portability. Bear in mind problems of printing, comparisons, and structure alignment related to 32-bit and 64-bit data representations .
* Use nullptr when zeroing out a pointer.
* Prefer sizeof(varname) to sizeof(type).
* Use auto where it improves code readability.  Avoid where it makes the code less understandable.

* Use auto if:
  - variables are local
  - code becomes more readable
* Avoid auto when:
  - the type of object is important to know
  - the rvalue is a basic type

* The non-member begin() and end() functions are encouraged.

#Existing Non-Conformant Code [general]

* It is permissible to deviate from the rules when dealing with code that does not conform to these guidelines.
* When asked, provide incremental refactoring advice which addresses a single deviation and is provably invariant.


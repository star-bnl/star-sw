#Naming

The most important consistency rules are those that govern naming. The style of a name immediately informs us what sort of thing the named entity is: a type, a variable, a function, a macro, etc., without requiring us to search for the declaration of that entity. The pattern-matching engine in our brains relies a great deal on these naming rules.

Naming rules are pretty arbitrary, but we feel that consistency is more important than individual preferences in this area, so regardless of whether you find them sensible or not, the rules are the rules.

##General Naming Rules
* Names should be meaningful; abbreviations should be avoided. They follow camel case convention. Types and variables, as well as access functions should be nouns, while functions should be "command" verbs.

##Specific STAR Naming Rules
* Each class name should begin with St and the appropriate three letter acronym (only first letter capitalized) to indicate its origin and prevent name clashes.

##File Names
* C++ code file names are derived from the class names. Following STAR's naming convention they hence always start with St. Each header file should contain only one or related class declarations for maintainability and for easier retrieval of class definitions. Files that contain only function code (e.g. Maker) and are unrelated to a specific class should start also with St followed by a descriptive name of the purpose of the code.
##Type Names
* Type names follow camel case convention and start with an upper case letter: MyClass, MyEnum.
##Variable Names
* Variable names follow camel case convention and start with a lower case letter: myLocalVariable.

    Class member variables are prefixed with m.
    No m prefix for struct members.
    Global variables are prefixed with g.
    constexpr variables are capitalized.
    No additional prefix for const.

##Function Names
* Regular functions follow camel case convention and start with a lower case letter: myFunction().

    Accessors and mutators match the name of the variable. Accessors should follow the name of the variable (DO NOT prefix it with get) and mutators are prefixed with set, for example: myMemberVariable(), setMyMemberVariable().
    Functions (including accessors) returning a boolean value should be prefixed with is or has.

##Namespace Names
* Namespace names follow camel case convention and start with an upper case letter: MyNamespace.
##Enumerator Names
* Enumerations and enumerators (the type and the values) follow camel case convention and start with St followed by an upper case letter: StBeamPolarizationAxis, StDetectorId.

    Enumerators in unscoped enumerations should have a common prefix/postfix derived from the enumerations name.
    Enum classes are already scoped and therefore the enumerators do not need a prefix/postfix.

##Macro Names
* All uppercase letters and underscores, prefixed with ST (for STAR) and the sub/project name, i.e. ST_PROJECT_PKG1_MY_MACRO.
STAR StRoot Makers Naming Standards
* This section attempts to explain the code directory structure and layout in STAR the rules and assumptions triggered in the make system (cons) solely on the basis of the name choice the existing exceptions to the rules.

#Formatting

Coding style and formatting are pretty arbitrary. However, a good project is much easier to follow if everyone uses the same style. Individuals may not agree with every aspect of the formatting rules, and some of the rules may be hard to get used to. Even so, it is important that all project contributors follow the style rules so that they can all read and understand everyone's code easily.

##Line Length
* Each line of text in your code should be at most 100 characters long.
##One Statement Per Line
* Prefer one statement per line because it improves code readability.
## Spaces vs. Tabs
* Indent code with at least 2 spaces. Prefer spaces over tabs.
##Function Declarations and Definitions
* A function declaration is on one line if possible. Otherwise the parameters that do not fit are on the next line(s). A function definition should not be part of the class declaration in the header file. Inline function should be defined in the header file but only below the class declaration.
Pointer and Reference Expressions
* No spaces around period or arrow. Pointer operators are either followed or preceded with a space.
##Boolean Expressions
* In the case of a boolean expression that is longer than the standard line length, lines should be broken up in a consistent way. All operators should be either at the beginning or at the end of the line.
##Variable and Array Initialization
* Prefer initialization with braces except for single-argument assignment.
##Preprocessor Directives
* The hash mark that starts a preprocessor directive is always at the beginning of the line.
##Classes
* Access specifiers in a class or a struct should not be indented. Lines containing methods and data member should be indented by at least 2 spaces, typically 4. Access specifiers should be ordered as public, protected, private. Declarations within a access specifier are to be ordered as Constructor, Destructor, Methods, Data Members.
##Constructor Initializer Lists
* Constructor initializer lists should be with subsequent lines indented properly. Alternatively, they can be all in a single line.
##Namespaces
* The contents of namespaces are not indented.
##Braces
* In control constructs (if statements, for loops etc.), it is recommended to use curly braces even when the body of the statement fits on one line.
##Horizontal Whitespace
* Recommended guidelines:

    One space should be used after each keyword.
    No extra spaces inside parenthesis and angle brackets (templates).
    Spaces should be used around binary operators.
    No space between a unary operator and its operand.
    Never put trailing whitespace at the end of a line.

##Vertical Whitespace
* Use only one empty line to separate code.
##Where to put const
* Put const before the type when defining a const variable.
##Comments

Though a pain to write, comments are absolutely vital to keeping our code readable. The following rules describe what you should comment and where. But remember: while comments are very important, the best code is self-documenting. Giving sensible names to types and variables is much better than using obscure names that you must then explain through comments.

When writing your comments, write for your audience: the next contributor who will need to understand your code. Be generous — the next one may be you!

##Comment Style
* Use either the // or /* */ syntax, as long as you are consistent.
##File Comment Block
* Every file should have a comment at the top describing its contents. It should contains the CVS macros $Id$ and $Log$, the primary/original author, as well as a brief description of the class.
##Function Comments
* Declaration comments describe use of the function; comments at the definition of a function describe operation.
##Variable Comments
* In general the actual name of the variable should be descriptive enough to give a good idea of what the variable is used for. In certain cases, more comments are required.
##Implementation Comments
* In your implementation you should have comments in tricky, non-obvious, interesting, or important parts of your code.
##Punctuation, Spelling and Grammar
* Pay attention to punctuation, spelling, and grammar; it is easier to read well-written comments than badly written ones.
##Commenting Out Code
* Do not leave commented out code in the source file after testing. Proper utilization of revision control system can be relied on to retrieve any version of the code.
#Other
##Printing Messages in STAR
* For printing messages in the STAR framework use the StMessage message manager package.
##Handling Error Conditions in STAR Framework
* To exit your code on an error condition in the STAR framework, return one of the STAR defined return codes from your Maker.
##Exceptions to the Rules

The coding conventions described above have to be followed. However, like all good rules, these sometimes have exceptions.
Existing Non-Conformant Code
* It is permissible to deviate from the rules when dealing with code that does not conform to this style guide. For example, in naming something that is analogous to an existing C or C++ entity then the existing naming convention scheme can be followed.

#Parting Words

Use common sense and BE CONSISTENT.

When editing code, take a few minutes to look at the code and determine its style.

The point about having style guidelines is to have a common vocabulary of coding so people can concentrate on what the programmer is saying, rather than on how he/she is saying it. Global style rules are presented here so people know the vocabulary. However, local style is also important. If the code added to a file looks drastically different from the existing code around it, the discontinuity throws readers out of their rhythm when they go to read it. Try to avoid this. 
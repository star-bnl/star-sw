# General naming rules

* Class, struct, function and variable names should be meaningful
** Prefer nouns for variables and access functions
** Prefer verbs for functions

# STAR naming rules

* Class names start with the prefix `St`
** Followed by the experimental subsystem's three or four letter acronym, or a short name for the software system it belongs to
* Member variables start with the prefix 'm'
* Type names should be in CamelCase.

# File Names

* C++ filenames are derived from the filename of the (primary) class or struct they define.  Thus they begin
* Files which contain only functions should also start with an 'St'


# Variable Names

* Local variables are in camelCase.
* Class member variables are prefixed by the letter `m`
* Struct member variables are not prefixed, and are in camelCase.
* Global variables should be avoided.  Where necessary, prefix with `g`.
* constexpr variables should be fully capitalized
* No prefix necessary for const.

# Function names
* Regular functions follow camelCase, e.g. myFunction()
* Accessors and mutators match the name of the variable.
** e.g. setMyVariable( const int value ), int myVariable()
** Do not use `get` to prefix an accessor

# Enumerator Names
* Enumerations and enumerators (the type and the values) follow CamelCase and start with `St`
** e.g. StBeamPolarizationAxis, StDetectorId.
* Enumerators in unscoped enumerations should have a common prefix/postfix derived from the enumerations name.
* Enum classes are already scoped and therefore the enumerators do not need a prefix/postfix.

# C Preprocessor Macro Names
* All uppercase letters and underscores, prefixed with ST (for STAR) and the sub/project name
** e.g. ST_PROJECT_PKG1_MY_MACRO

# STAR StRoot Naming Standards

This section attempts to explain the code directory structure and layout in STAR the rules and assumptions triggered in the make system (cons) solely on the basis of the name choice the existing exceptions to the rules.


The following naming convention keeps consistency between STAR code and packages written by many users. Not only it enables users to have a feel for what-does-what but also, it allows managers to define basic default set of compilation rules depending sometimes naming convention. Naming conventions in general are fundamental to all large projects and although N users will surely have N best-solution, the rules should be enforced as much as possible.

The tag `XXX` denotes a freely chosen name possibly constrained by the preceding rules.

## The Directory Structure Under the StRoot Tree

* StRoot/XXX

Only base class that should be freely named.  StarClassLibrary, StarRoot, Star2Root, StarGenerator, Sti*

User discouraged, request should be accompanied with a strong reasoning.

* StRoot/StXXX

A directory tree which will contain a base class many makers will use and derive from.
In this category, XXX can be anything. For example, StChain, StEvent, StEventUtilities.

User discouraged, request should be accompanied with a strong reasoning.

* StRoot/StXXXMaker

A tree for a Maker, that is, code compiled in this tree will be assembled as one self-sufficient package. A maker is a particular class deriving from StMaker. Its purpose is to run from within a chain (StChain) of makers and perform a specific task.

In this category, sub-name convention are as follows:

**    StXXXDbMaker a maker containing the database calls for the sub-system XXX
**    StXXXSimulationMaker or StXXXSimulatorMaker a simulation maker for the subsystem XXX
**    StXXXCalibMaker or StXXXCalibrationMaker a calibration maker for the sub-system XXX
**    StXXXMixerMaker a data/simulation mixer code for he sub-system XXX
**    StXXXDisplayMaker a self-explained named Graphical tool
**    StXXTagMaker a maker collecting tags for the sub-system or analysis XXX

while XXX is in principle a detector sub-system identification (3 to 4 letters uniquely designating the sub-system), it may also be anything but a detector sub-system (StAssociationMaker, StMiniMcMaker, StDbMaker) or of the form XXX=analysis or physics study.

* StXXXRaw*/

Any directory with named with the word Raw shall cause our build system include the necessary path for the Run-Time-System DAQ reader files automatically. This convention is additive to any other description and convention herein.

Example: StEmcRawMaker is a "maker" as described above) and a code base using the DAQ reader and so would be the expectation for Stl3RawReaderMaker or StFgtRawMaker.

* StRoot/StXXXUtil or StRoot/StXXXUtilities

Code compiled in a Util or Utilities tree should be code which does not perform any action (nor a maker) but constitute by itself a set of utility classes and functions. Other classes may depend on a Utility library.

**    XXXUtil : XXX IS a sub-system detector.
**    XXXUtilities : XXX IS NOT a detector sub-system (this is reserved)

* StRoot/StXXXPool

This tree will contain a set of sub-directories chosen by the user, each sub-directory maybe a self-contained project with no relation with anything else. Each sub-directory will therefore lead to the creation of a separate library. The naming convention for the library creation is as follow :

    If the subdirectory is named like StYYY, the library will inherit the same name. Beware of potential name clash in this case
    If the subdirectory has an arbitrary name YYY, the final library name will be have the name StXXXPoolYYY.

The Pool category has some special compilation internal rules: if it does not compile, it may be removed from compilation entirely. As such, codes appearing in Pool directory trees cannot be part of a production maker dependency. A typical usage for this structure is to provide a Pool (or collection) of lose codes not used in production (utility tools for sub-systems, analysis codes or utilities). XXX can be either a Physics Work Group acronym or a detector sub-system acronym.

* StRoot/StXXXClient

This tree will behave like the Pool trees in terms of library naming creation (separate libraries will be created, one per compilable sub-directory). XXX can be anything relevant for a sub-system. Client directories MUST compile (unlike the pools) and may be part of a dependency of a data processing chain. Its general goal is to provide a different tree structure for a set of code providing a "service" widely used across makers. For example, the Run Time System (RTS) have a Client tree containing DAQ related reading codes.

# Directory Trees and Implicit/Hidden rules

* StRoot/StXXX and StRoot/

** README

A basic documentation in plain text (not mandatory). If exists, the software guide will display the information contained in this file

** doc/

A directory containing more elaborate documentation, either in html or in LaTeX. Note that if a file named index.html exists, the software guide will link to it.

** local/

A subdirectory containing stand-alone Makefiles for the package and/or standalone configuration files.

This directory is deprecated. Do not use.

** examples/

A directory having a collection of code using the Maker or utility package of interest (case insensitive)

** macros/

A directory containing root macros example making use of the maker

** kumac/

This is an obsolete directory name.  Do not use.  It may also appears in the pams/ tree structure. 

** test/

This directory may contain test programs (executables should in principle not appear in our standard but be assembled)

This directory is deprecated. Do not use.

** html/

This directory is deprecated. Do not use.

** images/

A directory containing images such as bitmap, pixmaps or other images used by your program but NOT assembled by any part of the build process. XPM files necessary for Qt for example should not be placed in this directory as explicit rules exists in cons to handle those (but cons will ignore the xpm placed in images/).

** wrk/ and run/

This directory is deprecated. Do not use.

** include/

A directory containing a set of common include files.

External packages under StarGenerator may utilize.  Otherwise, this directory
is deprecated.  Do not use.

** Any other name

Will be searched for code one level down only. All compiled code will be assembled in one library named after to StXXX.... Each sub-directory will be compiled separately that is, each must contain code using explicit include path as the only default search paths for includes will be the one described by CPPPATH and its own directory.

However, if there is a need for StRoot/StXXX sub-directories compilation to include every available sub-paths (other than the exceptions noted above) (a) as a list of default path in a compiler option or if you want a default include/ directory (b) to be always added in a default include path compiler option statement, you may request this feature to be enabled. To do that, send an email to the starsoft-hn mailing list.

Include statement can ALWAYS refer to the relative path after the StRoot/portion as the StRoot/ path is a default in CPPPATH.


* StRoot/StXXXPool and StRoot/StXXXClient

** doc/ local/ examples/ macros/ kumac/ test/ html/ images/ wrk/ run/ include/

As noted above (i.e. the content of those directories will be skipped by the build system).

local/, test/, and include/ are legacy names and should not be used.

wrk/ and run/ are are discouraged for users.

** Any other name

The presence of every sub-directory will create a different dynamic library. Note that this is NOT the case with the other name format (all compiled code would go in a unique library name)

The convention is as follows:

    If the name starts with St, for example StZZZ, a library StZZZ.so will be created containing every compiled code available in StZZZ directory. In this form, the sub-directory MUST be self-sufficient i.e. all code and include (apart from the default paths) must be in the sub-directory StZZZ
    If the name does NOT start with St, for example WWW, a library StXXXPoolWWW.so will be created containing all compile code available in WWW directory.

* Current Patterned Exceptions

** StEventDisplay.*
Directories within this pattern will be compiled using the extra include path pointed by the environment variable QTDIR. The moc program will run on any include containing the Q_OBJECT directive, -DR__QT define is added to CXXFLAGS.
** StDbLib StDbBroker
Those are special. Compilation will consider MySQL includes and the created dynamic library will be linked against MySQL
** St.*Db.*
Any directory following this pattern will use the MySQL include as an extra include path for the CPPPATH directive

** StTrsMaker StRTSClient
Are two exceptions of kind (b) [see above] and use their own include/ directory as a general extraneous include path.
** StHbtMaker
For this maker, a pre-defined list of sub-directories is being added to the (CPPPATH).

** StAssociationMaker StMuDSTMaker .*EmcUtil StEEmcPool StTofPool StRichPool Sti.*

This form will include in the CPPPATH every sub-directories found one level below. Only macros/, examples/, and doc/ are excluded within this form noted in (a) [see above]. For the Pool directory, the extraneous rule mentioned here is additive to the one of Pool directories.




# Formatting

Coding style and formatting are pretty arbitrary. However, a good project is much easier to follow if everyone uses the same style. Individuals may not agree with every aspect of the formatting rules, and some of the rules may be hard to get used to. Even so, it is important that all project contributors follow the style rules so that they can all read and understand everyone's code easily.

* Try to keep line length within the approximate width of a terminal (~100 characters)
** Break long statements across multiple lines as appropriate
* Prefer one statement per line with code readability as the goal
* Indent code at least two spaces.  Prefer spaces over tabs.
* A function declaration is on one line if possible. Otherwise the parameters that do not fit are on the next line(s).
* A function definition should not be part of the class declaration in the header file.
* Inline function should be defined in the header file but only below the class declaration.

* In control constructs (if statements, for loops etc.), it is recommended to use curly braces even when the body of the statement fits on one line.

* Put const before the type when defining a const variable. 

## Pointer and reference expressions
* No spaces around period or arrow.
* Pointer operators are either followed or preceded with a space.
* Prefer `int* variable` over `int *variable` for pointer declarations.

## Boolean expressions
* Break long boolean expressions up in a consistent way, e.g. all operators should either be at the beginning or end of a line.

## Classes

* Access specifiers in a class or a struct should not be indented.
* Lines containing methods and data member should be indented by at least 2 spaces, typically 4.
* Access specifiers should be ordered as public, protected, private.
* Declarations within a access specifier are to be ordered as Constructor, Destructor, Methods, Data Members.
* Constructor initializer lists should be with subsequent lines indented properly. Alternatively, they can be all in a single line.


## Whitespace

* One space should be used after each keyword.
* No extra spaces inside parenthesis and angle brackets (templates).
* Spaces should be used around binary operators.
* No space between a unary operator and its operand.
* Never put trailing whitespace at the end of a line.

* Prefer one empty line to separate code. 



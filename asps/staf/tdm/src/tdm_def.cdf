**######################################################################
**######################################################################
**######################################################################
**:Copyright 1997, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        tdm_def.cdf
**:DESCRIPTION: Command Definition File for TDM package.
**:<--------------------------------------------------------------------
**
>NAME TDM_DEF
**
************************************************************************
** TDM
>MENU TDM
>GUIDANCE
Table_and_Dataset_Memory commands.
.
 #(@)$Id: tdm_def.cdf,v 1.8 1998/01/03 01:31:22 tull Exp $
.
TDM is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus archictecture of StAF in a CORBA compliant
interface layer.
.
Each ASP is comprised of an object factory interface (eg. tdmFactory)
and zero or more worker object interfaces.
.
TDM worker objects include:
   tdmDataset - See TDM/DATASET
      - C++ representation of DSL datasets.
   tdmTable - See TDM/TABLE
      - C++ representation of DSL tables.
.
The TDM package is a C++ classs library implementation of the DSL
package for memory-resident datasets and tables.
.
TDM is responsible for memory allocation and management for DSL
datasets and tables, and management of the data dictionary of table
types.
.
tdmDataset objects are container objects which can contain other
tdmDataset objects and/or tdmTable objects. The recursive inclusion of
datasets within datasets forms a hierarchic organization of datasets
which can be thought of as a tree or file-system-like hierarchy.
.
With the ability to link datasets and tables to more than one place,
datasets can form any general directed graph. The TDM interface limits
this more general organization to directed acyclical graphs (ie. When
navigating in only one direction along each node of the graph, each
dataset appears at most once on a navigational path.). This means that
no dataset can contain another dataset which eventually contains the
original dataset (eg. The Unix-like path foo/blah/foo/blah/foo is a
valid path iff each of the foo and blah datasets are distict. foo is
not the same dataset as foo/blah/foo or foo/blah/foo/blah/foo.).
.
Datasets are named, can be contained within other datasets, and can
contain other datasets, and/or tables.
.
DSL tables can be considered arrays of C structs of fixed length
definable in IDL (Interface Definition Language) from the base types:
char, octet, short, unsigned short, long, unsigned long, float, double.
.
Tables are named and typed, can be contained within datasets, and
contain data organized into columns (fields within a C struct) and rows
(elements within the array).
.
Each table column has a name (variable name), a type (a simple or
complex variable type), a rank (the number of dimensions), and a shape
(the sequence of the dimension sizes). Variable types can be scalars,
vectors, multi-dimensional arrays (current limit of 4 indicies),
structs, or any derived type from these complex types.
.
Table types are named (for convienence) and define the columns of a
table in IDL.
.
Some sample IDL table type definitions are:
.
   struct point { float x,y; };
.
   struct point { float x,y,z; };
.
   struct cartisian { float x,y,z; };
.
   struct namedLine {char name[32]; struct point {float x,y,z;}p[2];};
.
Although two of the IDLs above have the same type name, and two have
identical column definitions, these four table types can exist in the
DSL managed data dictionary at the same time and will be treated as
four seperate table types (Obviously, the shortcut of using a type name
instead of the entire type definition will not behave in a
deterministic fashion for the two types named "point".).
.
** ---------------------------------------------------------------------
** TDM/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show the current count of TDM worker objects.
.
DESCRIPTION: 
.
COUNT is a readonly long attribute which reflects the number of TDM
worker objects currently registered with the TDM object factory.
Constructing a new TDM worker object increments COUNT by 1,
destructing an existing TDM worker object decrements COUNT by 1.
.
TDM worker objects include:
   tdmDataset - See TDM/DATASET
      - C++ representation of DSL datasets.
   tdmTable - See TDM/TABLE
      - C++ representation of DSL tables.
.
ARGUMENTS: 
.
   None.
.
RETURN:
.
   The current value of COUNT is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current count of TDM worker objects.
.
   StAF> TDM/COUNT
   TDM:    Object count = 18
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDM_COUNT
**
** ---------------------------------------------------------------------
** TDM/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all currently registered TDM worker objects.
.
DESCRIPTION: 
.
Show a one-line description for each TDM worker object currently
registered with the TDM object factory in a table for quick,
simple perusal.
.
The one-line description for each object is the result of an invokation
of that object's listing method. The typical content of this listing is:
	0> OID
	   The object's OID attribute (see SOC) presented as "%5d".
	1> Lock State
           The object's LOCK attribute (see SOC) presented as the
	   divider character between the OID column and the NAME:OBJECT
	   column. An object whose LOCK attribute is TRUE (cannot be
	   deleted) uses the "-" character, whereas an object whose
	   LOCK attribute is FALSE (can be deleted) uses "|" character.
	2> NAME:OBJECT
	   The object's NAME attribute (see SOC) presented as "%-15s".
	   Object names longer than 15 characters are abreviated with a
	   "~" character at midpoint.
	   An object name is synonymous with an object instance.
	3> TYPE:CLASS
	   The object's TYPE attribute (see SOC) presented as "%-15s".
	   Object types longer than 15 characters are abreviated with a
	   "~" character at midpoint. 
	   An object type is synonymous with an object class.
	4> DESCRIPTION
	   A class-specific description of the object.
	   More guidance needed here.
.
TDM worker objects include:
   tdmDataset - See TDM/DATASET
      - C++ representation of DSL datasets.
   tdmTable - See TDM/TABLE
      - C++ representation of DSL tables.
.
ARGUMENTS: 
.
   None.
.
RETURN: 
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmFactory::list()
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. List all currently registered TDM worker objects.
.
   StAF> TDM/LIST
   +-------------------------------------------------------------------
   |*********************** TDM - Not a valid TDM listing *************
   +-------+-----------------+-----------------+-----------------------
   | OID   | NAME:OBJECT     | TYPE:CLASS      | DESCRIPTION
   +-------+-----------------+-----------------+-----------------------
   +-------+-----------------+-----------------+-----------------------
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDM_LIST
**
** ---------------------------------------------------------------------
** TDM/NEWDATASET NAME
>COMMAND NEWDATASET
>PARAMETERS
NAME 'Name for new tdmDataset object' C
>GUIDANCE
Create a new tdmDataset object.
.
DESCRIPTION: 
.
Each tdmDataset created by the tdmFactory shows up as an object
managed by the tdmFactory (see TDM/COUNT and TDM/LIST) and
registered with the socCatalog (see SOC/COUNT and SOC/LIST).
.
ARGUMENTS: 
.
   NAME - Case-sensitive alphanumeric name for new tdmDataset object.
   - Use this name as part of SOREF (see SOC) to specify this particular
     tdmDataset object in subsequent commands.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmFactory::newDataset
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new tdmDataset with NAME "bob"
.
   StAF> TDM/NEWDATASET bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_CREATED - The object creation failed. See error stack for
      detailed explaination of failure.
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
   TDM/DATASET
.
>ACTION KAM_TDM_NEWDATASET
**
** ---------------------------------------------------------------------
** TDM/NEWTABLE NAME SPEC MAXROWCOUNT
>COMMAND NEWTABLE
>PARAMETERS
NAME 'Name for new tdmTable object' C
SPEC    'Type specifier for a table type' C
MAXROWCOUNT        'Count of rows allocated in memory' I R='0:'
>GUIDANCE
Create a new tdmTable object.
.
DESCRIPTION: 
.
Each tdmTable created by the tdmFactory shows up as an object
managed by the tdmFactory (see TDM/COUNT and TDM/LIST) and
registered with the socCatalog (see SOC/COUNT and SOC/LIST).
.
ARGUMENTS: 
.
   NAME - Case-sensitive alphanumeric name for new tdmTable object.
   - Use this name as part of SOREF (see SOC) to specify this particular
     tdmTable object in subsequent commands.
   - More guidance needed here.
.
   SPEC - Type specifier for a table type.
   - More guidance needed here.
.
   MAXROWCOUNT - Count of rows allocated in memory.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmFactory::newTable
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new tdmTable with NAME "bob"
.
   StAF> TDM/NEWTABLE bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_CREATED - The object creation failed. See error stack for
      detailed explaination of failure.
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
   TDM/TABLE
.
>ACTION KAM_TDM_NEWTABLE
**
** ---------------------------------------------------------------------
** TDM/ALLOCSTATS
>COMMAND ALLOCSTATS
>PARAMETERS
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
ALLOCSTATS is a member function of the tdmFactory interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   None.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmFactory::ALLOCSTATS
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> TDM/ALLOCSTATS
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDM_ALLOCSTATS
**
************************************************************************
** TDM/DATASET
>MENU DATASET
>GUIDANCE
tdmDataset object commands.
.
Commands found under the TDM/DATASET menu can be applied to objects
which implement the tdmDataset interface.
.
More guidance needed here.
.
**
** ---------------------------------------------------------------------
** TDM/DATASET/ENTRYCOUNT SOREF
>COMMAND ENTRYCOUNT
>PARAMETERS
SOREF 'tdmDataset object SORef' C
>GUIDANCE
Get the ENTRYCOUNT attribute of the tdmDataset SOREF.
.
DESCRIPTION: 
.
ENTRYCOUNT is a readonly attribute which reflects the value of the ENTRYCOUNT
attribute of the tdmDataset SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the tdmDataset interface.
.
RETURN:
.
   The current value of ENTRYCOUNT is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the ENTRYCOUNT attribute of
    tdmDataset "bob".
.
   StAF> TDM/DATASET/ENTRYCOUNT bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmDataset interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMDATASET_ENTRYCOUNT
**
** ---------------------------------------------------------------------
** TDM/DATASET/NAME SOREF
>COMMAND NAME
>PARAMETERS
SOREF 'tdmDataset object SORef' C
>GUIDANCE
Get the NAME attribute of the tdmDataset SOREF.
.
DESCRIPTION: 
.
NAME is a readonly attribute which reflects the value of the NAME
attribute of the tdmDataset SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the tdmDataset interface.
.
RETURN:
.
   The current value of NAME is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the NAME attribute of
    tdmDataset "bob".
.
   StAF> TDM/DATASET/NAME bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmDataset interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMDATASET_NAME
**
** ---------------------------------------------------------------------
** TDM/DATASET/ADDDATASET SOREF NAME
>COMMAND ADDDATASET
>PARAMETERS
SOREF 'tdmDataset object SORef' C
NAME 'Name for new tdmDataset object' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
ADDDATASET is a member function of objects which implement the tdmDataset
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the tdmDataset interface.
.
   NAME - Name for new tdmDataset object
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmDataset::ADDDATASET
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the ADDDATASET method function of tdmDataset "bob"
     More guidance needed here.
.
   StAF> TDM/DATASET/ADDDATASET bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmDataset interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMDATASET_ADDDATASET
**
** ---------------------------------------------------------------------
** TDM/DATASET/ADDTABLE SOREF SOREF NAME SPEC MAXROWCOUNT
>COMMAND ADDTABLE
>PARAMETERS
SOREF 'tdmDataset object SORef' C
NAME 'Name for new tdmTable object' C
SPEC    'Type specifier for a table type' C
MAXROWCOUNT        'Count of rows allocated in memory' I R='0:'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
ADDTABLE is a member function of objects which implement the tdmDataset
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the tdmDataset interface.
.
   NAME - Name for new tdmDataset object
   - More guidance needed here.
.
   SPEC - Type specifier for a table type.
   - More guidance needed here.
.
   MAXROWCOUNT - Count of rows allocated in memory.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmDataset::ADDTABLE
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the ADDTABLE method function of tdmDataset "bob"
     More guidance needed here.
.
   StAF> TDM/DATASET/ADDTABLE bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmDataset interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMDATASET_ADDTABLE
**
** ---------------------------------------------------------------------
** TDM/DATASET/SHOW SOREF
>COMMAND SHOW
>PARAMETERS
SOREF 'tdmDataset object SORef' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
SHOW is a member function of objects which implement the tdmDataset
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the tdmDataset interface.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmDataset::SHOW
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the SHOW method function of tdmDataset "bob"
     More guidance needed here.
.
   StAF> TDM/DATASET/SHOW bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmDataset interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMDATASET_SHOW
**
************************************************************************
** TDM/TABLE
>MENU \TABLE
>GUIDANCE
tdmTable object commands.
.
Commands found under the TDM/TABLE menu can be applied to objects
which implement the tdmTable interface.
.
More guidance needed here.
.
**
** ---------------------------------------------------------------------
** TDM/TABLE/MAXROWCOUNT SOREF [ NEW_VALUE ]
>COMMAND MAXROWCOUNT
>PARAMETERS
SOREF 'tdmTable object SORef' C
+
NEW_VALUE 'New value of MAXROWCOUNT attribute' I D=-1 R='-1:'
>GUIDANCE
Get or set the MAXROWCOUNT attribute of the tdmTable SOREF.
.
DESCRIPTION: 
.
MAXROWCOUNT is a read-writable attribute which determines the value of
the MAXROWCOUNT attribute.
.
To get the current value of MAXROWCOUNT, leaving MAXROWCOUNT unchanged, do not
specify a new value in the optional argument NEW_VALUE.
.
To set a new value of MAXROWCOUNT, specify the new value as the optional
argument NEW_VALUE.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the tdmTable interface.
.
   NEW_VALUE - New value for the MAXROWCOUNT attribute.
   - DEFAULT: Show the current value of MAXROWCOUNT, do not change it.
.
RETURN:
.
   The current value of MAXROWCOUNT is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the MAXROWCOUNT attribute of tdmTable 
    object "bob".
.
   StAF> TDM/TABLE/MAXROWCOUNT bob
.
EG2. Set the MAXROWCOUNT attribute of tdmTable object "bob" to 123.
.
   StAF> TDM/TABLE/MAXROWCOUNT bob 123
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTABLE_MAXROWCOUNT
**
** ---------------------------------------------------------------------
** TDM/TABLE/ROWCOUNT SOREF [ NEW_VALUE ]
>COMMAND ROWCOUNT
>PARAMETERS
SOREF 'tdmTable object SORef' C
+
NEW_VALUE 'New value of ROWCOUNT attribute' I D=-1 R='-1:'
>GUIDANCE
Get or set the ROWCOUNT attribute of the tdmTable SOREF.
.
DESCRIPTION: 
.
ROWCOUNT is a read-writable attribute which determines the value of
the ROWCOUNT attribute.
.
To get the current value of ROWCOUNT, leaving ROWCOUNT unchanged, do not
specify a new value in the optional argument NEW_VALUE.
.
To set a new value of ROWCOUNT, specify the new value as the optional
argument NEW_VALUE.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the tdmTable interface.
.
   NEW_VALUE - New value for the ROWCOUNT attribute.
   - DEFAULT: Show the current value of ROWCOUNT, do not change it.
.
RETURN:
.
   The current value of ROWCOUNT is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the ROWCOUNT attribute of tdmTable 
    object "bob".
.
   StAF> TDM/TABLE/ROWCOUNT bob
.
EG2. Set the ROWCOUNT attribute of tdmTable object "bob" to 123.
.
   StAF> TDM/TABLE/ROWCOUNT bob 123
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTABLE_ROWCOUNT
**
** ---------------------------------------------------------------------
** TDM/TABLE/COLUMNCOUNT SOREF
>COMMAND COLUMNCOUNT
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
Get the COLUMNCOUNT attribute of the tdmTable SOREF.
.
DESCRIPTION: 
.
COLUMNCOUNT is a readonly attribute which reflects the value of the COLUMNCOUNT
attribute of the tdmTable SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the tdmTable interface.
.
RETURN:
.
   The current value of COLUMNCOUNT is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the COLUMNCOUNT attribute of
    tdmTable "bob".
.
   StAF> TDM/TABLE/COLUMNCOUNT bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTABLE_COLUMNCOUNT
**
** ---------------------------------------------------------------------
** TDM/TABLE/NAME SOREF
>COMMAND NAME
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
Get the NAME attribute of the tdmTable SOREF.
.
DESCRIPTION: 
.
NAME is a readonly attribute which reflects the value of the NAME
attribute of the tdmTable SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the tdmTable interface.
.
RETURN:
.
   The current value of NAME is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the NAME attribute of
    tdmTable "bob".
.
   StAF> TDM/TABLE/NAME bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTABLE_NAME
**
** ---------------------------------------------------------------------
** TDM/TABLE/ROWSIZE SOREF
>COMMAND ROWSIZE
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
Get the ROWSIZE attribute of the tdmTable SOREF.
.
DESCRIPTION: 
.
ROWSIZE is a readonly attribute which reflects the value of the ROWSIZE
attribute of the tdmTable SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the tdmTable interface.
.
RETURN:
.
   The current value of ROWSIZE is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the ROWSIZE attribute of
    tdmTable "bob".
.
   StAF> TDM/TABLE/ROWSIZE bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTABLE_ROWSIZE
**
** ---------------------------------------------------------------------
** TDM/TABLE/SPECIFIER SOREF
>COMMAND SPECIFIER
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
Get the SPECIFIER attribute of the tdmTable SOREF.
.
DESCRIPTION: 
.
SPECIFIER is a readonly attribute which reflects the value of the SPECIFIER
attribute of the tdmTable SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the tdmTable interface.
.
RETURN:
.
   The current value of SPECIFIER is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the SPECIFIER attribute of
    tdmTable "bob".
.
   StAF> TDM/TABLE/SPECIFIER bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTABLE_SPECIFIER
**
** ---------------------------------------------------------------------
** TDM/TABLE/TYPENAME SOREF
>COMMAND TYPENAME
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
Get the TYPENAME attribute of the tdmTable SOREF.
.
DESCRIPTION: 
.
TYPENAME is a readonly attribute which reflects the value of the TYPENAME
attribute of the tdmTable SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the tdmTable interface.
.
RETURN:
.
   The current value of TYPENAME is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the TYPENAME attribute of
    tdmTable "bob".
.
   StAF> TDM/TABLE/TYPENAME bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTABLE_TYPENAME
**
** ---------------------------------------------------------------------
** TDM/TABLE/PRINT SOREF [ NROWS IFIRST ]
>COMMAND PRINT
>PARAMETERS
SOREF 'tdmTable object SORef' C
+
NROWS   'Number of rows to print' I D=10
IFIRST  'First row to print' I D=0
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
PRINT is a member function of objects which implement the tdmTable
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the tdmTable interface.
.
   NROWS - Number of rows to print.
   - More guidance needed here.
.
   IFIRST - First row to print. 
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmTable::PRINT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the PRINT method function of tdmTable "bob"
     More guidance needed here.
.
   StAF> TDM/TABLE/PRINT bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTABLE_PRINT
**
** ---------------------------------------------------------------------
** TDM/TABLE/SHOW SOREF
>COMMAND SHOW
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
SHOW is a member function of objects which implement the tdmTable
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the tdmTable interface.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmTable::SHOW
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the SHOW method function of tdmTable "bob"
     More guidance needed here.
.
   StAF> TDM/TABLE/SHOW bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTABLE_SHOW
**
************************************************************************
** TDM/TABLE/CELL
>MENU CELL
>GUIDANCE
tdmTable object CELL component commands.
.
Commands found under the TDM/TABLE/CELL menu can be applied to CELL
components of objects which implement the tdmTable interface.
.
More guidance needed here.
.
**
** ---------------------------------------------------------------------
** TDM/TABLE/CELL/GETVALUE SOREF      
>COMMAND GETVALUE
>PARAMETERS
SOREF 'tdmTable.CELL component SORef' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
GETVALUE is a member function of CELL components of objects which
implement the tdmTable interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting a CELL component of an object implementing the 
     tdmTable interface.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> TDM/TABLE/CELL/GETVALUE bob.head
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No CELL component specified by SOREF can be
      found for an object which implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTABLE_CELL_GETVALUE
**
** ---------------------------------------------------------------------
** TDM/TABLE/CELL/PUTVALUE SOREF VALUES
>COMMAND PUTVALUE
>PARAMETERS
SOREF 'tdmTable.CELL component SORef' C
VALUES  'List of new cell values' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
PUTVALUE is a member function of CELL components of objects which
implement the tdmTable interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting a CELL component of an object implementing the 
     tdmTable interface.
.
   VALUES - List of new cell values.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> TDM/TABLE/CELL/PUTVALUE bob.head
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No CELL component specified by SOREF can be
      found for an object which implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTABLE_CELL_PUTVALUE
**
************************************************************************
** TDM/TYPESPECIFIERS
>MENU \\TYPESPECIFIERS
>GUIDANCE
tdmTypespecifiers object commands.
.
Commands found under the TDM/TYPESPECIFIERS menu can be applied to objects
which implement the tdmTypespecifiers interface.
.
More guidance needed here.
.
**
** ---------------------------------------------------------------------
** TDM/TYPESPECIFIERS/LIST [ TYPEID ]
>COMMAND LIST
>PARAMETERS
+
TYPEID  'Table type ID' I D=-1
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
LIST is a member function of objects which implement the tdmTypespecifiers
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   TYPEID - Table type ID.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmTypespecifiers::LIST
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the LIST method function of tdmTypespecifiers "bob"
.
   StAF> TDM/TYPESPECIFIERS/LIST bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTypespecifiers interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTYPESPECIFIERS_LIST
**
** ---------------------------------------------------------------------
** TDM/TYPESPECIFIERS/LOAD IDL_FILE
>COMMAND LOAD
>PARAMETERS
IDL_FILE 'IDL file containing table IDL' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
LOAD is a member function of objects which implement the tdmTypespecifiers
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   IDL_FILE - IDL file containing table IDL.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmTypespecifiers::LOAD
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the LOAD method function of tdmTypespecifiers "bob"
.
   StAF> TDM/TYPESPECIFIERS/LOAD bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTypespecifiers interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTYPESPECIFIERS_LOAD
**
** ---------------------------------------------------------------------
** TDM/TYPESPECIFIERS/SHOW [ TYPENAME ]
>COMMAND SHOW
>PARAMETERS
+
TYPENAME        'Table type name' C D='*'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
SHOW is a member function of objects which implement the tdmTypespecifiers
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   TYPENAME - Table type name.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmTypespecifiers::SHOW
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the SHOW method function of tdmTypespecifiers "bob"
.
   StAF> TDM/TYPESPECIFIERS/SHOW bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the tdmTypespecifiers interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TDMTYPESPECIFIERS_SHOW
**

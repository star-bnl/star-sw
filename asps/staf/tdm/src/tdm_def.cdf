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
 #(@)$Id: tdm_def.cdf,v 1.18 1998/07/18 18:13:38 ward Exp $
 Edited by Bill Love on 23-24 Feb 1998
.
TDM is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus architecture of StAF in a CORBA compliant
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
Table types are named (for convenience) and define the columns of a
table in IDL.
.
Some sample IDL table type definitions are:
.
   struct point { float x,y; };
.
   struct point { float x,y,z; };
.
   struct cartesian { float x,y,z; };
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
Constructing a new TDM worker object increments COUNT by 1.  
Destroying an existing TDM worker object decrements COUNT by 1.
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
   (see SOC).  A message is also printed to stdout.
.
EXAMPLES: 
.
EG1. Show the current count of TDM worker objects.
.
   StAF> TDM/COUNT
   TDM:    Object count = 18
.
BUGS: 
   None known.
.
>ACTION kam_tdm_count_%C
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
Show a tabular one-line description for each TDM worker object currently
registered with the TDM object factory.  Note the DUI/cd, ls and pwd 
commands give a UNIX-like access to the lists of Datasets and Tables. 
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
	   A class-specific description of the object.  For Datasets the
           number of entries are shown and for tables the number of used
           and allocated rows and the number of bytes of each row.
.
TDM worker objects include:
   tdmDataset - See TDM/DATASET
      - C++ representation of DSL datasets.
   tdmTable - See TDM/TABLE
      - C++ representation of DSL tables.
.
ARGUMENTS: 
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
 STAF[3] tdm/list
 
 +---------------------------------------------------------------------
 |******************** TDM - Table & Dataset Memory listing ***********
 +-------+-----------------+-----------------+-------------------------
 | IDREF | NAME:OBJECT     | TYPE:CLASS      | DESCRIPTION                      
 +-------+-----------------+-----------------+-------------------------
 |     3 | /dui            | tdmDataset      | 0 ent.s                          
 +-------+-----------------+-----------------+-------------------------

EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
   DUI/LS
.
>ACTION kam_tdm_list_%C
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
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmFactory::newDataset method is pushed onto the 
   STAF_STATUS stack (see SOC).
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
      detailed explanation of failure.
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
   TDM/DATASET
.
>ACTION kam_tdm_newdataset_%C
**
** ---------------------------------------------------------------------
** TDM/NEWTABLE NAME SPEC MAXROWCOUNT
>COMMAND NEWTABLE
>PARAMETERS
NAME 'Name for new tdmTable object' C
SPEC    'Type specifier for a table type' C
MAXROWCOUNT        'Count of rows allocated in memory' I R='0:'
>GUIDANCE
Create a new tdmTable object in the current Dataset.
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
.
   SPEC - Type specifier for a table type.  This can either be simply
     the name of an existing table type or a complete definition of a 
     new table type.
.
   MAXROWCOUNT - Number of table rows to be allocated in memory.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmFactory::newTable
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new Table in Dataset "bob" of type "tpt_spars" with 100 rows
.
 STAF[19] dui/cd bob
 STAF[20] tdm/newtable george tpt_spars 100
 STAF[21] dui/ls
 DUI:    Listing = ...
  Name             * Type             * Used     * Alloc'd  * Size    
 T           george *        tpt_spars *        0 *      100 *      216
.
EXCEPTIONS: 
.
   OBJECT_NOT_CREATED - The object creation failed. See error stack for
      detailed explanation of failure.
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
   TDM/TABLE
.
>ACTION kam_tdm_newtable_%C
**
** ---------------------------------------------------------------------
** TDM/ALLOCSTATS
>COMMAND ALLOCSTATS
>PARAMETERS
>GUIDANCE
Print statistics of dataset and table usage.
.
DESCRIPTION: 
.
ALLOCSTATS is a member function of the tdmFactory interface.
Numbers printed are maybe of some value to debuggers (people, not DBX).
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
EG1. 
   StAF> TDM/ALLOCSTATS
AllocStats: bufSize 0, dsetSize 51606632, listSize 0, memCalls 826, tidSize 85199
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_tdm_allocstats_%C
**
************************************************************************
** TDM/DATASET
>MENU DATASET
>GUIDANCE
tdmDataset object commands.  Nothing useful yet.
.
Commands found under the TDM/DATASET menu can be applied to objects
which implement the tdmDataset interface.
.
**
** ---------------------------------------------------------------------
** TDM/DATASET/ENTRYCOUNT SOREF
>COMMAND ENTRYCOUNT
>PARAMETERS
SOREF 'tdmDataset object SORef' C
>GUIDANCE
Get the ENTRYCOUNT of a tdmDataset.
.
DESCRIPTION: 
.
The ENTRYCOUNT is the total number of tables and other datasets contained 
in the dataset.
ENTRYCOUNT is a readonly attribute which reflects the value of the ENTRYCOUNT
attribute of the tdmDataset SOREF. 
Readonly attributes cannot be changed from the user interface.
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
    tdmDataset "Maps".
.
 staf++ > tdm/dataset/entrycount Maps
 TDMDATASET:     Entry Count = 2 
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
>ACTION kam_tdmdataset_entrycount_%C
**
** ---------------------------------------------------------------------
** TDM/DATASET/NAME SOREF
>COMMAND NAME
>PARAMETERS
SOREF 'tdmDataset object SORef' C
>GUIDANCE
Get the NAME attribute of the tdmDataset SOREF.  (Redundant?)
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
EG1.    
   Staf>tdm/dataset/name ProducedData      
TDMDATASET:     DSL name = (ProducedData) 
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
>ACTION kam_tdmdataset_name_%C
**
** ---------------------------------------------------------------------
** TDM/DATASET/ADDDATASET SOREF NAME
>COMMAND ADDDATASET
>PARAMETERS
SOREF 'tdmDataset object SORef' C
NAME 'Name for new tdmDataset object' C
>GUIDANCE
Not Yet Implemented.  Intended to copy a dataset?
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
>ACTION kam_tdmdataset_adddataset_%C
**
** ---------------------------------------------------------------------
** TDM/DATASET/ADDTABLE SOREF NAME SPEC MAXROWCOUNT
>COMMAND ADDTABLE
>PARAMETERS
SOREF 'tdmDataset object SORef' C
NAME 'Name for new tdmTable object' C
SPEC    'Type specifier for a table type' C
MAXROWCOUNT        'Count of rows allocated in memory' I R='0:'
>GUIDANCE
Not implemented yet.  Intended to add tables to datasets?  See TDM/NEWTABLE
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
   NAME - Name for new tdmTable object 
   SPEC - Type specifier for a table type.
.
   MAXROWCOUNT - Count of rows allocated in memory.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmDataset::ADDTABLE
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the ADDTABLE method function of tdmDataset "Geometry"
.
tdm/dataset/addtable Geometry tpg_detector tpg_detector 5
NOT_YET_IMPLEMENTED-/afs/rhic/.../asps/staf/tdm/src/tdmClasses.c
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
>ACTION kam_tdmdataset_addtable_%C
**
** ---------------------------------------------------------------------
** TDM/DATASET/SHOW SOREF
>COMMAND SHOW
>PARAMETERS
SOREF 'tdmDataset object SORef' C
>GUIDANCE
Not implemented yet.  No idea what its for.
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
>ACTION kam_tdmdataset_show_%C
**
************************************************************************
** TDM/TABLE
>MENU \TABLE
>GUIDANCE
tdmTable object commands.  Work with Tables and Cells.
.
Commands found under the TDM/TABLE menu can be applied to objects
which implement the tdmTable interface.
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
Get or set the MAXROWCOUNT of a Table.  I.e. the size.
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
EG1. Show the current MAXROWCOUNT of Table "george" 
.
 STAF[29] tdm/table/maxrowcount george
 TDMTABLE:       Max Row Count = 100 
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
>ACTION kam_tdmtable_maxrowcount_%C
**
** ---------------------------------------------------------------------
** TDM/TABLE/ROWCOUNT SOREF [ NEW_VALUE ]
>COMMAND ROWCOUNT
>PARAMETERS
SOREF 'tdmTable object SORef' C
+
NEW_VALUE 'New value of ROWCOUNT attribute' I D=-1 R='-1:'
>GUIDANCE
Get or set the ROWCOUNT of a Table.  I.e. the number used.
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
argument NEW_VALUE.  NEW_VALUE cannot be greater than MAXROWCOUNT.
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
EG1. Show the current ROWCOUNT of Table "george"
.
 STAF[32] tdm/table/rowcount george      
 TDMTABLE:       Row Count = 0    
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
   INVALID_ROW_COUNT - attempt to set rowcount larger than MAXROWCOUNT.
.
BUGS: 
.
   None known. 
.
>ACTION kam_tdmtable_rowcount_%C
**
** ---------------------------------------------------------------------
** TDM/TABLE/COLUMNCOUNT SOREF
>COMMAND COLUMNCOUNT
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
Get the COLUMNCOUNT (number of variables in a row) of a Table.
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
EG1. Show the current COLUMNCOUNT of Table "george".
.
 STAF[33] tdm/table/columncount george
 TDMTABLE:       Column Count = 10 
 
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
>ACTION kam_tdmtable_columncount_%C
**
** ---------------------------------------------------------------------
** TDM/TABLE/NAME SOREF
>COMMAND NAME
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
Get the NAME attribute of the tdmTable SOREF. Usefulness?
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
EG1. Show the current value of the NAME attribute of tdmTable "george".
.
 STAF[34] tdm/table/name george       
 TDMTABLE:       DSL Name = (george) 
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
>ACTION kam_tdmtable_name_%C
**
** ---------------------------------------------------------------------
** TDM/TABLE/ROWSIZE SOREF
>COMMAND ROWSIZE
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
Get the ROWSIZE (bytes per row) of a Table.
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
EG1. Show the current ROWSIZE of Table "george".
.
 STAF[35] tdm/table/rowsize george
 TDMTABLE:       Row Size = 216 bytes 
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
 DUI/LS
.
>ACTION kam_tdmtable_rowsize_%C
**
** ---------------------------------------------------------------------
** TDM/TABLE/SPECIFIER SOREF
>COMMAND SPECIFIER
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
Get and Print the type SPECIFIER  of a tdmTable.
.
DESCRIPTION: 
.
SPECIFIER is a readonly attribute which reflects the value of the SPECIFIER
attribute of the tdmTable SOREF. Readonly attributes cannot be changed
from the user interface.  The specifier is the Table "type"
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
EG1. Show the type SPECIFIER of Table "george".
.
 STAF[36] tdm/table/specifier george
 TDMTABLE:       Type Specifier = ...
 struct tpt_spars {
        long first_row,  last_row,  nskip,  skip[45],  hole,  nmin,  ilimit;
        float oy,  oz,  outlimit;
}
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
>ACTION kam_tdmtable_specifier_%C
**
** ---------------------------------------------------------------------
** TDM/TABLE/TYPENAME SOREF
>COMMAND TYPENAME
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
Get the TYPENAME of a Table.  
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
EG1. Show the current TYPENAME of Table "george".
.
 STAF[37] tdm/table/typename george 
 TDMTABLE:       Type Name = (tpt_spars) 
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
>ACTION kam_tdmtable_typename_%C
**
** ---------------------------------------------------------------------
** TDM/TABLE/DUMP SOREF NROWS IFIRST NAMEOFFILE COLUMNLIST
>COMMAND DUMP
>PARAMETERS
SOREF 'name of table' C
NROWS   'Number of rows to dump' I D=10
IFIRST  'First row to dump' I D=0
NAMEOFFILE 'Name of output file' C
COLUMNLIST 'List of columns' C
>GUIDANCE
Dumps a table to file.
.
The IFIRST parameter counts from zero _UNLIKE_ Fortran.
.
If you want all the rows, use a large number for NROWS, and zero
for IFIRST.
.
If you want output to the screen instead of a file,
write 'screen' for the filename.
.
The COLUMNLIST parameters are used
to select a subset of the columns.  
Separate the column names with spaces or carets (^).
See the example below.
If you want all the columns, type 'allColumns'.
.
EXAMPLE: 
           table  tpg_cathode
        num rows  10
       first row  0
     output file  myfile.dat
         columns  id offset pedestal
.
 STAF[46] tdm/table/dump tpg_cathode 10 0 myfile.dat id offset pedestal
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - Table not found.
.
BUGS: 
.
   None known.
.
>ACTION kam_tdmtable_dump_%C
**
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
Print the contents (or some sequential rows) of a table.
.
DESCRIPTION: 
.
PRINT is a member function of objects which implement the tdmTable
interface which causes the object to print its contents.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the tdmTable interface.
   i. e. the Table name.
.
   NROWS - Number of rows to print.
.
   IFIRST - First row to print. 
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmTable::PRINT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the PRINT method function of tdmTable "tpg_cathode"
.
 STAF[46] tdm/table/print  tpg_cathode
 ROW #  cath_mat        cath_in_rad     cath_out_rad    cath_thick
     0: -4      46.825  200     0.00762
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
>ACTION kam_tdmtable_print_%C
**
** ---------------------------------------------------------------------
** TDM/TABLE/SHOW SOREF
>COMMAND SHOW
>PARAMETERS
SOREF 'tdmTable object SORef' C
>GUIDANCE
Show the type definition of a table.
.
DESCRIPTION: 
.
SHOW is a member function of objects which implement the tdmTable
interface which prints the definition of the table type.
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
EG1. 
 STAF> tdm/table/show tphit
 TDMTABLE:       Table = ...
 struct tcl_tphit {
        long cluster,  flag,  id,  id_globtrk,  nseq,  row,  track;
        float alpha,  dalpha,  dlambda,  dq,  dx,  dy,  dz,  lambda,  phi,  prf,  q,  x,  y,  z,  zrf;
 } 
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
TDM/TABLE/SPECIFIER gives similar information.
.
>ACTION kam_tdmtable_show_%C
**
************************************************************************
** TDM/TABLE/CELL
>MENU CELL
>GUIDANCE
tdmTable object CELL component commands.  Access to individual Table Cells.
.
Commands found under the TDM/TABLE/CELL menu can be applied to CELL
components of objects which implement the tdmTable interface.
.
**
** ---------------------------------------------------------------------
** TDM/TABLE/CELL/GETVALUE SOREF      [OFF|ON]
>COMMAND GETVALUE
>PARAMETERS
SOREF 'tdmTable.CELL component SORef' C
+
SCREEN 'Screen output. Either OFF or ON.' C D='OFF'
>GUIDANCE
Return the value contained in a single cell of a table.
SCREEN controls whether the returned value is written
to the screen.  
.
DESCRIPTION: 
.
GETVALUE is a member function of CELL components of objects which
implement the tdmTable interface whcih returns the contents of the CELL.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting a CELL component of an object implementing the 
     tdmTable interface.  Must be enclosed in single quotes with the
     row number in square brackets and the column name preceded by a 
     period.  If the row number is the content of a KUIP variable
     (a common usage) the need to include the variable in square brackets
     requires the ugly format 'table_name['//[row_variable]//'].col_name
.
RETURN:
.
   The contents of the cell are returned in staf_result(1)
   The contents of the cell are also optionally printed to the 
   screen.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
 EG1. STAF> tdm/table/cell/getvalue 'tpt_spars[0].last_row' ON_SCREEN
 TDMTABLE:       Cell data =     45
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No CELL component specified by SOREF can be
      found for an object which implements the tdmTable interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
   INVALID_TABLE_COLUMN - Found the table but not the cell.
.
BUGS: 
.
   An ill-formed cell reference causes a segmentation fault.

 STAF[55] tdm/table/cell/get 'tpg_cathode.cath_mat'   
 
 *** Break *** Segmentation violation
 
 TRACEQ.  In-line trace-back still not available.
 
.
SEE ALSO: 
.
>ACTION kam_tdmtable_cell_getvalue_%C
**
** ---------------------------------------------------------------------
** TDM/TABLE/CELL/PUTVALUE SOREF VALUES
>COMMAND PUTVALUE
>PARAMETERS
SOREF 'tdmTable.CELL component SORef' C
VALUES  'List of new cell values' C
>GUIDANCE
Insert data into a cell of a table.
.
DESCRIPTION: 
.
PUTVALUE can be used to change the contents of one cell of a table.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting a CELL component of a Table.  Note the table row is
   enclosed in square brackets and the column name should follow
   a period.  The whole SOREF is enclosed in single quotes.  If the
   row number is the content of a KUIP variable (a common usage) the
   need to include the variable in square brackets requires the ugly
   format 
           'table_name['//[row_variable]//'].col_name
.
.
   VALUES - List of new cell values.  Unless the cell is defined as an
   array, this will be a single VALUE.  For an array cell, all members
   must be listed, separated by blanks.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1.
 tdm/table/cell/putvalue 'tpt_spars[0].nskip' 10 
.
EG2.
 tdm/table/cell/putvalue 'tpt_spars[0].skip' 1 2 3 4 5 6 7 8 9 10
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND -  Table name is wrong or SOREF not in single quotes.
   INVALID_TABLE_COLUMN -  CELL specified by SOREF not found.
. 
BUGS: 
.
   Ill-formed SOREF's produce Segmentation violation errors.
   If the variable is an array, unspecified members are often filled with
   arbitrary junk.  Makes it impractical to use interactively on large 
   arrays.   It would be nice to have a more flexible input
   structure like 1 2 3 5*0 11*1 9 8 2 - or something like that.
.
SEE ALSO: 
.
>ACTION kam_tdmtable_cell_putvalue_%C
**
************************************************************************
** TDM/TYPESPECIFIERS
>MENU \\TYPESPECIFIERS
>GUIDANCE
TDM/TYPESPECIFIERS commands show or manipulate Table type definitions.
.
**
** ---------------------------------------------------------------------
** TDM/TYPESPECIFIERS/LIST [ TYPEID ]
>COMMAND LIST
>PARAMETERS
+
TYPEID  'Table type ID' I D=-1
>GUIDANCE
List one or all (ID negative) table type names.   
.
DESCRIPTION: 
.
LIST is a member function of objects which implement the tdmTypespecifiers
interface.  Reminds user of which types have been defined in case he forgot
the spelling of the name.
.
ARGUMENTS: 
.
   TYPEID - Table type ID.
   Only useful value would seem to be the default (-1)
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmTypespecifiers::LIST
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1.  List the third type in the current list. 
.
 STAF[57] tdm/type/list 3
 TDM:    Type name = (tpt_track)  
.
EXCEPTIONS: 
.
   INVALID_TYPE_ID  ID specified is out of range - I.e. larger than total
 number of Types defined.
.
BUGS: 
.
   None known.
.
>ACTION kam_tdmtypespecifiers_list_%C
**
** ---------------------------------------------------------------------
** TDM/TYPESPECIFIERS/LOAD IDL_FILE
>COMMAND LOAD
>PARAMETERS
IDL_FILE 'IDL file containing table IDL' C
>GUIDANCE
Read Table type specifier data from an IDL file. 
.
DESCRIPTION: 
.
LOAD is a member function of objects which implement the tdmTypespecifiers
interface which reads a type specification from an idl format file.
.
ARGUMENTS: 
.
   IDL_FILE - IDL file containing table IDL.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmTypespecifiers::LOAD
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
 StaF > tdm/type/load tpg_transform.idl
 file = tpg_transform.idl
 *************************
 ---
                                                                                                                                                                                           
 
                                                        
   struct tpg_transform {  
       float     global_origin[3];                                            
       float     local_origin[3];                                           
       float     phi_limhi;                                              
       float     phi_limlo;                                            
       float     sector_angle;                               
       float     sector_cos;                          
       float     sector_sin;                         
       float     y_local_limhi;                                      
       float     y_local_limlo;                                      
       float     z_global_limhi;                                            
       float     z_global_limlo;                                           
       float     z_local_limhi;                                              
   };
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
>ACTION kam_tdmtypespecifiers_load_%C
**
** ---------------------------------------------------------------------
** TDM/TYPESPECIFIERS/SHOW [ TYPENAME ]
>COMMAND SHOW
>PARAMETERS
+
TYPENAME        'Table type name' C D='*'
>GUIDANCE
Print the definition of the Table type name given.
.
DESCRIPTION: 
.
SHOW is a member function of objects which implement the tdmTypespecifiers
interface.  Lists the data types and names of the columns of a table type.
.
ARGUMENTS: 
.
   TYPENAME - Table type name.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tdmTypespecifiers::SHOW
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. .
   StAF> TDM/TYPESPECIFIERS/SHOW tpt_spars
 TDM:    Type spec = ...
 struct tpt_spars {
        long first_row,  last_row,  nskip,  skip[45],  hole,  nmin,  ilimit;
        float oy,  oz,  outlimit; 
 }
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
 TDM/TABLE/SHOW
.
>ACTION kam_tdmtypespecifiers_show_%C
**

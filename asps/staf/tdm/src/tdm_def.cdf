**:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        tdm_def.cdf
**:DESCRIPTION: Table & Dataset Memory Command Definition File 
**:HISTORY:	30apr96-v100a-cet- Beta Release Version
**:HISTORY:     26apr96-v003a-cet- Add many new commands
**:HISTORY:     29nov95-v001a-cet- Update
**:HISTORY:     18arp95-v000a-cet- Creation
**:<--------------------------------------------------------------------

>NAME TDM_DEF

************************************************************************
************************************************************************
** TDM
>MENU TDM
>GUIDANCE
Table & Dataset Memory commands.
$Id: tdm_def.cdf,v 1.3 1997/01/30 22:08:50 tull Exp $
.
Commands for the Table & Dataset Memory (TDM) ASP.
.
The TDM package is a C++ classs library implementation of the DSL
Package for memory-resident datasets and tables.
.
TDM is responsible for memory allocation and management for DSL
datasets and tables, and management of the data tictionary of table
types.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** TDM/ALLOCSTATS
>COMMAND ALLOCSTATS
>PARAMETERS
>GUIDANCE
Show DSL allocation statistics.
.
Show the statistics for membory allocation and deallocation of DSL
tables and datasets.
.
>ACTION KAM_TDM_ALLOCSTATS

** ---------------------------------------------------------------------
** TDM/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show count of known TDM objects.
.
Show the number of dataset and table objects currently instantiated and
managed by the TDM object factory.
.
>ACTION KAM_TDM_COUNT

** ---------------------------------------------------------------------
** TDM/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all known TDM objects.
.
List all dataset and table objects currently instantiated and
managed by the TDM object factory.
.
>ACTION KAM_TDM_LIST

** ---------------------------------------------------------------------
** TDM/NEWDATASET NAME [ SETDIM ]
>COMMAND NEWDATASET
>PARAMETERS
NAME 'tdmDataset name.' C
+
DIM 'dataset dimension.' I D=100
>GUIDANCE
Create a new tdmDataset.
.
Create a new dataset unencapsulated by any other dataset.
.
>ACTION KAM_TDM_NEWDATASET

** ---------------------------------------------------------------------
** TDM/NEWTABLE NAME [ SPEC ROWCOUNT ]
>COMMAND NEWTABLE
>PARAMETERS
NAME 'tdmTable name.' C
SPEC 'Type specifier for a table row.' C
ROWCOUNT 'Number of rows in the table.' I R='0:'
>GUIDANCE
Create a new tdmTable.
.
     NAME -- Case-sensitive alphanumeric name for table to create.
	     EG.: fred, Table001, tpc_calib
.
     SPEC -- IDL specification string or table type name for new table.
	     EG.: 'struct point {double x,y,z;};', point
.
 ROWCOUNT -- Initial number of rows to allocate for new table. (0 >=1)
.
>ACTION KAM_TDM_NEWTABLE

************************************************************************
************************************************************************
** TDM/TYPESPECIFIERS
>MENU TYPESPECIFIERS
>GUIDANCE
dsl type specifier commands.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** TDM/TYPESPECIFIERS/LIST [ NTYPE ]
>COMMAND LIST
>PARAMETERS
+
NTYPE 'Table type id.' I D='-1'
>GUIDANCE
List one or all table type names.
.
>ACTION KAM_TDM_TYPE_LIST

** ---------------------------------------------------------------------
** TDM/TYPESPECIFIERS/SHOW [ TYPENAME OPTION ]
>COMMAND SHOW
>PARAMETERS
+
TYPENAME 'Table type name.' C D='*'
OPTION 'Options' C D=' ' R=' '
>GUIDANCE
Show table type names.
.
Possible OPTION values are:
   ' '   Show everything.
.
 ************************
 * Still In Development *
 ************************
.
>ACTION KAM_TDM_TYPE_SHOW

************************************************************************
************************************************************************
** TDM/TABLE
>MENU \TABLE
>GUIDANCE
tdmTable commands.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** TDM/TABLE/NAME TDMTABLE
>COMMAND NAME
>PARAMETERS
TDMTABLE 'tdmTable name.' C
>GUIDANCE
Show the DSL name of a tdmTable Object.
.
>ACTION KAM_TDMTABLE_NAME

** ---------------------------------------------------------------------
** TDM/TABLE/ROWCOUNT TDMTABLE [ROWCOUNT]
>COMMAND ROWCOUNT
>PARAMETERS
TDMTABLE 'tdmTable name.' C
+
ROWCOUNT 'Number of rows filled' I D=-1
>GUIDANCE
Show or change the number of rows filled.
.
>ACTION KAM_TDMTABLE_ROWCOUNT

** ---------------------------------------------------------------------
** TDM/TABLE/MAXROWCOUNT TDMTABLE [MAXROWCOUNT]
>COMMAND MAXROWCOUNT
>PARAMETERS
TDMTABLE 'tdmTable name.' C
+
MAXROWCOUNT 'Number of rows allocated' I D=-1
>GUIDANCE
Show or change the number of rows allocated.
.
>ACTION KAM_TDMTABLE_MAXROWCOUNT

** ---------------------------------------------------------------------
** TDM/TABLE/COLUMNCOUNT TDMTABLE
>COMMAND COLUMNCOUNT
>PARAMETERS
TDMTABLE 'tdmTable name.' C
>GUIDANCE
Show the number of columns.
.
>ACTION KAM_TDMTABLE_COLCOUNT

** ---------------------------------------------------------------------
** TDM/TABLE/TYPENAME TDMTABLE
>COMMAND TYPENAME
>PARAMETERS
TDMTABLE 'tdmTable name.' C
>GUIDANCE
Show the tdmTable type.
.
>ACTION KAM_TDMTABLE_TYPENAME

** ---------------------------------------------------------------------
** TDM/TABLE/SPECIFIER TDMTABLE
>COMMAND SPECIFIER
>PARAMETERS
TDMTABLE 'tdmTable name.' C
>GUIDANCE
Show the tdmTable specification.
.
>ACTION KAM_TDMTABLE_SPECIFIER

** ---------------------------------------------------------------------
** TDM/TABLE/ROWSIZE TDMTABLE
>COMMAND ROWSIZE
>PARAMETERS
TDMTABLE 'tdmTable name.' C
>GUIDANCE
Show the tdmTable row size.
.
>ACTION KAM_TDMTABLE_ROWSIZE

** ---------------------------------------------------------------------
** TDM/TABLE/PRINT TDMTABLE [ NROWS IFIRST FILE ]
>COMMAND PRINT
>PARAMETERS
TDMTABLE 'tdmTable name.' C
+
NROWS 'Number of rows.' I D=10
IFIRST 'First row.' I D=0
FILE 'name of ASCII file.' C D='-'
>GUIDANCE
Print the contents of a tdmTable to stdout or a file.
.
   FILE = "-" :: Print output to standard out.
.
 ************************
 * Still In Development *
 ************************
.
>ACTION KAM_TDMTABLE_PRINT

** ---------------------------------------------------------------------
** TDM/TABLE/SHOW TDMTABLE [ OPTION ]
>COMMAND SHOW
>PARAMETERS
TDMTABLE 'tdmTable name.' C
+
OPTION 'Options.' C D=' ' R=' '
>GUIDANCE
Show the definition of a tdmTable.
.
Possible OPTION values are:
   ' '    Show everything.
.
 ************************
 * Still In Development *
 ************************
.
>ACTION KAM_TDMTABLE_SHOW

************************************************************************
************************************************************************
** TDM/TABLE/CELL
>MENU CELL
>GUIDANCE
tdmTable cell commands.
.
 ************************
 * Still In Development *
 ************************
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** TDM/TABLE/CELL/GETVALUE CELLNAME
>COMMAND GETVALUE
>PARAMETERS
CELLNAME 'tdmTable cell name.' C
>GUIDANCE
Print the contents of a tdmTable cell.
.
 ************************
 * Still In Development *
 ************************
.
>ACTION KAM_TDMTABLE_CELL_GETVALUE

** ---------------------------------------------------------------------
** TDM/TABLE/CELL/PUTVALUE CELLNAME VALUES
>COMMAND PUTVALUE
>PARAMETERS
CELLNAME 'tdmTable cell name.' C
VALUES 'list of values.' C
>GUIDANCE
Change the contents of a tdmTable cell.
.
 ************************
 * Still In Development *
 ************************
.
>ACTION KAM_TDMTABLE_CELL_PUTVALUE

************************************************************************
************************************************************************
** TDM/TABLE/COLUMN
>MENU \COLUMN
>GUIDANCE
tdmTable column commands.
.
 ************************
 * Not yet Implemented  *
 ************************
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** TDM/TABLE/COLUMN/FIND COLUMNAME
>COMMAND FIND
>PARAMETERS
COLUMNAME 'tdmTable.column name.' C
>GUIDANCE
Find the column number of a named column.
.
See STAF/SYNTAX for syntax of column names.
.
 ************************
 * Not Yet Implemented  *
 ************************
.
>ACTION KAM_TDMTABLE_COLUMN_FIND

** ---------------------------------------------------------------------
** TDM/TABLE/COLUMN/NAME [ NCOL ]
>COMMAND NAME
>PARAMETERS
NCOL 'tdmTable.number.' C
>GUIDANCE
Show the name of a tdmTable column.
.
 ************************
 * Not Yet Implemented  *
 ************************
.
>ACTION KAM_TDMTABLE_COLUMN_NAME

** ---------------------------------------------------------------------
** TDM/TABLE/COLUMN/TYPE [ NCOL ]
>COMMAND TYPE
>PARAMETERS
NCOL 'tdmTable.number.' C
>GUIDANCE
Show the type name of a tdmTable column.
.
 ************************
 * Not Yet Implemented  *
 ************************
.
>ACTION KAM_TDMTABLE_COLUMN_TYPE

** ---------------------------------------------------------------------
** TDM/TABLE/COLUMN/CODE [ NCOL ]
>COMMAND CODE
>PARAMETERS
NCOL 'tdmTable.number.' C
>GUIDANCE
Show the type code of a tdmTable column.
.
 ************************
 * Not Yet Implemented  *
 ************************
.
>ACTION KAM_TDMTABLE_COLUMN_CODE

** ---------------------------------------------------------------------
** TDM/TABLE/COLUMN/RANK [ NCOL ]
>COMMAND RANK
>PARAMETERS
NCOL 'tdmTable.number.' C
>GUIDANCE
Show the rank of a tdmTable column.
.
 ************************
 * Not Yet Implemented  *
 ************************
.
>ACTION KAM_TDMTABLE_COLUMN_RANK

** ---------------------------------------------------------------------
** TDM/TABLE/COLUMN/SHAPE [ NCOL ]
>COMMAND SHAPE
>PARAMETERS
NCOL 'tdmTable.number.' C
>GUIDANCE
Show the shape of a tdmTable column.
.
 ************************
 * Not Yet Implemented  *
 ************************
.
>ACTION KAM_TDMTABLE_COLUMN_SHAPE

** ---------------------------------------------------------------------
** TDM/TABLE/COLUMN/ELCOUNT [ NCOL ]
>COMMAND ELCOUNT
>PARAMETERS
NCOL 'tdmTable.number.' C
>GUIDANCE
Show the element count of a tdmTable column.
.
 ************************
 * Not Yet Implemented  *
 ************************
.
>ACTION KAM_TDMTABLE_COLUMN_ELCOUNT

** ---------------------------------------------------------------------
** TDM/TABLE/COLUMN/SIZE [ NCOL ]
>COMMAND SIZE
>PARAMETERS
NCOL 'tdmTable.number.' C
>GUIDANCE
Show the size in bytes of a tdmTable column.
.
 ************************
 * Not Yet Implemented  *
 ************************
.
>ACTION KAM_TDMTABLE_COLUMN_SIZE


************************************************************************
************************************************************************
** TDM/DATASET
>MENU \\DATASET
>GUIDANCE
tdmDataset commands.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** TDM/DATASET/NAME TDMDATASET
>COMMAND NAME
>PARAMETERS
TDMDATASET 'tdmDataset name.' C
>GUIDANCE
Show the DSL name of a tdmDataset Object.
.
>ACTION KAM_TDMDATASET_NAME

** ---------------------------------------------------------------------
** TDM/DATASET/ENTRYCOUNT TDMDATASET
>COMMAND ENTRYCOUNT
>PARAMETERS
TDMDATASET 'tdmDataset name.' C
>GUIDANCE
Show the number of entries in a tdmDataset Object.
.
>ACTION KAM_TDMDATASET_ENTRYCOUNT

** ---------------------------------------------------------------------
** TDM/DATASET/MAXENTRYCOUNT TDMDATASET
**OLD_DSL >COMMAND MAXENTRYCOUNT
**OLD_DSL >PARAMETERS
**OLD_DSL TDMDATASET 'tdmDataset name.' C
**OLD_DSL >GUIDANCE
**OLD_DSL Show the maximum allowable number of entries in a tdmDataset
**OLD_DSL Object.
**OLD_DSL .
**OLD_DSL >ACTION KAM_TDMDATASET_MAXENTRYCOUNT

** ---------------------------------------------------------------------
** TDM/DATASET/ADDDATASET TDMDATASET NAME [ DIM ]
>COMMAND ADDDATASET
>PARAMETERS
TDMDATASET 'tdmDataset name.' C
NAME 'tdmTable name.' C
+
DIM 'New dataset dimension.' I D=100
>GUIDANCE
Add a tdmDataset to a tdmDataset.
.
 ************************
 * Not yet Implemented  *
 ************************
.
>ACTION KAM_TDMDATASET_ADDDATASET

** ---------------------------------------------------------------------
** TDM/DATASET/ADDTABLE TDMDATASET NAME SPEC ROWCOUNT
>COMMAND ADDTABLE
>PARAMETERS
TDMDATASET 'tdmDataset name.' C
NAME 'tdmTable name.' C
SPEC 'Type specifier for a table row.' C
ROWCOUNT 'Number of rows in the table.' I
>GUIDANCE
Add a tdmTable to a tdmDataset.
.
 ************************
 * Not yet Implemented  *
 ************************
.
>ACTION KAM_TDMDATASET_ADDTABLE

** ---------------------------------------------------------------------
** TDM/DATASET/SHOW TDMDATASET [ OPTION ]
>COMMAND SHOW
>PARAMETERS
TDMDATASET 'tdmDataset name.' C
+
OPTION 'Options' C D=' ' R=' '
>GUIDANCE
Show the hierarchy of a tdmDataset.
.
Possible OPTION values are:
   ' '   Show everything.
.
 ************************
 * Not yet Implemented  *
 ************************
.
>ACTION KAM_TDMDATASET_SHOW

** ---------------------------------------------------------------------
** TDM/DATASET/ENTRYNAME NENTRY
>COMMAND ENTRYNAME
>PARAMETERS
NENTRY 'tdmDataset.entryNumber.' C
>GUIDANCE
Show the name of a numbered tdmDataset entry.
.
 ************************
 * Not yet Implemented  *
 ************************
.
>ACTION KAM_TDMDATASET_ENTRYNAME

** ---------------------------------------------------------------------
** TDM/DATASET/FINDENTRY ENTRYNAME
>COMMAND FINDENTRY
>PARAMETERS
ENTRYNAME 'tdmDataset.entryName.' C
>GUIDANCE
Find and show entry number of named tdmDataset entry.
.
 ************************
 * Not yet Implemented  *
 ************************
.
>ACTION KAM_TDMDATASET_FINDENTRY


**######################################################################
**######################################################################
**######################################################################
**:Copyright 1997, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        top_def.cdf
**:DESCRIPTION: Command Definition File for TOP package.
**:<--------------------------------------------------------------------
**
>NAME TOP_DEF
**
************************************************************************
** TOP
>MENU TOP
>GUIDANCE
Table_OPerators commands.
.
 #(@)$Id: top_def.cdf,v 1.13 1998/07/09 11:29:19 ward Exp $
.
TOP is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus archictecture of StAF in a CORBA compliant
interface layer.
.
Each ASP is comprised of an object factory interface (eg. topFactory)
and zero or more worker object interfaces.
.
TOP worker objects include:
   topCut - See TOP/CUT_AGENT
      - An operator object for selecting rows from a table.
   topJoin - See TOP/JOIN_AGENT
      - An operator object for joining row-by-row data from two tables.
   topProject - See TOP/PROJECT_AGENT
      - An operator object for selecting columns from a table.
   topSort - See TOP/SORT_AGENT
      - An operator object for reordering rows in a table.
.
** ---------------------------------------------------------------------
** TOP/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show the current count of TOP worker objects.
.
DESCRIPTION: 
.
COUNT is a readonly long attribute which reflects the number of TOP
worker objects currently registered with the TOP object factory.
Constructing a new TOP worker object increments COUNT by 1,
destroying an existing TOP worker object decrements COUNT by 1.
.
TOP worker objects include:
   topCut - See TOP/CUT_AGENT
      - An operator object for selecting rows from a table.
   topJoin - See TOP/JOIN_AGENT
      - An operator object for joining row-by-row data from two tables.
   topProject - See TOP/PROJECT_AGENT
      - An operator object for selecting columns from a table.
   topSort - See TOP/SORT_AGENT
      - An operator object for reordering rows in a table.
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
EG1. Show the current count of TOP worker objects.
.
   StAF> TOP/COUNT
   TOP:    Object count = 18
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_top_count_%C
**
** ---------------------------------------------------------------------
** TOP/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all currently registered TOP worker objects.
.
DESCRIPTION: 
.
Show a one-line description for each TOP worker object currently
registered with the TOP object factory in a table for quick,
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
	   Object names longer than 15 characters are abbreviated with a
	   "~" character at midpoint.
	   An object name is synonymous with an object instance.
	3> TYPE:CLASS
	   The object's TYPE attribute (see SOC) presented as "%-15s".
	   Object types longer than 15 characters are abbreviated with a
	   "~" character at midpoint. 
	   An object type is synonymous with an object class.
	4> DESCRIPTION
	   A class-specific description of the object.
	   Sorts list nothing.  Joins lists too much to fit the field.
.
TOP worker objects include:
   topCut - See TOP/CUT_AGENT
      - An operator object for selecting rows from a table.
   topJoin - See TOP/JOIN_AGENT
      - An operator object for joining row-by-row data from two tables.
   topProject - See TOP/PROJECT_AGENT
      - An operator object for selecting columns from a table.
   topSort - See TOP/SORT_AGENT
      - An operator object for reordering rows in a table.
.
ARGUMENTS: 
.
   None.
.
RETURN: 
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topFactory::list()
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. List all currently registered TOP worker objects.
.
 staf++ > top/list
 
 +---------------------------------------------------------------------
 |*********************** TOP - Table Operators listing ***************
 +-------+-----------------+-----------------+-------------------------
 | IDREF | NAME:OBJECT     | TYPE:CLASS      | DESCRIPTION             
 +-------+-----------------+-----------------+-------------------------
 |    58 | s025            | topSort         |                         
 |    59 | j025            | topJoin         | {tphit.id id, t#{tphit.i 
 |    60 | s050            | topSort         |                          
 |    61 | s050a           | topSort         |                          
 |    62 | j050            | topJoin         | {id, alpha, lam#{ntphit. 
 +---------------------------------------------------------------------
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_top_list_%C
**
** ---------------------------------------------------------------------
** TOP/NEWCUT NAME CUTFUNC
>COMMAND NEWCUT
>PARAMETERS
NAME 'Name for new topCut object' C
CUTFUNC 'Cut function' C
>GUIDANCE
Create a new topCut object.
.
DESCRIPTION: 
.
Each topCut created by the topFactory shows up as an object
managed by the topFactory (see TOP/COUNT and TOP/LIST) and
registered with the socCatalog (see SOC/COUNT and SOC/LIST).
.
This is the first step in making table cuts and/or filters.
.
ARGUMENTS: 
.
   NAME - Case-sensitive alphanumeric name for new topCut object.
   - Use this name as part of SOREF (see SOC) to specify this particular
     topCut object in subsequent commands.
.
   CUTFUNC - Cut function.
   - A FORTRAN-like string defining a criterion on a table row.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topFactory::newCut
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new topCut.
.
   StAF> TOP/NEWCUT slowPions pid.eq.5.and.invpt.gt.1.12e3
.
In this case 'slowPions' is the name of the topCut object.
You will need this name in the second step.
The identifiers 'pid' and 'invpt' are column names.
.
For the second step, use either TOP/CUT_AGENT/FILTER or 
TOP/CUT_AGENT/CUT.
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
   TOP/CUT_AGENT
.
>ACTION kam_top_newcut_%C
**
** ---------------------------------------------------------------------
** TOP/NEWJOIN NAME [ SELECT WHERE ]
>COMMAND NEWJOIN
>PARAMETERS
NAME 'Name for new topJoin object' C
+
SELECT  'Selection specification' C D='-'
WHERE   'Where clause specification' C D='-'
>GUIDANCE
Create a new topJoin object.
.
DESCRIPTION: 
.
Each topJoin created by the topFactory shows up as an object
managed by the topFactory (see TOP/COUNT and TOP/LIST) and
registered with the socCatalog (see SOC/COUNT and SOC/LIST).
.
ARGUMENTS: 
.
   NAME - Case-sensitive alphanumeric name for new topJoin object.
   - Use this name as part of SOREF (see SOC) to specify this particular
     topJoin object in subsequent commands.
   - More guidance needed here.
.
   SELECT - Selection specification.
          - See TOP/PROJECT_AGENTfor syntax of SELECT.
.
   WHERE - Where clause specification.
         - See TOP/JOIN_AGENT for syntax of WHERE.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topFactory::newJoin
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new topJoin with NAME "j050" which will join 16 columns 
of the ntphit and strack tables where ntphit.track matches strack.trk
.
 STAF[1] newjoin j050 '{id, alpha, lambda, row, x, y, z, track, cluster, _
 STAF[1]_    q, xave, sigma, skew, kurto, npnt, chisqxy, chisqz}' _
 STAF[1]_    '{ntphit.track strack.trk}'

.
EXCEPTIONS: 
.
   OBJECT_NOT_CREATED - The object creation failed. See error stack for
      detailed explanation of failure.
.
BUGS: 
.
   If a join of the name given already exists newjoin fails w/o any 
 message.
.
SEE ALSO: 
.
   TOP/JOIN_AGENT
.
>ACTION kam_top_newjoin_%C
**
** ---------------------------------------------------------------------
** TOP/NEWPROJECT NAME [ SELECT ]
>COMMAND NEWPROJECT
>PARAMETERS
NAME 'Name for new topProject object' C
+
SELECT  'Selection specification' C D='-'
>GUIDANCE
Create a new topProject object.
.
DESCRIPTION: 
.
Each topProject created by the topFactory shows up as an object
managed by the topFactory (see TOP/COUNT and TOP/LIST) and
registered with the socCatalog (see SOC/COUNT and SOC/LIST).
.
ARGUMENTS: 
.
   NAME - Case-sensitive alphanumeric name for new topProject object.
   - Use this name as part of SOREF (see SOC) to specify this particular
     topProject object in subsequent commands.
   - More guidance needed here.
.
   SELECT - Selection specification.
          - See TOP/PROJECT_AGENT for syntax of SELECT.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topFactory::newProject
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new topProject with NAME "bob"
.
   StAF> TOP/NEWPROJECT bob
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
   TOP/PROJECT
.
>ACTION kam_top_newproject_%C
**
** ---------------------------------------------------------------------
** TOP/NEWSORT NAME COLUMN
>COMMAND NEWSORT
>PARAMETERS
NAME 'Name for new topSort object' C
COLUMN  'Column name upon which to sort' C
>GUIDANCE
Create a new topSort object to sort rows of a table.
.
DESCRIPTION: 
.
Each topSort created by the topFactory shows up as an object
managed by the topFactory (see TOP/COUNT and TOP/LIST) and
registered with the socCatalog (see SOC/COUNT and SOC/LIST).
.
ARGUMENTS: 
.
   NAME - Case-sensitive alphanumeric name for new topSort object.
   - Use this name as part of SOREF (see SOC) to specify this particular
     topSort object in subsequent commands.
.
   COLUMN - Column name upon which to sort.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topFactory::newSort
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new topSort which will sort tables on column "id".
.
   StAF> TOP/NEWSORT bob id
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
   TOP/SORT_AGENT
.
>ACTION kam_top_newsort_%C
**
************************************************************************
** TOP/CUT_AGENT
>MENU CUT_AGENT
>GUIDANCE
topCut object commands.
.
Commands found under the TOP/CUT_AGENT menu can be applied to objects
which implement the topCut interface.
.
The CUT (or FILTER) operation in TOP is analogous to the cut operation
on NTuples in PAW. It eliminates rows from a table which do not satisfy
some criterion expressed as a function of the column variables of that
table.
.
The distinction between CUT and FILTER is whether a new table is
created (FILTER) with rows that pass the cut criterion or rows within
the input table which do not pass the cut criterion are removed (CUT).
.
**
** ---------------------------------------------------------------------
** TOP/CUT_AGENT/FUNCTION SOREF
>COMMAND FUNCTION
>PARAMETERS
SOREF 'topCut object SORef' C
>GUIDANCE
Get the FUNCTION attribute of the topCut SOREF.
.
DESCRIPTION: 
.
FUNCTION is a readonly attribute which defines the criterion upon which
to select valid rows from a table. Readonly attributes cannot be
changed from the user interface.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the topCut interface.
.
RETURN:
.
   None.
.
EXAMPLES: 
.
EG1. Show the current value of the FUNCTION attribute of
    topCut "bob".
.
   StAF> TOP/CUT_AGENT/FUNCTION slowPions
   pid.eq.5 .and. invpt.gt.1.12e3
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topCut interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_topcut_agent_function_%C
**
** ---------------------------------------------------------------------
** TOP/CUT_AGENT/CUT SOREF TABLE [ CUTFUNC ]
>COMMAND CUT
>PARAMETERS
SOREF 'topCut object SORef' C
TABLE   'tdmTable name' C
+
CUTFUNC 'Cut function' C D='.'
>GUIDANCE
Eliminate rows from a table which fail the cut function.
.
DESCRIPTION: 
.
If FUNC is not specified:
.
Using the cut previously specified with the TOP/NEWCUT command,
remove all the rows from TABLE1 which do not pass the cut.
.
If FUNC is specified:
.
Remove all the rows from TABLE1 which do not
pass the cut, creating a new cut agent as a byproduct.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topCut interface.
.
   TABLE - tdmTable name.
   - The table to be cut.
.
   CUTFUNC - Cut function.
   - A cut function with which to create a new topCut object.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topCut::CUT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the CUT method function of topCut "bob"
     More guidance needed here.
.
   StAF> TOP/CUT_AGENT/CUT bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topCut interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_topcut_agent_cut_%C
**
** ---------------------------------------------------------------------
** TOP/CUT_AGENT/FILTER SOREF TABLE1 TABLE2 [ CUTFUNC ]
>COMMAND FILTER
>PARAMETERS
SOREF 'topCut object SORef' C
TABLE1  'tdmTable name of input table' C
TABLE2  'tdmTable name of output table' C
+
CUTFUNC 'Cut function' C D='.'
>GUIDANCE
Fill output table with rows from input table passing cut function.
.
DESCRIPTION: 
.
If FUNC is not specified:
.
Using the cut previously specified with the TOP/NEWCUT command,
write a new table named TABLE2 using all the rows of TABLE1
that pass the cut.
.
If FUNC is specified:
.
Write a new table named TABLE2 using all the rows of TABLE1 that pass
the cut function, creating a new cut agent as a byproduct.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topCut interface.
.
   TABLE1 - tdmTable name of input table.
.
   TABLE2 - tdmTable name of output table.
          - If TABLE2 does not exist, it will be created with the
	    same columns TABLE1.
.
   CUTFUNC - Cut function.
   - A cut function with which to create a new topCut object.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topCut::FILTER
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. FILTER rows from tb1 into tb2 where x > 1000.
.
   StAF> TOP/CUT_AGENT/FILTER bigx tb1 tb2 x.gt.1000
   More guidance needed here.
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topCut interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_topcut_agent_filter_%C
**
************************************************************************
** TOP/JOIN_AGENT
>MENU \JOIN_AGENT
>GUIDANCE
topJoin object commands.
.
Commands found under the TOP/JOIN_AGENT menu can be applied to objects
which implement the topJoin interface.
.
A JOIN (or FASTJOIN) operation in TOP is analogous to a join operation
in any Relational DataBase system. A topJoin object combines data
from two tables into a third table on a row-by-row basis subject to a 
condition on one or more column variables as defined by a where clause
specification string.
.
In TOP, the join method function of a topJoin object actually performs
a join and a project. Hence, the JOIN (or FASTJOIN) command takes both
a Where Clause *and* a Selection Specification (see TOP/PROJECT).
.
    +--------------------------------------+
    | Where Clause                         |
    +--------------------------------------+
    A where clause is used to define the condition for joining
    rows of two tables in a join (and project).
.
    The syntax of a where clause has the format:
        '{' join_pair_specifier ',' ... '}'
    where a join_pair_specifier is of the form:
        column_specifier
    or:
        column_specifier <WHITE_SPACE> column_specifier
    where the two column_specifier's MUST come from different tables
    and where a column_specifier is of the form:
        column_name
    or:
        table_name '.' column_name
.
    If a where clause is applied for a join (and project), the
    output table will contain rows composed of data from both 
    tables where both tables have equal (INTEGER) values in the
    specified columns (eg. equal primary-key/foreign-key pairs).
.
    If no where clause is applied for a join (and project) the join
    will occur on all (INTEGER) columns with the same name in both
    tables.
.
    Examples:
        '{id}'
        '{pkey fkey}'
        '{tab1.pkey tab2.fkey}'
        '{g2t_vertex.id g2t_track.start_vertex_p}'
.
**
** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/SELECTSPEC SOREF
>COMMAND SELECTSPEC
>PARAMETERS
SOREF 'topJoin object SORef' C
>GUIDANCE
Get the SELECTSPEC attribute of the topJoin SOREF.
.
DESCRIPTION: 
.
SELECTSPEC is a readonly attribute which defines the mapping of input
table columns to output table columns for JOIN (or FASTJOIN) operations
performed by the topJoin SOREF. Readonly attributes cannot be changed
from the user interface.
.
SELECTSPEC is the SQL-like Selection Specification string for a topJoin
object.  See TOP/PROJECT_AGENT for syntax of Selection Specification strings.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the topJoin interface.
.
RETURN:
.
   None.
.
EXAMPLES: 
.
EG1. Show the current value of the SELECTSPEC attribute of
topJoin "bob".
.
   StAF> TOP/JOIN_AGENT/SELECTSPEC bob
   More guidance needed here.
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topJoin interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
   TOP/PROJECT
.
>ACTION kam_topjoin_agent_selectspec_%C
**
** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/WHERECLAUSE SOREF
>COMMAND WHERECLAUSE
>PARAMETERS
SOREF 'topJoin object SORef' C
>GUIDANCE
Get the WHERECLAUSE attribute of the topJoin SOREF.
.
DESCRIPTION: 
.
WHERECLAUSE is a readonly attribute which defines hows rows of two
tables match for JOIN (or FASTJOIN) operations performed by topJoin
SOREF. Readonly attributes cannot be changed from the user interface.
.
WHERECLAUSE is the SQL-like Where Clause string for a topJoin object.
See TOP/JOIN_AGENT for syntax of Where Clause strings.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the topJoin interface.
.
RETURN:
.
   None.
.
EXAMPLES: 
.
EG1. Show the current value of the WHERECLAUSE attribute of
topJoin "bob".

.
   StAF> TOP/JOIN_AGENT/WHERECLAUSE bob
   More guidance needed here.
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topJoin interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
   TOP/JOIN
.
>ACTION kam_topjoin_agent_whereclause_%C
**
** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/FASTJOIN SOREF TABLE1 TABLE2 TABLE3 [ SELECT WHERE ]
>COMMAND FASTJOIN
>PARAMETERS
SOREF 'topJoin object SORef' C
TABLE1  'tdmTable name of 1st input table' C
TABLE2  'tdmTable name of 2nd input table' C
TABLE3  'tdmTable name of output table' C
+
SELECT  'Selection specification' C D='-'
WHERE   'Where clause specification' C D='-'
>GUIDANCE
Join two sorted tables to fill a third.
.
DESCRIPTION: 
.
FASTJOIN is a member function of objects which implement the topJoin
interface.
.
You can read about joining in a book on relational databases
(eg, an SQL reference book).
You can use the same JOIN agent with either JOIN or FASTJOIN.
FASTJOIN works the same as JOIN, except as noted below.
.
It runs very fast (proportional to n instead of n squared).
Eg, two 10,000 row tables that previously took 20 minutes will
run in 0.12 seconds.
.
You must first sort each table on its corresponding
column in the WHERE clause of FASTJOIN.
You can sort the tables easily with TOP/SORT_AGENT/SORT.
This sort is based on a fancy sorting algorithm (quicksort),
and runs very fast.
.
The WHERE parameter must be simple, ie,
'{' column_name <WHITE_SPACE> column_name '}'.
No commas, no periods.
The first  column_name is for the first  table.
The second column_name is for the second table.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topJoin interface.
.
   TABLE1 - tdmTable name of 1st input table.
.
   TABLE2 - tdmTable name of 2nd input table.
.
   TABLE3 - tdmTable name of output table.
	  - Result of JOIN operation.
          - If TABLE3 does not exist, it will be created with the
	    proper columns as defined by topJoin object SOREF.
.
   SELECT - Selection specification.
	  - See TOP/PROJECT_AGENT for syntax of SELECT.
          - If topJoin object SOREF does not exist, create it with
	    selection specification string SELECT.
.
   WHERE - Where clause specification.
	 - See TOP/JOIN_AGENT for syntax of WHERE.
         - If topJoin object SOREF does not exist, create it with
	   where clause string WHERE.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topJoin::FASTJOIN
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Use an existing join object, j050 (defined by top/newjoin)
     to join selected rows of the presorted ntphit and strack tables
     into table newhit.

 staf++ > fastjoin j050 ntphit  ProducedData/Tracks/strack  newhit.
 Real time 00:00:00, CP time 0.000
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topJoin interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
   TOP/PROJECT_AGENT
   TOP/JOIN_AGENT/JOIN
.
>ACTION kam_topjoin_agent_fastjoin_%C
**
** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/JOIN SOREF TABLE1 TABLE2 TABLE3 [ SELECT WHERE ]
>COMMAND JOIN
>PARAMETERS
SOREF 'topJoin object SORef' C
TABLE1  'tdmTable name of 1st input table' C
TABLE2  'tdmTable name of 2nd input table' C
TABLE3  'tdmTable name of output table' C
+
SELECT  'Selection specification' C D='-'
WHERE   'Where clause specification' C D='-'
>GUIDANCE
Join two unsorted tables row-by-row to fill a third.
.
DESCRIPTION: 
.
JOIN is a member function of objects which implement the topJoin
interface.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topJoin interface.
.
   TABLE1 - tdmTable name of 1st input table.
.
   TABLE2 - tdmTable name of 2nd input table.
.
   TABLE3 - tdmTable name of output table.
          - Result of JOIN operation.
          - If TABLE3 does not exist, it will be created with the
	    proper columns as defined by topJoin object SOREF.
.
   SELECT - Selection specification.
          - See TOP/PROJECT_AGENT for syntax of SELECT.
          - If topJoin object SOREF does not exist, create it with
	    selection specification string SELECT.
.
   WHERE - Where clause specification.
         - See TOP/JOIN_AGENT for syntax of WHERE.
         - If topJoin object SOREF does not exist, create it with
	   where clause specification string WHERE.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topJoin::JOIN
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Use an existing join object, j050 (defined by top/newjoin)
     to join selected rows of the unsorted ntphit and strack tables
     into table newhit.
.
 staf++ >   join j050 ntphit  ProducedData/Tracks/strack  newhit         
 Real time 00:00:00, CP time 0.010
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topJoin interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
   JOIN can be very slow for large tables. If your tables are sorted,
   please use FASTJOIN.
.
SEE ALSO: 
   TOP/PROJECT
   TOP/JOIN
.
>ACTION kam_topjoin_agent_join_%C
**
************************************************************************
** TOP/PROJECT_AGENT
>MENU \PROJECT_AGENT
>GUIDANCE
topProject object commands.
.
Commands found under the TOP/PROJECT_AGENT menu can be applied to
objects which implement the topProject interface.
.
A PROJECT operation in TOP is analogous to a project operation in any
Relational DataBase system. A topProject object takes as input a table
with columns a,b,c,e,f and projects it onto a table (new or extant)
with columns a,e,c (as an example). The definition of the projection is
contained in a selection specification string.
.
    +--------------------------------------+
    | Selection Specification              |
    +--------------------------------------+
    A selection specification is used to select columns for a
    table output from a project (or join and project).
.
    The syntax of a selection specification has the format:
        '{' select_column_specifier ',' .... '}'
    where a select_column_specifier is of the form:
        column_specifier
    or:
        column_specifier <WHITE_SPACE> new_column_name
    where a column_specifier is of the form:
        column_name
    or:
        table_name '.' column_name
.
    If a selection specification is applied for a project (or a
    join and project), the output table will contain only those
    columns specified by the selection specification.
.
    If no selection specification is applied for a project (or a 
    join and project), the output table will contain all columns
    from the input table(s).
.
    Examples:
        '{x, y, z}'
        '{x q1, y q2, z q3}'
        '{intab.x outab.q1, intab.y outab.q2, intab.z outab.q3}'
        '{g2t_vertex.id vid, g2t_track.ge_pid, g2t_track.n_tpc_hit}'
.
**
** ---------------------------------------------------------------------
** TOP/PROJECT_AGENT/SELECTSPEC SOREF
>COMMAND SELECTSPEC
>PARAMETERS
SOREF 'topProject object SORef' C
>GUIDANCE
Get the SELECTSPEC attribute of the topProject SOREF.
.
DESCRIPTION: 
.
SELECTSPEC is a readonly attribute which defines the mapping of input
table columns to output table columns for PROJECT operations performed
by the topProject SOREF. Readonly attributes cannot be changed from the
user interface.
.
SELECTSPEC is the SQL-like Selection Specification string for a
topProject object. See TOP/PROJECT_AGENT for syntax of Selection
Specification strings.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the topProject interface.
.
RETURN:
.
   None.
.
EXAMPLES: 
.
EG1. Show the current value of the SELECTSPEC attribute of
topProject "bob".
.
   StAF> TOP/PROJECT_AGENT/SELECTSPEC bob
   More guidance needed here.
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topProject interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
   TOP/PROJECT
.
>ACTION kam_topproject_agent_selectspec_%C
**
** ---------------------------------------------------------------------
** TOP/PROJECT_AGENT/PROJECT SOREF TABLE1 TABLE2 [ SELECT ]
>COMMAND PROJECT
>PARAMETERS
SOREF 'topProject object SORef' C
TABLE1  'tdmTable name of input table' C
TABLE2  'tdmTable name of output table' C
+
SELECT  'Selection specification' C D='-'
>GUIDANCE
Project columns of one table onto another.
.
DESCRIPTION: 
.
PROJECT is a member function of objects which implement the topProject
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topProject interface.
.
   TABLE1 - tdmTable name of input table.
.
   TABLE2 - tdmTable name of output table.
            Slashes are not allowed in this name, as a workaround
            for a certain Staf bug.
          - If TABLE2 does not exist, it will be created with the
	    proper columns as defined by topProject object SOREF.
.
   SELECT - Selection specification.
          - See TOP/PROJECT_AGENT for syntax of SELECT.
          - If topProject object SOREF does not exist, create it with
	    selection specification string SELECT.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topProject::PROJECT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topProject interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   Staf mishandles the name of the output table if it contains
   slashes.  The logic is complex, and the safest fix I can think
   of is to make slashes in such names illegal.  I will do this
   in July 98.  Herb.
.
SEE ALSO: 
.
>ACTION kam_topproject_agent_project_%C
**
************************************************************************
** TOP/SORT_AGENT
>MENU \SORT_AGENT
>GUIDANCE
topSort object commands.
.
Commands found under the TOP/SORT_AGENT menu can be applied to objects
which implement the topSort interface.
.
The SORT operation in TOP sorts a table on a single column of the
table.
.
**
** ---------------------------------------------------------------------
** TOP/SORT_AGENT/COLUMN SOREF
>COMMAND COLUMN
>PARAMETERS
SOREF 'topSort object SORef' C
>GUIDANCE
Get the COLUMN attribute of the topSort SOREF.
.
DESCRIPTION: 
.
COLUMN is a readonly attribute which defines the sorting order of SORT
operations performed by the topSort object SOREF. Readonly attributes
cannot be changed from the user interface.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the topSort interface.
.
RETURN:
.
   None.
.
EXAMPLES: 
.
EG1. Show the current value of the COLUMN attribute of topSort "bob".
.
   StAF> TOP/SORT_AGENT/COLUMN bob
   More guidance needed here.
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topSort interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_topsort_agent_column_%C
**
** ---------------------------------------------------------------------
** TOP/SORT_AGENT/SORT SOREF TABLE
>COMMAND SORT
>PARAMETERS
SOREF 'topSort object SORef' C
TABLE   'tdmTable name' C
>GUIDANCE
Sort table on column variable.
.
DESCRIPTION: 
.
SORT reorders the rows in a table so that the column denoted by the
COLUMN attribute of topSort object SOREF is sorted in ascending order.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topSort interface.
.
   TABLE - tdmTable name.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topSort::SORT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Use the sort agent s025, defined by TOP/NEWSORT to sort the
     table named.  
.
 staf++ >  TOP/SORT_AGENT/SORT s025 ProducedData/Hits/tphitau
 number of rows is 71.
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topSort interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_topsort_agent_sort_%C
*************************************************************************
>MENU \ARITHMETIC
>GUIDANCE
Simple arithmetic operations on tables.
>COMMAND OPERATE
>PARAMETERS
TABLE 'Name of table' C
COLUMN 'Name of column' C
OPERATION 'Either add, multiply, divide, or subtract' C
VALUE 'Value to apply' C
>GUIDANCE
Simple arithmetic operations on tables.
Usage is almost self-explanatory.
.
Example:
.
TOP/ARITHMETIC/OPERATE /dui/tpc/tphit id subtract 1
.
This would subtract 1 from all the values in the id column.
>ACTION kam_operate_%C
*************************************************************************

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
 #(@)$Id: top_def.cdf,v 1.8 1997/12/22 17:47:48 tull Exp $
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
   topObject - See TOP/OBJECT
	       - More guidance needed here.
.
More guidance needed here.
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
destructing an existing TOP worker object decrements COUNT by 1.
.
TOP worker objects include:
   More guidance needed here.
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
>ACTION KAM_TOP_COUNT
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
TOP worker objects include:
   More guidance needed here.
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
   StAF> TOP/LIST
   +-------------------------------------------------------------------
   |*********************** TOP - Not a valid TOP listing *************
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
>ACTION KAM_TOP_LIST
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
ARGUMENTS: 
.
   NAME - Case-sensitive alphanumeric name for new topCut object.
   - Use this name as part of SOREF (see SOC) to specify this particular
     topCut object in subsequent commands.
   - More guidance needed here.
.
   CUTFUNC - Cut function.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topFactory::newCut
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new topCut with NAME "bob"
.
   StAF> TOP/NEWCUT bob
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
   TOP/CUT
.
>ACTION KAM_TOP_NEWCUT
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
   - More guidance needed here.
.
   WHERE - Where clause specification.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topFactory::newJoin
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new topJoin with NAME "bob"
.
   StAF> TOP/NEWJOIN bob
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
   TOP/JOIN
.
>ACTION KAM_TOP_NEWJOIN
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
   - More guidance needed here.
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
>ACTION KAM_TOP_NEWPROJECT
**
** ---------------------------------------------------------------------
** TOP/NEWSORT NAME COLUMN
>COMMAND NEWSORT
>PARAMETERS
NAME 'Name for new topSort object' C
COLUMN  'Column name upon which to sort' C
>GUIDANCE
Create a new topSort object.
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
   - More guidance needed here.
.
   COLUMN - Column name upon which to sort.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topFactory::newSort
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new topSort with NAME "bob"
.
   StAF> TOP/NEWSORT bob
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
   TOP/SORT
.
>ACTION KAM_TOP_NEWSORT
**
************************************************************************
** TOP/CUT_AGENT
>MENU CUT_AGENT
>GUIDANCE
topCut_agent object commands.
.
Commands found under the TOP/CUT_AGENT menu can be applied to objects
which implement the topCut_agent interface.
.
More guidance needed here.
.
**
** ---------------------------------------------------------------------
** TOP/CUT_AGENT/FUNCTION SOREF
>COMMAND FUNCTION
>PARAMETERS
SOREF 'topCut_agent object SORef' C
>GUIDANCE
Get the FUNCTION attribute of the topCut_agent SOREF.
.
DESCRIPTION: 
.
FUNCTION is a readonly attribute which reflects the value of the FUNCTION
attribute of the topCut_agent SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the topCut_agent interface.
.
RETURN:
.
   The current value of FUNCTION is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the FUNCTION attribute of
    topCut_agent "bob".
.
   StAF> TOP/CUT_AGENT/FUNCTION bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topCut_agent interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TOPCUT_AGENT_FUNCTION
**
** ---------------------------------------------------------------------
** TOP/CUT_AGENT/CUT SOREF TABLE [ CUTFUNC ]
>COMMAND CUT
>PARAMETERS
SOREF 'topCut_agent object SORef' C
TABLE   'tdmTable name' C
+
CUTFUNC 'Cut function' C D='.'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
CUT is a member function of objects which implement the topCut_agent
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topCut_agent interface.
.
   TABLE - tdmTable name.
   - More guidance needed here.
.
   CUTFUNC - Cut function.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topCut_agent::CUT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the CUT method function of topCut_agent "bob"
     More guidance needed here.
.
   StAF> TOP/CUT_AGENT/CUT bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topCut_agent interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TOPCUT_AGENT_CUT
**
** ---------------------------------------------------------------------
** TOP/CUT_AGENT/FILTER SOREF TABLE1 TABLE2 [ CUTFUNC ]
>COMMAND FILTER
>PARAMETERS
SOREF 'topCut_agent object SORef' C
TABLE1  'tdmTable name of input table' C
TABLE2  'tdmTable name of output table' C
+
CUTFUNC 'Cut function' C D='.'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
FILTER is a member function of objects which implement the topCut_agent
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topCut_agent interface.
.
   TABLE1 - tdmTable name of input table.
   - More guidance needed here.
.
   TABLE2 - tdmTable name of output table.
   - More guidance needed here.
.
   CUTFUNC - Cut function.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topCut_agent::FILTER
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the FILTER method function of topCut_agent "bob"
     More guidance needed here.
.
   StAF> TOP/CUT_AGENT/FILTER bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topCut_agent interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TOPCUT_AGENT_FILTER
**
************************************************************************
** TOP/JOIN_AGENT
>MENU \JOIN_AGENT
>GUIDANCE
topJoin_agent object commands.
.
Commands found under the TOP/JOIN_AGENT menu can be applied to objects
which implement the topJoin_agent interface.
.
More guidance needed here.
.
**
** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/SELECTSPEC SOREF
>COMMAND SELECTSPEC
>PARAMETERS
SOREF 'topJoin_agent object SORef' C
>GUIDANCE
Get the SELECTSPEC attribute of the topJoin_agent SOREF.
.
DESCRIPTION: 
.
SELECTSPEC is a readonly attribute which reflects the value of the SELECTSPEC
attribute of the topJoin_agent SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the topJoin_agent interface.
.
RETURN:
.
   The current value of SELECTSPEC is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the SELECTSPEC attribute of
    topJoin_agent "bob".
.
   StAF> TOP/JOIN_AGENT/SELECTSPEC bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topJoin_agent interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TOPJOIN_AGENT_SELECTSPEC
**
** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/WHERECLAUSE SOREF
>COMMAND WHERECLAUSE
>PARAMETERS
SOREF 'topJoin_agent object SORef' C
>GUIDANCE
Get the WHERECLAUSE attribute of the topJoin_agent SOREF.
.
DESCRIPTION: 
.
WHERECLAUSE is a readonly attribute which reflects the value of the WHERECLAUSE
attribute of the topJoin_agent SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the topJoin_agent interface.
.
RETURN:
.
   The current value of WHERECLAUSE is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the WHERECLAUSE attribute of
    topJoin_agent "bob".
.
   StAF> TOP/JOIN_AGENT/WHERECLAUSE bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topJoin_agent interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TOPJOIN_AGENT_WHERECLAUSE
**
** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/FASTJOIN SOREF TABLE1 TABLE2 TABLE3 [ SELECT WHERE ]
>COMMAND FASTJOIN
>PARAMETERS
SOREF 'topJoin_agent object SORef' C
TABLE1  'tdmTable name of 1st input table' C
TABLE2  'tdmTable name of 2nd input table' C
TABLE3  'tdmTable name of output table' C
+
SELECT  'Selection specification' C D='-'
WHERE   'Where clause specification' C D='-'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
FASTJOIN is a member function of objects which implement the topJoin_agent
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topJoin_agent interface.
.
   TABLE1 - tdmTable name of 1st input table.
   - More guidance needed here.
.
   TABLE2 - tdmTable name of 2nd input table.
   - More guidance needed here.
.
   TABLE3 - tdmTable name of output table.
   - More guidance needed here.
.
   SELECT - Selection specification.
   - More guidance needed here.
.
   WHERE - Where clause specification.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topJoin_agent::FASTJOIN
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the FASTJOIN method function of topJoin_agent "bob"
     More guidance needed here.
.
   StAF> TOP/JOIN_AGENT/FASTJOIN bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topJoin_agent interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TOPJOIN_AGENT_FASTJOIN
**
** ---------------------------------------------------------------------
** TOP/JOIN_AGENT/JOIN SOREF TABLE1 TABLE2 TABLE3 [ SELECT WHERE ]
>COMMAND JOIN
>PARAMETERS
SOREF 'topJoin_agent object SORef' C
TABLE1  'tdmTable name of 1st input table' C
TABLE2  'tdmTable name of 2nd input table' C
TABLE3  'tdmTable name of output table' C
+
SELECT  'Selection specification' C D='-'
WHERE   'Where clause specification' C D='-'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
JOIN is a member function of objects which implement the topJoin_agent
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topJoin_agent interface.
.
   TABLE1 - tdmTable name of 1st input table.
   - More guidance needed here.
.
   TABLE2 - tdmTable name of 2nd input table.
   - More guidance needed here.
.
   TABLE3 - tdmTable name of output table.
   - More guidance needed here.
.
   SELECT - Selection specification.
   - More guidance needed here.
.
   WHERE - Where clause specification.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topJoin_agent::JOIN
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the JOIN method function of topJoin_agent "bob"
     More guidance needed here.
.
   StAF> TOP/JOIN_AGENT/JOIN bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topJoin_agent interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TOPJOIN_AGENT_JOIN
**
************************************************************************
** TOP/PROJECT_AGENT
>MENU \PROJECT_AGENT
>GUIDANCE
topProject_agent object commands.
.
Commands found under the TOP/PROJECT_AGENT menu can be applied to objects
which implement the topProject_agent interface.
.
More guidance needed here.
.
**
** ---------------------------------------------------------------------
** TOP/PROJECT_AGENT/SELECTSPEC SOREF
>COMMAND SELECTSPEC
>PARAMETERS
SOREF 'topProject_agent object SORef' C
>GUIDANCE
Get the SELECTSPEC attribute of the topProject_agent SOREF.
.
DESCRIPTION: 
.
SELECTSPEC is a readonly attribute which reflects the value of the SELECTSPEC
attribute of the topProject_agent SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the topProject_agent interface.
.
RETURN:
.
   The current value of SELECTSPEC is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the SELECTSPEC attribute of
    topProject_agent "bob".
.
   StAF> TOP/PROJECT_AGENT/SELECTSPEC bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topProject_agent interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TOPPROJECT_AGENT_SELECTSPEC
**
** ---------------------------------------------------------------------
** TOP/PROJECT_AGENT/PROJECT SOREF TABLE1 TABLE2 [ SELECT ]
>COMMAND PROJECT
>PARAMETERS
SOREF 'topProject_agent object SORef' C
TABLE1  'tdmTable name of input table' C
TABLE2  'tdmTable name of output table' C
+
SELECT  'Selection specification' C D='-'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
PROJECT is a member function of objects which implement the topProject_agent
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topProject_agent interface.
.
   TABLE1 - tdmTable name of input table.
   - More guidance needed here.
.
   TABLE2 - tdmTable name of output table.
   - More guidance needed here.
.
   SELECT - Selection specification.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topProject_agent::PROJECT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the PROJECT method function of topProject_agent "bob"
     More guidance needed here.
.
   StAF> TOP/PROJECT_AGENT/PROJECT bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topProject_agent interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TOPPROJECT_AGENT_PROJECT
**
************************************************************************
** TOP/SORT_AGENT
>MENU \SORT_AGENT
>GUIDANCE
topSort_agent object commands.
.
Commands found under the TOP/SORT_AGENT menu can be applied to objects
which implement the topSort_agent interface.
.
More guidance needed here.
.
**
** ---------------------------------------------------------------------
** TOP/SORT_AGENT/COLUMN SOREF
>COMMAND COLUMN
>PARAMETERS
SOREF 'topSort_agent object SORef' C
>GUIDANCE
Get the COLUMN attribute of the topSort_agent SOREF.
.
DESCRIPTION: 
.
COLUMN is a readonly attribute which reflects the value of the COLUMN
attribute of the topSort_agent SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the topSort_agent interface.
.
RETURN:
.
   The current value of COLUMN is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the COLUMN attribute of
    topSort_agent "bob".
.
   StAF> TOP/SORT_AGENT/COLUMN bob
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topSort_agent interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TOPSORT_AGENT_COLUMN
**
** ---------------------------------------------------------------------
** TOP/SORT_AGENT/SORT SOREF TABLE
>COMMAND SORT
>PARAMETERS
SOREF 'topSort_agent object SORef' C
TABLE   'tdmTable name' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
SORT is a member function of objects which implement the topSort_agent
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the topSort_agent interface.
.
   TABLE - tdmTable name.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   topSort_agent::SORT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the SORT method function of topSort_agent "bob"
     More guidance needed here.
.
   StAF> TOP/SORT_AGENT/SORT bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the topSort_agent interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_TOPSORT_AGENT_SORT
**

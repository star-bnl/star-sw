**######################################################################
**######################################################################
**:Copyright 1997, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        tnt_def.cdf
**:DESCRIPTION: Command Definition File for TNT package.
**:<--------------------------------------------------------------------
**
>NAME TNT_DEF
**
************************************************************************
** TNT
>MENU TNT
>GUIDANCE
Table_to_NTuple commands.
.
 #(@)$Id: tnt_def.cdf,v 1.7 1998/03/16 02:13:16 fisyak Exp $
 #Edited by Bill Love 23-25 Feb 98
.
TNT is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus architecture of StAF in a CORBA compliant
interface layer.
.
Each ASP is comprised of an object factory interface (eg. tntFactory)
and zero or more worker object interfaces.
.
TNT worker objects include:
   tntCWNtuple - See TNT/CWNTUPLE
      - A C++ wrapper for getting table data into and out of HBOOK
	Column-Wise Ntuples.
.
The TNT package provides an interface for importing DSL table data into
HBOOK Column-Wise Ntuples and for exporting HBOOK Column-Wise Ntuple
data to DSL tables (See notice below).
.
Although Column-Wise Ntuples do not gracefully handle all the data
types contained in DSL tables, data conversion is performed on import
and export to provide full functionality. Nonetheless, some DSL table
types cannot be mapped onto HBOOK Column-Wise Ntuples (N.B. Users can
use TOP/PROJECT to project data into an importable DSL table type.).
.
NOTICE: Exporting data from CWNtuples to tables is not yet implemented.
.
** ---------------------------------------------------------------------
** TNT/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show the current count of TNT worker objects.
.
DESCRIPTION: 
.
COUNT is a readonly long attribute which reflects the number of TNT
worker objects currently registered with the TNT object factory.
Constructing a new TNT worker object increments COUNT by 1,
destroying an existing TNT worker object decrements COUNT by 1.
.
TNT worker objects now include:
   tntCWNtuple.
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
EG1. Show the current count of TNT worker objects.
.
   StAF> TNT/COUNT
   TNT:    Object count = 18
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_tnt_count_%C
**
** ---------------------------------------------------------------------
** TNT/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all currently registered TNT worker objects.
.
DESCRIPTION: 
.
Show in a table a one-line description for each TNT worker object currently
registered with the TNT object factory.
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
	   for tntCWNtuples this is a blank.
.
TNT worker objects so far include only tntCWNtuple:
.
ARGUMENTS: 
.
   None.
.
RETURN: 
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tntFactory::list()
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. List all currently registered TNT worker objects.
.
  staf++ > tnt/list
 +-------+-----------------+-----------------+---------------
 | IDREF | NAME:OBJECT     | TYPE:CLASS      | DESCRIPTION   
 +-------+-----------------+-----------------+---------------
 |   105 | tntCWNtuple100  | tntCWNtuple     |                
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO:
.
Ntuple/list 
.
>ACTION kam_tnt_list_%C
**
** ---------------------------------------------------------------------
** TNT/NEWCWNTUPLE HID TABLE
>COMMAND NEWCWNTUPLE
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
TABLE   'tdmTable name' C
>GUIDANCE
Create a new tntCwntuple object.  Fill it with the current TABLE contents.
.
DESCRIPTION: 
.
Each tntCwntuple created by the tntFactory shows up as an object
managed by the tntFactory (see TNT/COUNT and TNT/LIST) and
registered with the socCatalog (see SOC/COUNT and SOC/LIST).
The names of the HBOOK ntuple columns are those of the table columns.  Each
row of the table produces an entry in the Column Wise Ntuple.
.
ARGUMENTS: 
.
   HID  - HBOOK ID of the Ntuple. 
   - Use this HID to specify this particular
     tntCwntuple object in subsequent commands.
   - Currently HBOOK ntuple ID's are integers.
.
   TABLE - tdmTable name.
   - Name of the StAF table to be loaded into the HBOOK ntuple.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tntFactory::newCwntuple
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new tntCwntuple from table "bob"
.
   StAF> TNT/NEWCWNTUPLE 100 bob
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
   TNT/CWNTUPLE
.
>ACTION kam_tnt_newcwntuple_%C
**
************************************************************************
** TNT/CWNTUPLE
>MENU CWNTUPLE
>GUIDANCE
tntCwntuple object commands.
.
Commands found under the TNT/CWNTUPLE menu can be applied to objects
which implement the tntCwntuple interface.  Currently refers to HBOOK
Columnwise Ntuples.
.
**
** ---------------------------------------------------------------------
** TNT/CWNTUPLE/HID HID
>COMMAND HID
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
>GUIDANCE
Get the HID attribute of the tntCwntuple HID.  Useful?
.
DESCRIPTION: 
.
HID is a readonly attribute which reflects the value of the HID
attribute of the tntCwntuple HID. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   HID - HBOOK ID for CWNtuple.
   -  denoting an object implementing the tntCwntuple interface.
.
RETURN:
.
   The current value of HID is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the HID attribute of
    tntCwntuple 100.
.
   StAF> TNT/CWNTUPLE/HID 100
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by HID can be found which
      implements the tntCwntuple interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_tntcwntuple_hid_%C
**
** ---------------------------------------------------------------------
** TNT/CWNTUPLE/TITLE HID
>COMMAND TITLE
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
>GUIDANCE
Get the TITLE attribute of the tntCwntuple HID.
.
DESCRIPTION: 
.
TITLE is a readonly attribute which reflects the value of the TITLE
attribute of the tntCwntuple HID. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   HID - HBOOK ID for CWNtuple.
   -  denoting an object implementing the tntCwntuple interface.
.
RETURN:
.
   The current value of TITLE is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the TITLE attribute of
    tntCwntuple 100.
.
   StAF> TNT/CWNTUPLE/TITLE 100
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by HID can be found which
      implements the tntCwntuple interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_tntcwntuple_title_%C
**
** ---------------------------------------------------------------------
** TNT/CWNTUPLE/ENTRYCOUNT HID
>COMMAND ENTRYCOUNT
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
>GUIDANCE
Get the ENTRYCOUNT attribute of tntCwntuple HID. I.e. number of rows filled.
.
DESCRIPTION: 
.
ENTRYCOUNT is a readonly attribute which reflects the value of the ENTRYCOUNT
attribute of the tntCwntuple HID. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   HID - HBOOK ID for CWNtuple.
   -  denoting an object implementing the tntCwntuple interface.
.
RETURN:
.
   The current value of ENTRYCOUNT is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the ENTRYCOUNT attribute of
    tntCwntuple 100.
.
   StAF> TNT/CWNTUPLE/ENTRYCOUNT 100
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by HID can be found which
      implements the tntCwntuple interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_tntcwntuple_entrycount_%C
**
** ---------------------------------------------------------------------
** TNT/CWNTUPLE/COLUMNCOUNT HID
>COMMAND COLUMNCOUNT
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
>GUIDANCE
Get the COLUMNCOUNT attribute of the tntCwntuple HID.
.
DESCRIPTION: 
.
COLUMNCOUNT is a readonly attribute which reflects the value of the COLUMNCOUNT
attribute of the tntCwntuple HID. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
ARGUMENTS: 
.
   HID - HBOOK ID for CWNtuple.
   -  denoting an object implementing the tntCwntuple interface.
.
RETURN:
.
   The current value of COLUMNCOUNT is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the COLUMNCOUNT attribute of
    tntCwntuple 100.
.
   StAF> TNT/CWNTUPLE/COLUMNCOUNT 100
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by HID can be found which
      implements the tntCwntuple interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_tntcwntuple_columncount_%C
**
** ---------------------------------------------------------------------
** TNT/CWNTUPLE/APPEND HID TABLE
>COMMAND APPEND
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
TABLE   'tdmTable name' C
>GUIDANCE
Add the contents of the table to the current contents of the ntuple.
.
DESCRIPTION: 
.
APPEND is a member function of objects which implement the tntCwntuple
interface.  The contents of the table are added to the existing contents
of the ntuple. 
.
ARGUMENTS: 
.
   HID - HBOOK ID for CWNtuple.
   - denoting an object implementing the tntCwntuple interface.
.
   TABLE - tdmTable name.
   - A table whose type matches the definition of the ntuple.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tntCwntuple::APPEND
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the APPEND method function of tntCwntuple 100 on table bob
.
   StAF> TNT/CWNTUPLE/APPEND 100 bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by HID can be found which
      implements the tntCwntuple interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_tntcwntuple_append_%C
**
** ---------------------------------------------------------------------
** TNT/CWNTUPLE/IMPORT HID TABLE
>COMMAND IMPORT
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
TABLE   'tdmTable name' C
>GUIDANCE
Load the contents of a table into an existing Ntuple.  
.
DESCRIPTION: 
.
IMPORT is a member function of objects which implement the tntCwntuple
interface. Though the name seems backwards, it loads a table into an
existing ntuple, not the reverse.  Previous contents of the ntuple are
replaced.  The ntuple definition and the table type must match.
.
ARGUMENTS: 
.
   HID - HBOOK ID for CWNtuple.
   - denoting an ntuple object.
.
   TABLE - tdmTable name.
   - A StAF table of a type whose columns match those of the ntuple.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tntCwntuple::IMPORT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Replace contentes of ntuple 20 with table "bob"
.
   StAF> TNT/CWNTUPLE/IMPORT 20 bob 
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by HID can be found which
      implements the tntCwntuple interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   If the specified table does not exist, IMPORT is a quiet no-op.
   Similarly, if the table type is wrong, no message, no action.
   Despite the Exception advertised above, if the ntuple doesn't
   exist, no message appears.
.
SEE ALSO:
 TNT/NEWCWNTUPLE, TNT/CWNTUPLE/APPEND
.
>ACTION kam_tntcwntuple_import_%C
**

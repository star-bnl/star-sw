**######################################################################
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
 #(@)$Id: tnt_def.cdf,v 1.4 1997/12/22 17:46:25 tull Exp $
.
TNT is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus archictecture of StAF in a CORBA compliant
interface layer.
.
Each ASP is comprised of an object factory interface (eg. tntFactory)
and zero or more worker object interfaces.
.
TNT worker objects include:
   tntObject - See TNT/OBJECT
	       - More guidance needed here.
.
More guidance needed here.
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
destructing an existing TNT worker object decrements COUNT by 1.
.
TNT worker objects include:
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
>ACTION KAM_TNT_COUNT
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
Show a one-line description for each TNT worker object currently
registered with the TNT object factory in a table for quick,
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
TNT worker objects include:
   More guidance needed here.
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
   StAF> TNT/LIST
   +-------------------------------------------------------------------
   |*********************** TNT - Not a valid TNT listing *************
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
>ACTION KAM_TNT_LIST
**
** ---------------------------------------------------------------------
** TNT/NEWCWNTUPLE HID TABLE
>COMMAND NEWCWNTUPLE
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
TABLE   'tdmTable name' C
>GUIDANCE
Create a new tntCwntuple object.
.
DESCRIPTION: 
.
Each tntCwntuple created by the tntFactory shows up as an object
managed by the tntFactory (see TNT/COUNT and TNT/LIST) and
registered with the socCatalog (see SOC/COUNT and SOC/LIST).
.
ARGUMENTS: 
.
   HID 'More guidance needed here' I 
   - Use this HID to specify this particular
     tntCwntuple object in subsequent commands.
   - More guidance needed here.
.
   TABLE - tdmTable name.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tntFactory::newCwntuple
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Create a new tntCwntuple with NAME "bob"
.
   StAF> TNT/NEWCWNTUPLE bob
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
   TNT/CWNTUPLE
.
>ACTION KAM_TNT_NEWCWNTUPLE
**
************************************************************************
** TNT/CWNTUPLE
>MENU CWNTUPLE
>GUIDANCE
tntCwntuple object commands.
.
Commands found under the TNT/CWNTUPLE menu can be applied to objects
which implement the tntCwntuple interface.
.
More guidance needed here.
.
**
** ---------------------------------------------------------------------
** TNT/CWNTUPLE/HID HID
>COMMAND HID
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
>GUIDANCE
Get the HID attribute of the tntCwntuple HID.
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
    tntCwntuple "bob".
.
   StAF> TNT/CWNTUPLE/HID bob
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
>ACTION KAM_TNTCWNTUPLE_HID
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
    tntCwntuple "bob".
.
   StAF> TNT/CWNTUPLE/TITLE bob
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
>ACTION KAM_TNTCWNTUPLE_TITLE
**
** ---------------------------------------------------------------------
** TNT/CWNTUPLE/ENTRYCOUNT HID
>COMMAND ENTRYCOUNT
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
>GUIDANCE
Get the ENTRYCOUNT attribute of the tntCwntuple HID.
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
    tntCwntuple "bob".
.
   StAF> TNT/CWNTUPLE/ENTRYCOUNT bob
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
>ACTION KAM_TNTCWNTUPLE_ENTRYCOUNT
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
    tntCwntuple "bob".
.
   StAF> TNT/CWNTUPLE/COLUMNCOUNT bob
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
>ACTION KAM_TNTCWNTUPLE_COLUMNCOUNT
**
** ---------------------------------------------------------------------
** TNT/CWNTUPLE/APPEND HID TABLE
>COMMAND APPEND
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
TABLE   'tdmTable name' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
APPEND is a member function of objects which implement the tntCwntuple
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   HID - HBOOK ID for CWNtuple.
   - denoting an object implementing the tntCwntuple interface.
.
   TABLE - tdmTable name.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tntCwntuple::APPEND
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the APPEND method function of tntCwntuple "bob"
     More guidance needed here.
.
   StAF> TNT/CWNTUPLE/APPEND bob 
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
>ACTION KAM_TNTCWNTUPLE_APPEND
**
** ---------------------------------------------------------------------
** TNT/CWNTUPLE/IMPORT HID TABLE
>COMMAND IMPORT
>PARAMETERS
HID     'HBOOK ID for CWNtuple.' I
TABLE   'tdmTable name' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
IMPORT is a member function of objects which implement the tntCwntuple
interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   HID - HBOOK ID for CWNtuple.
   - denoting an object implementing the tntCwntuple interface.
.
   TABLE - tdmTable name.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tntCwntuple::IMPORT
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Invoke the IMPORT method function of tntCwntuple "bob"
     More guidance needed here.
.
   StAF> TNT/CWNTUPLE/IMPORT bob 
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
>ACTION KAM_TNTCWNTUPLE_IMPORT
**

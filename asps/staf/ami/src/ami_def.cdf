**######################################################################
**######################################################################
**######################################################################
**:Copyright 1997, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        ami_def.cdf
**:DESCRIPTION: Command Definition File for AMI package.
**:<--------------------------------------------------------------------
**
>NAME AMI_DEF
**
************************************************************************
** AMI
>MENU AMI
>GUIDANCE
Analysis_Module_Invoker commands.
.
 #(@)$Id: ami_def.cdf,v 1.7 1998/03/16 00:49:13 fisyak Exp $
.
AMI is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus architecture of StAF in a CORBA compliant
interface layer.
.
Each ASP is comprised of an object factory interface (eg. amiFactory)
and zero or more worker object interfaces.
.
AMI worker objects include:
   amiInvoker - See AMI/MODULE
      - An object for invoking user written analysis module functions.
.
AMI provides a table-based interface to user code written in FORTRAN,
C, or C++ (see AMI/MODULE).
.
** ---------------------------------------------------------------------
** AMI/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show the current count of AMI worker objects.
.
DESCRIPTION: 
.
COUNT is a readonly long attribute which reflects the number of AMI
worker objects currently registered with the AMI object factory.
Constructing a new AMI worker object increments COUNT by 1,
destroying an existing AMI worker object decrements COUNT by 1.
.
AMI worker objects include:
   amiInvoker - See AMI/MODULE
      - An object for invoking user written analysis module functions.
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
EG1. Show the current count of AMI worker objects.
.
   StAF> AMI/COUNT
   AMI:    Object count = 5
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_ami_count_%C
**
** ---------------------------------------------------------------------
** AMI/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all currently registered AMI worker objects.
.
DESCRIPTION: 
.
Show a one-line description for each AMI worker object currently
registered with the AMI object factory in a table format.
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
	   For AMI objects the number of table arguments is listed.
.
AMI worker objects include:
   amiInvoker - See AMI/MODULE
      - An object for invoking user written analysis module functions.
.
ARGUMENTS: 
.
   None.
.
RETURN: 
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   amiFactory::list()
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. List all current AMI worker objects.
.
 staf++ > ami/list

 +---------------------------------------------------------------------
 |****************** AMI - Analysis Module Interface listing **********
 +-------+-----------------+-----------------+-------------------------
 | IDREF | NAME:OBJECT     | TYPE:CLASS      | DESCRIPTION             
 +-------+-----------------+-----------------+-------------------------
 |     9 | tcl_mak~lusters | amiInvoker      | 6 arg.s                 
 |    10 | tpeam           | amiInvoker      | 13 arg.s                
 |    11 | tpham           | amiInvoker      | 10 arg.s                
 |    12 | reformat        | amiInvoker      | 13 arg.s                
 |    13 | tfc_calc_delta  | amiInvoker      | 2 arg.s                 
 |    14 | tfc_stability   | amiInvoker      | 4 arg.s                 
 |    15 | tstam           | amiInvoker      | 8 arg.s                 
 |    16 | tstgain         | amiInvoker      | 5 arg.s                 
 |    17 | xyz             | amiInvoker      | 5 arg.s                 
 |    18 | tpg_main        | amiInvoker      | 3 arg.s                 
 |    19 | tpt             | amiInvoker      | 4 arg.s                 
 |    20 | tpt_sts         | amiInvoker      | 6 arg.s                 
 +-------+-----------------+-----------------+-------------------------
                                                                     
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_ami_list_%C
**
** ---------------------------------------------------------------------
** AMI/CALL PAM [ TABLES ]
>COMMAND CALL
>PARAMETERS
PAM     'Physics Analysis Module function name.' C
+
TABLES  'List of PAM argument tables.' C D='-'
>GUIDANCE
OBSOLETE -- PLEASE USE AMI/MODULE/CALL.
>ACTION kam_ami_call_%C
**
************************************************************************
** AMI/MODULE
>MENU MODULE
>GUIDANCE
amiModule object commands.
.
Commands found under the AMI/MODULE menu can be applied to objects
which implement the amiModule interface.
.
amiModules are the "physics-algorithm objects" within StAF.
.
The amiModule class provides a standard C++ object interface to
user-written PAMs. Each amiModule object corresponds to a different PAM
function.
.
A Physics Analysis Module (PAM) is a user-written function in C, C++,
or FORTRAN which adheres to one of the StAF PAM APIs.  Currently there
is only one API for PAMs in StAF. A PAM which adheres to this API is
called a type 1 PAM
.
**
** ---------------------------------------------------------------------
** AMI/MODULE/RANK SOREF
>COMMAND RANK
>PARAMETERS
SOREF 'amiModule object SORef' C
>GUIDANCE
Get the RANK of a Module.  I.e. the number of call arguments in its Module definition .idl file. 
.
DESCRIPTION: 
.
RANK is a readonly attribute which reflects the value of the RANK
attribute of the amiModule SOREF. Readonly attributes cannot be changed
from the user interface.
.
NB. Readonly attributes are not necessarily static attributes.
.
The rank of an analysis module function is determined by the IDL
(Interface Definition Language) definition of the analysis module
function interface.  In short, the RANK is the number of tables in
the PAM's call list. 
.
The IDL file:
.
   /* connect_the_dots.idl */
   #include "PAM.idl"   // generic include
   #include "point.idl" // point table type definition
   #include "line.idl"  // line table type definition
   interface connect_the_dots : amiModule {
      STAFCV_T call (
         in point beg_pts, // input table of type point
         in point end_pts, // input table of type point
         out line lines    // output table of type line
      );
   };
.
... defines an interface which takes two tables of type point as input
and outputs one table of type line. This analysis module has a rank of
three (i.e. It takes three tables as arguments.).
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   -  denoting an object implementing the amiModule interface.
.
RETURN:
.
   The current value of RANK is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current value of the RANK attribute of amiModule "pamf".
.
   Kuip> AMI/MODULE/RANK pamf
   AMI:    Analysis module rank = 2
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the amiModule interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_amimodule_rank_%C
**
** ---------------------------------------------------------------------
** AMI/MODULE/CALL SOREF [ TABLES ]
>COMMAND CALL
>PARAMETERS
SOREF 'amiModule object SORef' C
+
TABLES  'List of PAM argument tables.' C D='-'
>GUIDANCE
The way to get work done.  CALL executes the PAM (SOREF) and specifies
what tables it will operate on.
.
DESCRIPTION: 
.
CALL is a member function of objects which implement the amiModule
interface.
.
Invoke a Physics Analysis Module on data tables. IN and INOUT tables
must exist before being passed to the PAM.  OUT tables can, but need
not, exist.   Non-existent OUT tables will be created at invokation
of the PAM by CALL.
.
Non-existent OUT tables can be created with a user-specified memory
allocation by specifying the number of rows to be allocated in
parenthesis after the table name (The default is to allocate only one
row's worth of memory.).
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the amiModule interface.
.
   TABLES - List of PAM argument tables.
   - A list of memory-resident data tables, by name, upon which to
     invoke the Physics Analysis Module function.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   amiModule::CALL
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Call an example analysis module function.
.
   Kuip> AMI/MODULE/CALL pam tab1 tab2(4000) tab3
   If tab1 exists, and tab2 and tab3 are output tables, this command
   will create tab2 with 4000 rows allocated and tab3 with 1 row
   allocated, and then call pam on the three tables.
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the amiModule interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_amimodule_call_%C
**
** ---------------------------------------------------------------------
** AMI/MODULE/SHOW SOREF
>COMMAND SHOW
>PARAMETERS
SOREF 'amiModule object SORef' C
>GUIDANCE
Show definition of Analysis Module invoker.  I.e. show the tables
expected in calling the PAM.
.
DESCRIPTION: 
.
SHOW is a member function of objects which implement the amiModule
interface.
.
The definition of a PAM shows the number, types, and I/O modes of the
data tables upon which the PAM operates.
.
ARGUMENTS: 
.
   SOREF - Stringified Object REFerence (see SOC).
   - denoting an object implementing the amiModule interface.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   amiModule::SHOW
   method is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Show the definition of amiModule "pamf".
.
   Kuip> AMI/MODULE/SHOW pamf
   AMI:    Table Specification = ...
   struct scalars {
           short aShort;
           unsigned short aUshort;
           long aLong;
           unsigned long aUlong;
           char aChar;
           octet aOctet;
           float aFloat;
           double aDouble;
   }; .
   AMI:    Table Specification = ...
   struct vectors {
           short bShorts[3];
           unsigned short bUshorts[3];
           long bLongs[3];
           unsigned long bUlongs[3];
           char bChars[3];
           octet bOctets[3];
           float bFloats[3];
           double bDoubles[3];
   }; .
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the amiModule interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_amimodule_show_%C 
**



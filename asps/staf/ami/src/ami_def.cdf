**:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        ami_def.cdf
**:DESCRIPTION: Command Definition File for AMI
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:HISTORY:	30apr96-v100a-cet- Beta Release Version
**:HISTORY:     16dec95-v001a-cet- Update
**:HISTORY:     07jul95-v000a-cet- Creation
**:<--------------------------------------------------------------------

>NAME AMI_DEF

************************************************************************
************************************************************************
** AMI
>MENU AMI
>Guidance
User interface commands for the Analysis Module Invoker ASP.
.
 #(@)$Id: ami_def.cdf,v 1.2 1997/08/18 22:33:56 tull Exp $
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** AMI/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show the current count of AMI worker objects.
.
DESCRIPTION: 
COUNT is a readonly long attribute which reflects the number of AMI
worker objects currently instantiated by the AMI object factory.
Constructing a new ami worker object adds 1 to COUNT, destructing an
existing ami worker object subtracts 1 from COUNT.
.
Ami worker objects include:
   amiModule - See AMI/MODULE.
.
ARGUMENTS: 
   None.
.
EXAMPLE: 
.
EXCEPTIONS: 
.
BUGS: 
   None known.
.
SEE ALSO: 
>ACTION KAM_AMI_COUNT

** ---------------------------------------------------------------------
** AMI/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all currently instantiated AMI worker objects.
.
DESCRIPTION: 
Show a one-line description for each AMI worker object currently
instantiated by the AMI object factory.
.
ARGUMENTS: 
   None.
.
EXAMPLE: 
.
EXCEPTIONS: 
.
BUGS: 
   None known.
.
SEE ALSO: 
>ACTION KAM_AMI_LIST

** ---------------------------------------------------------------------
** AMI/CALL SOREF [TABLES]
>COMMAND CALL
>PARAMETERS
SOREF 'Stringified Object REFerence.' C
+
TABLES 'Argument table list.' C D='-'
>GUIDANCE
Invoke a Physics Analysis Module on data tables.
.
DESCRIPTION: 
Invoke a Physics Analysis Module on pre-existing and/or non-existant 
data tables. IN and INOUT tables must exist before being passed to the
Physics Analysis Module. OUT tables can, but need not, exist before
being passed to the Physics Analysis Module. Non-existant OUT tables
will be created at invokation of the Physics Analysis Module.
.
Non-existant OUT tables can be created with a user-specified memory
allocation by specifying the number of rows to be allocated in
parenthisis after the table name (The default is to allocate only one
row's worth of memory.).
.
ARGUMENTS: 
   SOREF - Stringified Object REFerence
      ... of the Physics Analysis Module to invoke.
      See help for SOC for details.
   TABLES - Argument table list
      A list of memory-resident data tables, by name, upon which to
      invoke the Physics Analysis Module.
.
EXAMPLE: 
   AMI/CALL pam tab1 tab2(4000) tab3
.
   If tab1 exists, and tab2 and tab3 are output tables, this command
   will create tab2 with 4000 rows allocated and tab3 with 1 row 
   allocated, and then call pam on the three tables.
.
EXCEPTIONS: 
.
BUGS: 
   None known.
.
SEE ALSO: 
>ACTION KAM_AMI_CALL

************************************************************************
************************************************************************
** AMI/MODULE
>MENU MODULE
>Guidance
User interface commands for amiModule objects.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** AMI/MODULE/RANK SOREF
>COMMAND RANK
>PARAMETERS
SOREF 'Stringified Obect Reference.' C
>GUIDANCE
Show rank of Physics Analysis Module.
.
The rank of a physics analysis module is a readonly attribute of the
amiModule class and all its derived classes. The rank of an analysis
module is determined by the IDL (Interface Definition Language)
definition of the analysis module interface.
.
The rank of a physics analysis module is the number of tables passed as
arguments to the physics analysis module at invokation.
.
PARAMETERS:
   SOREF - Stringified Object REFerence
   See SOC help for details.
.
EXAMPLE:
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
   ... defines an interface which takes two tables of type point as
   input and outputs one table of type line. This analysis module has a
   rank of three (i.e. It takes three tables as arguments.).
.
EXCEPTIONS:
   OBJECT_NOT_FOUND - The object specified by SOREF is currently
   unavailable. Please see SOC/BIND to dynamically bind the proper
   resources, or rebuild executable with proper resources statically
   linked.
.
>ACTION KAM_AMIINVOKER_RANK

** ---------------------------------------------------------------------
** AMI/MODULE/SHOW PAM [ OPTION ]
>COMMAND SHOW
>PARAMETERS
PAM 'Physics Analysis Module.' C
+
OPTION 'Options' C D=' ' R=' ,P,T,S'
>GUIDANCE
Show definition of Physics Analysis Module.
.
Possible OPTION values are:
   ' '   Show everything.
    P    Show call prototype.
    T    Show table type names.
    S    Show table type specifiers.
.
 ************************
 * Still In Development *
 ************************
.
>ACTION KAM_AMIINVOKER_SHOW


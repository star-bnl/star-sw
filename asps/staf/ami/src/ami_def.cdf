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
Analysis Module Interface commands.
.
VERSION v1.00a (30apr96).
.
Commands for the Analysis Module Interface ASP.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** AMI/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
Show count of known AMI objects.
.
>ACTION KAM_AMI_COUNT

** ---------------------------------------------------------------------
** AMI/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all AMI objects.
.
>ACTION KAM_AMI_LIST

** ---------------------------------------------------------------------
** AMI/CALL PAM [TABLES]
>COMMAND CALL
>PARAMETERS
PAM 'Physics Analysis Module.' C
+
TABLES 'Argument table list.' C D='-'
>GUIDANCE
Call a Physics Analysis Module.
.
Non-existant output tables can be created with a user-specified memory
allocation by specifying the number of rows to be allocated in
parenthisis after the table name.
.
Example:
    AMI/CALL pam tab1 tab2(4000) tab3(50)
.
        If tab1 exists, and tab2 and tab3 are output tables, this
	command will create tab2 with 4000 rows allocated and tab3 with
	50 rows allocated, and then call pam on the three tables.
.
 ************************
 * Still In Development *
 ************************
.
>ACTION KAM_AMI_CALL

************************************************************************
************************************************************************
** AMI/MODULE
>MENU MODULE
>Guidance
AMI commands for PAMs.
.
PAM == Physics Analysis Module.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** AMI/MODULE/RANK SOREF
>COMMAND RANK
>PARAMETERS
SOREF 'Stringified Obect Reference.' C
>GUIDANCE
Show rank of Physics Analysis Module "SOREF".
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
	SOREF - A stringified object reference (see SOC) specifying a
	particular object of the amiModule class.
.
EXAMPLE:
	The IDL file: 
.
	#include "PAMI.h"
	#include "point.h"
	#include "line.h"
	interface connect_the_dots : amiModule {
		STAFCV_T call (
			in point beg_pts,
			in point end_pts,
			out line lines
		);
	};
.
	... defines an interface which takes two tables of type point
	as input and outputs one table of type line. This analysis
	module has a rank of three (3).
.
EXCEPTIONS:
	OBJECT_NOT_FOUND - The analysis module named "PAM" is not
	currently available in the system. Please see SOC/BIND to
	dynamically bind the proper resources, or rebuild executable
	with proper resources statically linked.
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


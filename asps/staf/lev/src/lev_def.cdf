**:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        lev_def.cdf
**:DESCRIPTION: Command Definition File for LEV
**:AUTHOR:      hjw - Herb Ward, ward@physics.utexas.edu
**:HISTORY:     01jul96-v000a-hjw- Creation
**:<--------------------------------------------------------------------

>NAME LEV_DEF

************************************************************************
************************************************************************
** LEV
>MENU LEV
>Guidance
Logging of Environmental and Version info commands.
.
VERSION v0.00a (05jul96)
.
Commands for the  Logging of Environmental and Version info ASP.
.
All commands and functions should be considered "Alpha Software".
Commands and functionality are subject to change without notice.
.
Some commands may be marked with the following banners.
.
 ************************       Meaning that the command is merely a
 * Not Yet Implemented  *       placeholder command. The commmand does
 ************************       not have any functionality.
.
 ************************       Meaning that the software underlying
 * Still In Development *       the command is in development and,
 ************************       hence, unstable and evolving.
.
************************************************************************
************************************************************************
** ---------------------------------------------------------------------
** LEV/UPDATE
>COMMAND UPDATE
>PARAMETERS
>GUIDANCE
Updates the table of versions (config/levVer) with respect to 
recently added STAF objects [eg, physics analysis modules (PAMs), tables, 
ASPs, etc.].
This table of versions is written into output files.
Do not confuse the table of versions with the LEV table
that holds the environmental info (config/levEnv).
Almost all of config/levEnv (except
for timestamps for various events (eg end time), 
is written automatically.
.
 ************************
 * Still In Development *
 ************************
.
>ACTION kam_lev_update_%C
** ---------------------------------------------------------------------
** LEV/REGISTER_VERSION NAME VERSION
>COMMAND REGISTER_VERSION
>PARAMETERS
NAME 'name of kumac file' C
VERSION 'version string from CVS' C
>GUIDANCE
This command is meant to register the version of a kumac file.
.
SPECIAL NOTE: Please convert the @ signs below to $ signs.  This
maneuver is because STAF's KUIP cdf files go thru CVS.
.
This command is meant to register the version of a kumac file
into LEV's table of versions for inclusion into the final output file.
Thus, if the name of the kumac file is cat_fude.kumac, 
you would insert the following into it:  
LEV/REGISTER_VERSION cat_fude.kumac @Id:@ 
The @Id:@ will be overwritten by CVS with an informative version 
'timestamp' the first time you check the package in (assuming that you have 
run sl_add_file for the kumac file).  You do not have to sl_retrieve_pkg to 
have the @Id:@ overwritten, only sl_store_pkg.  Further, you don't have to 
change it back to @Id:@ every time you want to update the overwrite.
.
 ************************
 * Still In Development *
 ************************
.
>ACTION kam_lev_register_version_%C



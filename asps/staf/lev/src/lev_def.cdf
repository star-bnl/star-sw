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
Updates the table of versions (config/levVersions) with respect to 
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
>ACTION KAM_LEV_UPDATE



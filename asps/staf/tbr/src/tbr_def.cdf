**######################################################################
**######################################################################
**######################################################################
**:Copyright 1997, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        tbr_def.cdf
**:DESCRIPTION: Command Definition File for TBR package.
**:<--------------------------------------------------------------------
**
>NAME TBR_DEF
**
************************************************************************
** TBR
>MENU TBR
>GUIDANCE
Table_BRowser commands.
.
 #(@)$Id: tbr_def.cdf,v 1.7 1998/03/16 02:00:19 fisyak Exp $
.
TBR is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus archictecture of StAF in a CORBA compliant
interface layer.
.
The TBR package is an ASP-like interface to the Motif Table BRowser.
It is intended to provide an interactive GUI to view and examine a DSL
hierarchy.
.
** ---------------------------------------------------------------------
** TBR/VIEWDATASET
>COMMAND VIEWDATASET
>PARAMETERS
>GUIDANCE
Launch the Motif Table BRowser and view dataset hierarchy.
.
DESCRIPTION: 
.
More guidance needed here.
.
ARGUMENTS: 
.
   None.
.
RETURN:
.
EXAMPLES: 
.
EG1. Launch the Motif Table BRowser and view dataset hierarchy.
.
   StAF> TBR/VIEWDATASET
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_tbr_viewdataset_%C
**

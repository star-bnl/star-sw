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
 #(@)$Id: tbr_def.cdf,v 1.4 1997/12/22 17:46:53 tull Exp $
.
TBR is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus archictecture of StAF in a CORBA compliant
interface layer.
.
Each ASP is comprised of an object factory interface (eg. tbrFactory)
and zero or more worker object interfaces.
.
TBR worker objects include:
   tbrObject - See TBR/OBJECT
	       - More guidance needed here.
.
More guidance needed here.
.
** ---------------------------------------------------------------------
** TBR/VIEWDATASET
>COMMAND VIEWDATASET
>PARAMETERS
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
VIEWDATASET is a member function of the tbrFactory interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   None.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   tbrFactory::VIEWDATASET
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
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
>ACTION KAM_TBR_VIEWDATASET
**

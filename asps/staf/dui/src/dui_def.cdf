**######################################################################
**######################################################################
**######################################################################
**:Copyright 1997, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        dui_def.cdf
**:DESCRIPTION: Command Definition File for DUI package.
**:<--------------------------------------------------------------------
**
>NAME DUI_DEF
**
************************************************************************
** DUI
>MENU DUI
>GUIDANCE
Dataset_Unix_like_Interface commands.
.
 #(@)$Id: dui_def.cdf,v 1.5 1997/12/22 17:41:12 tull Exp $
.
DUI is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus archictecture of StAF in a CORBA compliant
interface layer.
.
Each ASP is comprised of an object factory interface (eg. duiFactory)
and zero or more worker object interfaces.
.
DUI worker objects include:
   duiObject - See DUI/OBJECT
	       - More guidance needed here.
.
More guidance needed here.
.
** ---------------------------------------------------------------------
** DUI/CD [ PATH ]
>COMMAND CD
>PARAMETERS
+
PATH    'Unix-like dataset path' C D='/dui'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
CD is a member function of the duiFactory interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   PATH - Unix-like dataset path
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::CD
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> DUI/CD
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DUI_CD
**
** ---------------------------------------------------------------------
** DUI/CP SOURCE TARGET
>COMMAND CP
>PARAMETERS
SOURCE  'Source table name' C
TARGET  'Target table/dataset name' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
CP is a member function of the duiFactory interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOURCE - Source table name.
   - More guidance needed here.
.
   TARGET - Target table/dataset name.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::CP
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> DUI/CP
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DUI_CP
**
** ---------------------------------------------------------------------
** DUI/DF
>COMMAND DF
>PARAMETERS
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
DF is a member function of the duiFactory interface.
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
   duiFactory::DF
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> DUI/DF
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DUI_DF
**
** ---------------------------------------------------------------------
** DUI/DU [ PATH MINSIZE ]
>COMMAND DU
>PARAMETERS
+
PATH    'Unix-like dataset path' C D='.'
MINSIZE 'Minimum size of reported tables' I D=0
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
DU is a member function of the duiFactory interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   PATH - Unix-like dataset path.
   - More guidance needed here.
.
   MINSIZE - Minimum size of reported tables.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::DU
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> DUI/DU
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DUI_DU
**
** ---------------------------------------------------------------------
** DUI/LN SOURCE TARGET
>COMMAND LN
>PARAMETERS
SOURCE  'Source table name' C
TARGET  'Target table/dataset name' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
LN is a member function of the duiFactory interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOURCE - Source table name.
   - More guidance needed here.
.
   TARGET - Target table/dataset name.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::LN
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> DUI/LN
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DUI_LN
**
** ---------------------------------------------------------------------
** DUI/LS [ PATH ]
>COMMAND LS
>PARAMETERS
+
PATH    'Unix-like dataset path' C D='.'
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
LS is a member function of the duiFactory interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   PATH - Unix-like dataset path.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::LS
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> DUI/LS
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DUI_LS
**
** ---------------------------------------------------------------------
** DUI/MKDIR PATH
>COMMAND MKDIR
>PARAMETERS
PATH    'Unix-like dataset path' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
MKDIR is a member function of the duiFactory interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   PATH - Unix-like dataset path.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::MKDIR
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> DUI/MKDIR
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DUI_MKDIR
**
** ---------------------------------------------------------------------
** DUI/MV SOURCE TARGET
>COMMAND MV
>PARAMETERS
SOURCE  'Source table name' C
TARGET  'Target table/dataset name' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
MV is a member function of the duiFactory interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   SOURCE - Source table name.
   - More guidance needed here.
.
   TARGET - Target table/dataset name.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::MV
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> DUI/MV
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DUI_MV
**
** ---------------------------------------------------------------------
** DUI/PWD
>COMMAND PWD
>PARAMETERS
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
PWD is a member function of the duiFactory interface.
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
   duiFactory::PWD
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> DUI/PWD
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DUI_PWD
**
** ---------------------------------------------------------------------
** DUI/RM PATH
>COMMAND RM
>PARAMETERS
PATH    'Unix-like table path' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
RM is a member function of the duiFactory interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   PATH - Unix-like table path.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::RM
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> DUI/RM
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DUI_RM
**
** ---------------------------------------------------------------------
** DUI/RMDIR PATH
>COMMAND RMDIR
>PARAMETERS
PATH    'Unix-like dataset path' C
>GUIDANCE
More guidance needed here.
.
DESCRIPTION: 
.
RMDIR is a member function of the duiFactory interface.
.
More guidance needed here.
.
ARGUMENTS: 
.
   PATH - Unix-like dataset path.
   - More guidance needed here.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::RMDIR
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> DUI/RMDIR
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_DUI_RMDIR
**

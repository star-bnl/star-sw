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
 #(@)$Id: dui_def.cdf,v 1.6 1998/01/24 19:04:29 ward Exp $
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
   DUI HAS NO WORKER OBJECTS.
.
The DUI package has only one interface class defined, the duiFactory.
.
The duiFactory interface is a subclass of the tdmFactory interface.
Thus any object factory implementing the duiFactory interface can and
should assume the role of the tdmFactory in creating and managing in
memory tables and datasets.
.
The Dataset Unix-like Interface of DUI provides a convienient and
familiar user interface for the navigation, creation, and manipulation
of dataset objects and table objects.
.
tdmDataset objects (C++ representation of DSL datasets) are container
objects which can contain other tdmDataset objects and/or tdmTable
objects (C++ representation of DSL tables).
.
In DUI, the dataset hierarchy created by the recursive inclusion of
datasets within one another is presented as a Unix file system-like
hierarchy which can be navigated with the CD command.
.
Thus datasets in DUI are analogous to directories in a Unix file
system, and tables are analogous to files in a Unix file system.
.
In addition to the Unix-like commands of duiFactory, the DUI package
extends TDM by the introduction of two concepts:
.
   1 - Root Dataset
       - All datasets and tables created within the DUI context are
         found within a single Unix file system-like hierarchy. The
	 root dataset is named "/dui" and is created automatically by
	 the initialization and startup of DUI.
   2 - Current Working Dataset
       - At any instant, one dataset within the hierachy is designated
         (by the history of DUI commands) as the current working
	 dataset. This CWD determines the evaluation of relative paths
	 used by DUI commands.
.
All DUI commands should take either an absolute path name to a dataset
or table (ie. /dui/fee/fii/foe/fum) or a relative path name
(ie. ../../foe/fum).
.
With few exceptions, the behavior of DUI commands is a very faithful
mimicking of the analogous Unix commands (without any additional
modifier flags). When in doubt, assume that a DUI command will behave
as you would expect a Unix command to behave.
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

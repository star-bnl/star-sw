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
 #(@)$Id: dui_def.cdf,v 1.15 1998/08/13 02:08:24 perev Exp $  Edited by Bill Love - 24 Feb 1998
.
DUI is an Analysis Service Package (ASP) for the Standard Analysis
Framework (StAF). An ASP is a package of object interfaces which plug
into the software bus archictecture of StAF in a CORBA compliant
interface layer.
.
Each ASP is comprised of an object factory interface (eg. duiFactory)
and zero or more worker object interfaces.
.
DUI has no worker objects.
.
The DUI package has only one interface class defined, the duiFactory.
.
The duiFactory interface is a subclass of the tdmFactory interface.
Thus any object factory implementing the duiFactory interface can and
should assume the role of the tdmFactory in creating and managing in
memory tables and datasets.
.
The Dataset Unix-like Interface of DUI provides a convenient and
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
CD moves through the dataset heirarchy to change the "current dataset"
.
DESCRIPTION: 
.
CD is a member function of the duiFactory interface.  It is 
used to change the current working dataset.  
.
ARGUMENTS: 
.
   PATH - Unix-like dataset path, either absolute, starting from the 
   /dui root or relative, starting from the current working dataset.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::CD
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. To return to the root (/dui) directory
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
>ACTION kam_dui_cd_%C
**
** ---------------------------------------------------------------------
** DUI/APPEND SOURCE TARGET
>COMMAND APPEND
>PARAMETERS
SOURCE  'Source table name' C
TARGET  'Target table/dataset name' C
>GUIDANCE
Appends SOURCE to the end of TARGET.
.
DESCRIPTION: 
.
Appends SOURCE to the end of TARGET.
TARGET must already exist.
.
ARGUMENTS: 
.
   SOURCE - Source table name.  The name of an existing table.
.
   TARGET - Appendee.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::APPEND
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Append harry onto the end of bob.
.
   StAF> DUI/APPEND harry bob
.
EXCEPTIONS: 
.
 SRC_NOT_FOUND - The source table doesn't exist.
 TGT_DOES_NOT_EXIST - The target table doesn't exist.
.
BUGS: 
.
   None, it was written by Herb.
.
>ACTION kam_dui_append_%C
**
** ---------------------------------------------------------------------
** ---------------------------------------------------------------------
** DUI/CP SOURCE TARGET
>COMMAND CP
>PARAMETERS
SOURCE  'Source table name' C
TARGET  'Target table/dataset name' C
>GUIDANCE
Make a copy of a table - put it in a specified dataset.
.
DESCRIPTION: 
.
CP is a member function of the duiFactory interface which creates 
a copy of an existing table with a new name.  Can be in the same
or another dataset.  The content of the new table will be the same
as the source.
.
ARGUMENTS: 
.
   SOURCE - Source table name.  The name of an existing table.
.
   TARGET - Target table/dataset name.  The name of a new table or 
of a dataset where the table will be placed.  If a dataset is given
without a table name the new table will have the same name as the source.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::CP
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Copy table tsspar into dataset bob.
.
   StAF> DUI/CP tsspar bob
.
EXCEPTIONS: 
.
 SRC_NOT_FOUND - The source table doesn't exist.
 TGT_ALREADY_EXISTS - cannot copy a table over an existing table.
.
BUGS: 
.
   None known.
.
>ACTION kam_dui_cp_%C
**
** ---------------------------------------------------------------------
** DUI/DF [ MARKER_STRING ]
>COMMAND DF
>PARAMETERS
+
MARKER_STRING 'Any text string to mark the command in a KUMAC' C D='xxx'
>GUIDANCE
Print the memory usage of the tables (and all other dynamically
allocated memory).  The optional parameter allows tracing when
many DUI/DF commands are placed in a kumac file.
.
DESCRIPTION: 
.
DF is a member function of the duiFactory interface which prints
the total memory in use.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::DF
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Use the command.
.
 staf++ > dui/df
 92,157,884 Bytes of memory allocated
 staf++ > dui/df positionNumber32
 92,157,884 Bytes of memory allocated (positionNumber32)
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
>ACTION kam_dui_df_%C
**
** ---------------------------------------------------------------------
** DUI/DU
>COMMAND DU
>PARAMETERS
>GUIDANCE
This command is useful for finding memory-hog tables.
It lists all the tables and directories.  For the tables, the
amount of allocated memory in bytes is shown (ie, maxrow x row_size).
.
DESCRIPTION: 
.
DU is a member function of the duiFactory interface.
.
ARGUMENTS: 
NONE
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::DU
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Dump the table space in use.  Do you see the hog?
.
 staf > dui/du
 /dui/BEGIN_RUN/TimeStamp ------------------ 28 bytes       1 rows
 /dui/BEGIN_RUN/BeginRunInfo                256 bytes       1 rows
 /dui/BEGIN_RUN/SCReadout                   864 bytes       6 rows
   etc., etc., etc.
 /dui/ProducedData/Pixels/adcxyz --  48,000,000 bytes  1000000 rows
   etc., etc., etc.
                        Total bytes  51,610,073
.
BUGS: 
.
   None known.
.
>ACTION kam_dui_du_%C
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
>ACTION kam_dui_ln_%C
**
** ---------------------------------------------------------------------
** DUI/LS [ PATH ]
>COMMAND LS
>PARAMETERS
+
PATH    'Unix-like dataset path' C D='.'
>GUIDANCE
List the contents of the specified dataset.  
.
DESCRIPTION: 
.
LS is a member function of the duiFactory interface.  Datasets
have the name and number of members listed.  Tables have maxrowcount,
rowcount and rowsize in bytes.
.
ARGUMENTS: 
.
   PATH - Unix-like dataset path, absolute (from /dui) or relative.
If left blank the current working dataset is listed.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::LS method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. List current working dataset.
.
 staf++ > ls
 DUI:    Listing = ...
  Name             * Type             * Used     * Alloc'd  * Size    
 D         Switches *                  *        4 *       -1 *       -1
 D             Maps *                  *        2 *       -1 *       -1
 D   PedestalsGains *                  *        3 *       -1 *       -1
 .... etc., etc., etc.
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_dui_ls_%C
**
** ---------------------------------------------------------------------
** DUI/MKDIR PATH
>COMMAND MKDIR
>PARAMETERS
PATH    'Unix-like dataset path' C
>GUIDANCE
Make a new empty dataset at the specified path.
.
DESCRIPTION: 
.
MKDIR is a member function of the duiFactory interface.  It creates a 
dataset object.  The working dataset is not changed.
.
ARGUMENTS: 
.
   PATH - Unix-like dataset path.  Absolute or relative.  See DUI/CD.

.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::MKDIR
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Make a new dataset  named "bob" in the current working dataset.
.
   StAF> DUI/MKDIR bob
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_dui_mkdir_%C
**
** ---------------------------------------------------------------------
** DUI/MV SOURCE TARGET
>COMMAND MV
>PARAMETERS
SOURCE  'Source table name' C
TARGET  'Target dataset name' C
>GUIDANCE
Move a table to a different dataset.
.
DESCRIPTION: 
.
MV is a member function of the duiFactory interface.  It changes the
directory (dataset) but not the name of a table.
.
ARGUMENTS: 
.
   SOURCE - Source table name.  The name of the table to move.
.
   TARGET - Target dataset name.  The new dataset to contain the table.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::MV
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Move the fmtpar table to dataset bob.
.
 staf++ > mv fmtpar bob  
.
EXCEPTIONS: 
 SECOND_PARAM_MUST_BE_DIR  - attempt to change the name of the table fails.
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_dui_mv_%C
**
** ---------------------------------------------------------------------
** DUI/PWD
>COMMAND PWD
>PARAMETERS
>GUIDANCE
Print the name of the current working Directory (Dataset).
.
DESCRIPTION: 
.
PWD is a member function of the duiFactory interface.  It prints the
name of the current working dataset.
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
EG1. Show the current dataset.
.
 staf++ > dui/pwd
 DUI:    Current Working Directory = (/dui/Switches) 
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
>ACTION kam_dui_pwd_%C
**
** ---------------------------------------------------------------------
** DUI/RM PATH
>COMMAND RM
>PARAMETERS
PATH    'Unix-like table path' C
>GUIDANCE
Delete a table. DUI/RM will also delete a Dataset. 
.
DESCRIPTION: 
.
RM is a member function of the duiFactory interface.  It
removes (deletes) the table or Dataset named.
.
ARGUMENTS: 
.
   PATH - Unix-like table path.  A relative or absolute path to an existing 
Table or Dataset.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::RM
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Delete the tclpar table from dataset bob.
.
   StAF> dui/rm bob/tclpar
.
EXCEPTIONS: 
.
 REMOVAL_FAILED - generally if the object named does not exist.
.
BUGS: 
.
   I'm not sure it's supposed to delete datasets.  Probably RM is a 
legal KUIP abbreviation for RMDIR.
.
>ACTION kam_dui_rm_%C
**
** ---------------------------------------------------------------------
** DUI/RMDIR PATH
>COMMAND RMDIR
>PARAMETERS
PATH    'Unix-like dataset path' C
>GUIDANCE
Remove the named dataset and all its tables.
.
DESCRIPTION: 
.
RMDIR is a member function of the duiFactory interface.  It deletes a 
dataset and any tables contained therein.
.
ARGUMENTS: 
.
   PATH - Unix-like dataset path.  A relative or absolute path to an existing 
Dataset.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   duiFactory::RMDIR
   method is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Delete the "bob" dataset.
.
   StAF> DUI/RMDIR bob
.
EXCEPTIONS:
. 
 DIR_NOT_FOUND - the named dataset was not found.

 USE_RM_FOR_TABLES_NOT_RMDIR - attempt to remove a table with dui/rmdir
BUGS: 
.
   None known.
.
>ACTION kam_dui_rmdir_%C
**********************************************************************
** DUI/PRECIOUS
>COMMAND PRECIOUS
>PARAMETERS
>GUIDANCE
Marks all existing tables as precious.
See the related command RM_NONPRECIOUS.
>ACTION kam_dui_precious_%C
**********************************************************************
** DUI/RM_NONPRECIOUS
>COMMAND RM_NONPRECIOUS
>PARAMETERS
>GUIDANCE
Deletes all non-precious tables.
See the related command PRECIOUS.
Typically, you would run the PRECIOUS command before an
event loop, and then run RM_NONPRECIOUS at the bottom
of the loop to remove trash, ie:  
   DUI/PRECIOUS
   top_of_loop
      contents of loop
      DUI/RM_NONPRECIOUS 
   bottom_of_loop
>ACTION kam_dui_rm_nonprecious_%C
**********************************************************************

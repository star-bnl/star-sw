>NAME TDM_DEF
>MENU TDM
>GUIDANCE
**************************************************************** /TDM/COUNT
>COMMAND COUNT
>PARAMETERS
>GUIDANCE
>ACTION KAM_TDM_COUNT
***************************************************************** /TDM/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
>ACTION KAM_TDM_LIST
************************************************************* /TDM/NEWTABLE
>COMMAND NEWTABLE
>PARAMETERS
ALIAS '' C
SPEC '' C
ROWS '' I
>GUIDANCE
>ACTION KAM_TDM_NEWTABLE
*********************************************************** /TDM/NEWDATASET
>COMMAND NEWDATASET
>PARAMETERS
ALIAS '' C
SETDIM '' I
>GUIDANCE
>ACTION KAM_TDM_NEWDATASET
***************************************************************************
>MENU OBJECT
>GUIDANCE
******************************************************* /TDM/OBJECT/DSLNAME
>COMMAND DSLNAME
>PARAMETERS
>GUIDANCE
>ACTION
***************************************************** /TDM/OBJECT/ISDATASET
>COMMAND ISDATASET
>PARAMETERS
>GUIDANCE
>ACTION
******************************************************* /TDM/OBJECT/ISTABLE
>COMMAND ISTABLE
>PARAMETERS
>GUIDANCE
>ACTION
***************************************************************************
>MENU \TABLE
>GUIDANCE
********************************************************* /TDM/TABLE/ISTYPE
>COMMAND ISTYPE
>PARAMETERS
ATYPE '' C
>GUIDANCE
>ACTION
****************************************************** /TDM/TABLE/PRINTROWS
>COMMAND PRINTROWS
>PARAMETERS
IFIRST '' I
NROWS '' I
>GUIDANCE
>ACTION
*********************************************************** /TDM/TABLE/SHOW
>COMMAND SHOW
>PARAMETERS
>GUIDANCE
>ACTION
***************************************************** /TDM/TABLE/FINDCOLUMN
>COMMAND FINDCOLUMN
>PARAMETERS
NAME '' C
COLUMN '' X
>GUIDANCE
>ACTION
****************************************************** /TDM/TABLE/GETCOLUMN
>COMMAND GETCOLUMN
>PARAMETERS
NCOL '' I
COLUMN '' X
>GUIDANCE
>ACTION
*************************************************** /TDM/TABLE/COLUMNNUMBER
>COMMAND COLUMNNUMBER
>PARAMETERS
NAME '' C
>GUIDANCE
>ACTION
***************************************************** /TDM/TABLE/COLUMNNAME
>COMMAND COLUMNNAME
>PARAMETERS
NCOL '' I
>GUIDANCE
>ACTION
************************************************* /TDM/TABLE/COLUMNTYPECODE
>COMMAND COLUMNTYPECODE
>PARAMETERS
NCOL '' I
>GUIDANCE
>ACTION
***************************************************** /TDM/TABLE/COLUMNSIZE
>COMMAND COLUMNSIZE
>PARAMETERS
NCOL '' I
>GUIDANCE
>ACTION
***************************************************** /TDM/TABLE/COLUMNRANK
>COMMAND COLUMNRANK
>PARAMETERS
NCOL '' I
>GUIDANCE
>ACTION
**************************************************** /TDM/TABLE/COLUMNSHAPE
>COMMAND COLUMNSHAPE
>PARAMETERS
NCOL '' I
NDIM '' I
>GUIDANCE
>ACTION
************************************************** /TDM/TABLE/COLUMNELCOUNT
>COMMAND COLUMNELCOUNT
>PARAMETERS
NCOL '' I
>GUIDANCE
>ACTION
************************************************* /TDM/TABLE/COLUMNTYPENAME
>COMMAND COLUMNTYPENAME
>PARAMETERS
NCOL '' I
>GUIDANCE
>ACTION
******************************************************** /TDM/TABLE/GETDATA
>COMMAND GETDATA
>PARAMETERS
DATA '' X
>GUIDANCE
>ACTION
******************************************************** /TDM/TABLE/PUTDATA
>COMMAND PUTDATA
>PARAMETERS
DATA '' X
>GUIDANCE
>ACTION
******************************************************** /TDM/TABLE/GETCELL
>COMMAND GETCELL
>PARAMETERS
DATA '' X
NROW '' I
NCOL '' I
>GUIDANCE
>ACTION
******************************************************** /TDM/TABLE/PUTCELL
>COMMAND PUTCELL
>PARAMETERS
DATA '' X
NROW '' I
NCOL '' I
>GUIDANCE
>ACTION
**************************************************** /TDM/TABLE/COLUMNCOUNT
>COMMAND COLUMNCOUNT
>PARAMETERS
>GUIDANCE
>ACTION
**************************************************** /TDM/TABLE/MAXROWCOUNT
>COMMAND MAXROWCOUNT
>PARAMETERS
+
MAXROWCOUNT '???' I
>GUIDANCE
>ACTION
******************************************************* /TDM/TABLE/ROWCOUNT
>COMMAND ROWCOUNT
>PARAMETERS
+
ROWCOUNT '???' I
>GUIDANCE
>ACTION
******************************************************** /TDM/TABLE/ROWSIZE
>COMMAND ROWSIZE
>PARAMETERS
>GUIDANCE
>ACTION
******************************************************* /TDM/TABLE/TYPENAME
>COMMAND TYPENAME
>PARAMETERS
>GUIDANCE
>ACTION
************************************************** /TDM/TABLE/TYPESPECIFIER
>COMMAND TYPESPECIFIER
>PARAMETERS
>GUIDANCE
>ACTION
***************************************************************************
>MENU \DATASET
>GUIDANCE
*************************************************** /TDM/DATASET/ADDDATASET
>COMMAND ADDDATASET
>PARAMETERS
NAME '' C
SETDIM '' I
>GUIDANCE
>ACTION
***************************************************** /TDM/DATASET/ADDTABLE
>COMMAND ADDTABLE
>PARAMETERS
NAME '' C
SPEC '' C
ROWS '' I
>GUIDANCE
>ACTION
************************************************* /TDM/DATASET/GETENTRYTYPE
>COMMAND GETENTRYTYPE
>PARAMETERS
TYPE '' C
NUM '' I
>GUIDANCE
>ACTION
********************************************** /TDM/DATASET/GETDATASETENTRY
>COMMAND GETDATASETENTRY
>PARAMETERS
DATASET '' X
NUM '' I
>GUIDANCE
>ACTION
************************************************ /TDM/DATASET/GETTABLEENTRY
>COMMAND GETTABLEENTRY
>PARAMETERS
TABLE '' X
NUM '' I
>GUIDANCE
>ACTION
********************************************* /TDM/DATASET/FINDDATASETENTRY
>COMMAND FINDDATASETENTRY
>PARAMETERS
DATASET '' X
NAME '' C
>GUIDANCE
>ACTION
*********************************************** /TDM/DATASET/FINDTABLEENTRY
>COMMAND FINDTABLEENTRY
>PARAMETERS
TABLE '' X
NAME '' C
>GUIDANCE
>ACTION
************************************************ /TDM/DATASET/GETDESCRIPTOR
>COMMAND GETDESCRIPTOR
>PARAMETERS
DESCRIPTOR '' C
>GUIDANCE
>ACTION
*************************************************** /TDM/DATASET/ENTRYCOUNT
>COMMAND ENTRYCOUNT
>PARAMETERS
>GUIDANCE
>ACTION
***************************************************************************

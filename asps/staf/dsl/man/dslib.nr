.TH "DSLIB" 3 "6 Mar 1995" "DRAFT" "DATA STRUCTURE LIB"
.SH SYNOPSIS
.nf
.ft B
#include "dstype.h"
.ft
.fi
.sp
.LP
.ft B
.nf
int dsAddTable(pDataset, name, typeSpecifier, rowCount, ppData)
DS_DATASET_T *pDataset;
char *name;
char *typeSpecifier;
unsigned rowCount;
void *ppData;
.fi
.ft
.sp
.LP
.ft B
.nf
int dsAllocTables(pDataset)
DS_DATASET_T *pDataset;
.fi
.ft
.sp
.LP
.ft B
.nf
int dsEquijoin(pJoinTable, pTableOne, pTableTwo, aliases, joinList, projectList)
DS_DATASET_T *pJoinTable;
DS_DATASET_T *pTableOne;
DS_DATASET_T *pTableTwo;
char *aliases;
char *joinList;
char *projectList;
.fi
.ft
.sp
.LP
.ft B
.nf
int dsFreeDataset(pDataset)
DS_DATASET_T *pDataset;
.fi
.ft
.sp
.LP
.ft B
.nf
int dsMapTable(pDataset, name, typeSpecifier, pRowCount, ppData)
DS_DATASET_T *pDataset;
char *name;
char *typeSpecifier;
unsigned *pRowCount;
void *ppData;
.fi
.ft
.sp
.LP
.ft B
.nf
int dsNewDataset(ppDataset, name, setDim)
DS_DATASET_T **ppDataset;
char *name;
unsigned setDim;
.fi
.ft
.sp
.LP
.ft B
.nf
int dsNewTable(ppTable, name, typeSpecifier, rowCount, pData)
DS_DATASET_T **ppTable;
char *name;
char *typeSpecifier;
unsigned rowCount;
void *pData;
.fi
.ft
.sp
.LP
.ft B
.nf
int dsPerror(msg)
char *msg;
.fi
.ft
.sp
.LP
.ft B
.nf
int dsProjectTable(pProjection, pTable, projectList)
DS_DATASET_T *pProjection;
DS_DATASET_T *pTable;
char *projectList;
.fi
.ft
.sp
.LP
.ft B
.nf
int dsTargetTable(ppTable, tableName, typeName,
     parentOne, parentTwo, aliases, projectList)
DS_DATASET_T **ppTable;
char *tableName;
char *typeName;
DS_DATASET_T *parentOne;
DS_DATASET_T *parentTwo;
char *aliases;
char *projectList;
.fi
.ft
.SH DESCRIPTION
.LP
.B dsAddTable(\|)
adds a table called
.I name
to the dataset specified by
.IR pDataset .
This table has
.I rowCount
rows of the type specified by
.IR typeSpecifier .
.I ppData
points to a data pointer for the table.  If
.I ppData
points to a NULL pointer, memory is allocated for the table and its address
is returned in the pointer located at
.IR ppData .
.LP
.B dsAllocTable(\|)
allocates memory for any tables in the dataset specified by
.I pDataset
that have NULL data pointers.  This memory must freed by
.BR dsFreeDataset .
.LP
.B dsEquijoin(\|)
constructs the equijoin of
.I pTableOne
and
.I pTableTwo
and returns it in 
.IR pJoinTable .
The columns to be returned in
.I pJoinTable
are specified by
.I projectList
which has the format '{' select_column_specifier ',' .... '}'.
A select_column_specifier is either a single column_name or
table_name '.' column_name column_name.
A select_column_specifier specifies a column in
.I pJoinTable
and a column in either
.I pTableOne
or
.IR pTableTwo .
Columns of 
.I joinTable
that are not specified by a select_column_specifier are matched to a column of
.I pTableOne
or
.I pTableTwo 
if the column occurs in only one table or occurs in both and the pair are joined.
.I projectList
may be NULL if all columns are to be selected by the default rule, such as in a
natural join.
Table names can be given aliases to distinguish the role they play in a
self-join or when two tables have the same name.  Alises are specified by
.I aliases
which has the format '{' table_name alias_name [ ',' table_name alias_name ] '}'.
.I aliases
may be NULL if no aliases are required.   
Pairs of columns to be joined are specified by
.I joinList
which has the format '{' join_pair_specifier ',' ... '}'.
A join_pair_specifier is either a single column_name or 
table_name '.' column_name table_name '.' column_name. 
column_specifier has the format table_name '.' column_name.
A join_pair_specifier specifies a column from
.I pTableOne
and a column from
.I pTableTwo
that are to be joined.  If
.I joinList
is NULL, a natural join is performed.
.LP 
.B dsFreeDataset(\|)
frees all memory allocated for the dataset
.I pDataset
by routines such as
.BR dsAllocTable(\|) ,
.BR dsNewDataset(\|) ,
.B xdr_dataset(\|)
and
.BR xdr_dataset_type(\|) .
.LP
.B dsMapTable(\|)
associates a table in a dataset with a program variable.
.LP
.B dsNewDataset(\|)
creates a new dataset descriptor for a dataset called
.I name
with a maximum of
.I setDim
entries.  One entry is used for the root of the dataset.
If
.I ppDataset
points to a NULL pointer, memory is allocated for the dataset descriptor.
.LP
.B dsNewTable(\|)
creates a new table descriptor for a table called
.I name
with
.I rowCount
rows.
.I pData
points to data for the table and
.I ppTable
points to a pointer for the descriptor for the table.
If
.I ppTable
points to a NULL pointer, memory is allocated for the table descriptor.   
.LP
.B dsPerror(\|)
prints a message to standard error describing the last error that occurred in a
dataset library function.
.LP
.B dsProjectTable(\|)
projects columns from
.I pTable
into columns of 
.I pProjection
as specified by
.IR projectList .
.LP
.B dsTargetTable(\|)
constructs a result table for use in calls to
.B dsEquijoin(\|)
and
.BR dsProjectTable(\|) .
.SH SEE ALSO
<to be supplied>

/* Copyright 1995-1998, Lawrence Berkeley Laboratory */

/* dsjoin.c - routines that do joins and projects */

/*
modification history
--------------------
14oct97,cet  Regular join is faster for all tables. Too much "behind
	the API fiddling" to recover. Now NOOPT function.
08aug97,hjw  join runs much faster for sorted tables (n instead of n squared)
14mar95,whg  collected from other files 
*/

/*
DESCRIPTION
relation database join and project operations for tables
*/
#define DS_PRIVATE
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "dstype.h"
#include "asuAlloc.h"
#include "emlLib.h"

/***********************************************************************
*/
int topFastjoin(DS_DATASET_T *pJoinTable, DS_DATASET_T *pTableOne,
	DS_DATASET_T *pTableTwo, char *aliases, 
        char *joinList, char *projectList)
{
	return dsEquijoin(pJoinTable, pTableOne, pTableTwo, aliases,
			joinList, projectList);
}

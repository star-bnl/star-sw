/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>--------------------------------------------------------------------
*:FILE:         ds2_utils.c
*:DESCRIPTION:  Utility functions for 2-struct tables
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      18jan96-v000a-cet- creation
*:<--------------------------------------------------------------------
*/

/*-------------------------------------------- MACROS               -*/
#define F77_TRUE 0
#define F77_FALSE -1

/*-------------------------------------------- INCLUDES             -*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "emlLib.h"
#include "dstype.h"
#include "table_header.h"

/*-------------------------------------------- TYPEDEFS             -*/
/*-------------------------------------------- GLOBALS              -*/
/*-------------------------------------------- PROTOTYPES           -*/
int ds2ReallocTable(TABLE_HEAD_ST** ppHead,char** ppData
		, size_t newCount);
int ds2realloctable_(TABLE_HEAD_ST** ppHead,char** ppData
		, size_t newCount);

/*
*:>--------------------------------------------------------------------
*:ROUTINE:      ds2ReallocTable
*:DESCRIPTION:  C-compatable 2-struct analog of dsSetTableMaxCount
*:ARGUMENTS:    "same" as dsSetTableMaxCount
*:RETURN VALUE: TRUE or FALSE
*:<--------------------------------------------------------------------
*/
int ds2ReallocTable(TABLE_HEAD_ST** ppHead,char** ppData
		, size_t newCount)
{
	TABLE_HEAD_ST *pHead=*ppHead;
	char *pData=*ppData;
	DS_DATASET_T* pTable;
	char *pDat=NULL;

	bool_t result;
	size_t rowsize, rowcount, maxcount;

	pTable = (DS_DATASET_T*)(pHead->dsl_pointer);
	pDat = (char*)(pHead->data_pointer);

	if( !dsIsTable(&result, pTable) || !result
	||  !dsTableRowSize(&rowsize, pTable)
			|| !(rowsize == pHead->rbytes)
	||  !dsTableMaxRowCount(&maxcount, pTable)
			|| !(maxcount == pHead->maxlen)
	||  !dsTableRowCount(&rowcount, pTable)
			|| !(rowcount == pHead->nok)
	||  !(pDat == pData)
	||  !dsTableDataAddress(&pDat, pTable)
			|| !(pDat == pData)
	){
	   EML_ERROR(CORRUPT_TABLE_STRUCTS);
	   /*	   return FALSE;  EML_ERROR returns on its own  (done) */
	}

	if( !dsReallocTable(pTable,newCount)
	||  !dsTableDataAddress(&pDat, pTable)
	){
	   EML_ERROR(CANT_REALLOCATE);
	   /*	   return FALSE;  EML_ERROR returns on its own */
	}
	*ppData = pDat;
	pHead->maxlen = newCount;
	pHead->data_pointer = (long) pDat;
	return TRUE;
}

/*
*:>--------------------------------------------------------------------
*:ROUTINE:      ds2realloctable_
*:DESCRIPTION:  F77-compatable wrapper for ds2ReallocTable_
*:ARGUMENTS:    "same" as ds2ReallocTable
*:RETURN VALUE: FORTRAN TRUE or FALSE
*:<--------------------------------------------------------------------
*/
int ds2realloctable_(TABLE_HEAD_ST** ppHead,char** ppData
		, size_t newCount)
{
	if(ds2ReallocTable(ppHead,ppData,newCount)){
		return F77_TRUE;
	}
	else{
		return F77_FALSE;
	}
}

/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:	dui_dslUnix.C
*:DESCRIPTION:	Functions to perform Unix commands on DSL entities.
*:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:	-- STILL IN DEVELOPMENT --
*:HISTORY:	11dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*------------------------------------------------ INCLUDES           */
#include <string.h>
#include "asuAlloc.h"
#include "dstype.h"
#include "dui_types.h"
#include "emlLib.h"

/*------------------------------------------------ GLOBALS            */
#define DUI_LST_FORMAT "%1s %16s * %16s * %8d * %8d * %8d\n"
#define DUI_LSD_FORMAT "%1s %16s * %16s * %8d * %8d * %8d\n"
#define DUI_LSH_FORMAT "%1s %16s * %16s * %8s * %8s * %8s\n"

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	int dui_ls_l_Table(DS_DATASET_T *pDS, char*& listing)
*:DESCRIPTION:	ls -l a DSL table
*:ARGUMENTS:	----------
*:RETURN VALUE:	-- NONE --
*:<---------------------------------------------------------------------
*/
int dui_ls_l_Table(DS_DATASET_T *pDS, char*& listing)
{
   bool_t isTable;
   const char *name, *tname;
   size_t rowcount, maxrowcount, rowsize;

   if( !dsIsTable(&isTable, pDS)
   ||  !isTable
   ||  !dsTableName(&name,pDS)
   ||  !dsTableTypeName(&tname,pDS)
   ||  !dsTableRowCount(&rowcount,pDS)
   ||  !dsTableMaxRowCount(&maxrowcount,pDS)
   ||  !dsTableRowSize(&rowsize,pDS)
   ){
      EML_PUSHERROR(INVALID_DSL_TABLE);
      return FALSE;
   }
   char* result = (char*)MALLOC(256);
   sprintf(result,DUI_LST_FORMAT
   		,"T",name,tname,rowcount,maxrowcount,rowsize);
   strcat(listing,result);
   FREE(result); /*fix memory leak -akio*/
   return TRUE;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	int dui_ls_ld_Dataset(DS_DATASET_T *pDS, char*& listing)
*:DESCRIPTION:	ls -ld a DSL dataset
*:ARGUMENTS:	----------
*:RETURN VALUE:	-- NONE --
*:<---------------------------------------------------------------------
*/
int dui_ls_ld_Dataset(DS_DATASET_T *pDS,char*& listing)
{
   bool_t isDataset;
   const char *name;
   size_t elcount;

   if( !dsIsDataset(&isDataset, pDS)
   ||  !isDataset
   ||  !dsDatasetName(&name,pDS)
   ||  !dsDatasetEntryCount(&elcount,pDS)
   ){
      EML_PUSHERROR(INVALID_DSL_DATASET);
      return FALSE;
   }
   char* result = (char*)MALLOC(256);
   sprintf(result,DUI_LSD_FORMAT
   		,"D",name," ",elcount,-1,-1);
   strcat(listing,result);
   FREE(result); /*fix memory leak -akio*/
   return TRUE;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	int dui_ls_l_Dataset(DS_DATASET_T *pDS, char*& listing)
*:DESCRIPTION:	ls -l a DSL dataset
*:ARGUMENTS:	----------
*:RETURN VALUE:	-- NONE --
*:<---------------------------------------------------------------------
*/
int dui_ls_l_Dataset(DS_DATASET_T *pDS, char*& listing)
{
   bool_t isDataset,isTable;
   DS_DATASET_T *pEntry;
   size_t elcount;
   int i;

   if( !dsIsDataset(&isDataset, pDS)
   ||  !isDataset
   ||  !dsDatasetEntryCount(&elcount,pDS)
   ){
      EML_PUSHERROR(INVALID_DSL_DATASET);
      return FALSE;
   }
   dui_ls_l_Header(listing);
   for( i=0;i<(int)elcount;i++ ){
      if( !dsDatasetEntry(&pEntry,pDS,i)
      ||  !dsIsTable(&isTable, pEntry)
      ||  !dsIsDataset(&isDataset, pEntry)
      ||  !(isTable || isDataset)
      ){
	 EML_PUSHERROR(BAD_DSL_ENTRY);
	 return FALSE;
      } else {
	 if(isTable)dui_ls_l_Table(pEntry,listing);
	 if(isDataset)dui_ls_ld_Dataset(pEntry,listing);
      }
   }
   return TRUE;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	int dui_ls_l_Header(char*& listing)
*:DESCRIPTION:	ls -l Header string
*:ARGUMENTS:	----------
*:RETURN VALUE:	-- NONE --
*:<---------------------------------------------------------------------
*/
int dui_ls_l_Header(char*& listing)
{
   char* result = (char*)MALLOC(256);
   sprintf(result,DUI_LSH_FORMAT
   		," "
		,"Name            "
		,"Type            "
		,"Used    "
		,"Alloc'd "
		,"Size    ");
//		,"123456789 123456789 "
   strcat(listing,result);
   FREE(result); /*fix memory leak -akio*/
   return TRUE;
}


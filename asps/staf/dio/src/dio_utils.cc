/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         dio_utils.C
*:DESCRIPTION:  Utility functions for DIO.
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      13dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*-------------------------------------------- INCLUDES             --*/
#include "dstype.h"
#include "asuLib.h"
#include "dio_types.h"

/*-------------------------------------------- MACROS               --*/
/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      int dio_addHierarchy(pDS,pAdd)
*:DESCRIPTION:  Add an entire DSL hierarchy to a dataset.
*:ARGUMENTS:    ----------
*:RETURN VALUE: TRUE or FALSE
*:<---------------------------------------------------------------------
*/
int dio_addHierarchy(DS_DATASET_T *pDS,DS_DATASET_T *pAdd)
{
   DS_DATASET_T *pEntry=NULL, *pAdded=NULL;
   bool_t isDataset, isTable;
   char *name,*spec;
   size_t count,maxcount;
   char *pData;

/*printf("Add (%s) to (%s) \n", pAdd->name, pDS->name);*/
   if( !dsIsDataset(&isDataset,pAdd)
   ||  !dsIsTable(&isTable,pAdd)
   ||  !(isDataset || isTable)
   ){
      dsPerror("bad DS_DATASET_T pointer");
      return FALSE;
   }
   if( isDataset ){
      if( !dsDatasetName(&name,pAdd)
      ||  !dsDatasetEntryCount(&count,pAdd)
      ||  !dsDatasetMaxEntryCount(&maxcount,pAdd)
      ||  !dsAddDataset(pDS,name,maxcount,pAdded)
      ||  !dsFindEntry(&pAdded,pDS,name)
      ){
	 dsPerror("can't add dataset");
	 return FALSE;
      }
      for( int i=0;i<count;i++ ){
	 pEntry = NULL;
	 if( !dsDatasetEntry(&pEntry,pAdd,(size_t)i)
	 ||  !dio_addHierarchy(pAdded,pEntry)
	 ){
	    dsPerror("can't add entry");
	 }
      }
   } else if( isTable ){
	 if( !dsTableName(&name,pAdd)
	 ||  !dsTableTypeSpecifier(&spec,pAdd)
	 ||  !dsTableRowCount(&count,pAdd)
	 ||  !dsTableMaxRowCount(&maxcount,pAdd)
	 ||  !dsTableDataAddress(&pData,pAdd)
	 ||  !(0<count && count<=maxcount)
	 ||  !dsAddTable(pDS,name,spec,maxcount,&pData)
	 ||  !dsFindEntry(&pAdded,pDS,name)
	 ||  !dsSetTableRowCount(pAdded,count)
	 ){
	    dsPerror("can't add table");
	    return FALSE;
	 }
   }
   return TRUE;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      char* dio_mode2text(DIO_MODE_T mode)
*:DESCRIPTION:  Return a text string version of mode.
*:ARGUMENTS:    ----------
*:RETURN VALUE: text
*:<---------------------------------------------------------------------
*/
char* dio_mode2text(DIO_MODE_T mode)
{
	switch(mode) {
	case DIO_READ_MODE:
	   return "READONLY";
	case DIO_WRITE_MODE:
	   return "WRITEONLY";
	case DIO_UPDATE_MODE:
	   return "READWRITE";
	case DIO_UNKNOWN_MODE:
	default:
	   return "***UNKNOWN***";
	}
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      DIO_MODE_T dio_text2mode(char* text)
*:DESCRIPTION:  ----------
*:ARGUMENTS:    ----------
*:RETURN VALUE: ----------
*:<---------------------------------------------------------------------
*/
DIO_MODE_T dio_text2mode(char* text)
{
	switch(text[0]) {
	case 'r': case 'R':
	   return DIO_READ_MODE;
	case 'w': case 'W':
	   return DIO_WRITE_MODE;
	case 'u': case 'U':
	   return DIO_UPDATE_MODE;
	default:
	   return DIO_UNKNOWN_MODE;
	}
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      char* dio_state2text(DIO_STATE_T state)
*:DESCRIPTION:  ----------
*:ARGUMENTS:    ----------
*:RETURN VALUE: text
*:<---------------------------------------------------------------------
*/
char* dio_state2text(DIO_STATE_T state)
{
	switch(state) {
	case DIO_OPEN_STATE:
	   return "OPENED";
	case DIO_CLOSE_STATE:
	   return "CLOSED";
	case DIO_READ_STATE:
	   return "READING";
	case DIO_WRITE_STATE:
	   return "WRITING";
	case DIO_UNKNOWN_STATE:
	default:
	   return "***UNKNOWN***";
	}
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      DIO_STATE_T dio_text2state(char* text)
*:DESCRIPTION:  ----------
*:ARGUMENTS:    ----------
*:RETURN VALUE: state
*:<---------------------------------------------------------------------
*/
DIO_STATE_T dio_text2state(char* text)
{
	switch(text[0]) {
	case 'o': case 'O':
	   return DIO_OPEN_STATE;
	case 'c': case 'C':
	   return DIO_CLOSE_STATE;
	case 'r': case 'R':
	   return DIO_READ_STATE;
	case 'w': case 'W':
	   return DIO_WRITE_STATE;
	default:
	   return DIO_UNKNOWN_STATE;
	}
}


/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         dio_utils.C
*:DESCRIPTION:  Utility functions for DIO.
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      14nov97-v030a-cet- Remove NOISY warnings
*:HISTORY:      30dec96-v020a-cet- NEW_DSL -> OLD_DSL option
*:HISTORY:      13dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*-------------------------------------------- INCLUDES             --*/
#include "dstype.h"
#define DSL
#include "asuLib.h"
#include "emlLib.h"
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
   const char *name,*spec;
   size_t count,maxcount;
   char *pData;

/*printf("Add (%s) to (%s) \n", pAdd->name, pDS->name);*/
   if( !dsIsDataset(&isDataset,pAdd)
   ||  !dsIsTable(&isTable,pAdd)
   ||  !(isDataset || isTable)
   ){
      EML_WARNING("bad DS_DATASET_T pointer");
      return FALSE;
   }

/*-" Remove old version of pAdd. \n"-*/
   pAdded = NULL;
/*^^^ DEBUG - remove only tables?
   if( isDataset ){
      if( dsDatasetName(&name,pAdd)
      &&  dsFindEntry(&pAdded,pDS,name)
      &&  (NULL != pAdded)
      ){
	 if( !dsFreeDataset(pAdded) ){
	    EML_WARNING("can't free dataset");
	    return FALSE;
         }
	 pAdded = NULL;
      }
   } else
^^^*/
   if( isTable ){
      if( dsTableName(&name,pAdd)
      &&  dsFindEntry(&pAdded,pDS,name)
      &&  (NULL != pAdded)
      ){
	 if( !dsFreeDataset(pAdded) ){
	    EML_WARNING("can't free table");
	    return FALSE;
         }
	 pAdded = NULL;
      }
   }

/*-" Add a dataset. \n "-*/
   if( isDataset ){
      if( !dsDatasetName(&name,pAdd)
      ||  !dsDatasetEntryCount(&count,pAdd)
      ||  !dsDatasetMaxEntryCount(&maxcount,pAdd)
#ifndef OLD_DSL
      ||  !dsNewDataset(&pAdded, name)
      ||  !dsLink(pDS, pAdded)
#else   /*OLD_DSL*/
      ||  !dsAddDataset(pDS,name,maxcount+100,pAdded) //HACK 100
      ||  !dsFindEntry(&pAdded,pDS,name)
#endif  /*OLD_DSL*/
      ){
	 EML_MESSAGE("dataset = (%s)\n",pAdd->name);
         EML_WARNING("can't add dataset");
/*^^^ DEBUG - remove only tables?
         return FALSE;
^^^*/
      }
/*-" Add each entry. \n "-*/
      for( int i=0;i<count;i++ ){
         pEntry = NULL;
         if( !dsDatasetEntry(&pEntry,pAdd,(size_t)i)
         ||  !dio_addHierarchy(pAdded,pEntry)
         ){
	    EML_MESSAGE("entry = (%s)\n",pEntry->name);
            EML_WARNING("can't add entry");
         }
      }
/*-" Add a table. \n "-*/
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
	 EML_MESSAGE("table = (%s)\n",pAdd->name);
	 EML_WARNING("can't add table");
	 return FALSE;
      }
   }
   return TRUE;
}

/*
*:>--------------------------------------------------------------------
*:ROUTINE:      int dio_mapHierarchy(pDS,pAdd)
*:DESCRIPTION:  Map an entire DSL hierarchy into a (existing?) dataset.
*:ARGUMENTS:    ----------
*:RETURN VALUE: TRUE or FALSE
*:<--------------------------------------------------------------------
This function should be called in the following manner:
	pDSin = NULL;
	xdr_dataset_type(&ixdr,&pDSin,0);
	dio_mapHierarchy(pDSmem,pDSin);
	dsAllocTables(pDSin);
	xdr_dataset_data(&ixdr,pDSin);
	dsFreeDataset(pDSin);
*/
int dio_mapHierarchy(DS_DATASET_T *pDS,DS_DATASET_T *pAdd)
{
   DS_DATASET_T *pEntry=NULL, *pAdded=NULL;
   bool_t isDataset, isTable, result;
   const char *name,*spec;
   size_t count,maxcount;
   size_t allspace;
   char *pData;
   int i;

/* Sanity Check */
   if( !dsIsDataset(&isDataset,pAdd)
   ||  !dsIsTable(&isTable,pAdd)
   ||  !(isDataset || isTable)
   ){
      EML_WARNING("bad DS_DATASET_T pointer");
      return FALSE;
   }
/* Add Dataset */
   if( isDataset ){
      if( !dsDatasetName(&name,pAdd)
      ||  !dsDatasetEntryCount(&count,pAdd)
      ||  !dsDatasetMaxEntryCount(&maxcount,pAdd)
      ){
	 EML_WARNING("bad dataset");
	 return FALSE;
      }
      if( !dsFindEntry(&pAdded,pDS,name) ){
#ifndef OLD_DSL
	 if( !dsNewDataset(&pAdded, name)
	 ||  !dsLink(pDS, pAdded)
#else   /*OLD_DSL*/
	 if( !dsAddDataset(pDS,name,maxcount+100,pAdded) /* HACK 100 */
	 ||  !dsFindEntry(&pAdded,pDS,name)
#endif  /*OLD_DSL*/
	 ){
/*TOO NOISY	    EML_WARNING("can't map dataset"); */
	    return FALSE;
	 }
      }
      else {		/* dataset already exists */
/* Clear all table data. */
         dio_clearDataset(pAdded);
      }
      for( i=0;i<count;i++ ){
	 pEntry = NULL;
	 if( !dsDatasetEntry(&pEntry,pAdd,(size_t)i)
	 ||  !dio_mapHierarchy(pAdded,pEntry)
	 ){
/*TOO NOISY	    EML_WARNING("can't map entry"); */
	 }
      }
/* Add Table */
   } else if( isTable ){
	 if( !dsTableName(&name,pAdd)
	 ||  !dsTableTypeSpecifier(&spec,pAdd)
	 ||  !dsTableRowCount(&count,pAdd)
	 ||  !dsTableMaxRowCount(&maxcount,pAdd)
	 ||  !(0<=count && count<=maxcount)
	 ){
	    EML_WARNING("bad table");
	    return FALSE;
	 }
	 if( !dsFindEntry(&pAdded,pDS,name) ){
	    pData = NULL;
	    if( !dsAddTable(pDS,name,spec,maxcount,&pData)
	    ||  !dsFindEntry(&pAdded,pDS,name)
	    ||  !dsSetTableRowCount(pAdded,maxcount)
	    ){
	       if( 0 == maxcount )return TRUE; /* SPECIAL CASE */
/*TOO NOISY	       EML_WARNING("can't map table"); */
	       return FALSE;
	    }
	    pAdd->p.data = pData; /* Evil DSL HACK */
	 }
/* HACK - Data should really be read into same location */
	 else {		/* table already exists */
	    if( !dsTableIsType(&result, pAdded, spec) 
	    ||  !result
	    ||  !dsTableMaxRowCount(&allspace,pAdded)
	    ){
/*TOO NOISY	       EML_WARNING("can't map table"); */
	       return FALSE;
	    }
/* Grow the destination table. */
	    if( allspace < maxcount ){
	       if( !dsReallocTable(pAdded,maxcount) ){
		  EML_WARNING("can't reallocate table");
		  return FALSE;
	       }
	    }
/* Map memory. */
	    if( !dsTableDataAddress(&pData,pAdded)
	    ||  !dsSetTableRowCount(pAdded,maxcount)
	    ){
/*TOO NOISY	       EML_WARNING("can't map table"); */
	       return FALSE;
	    }
	    pAdd->p.data = pData; /* Evil DSL HACK */
	 }	
   }
   return TRUE;
}

/*
*:>--------------------------------------------------------------------
*:ROUTINE:      int dio_clearDataset(pDS)
*:DESCRIPTION:  Add an entire DSL hierarchy to a dataset.
*:ARGUMENTS:    ----------
*:RETURN VALUE: TRUE or FALSE
*:<--------------------------------------------------------------------
*/
int dio_clearDataset(DS_DATASET_T *pDS)
{
   int i;
   bool_t isDataset, isTable;
   size_t count;
   DS_DATASET_T *pEntry;

/* Sanity check. */
   if( !dsIsDataset(&isDataset,pDS)
   ||  !dsIsTable(&isTable,pDS)
   ||  !(isDataset || isTable)
   ){
      EML_WARNING("corrupt DS_DATASET_T pointer");
      return (int)FALSE;	/*INS++:RETURN_INCONSISTENT-BUGFIX?*/
   }

/* Handle datasets recursively. */
   if(isDataset){
      if( !dsDatasetEntryCount(&count,pDS) ){
/* TOO NOISY	 EML_WARNING("error clearing dataset"); */
	 return (int)FALSE;
      }
      for( i=0;i<count;i++ ){
	 pEntry = NULL;
	 if( !dsDatasetEntry(&pEntry,pDS,(size_t)i)
	 ||  !dio_clearDataset(pEntry)
	 ){
/* TOO NOISY	    EML_WARNING("error clearing entry"); */
	    return (int)FALSE;
	 }
      }
   }
/* Zero out table data. */
   else if(isTable){
      if( !dsSetTableRowCount(pDS,0) ){
/* TOO NOISY	 EML_WARNING("error clearing table"); */
	 return (int)FALSE;
      }
   }
  /* I have no idea what should be here, and I did 
     this to make Microsoft compiler happy. V.Fine 11.02.98 */
   return (int)FALSE; 
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
	case DIO_READ_MODE:		return "READONLY";
	case DIO_WRITE_MODE:		return "WRITEONLY";
	case DIO_UPDATE_MODE:		return "READWRITE";
	case DIO_UNKNOWN_MODE:
	default:			return "***UNKNOWN***";
	}
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      DIO_MODE_T dio_text2mode(const char* text)
*:DESCRIPTION:  ----------
*:ARGUMENTS:    ----------
*:RETURN VALUE: ----------
*:<---------------------------------------------------------------------
*/
DIO_MODE_T dio_text2mode(const char* text)
{
	switch(text[0]) {
	case 'r': case 'R':		return DIO_READ_MODE;
	case 'w': case 'W':		return DIO_WRITE_MODE;
	case 'u': case 'U':		return DIO_UPDATE_MODE;
	default:			return DIO_UNKNOWN_MODE;
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
const char* dio_state2text(DIO_STATE_T state)
{
	switch(state) {
	case DIO_OPEN_STATE:		return "OPENED";
	case DIO_CLOSE_STATE:		return "CLOSED";
	case DIO_READ_STATE:		return "READING";
	case DIO_WRITE_STATE:		return "WRITING";
	case DIO_UNKNOWN_STATE:
	default:			return "***UNKNOWN***";
	}
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      DIO_STATE_T dio_text2state(const char* text)
*:DESCRIPTION:  ----------
*:ARGUMENTS:    ----------
*:RETURN VALUE: state
*:<---------------------------------------------------------------------
*/
DIO_STATE_T dio_text2state(const char* text)
{
	switch(text[0]) {
	case 'o': case 'O':		return DIO_OPEN_STATE;
	case 'c': case 'C':		return DIO_CLOSE_STATE;
	case 'r': case 'R':		return DIO_READ_STATE;
	case 'w': case 'W':		return DIO_WRITE_STATE;
	default:			return DIO_UNKNOWN_STATE;
	}
}


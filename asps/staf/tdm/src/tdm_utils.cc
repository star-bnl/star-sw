/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:		tdm_utils.C
*:DESCRIPTION:	Utility functions  for AMI package.
*:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:		-- STILL IN DEVELOPMENT --
*:HISTORY:	06feb96-v001a-cet- moved from AMI to TDM
*:HISTORY:	19dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*-------------------------------------------- INCLUDES             --*/
#include <stdlib.h>
#include <string.h>
#include "emlLib.h"
#include "tdm_types.h"
#include "asuAlloc.h"

/*-------------------------------------------- MACROS               --*/
/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      tdm_cvtDst2st
*:DESCRIPTION:  Convert DSL table to 2-struct (TAS) table.
*:ARGUMENTS:    DS_DATASET_T *pT	- DSL table
*:ARGUMENTS:    TABLE_HEAD_ST *& tbl_h	- table header struct
*:ARGUMENTS:    char *& tbl_d		- table data struct array
*:RETURN VALUE: TRUE or FALSE
*:<---------------------------------------------------------------------
*/
int tdm_cvtDst2st(DS_DATASET_T *pT
		, TABLE_HEAD_ST *& tbl_h, char *& tbl_d)
{
   TABLE_HEAD_ST header;
   const char *name=(header.name);
   const char *type=(header.type);
   bool_t result;
   char* pData=NULL;

   if( !dsIsTable(&result,pT)
   ||  !result
   ||  !dsTableName(&name,pT)
   ||  !dsTableTypeName(&type,pT)
   ||  !dsTableMaxRowCount((size_t*)(&(header.maxlen)),pT)
   ||  !dsTableRowCount((size_t*)(&(header.nok)),pT)
   ||  !dsTableRowSize((size_t*)(&(header.rbytes)),pT)
   ||  !dsTableDataAddress(&pData,pT)
   ){
      EML_PUSHERROR(CANNOT_CONVERT_DSL_TABLE_TO_TAS_TABLE);
      return FALSE;
   }
   tbl_h = (TABLE_HEAD_ST*)MALLOC(sizeof(header));

//- HACK -- THIS SHOULD BE FREE()'D

/*- WARNING - Following strncpy's write \000 into next struct word. -*/
   strncpy(tbl_h->name,name,20);		/*- WARNING -*/
 
/* OSF1 ifdefs added 12 Dec. 1997 by J. Lajoie - lajoie@iastate.edu
   - use a cast to long (not int!) for 64-bit architecture 
  Herb Ward took the int out, so you may want to take the long out.
*/
#ifdef OSF1
   memset((char*)(long(tbl_h->name)+strlen(name)),' ',
	  20-strlen(name));
#else
   memset((char*)((tbl_h->name)+strlen(name)),' ',
	  20-strlen(name));
#endif
   
   strncpy(tbl_h->type,type,20);		/*- WARNING -*/

/* OSF1 ifdefs added 12 Dec. 1997 by J. Lajoie - lajoie@iastate.edu
   - use a cast to long (not int!) for 64-bit architecture */
#ifdef OSF1
   memset((char*)(long(tbl_h->type)+strlen(type)),' ',
	  20-strlen(type));
#else
   memset((char*)((tbl_h->type)+strlen(type)),' ',
	  20-strlen(type));
#endif

   tbl_h->maxlen = header.maxlen;
   tbl_h->nok = header.nok;
   tbl_h->rbytes = header.rbytes;
   tbl_h->dsl_pointer = (long)pT;
   tbl_h->data_pointer = (long)pData;
   tbl_d = pData;

   return TRUE;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      tdm_nameMatch
*:DESCRIPTION:  
*:ARGUMENTS:    
*:ARGUMENTS:    
*:ARGUMENTS:    
*:RETURN VALUE: 
*:<---------------------------------------------------------------------
*/
int tdm_nameMatch(char *a, char *b)
{
   if( 0 == strcmp(a,b) ){
      return TRUE;
   }
   else {
      return FALSE;
   }
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	tdm_printCell
*:DESCRIPTION:	print a table cell
*:ARGUMENTS:	TDM_COLUMN_T column
*:RETURN VALUE: 
*:<---------------------------------------------------------------------
*/
void tdm_printCell(FILE *stream, TDM_CELLDATA_T *data
		, TDM_COLUMN_T *col)
{
   int i;
   int count = data->_length;

   if( data->_d != col->code ){
      fprintf(stream, "Data and column are inconsistant.\n");
      return;
   }
   fprintf(stream, " %s = \n", col->name);
   switch(col->code) {

   case DS_TYPE_CHAR:
      fprintf(stream, "\t%s", data->data.c);
      break;

   case DS_TYPE_OCTET:
      for (i = 0; i < count; i++) {
	 fprintf(stream, "\t%u", data->data.o[i]);
      }
      break;

   case DS_TYPE_SHORT:
      for (i = 0; i < count; i++) {
	 fprintf(stream, "\t%hd", data->data.s[i]);
      }
      break;

   case DS_TYPE_U_SHORT:
      for (i = 0; i < count; i++) {
	 fprintf(stream, "\t%hu", data->data.us[i]);
      }
      break;

   case DS_TYPE_LONG:
      for (i = 0; i < count; i++) {
	 fprintf(stream, "\t%ld", data->data.l[i]);
      }
      break;

   case DS_TYPE_U_LONG:
      for (i = 0; i < count; i++) {
	 fprintf(stream, "\t%lu", data->data.ul[i]);
      }
      break;

   case DS_TYPE_FLOAT:
      for (i = 0; i < count; i++) {
	 fprintf(stream, "\t%g", data->data.f[i]);
      }
      break;

   case DS_TYPE_DOUBLE:
      for (i = 0; i < count; i++) {
	 fprintf(stream, "\t%g", data->data.d[i]);
      }
      break;

   case DS_TYPE_STRUCT:
      fprintf(stream, "Unable to print structs at this time.\n");
      break;

   default:
      fprintf(stream, "\tINVALID_TYPE");
      break;
   }
}


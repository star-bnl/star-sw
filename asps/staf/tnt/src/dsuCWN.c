#include <string.h>
#include "asuAlloc.h"
#include "dstype.h"
#include "dsuCWN.h"

#ifndef MIN
#define MIN(A,B) (A<B?A:B)
#endif

/*- bytes needed to store A in integral number of long-words -*/
/*- NB - For CWNs, "sizeof(long)" should really be "4" -*/
#ifndef LONGWORDIFY
#define LONGWORDIFY(A) (sizeof(long)*((A+sizeof(long)-1)/sizeof(long)))
#endif

/*----------------------------------------------------------------------
** return CWN chform string analogous to DSL table spec string
----------------------------------------------------------------------*/
char * 
dsuCWNdsSpec2chform(char *spec) {
   DS_DATASET_T *pTable = NULL;
   char *chform = NULL;
   char *pData = NULL;

   pTable = NULL; 
   pData = NULL;
   if( !dsNewTable(&pTable,"dsu__temp",spec,0,pData)
   ||  NULL == (chform = dsuTableChform(pTable))
   ||  !dsFreeDataset(pTable)
   ){
      EML_ERROR(INVALID_TABLE_SPEC);
   }
   return chform;
}

/*----------------------------------------------------------------------
** return DSL table spec string analogous to CWN chform string
----------------------------------------------------------------------*/
char * 
dsuCWNchform2dsSpec(char *chform)
{
/* /////////////////////////////////////////////////////// WORKING */
   EML_ERROR(NOT_YET_IMPLEMENTED);
}


/*----------------------------------------------------------------------
** create data buffer with 4-byte (longword) aligned table data cols
----------------------------------------------------------------------*/
STAFCV_T 
dsuLongwordifyBuffer(DS_DATASET_T *pTable, size_t irow, char **pData) {

  long ic;
  size_t size,colCount,colNumber;
  long expandedSize;
  
  /*- Calculate expanded size of row and alloc memory. -*/
  if( !dsTableColumnCount(&colCount,pTable)
      ||  !(0 < colCount)){
    EML_ERROR(INVALID_TABLE);
  }
  expandedSize = 0;
  for( ic=0;ic<colCount;ic++ ){
    if( !dsColumnSize(&size, pTable,colNumber) ){
      EML_ERROR(INVALID_TABLE_CELL);
    }
    expandedSize += LONGWORDIFY(size);
  }
  *pData = (char *) MALLOC(expandedSize);
  memset(pData,0,expandedSize);

  return STAFCV_OK;
}

/*----------------------------------------------------------------------
** fill data buffer with 4-byte (longword) aligned table data
----------------------------------------------------------------------*/
STAFCV_T 
dsuLongwordifyRow(DS_DATASET_T *pTable, size_t irow, char **ppData) 
{
   long ic,i;
   size_t colCount;

   size_t elCount;
   size_t colSize;
   DS_TYPE_CODE_T code;
   char *pCell;
   typedef union pbuff{			/* pointer to data buffer */
      char		*c;
      unsigned char	*o;
      short		*s;
      unsigned short	*us;
      long		*l;
      unsigned long	*ul;
      float		*f;
      double		*d;
      void		*v;
   }PBUFF;
   PBUFF src,dest;

   dest.c = *ppData;
/*- Copy each cell of row into new expanded row. -*/
   for( ic=0;ic<colCount;ic++ ){
      if( !dsColumnTypeCode(&code, pTable,ic)
      ||  !dsColumnElcount(&elCount, pTable, ic)
      ||  !dsCellAddress(&pCell, pTable, irow, ic)
      ||  !dsColumnSize(&colSize, pTable, ic)
      ){
	 EML_ERROR(INVALID_TABLE_CELL);
      }
      src.c = pCell;
      if( DS_TYPE_CHAR == code ){
	 memset(dest.c,' ',LONGWORDIFY(colSize));
	 strncpy(dest.c,src.c,colSize); 
	 dest.c[colSize]=0; /* hjw 19Feb98 */
	 src.c += colSize;
	 dest.c += LONGWORDIFY(colSize);
      }
      else {
	 for( i=0;i<elCount;i++ ){
	    switch(code) {
	    case DS_TYPE_CHAR:
	       break;
	    case DS_TYPE_OCTET:
	       *dest.ul = *src.o; break;
	    case DS_TYPE_SHORT:
	       *dest.l = *src.s; break;
	    case DS_TYPE_U_SHORT:
	       *dest.ul = *src.us; break;
	    case DS_TYPE_LONG:
	       *dest.l = *src.l; break;
	    case DS_TYPE_U_LONG:
	       *dest.ul = *src.ul; break;
	    case DS_TYPE_FLOAT:
	       *dest.f = *src.f; break;
	    case DS_TYPE_DOUBLE:
	       *dest.d = *src.d; break;
	    case DS_TYPE_STRUCT:
	    default:
	       break;
	    }
	    src.c += colSize/elCount;
	    dest.c += LONGWORDIFY(colSize)/elCount;
	 }
      }
   }

   return STAFCV_OK;
}

/*----------------------------------------------------------------------
** fill table row data with 4-byte (longword) boundried data from buffer
----------------------------------------------------------------------*/
STAFCV_T dsuUnLongwordifyRow(DS_DATASET_T *table, size_t irow
		, char **ppData)
{
/* /////////////////////////////////////////////////////// WORKING */
   EML_ERROR(NOT_YET_IMPLEMENTED);
}


/*----------------------------------------------------------------------
** return offset in bytes to table column data from start of row
----------------------------------------------------------------------*/
size_t dsuColumnOffset(DS_DATASET_T *table, size_t icolumn)
{
/* /////////////////////////////////////////////////////// WORKING */
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

/*----------------------------------------------------------------------
** return CWN chform for DSL table (";" == block delimiter)
----------------------------------------------------------------------*/
char * 
dsuTableChform(DS_DATASET_T *pTable)
{
   size_t colCount;
   int ic;
   DS_TYPE_CODE_T code,last;
   char *buffer,*b;
   char *chform,*c;

   buffer = (char *)MALLOC(2048);	/* HACK - Max size */
   b = buffer;

   /* Make sure this table has columns! */
   if (!dsTableColumnCount(&colCount,pTable) 
       || (colCount <= 0)) {
     dsPerror("INVALID_TABLE");
     chform = NULL;
     FREE(buffer);
     return chform;
   }
   if (!dsColumnTypeCode(&last, pTable, 0) 
       || NULL == (c = dsuColumnChform(pTable,0))) {
     dsPerror("INVALID_TABLE_COLUMN");
     chform = NULL;
     FREE(buffer);
     return chform;
   }
   strcpy(buffer,c); 
   FREE(c);
   for (ic = 1; ic < colCount; ic++) {
     if (!dsColumnTypeCode(&code, pTable, (size_t)ic)
	 ||  NULL == (c = dsuColumnChform(pTable, (size_t)ic))) {
       dsPerror("INVALID_TABLE_COLUMN");
       chform = NULL;
       FREE(buffer);
       return chform;
      }
     if ((DS_TYPE_CHAR == last && DS_TYPE_CHAR != code)
	 ||  (DS_TYPE_CHAR != last && DS_TYPE_CHAR == code)) {
       strcat(buffer,";");	/* next block */
     } else {
       strcat(buffer,",");	/* next variable */
     }
     strcat(buffer,c); FREE(c);
   }
   chform = (char*)MALLOC(strlen(buffer) + 1);
   FREE(buffer);
   return chform;
}

/*----------------------------------------------------------------------
** return CWN chform for column
----------------------------------------------------------------------*/
char * 
dsuColumnChform(DS_DATASET_T *pTable, size_t icolumn)
{
   const char *name;
   DS_TYPE_CODE_T code;
   size_t size, count, colNumber;

   char chn[9], *chname=chn;
   char chd[13], *chdim=chd;
   char cht[21], *chtype=cht;
   char *chform, *c;

   if( !dsColumnName(&name, pTable,colNumber)
   ||  !dsColumnTypeCode(&code, pTable,colNumber)
   ||  !dsColumnSize(&size, pTable, colNumber)
   ||  !dsColumnElcount(&count, pTable, colNumber)
   ){
      dsPerror("INVALID_TABLE_COLUMN");
      chform = NULL;
      return chform;
   }
   strncpy(chname,name,8); 
   chname[8]=0; /* hjw 19Feb98 */
   if( 1 < count ){
      sprintf(chdim,"(%d)",count);
   }
   else {
      chdim[0]=0;
   }
   switch(code) {
      case DS_TYPE_CHAR:
	 sprintf(chtype,"C*%d",MIN(32,LONGWORDIFY(size)));
         break;
      case DS_TYPE_OCTET:
	 strcpy(chtype,"U*4:8");	/* :[0,255]");		*/
         break;
      case DS_TYPE_SHORT:
	 strcpy(chtype,"I*4:16");	/* :[-32768,32767]");	*/
         break;
      case DS_TYPE_U_SHORT:
	 strcpy(chtype,"U*4:16");	/* :[0,65535]");	*/
         break;
      case DS_TYPE_LONG:
	 strcpy(chtype,"I*4");
         break;
      case DS_TYPE_U_LONG:
	 strcpy(chtype,"U*4");
         break;
      case DS_TYPE_FLOAT:
	 strcpy(chtype,"R*4");
         break;
      case DS_TYPE_DOUBLE:
	 strcpy(chtype,"R*8");
         break;
      case DS_TYPE_STRUCT:
      default:
	 dsPerror("INVALID_TABLE_COLUMN_TYPE");
	 c = NULL;
	 return c;
         break;
   }
   if( DS_TYPE_CHAR != code ){
      chform = (char *)MALLOC(strlen(chname) + strlen(chdim) +
   		strlen(chtype) +1);
      strcpy(chform,chname);
      strcat(chform,chdim);
      strcat(chform,chtype);
   }
   else {
      chform = (char *)MALLOC(strlen(chname) + strlen(chtype) +1);
      strcpy(chform,chname);
      strcat(chform,chtype);
   }
   return chform;
}


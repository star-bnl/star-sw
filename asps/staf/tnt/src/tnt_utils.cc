#include <stdlib.h>
#include "dstype.h"
#include "asuAlloc.h"
#include "tdmLib.h"
#include "tntLib.h" 
#include "asuAlloc.h"
#ifndef MIN
#define MIN(A,B) (A<B?A:B)
#endif

/*- bytes needed to store A in integral number of long-words -*/
/*- NB - For CWNs, "sizeof(long)" should really be "4" -*/
#ifndef LONGWORDIFY
#define LONGWORDIFY(A) (sizeof(long)*((A+sizeof(long)-1)/sizeof(long)))
#endif

char *
tntColumnChform(tdmTable *table, long iColumn) {
  char *chform = 0, *colName = 0;
  char temp[20];
  char chShape[20];
  char chtype[10];
  int i;

  colName = table->columnName(iColumn);

  switch(table->columnTypeCode(iColumn)) {
  case DS_TYPE_CHAR:
    sprintf(chtype,":C*%d",MIN(32,tntLongwordifyColumnSize(table,iColumn)));
    break;
  case DS_TYPE_OCTET:
    strcpy(chtype,":U*4:8");	/* :[0,255]");		*/
    break;
  case DS_TYPE_SHORT:
    strcpy(chtype,":I*4:16");	/* :[-32768,32767]");	*/
    break;
  case DS_TYPE_U_SHORT:
    strcpy(chtype,":U*4:16");	/* :[0,65535]");	*/
    break;
  case DS_TYPE_LONG:
    strcpy(chtype,":I*4");
    break;
  case DS_TYPE_U_LONG:
    strcpy(chtype,":U*4");
    break;
  case DS_TYPE_FLOAT:
    strcpy(chtype,":R*4");
    break;
  case DS_TYPE_DOUBLE:
    strcpy(chtype,":R*8");
    break;
  case DS_TYPE_STRUCT:
  default:
    EML_ERROR(INVALID TYPE);
  }
  
  chShape[0] = 0; // Reset string to null.
  if (table->columnTypeCode(iColumn) != DS_TYPE_CHAR) {
    for (i = 0; i < table->columnRank(iColumn); i++) {
      if (i == 0) {
	sprintf(chShape,"(%ld",table->columnShape(iColumn,i));
      } else { 
	sprintf(temp,",%ld",table->columnShape(iColumn,i));
	strcat(chShape,temp);
      }
    }
    if (table->columnRank(iColumn) != 0) {
      sprintf(temp,")");
      strcat(chShape,temp);
    }    
  }

  chform = (char *) MALLOC(strlen(colName) + strlen(chShape) 
			   + strlen(chtype) + 1);
  strcpy(chform,colName);
  FREE(colName);
  strcat(chform,chShape);
  strcat(chform,chtype);
  
  return chform;
}

size_t
tntLongwordifyRowSize(tdmTable *table) 
{
  long i;
  long size, longwordifiedSize;
  
  /*- Calculate longwordified size of row */
  if(table->columnCount() <= 0) {
    EML_CONTEXT("ERROR: table has fewer than 1 columns.\n");
    EML_ERROR(INVALID_TABLE);
  }

  longwordifiedSize = 0;
  for (i = 0; i < table->columnCount(); i++) {
    size = tntLongwordifyColumnSize(table, i);
    longwordifiedSize += size;
  }

  return longwordifiedSize;
}

size_t
tntLongwordifyColumnSize(tdmTable *table, long iColumn) 
{
  size_t size, longwordifiedSize;
  
  size = table->columnSize(iColumn);
  longwordifiedSize = LONGWORDIFY(size);

  return longwordifiedSize;
}

//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        tdmClasses.C
//:DESCRIPTION: Table & Dataset Memory Orbix-object C++ code
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     23dec96-v004a-cet- OLD_DSL now an option
//:HISTORY:     18apr96-v003c-cet- create tdmObject class
//:HISTORY:     30nov95-v003b-cet- move dui to seperate ASP
//:HISTORY:     29nov95-v003a-cet- get working with MOAST
//:HISTORY:     13sep95-v002b-cet- rework ctors & dtors
//:HISTORY:	13sep95-v002a-cet- manager now a factory, add dui...
//:BUGS:        07jul95-v001a-cet- memory is not returned in dtors.
//:HISTORY:     06jul95-v000a-cet- creation
//:<--------------------------------------------------------------------

//:----------------------------------------------- INCLUDES           --
#include <sys/types.h>
#ifndef WIN32
# include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "asuAlloc.h"
#include "emlLib.h"
#include "sutLib.h"
#include "socLib.h"
#include "tdmClasses.hh"

//:----------------------------------------------- MACROS             --
#define TOKENS " \n\t"
#define LINESIZE 2000
#define MCIF      500  /* max cols in temp file, incs vector multiplicity */
#define COLLIST   200
#define NCOL      200  /* max cols in user specification */
#include "tdm_macros.h"
#ifndef MIN
#define MIN(A,B) ( ( A < B ) ? A : B )
#endif /*MIN*/
#define VALID_CELL(R,C) ( (0 <= R && R < maxRowCount()) \
		&& (0 <= C && C < columnCount()) )
#define FULL_ROW(R) (0 <= R && R < rowCount())
#define EMPTY_ROW(R) (rowCount() <= R && R < maxRowCount())

//:----------------------------------------------- PROTOTYPES         --
//extern CC_P int dsTypeSpecifier(char **ptr, size_t *pLen, size_t tid);
extern CC_P void dsuPrintData(FILE *stream , DS_TYPE_CODE_T type
		, unsigned int count , void *data);

//:#####################################################################
//:=============================================== CLASS              ==
// tdmObject

//:----------------------------------------------- CTORS & DTOR       --
   tdmObject:: tdmObject()
		: socObject("NULL","tdmObject") {
      pDSthis = NULL;
   }

   tdmObject:: tdmObject(const DS_DATASET_T* pDS)
		: socObject(pDS->name, "tdmObject") {
      pDSthis = (DS_DATASET_T *)pDS;
   }

   tdmObject:: ~tdmObject(){ };
//:----------------------------------------------- ATTRIBUTES         --
char * tdmObject::  dslName () {
   const char *c=NULL;
   char *cc=NULL;
   if( isDataset() ){
      if( !dsDatasetName(&c,pDSthis) ){
	 EML_PUSHERROR(DSL_ERROR);
	 return NULL;
      }
   }
   else if( isTable() ){
      if( !dsTableName(&c,pDSthis) ){
	 EML_PUSHERROR(DSL_ERROR);
	 return NULL;
      }
   }
   else {
     return NULL;
   }
   cc = (char*)MALLOC(strlen(c)+1);
   strcpy(cc,c);
   return cc;
}

//----------------------------------
unsigned char tdmObject:: isDataset () {

   bool_t r;

   if( !dsIsDataset(&r, pDSthis) ){
      EML_PUSHERROR(DSL_ERROR);
      return FALSE;
   }
   if( r ){ return TRUE; }
   else { return FALSE; }
}

//----------------------------------
unsigned char tdmObject:: isTable () {

   bool_t r;

   if( !dsIsTable(&r, pDSthis) ){
      EML_PUSHERROR(DSL_ERROR);
      return FALSE;
   }
   if( r ){ return TRUE; }
   else { return FALSE; }
}

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char tdmObject:: implementsInterface (const char * iface) {
   if( 0 == strcmp("tdmObject",iface)
   ||  socObject::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//- Use this function ONLY in a collocated process.
DS_DATASET_T * tdmObject:: dslPointer() {
   return pDSthis;
}

//:----------------------------------------------- PROT FUNCTIONS     --

//:#####################################################################
//:=============================================== CLASS              ==
// tdmTable

//:----------------------------------------------- CTORS & DTOR       --
tdmTable:: tdmTable(const char * name, const DS_DATASET_T *pDS)
		: tdmObject(pDS)
		, socObject(name, "tdmTable") {
   myPtr = (SOC_PTR_T)this;
   bool_t result;

   if( !dsIsTable(&result, (DS_DATASET_T *)pDS)
   ||  !result
   ){
      EML_PUSHERROR(DSL_ERROR);
      pDSthis = NULL;
   }
   pDSthis = (DS_DATASET_T *)pDS;
}

//----------------------------------
tdmTable:: tdmTable(const DS_DATASET_T *pDS)
		: tdmObject(pDS)
		, socObject(pDS->name, "tdmTable") {
   myPtr = (SOC_PTR_T)this;
   bool_t result;

   if( !dsIsTable(&result, (DS_DATASET_T *)pDS)
   ||  !result
   ){
      EML_PUSHERROR(DSL_ERROR);
      pDSthis = NULL;
   }
   pDSthis = (DS_DATASET_T *)pDS;
}

//----------------------------------
tdmTable:: tdmTable(const char *name, const char * spec, long rows)
		: tdmObject()
		, socObject(name, "tdmTable") {
   myPtr = (SOC_PTR_T)this;
   pDSthis = NULL;
   char *pData = NULL;
   if( !dsNewTable(&pDSthis, (char*)name, (char*)spec, rows, pData)
   ||  !dsAllocTables(pDSthis)
   ){
      EML_PUSHERROR(DSL_ERROR);
      pDSthis = NULL;
   }
}

//----------------------------------
tdmTable:: ~tdmTable() { }

//:----------------------------------------------- ATTRIBUTES         --
long tdmTable::  columnCount () {
   size_t ccount;

   if(!dsTableColumnCount(&ccount,pDSthis)) {
      EML_PUSHERROR(DSL_ERROR);
      return -1; /*-TDM_E_BAD_COLUMNCOUNT-*/
   }
   return (long)ccount;
}

//----------------------------------
/* OVER-RIDE tdmObject::dslName */
char * tdmTable::  dslName () {
   const char* c=NULL;
   char* cc=NULL;
   if( !dsTableName(&c, pDSthis) ){
      EML_PUSHERROR(DSL_ERROR);
      return NULL;
   }
   cc = (char*)MALLOC(strlen(c) +1);
   strcpy(cc,c);
   return cc;
}

//----------------------------------
void tdmTable:: maxRowCount (long maxRowCount) {
   if( !dsReallocTable(pDSthis, (size_t)maxRowCount) ){
      dsPerror("DSL_MAXROWCOUNT_UNCHANGED");
   }
}

//----------------------------------
long tdmTable::  maxRowCount () {
   size_t count;
   if( !dsTableMaxRowCount(&count, pDSthis) ){
      EML_PUSHERROR(DSL_ERROR);
      return -1; /*-TDM_E_BAD_MAXROWCOUNT-*/
   }
   return (long)count;
}

//----------------------------------
void tdmTable:: rowCount (long rowCount) {
   if( !dsSetTableRowCount(pDSthis, (size_t)rowCount) ){
      dsPerror("DSL_ROWCOUNT_UNCHANGED");
   }
}

//----------------------------------
long tdmTable::  rowCount () {
   size_t count;
   if( !dsTableRowCount(&count, pDSthis) ){
      EML_PUSHERROR(DSL_ERROR);
      return -1; /*-TDM_E_BAD_ROWCOUNT-*/
   }
   return (long)count;
}

//----------------------------------
long tdmTable::  rowSize () {
   size_t rsize;

   if(!dsTableRowSize(&rsize,pDSthis)) {
      EML_PUSHERROR(DSL_ERROR);
      return -1; /*-TDM_E_BAD_ROWSIZE-*/
   }
   return (long)rsize;
}

//----------------------------------
char * tdmTable::  typeName () {
   const char* tname=NULL;
   char* tn=NULL;

   if(!dsTableTypeName(&tname,pDSthis)) {
      EML_PUSHERROR(DSL_ERROR);
      return NULL;
   }
   tn = (char*)MALLOC(strlen(tname) +1);
   strcpy(tn,tname);
   return tn;
}

//----------------------------------
char * tdmTable::  typeSpecifier () {
   const char* tspec=NULL;

   if(!dsTableTypeSpecifier(&tspec,pDSthis)) {
      EML_PUSHERROR(DSL_ERROR);
      return NULL;
   }
   char *c=(char*)MALLOC(strlen(tspec) +1);
   strcpy(c,tspec);
   return c;
}

//----------------------------------
// override socObject::listing()
char * tdmTable::  listing () {
   char* c = socObject::listing();
   char* cc = NULL;
   cc = (char*)MALLOC(79+100); /*fix write bad  -akio*/
   sprintf(cc,"%s %ld/%ld rows; %ld bytes",c,rowCount()
		,maxRowCount() ,rowSize());
   FREE(c);
   return cc;
}

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char tdmTable :: implementsInterface (const char * iface) {
   if( 0 == strcmp("tdmTable",iface)
   ||  tdmObject::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;

}

//-02feb96- return result as return value, not argument
unsigned char tdmTable:: isType (const char * aType) {
   bool_t rslt;
   if( !dsTableIsType(&rslt, pDSthis, (char*)aType) ){
      EML_PUSHERROR(DSL_ERROR);
      return FALSE;
   }
   return rslt;
}
STAFCV_T tdmTable::dumpRows(long ifirst,long nrows,char *out,char *colList) {

  FILE *gg,*ff; long i; DS_TYPE_T *dstype; char *pCellData; char *c=NULL;
  char colListCopy[COLLIST+1],tmp[100],haveSetMask=0,mask[MCIF];
  char nformat[100],format[100];
  char *col[NCOL],line[LINESIZE+2],linecopy[LINESIZE+2],*cc,*dd;
  int maxlineindex,pass,size[MCIF],imask,nmask,ncol,lineindex;

  if(strlen(colList)>COLLIST) EML_ERROR(COL_LIST_TOO_LONG);
  strcpy(colListCopy,colList); 
  cc=strtok(colListCopy,"^"); ncol=0;
  while(cc) {
    if(ncol>=NCOL) EML_ERROR(COL_LIST_HAS_TOO_MANY_ITEMS);
    col[ncol++]=cc; cc=strtok(NULL,"^");
  }
  sprintf(tmp,"/tmp/staf.dump.%d",getpid());
  ff=fopen(tmp,"w"); if(!ff) EML_ERROR(CANT_WRITE_FILE);
  if( !dsTypePtr(&dstype,pDSthis->tid)) {
     EML_ERROR(BAD_TABLE_TYPE);
  }

  long ii=rowCount(); if(ifirst<0||ii<=ifirst) EML_ERROR(INVALID_TABLE_ROW);

  /* Print Table Header Column Names-*/
  fprintf(ff," ROW #");
  for( i=0;i<columnCount();i++ ){
     fprintf(ff,"\t%s",c=columnName(i)); FREE(c);
     if( DS_TYPE_CHAR == columnTypeCode(i) ) {
	 long ii = columnElcount(i)-strlen(c=columnName(i));
	 FREE(c);
	 for(;0<ii;ii--){ fprintf(ff," "); }
     } else {
	 if( 1 < columnElcount(i) ){
	    fprintf(ff,"[0]");
	    for( long n=1;n<columnElcount(i);n++ ){
	       if( (8 > n)
	       ||  (columnElcount(i) == n)
	       ||  (12 > columnElcount(i))
	       ){
		  fprintf(ff,"\t%s[%ld]",c=columnName(i),n); FREE(c);
	       } else if( (8 == n) ) {
		  fprintf(ff,"\t***%ld HEADERS UNPRINTED",columnElcount(i)-9);
	       }
	    }
	 }
     }
  }
  fprintf(ff,"\n");
  /* Print Table Data */
  pCellData = (char*)pDSthis->p.data;
  pCellData += ifirst*rowSize();
  for( i=ifirst;i<MIN(ii,ifirst+nrows);i++){
     fprintf(ff,"%6ld:",i); dsPrintData(ff, dstype, 1, pCellData);
     pCellData += rowSize(); fprintf(ff,"\n");
  }
  fclose(ff);
  for(pass=0;pass<2;pass++) { /* first pass compiles info (eg, col widths) */
    ff=fopen(tmp,"r"); if(!ff) EML_ERROR(CANT_READ_FILE);
    if(pass==1) {
      if(strcmp(out,"screen")) {
        gg=fopen(out,"w"); if(!gg) EML_ERROR(CANT_WRITE_FILE); 
      } else gg=stdout;
    }
    lineindex=0; if(pass==0) maxlineindex=0;
    if(pass==1) {
      sprintf(format,"%d",maxlineindex+ifirst);
      sprintf(nformat,"%%%dd ",strlen(format));
    }
    while(fgets(line,LINESIZE,ff)) {
      if(strlen(line)>LINESIZE-5) {
        fclose(ff); if(pass==1&&strcmp(out,"screen")) fclose(gg); 
        EML_ERROR(LINE_TOO_BIG); /* too many cols */
      }
      if(!haveSetMask) {
        haveSetMask=7;
        for(i=MCIF-1;i>=0;i--) { size[i]=0; mask[i]=0; }
        nmask=0;
        strcpy(linecopy,line); cc=strtok(linecopy,TOKENS);
        cc=strtok(NULL,TOKENS); cc=strtok(NULL,TOKENS); /* discard first 2 */
        while(cc) {
          if(nmask>=MCIF) {
            fclose(ff); if(pass==1&&strcmp(out,"screen")) fclose(gg); 
            EML_ERROR(TOO_MANY_COLS);
          }
          dd=strstr(cc,"["); if(dd) dd[0]=0;
          for(i=0;i<ncol;i++) {
            if( !strcmp(col[i],cc) || !strcmp(col[i],"allColumns") ) {
              mask[nmask]=7; break;
            }
          }
          cc=strtok(NULL,TOKENS); nmask++;
        }
      }
      if(lineindex<=0) {
        strtok(line,TOKENS); strtok(NULL,TOKENS); 
        if(pass==1) for(ii=atoi(nformat+1);ii>=0;ii--) fprintf(gg," ");
      } else {
        strtok(line,TOKENS); 
        if(pass==1) fprintf(gg,nformat,lineindex+ifirst-1); /* -1 <-colheads*/
      }
      cc=strtok(NULL,TOKENS); imask=0;
      while(cc) {
        if(mask[imask]) {
          if(pass==0) {
            if(size[imask]<strlen(cc)) size[imask]=strlen(cc);
          } else {
            sprintf(format,"%%%ds ",size[imask]); fprintf(gg,format,cc); 
          }
        }
        cc=strtok(NULL,TOKENS); imask++;
      }
      if(pass==1) fprintf(gg,"\n");
      lineindex++; if(pass==0) maxlineindex++;
    }
    fclose(ff); 
    if(pass==1) { if(strcmp(out,"screen")) fclose(gg); }
  }
  if(strcmp(out,"screen")) printf("Have written %s:  vi %s   emacs %s\n",
    out,out,out);
  EML_SUCCESS(STAFCV_OK);
}
STAFCV_T tdmTable:: printRows (long ifirst, long nrows) {

   long i;
   DS_TYPE_T *dstype;
   char *pCellData;
   char *c=NULL;

   if( !dsTypePtr(&dstype,pDSthis->tid)) {
      EML_ERROR(BAD_TABLE_TYPE);
   }
   long ii=rowCount();
   if(ifirst < 0 || ii <= ifirst){
      EML_ERROR(INVALID_TABLE_ROW);
   }
/*- Print Table Header Column Names -*/
   fprintf(stdout," ROW #");
   for( i=0;i<columnCount();i++ ){
      fprintf(stdout,"\t%s",c=columnName(i));
      /*HACK:                 FREE(c);        UNKNOWN BUG*/
      FREE(c);  /*fix memory leak, or take out HACK. Works fine  -akio*/
      if( DS_TYPE_CHAR == columnTypeCode(i) ){
	 long ii = columnElcount(i)-strlen(c=columnName(i));
 	 FREE(c);
	 for(;0<ii;ii--){ fprintf(stdout," "); }
//	 for(long ii=0;ii<iii;i++){ fprintf(stdout," "); }
      }
      else {
	 if( 1 < columnElcount(i) ){
	    fprintf(stdout,"[0]");
	    for( long n=1;n<columnElcount(i);n++ ){
	       if( (8 > n)
	       ||  (columnElcount(i) == n)
	       ||  (12 > columnElcount(i))
	       ){
		  fprintf(stdout,"\t%s[%ld]",c=columnName(i),n);
		  FREE(c);
	       }
	       else if( (8 == n) ){
		  fprintf(stdout,"\t***%ld HEADERS UNPRINTED***"
			   ,columnElcount(i)-9);
	       }
	    }
	 }
      }
   }
   fprintf(stdout,"\n");
/*- Print Table Data -*/
   pCellData = (char*)pDSthis->p.data;
   pCellData += ifirst*rowSize();
   for( i=ifirst;i<MIN(ii,ifirst+nrows);i++){
      fprintf(stdout,"%6ld:",i);
/*REPLACE *** dsPrintData with dsuPrintData ***
      dsPrintData(stdout, dstype
		, 1 
		, (char *)pDSthis->p.data +i*dstype->size);
*/
/*REPLACE *** dsuPrintData with dsPrintData ***
      for(j=0;j<columnCount();j++){
	 dsuPrintData(stdout, columnTypeCode(j), columnElcount(j)
			, pCellData);
	 pCellData += columnSize(j);
      }
*/
      dsPrintData(stdout, dstype, 1, pCellData);
      pCellData += rowSize();
      fprintf(stdout,"\n");
   }
   
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
char * 
tdmTable::printRow (long nrow) {
  // Just to hush pedantic compilers
  static void *pn = &nrow;
  
  return NULL;
}

//----------------------------------
STAFCV_T tdmTable:: show () {
   const char* tspec=NULL;

   if(!dsTableTypeSpecifier(&tspec,pDSthis)) {
      EML_ERROR(DSL_ERROR);
   }
   printf("%s \n",tspec);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T tdmTable:: findColumn (TDM_COLUMN_T& column
		, const char * name) {
   long ncol = columnNumber(name);
   return getColumn(column,ncol);
}

//----------------------------------
STAFCV_T tdmTable:: getColumn (TDM_COLUMN_T& column, long ncol) {
   if(ncol < 0 || columnCount() <= ncol ){
      EML_ERROR(INVALID_TABLE_COLUMN);
   }
   column.nCol = ncol;
   column.name = columnName(ncol);
   column.type = columnTypeName(ncol);
   column.code = columnTypeCode(ncol);
   column.size = columnSize(ncol);
   column.rank = columnRank(ncol);
   for(int i=0;i<column.rank;i++) column.shape[i] = columnShape(ncol,i);
   column.elcount = columnElcount(ncol);

   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
long tdmTable:: columnNumber (const char * name) {
   size_t cnum;
   if( !dsFindColumn(&cnum, pDSthis, (char*)name) ){
      EML_PUSHERROR(DSL_ERROR);
      return -1; /*-TDM_E_COLUMN_NOTFOUND-*/
   }
   return (long)cnum;
}

//----------------------------------
char * tdmTable:: columnName (long ncol) {
   const char *c;
   if( !dsColumnName(&c,pDSthis,ncol) ){
      return NULL;
   }
   char *cc = (char*)MALLOC(strlen(c) +1);		/*HACK:LEAK*/
   strcpy(cc,c);
   return cc;
}

//----------------------------------
DS_TYPE_CODE_T tdmTable:: columnTypeCode (long ncol) {
   DS_TYPE_CODE_T code;
   if( !dsColumnTypeCode(&code,pDSthis,ncol) ){
//    return TDM_TYPE_UNKNOWN;
      					/* this should never happen*/
      EML_PUSHERROR(DSL_INVALID_TYPE_CODE);
   }
   return code;
}

//----------------------------------
char * tdmTable:: columnTypeName (long ncol) {
   const char *c;
   if( !dsColumnTypeName(&c,pDSthis,ncol) ){
      return NULL;
   }
   char *cc = (char*)MALLOC(strlen(c) +1);		/*HACK:LEAK*/
   strcpy(cc,c);
   return cc;
}

//----------------------------------
long tdmTable:: columnSize (long ncol) {
   size_t c;
   if( !dsColumnSize(&c,pDSthis,ncol) ){
      return -1; /*-TDM_E_BAD_COLUMNSIZE-*/
   }
   return c;
}

//----------------------------------
long tdmTable:: columnRank (long ncol) {
   size_t c;
   if( !dsColumnDimCount(&c,pDSthis,ncol) ){
      return -1; /*-TDM_E_BAD_COLUMNRANK-*/
   }
   return c;
}

//----------------------------------
long tdmTable:: columnShape (long ncol, long ndim) {
   size_t c[8];		/* HACK - limit to RANK <= 8 */
   if( !dsColumnDimensions(c,pDSthis,ncol)
   ||  !( ndim < columnRank(ncol) )
   ){
      return -1; /*-TDM_E_BAD_COLUMNSHAPE-*/
   }
   return c[ndim];
}

//----------------------------------
long tdmTable:: columnElcount (long ncol) {
   size_t c;
   if( !dsColumnElcount(&c,pDSthis,ncol) ){
      return -1; /*-TDM_E_BAD_COLUMNELCOUNT-*/
   }
   return c;
}

//----------------------------------
//- DATA ACCESS
//----------------------------------
STAFCV_T tdmTable:: getCell (TDM_CELLDATA_T& data
		, long nrow, long ncol) {
   if( !VALID_CELL(nrow,ncol) ) {
     EML_CONTEXT("ERROR: Check that row number %d is in range (A).\n",nrow);
     EML_ERROR(INVALID_TABLE_CELL);
   }
   data.data.v = cellAddress(nrow,ncol);
   if(!data.data.v) {
     EML_CONTEXT("ERROR: Check that row number %d is in range (B).\n",nrow);
     EML_ERROR(INVALID_ROW_OR_COLUMN);
   }
   data._d = columnTypeCode(ncol);
   data._maximum = data._length = data._size = columnSize(ncol);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T tdmTable:: putCell (const TDM_CELLDATA_T& data
		, long nrow, long ncol) {
   if( !VALID_CELL(nrow,ncol) ) EML_ERROR(INVALID_TABLE_CELL);
   void *pData = cellAddress(nrow,ncol);
   if(!pData) {
      EML_CONTEXT("ERROR: Check that row number %d is in range (C).\n",nrow);
      EML_ERROR(INVALID_ROW_OR_COLUMN);
   }
   memcpy(pData,data.data.v,columnSize(ncol));
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T tdmTable:: getData (TDM_DATABLOCK_T& data) {
   size_t nrows,mxrow,rsize;
   char* pData;

   if( !dsTableDataAddress(&pData,pDSthis)
   ||  !dsTableRowCount(&nrows,pDSthis)
   ||  !dsTableMaxRowCount(&mxrow,pDSthis)
   ||  !dsTableRowSize(&rsize,pDSthis)
   ){
      data._maximum = 0;
      data._length = 0;
      data._buffer = NULL;
      EML_ERROR(BAD_TABLE);
   }
   data._maximum = mxrow*rsize;
   data._length = nrows*rsize;
   data._buffer = (unsigned char*)MALLOC(data._maximum);
   memcpy(data._buffer,pData,data._length);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T tdmTable:: putData (const TDM_DATABLOCK_T& data) {
   size_t nrows,mxrow,rsize;
   char* pData;

   if( !dsTableDataAddress(&pData,pDSthis)
   ||  !dsTableRowCount(&nrows,pDSthis)
   ||  !dsTableMaxRowCount(&mxrow,pDSthis)
   ||  !dsTableRowSize(&rsize,pDSthis)
   ||  data._length<=0 || mxrow*rsize < data._length
   ){
      EML_ERROR(BAD_TABLE);
   }
   memcpy(pData,data._buffer,data._length);
   EML_SUCCESS(STAFCV_OK);

}

//----------------------------------------------------------------------
//- CO-LOCATED METHODS - DO NOT USE THESE IN DISTRIBUTED SYSTEM
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//- TEMPORARY LEGACY METHODS
//----------------------------------------------------------------------
STAFCV_T tdmTable:: cvtDslPointer(DSL_PTR_T& pDS) {

   pDS = (DSL_PTR_T)pDSthis;
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T tdmTable:: cvtTasStructs(TABLE_HEAD_ST *& head
		, char *& data) {

   return tdm_cvtDst2st(pDSthis,head,data);
}

void * tdmTable:: cellAddress(long nrow, long ncol) {

   char *pData;
   size_t rMax, rCount, cCount, rSize;
   size_t cSize;

   if( !dsTableMaxRowCount(&rMax,pDSthis)
   ||  !dsTableRowCount(&rCount,pDSthis)
   ||  !dsTableColumnCount(&cCount,pDSthis)
   ||  !dsTableRowSize(&rSize,pDSthis)
   ||  !(0 <= nrow && nrow < (long) rMax)
   ||  !(0 <= ncol && ncol < (long) cCount)
   ){
      return NULL;
   }

   if( 0 <= nrow && nrow < (long) rCount ){
      if( !dsCellAddress(&pData, pDSthis, nrow, ncol) ){
	 return NULL;
      }
   }
   else {
      return NULL;   // hjw Sun Jul  5 11:31:49 EDT 1998
      /* hjw Sun Jul  5 11:31:49 EDT 1998, this returns a pointer
      ** to a position within the first row, which seems incorrect
      ** when the row number is out of range.
      ** So, I am commenting it and substituting a return of NULL.
      **
      ** if( !dsTableDataAddress(&pData,pDSthis) ){
      **   return NULL;
      ** }
      ** for(int i=0;i<ncol;i++){
      **   if( !dsColumnSize(&cSize,pDSthis,i) ){
      **     return NULL;
      **   }
      **   pData += cSize;
      ** }
      ** hjw Sun Jul  5 11:31:49 EDT 1998 */
   }
   return (void*)pData;
}
//:----------------------------------------------- PROT FUNCTIONS     --

//:#####################################################################
//:=============================================== CLASS              ==
// tdmDataset

//:----------------------------------------------- CTORS & DTOR       --
tdmDataset:: tdmDataset(const char * name, const DS_DATASET_T *pDS)
		: tdmObject(pDS)
		, socObject(name, "tdmDataset") {
   myPtr = (SOC_PTR_T)this;
   bool_t result;

   if( !dsIsDataset(&result, (DS_DATASET_T *)pDS)
   ||  !result
   ){
      EML_PUSHERROR(DSL_NO_DATASET_POINTER);
      pDSthis = NULL;
   }
   pDSthis = (DS_DATASET_T *)pDS;
}

//----------------------------------
tdmDataset:: tdmDataset(const DS_DATASET_T *pDS)
		: tdmObject(pDS)
		, socObject(pDS->name, "tdmDataset") {
   myPtr = (SOC_PTR_T)this;
   bool_t result;

   if( !dsIsDataset(&result, (DS_DATASET_T *)pDS)
   ||  !result
   ){
      EML_PUSHERROR(DSL_NO_DATASET_POINTER);
      pDSthis = NULL;
   }
   pDSthis = (DS_DATASET_T *)pDS;
}

//----------------------------------
tdmDataset:: tdmDataset(const char* name, long setDim)
		: socObject(name, "tdmDataset") {
  // Just to hush pedantic compilers
  static void *ps = &setDim;

   myPtr = (SOC_PTR_T)this;
   pDSthis = NULL;
#ifndef	OLD_DSL
   if( !dsNewDataset(&pDSthis, (char*)name) ){
#else	/*OLD_DSL*/
   if( !dsNewDataset(&pDSthis, (char*)name, setDim) ){
#endif	/*OLD_DSL*/
      EML_PUSHERROR(DSL_NO_DATASET_POINTER);
      pDSthis = NULL;
   }
}

//----------------------------------
tdmDataset:: ~tdmDataset() {
}

//:----------------------------------------------- ATTRIBUTES         --
/* OVER-RIDE tdmObject::dslName */
char * tdmDataset::  dslName () {
   const char* c;
   char* cc=NULL;
   if( !dsDatasetName(&c, pDSthis) ){
      EML_PUSHERROR(DSL_ERROR);
      return NULL;
   }
   cc = (char*)MALLOC(strlen(c) +1);
   strcpy(cc,c);
   return cc;
}

//----------------------------------
long tdmDataset::  entryCount () {
   size_t count;
   if( !dsDatasetEntryCount(&count, pDSthis) ){
      EML_PUSHERROR(DSL_ERROR);
      return -1; /*-TDM_E_BAD_ENTRYCOUNT-*/
   }
   return count;
}

//----------------------------------
#ifdef OLD_DSL
long tdmDataset::  maxEntryCount () {
   size_t count;
   if( !dsDatasetMaxEntryCount(&count, pDSthis) ){
      EML_PUSHERROR(DSL_ERROR);
      return -1; /*-TDM_E_BAD_MAXENTRYCOUNT-*/
   }
   return count;
}
#endif /*OLD_DSL*/

//----------------------------------
// override socObject::listing()
char * tdmDataset::  listing () {
   char* c = socObject::listing();
   char* cc = NULL;
   cc = (char*)MALLOC(79);
   memset(cc,0,79);
   sprintf(cc,"%s %ld ent.s",c,entryCount());
   FREE(c);
   return cc;
}

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char tdmDataset :: implementsInterface (const char * iface) {
   if( 0 == strcmp("tdmDataset",iface)
   ||  tdmObject::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;

}

STAFCV_T tdmDataset:: addDataset (const char * name, long setDim) {
  // Just to hush pedantic compilers
  static void *pn = &name;
  static void *ps = &setDim;

//BUG- add dsAddDataset functionality
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T 
tdmDataset::addTable (const char * name, const char * spec, long rows) {
  // Just to hush pedantic compilers
  static void *pn = &name;
  static void *ps = &spec;
  static void *pr = &rows;
  

//BUG- add dsAddTable functionality
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T tdmDataset:: getEntryType (char *& type, long num) {
  // Just to hush pedantic compilers
  static void *pn = &num;

//BUG- should return "table" or "dataset"
   type = NULL;
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T tdmDataset:: getDatasetEntry (tdmDataset*& dataset
		, long num) {
  // Just to hush pedantic compilers
  static void *pn = &num;

//BUG- add dsDatasetEntry functionality for dataset
   dataset = NULL;
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T tdmDataset:: cvtDslPointer(DSL_PTR_T& pDS) {
   pDS = (DSL_PTR_T)pDSthis;
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T tdmDataset:: getTableEntry (tdmTable*& table, long num) {
  // Just to hush pedantic compilers
  static void *pn = &num;

//BUG- add dsDatasetEntry functionality for table
   table = NULL;
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T tdmDataset:: findDatasetEntry (tdmDataset*& dataset
		, const char * name) {
  // Just to hush pedantic compilers
  static void *pn = &name;

//BUG- add dsFindEntry functionality for dataset
   dataset = NULL;
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T 
tdmDataset::findTableEntry (tdmTable*& table, const char * name) 
{
  // Just to hush pedantic compilers
  static void *pn = &name;

  //BUG- add dsFindEntry functionality for table
   table = NULL;
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T tdmDataset:: getDescriptor (char *& descriptor) {
//BUG- return full dataset description
   descriptor = NULL;
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**

//:#####################################################################
//:=============================================== CLASS              ==
//: tdmFactory

//:----------------------------------------------- CTORS & DTOR       --
tdmFactory:: tdmFactory()
		: socFactory()
		, socObject() {
// EML_MESSAGE(TDM_FACTORY_NULL_CREATOR);
}

tdmFactory:: tdmFactory(const char * name)
		: socFactory()
		, socObject(name, "tdmFactory") {
   myPtr = (SOC_PTR_T)this;
   lock(TRUE);
}

//----------------------------------
tdmFactory:: ~tdmFactory() {
}

//:----------------------------------------------- ATTRIBUTES         --
//:**NONE**

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char tdmFactory :: implementsInterface (const char * iface) {
   if( 0 == strcmp("tdmFactory",iface)
   ||  socFactory::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//----------------------------------
STAFCV_T tdmFactory:: deleteDataset (const char * name) {
   if( !soc->deleteObject(name,"tdmDataset") ){
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T tdmFactory:: deleteTable (const char * name) {
   if( !soc->deleteObject(name,"tdmTable") ){
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
tdmDataset* tdmFactory:: findDataset (const char * name) {
   socObject* obj=NULL;
   if( NULL == (obj = soc->findObject(name,"tdmDataset")) ){
      return NULL;
   }
   tdmDataset *d = TDMDATASET(obj);
   return d;
}

//----------------------------------
tdmTable* tdmFactory:: findTable (const char * name) {
   socObject* obj=NULL;
   if( NULL == (obj = soc->findObject(name,"tdmTable")) ){
      return NULL;
   }
   tdmTable *t = TDMTABLE(obj);
   return t;
}

//----------------------------------
tdmDataset* tdmFactory:: getDataset (IDREF_T id) {
   socObject* obj;
   if( (NULL == (obj = soc->getObject(id)))
   ||  (0 != strcmp(obj->type(),"tdmDataset"))
   ){
      return NULL;
   }
   tdmDataset* d = TDMDATASET(obj);
   return d;
}

//----------------------------------
tdmTable* tdmFactory:: getTable (IDREF_T id) {
   socObject* obj;
   if( (NULL == (obj = soc->getObject(id)))
   ||  (0 != strcmp(obj->type(),"tdmTable"))
   ){
      return NULL;
   }
   tdmTable* t = TDMTABLE(obj);
   return t;
}

//----------------------------------
char * tdmFactory:: list () {
   char tit[] = 
                "\n"
                "+-------------------------------------------"
                "-----------------------------------\n"
		"|******************** "
		"TDM - Table & Dataset Memory listing"
		" ********************\n"
                "%s\n";
   char *c = socFactory::list();

   char *cc = (char*)MALLOC(strlen(c) +1 +strlen(tit));

   sprintf(cc,tit,c);
   FREE(c);
   return cc;

}

//----------------------------------
tdmDataset* tdmFactory:: newDataset (const char * name, long setDim) {
   IDREF_T id;
   if( soc->idObject(name,"tdmDataset",id) ){
      return NULL;
   }
   static tdmDataset* p;
   p = new tdmDataset(name,setDim);
   if( !soc->idObject(name,"tdmDataset",id) ){
      return NULL;
   }
   addEntry(id);
   return p;
}

//----------------------------------
tdmTable* tdmFactory:: newTable (const char * name, const char * spec
		, long rows) {
   IDREF_T id;
   if( soc->idObject(name,"tdmTable",id) ){
      return NULL;
   }
   static tdmTable* p;
   p = new tdmTable(name,spec,rows);
   if( !soc->idObject(name,"tdmTable",id) ){
      return NULL;
   }
   addEntry(id);
   return p;
}

//----------------------------------
tdmDataset* tdmFactory:: createDataset (const char * name
		, DS_DATASET_T *pDS) {
   IDREF_T id;
   if( soc->idObject(name,"tdmDataset",id) ){
      return NULL;
   }
   tdmDataset* p = new tdmDataset(name,pDS);
   addEntry(p->idRef());
   return p;
}

//----------------------------------
tdmTable* tdmFactory:: createTable (const char * name
		, DS_DATASET_T *pDS) {
   IDREF_T id;
   if( soc->idObject(name,"tdmTable",id) ){
      return NULL;
   }
   tdmTable* p = new tdmTable(name,pDS);
   addEntry(p->idRef());
   return p;
}

//----------------------------------
STAFCV_T tdmFactory:: getTypeName (long tid, char *& name) {
   const char 	*spec=NULL;	size_t lspec=0;
   const char 	*pre =NULL;	size_t lpre=0;
   const char 	*post=NULL;
   const char 	*buff=NULL;	size_t lbuff=0;

   if( !dsTypeSpecifier(&spec,&lspec,(size_t)tid)
   ||  (0 == (pre = strstr(spec,"struct")))
   ||  (0 == (post = strstr(spec,"{")))
   ||  (0 >= (post - pre))
   ){
      name = NULL;
      EML_ERROR(INVALID_TYPE_ID);
   }
   buff = (char*)pre;
   lpre = 6;
   buff += lpre;
   while ( !isalnum(buff[0]) && !(buff[0] == '_') ) buff++;
   lbuff = 0;
   while ( isalnum(buff[lbuff]) || (buff[lbuff] == '_') ) lbuff++;
   name = (char*)MALLOC(lbuff+1); 
   name[0]=0; strncat(name,buff,lbuff); 
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T tdmFactory:: getTypeSpecification (long tid, char *& spec) {
   const char *c;
   size_t l;

   if( !dsTypeSpecifier(&c,&l,(size_t)tid) ){
/*-   spec = NULL;				14jul97 BUGFIX -*/
      spec = (char*)MALLOC(9); strcpy(spec,"**NONE**");
      EML_ERROR(INVALID_TYPE_ID);
   }
   spec = (char*)MALLOC(l+1);
   strcpy(spec,c);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T tdmFactory:: findTypeSpecification (const char * name
		, char *& spec) {
   char *nm=NULL;

   for(int i=1;;i++){
      if( !getTypeName(i,nm) ){
         EML_ERROR(TYPE_NOT_FOUND);
      }
      if( (0 == strcmp(name,nm)) ){
         FREE(nm);
         getTypeSpecification(i,spec);
         EML_SUCCESS(STAFCV_OK);
      }
      FREE(nm);
   }
   EML_ERROR(INVALID_TYPE_NAME); /*- 14jul97 BUGFIX -*/
}

//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

// ---------------------------------------------------------------------


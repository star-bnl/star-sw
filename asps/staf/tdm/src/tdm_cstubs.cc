/*:
*:>---------------------------------------------------------------------
*:FILE:         tdm_cstubs.cc
*:DESCRIPTION:  C++ KUIP Action Modules for TDM
*:AUTHOR:       
*:BUGS:        
*:<---------------------------------------------------------------------
*/
#undef CORBA
/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#include "dstype.h"

#include "asuAlloc.h"
#include "emlLib.h"
#include "tdmLib.h"
#include "sutLib.h"
#include "ahsLib.h"

extern CC_P void dsuPrintData(FILE *stream , DS_TYPE_CODE_T type
                , unsigned int count , void *data);
char * 
temp_tablename()
{
  static int i=0;
  static char c[10],*n=c;
  sprintf(n,"t_tmp%d",i);
  i++;
  return n;
}

STAFCV_T 
tdm_allocstats()
{
#ifndef	OLD_DSL
   dsAllocStats();
#else	/*OLD_DSL*/
   dsDatasetAllocStats();
#endif	/*OLD_DSL*/
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdm_count()
{
   printf("TDM:\tObject count = %ld \n",tdm->count());
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdm_list()
{
   char *herb980615;
   char *c=NULL;
   c = tdm->list();
   herb980615=strtok(c,"\n");
   while(herb980615) {
     printf("%s\n",herb980615);
     herb980615=strtok(NULL,"\n");
   }
   FREE(c);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdm_newdataset(char* name)
{
  long dim = 0; //HACK - REMOVE THIS JUNK!!! put back by akio
  if( !tdm->newDataset(name,dim) ){
    EML_FAILURE(METHOD_FAILURE);
  }
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdm_newtable(char* name, char* spec, long rowcount)
{
   if( sutMatchPrefix("struct",spec) ){
      if( !tdm->newTable(name,spec,rowcount) ){
	 EML_FAILURE(METHOD_FAILURE);
      }
      EML_SUCCESS(STAFCV_OK);
   }
   else {
      char *specs=NULL;
      if( !tdm->findTypeSpecification(spec,specs)
      ||  !tdm->newTable(name,specs,rowcount) ){
	 FREE(specs);
	 EML_FAILURE(METHOD_FAILURE);
      }
      FREE(specs);
      EML_SUCCESS(STAFCV_OK);
   }
}

STAFCV_T 
tdmtypespecifiers_list(long tid)
{
   char *name=NULL;

   if( tid < 0 ){
      for(int i=1;;i++){
         if( !tdm->getTypeName(i,name) ){
            break;
         }
         printf("TDM:\tType name = (%s) \n",name);
         FREE(name);
      }
   }
   else {
      if( !tdm->getTypeName(tid,name) ){
         EML_FAILURE(METHOD_FAILURE);
      }
      printf("TDM:\tType name = (%s) \n",name);
      FREE(name);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtypespecifiers_show(char* name)
{
   char *tspec=NULL;
   char *tname=NULL;

   for(int i=1;;i++){
      if( !tdm->getTypeSpecification(i,tspec)
      ||  !tdm->getTypeName(i,tname)
      ){
        printf("TDM:\tType spec = %s\n", tspec); /*shall we comment out this??*/
        if(tspec) FREE(tspec); /*fix memory leak -akio*/
        if(tname) FREE(tname); /*fix memory leak -akio*/
        break;
      }
      if( sutMatchWild(name,tname) ){
	 printf("TDM:\tType spec = ...\n%s\n.\n",tspec);
         /* FREE(tspec); fix memory leak -akio*/
      }
      FREE(tspec); /*fix memory leak -akio*/
      FREE(tname); /*fix memory leak -akio*/
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtypespecifiers_load(char * fname)
{
   printf("file = %s\n",fname); fflush(0);
   FILE *f = NULL;
   if( NULL == (f = fopen(fname,"rb")) ){
      fclose(f);
      EML_FAILURE(FILE_OPEN_FAILURE);
   }
   char * spec = (char*)MALLOC(16384); /* more space... -akio*/
   memset(spec,0,16384);
   char * s = spec;
   char * buffer = (char*)MALLOC(256);
   memset(buffer,0,256);
   size_t l;
   sprintf(spec,"empty"); /*fix read overflow(not null terminated) -akio*/
   while( fgets(buffer,255,f) ){
      printf("*"); fflush(0);
      l = strlen(buffer);
      if( (strlen(spec) + l) >= 16382 ){ /* more space... -akio*/
	 FREE(spec);
	 FREE(buffer);
	 fclose(f);
	 EML_FAILURE(SPEC_TOO_LONG);
      }
      strncpy(s,buffer,l); /*fix "not null terminated" -akio*/ 
      s[l]='\0'; /* hjw 19Feb98 */
      s += l;
   }
   unsigned char ccmt=0;
   unsigned char cccmt=0;
   for( int i=0; i<(int)strlen(spec); i++ ){
      if( '/' == spec[i] && '*' == spec[i+1] )ccmt = 1;
      if( '*' == spec[i] && '/' == spec[i+1] ){
	 spec[i] = spec[i+1] = ' '; ccmt = 0;
      }
      if( '/' == spec[i] && '/' == spec[i+1] )cccmt = 1;
      if( '\n' == spec[i] )cccmt = 0;
      if( ccmt || cccmt ) spec[i] = ' ';
   }
   printf("\n---\n%s\n---\n",spec); fflush(0);

//   if( !tdm->newTable("tdm_temporary",spec,1) HACK ???????????
//   ||  !tdm->deleteTable("tdm_temporary")     HACK ???????????
   DS_DATASET_T *ttmp=NULL;
   char *ptmp;
   char *ntmp;
   ptmp=NULL;
   if( !dsNewTable(&ttmp,ntmp=temp_tablename(),spec,1,ptmp)
   ){
     FREE(spec); 
     FREE(buffer);
     EML_FAILURE(CANT_CREATE_TEMP_TABLE);
   }

   FREE(ttmp); /*fix memory leak -akio*/
   FREE(spec); 
   FREE(buffer);
   fclose(f);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmdataset_adddataset(char* dsname,char* name)
{
   tdmDataset *dataset;
   long dim = 0; //HACK - REMOVE THIS JUNK!!!  put back by akio
   if( NULL == (dataset = tdm->findDataset(dsname))
   ||  !dataset->addDataset(name,dim)
   ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmdataset_addtable(char* dsname, char* name, char* spec
	, long rowcount)
{
   tdmDataset* dataset;
   if( NULL == (dataset = tdm->findDataset(dsname))
   ||  !dataset->addTable(name,spec,rowcount)
   ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmdataset_entrycount(char* name)
{
   tdmDataset* dataset;		/* tdmDataset object */

   long result=0;

   if( NULL == (dataset = tdm->findDataset(name)) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   printf("TDMDATASET:\tEntry Count = %ld \n"
		,result=dataset->entryCount());
   set_staf_result((float)result);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmdataset_entryname()
{
EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
tdmdataset_findentry()
{
EML_FAILURE(NOT_YET_IMPLEMENTED);
}


#ifdef OLD_DSL
STAFCV_T 
tdmdataset_maxentrycount(char* name)
{
   tdmDataset* dataset;		/* tdmDataset object */
   long result=0;

   if( NULL == (dataset = tdm->findDataset(name)) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   printf("TDMDATASET:\tMax Entry Count = %d \n"
		,result=dataset->maxEntryCount());
   set_staf_result((float)result);
   EML_SUCCESS(STAFCV_OK);
}
#endif /*OLD_DSL*/

STAFCV_T 
tdmdataset_name(char* name)
{
   tdmDataset* dataset;

   if( NULL == (dataset = tdm->findDataset(name)) ){
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   char *c=NULL;
   printf("TDMDATASET:\tDSL name = (%s) \n",c = dataset->dslName());
   FREE(c);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmdataset_show()
{
EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
tdmtable_cell_getvalue(char* cellSpec,char *screenSwitch)
{
   tdmTable* table;		/* tdmTable object */

   /*- HACK - preliminary AHS calls do not work!!! -*/
   char *cs = (char*)MALLOC(strlen(cellSpec) +1);
   strcpy(cs,cellSpec);
   char* css = cs; /*fix memory leak -akio*/
   char *tname = strtok(css,"[]."); /*fix memory leak -akio*/
   char *c = strtok(NULL,"[].");
   int nrow = atoi(c);
   char *cname = strtok(NULL,"[].");

/**# DEBUG - does not work!!!
   AHS_STRUCT_T a;
   if( !isValidAhsSpec(cellSpec)
   ||  !ahs_zeroAHS(a)
   ||  !ahs_parseSpec(cellSpec,&a)		// HACK - MEM LEAK **
   ){
      EML_MESSAGE(INVALID_CELL_SPECIFICATION);
   }
   AHSNODE_T *t=&(a.nodes[a.pathDepth-1]);
   int nrow = t->shape.indexes[0];
   char *tname=t->name;
   char *cname=a.entryName;
#**/
   
   if( NULL == (table = tdm->findTable((tname))) ){
      FREE(cs); /*fix memory leak -akio*/
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",tname);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }

   TDM_COLUMN_T col;
   if( !table->findColumn(col,cname) ){
      FREE(cs); /*fix memory leak -akio*/
      EML_CONTEXT("ERROR: Are you sure you have a column named '%s'?\n",cname);
      EML_FAILURE(METHOD_FAILURE);
   }

   int ncol = col.nCol;
	
   TDM_CELLDATA_T cellData;
   if( !table->getCell(cellData,nrow,ncol) ){
      FREE(cs); /*fix memory leak -akio*/
      EML_FAILURE(METHOD_FAILURE);
   }
   if(screenSwitch[1]=='N'||screenSwitch[1]=='n') {
      printf("TDMTABLE:\tCell data = ");
      dsuPrintData(stdout,cellData._d,col.elcount,cellData.data.v);
      printf("\n");
   }
   float result;
   switch( cellData._d ){
      case DS_TYPE_CHAR:
         result = *(cellData.data.c);
	 break;
      case DS_TYPE_OCTET:
         result = *(cellData.data.o);
	 break;
      case DS_TYPE_SHORT:
         result = *(cellData.data.s);
	 break;
      case DS_TYPE_U_SHORT:
         result = *(cellData.data.us);
	 break;
      case DS_TYPE_LONG:
         result = *(cellData.data.l);
	 break;
      case DS_TYPE_U_LONG:
         result = *(cellData.data.ul);
	 break;
      case DS_TYPE_FLOAT:
         result = *(cellData.data.f);
	 break;
      case DS_TYPE_DOUBLE:
         result = *(cellData.data.d);
	 break;
      case DS_TYPE_STRUCT:
	 result = 11301957; /*-TDM_E_UNIMPLEMENTED_TYPE-*/
	 set_staf_result(result);
         FREE(col.name); /*fix memory leak -akio*/
         FREE(col.type); /*fix memory leak -akio*/
         FREE(cs); /*fix memory leak -akio*/
	 EML_FAILURE(NOT_YET_IMPLEMENTED);
	 break;
      default:
	 result = -11301957; /*-TDM_E_UNKNOWN_TYPE-*/
	 set_staf_result(result);
	 printf(" code = %d \n",cellData._d);
	 printf(" codes = %d %d %d %d %d %d %d %d %d\n"
	 ,DS_TYPE_CHAR,DS_TYPE_OCTET,DS_TYPE_SHORT,DS_TYPE_U_SHORT
	 ,DS_TYPE_LONG,DS_TYPE_U_LONG,DS_TYPE_FLOAT,DS_TYPE_DOUBLE
	 ,DS_TYPE_STRUCT);
         FREE(col.name); /*fix memory leak -akio*/
         FREE(col.type); /*fix memory leak -akio*/
         FREE(cs); /*fix memory leak -akio*/
	 EML_FAILURE(INVALID_TYPE);
	 break;
   }
   set_staf_result(result);

   FREE(col.name); /*fix memory leak -akio/phenix*/
   FREE(col.type); /*fix memory leak -akio/phenix*/
   FREE(cs); /*fix memory leak -akio/phenix*/
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtable_cell_putvalue(char* cellSpec, long nv, char **values)
{
   tdmTable* table;		/* tdmTable object */

   int iv,lv=0;

   /*- HACK - preliminary AHS calls do not work!!! -*/
   char *cs = (char*)MALLOC(strlen(cellSpec) +1);
   strcpy(cs,cellSpec);
   char* css = cs; /*fix memory leak -akio*/
   char *tname = strtok(css,"[]."); /*fix memory leak -akio*/
   char *c = strtok(NULL,"[].");
   int nrow = atoi(c);
   char *cname = strtok(NULL,"[].");
   
   if( NULL == (table = tdm->findTable((tname))) ){
      FREE(cs); /*fix memory leak -akio*/
      EML_FAILURE(OBJECT_NOT_FOUND);
   }

   TDM_COLUMN_T col;
   if( !table->findColumn(col,cname) ){
      FREE(cs); /*fix memory leak -akio*/
      EML_CONTEXT("ERROR: Are you sure you have a column named '%s'?\n",cname);
      EML_FAILURE(METHOD_FAILURE);
   }

   int ncol = col.nCol;
   DS_TYPE_CODE_T tcode = col.code;
   long elsize = col.size/col.elcount;	/* element size (bytes) */
	
   TDM_CELLDATA_T cellData;
   cellData._d = tcode;
   cellData.data.v = MALLOC(col.size);
   TDM_CELLDATA_T buff;
   buff.data.v = cellData.data.v;

   char *value=NULL;
   for( int np=0;np<nv;np++ ){
      switch( tcode ){
	 case DS_TYPE_CHAR:
	    lv = 0;
	    for( iv=0;iv<nv;iv++ ){ lv += strlen(values[iv]); }
	    value = (char*)MALLOC(lv +1);
	    strncpy(value,values[0],lv);
	    value[lv]=0;
	    for( iv=1;iv<nv;iv++ ){ strcat(value,values[iv]); }
	    strncpy(cellData.data.c,value,col.size); 
	    cellData.data.c[col.size-1]=0; /* hjw 19Feb98 */
	    FREE(value);
	    break;
	 case DS_TYPE_OCTET:
	    *(buff.data.o) = atol(values[np]);
	    break;
	 case DS_TYPE_SHORT:
	    *(buff.data.s) = atol(values[np]);
	    break;
	 case DS_TYPE_U_SHORT:
	    *(buff.data.us) = atol(values[np]);
	    break;
	 case DS_TYPE_LONG:
	    *(buff.data.l) = atol(values[np]);
	    break;
	 case DS_TYPE_U_LONG:
	    *(buff.data.ul) = atol(values[np]);
	    break;
	 case DS_TYPE_FLOAT:
	    *(buff.data.f) = atof(values[np]);
	    break;
	 case DS_TYPE_DOUBLE:
	    *(buff.data.d) = atof(values[np]);
	    break;
	 case DS_TYPE_STRUCT:
            FREE(col.name); /*fix memory leak -akio*/
            FREE(col.type); /*fix memory leak -akio*/
	    FREE(cellData.data.v);
            FREE(cs);       /*fix memory leak -akio*/
	    EML_FAILURE(NOT_YET_IMPLEMENTED);
	    break;
	 default:
            FREE(col.name); /*fix memory leak -akio*/
            FREE(col.type); /*fix memory leak -akio*/
	    FREE(cellData.data.v);
	    FREE(cs);       /*fix memory leak -akio*/
	    EML_FAILURE(INVALID_TYPE);
	    break;
      }
      buff.data.c += elsize;
   }

   if( !table->putCell(cellData,nrow,ncol) ){
      FREE(col.name); /*fix memory leak -akio/phenix*/
      FREE(col.type); /*fix memory leak -akio/phenix*/
      FREE(cellData.data.v);
      FREE(cs);       /*fix memory leak -akio/phenix*/
      EML_FAILURE(METHOD_FAILURE);
   }

   FREE(col.name); /*fix memory leak -akio/phenix*/
   FREE(col.type); /*fix memory leak -akio/phenix*/
   FREE(cellData.data.v);
   FREE(cs);       /*fix memory leak -akio/phenix*/
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtable_columncount(char* name)
{
   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tColumn Count = %ld \n"
		,result=table->columnCount());
   set_staf_result((float)result);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtable_column_code()
{
EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
tdmtable_column_elcount()
{
EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
tdmtable_column_find()
{
EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
tdmtable_column_name()
{
EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
tdmtable_column_rank()
{
EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
tdmtable_column_shape()
{
EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
tdmtable_column_size()
{
EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
tdmtable_column_type()
{
EML_FAILURE(NOT_YET_IMPLEMENTED);
}

STAFCV_T 
tdmtable_maxrowcount(char* name,long maxrowcount)
{
   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   if( maxrowcount >= 0 ){				/* SET */
      table->maxRowCount(maxrowcount);
   }
   else {						/* GET */
      printf("TDMTABLE:\tMax Row Count = %ld \n"
      		,result=table->maxRowCount());
      set_staf_result((float)result);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtable_name(char* name)
{
   tdmTable* table;		/* tdmTable object */

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   char *c=NULL;
   printf("TDMTABLE:\tDSL Name = (%s) \n",c = table->dslName());
   FREE(c);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtable_dump(char* name, long nrows, long ifirst,char *outfile,char *colList)
{

   tdmTable* table;		
   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   table->dumpRows(ifirst,nrows,outfile,colList);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtable_print(char* name, long nrows, long ifirst)
{
   tdmTable* table;		/* tdmTable object */

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   table->printRows(ifirst,nrows);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtable_rowcount(char* name, long rowcount)
{
   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   if( rowcount >= 0 ){					/* SET */
      table->rowCount(rowcount);
   }
   else {						/* GET */
      printf("TDMTABLE:\tRow Count = %ld \n",result=table->rowCount());
      set_staf_result((float)result);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtable_rowsize(char* name)
{
   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tRow Size = %ld bytes \n"
		,result=table->rowSize());
   set_staf_result((float)result);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtable_show(char* name)
{
   tdmTable* table;		/* tdmTable object */

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tTable = ...\n"); table->show(); printf("\n.\n");
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtable_specifier(char* name)
{
   tdmTable* table;		/* tdmTable object */
   char *c=NULL;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tType Specifier = ...\n%s\n.\n"
		,c=table->typeSpecifier());
   FREE(c);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tdmtable_typename(char* name)
{
   tdmTable* table=NULL;		/* tdmTable object */
   char *c=NULL;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tType Name = (%s) \n",c=table->typeName());
   FREE(c);
   EML_SUCCESS(STAFCV_OK);
}


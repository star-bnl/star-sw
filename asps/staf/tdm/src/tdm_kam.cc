/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         tdm_kam.C
*:DESCRIPTION:  C++ KUIP Action Modules for TDM
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      23dec96-v020a-cet- NEW_DSL -> OLD_DSL option
*:HISTORY:      09jul96-v011a-cet- modify putvalue for vectors
*:HISTORY:      29nov95-v010a-cet- recreation
*:HISTORY:      19jul95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#include "dstype.h"

#define KUIP
#include "kuip.h"

#include "asuAlloc.h"
#include "emlLib.h"
#include "tdmLib.h"
#include "sutLib.h"
#include "ahsLib.h"

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
extern "C" void dsuPrintData(FILE *stream , DS_TYPE_CODE_T type
		, unsigned int count , void *data);

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdm_allocstats_
*:DESCRIPTION:  KUIP Action Module to show DSL allocation stats
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/ALLOCSTATS
*:<---------------------------------------------------------------------
*/
void kam_tdm_allocstats_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

        STAFCV_T status = tdm_allocstats();
}
STAFCV_T tdm_allocstats()
{
#ifndef	OLD_DSL
   dsAllocStats();
#else	/*OLD_DSL*/
   dsDatasetAllocStats();
#endif	/*OLD_DSL*/
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdm_count_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/FACTORY/COUNT
*:<---------------------------------------------------------------------
*/
void kam_tdm_count_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

        STAFCV_T status = tdm_count();
}
STAFCV_T tdm_count()
{
   printf("TDM:\tObject count = %d \n",tdm->count());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdm_list_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/FACTORY/LIST
*:<---------------------------------------------------------------------
*/
void kam_tdm_list_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

        STAFCV_T status = tdm_list();
}
STAFCV_T tdm_list()
{
   char *c=NULL;
   printf("%s",c = tdm->list() );
   FREE(c);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdm_newdataset_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/FACTORY/NEWDATASET NAME SETDIM
*:<---------------------------------------------------------------------
*/
void kam_tdm_newdataset_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* dataset name */
   long dim = ku_geti();	/* dataset dimension */

        STAFCV_T status = tdm_newdataset(name,dim);
}
STAFCV_T tdm_newdataset(char* name, long dim)
{
   if( !tdm->newDataset(name,dim) ){
      EML_FAILURE(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdm_newtable_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/FACTORY/NEWTABLE NAME SPEC ROWCOUNT
*:<---------------------------------------------------------------------
*/
void kam_tdm_newtable_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */
   char* spec = ku_gets();	/* table row specifier */
   long rowcount = ku_geti();	/* rows to allocate */

        STAFCV_T status = tdm_newtable(name,spec,rowcount);
}
STAFCV_T tdm_newtable(char* name, char* spec, long rowcount)
{
   if( sutMatchPrefix("struct",spec) ){
      if( !tdm->newTable(name,spec,rowcount) ){
	 EML_FAILURE(KAM_METHOD_FAILURE);
      }
      EML_SUCCESS(STAFCV_OK);
   }
   else {
      char *specs=NULL;
      if( !tdm->findTypeSpecification(spec,specs)
      ||  !tdm->newTable(name,specs,rowcount) ){
	 FREE(specs);
	 EML_FAILURE(KAM_METHOD_FAILURE);
      }
      FREE(specs);
      EML_SUCCESS(STAFCV_OK);
   }
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdm_type_list_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TYPESPECIFIERS/LIST [ TID ]
*:<---------------------------------------------------------------------
*/
void kam_tdm_type_list_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   long tid = ku_geti();	/* table type id */

        STAFCV_T status = tdm_type_list(tid);
}
STAFCV_T tdm_type_list(long tid)
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
         EML_FAILURE(KAM_METHOD_FAILURE);
      }
      printf("TDM:\tType name = (%s) \n",name);
      FREE(name);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdm_type_show_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/SPECIFICATION [ NAME ]
*:<---------------------------------------------------------------------
*/
void kam_tdm_type_show_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char *name = ku_gets();	/* table name */

        STAFCV_T status = tdm_type_show(name);
}
STAFCV_T tdm_type_show(char* name)
{
   char *tspec=NULL;
   char *tname=NULL;

   for(int i=1;;i++){
      if( !tdm->getTypeSpecification(i,tspec)
      ||  !tdm->getTypeName(i,tname)
      ){
	 break;
      }
      if( sutMatchWild(name,tname) ){
	 printf("TDM:\tType spec = ...\n%s\n.\n",tspec);
	 FREE(tspec);
      }
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdm_type_load_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TYPESPECIFIERS/LIST [ TID ]
*:<---------------------------------------------------------------------
*/
void kam_tdm_type_load_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* fname = ku_gets();	/* idl file name */

        STAFCV_T status = tdm_type_load(fname);
}
STAFCV_T tdm_type_load(char * fname)
{
   printf("file = %s\n",fname); fflush(0);
   FILE *f = NULL;
   if( NULL == (f = fopen(fname,"rb")) ){
      fclose(f);
      EML_FAILURE(KAM_FILE_OPEN_FAILURE);
   }
   char * spec = (char*)MALLOC(2048);
   char * s = spec;
   char * buffer = (char*)MALLOC(256);
   size_t l;
   while( fgets(buffer,255,f) ){
      printf("*"); fflush(0);
      l = strlen(buffer);
      if( (strlen(spec) + l) >= 2047 ){
	 FREE(spec); FREE(buffer);
	 fclose(f);
	 EML_FAILURE(KAM_SPEC_TOO_LONG);
      }
      strncpy(s,buffer,l);
      s += l;
   }
   unsigned char ccmt=0;
   unsigned char cccmt=0;
   for( int i=0; i<strlen(spec); i++ ){
      if( '/' == spec[i] && '*' == spec[i+1] )ccmt = 1;
      if( '*' == spec[i] && '/' == spec[i+1] ){
	 spec[i] = spec[i+1] = ' '; ccmt = 0;
      }
      if( '/' == spec[i] && '/' == spec[i+1] )cccmt = 1;
      if( '\n' == spec[i] )cccmt = 0;
      if( ccmt || cccmt ) spec[i] = ' ';
   }
   printf("\n---\n%s\n---\n",spec); fflush(0);

   if( !tdm->newTable("tdm_temporary",spec,1)
   ||  !tdm->deleteTable("tdm_temporary")
   ){
      EML_FAILURE(KAM_METHOD_FAILURE);
   }

   FREE(spec); FREE(buffer);
   fclose(f);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmdataset_adddataset_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/DATASET/ADDDATASET TDMDATASET NAME [ DIM ]
*:<---------------------------------------------------------------------
*/
void kam_tdmdataset_adddataset_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* dsname = ku_gets();	/* dataset name */
   char* name = ku_gets();	/* new dataset name */
   long dim = ku_geti();	/* new dataset dimension */

        STAFCV_T status = tdmdataset_adddataset(dsname,name,dim);
}
STAFCV_T tdmdataset_adddataset(char* dsname,char* name,long dim)
{
   tdmDataset *dataset;
   if( NULL == (dataset = tdm->findDataset(dsname))
   ||  !dataset->addDataset(name,dim)
   ){
      EML_FAILURE(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmdataset_addtable_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/DATASET/ADDTABLE TDMDATASET NAME SPEC ROWCOUNT
*:<---------------------------------------------------------------------
*/
void kam_tdmdataset_addtable_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* dsname = ku_gets();	/* dataset name */
   char* name = ku_gets();	/* table name */
   char* spec = ku_gets();	/* table row specifier */
   long rowcount = ku_geti();	/* rows to allocate */

        STAFCV_T status = tdmdataset_addtable(dsname, name, spec
		, rowcount);
}
STAFCV_T tdmdataset_addtable(char* dsname, char* name, char* spec
	, long rowcount)
{
   tdmDataset* dataset;
   if( NULL == (dataset = tdm->findDataset(dsname))
   ||  !dataset->addTable(name,spec,rowcount)
   ){
      EML_FAILURE(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmdataset_entrycount_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/DATASET/ENTRYCOUNT NAME
*:<---------------------------------------------------------------------
*/
void kam_tdmdataset_entrycount_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* dataset name */

        STAFCV_T status = tdmdataset_entrycount(name);
}
STAFCV_T tdmdataset_entrycount(char* name)
{
   tdmDataset* dataset;		/* tdmDataset object */

   long result=0;

   if( NULL == (dataset = tdm->findDataset(name)) ){
      EML_FAILURE(KAM_METHOD_FAILURE);
   }
   printf("TDMDATASET:\tEntry Count = %d \n"
		,result=dataset->entryCount());
   set_staf_result((float)result);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmdataset_entryname_()
{
        STAFCV_T status = tdmdataset_entryname();
}
STAFCV_T tdmdataset_entryname()
{
EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmdataset_findentry_()
{
        STAFCV_T status = tdmdataset_findentry();
}
STAFCV_T tdmdataset_findentry()
{
EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmdataset_maxentrycount_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/DATASET/MAXENTRYCOUNT NAME
*:<---------------------------------------------------------------------
*/
#ifdef OLD_DSL
void kam_tdmdataset_maxentrycount_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* dataset name */

        STAFCV_T status = tdmdataset_maxentrycount(name);
}
STAFCV_T tdmdataset_maxentrycount(char* name)
{
   tdmDataset* dataset;		/* tdmDataset object */
   long result=0;

   if( NULL == (dataset = tdm->findDataset(name)) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }
   printf("TDMDATASET:\tMax Entry Count = %d \n"
		,result=dataset->maxEntryCount());
   set_staf_result((float)result);
   EML_SUCCESS(STAFCV_OK);
}
#endif /*OLD_DSL*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmdataset_name_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/DATASET/NAME NAME
*:<---------------------------------------------------------------------
*/
void kam_tdmdataset_name_()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* dataset name */

        STAFCV_T status = tdmdataset_name(name);
}
STAFCV_T tdmdataset_name(char* name)
{
   tdmDataset* dataset;

   if( NULL == (dataset = tdm->findDataset(name)) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }
   char *c=NULL;
   printf("TDMDATASET:\tDSL name = (%s) \n",c = dataset->dslName());
   FREE(c);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmdataset_show_()
{
        STAFCV_T status = tdmdataset_show();
}
STAFCV_T tdmdataset_show()
{
EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_cell_getvalue_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* cellSpec = ku_gets();	/* table.cell specification */

        STAFCV_T status = tdmtable_cell_getvalue(cellSpec);
}
STAFCV_T tdmtable_cell_getvalue(char* cellSpec)
{
   tdmTable* table;		/* tdmTable object */

   /*- HACK - preliminary AHS calls do not work!!! -*/
   char *cs = (char*)MALLOC(strlen(cellSpec) +1);
   strcpy(cs,cellSpec);
   char *tname = strtok(cs,"[].");
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
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }

   TDM_COLUMN_T col;
   if( !table->findColumn(col,cname) ){
      EML_FAILURE(KAM_METHOD_FAILURE);
   }

   int ncol = col.nCol;
	
   TDM_CELLDATA_T cellData;
   if( !table->getCell(cellData,nrow,ncol) ){
      EML_FAILURE(KAM_METHOD_FAILURE);
   }

   printf("TDMTABLE:\tCell data = ");
   dsuPrintData(stdout,cellData._d,col.elcount,cellData.data.v);
   printf("\n");

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
	 EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
	 break;
      default:
	 result = -11301957; /*-TDM_E_UNKNOWN_TYPE-*/
	 set_staf_result(result);
	 printf(" code = %d \n",cellData._d);
	 printf(" codes = %d %d %d %d %d %d %d %d %d\n"
	 ,DS_TYPE_CHAR,DS_TYPE_OCTET,DS_TYPE_SHORT,DS_TYPE_U_SHORT
	 ,DS_TYPE_LONG,DS_TYPE_U_LONG,DS_TYPE_FLOAT,DS_TYPE_DOUBLE
	 ,DS_TYPE_STRUCT);
	 EML_FAILURE(KAM_INVALID_TYPE);
	 break;
   }
   set_staf_result(result);

   FREE(cs);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_cell_putvalue_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* cellSpec = ku_gets();	/* table.cell name specification */
   char** values;		/* array of value strings */
/** THIS HAS TO BE REWRITTEN **/
/** 03dec96 - THIS HAS BEEN REWRITTEN? **/

   values = new char*[npars-1];
   for( int np=0;np<npars-1;np++ ){
      values[np] = ku_gets();
   }
   STAFCV_T status = tdmtable_cell_putvalue(cellSpec,npars-1,values);
}
STAFCV_T tdmtable_cell_putvalue(char* cellSpec, long nv, char **values)
{
   tdmTable* table;		/* tdmTable object */

   int iv,lv=0;

   /*- HACK - preliminary AHS calls do not work!!! -*/
   char *cs = (char*)MALLOC(strlen(cellSpec) +1);
   strcpy(cs,cellSpec);
   char *tname = strtok(cs,"[].");
   char *c = strtok(NULL,"[].");
   int nrow = atoi(c);
   char *cname = strtok(NULL,"[].");
   
   if( NULL == (table = tdm->findTable((tname))) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }

   TDM_COLUMN_T col;
   if( !table->findColumn(col,cname) ){
      EML_FAILURE(KAM_METHOD_FAILURE);
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
	    strncpy(value,values[0],strlen(value));
	    for( iv=1;iv<nv;iv++ ){ strcat(value,values[iv]); }
	    value[strlen(value)] = 0;
	    strncpy(cellData.data.c,value,col.size);
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
	    FREE(cellData.data.v);
	    FREE(cs);
	    EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
	    break;
	 default:
	    FREE(cellData.data.v);
	    FREE(cs);
	    EML_FAILURE(KAM_INVALID_TYPE);
	    break;
      }
      buff.data.c += elsize;
   }

   if( !table->putCell(cellData,nrow,ncol) ){
      FREE(cellData.data.v);
      FREE(cs);
      EML_FAILURE(KAM_METHOD_FAILURE);
   }

   FREE(cellData.data.v);
   FREE(cs);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtable_colcount_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TABLE/COLCOUNT NAME
*:<---------------------------------------------------------------------
*/
void kam_tdmtable_colcount_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */

        STAFCV_T status = tdmtable_colcount(name);
}
STAFCV_T tdmtable_colcount(char* name)
{
   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tColumn Count = %d \n"
		,result=table->columnCount());
   set_staf_result((float)result);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_code_()
{
        STAFCV_T status = tdmtable_column_code();
}
STAFCV_T tdmtable_column_code()
{
EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_elcount_()
{
        STAFCV_T status = tdmtable_column_elcount();
}
STAFCV_T tdmtable_column_elcount()
{
EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_find_()
{
        STAFCV_T status = tdmtable_column_find();
}
STAFCV_T tdmtable_column_find()
{
EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_name_()
{
        STAFCV_T status = tdmtable_column_name();
}
STAFCV_T tdmtable_column_name()
{
EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_rank_()
{
        STAFCV_T status = tdmtable_column_rank();
}
STAFCV_T tdmtable_column_rank()
{
EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_shape_()
{
        STAFCV_T status = tdmtable_column_shape();
}
STAFCV_T tdmtable_column_shape()
{
EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_size_()
{
        STAFCV_T status = tdmtable_column_size();
}
STAFCV_T tdmtable_column_size()
{
EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_type_()
{
        STAFCV_T status = tdmtable_column_type();
}
STAFCV_T tdmtable_column_type()
{
EML_FAILURE(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtable_maxrowcount_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TABLE/MAXROWCOUNT NAME [ MAXROWCOUNT ]
*:<---------------------------------------------------------------------
*/
void kam_tdmtable_maxrowcount_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */
   long maxrowcount = ku_geti();/* number of rows allocated */

        STAFCV_T status = tdmtable_maxrowcount(name,maxrowcount);
}
STAFCV_T tdmtable_maxrowcount(char* name,long maxrowcount)
{
   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }
   if( maxrowcount >= 0 ){				/* SET */
      table->maxRowCount(maxrowcount);
   }
   else {						/* GET */
      printf("TDMTABLE:\tMax Row Count = %d \n"
      		,result=table->maxRowCount());
      set_staf_result((float)result);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtable_name_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TABLE/NAME NAME
*:<---------------------------------------------------------------------
*/
void kam_tdmtable_name_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */

        STAFCV_T status = tdmtable_name(name);
}
STAFCV_T tdmtable_name(char* name)
{
   tdmTable* table;		/* tdmTable object */

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }
   char *c=NULL;
   printf("TDMTABLE:\tDSL Name = (%s) \n",c = table->dslName());
   FREE(c);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtable_print_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TABLE/PRINT TNAME [ NROWS IFIRST ]
*:<---------------------------------------------------------------------
*/
void kam_tdmtable_print_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */
   long nrows = ku_geti();	/* number of rows to print */
   long ifirst = ku_geti();	/* first row number to print */

        STAFCV_T status = tdmtable_print(name, nrows, ifirst);
}
STAFCV_T tdmtable_print(char* name, long nrows, long ifirst)
{
   tdmTable* table;		/* tdmTable object */

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }
   table->printRows(ifirst,nrows);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtable_rowcount_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TABLE/ROWCOUNT NAME [ ROWCOUNT ]
*:<---------------------------------------------------------------------
*/
void kam_tdmtable_rowcount_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */
   long rowcount = ku_geti();	/* number of rows filled */

        STAFCV_T status = tdmtable_rowcount(name, rowcount);
}
STAFCV_T tdmtable_rowcount(char* name, long rowcount)
{
   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }
   if( rowcount >= 0 ){					/* SET */
      table->rowCount(rowcount);
   }
   else {						/* GET */
      printf("TDMTABLE:\tRow Count = %d \n",result=table->rowCount());
      set_staf_result((float)result);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtable_rowsize_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TABLE/ROWSIZE NAME
*:<---------------------------------------------------------------------
*/
void kam_tdmtable_rowsize_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */

        STAFCV_T status = tdmtable_rowsize(name);
}
STAFCV_T tdmtable_rowsize(char* name)
{
   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tRow Size = %d bytes \n"
		,result=table->rowSize());
   set_staf_result((float)result);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtable_show_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TABLE/SHOW TNAME [ OPTION ]
*:<---------------------------------------------------------------------
*/
void kam_tdmtable_show_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */
   char* option = ku_gets();	/* options */

        STAFCV_T status = tdmtable_show(name, option);
}
STAFCV_T tdmtable_show(char* name, char* option)
{
   tdmTable* table;		/* tdmTable object */

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tTable = ...\n"); table->show(); printf("\n.\n");
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtable_specifier_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TABLE/SPECIFIER NAME
*:<---------------------------------------------------------------------
*/
void kam_tdmtable_specifier_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */

        STAFCV_T status = tdmtable_specifier(name);
}
STAFCV_T tdmtable_specifier(char* name)
{
   tdmTable* table;		/* tdmTable object */
   char *c=NULL;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tType Specifier = ...\n%s\n.\n"
		,c=table->typeSpecifier());
   FREE(c);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtable_typename_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TABLE/TYPE NAME
*:<---------------------------------------------------------------------
*/
void kam_tdmtable_typename_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */

        STAFCV_T status = tdmtable_typename(name);
}
STAFCV_T tdmtable_typename(char* name)
{
   tdmTable* table=NULL;		/* tdmTable object */
   char *c=NULL;

   if( NULL == (table = tdm->findTable(name)) ){
      EML_FAILURE(KAM_OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tType Name = (%s) \n",c=table->typeName());
   FREE(c);
   EML_SUCCESS(STAFCV_OK);
}


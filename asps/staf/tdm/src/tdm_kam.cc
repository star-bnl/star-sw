/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         tdm_kam.C
*:DESCRIPTION:  C++ KUIP Action Modules for TDM
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
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
void kam_tdm_allocstats_(){kam_tdm_allocstats();}
int kam_tdm_allocstats()
{
   long npars = ku_npar();      /* number of KUIP parameters */

#ifdef	OLD_DSL
   dsDatasetAllocStats();
#else	/*OLD_DSL*/
   dsAllocStats();
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
void kam_tdm_count_(){kam_tdm_count();}
int kam_tdm_count()
{
   long npars = ku_npar();      /* number of KUIP parameters */

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
void kam_tdm_list_(){kam_tdm_list();}
int kam_tdm_list()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   printf("%s",tdm->list() );
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
void kam_tdm_newdataset_(){kam_tdm_newdataset();}
int kam_tdm_newdataset()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* dataset name */
   long dim = ku_geti();	/* dataset dimension */

   if( !tdm->newDataset(name,dim) ){
      EML_ERROR(KAM_METHOD_FAILURE);
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
void kam_tdm_newtable_(){kam_tdm_newtable();}
int kam_tdm_newtable()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */
   char* spec = ku_gets();	/* table row specifier */
   long rowcount = ku_geti();	/* rows to allocate */

   if( sutMatchPrefix("struct",spec) ){
      if( !tdm->newTable(name,spec,rowcount) ){
	 EML_ERROR(KAM_METHOD_FAILURE);
      }
      EML_SUCCESS(STAFCV_OK);
   }
   else {
      char *specs=NULL;
      if( !tdm->findTypeSpecification(spec,specs)
      ||  !tdm->newTable(name,specs,rowcount) ){
	 ASUFREE(specs);
	 EML_ERROR(KAM_METHOD_FAILURE);
      }
      ASUFREE(specs);
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
void kam_tdm_type_list_(){kam_tdm_type_list();}
int kam_tdm_type_list()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   long tid = ku_geti();	/* table type id */

   char *name=NULL;
   char n[64]={0};

   if( tid < 0 ){
      for(int i=1;;i++){
	 if( !tdm->getTypeName(i,name) ){
	    break;
	 }
	 strncpy(n,name,strlen(name)); strtok(n," ");
	 printf("TDM:\tType name = (%s) \n",n);	//ame);
      }
   }
   else {
      if( !tdm->getTypeName(tid,name) ){
	 EML_ERROR(KAM_METHOD_FAILURE);
      }
      strncpy(n,name,strlen(name)); strtok(n," ");
      printf("TDM:\tType name = (%s) \n",n);	//ame);
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
void kam_tdm_type_show_(){kam_tdm_type_show();}
int kam_tdm_type_show()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char *name = ku_gets();	/* table name */

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
      }
   }
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
void kam_tdmdataset_adddataset_(){kam_tdmdataset_adddataset();}
int kam_tdmdataset_adddataset()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* dsname = ku_gets();	/* dataset name */
   char* name = ku_gets();	/* new dataset name */
   long dim = ku_geti();	/* new dataset dimension */

   tdmDataset* dataset;

   if( !tdm->findDataset(dsname, dataset)
   ||  !dataset->addDataset(name,dim)
   ){
      EML_ERROR(KAM_METHOD_FAILURE);
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
void kam_tdmdataset_addtable_(){kam_tdmdataset_addtable();}
int kam_tdmdataset_addtable()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* dsname = ku_gets();	/* dataset name */
   char* name = ku_gets();	/* table name */
   char* spec = ku_gets();	/* table row specifier */
   long rowcount = ku_geti();	/* rows to allocate */

   tdmDataset* dataset;

   if( !tdm->findDataset(dsname, dataset)
   ||  !dataset->addTable(name,spec,rowcount)
   ){
      EML_ERROR(KAM_METHOD_FAILURE);
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
void kam_tdmdataset_entrycount_(){kam_tdmdataset_entrycount();}
int kam_tdmdataset_entrycount()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* dataset name */

   tdmDataset* dataset;		/* tdmDataset object */

   long result=0;

   if( !tdm->findDataset(name, dataset) ){
      EML_ERROR(KAM_METHOD_FAILURE);
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
void kam_tdmdataset_entryname_(){kam_tdmdataset_entryname();}
int kam_tdmdataset_entryname()
{
EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmdataset_findentry_(){kam_tdmdataset_findentry();}
int kam_tdmdataset_findentry()
{
EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
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
void kam_tdmdataset_maxentrycount_(){kam_tdmdataset_maxentrycount();}
int kam_tdmdataset_maxentrycount()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* dataset name */

   tdmDataset* dataset;		/* tdmDataset object */
   long result=0;

   if( !tdm->findDataset(name, dataset) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("TDMDATASET:\tMax Entry Count = %d \n"
		,result=dataset->maxEntryCount());
   set_staf_result((float)result);
   EML_SUCCESS(STAFCV_OK);
}


/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmdataset_name_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/DATASET/NAME NAME
*:<---------------------------------------------------------------------
*/
void kam_tdmdataset_name_(){kam_tdmdataset_name();}
int kam_tdmdataset_name()
{
   long npars = ku_npar();	/* number of KUIP parameters */
   char* name = ku_gets();	/* dataset name */

   tdmDataset* dataset;

   if( !tdm->findDataset(name, dataset) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("TDMDATASET:\tDSL name = (%s) \n",dataset->dslName());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmdataset_show_(){kam_tdmdataset_show();}
int kam_tdmdataset_show()
{
EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_cell_getvalue_(){kam_tdmtable_cell_getvalue();}
int kam_tdmtable_cell_getvalue()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* cellSpec = ku_gets();	/* table.cell specification */

   tdmTable* table;		/* tdmTable object */

   /*- HACK - preliminary AHS calls do not work!!! -*/
   char *cs = (char*)ASUALLOC(strlen(cellSpec) +1);
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
   
   if( !tdm->findTable((tname), table) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }

   TDM_COLUMN_T col;
   if( !table->findColumn(col,cname) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }

   int ncol = col.nCol;
	
   TDM_CELLDATA_T cellData;
   if( !table->getCell(cellData,nrow,ncol) ){
      EML_ERROR(KAM_METHOD_FAILURE);
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
	 EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
	 break;
      default:
	 result = -11301957; /*-TDM_E_UNKNOWN_TYPE-*/
	 set_staf_result(result);
	 printf(" code = %d \n",cellData._d);
	 printf(" codes = %d %d %d %d %d %d %d %d %d\n"
	 ,DS_TYPE_CHAR,DS_TYPE_OCTET,DS_TYPE_SHORT,DS_TYPE_U_SHORT
	 ,DS_TYPE_LONG,DS_TYPE_U_LONG,DS_TYPE_FLOAT,DS_TYPE_DOUBLE
	 ,DS_TYPE_STRUCT);
	 EML_ERROR(KAM_INVALID_TYPE);
	 break;
   }
   set_staf_result(result);

   ASUFREE(cs);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_cell_putvalue_(){kam_tdmtable_cell_putvalue();}
int kam_tdmtable_cell_putvalue()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* cellSpec = ku_gets();	/* table.cell name specification */
   char* value; // = ku_gets();	/* value string */

   tdmTable* table;		/* tdmTable object */

   /*- HACK - preliminary AHS calls do not work!!! -*/
   char *cs = (char*)ASUALLOC(strlen(cellSpec) +1);
   strcpy(cs,cellSpec);
   char *tname = strtok(cs,"[].");
   char *c = strtok(NULL,"[].");
   int nrow = atoi(c);
   char *cname = strtok(NULL,"[].");
   
   if( !tdm->findTable((tname), table) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }

   TDM_COLUMN_T col;
   if( !table->findColumn(col,cname) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }

   int ncol = col.nCol;
   DS_TYPE_CODE_T tcode = col.code;
   long elsize = col.size/col.elcount;	/* element size (bytes) */
	
   TDM_CELLDATA_T cellData;
   cellData._d = tcode;
   cellData.data.v = ASUALLOC(col.size);
   TDM_CELLDATA_T buff;
   buff.data.v = cellData.data.v;

   char *v;
   for( int np=1;np<npars;np++ ){
      value = ku_gets();		/* value string */
      switch( tcode ){
	 case DS_TYPE_CHAR:
	    v = (char*)ASUALLOC(strlen(value) +1);
	    strcpy(v,value);
	    buff.data.c = (char*)v;
	    break;
	 case DS_TYPE_OCTET:
	    *(buff.data.o) = atol(value);
	    break;
	 case DS_TYPE_SHORT:
	    *(buff.data.s) = atol(value);
	    break;
	 case DS_TYPE_U_SHORT:
	    *(buff.data.us) = atol(value);
	    break;
	 case DS_TYPE_LONG:
	    *(buff.data.l) = atol(value);
	    break;
	 case DS_TYPE_U_LONG:
	    *(buff.data.ul) = atol(value);
	    break;
	 case DS_TYPE_FLOAT:
	    *(buff.data.f) = atof(value);
	    break;
	 case DS_TYPE_DOUBLE:
	    *(buff.data.d) = atof(value);
	    break;
	 case DS_TYPE_STRUCT:
	    ASUFREE(cellData.data.v);
	    ASUFREE(cs);
	    EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
	    break;
	 default:
	    ASUFREE(cellData.data.v);
	    ASUFREE(cs);
	    EML_ERROR(KAM_INVALID_TYPE);
	    break;
      }
      buff.data.c += elsize;
   }

   if( !table->putCell(cellData,nrow,ncol) ){
      ASUFREE(cellData.data.v);
      ASUFREE(cs);
      EML_ERROR(KAM_METHOD_FAILURE);
   }

   ASUFREE(cellData.data.v);
   ASUFREE(cs);
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
void kam_tdmtable_colcount_(){kam_tdmtable_colcount();}
int kam_tdmtable_colcount()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */

   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( !tdm->findTable(name, table) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
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
void kam_tdmtable_column_code_(){kam_tdmtable_column_code();}
int kam_tdmtable_column_code()
{
EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_elcount_(){kam_tdmtable_column_elcount();}
int kam_tdmtable_column_elcount()
{
EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_find_(){kam_tdmtable_column_find();}
int kam_tdmtable_column_find()
{
EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_name_(){kam_tdmtable_column_name();}
int kam_tdmtable_column_name()
{
EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_rank_(){kam_tdmtable_column_rank();}
int kam_tdmtable_column_rank()
{
EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_shape_(){kam_tdmtable_column_shape();}
int kam_tdmtable_column_shape()
{
EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_size_(){kam_tdmtable_column_size();}
int kam_tdmtable_column_size()
{
EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void kam_tdmtable_column_type_(){kam_tdmtable_column_type();}
int kam_tdmtable_column_type()
{
EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
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
void kam_tdmtable_maxrowcount_(){kam_tdmtable_maxrowcount();}
int kam_tdmtable_maxrowcount()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */
   long maxrowcount = ku_geti();/* number of rows allocated */

   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( !tdm->findTable(name, table) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
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
void kam_tdmtable_name_(){kam_tdmtable_name();}
int kam_tdmtable_name()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */

   tdmTable* table;		/* tdmTable object */

   if( !tdm->findTable(name, table) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tDSL Name = (%s) \n",table->dslName());
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
void kam_tdmtable_print_(){kam_tdmtable_print();}
int kam_tdmtable_print()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */
   long nrows = ku_geti();	/* number of rows to print */
   long ifirst = ku_geti();	/* first row number to print */

   tdmTable* table;		/* tdmTable object */

   if( !tdm->findTable(name,table) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
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
void kam_tdmtable_rowcount_(){kam_tdmtable_rowcount();}
int kam_tdmtable_rowcount()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */
   long rowcount = ku_geti();	/* number of rows filled */

   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( !tdm->findTable(name, table) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
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
void kam_tdmtable_rowsize_(){kam_tdmtable_rowsize();}
int kam_tdmtable_rowsize()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */

   tdmTable* table;		/* tdmTable object */
   long result=0;

   if( !tdm->findTable(name, table) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
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
void kam_tdmtable_show_(){kam_tdmtable_show();}
int kam_tdmtable_show()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */
   char* option = ku_gets();	/* options */

   tdmTable* table;		/* tdmTable object */

   if( !tdm->findTable(name,table) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
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
void kam_tdmtable_specifier_(){kam_tdmtable_specifier();}
int kam_tdmtable_specifier()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */

   tdmTable* table;		/* tdmTable object */

   if( !tdm->findTable(name, table) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tType Specifier = ...\n%s\n.\n"
		,table->typeSpecifier());
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
void kam_tdmtable_typename_(){kam_tdmtable_typename();}
int kam_tdmtable_typename()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* table name */

   tdmTable* table=NULL;		/* tdmTable object */

   if( !tdm->findTable(name, table) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("TDMTABLE:\tType Name = (%s) \n",table->typeName());
   EML_SUCCESS(STAFCV_OK);
}


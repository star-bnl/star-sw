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

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdm_allocstats_
*:DESCRIPTION:  KUIP Action Module to show DSL allocation stats
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/ALLOCSTATS
*:<---------------------------------------------------------------------
*/
void 
kam_tdm_allocstats_()
{
   tdm_allocstats();
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
void 
kam_tdm_count_()
{
   tdm_count();
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
void
kam_tdm_list_()
{
   tdm_list();
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
void 
kam_tdm_newdataset_()
{
   char* name = ku_gets();	/* dataset name */

   tdm_newdataset(name);
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
void 
kam_tdm_newtable_()
{
   char* name = ku_gets();	/* table name */
   char* spec = ku_gets();	/* table row specifier */
   long rowcount = ku_geti();	/* rows to allocate */

   tdm_newtable(name,spec,rowcount);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtypespecifiers_list_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TYPESPECIFIERS/LIST [ TID ]
*:<---------------------------------------------------------------------
*/
void 
kam_tdmtypespecifiers_list_()
{
   long tid = ku_geti();	/* table type id */

   tdmtypespecifiers_list(tid);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtypespecifiers_show_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/SPECIFICATION [ NAME ]
*:<---------------------------------------------------------------------
*/
void 
kam_tdmtypespecifiers_show_()
{
   char *name = ku_gets();	/* table name */

   tdmtypespecifiers_show(name);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtypespecifiers_load_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TYPESPECIFIERS/LIST [ TID ]
*:<---------------------------------------------------------------------
*/
void 
kam_tdmtypespecifiers_load_()
{
   char* fname = ku_gets();	/* idl file name */

   tdmtypespecifiers_load(fname);
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
void 
kam_tdmdataset_adddataset_()
{
   char* dsname = ku_gets();	/* dataset name */
   char* name = ku_gets();	/* new dataset name */

   tdmdataset_adddataset(dsname,name);
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
void 
kam_tdmdataset_addtable_()
{
   char* dsname = ku_gets();	/* dataset name */
   char* name = ku_gets();	/* table name */
   char* spec = ku_gets();	/* table row specifier */
   long rowcount = ku_geti();	/* rows to allocate */

   tdmdataset_addtable(dsname, name, spec, rowcount);
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
void 
kam_tdmdataset_entrycount_()
{
   char* name = ku_gets();	/* dataset name */

   tdmdataset_entrycount(name);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmdataset_entryname_()
{
  tdmdataset_entryname();
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmdataset_findentry_()
{
  tdmdataset_findentry();
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
void 
kam_tdmdataset_maxentrycount_()
{
   char* name = ku_gets();	/* dataset name */

   tdmdataset_maxentrycount(name);
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
void 
kam_tdmdataset_name_()
{
   char* name = ku_gets();	/* dataset name */

   tdmdataset_name(name);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmdataset_show_()
{
  tdmdataset_show();
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmtable_cell_getvalue_()
{
   char* cellSpec = ku_gets();	/* table.cell specification */
   char* screenSwitch = ku_gets();	/* table.cell specification */

   tdmtable_cell_getvalue(cellSpec,screenSwitch);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmtable_cell_putvalue_()
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
   tdmtable_cell_putvalue(cellSpec,npars-1,values);
   delete[] values; /*fix memory leak -akio*/
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtable_columncount_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TABLE/COLCOUNT NAME
*:<---------------------------------------------------------------------
*/
void 
kam_tdmtable_columncount_()
{
   char* name = ku_gets();	/* table name */

   tdmtable_columncount(name);
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmtable_column_code_()
{
  tdmtable_column_code();
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmtable_column_elcount_()
{
  tdmtable_column_elcount();
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmtable_column_find_()
{
  tdmtable_column_find();
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmtable_column_name_()
{
  tdmtable_column_name();
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmtable_column_rank_()
{
  tdmtable_column_rank();
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmtable_column_shape_()
{
  tdmtable_column_shape();
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmtable_column_size_()
{
  tdmtable_column_size();
}

/*
*:>#####################################################################
*:<#####################################################################
*/
void 
kam_tdmtable_column_type_()
{
  tdmtable_column_type();
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
void 
kam_tdmtable_maxrowcount_()
{
   char* name = ku_gets();	/* table name */
   long maxrowcount = ku_geti();/* number of rows allocated */

   tdmtable_maxrowcount(name,maxrowcount);
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
void 
kam_tdmtable_name_()
{
   char* name = ku_gets();	/* table name */

   tdmtable_name(name);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tdmtable_dump_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/TABLE/PRINT TNAME [ NROWS IFIRST ]
*:<---------------------------------------------------------------------
*/
#define COLLISTSIZE 1000
void 
kam_tdmtable_dump_()
{
   int nn = ku_npar()-4;         /* If user uses carets to separate col names,
                                 ** nn=1, if spaces, nn is # of cols. */
   char* name = ku_gets();	 /* table name */
   long nrows = ku_geti();	 /* number of rows to dump */
   long ifirst = ku_geti();	 /* first row number to dump */
   char* outputfile = ku_gets(); /* name of output file */
   char* oneElement,colList[COLLISTSIZE+1];
   int ii;
   colList[0]=0;
   for(ii=0;ii<nn;ii++) {
     oneElement=ku_gets();
     if(strlen(colList)+strlen(oneElement)+4>COLLISTSIZE) {
       printf("Column list truncated.\n"); break;
     }
     strcat(colList,oneElement); strcat(colList,"^");
   }

   /* tdmtable_dump expects the col list to be delimited with ^s */
   for(ii=0;colList[ii];ii++) { if(colList[ii]==' ') colList[ii]='^'; }

   tdmtable_dump(name, nrows, ifirst, outputfile, colList);
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
void 
kam_tdmtable_print_()
{
   char* name = ku_gets();	/* table name */
   long nrows = ku_geti();	/* number of rows to print */
   long ifirst = ku_geti();	/* first row number to print */

   tdmtable_print(name, nrows, ifirst);
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
void 
kam_tdmtable_rowcount_()
{
   char* name = ku_gets();	/* table name */
   long rowcount = ku_geti();	/* number of rows filled */

   tdmtable_rowcount(name, rowcount);
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
void 
kam_tdmtable_rowsize_()
{
   char* name = ku_gets();	/* table name */

   tdmtable_rowsize(name);
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
void 
kam_tdmtable_show_()
{
   char* name = ku_gets();	/* table name */

   tdmtable_show(name);
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
void 
kam_tdmtable_specifier_()
{
   char* name = ku_gets();	/* table name */

   tdmtable_specifier(name);
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
void 
kam_tdmtable_typename_()
{
   char* name = ku_gets();	/* table name */

   tdmtable_typename(name);
}


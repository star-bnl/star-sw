/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         tdm_types.h
*:DESCRIPTION:  Variable types for TDM
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      29nov95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#ifndef TDM_TYPES_H
#define TDM_TYPES_H

/*-------------------------------------------- INCLUDES             --*/
#include "asuLib.h"
#include "dstype.h"
#include "tdm_macros.h"
#include "table_header.h"

/*-------------------------------------------- CORBA                --*/
#ifdef CORBA
#include "tdm_i.hh"
#endif /*CORBA*/

/*-------------------------------------------- TYPEDEFS             --*/
#ifndef CORBA

#include "stafCorba.h"

TYPEDEF_SEQUENCE(unsigned char,TDM_DATABLOCK_T);

typedef enum tdm_fill_mode_t {		/* HACK << UNECESSARY */
   TDM_APPEND_MODE,
   TDM_OVERWRITE_MODE,
   TDM_READONLY_MODE,
   TDM_UNKNOWN_MODE
}TDM_FILL_MODE_T;

typedef long DSL_PTR_T;

typedef struct tdm_data_t {		/* generic data type */
   DS_TYPE_CODE_T _d;			/* type switch */
   union {				/* pointer to data */
      char *c;
      unsigned char *o;
      short *s;
      unsigned short *us;
      long *l;
      unsigned long *ul;
      float *f;
      double *d;
      void *v;
   }data;
}TDM_DATA_T;

/* HACK --- This is not consistant with the IDL file */
/*TYPEDEF_SEQUENCE(TDM_DATA_T,TDM_CELLDATA_T);*/
typedef struct tdm_celldata_t {		/* generic cell data type */
   long _maximum;			/* allocation (words) */
   long _length;			/* length (words) */
   long _size;				/* size (bytes) */
   DS_TYPE_CODE_T _d;			/* type switch */
   union {				/* pointer to data */
      char *c;
      unsigned char *o;
      short *s;
      unsigned short *us;
      long *l;
      unsigned long *ul;
      float *f;
      double *d;
      void *v;
   }data;
}TDM_CELLDATA_T;

typedef struct tdm_column_t {
   long nCol;           /* column number */
   char * name;         /* variable name */
   char * type;         /* type name */
   DS_TYPE_CODE_T code;	/* type code */
   long size;           /* size in bytes */
   long rank;           /* count of indices */
   long shape[4];       /* max of indices */
   long elcount;	/* number of elements */
}TDM_COLUMN_T;

#endif /*CORBA*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
extern CC_P int tdm_init();
extern CC_P int tdm_start();
extern CC_P int tdm_stop();
extern CC_P void tdm_printCell(FILE *stream, TDM_CELLDATA_T *data
		, TDM_COLUMN_T *col);
extern CC_P int tdm_nameMatch(char *a, char *b);

#ifdef __cplusplus
/*- Table conversion routines. -*/
extern CC_P int tdm_cvtDst2st(DS_DATASET_T *pT
		, TABLE_HEAD_ST *& tbl_h, char *& tbl_d);
#endif /*__cplusplus*/

#ifndef NOKUIP
extern CC_P void tdm_def_();

extern CC_P void kam_tdm_allocstats_();
extern CC_P void kam_tdm_count_();
extern CC_P void kam_tdm_list_();
extern CC_P void kam_tdm_newdataset_();
extern CC_P void kam_tdm_newtable_();
extern CC_P void kam_tdmtypespecifiers_list_();
extern CC_P void kam_tdmtypespecifiers_show_();
extern CC_P void kam_tdmtypespecifiers_load_();
extern CC_P void kam_tdmdataset_adddataset_();
extern CC_P void kam_tdmdataset_addtable_();
extern CC_P void kam_tdmdataset_entrycount_();
extern CC_P void kam_tdmdataset_entryname_();
extern CC_P void kam_tdmdataset_findentry_();
#ifdef OLD_DSL
extern CC_P void kam_tdmdataset_maxentrycount_();
#endif /*OLD_DSL*/
extern CC_P void kam_tdmdataset_name_();
extern CC_P void kam_tdmdataset_show_();
extern CC_P void kam_tdmtable_cell_getvalue_();
extern CC_P void kam_tdmtable_cell_putvalue_();
extern CC_P void kam_tdmtable_columncount_();
extern CC_P void kam_tdmtable_column_code_();
extern CC_P void kam_tdmtable_column_elcount_();
extern CC_P void kam_tdmtable_column_find_();
extern CC_P void kam_tdmtable_column_name_();
extern CC_P void kam_tdmtable_column_rank_();
extern CC_P void kam_tdmtable_column_shape_();
extern CC_P void kam_tdmtable_column_size_();
extern CC_P void kam_tdmtable_column_type_();
extern CC_P void kam_tdmtable_maxrowcount_();
extern CC_P void kam_tdmtable_name_();
extern CC_P void kam_tdmtable_print_();
extern CC_P void kam_tdmtable_dump_();
extern CC_P void kam_tdmtable_rowcount_();
extern CC_P void kam_tdmtable_rowsize_();
extern CC_P void kam_tdmtable_show_();
extern CC_P void kam_tdmtable_specifier_();
extern CC_P void kam_tdmtable_typename_();
#endif /*NOKUIP*/

extern CC_P STAFCV_T tdm_allocstats();
extern CC_P STAFCV_T tdm_count();
extern CC_P STAFCV_T tdm_list();
/*extern CC_P STAFCV_T tdm_newdataset(char* name, long dim);  -akio*/
extern CC_P STAFCV_T tdm_newdataset(char* name); /*-akio*/
extern CC_P STAFCV_T tdm_newtable(char* name, char* spec
		, long rowcount);
extern CC_P STAFCV_T tdmtypespecifiers_list(long tid);
extern CC_P STAFCV_T tdmtypespecifiers_show(char* name);
extern CC_P STAFCV_T tdmtypespecifiers_load(char* fname);
/*extern CC_P STAFCV_T tdmdataset_adddataset(char* dsname,char* name,long dim);  -akio*/
extern CC_P STAFCV_T tdmdataset_adddataset(char* dsname,char* name); /*akio*/
extern CC_P STAFCV_T tdmdataset_addtable(char* dsname, char* name
		, char* spec, long rowcount);
extern CC_P STAFCV_T tdmdataset_entrycount(char* name);
extern CC_P STAFCV_T tdmdataset_entryname();
extern CC_P STAFCV_T tdmdataset_findentry();
#ifdef OLD_DSL
extern CC_P STAFCV_T tdmdataset_maxentrycount(char* name);
#endif /*OLD_DSL*/
extern CC_P STAFCV_T tdmdataset_name(char* name);
extern CC_P STAFCV_T tdmdataset_show();
extern CC_P STAFCV_T tdmtable_cell_getvalue(char* cellSpec,char* screenSwitch);
extern CC_P STAFCV_T tdmtable_cell_putvalue(char* cellSpec, long nv
		, char **values);
extern CC_P STAFCV_T tdmtable_columncount(char* name);
extern CC_P STAFCV_T tdmtable_column_code();
extern CC_P STAFCV_T tdmtable_column_elcount();
extern CC_P STAFCV_T tdmtable_column_find();
extern CC_P STAFCV_T tdmtable_column_name();
extern CC_P STAFCV_T tdmtable_column_rank();
extern CC_P STAFCV_T tdmtable_column_shape();
extern CC_P STAFCV_T tdmtable_column_size();
extern CC_P STAFCV_T tdmtable_column_type();
extern CC_P STAFCV_T tdmtable_maxrowcount(char* name,long maxrowcount);
extern CC_P STAFCV_T tdmtable_name(char* name);
extern CC_P STAFCV_T tdmtable_dump(char* name, long nrows
		, long ifirst,char *outputfilename,char *columnList);
extern CC_P STAFCV_T tdmtable_print(char* name, long nrows
		, long ifirst);
extern CC_P STAFCV_T tdmtable_rowcount(char* name, long rowcount);
extern CC_P STAFCV_T tdmtable_rowsize(char* name);
extern CC_P STAFCV_T tdmtable_show(char* name);
extern CC_P STAFCV_T tdmtable_specifier(char* name);
extern CC_P STAFCV_T tdmtable_typename(char* name);

#endif /* TDM_TYPES_H */


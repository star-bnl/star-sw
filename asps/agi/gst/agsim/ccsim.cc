/*CMZ :          23/08/98  16.19.48  by  Pavel Nevski*/
/*-- Author :    Pavel Nevski   28/11/97*/
/*****************************************************/
/*               S T A F   i n t e r f a c e         */
/*****************************************************/
#ifndef STAF
 
extern "C" void staf_start_    () {}
extern "C" void staf_stop_     () {}
extern "C" int  tdm_map_table_ () {return 0;}
extern "C" int  tdm_clear_all_ () {return 0;}
extern "C" void ami_module_register_ ()    {}
 
#else
#include "PAM.h"
#include <stdio.h>
#include <stream.h>
#include <stdlib.h>
#include <string.h>
#include "asuLib.h"
#include "asuAlloc.h"
#include "emlLib.h"
#include "socLib.h"
#include "spxLib.h"
#include "tdmLib.h"
#include "duiLib.h"
#include "dioLib.h"
#include "amiLib.h"
#include "tntLib.h"
#include "topLib.h"
#include "dstype.h"
extern int gcflag_;
 
#define staf_start_          F77_NAME(staf_start,STAF_START)
#define staf_stop_           F77_NAME(staf_stop,STAF_STOP)
#define dui_cdir_            F77_NAME(dui_cdir,dui_cdir)
#define tdm_new_table_       F77_NAME(tdm_new_table,tdm_new_table)
#define tdm_map_table_       F77_NAME(tdm_map_table,TDM_MAP_TABLE)
#define tdm_find_spec_       F77_NAME(tdm_find_spec,TDM_FIND_SPEC)
#define tdm_get_spec_        F77_NAME(tdm_get_spec,TDM_GET_SPEC)
#define tdm_get_ccount_      F77_NAME(tdm_get_ccount,TDM_GET_CCOUNT)
#define tdm_get_column_      F77_NAME(tdm_get_column,TDM_GET_COLUMN)
#define tdm_clear_all_       F77_NAME(tdm_clear_all,TDM_CLEAR_ALL)
#define ami_call_            F77_NAME(ami_call,AMI_CALL)
#define ami_module_call_     F77_NAME(ami_module_call,AMI_MODULE_CALL)
#define ami_module_register_ F77_NAME(ami_module_register,AMI_MODULE_REGISTER)
#define xdf_open_            F77_NAME(xdf_open,XDF_OPEN)
#define xdf_close_           F77_NAME(xdf_close,XDF_CLOSE)
#define xdf_next_record_     F77_NAME(xdf_next_record,XDF_NEXT_RECORD)
#define xdf_get_struct_      F77_NAME(xdf_get_struct,XDF_GET_STRUCT)
#define xdf_getev_           F77_NAME(xdf_getev,XDF_GETEV)
#define cs_get_func_         F77_NAME(cs_get_func,CS_GET_FUNC)
/*---------------------------------------------------------------------------*/
 
typedef struct XdfLun_t
{ /* Analog of F77 logical unit */
  FILE 		*fFile;  	/* pointer to C file descriptor */
  XDR  		*fStream; 	/* XDR stream      */
  DS_DATASET_T 	*fDataSet; 	/* "root" dataset  */
  char 		fName[512];	/* File Name	*/
  char 		fType[8];	/* File type    */
} XdfLun_t;
 
/*---------------------------------------------------------------------------*/
/*                         p r o t o t y p e s                               */
 
/* extern "C"  int  dsTypeSpecifier (char**, unsigned*, int); */
 
extern "C" FNC_PTR_T type_of_call cs_get_func_(char* name,int n);
extern "C" void  staf_banner     (FILE* stream);
extern CC_P int  ami_load        (amiBroker *broker);
int              ami_load        (amiBroker *broker) {return 1;}
 
/****************************************************************************/
 
extern "C" void type_of_call staf_start_ ()
{
   asu_init(); asu_start();
   eml_init(); eml_start();
   soc_init(); soc_start();
   spx_init(); spx_start();
   tdm_init(); tdm_start();
   dui_init(); dui_start();
   dio_init(); dio_start();
   ami_init(); ami_start();
   top_init(); top_start();
   tnt_init(); tnt_start();
   if(gcflag_) staf_banner(stdout);
}
 
extern "C" void type_of_call staf_stop_ ()
{
   tnt_stop();
   ami_stop();
   dio_stop();
   dui_stop();
   tdm_stop();
   spx_stop();
   soc_stop();
   eml_stop();
   asu_stop();
}
 
/*---------------------------------------------------------------------------*/
 
extern "C" int dui_cdir_ (char* path,int lp)
{  if (!dui) return 0;
   if (path[0]!='.') dui->mkdir(path);
   return dui->cd(path);
}
 
/*---------------------------------------------------------------------------*/
 
extern "C" int type_of_call tdm_map_table_
#ifndef CERNLIB_MSSTDCALL
                             (char* path, char* name, char* spec, long* l,
                              char* data, int lp, int ln, int ls)
#else
                             (char* path, int lp,
                              char* name, int ln,
                              char* spec, int ls,
                              long* l,char* data)
#endif
{
  tdmDataset*       tDs = NULL; // pointer to tdm class member function
  tdmTable*         aDs = NULL; // pointer to table finder function
  DS_DATASET_T*     pDs = NULL; // pointer to directory table
  DS_DATASET_T*     dDs = NULL; // pointer to dataset table
 
  char cpath[132];  cpath[0]=0;  strncat(cpath,path,lp);
  char cname[32];   cname[0]=0;  strncat(cname,name,ln);
  char cspec[2048]; cspec[0]=0;  strncat(cspec,spec,ls);
  long k=*l;
  //             this should work,  but it does not
  //  ier=dsNewDataset(&ds,cpath);  tdm->createTable(name, ds);
 
  if (!tdm) { printf(" tdm_map_table ERROR: no tdm is found !\n"); return 0; }
  if (!(tDs=tdm->findDataset(cpath))) dui->mkdir(cpath);
 
  if ( (tDs=tdm->findDataset(cpath))
       &&  (tDs->cvtDslPointer((DSL_PTR_T &)pDs)) && pDs)
     { for (int i=0;  i < pDs->elcount;  i++)
       { // printf (" dataset %d %d %d %s tested \n",i,pDs->p.link[i],
         //          (pDs->p.link[i])->flags, (pDs->p.link[i])->name);
         if ((dDs=pDs->p.link[i]) && dDs->flags && !strcmp(dDs->name,cname))
         {  if (k<0)
            { size_t rsize;  dsTableRowSize(&rsize,dDs);
              if (rsize)     k=(-k)/(rsize/sizeof(k));
              int d=k*rsize+(*l)*sizeof(k);
              if (!d) printf (" table %s rsize=%d k=%d %d\n",cname,rsize,*l,d);
            }
            dDs->maxcount = k;
            dDs->elcount  = k;
            dDs->p.data   = data;
            return          k;
            // return (DSL_PTR_T &) dDs;
       } }
       return dsAddTable(pDs,cname,cspec,k,&data);
     }
  printf (" TDM_MAP_TABLE: directory %s not found \n",cpath);
  return 0;
}
 
extern "C" int type_of_call tdm_new_table_
#ifndef CERNLIB_MSSTDCALL
                 (char* name, char* spec, int* n, int ln, int ls)
#else
                 (char* name, int ln,
                  char* spec, int ls,
                  int* n)
#endif
{ int i;  char* s;
  /* skip spaces */  s=spec; while (*s==' ') { s++; }
  /*
     printf(" name = %s \n",name);
     printf(" spec = %s \n",spec);
     printf(" n    = %d \n",*n);    */
     i = tdm_newtable (name, s, *n);
     if (i) tdmtable_maxrowcount(name, *n);
     return i;
}
 
/*---------------------------------------------------------------------------*/
 
extern "C" int type_of_call tdm_find_spec_(char* c, int lc)
{
  char *pSpec;  int i;  unsigned lspec=0;
  for (i=1;;i++)
  { if (!dsTypeSpecifier((const char**)&pSpec, &lspec, i)) return 0;
    if (strstr(pSpec,c)) return i;
  }
}
 
extern "C" int type_of_call tdm_get_spec_(TABLE_HEAD_ST* pTab, char* c, int lc)
{
  char *pSpec;
/*
  printf(" dataset name = %20s \n",pTab->name);
  printf("         type = %20s \n",pTab->type);
  printf("         maxl = %d \n",  pTab->maxlen);
  printf("         nok  = %d \n",  pTab->nok);
  printf("         rbyte= %d \n",  pTab->rbytes);
  printf("         ds_p = %d \n",  pTab->dsl_pointer);
  printf("         dd_p = %d \n",  pTab->data_pointer);
*/
  if (!dsTableTypeSpecifier((const char**)&pSpec,
                            (ds_dataset_t*)pTab->dsl_pointer)) return 0;
     if (strlen(pSpec)<lc) strcpy(c,pSpec); else strncpy(c,pSpec,lc);
     return strlen(pSpec)+1;
}
 
/*---------------------------------------------------------------------------*/
 
extern "C" int type_of_call tdm_get_ccount_(TABLE_HEAD_ST* pTab)
 
{ unsigned ccount;
  if (!dsTableColumnCount(&ccount, (ds_dataset_t*)pTab->dsl_pointer)) return 0;
  return (int) ccount;
}
 
extern "C" int type_of_call tdm_get_column_(TABLE_HEAD_ST* pTab,int* k,char* c,char* d,
                                unsigned* l, unsigned* e, unsigned* m)
{ char *cc; char *dd;
  if (!dsColumnName     ((const char**)&cc,(ds_dataset_t*)pTab->dsl_pointer,*k)
   || !dsColumnTypeName ((const char**)&dd,(ds_dataset_t*)pTab->dsl_pointer,*k)
   || !dsColumnSize     (l  , (ds_dataset_t*) pTab->dsl_pointer,*k)
   || !dsColumnElcount  (e  , (ds_dataset_t*) pTab->dsl_pointer,*k)
   || !dsColumnDimCount (m  , (ds_dataset_t*) pTab->dsl_pointer,*k))  return 0;
 
  strcpy(d,dd);
  strcpy(c,cc);
  return strlen(cc)+1;
}
/*---------------------------------------------------------------------------*/
 
extern "C" int type_of_call tdm_clear_all_  (char* path, int lp)
{  tdmDataset*   tDs  = NULL;
   DS_DATASET_T  *ds, *dt, *du, *dd[20];
   int           i,j,l,mm[20];
 
   /* get starting address of a dataset */
 
   ds=0;
   if (tDs=tdm->findDataset(path))  tDs->cvtDslPointer ((DSL_PTR_T &) ds);
   if (!ds) { printf (" tdm_zero_all: bad path = %s \n",path); return 0; }
 
   l=0; dd[0]=ds; mm[0]=-1;
   for (;ds;)
   {
     if (ds->tid)
     { ds->elcount=0; if(gcflag_>1) printf(" clearing table %20s \n",ds->name);
       break;
     }
     du=0;
     for (j=mm[l]+1; j< ds->elcount; j++)
     {
       if (!(dt=ds->p.link[j])) continue;
       if (!(dt->tid)) { mm[l]=j; du=dt; break; }
       dt->elcount=0; if(gcflag_>1) printf(" clearing table %20s \n",dt->name);
     }
     /* new dataset found  - and selected */
     if (du)  { /* going  up  the tree */  l+=1; ds=du; mm[l]=-1; dd[l]=ds; }
     else     { /* going down the tree */  l-=1; if (l<0) break;  ds=dd[l]; }
   }
   return 1;
}
/***************************************************************************-*/
 
extern "C" int type_of_call ami_call_
#ifndef CERNLIB_MSSTDCALL
         (char* name,int* n, char* tables, int ln, int lt)
#else
         (char* name,int ln,
          int* n, char* tables,  int lt)
#endif
 
{ amiInvoker*    invoker;
  FNC_PTR_T      myPamFtn;
  long           myRank;
  tdmTable*      t[40];
  TABLE_HEAD_ST* h[40];
  char*          d[40];
  int            i,status;
 
  if (!ami || !tdm || !(invoker=ami->findInvoker(name))) return 0;
 
  myRank   = invoker->rank();
  myPamFtn = invoker->pFunction();
  for (i=0;i<*n;i++)
   {  t[i]=tdm->findTable(tables+i*lt);   if (!t[i]) return 0;
      if (!t[i]->cvtTasStructs(h[i],d[i]))  return 0;
      /* printf(" table found %20s \n",h[i]->name); */
   }
   status = ami_pamSwitch(myRank, myPamFtn, h, d);
   if (status) { for (i=0;i<*n;i++)  { t[i]->rowCount(h[i]->nok); }}
   return status;
}
 
extern "C" int type_of_call ami_module_call_
#ifndef CERNLIB_MSSTDCALL
  (char* name, int* n, char* tables,int ln, int lt)
#else
  (char* name, int ln, int* n, char* tables, int lt)
#endif
 
{
    int i; char* ctab[40];
    for (i=0;i<*n;i++)
    { ctab[i]=tables+i*lt; /* printf(" i,ctab = %d %s \n",i,ctab[i]); */ }
    return ami_call (name, *n, (char**) ctab);
}
 
 
#ifdef     DS_ADVANCED_try
int tabspeci_(int* i,char* c)
{ DS_TYPE_T *pType;
  if (!dsTypePtr(&pType, *i)) return 0;
       strcpy(c,pType->name); return strlen(pType->name)+1;
}
#endif
 
 
extern "C" void type_of_call ami_module_register_ (char* name, int n)
{
   STRING_SEQ_T specs;
   char sname[80];
   strncpy(sname,name,n);
 
   sname[n]='_';
   FNC_PTR_T address = cs_get_func_ (sname,n+1);
   if (!address) printf (" ==> ami_module_register : address=0\n");
 
   sname[n]='\0';
   specs._length = specs._maximum = 0;
   specs._buffer = 0;
 
   ami->deleteInvoker(sname);
   ami->newInvoker(sname,0,address,specs);
}
 
/****************************************************************************/
 
 
int xdf_open(XdfLun_t **Lun, char *FileName,char *mode)
{
  XdfLun_t *lun;
 
  if (*Lun) { printf("xdf_open. Error, lun is non zero %d\n",*lun); return 1;}
 
  lun = (XdfLun_t*)MALLOC(sizeof(XdfLun_t));
 
  strcpy(lun->fType,"r");
  if (mode && mode[0] && mode[0]!=' ') strcpy(lun->fType,mode);
  strcpy(lun->fName,FileName);
 
  lun->fFile = fopen(lun->fName,lun->fType);
 
  if(!lun->fFile) {/* open error */
    printf("xdf_open. Error, can not open file %s %s\n",lun->fName,lun->fType);
    FREE (lun); return 2;}
 
  if(!dsNewDataset(&lun->fDataSet,"NEVSKI")) {
    printf("xdf_open. Error, can not create data set for file %s %s\n",
                                                        lun->fName,lun->fType);
    FREE (lun); lun = NULL; return 2;}
 
    lun->fStream=(XDR*)MALLOC(sizeof(XDR));
 
    xdrstdio_create(lun->fStream, lun->fFile, XDR_DECODE);
 
   *Lun = lun; return 0;
}
/*---------------------------------------------------------------------------*/
int xdf_next_record(XdfLun_t *Lun)
 
{
   if(!Lun) {
     printf("xdf_next_record: Error, empty file handler\n"); return 13;}
 
 if (!xdr_dataset(Lun->fStream,&Lun->fDataSet))
  {printf("xdf_next_record: end of file %s",Lun->fName); return -1;}
 return 0;
}
 
/*---------------------------------------------------------------------------*/
 
int xdf_get_struct(XdfLun_t *lun, char *name,
     DS_DATASET_T **Entry,void **Data, size_t *nrows) {
 
   if(!lun) {
     printf("xdf_get_struct: Error, empty file handler\n"); return 13;}
 
if (*Entry) { FREE (*Entry); *Entry=NULL;}; 	/* ????? */
if (*Data)  { FREE (*Data);  *Data=NULL;};	/* ????? */
 
 
if (!dsFindEntry(Entry,lun->fDataSet,name)) return 1;
 
 
if (!dsTableDataAddress((char**)Data,*Entry)) return 2;
if (!dsTableRowCount(nrows,*Entry)) return 3;
return 0;
}
 
 
int xdf_close(XdfLun_t **Lun) {
  int ians;
 
   if(!*Lun) {
     printf("xdf_close: Error, empty file handler\n"); return 13;}
 
  ians = fclose((*Lun)->fFile);
  FREE ((*Lun)->fStream);
  FREE ((*Lun)->fDataSet);
  FREE ((*Lun)); *Lun=NULL;
  return ians;
}
 
/***********************fortran interface to xdf************************/
 
extern "C" void type_of_call xdf_open_
#ifndef CERNLIB_MSSTDCALL
(unsigned long *Lun, char *File, char *Mode,int *ier, int lFile, int lMode)
#else
(unsigned long *Lun, char *File, int lFile, char *Mode, int lMode,int *ier)
#endif
 
{
  char file[512], mode[8];
  int l;
 
  for (l=lFile; l&&File[l-1]==' '; l--);  file[0]=0; strncat(file,File,l);
  for (l=lMode; l&&Mode[l-1]==' '; l--);  mode[0]=0; strncat(mode,Mode,l);
 
  *ier = xdf_open((XdfLun_t **)Lun, file,mode);
}
 
 
extern "C" void xdf_close_(unsigned long *Lun,int *ier)
{
  *ier = xdf_close((XdfLun_t **)Lun);
}
 
/*-------------------------------------------------------------------------*/
 
extern "C" void type_of_call xdf_next_record_(unsigned long *Lun, int *ier)
{
  *ier = xdf_next_record((XdfLun_t*) *Lun);
}
 
 
extern "C" void type_of_call xdf_get_struct_
#ifndef CERNLIB_MSSTDCALL
    (unsigned long *Lun, char *Name,
     unsigned long *Entry, unsigned long *Data, size_t *nrows, int *ier,
     int lName)
#else
    (unsigned long *Lun, char *Name,int lName,
     unsigned long *Entry, unsigned long *Data, size_t *nrows, int *ier
     )
#endif
{
  char name[512];
  int l;
 
  for (l=lName; l&&Name[l-1]==' '; l--);  name[0]=0; strncat(name,Name,l);
 
  *ier = xdf_get_struct((XdfLun_t*) *Lun, name,
                         (DS_DATASET_T **)Entry,(void **)Data, nrows);
}
 
/*-------------------------------------------------------------------------*/
 
extern "C" void type_of_call xdf_getev_
#ifndef CERNLIB_MSSTDCALL
 (unsigned long* Lun, char* dir, int* ier, int ld)
#else
 (unsigned long* Lun, char* dir, int ld, int* ier)
#endif
{
   tdmDataset    *d;
   DSL_PTR_T     ddd;
   DS_DATASET_T  *pDS,*pDD;
   bool_t        result;
   XdfLun_t      *lun;
   XDR           *myXDR;
   char          dirc[512];
   int           l;
 
#define DO(A) ier+=1; if (!(A)) \
        { printf(" xdf_getev error: "#A" fails \n"); return; }
 
   for (l=ld; l&&dir[l-1]==' ';l--); dirc[0]=0; strncat(dirc,dir,l);
 
   ier=0;
   DO (tdm)
   DO (d=tdm->findDataset(dirc));
   DO (d->cvtDslPointer ((DSL_PTR_T &)pDD));
 
   DO (lun=(XdfLun_t*)*Lun);
   DO (myXDR=lun->fStream);
   DO (xdr_dataset_type (myXDR, &pDS));
   DO (pDS);
   DO (dsIsDataset (&result, pDS));
   DO (result);
 
   DO (dio_mapHierarchy (pDD, pDS));
   DO (dsAllocTables (pDS));
   DO (xdr_dataset_data (myXDR, pDS));
   DO (dsFreeDataset (pDS));
   ier=0;
}
 
/**************************************************************************/
#endif

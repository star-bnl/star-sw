/*Copyright 1996, Lawrence Berkeley National Laboratory
*:>--------------------------------------------------------------------
**:FILE:         tnt_kam.c
**:DESCRIPTION:  C++ KUIP Action Modules for TNT-TEMPLATE ASP
**:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:         -- STILL IN DEVELOPMENT --
**:HISTORY:      13jun96-v000a-cet- creation
**:<-------------------------------------------------------------------
*/

/*-------------------------------------------- INCLUDES            --*/
#include <stdlib.h>
#include <stdio.h>

#include "kuip.h"
#include "asuAlloc.h" 
#include "emlLib.h"
#include "tdmLib.h"
#include "tntLib.h"


/*-------------------------------------------- TYPEDEFS            --*/
/*-------------------------------------------- GLOBALS             --*/
/*-------------------------------------------- PROTOTYPES          --*/
#include "tntHBOOK.h"


/*---------------------------------------------------------------------
** TNT/COUNT
*/
void kam_tnt_count_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */

  STAFCV_T status = tnt_count();
}

STAFCV_T
tnt_count()
{
  printf("TNT:\tObject count = %d \n",tnt->count());
  EML_SUCCESS(STAFCV_OK);
}

/*---------------------------------------------------------------------
** TNT/LIST
*/
void kam_tnt_list_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */

  STAFCV_T status = tnt_list();
}

STAFCV_T 
tnt_list()
{
  char *s;
  printf("%s", s=tnt->list());
  FREE(s); /*fix memory leak -akio*/
  EML_SUCCESS(STAFCV_OK);
}

/*---------------------------------------------------------------------
** TNT/PAW
*/
void kam_tnt_paw_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
   
  STAFCV_T status = tnt_paw();
}

STAFCV_T 
tnt_paw() {

  tnt_start_paw_();
  EML_SUCCESS(STAFCV_OK);
}

/*---------------------------------------------------------------------
** TNT/SHARE
*/
void kam_tnt_share_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
   
  STAFCV_T status = tnt_share();
}

STAFCV_T 
tnt_share() {

  tnt_start_share_();
  EML_SUCCESS(STAFCV_OK);
}

/*---------------------------------------------------------------------
** TNT/NEWCWNTUPLE NAME TABLE
*/
void kam_tnt_newcwntuple_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
  char *tname = ku_gets();	/* name of tdmTable */

  STAFCV_T status = tnt_newcwntuple(hid, tname);
}

STAFCV_T 
tnt_newcwntuple(long hid, char* tname)
{
  tdmTable* table = tdm->findTable(tname);

  if (table == NULL || !tnt->createCWNtuple(hid,table)) {
    EML_FAILURE(FAILURE);
  }
  EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/HID NTUPLE
*/
void kam_tntcwntuple_hid_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
   
  STAFCV_T status = tntcwntuple_hid(hid);
}

STAFCV_T 
tntcwntuple_hid(long hid)
{
  tntCWNtuple* tuple = tnt->findCWNtuple(hid);

  if (tuple == NULL) {
    EML_CONTEXT("ERROR: can't find CW ntuple with HID %d.\n",hid);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  printf("TNTCWNTUPLE:\tHID = %d \n",tuple->hid());

  EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/TITLE NTUPLE
*/
void kam_tntcwntuple_title_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
   
  STAFCV_T status = tntcwntuple_title(hid);
}

STAFCV_T 
tntcwntuple_title(long hid)
{
  tntCWNtuple *tuple = tnt->findCWNtuple(hid);
  char *s; /*fix memory leak -akio*/
  
  if (tuple == NULL) {
    EML_CONTEXT("ERROR: can't find CW ntuple with HID %d.\n",hid);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  printf("TNTCWNTUPLE:\tTitle = (%s) \n",s=tuple->title()); /*fix memory leak -akio*/
  FREE(s);  /*fix memory leak -akio*/

  EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/ENTRYCOUNT NTUPLE
*/
void kam_tntcwntuple_entrycount_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
   
  STAFCV_T status = tntcwntuple_entrycount(hid);
}

STAFCV_T 
tntcwntuple_entrycount(long hid)
{
  tntCWNtuple *tuple = tnt->findCWNtuple(hid);
  
  if (tuple == NULL) {
    EML_CONTEXT("ERROR: can't find CW ntuple with HID %d.\n",hid);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  printf("TNTCWNTUPLE:\tEntry Count = %d \n",tuple->entryCount());

  EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/COLUMNCOUNT NTUPLE
*/
void kam_tntcwntuple_columncount_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
   
  STAFCV_T status = tntcwntuple_columncount(hid);
}

STAFCV_T 
tntcwntuple_columncount(long hid)
{
  tntCWNtuple *tuple = tnt->findCWNtuple(hid);
  
  if (tuple == NULL) {
    EML_CONTEXT("ERROR: can't find CW ntuple with HID %d.\n",hid);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  printf("TNTCWNTUPLE:\tColumn Count = %d \n",tuple->columnCount());

  EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/ZEBRADIR NTUPLE
*/
void kam_tntcwntuple_zebradir_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
   
  STAFCV_T status = tntcwntuple_zebradir(hid);
}

STAFCV_T 
tntcwntuple_zebradir(long hid)
{
  tntCWNtuple *tuple = tnt->findCWNtuple(hid);
  
  if (tuple == NULL) {
    EML_CONTEXT("ERROR: can't find CW ntuple with HID %d.\n",hid);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  /*   printf("TNTCWNTUPLE:\tZebra Dir = (%s) \n",tuple->zebraDir()); HACK*/

  EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/GETTABLE NTUPLE TABLE
*/
void kam_tntcwntuple_gettable_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
  char *tname = ku_gets();	/* name of tdmTable */

  STAFCV_T status = tntcwntuple_gettable(hid, tname);
}

STAFCV_T 
tntcwntuple_gettable(long hid, char* tname)
{
  tntCWNtuple* tuple = tnt->findCWNtuple(hid);
  tdmTable* table = tdm->findTable(tname);
  
  if (tuple == NULL || table == NULL) {
    EML_CONTEXT("ERROR: One is invalid:  HID %d or name %s.\n",hid,tname);
    EML_FAILURE(FAILURE);
  }

  EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/GETTABLE NTUPLE TABLE
*/
void kam_tntcwntuple_append_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
  char *tname = ku_gets();	/* name of tdmTable */

  STAFCV_T status = tntcwntuple_append(hid, tname);
}

STAFCV_T 
tntcwntuple_append(long hid, char* tname)
{
  tntCWNtuple* tuple = tnt->findCWNtuple(hid);
  tdmTable* table = tdm->findTable(tname);
  
  if (tuple == NULL || table == NULL) {
    EML_CONTEXT("ERROR: One is invalid:  HID %d or name %s.\n",hid,tname);
    EML_FAILURE(FAILURE);
  }
  tuple->append(table);

  EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/PUTTABLE NTUPLE TABLE
*/
void
kam_tntcwntuple_puttable_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
  char *tname = ku_gets();	/* name of tdmTable */

  STAFCV_T status = tntcwntuple_puttable(hid,tname);
}
STAFCV_T tntcwntuple_puttable(long hid, char* tname)
{
  tntCWNtuple *tuple = tnt->findCWNtuple(hid);
  tdmTable *table = tdm->findTable(tname);

  if (tuple == NULL || table == NULL) {
    EML_CONTEXT("ERROR: One is invalid:  HID %d or name %s.\n",hid,tname);
    EML_FAILURE(FAILURE);
  }

  EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/SHOW TNTCWNTUPLE [ OPTION ]
*/
void kam_tntcwntuple_show_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
   
  STAFCV_T status = tntcwntuple_show(hid);
}

void kam_tntcwntuple_import_()
{
  long npars = ku_npar();     /* no. of KUIP param.s */
  long hid = ku_geti();               /* hid of CWNtuple */
  char *tname = ku_gets();    /* name of tdmTable */

  STAFCV_T status = tntcwntuple_import(hid, tname);
}

STAFCV_T
tntcwntuple_import(long hid, char* tname)
{
  tntCWNtuple* tuple = tnt->findCWNtuple(hid);
  tdmTable* table = tdm->findTable(tname);

  if (tuple == NULL || table == NULL) {
    EML_ERROR(KAM_FAILURE);
  }
  tuple->import(table);

  EML_SUCCESS(STAFCV_OK);
}
void kam_tntcwntuple_clear_()
{
  long npars = ku_npar();     /* no. of KUIP param.s */
  long hid = ku_geti();               /* hid of CWNtuple */

  STAFCV_T status = tntcwntuple_clear(hid);
}

STAFCV_T
tntcwntuple_clear(long hid)
{
  tntCWNtuple* tuple = tnt->findCWNtuple(hid);

  if (tuple == NULL) {
    EML_ERROR(KAM_FAILURE);
  }
  tuple->clear();

  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tntcwntuple_show(long hid)
{
  tntCWNtuple *tuple = tnt->findCWNtuple(hid);

  if (tuple == NULL) {
    EML_CONTEXT("ERROR: can't find CW ntuple with HID %d.\n",hid);
    EML_FAILURE(FAILURE);
  }
  tuple->show();

  EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/PRINT TNTCWNTUPLE [ NROWS IFIRST ]
*/
void
kam_tntcwntuple_print_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
   
  STAFCV_T status = tntcwntuple_print(hid);
}

STAFCV_T 
tntcwntuple_print(long hid)
{
  tntCWNtuple *tuple = tnt->findCWNtuple(hid);

  if (tuple == NULL) {
    EML_CONTEXT("ERROR: can't find CW ntuple with HID %d.\n",hid);
    EML_FAILURE(FAILURE);
  }
  tuple->show();

  EML_SUCCESS(STAFCV_OK);
}

/* 
*:>--------------------------------------------------------------------
**:FILE:         tnt_cstubs.c
**:DESCRIPTION:  
**:AUTHOR:       Dave Morrison
**:BUGS:         
**:HISTORY:      
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

STAFCV_T
tnt_count()
{
  printf("TNT:\tObject count = %ld \n",tnt->count());
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tnt_list()
{
  char *herb980615;
  char *s;
  s=tnt->list();
  herb980615=strtok(s,"\n");
  while(herb980615) {
     printf("%s\n",herb980615);    
     herb980615=strtok(NULL,"\n");
  }
  FREE(s); /*fix memory leak -akio*/
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tnt_paw() {

  tnt_start_paw_();
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tnt_share() {

  tnt_start_share_();
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tnt_newcwntuple(long hid, char* tname)
{
  tdmTable* table = tdm->findTable(tname);

  if (table == NULL ) EML_FAILURE(FAILURE);
  if( !tnt->createCWNtuple(hid,table)) EML_FAILURE(FAILURE);
  EML_SUCCESS(STAFCV_OK);
}
 
STAFCV_T 
tntcwntuple_hid(long hid)
{
  tntCWNtuple* tuple = tnt->findCWNtuple(hid);

  if (tuple == NULL) {
    EML_CONTEXT("ERROR: can't find CW ntuple with HID %d.\n",hid);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  printf("TNTCWNTUPLE:\tHID = %ld \n",tuple->hid());

  EML_SUCCESS(STAFCV_OK);
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
 
STAFCV_T 
tntcwntuple_entrycount(long hid)
{
  tntCWNtuple *tuple = tnt->findCWNtuple(hid);
  
  if (tuple == NULL) {
    EML_CONTEXT("ERROR: can't find CW ntuple with HID %d.\n",hid);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  printf("TNTCWNTUPLE:\tEntry Count = %ld \n",tuple->entryCount());

  EML_SUCCESS(STAFCV_OK);
}
 
STAFCV_T 
tntcwntuple_columncount(long hid)
{
  tntCWNtuple *tuple = tnt->findCWNtuple(hid);
  
  if (tuple == NULL) {
    EML_CONTEXT("ERROR: can't find CW ntuple with HID %d.\n",hid);
    EML_FAILURE(OBJECT_NOT_FOUND);
  }
  printf("TNTCWNTUPLE:\tColumn Count = %ld \n",tuple->columnCount());

  EML_SUCCESS(STAFCV_OK);
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
 
STAFCV_T 
tntcwntuple_puttable(long hid, char* tname)
{
  tntCWNtuple *tuple = tnt->findCWNtuple(hid);
  tdmTable *table = tdm->findTable(tname);

  if (tuple == NULL || table == NULL) {
    EML_CONTEXT("ERROR: One is invalid:  HID %d or name %s.\n",hid,tname);
    EML_FAILURE(FAILURE);
  }

  EML_SUCCESS(STAFCV_OK);
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

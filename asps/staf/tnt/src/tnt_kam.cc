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
void kam_tnt_count_(){kam_tnt_count();}
int kam_tnt_count()
{
   long npars = ku_npar();	/* no. of KUIP param.s */

   printf("TNT:\tObject count = %d \n",tnt->count());
   EML_SUCCESS(STAFCV_OK);
}

/*---------------------------------------------------------------------
** TNT/LIST
*/
void kam_tnt_list_(){kam_tnt_list();}
int kam_tnt_list()
{
   long npars = ku_npar();	/* no. of KUIP param.s */

   printf("%s",tnt->list() );
   EML_SUCCESS(STAFCV_OK);
}

/*---------------------------------------------------------------------
** TNT/PAW
*/
void kam_tnt_paw_(){kam_tnt_paw();}
int kam_tnt_paw() {

   long npars = ku_npar();	/* no. of KUIP param.s */
   
   tnt_start_paw_();
   EML_SUCCESS(STAFCV_OK);
}

/*---------------------------------------------------------------------
** TNT/SHARE
*/
void kam_tnt_share_(){kam_tnt_share();}
int kam_tnt_share() {

   long npars = ku_npar();	/* no. of KUIP param.s */
   
   tnt_start_share_();
   EML_SUCCESS(STAFCV_OK);
}

/*---------------------------------------------------------------------
** TNT/NEWCWNTUPLE NAME TABLE
*/
void kam_tnt_newcwntuple_(){kam_tnt_newcwntuple();}
int kam_tnt_newcwntuple() {

   long npars = ku_npar();	/* no. of KUIP param.s */
   long hid = ku_geti();	/* hid of CWNtuple */
   char *tname = ku_gets();	/* name of tdmTable */

   tdmTable* table;

   if( !tdm->findTable(tname,table)
   ||  !tnt->createCWNtuple(hid,table)
   ){
      EML_ERROR(KAM_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/HID NTUPLE
*/
void kam_tntcwntuple_hid_(){kam_tntcwntuple_hid();}
int kam_tntcwntuple_hid() {
   long npars = ku_npar();	/* no. of KUIP param.s */
   long hid = ku_geti();	/* hid of CWNtuple */
   
   tntCWNtuple* tuple;

   if( !tnt->findCWNtuple(hid,tuple) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("TNTCWNTUPLE:\tHID = %d \n",tuple->hid());
   EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/TITLE NTUPLE
*/
void kam_tntcwntuple_title_(){kam_tntcwntuple_title();}
int kam_tntcwntuple_title() {
   long npars = ku_npar();	/* no. of KUIP param.s */
   long hid = ku_geti();	/* hid of CWNtuple */
   
   tntCWNtuple* tuple;

   if( !tnt->findCWNtuple(hid,tuple) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("TNTCWNTUPLE:\tTitle = (%s) \n",tuple->title());
   EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/ENTRYCOUNT NTUPLE
*/
void kam_tntcwntuple_entrycount_(){kam_tntcwntuple_entrycount();}
int kam_tntcwntuple_entrycount() {
   long npars = ku_npar();	/* no. of KUIP param.s */
   long hid = ku_geti();	/* hid of CWNtuple */
   
   tntCWNtuple* tuple;

   if( !tnt->findCWNtuple(hid,tuple) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("TNTCWNTUPLE:\tEntry Count = %d \n",tuple->entryCount());
   EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/COLUMNCOUNT NTUPLE
*/
void kam_tntcwntuple_colcount_(){kam_tntcwntuple_colcount();}
int kam_tntcwntuple_colcount() {
   long npars = ku_npar();	/* no. of KUIP param.s */
   long hid = ku_geti();	/* hid of CWNtuple */
   
   tntCWNtuple* tuple;

   if( !tnt->findCWNtuple(hid,tuple) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("TNTCWNTUPLE:\tColumn Count = %d \n",tuple->columnCount());
   EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/ZEBRADIR NTUPLE
*/
void kam_tntcwntuple_zebradir_(){kam_tntcwntuple_zebradir();}
int kam_tntcwntuple_zebradir() {
   long npars = ku_npar();	/* no. of KUIP param.s */
   long hid = ku_geti();	/* hid of CWNtuple */
   
   tntCWNtuple* tuple;

   if( !tnt->findCWNtuple(hid,tuple) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("TNTCWNTUPLE:\tZebra Dir = (%s) \n",tuple->zebraDir());
   EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/GETTABLE NTUPLE TABLE
*/
void kam_tntcwntuple_gettable_(){kam_tntcwntuple_gettable();}
int kam_tntcwntuple_gettable() {
   long npars = ku_npar();	/* no. of KUIP param.s */
   long hid = ku_geti();	/* hid of CWNtuple */
   char *tname = ku_gets();	/* name of tdmTable */

   tntCWNtuple* tuple;
   tdmTable* table;

   if( !tnt->findCWNtuple(hid,tuple)
   ||  !tdm->findTable(tname,table)
   ||  !tuple->getFromTable(table)
   ){
      EML_ERROR(KAM_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/PUTTABLE NTUPLE TABLE
*/
void kam_tntcwntuple_puttable_(){kam_tntcwntuple_puttable();}
int kam_tntcwntuple_puttable() {
   long npars = ku_npar();	/* no. of KUIP param.s */
   long hid = ku_geti();	/* hid of CWNtuple */
   char *tname = ku_gets();	/* name of tdmTable */

   tntCWNtuple* tuple;
   tdmTable* table;

   if( !tnt->findCWNtuple(hid,tuple)
   ||  !tdm->findTable(tname,table)
   ||  !tuple->putToTable(table)
   ){
      EML_ERROR(KAM_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/SHOW TNTCWNTUPLE [ OPTION ]
*/
void kam_tntcwntuple_show_(){kam_tntcwntuple_show();}
int kam_tntcwntuple_show() {
   long npars = ku_npar();	/* no. of KUIP param.s */
   long hid = ku_geti();	/* hid of CWNtuple */
   
   tntCWNtuple* tuple;

   if( !tnt->findCWNtuple(hid,tuple)
   ||  !tuple->show()
   ){
      EML_ERROR(KAM_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*---------------------------------------------------------------------
** TNT/CWNTUPLE/PRINT TNTCWNTUPLE [ NROWS IFIRST ]
*/
void kam_tntcwntuple_print_(){kam_tntcwntuple_print();}
int kam_tntcwntuple_print() {
   long npars = ku_npar();	/* no. of KUIP param.s */
   long hid = ku_geti();	/* hid of CWNtuple */
   
   tntCWNtuple* tuple;

   if( !tnt->findCWNtuple(hid,tuple)
   ||  !tuple->show()
   ){
      EML_ERROR(KAM_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

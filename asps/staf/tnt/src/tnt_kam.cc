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

/*---------------------------------------------------------------------
** TNT/LIST
*/
void kam_tnt_list_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */

  STAFCV_T status = tnt_list();
}

/*---------------------------------------------------------------------
** TNT/PAW
*/
void kam_tnt_paw_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
   
  STAFCV_T status = tnt_paw();
}

/*---------------------------------------------------------------------
** TNT/SHARE
*/
void kam_tnt_share_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
   
  STAFCV_T status = tnt_share();
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

/*---------------------------------------------------------------------
** TNT/CWNTUPLE/HID NTUPLE
*/
void kam_tntcwntuple_hid_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
   
  STAFCV_T status = tntcwntuple_hid(hid);
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

/*---------------------------------------------------------------------
** TNT/CWNTUPLE/ENTRYCOUNT NTUPLE
*/
void kam_tntcwntuple_entrycount_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
   
  STAFCV_T status = tntcwntuple_entrycount(hid);
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

/*---------------------------------------------------------------------
** TNT/CWNTUPLE/ZEBRADIR NTUPLE
*/
void kam_tntcwntuple_zebradir_()
{
  long npars = ku_npar();	/* no. of KUIP param.s */
  long hid = ku_geti();	/* hid of CWNtuple */
   
  STAFCV_T status = tntcwntuple_zebradir(hid);
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

void kam_tntcwntuple_clear_()
{
  long npars = ku_npar();     /* no. of KUIP param.s */
  long hid = ku_geti();               /* hid of CWNtuple */

  STAFCV_T status = tntcwntuple_clear(hid);
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

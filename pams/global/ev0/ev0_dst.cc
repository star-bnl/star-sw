/***************************************************************************
 *
 *
 * Author: K. Turner (with lots of help from Nathan Stone
 ***************************************************************************
 *
 * Description:  fills table dst_v0_vertex from ev0_aux
 *              
 **************************************************************************/
/* ------- STAF/ROOT generated includes ------- */
#include "ev0_dst.h"


/* ------- System includes -------------- */
#include <iostream.h>

  long ev0_dst_(
    TABLE_HEAD_ST  *ev0out_h,         EV0_AUX_ST          *ev0out,
    TABLE_HEAD_ST  *dst_v0_vertex_h,  DST_V0_VERTEX_ST    *dst_v0_vertex)
{
  
  /*
   *:>--------------------------------------------------------------------
   *: ROUTINE:     ev0_dst_
   *: DESCRIPTION: write compressed version of ev0out table into dst_v0_vertex
   *:              table   - used in rereconstruction of DSTs
   *:              
   *:              
   *:>--------------------------------------------------------------------
   */

  /*  ==================  Local Variables  ======================== */
  
  int itrk=0;

  /* ===========================  Begin Executable Code  =============== */
  
  /* check headers  */
    if(( ev0out_h->nok > dst_v0_vertex_h->maxlen ) ){
	printf(" Error in ev0_dst: Error in incoming table sizes!!!\n");
	return STAFCV_BAD;
    }


  /* Initialize valid rows in  table */
  if (!dst_v0_vertex_h->nok)
    dst_v0_vertex_h->nok = ev0out_h->nok; 
  
   cout << "***ev0out_h->nok        "  << (ev0out_h->nok)  << endl;
   cout << "***dst_v0_vertex_h->nok "  << (dst_v0_vertex_h->nok)  << endl;
  
  /*loop over rows and fill values*/
  for (itrk=0; itrk<ev0out_h->nok; itrk++) 
  {
    dst_v0_vertex[itrk].id        = ev0out[itrk].id;     
    dst_v0_vertex[itrk].id_vertex = ev0out[itrk].id_vertex;
    dst_v0_vertex[itrk].idneg     = ev0out[itrk].idneg;
    dst_v0_vertex[itrk].idpos     = ev0out[itrk].idpos;
    dst_v0_vertex[itrk].dcan      = ev0out[itrk].dcan;
    dst_v0_vertex[itrk].dcap      = ev0out[itrk].dcap;
    dst_v0_vertex[itrk].dcapn     = ev0out[itrk].dcapn;
    dst_v0_vertex[itrk].dcav0     = ev0out[itrk].dcav0;
    dst_v0_vertex[itrk].neg_px    = ev0out[itrk].neg_px;
    dst_v0_vertex[itrk].neg_py    = ev0out[itrk].neg_py;
    dst_v0_vertex[itrk].neg_pz    = ev0out[itrk].neg_pz;
    dst_v0_vertex[itrk].pos_px    = ev0out[itrk].pos_px;
    dst_v0_vertex[itrk].pos_py    = ev0out[itrk].pos_py;
    dst_v0_vertex[itrk].pos_pz    = ev0out[itrk].pos_pz;
  }/* end of loop */

  return STAFCV_OK;

}  /*  End of ev0_dst  */






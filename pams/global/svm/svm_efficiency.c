
#include "svm_efficiency.h"

long type_of_call svm_efficiency_(
		TABLE_HEAD_ST *svt_spt_h, SCS_SPT_ST *svt_spt,
                TABLE_HEAD_ST *svt_track_h, STK_TRACK_ST *svt_track,
                TABLE_HEAD_ST *tpc_spt_h, TCL_TPHIT_ST *tpc_spt,
		TABLE_HEAD_ST *tpc_track_h, TPT_TRACK_ST *tpc_track,
		TABLE_HEAD_ST *tte_mctrk_h, TTE_MCTRK_ST *tte_mctrk,
                TABLE_HEAD_ST *evt_match_h, SVM_EVT_MATCH_ST *evt_match,
                TABLE_HEAD_ST *svm_effic_h, SVM_EFFIC_ST *svm_effic
              )

{
/** ROUTINE svm_efficiency_
 ** DESCRIPTION Efficiency estimator for svm -tpc to svt track matcher
 ** AUTHOR Helen Caines caines@mps.ohio-state.edu
 ** Arguments
 **
 ** IN:
 **      svt_spt - SVT space points
 **      stk_track - SVT fitted tracks
 **      tcl_tphit - TPC space points
 **      tpt_track - TPC fitted tracks
 **      tte_eval - TPC evaluation structure for tpc tracking
 **      evt_match - Matched tracks from svm
 **
 ** OUT:
 **      svm_effic - SVM effic structure
 **
 **  RETURNS STAF condition value STAFCV_OK - good
 **                               STAFCV_BAD - failure 
*/
#define tp_max 10000

  long i,idsvt,idtpc;
  long tptrack[tp_max];

/*  Set up array linking tptrack -> tptrack_mc */

  if( evt_match_h->nok <= 0) {
     printf( "No matched tracks evt_match has zero entries");
  }
  for( i=0; i<tte_mctrk_h->nok; i++){
    if( tte_mctrk[i].recid > tp_max) {
      printf( "Array tptrack not large enough too many tptracks");
      return STAFCV_BAD;
    }
    tptrack[tte_mctrk[i].recid] = tte_mctrk[i].mcid;
  }

/* Loop over matched tracks and see if mc trk matched to svt is the
*  same as mc trk matched to tpc. If yes good_match =1 else good_match =0 */
  for( i=0; i<evt_match_h->nok; i++){
    svm_effic[i].idsvt = evt_match[i].idsvt;
    svm_effic[i].idtpc = evt_match[i].idtpc;
    svm_effic[i].idsvt_mc = svt_track[svm_effic[i].idsvt-1].id_mctrack;
    svm_effic[i].idtpc_mc = tptrack[evt_match[i].idtpc];
    if( svm_effic[i].idsvt_mc == svm_effic[i].idtpc_mc){
      svm_effic[i].good_match = 1;
    }
    else {
      svm_effic[i].good_match = 0;
    }
  }

/* Resize svm_effic */
  svm_effic_h->nok=evt_match_h->nok;
  return STAFCV_OK;
}


/*:>-------------------------------------------------------------------
**: FILE:       r_tpc.cc

**: HISTORY:
**:   
**:  
**:<------------------------------------------------------------------*/
#include "PAM.h"
#include "r_tpc.h"
#include "tcl_tphit.h"
#include "tpt_track.h"
#include "FTFinder.h"
#include "TrackerFrame.h"
#include "FTF_Mc_Track.h"

extern FTFinder      tracker ;


extern "C" long r_tpc_(
  TABLE_HEAD_ST     *tphit_h,      TCL_TPHIT_ST       *tphit,        
  TABLE_HEAD_ST     *tptrack_h,    TPT_TRACK_ST       *tptrack   )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    rft_
**: DESCRIPTION: Prepares data and calls fast tracking   
**:
**: 
**: AUTHOR:     ppy - Pablo P. Yepes, yepes@physics.rice.edu  
**: ARGUMENTS:
**:       IN:
**:        tcl_tphit      - TPC Space Points
**:      OUT:
**:        tpt_track      - TPC Tracks
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

  float Todeg = 57.29577951 ;
//
//   Check there is something coming in
//
  if ( tphit_h->nok < 1 ) {
     printf ( " \n rft: tphit table is empty " ) ;
     return STAFCV_BAD ;
  }
//
//    Check min hits per track
//
  if ( tracker.para->mn_hit_trk < 3 ) {
     printf ( " \n Minimum # hits per track %d ", tracker.para->mn_hit_trk ) ;
     return STAFCV_BAD ;
  }
//
//     Allocate memory 
//  
  int max_tracks     = (int)tptrack_h->maxlen ;
  tracker.n_hits     = (int)tphit_h->nok ;
  if ( tracker.hit   != 0 ) delete []tracker.hit ;
  tracker.hit        = new FTF_Hit[tphit_h->nok] ;
  if ( tracker.track != 0 ) delete []tracker.track ;
  tracker.track      = new FTF_Track[max_tracks] ;
  tracker.max_tracks = max_tracks ;

  int i ;
  for ( i = 0 ; i < tphit_h->nok ; i++ ) {
     tracker.hit[i].id  = tphit[i].id ;
     tracker.hit[i].i_r = (short)fmod(tphit[i].row,100)   ;
     tracker.hit[i].x   = tphit[i].x ;
     tracker.hit[i].y   = tphit[i].y ;
     tracker.hit[i].z   = tphit[i].z ;
     tracker.hit[i].dx  = tphit[i].dx ;
     tracker.hit[i].dy  = tphit[i].dy ;
     tracker.hit[i].dz  = tphit[i].dz ;
  }
//
//  Call tracker
//
  tracker.FTF ( ) ;
//
//    Move info to tpt tracks
//
 tptrack_h->nok = tracker.n_tracks ;

 for ( i = 0 ; i < tracker.n_tracks ; i++ ) {
    tptrack[i].id       = i + 1  ; 
    tptrack[i].flag     = 1                        ; 
    tptrack[i].nrec     = 
    tptrack[i].nfit     = tracker.track[i].n_hits  ; 
    tptrack[i].q        = tracker.track[i].q       ; 
    tptrack[i].chisq[0] = tracker.track[i].chi2[0] ; 
    tptrack[i].chisq[1] = tracker.track[i].chi2[1] ; 
    tptrack[i].invp     = 1. / tracker.track[i].pt ; 
    tptrack[i].phi0     = tracker.track[i].phi0 * Todeg ; 
    tptrack[i].psi      = tracker.track[i].psi  * Todeg ; 
    tptrack[i].r0       = tracker.track[i].r0      ; 
    tptrack[i].tanl     = tracker.track[i].tanl    ; 
    tptrack[i].z0       = tracker.track[i].z0      ; 
    tptrack[i].hitid    = 0      ; 
    tptrack[i].dedx[0]  = tracker.track[i].dedx ;
    tptrack[i].dedx[1]  = 0.F ;
 }
//
//    Move hit assignment info
//
   for ( i = 0 ; i < tphit_h->nok ; i++ ){
     if ( tracker.hit[i].track != 0 )
        tphit[i].track = 1000 * ( tracker.hit[i].track->id + 1 ) ;
     else 
        tphit[i].track = 0 ;
   }

  return STAFCV_OK;
}

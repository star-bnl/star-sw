/*:>--------------------------------------------------------------------
**: FILE:      
**: HISTORY: Dec 9, 1996 First version 
**:   
**:  
**:<------------------------------------------------------------------*/
#include "PAM.h"
#include "r_ftpc.h"
#include "r_ftpc_ctrl.h"
#include "fcl_fppoint.h"
#include "tpt_track.h"
#include "FTFinder.h"
#include "TrackerFrame.h"
#include "FTF_Mc_Track.h"

extern FTFinder      tracker ;


extern "C" long r_ftpc_(
  TABLE_HEAD_ST     *ctrl_h,       R_FTPC_CTRL_ST     *ctrl,        
  TABLE_HEAD_ST     *point_h,      FCL_FPPOINT_ST     *point,        
  TABLE_HEAD_ST     *fptrack_h,    TPT_TRACK_ST       *fptrack   )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    r_ftpc_
**: DESCRIPTION: Prepares data and calls fast tracking   
**:
**: 
**: AUTHOR:     ppy - Pablo P. Yepes, yepes@physics.rice.edu  
**: ARGUMENTS:
**:       IN:
**:        ctrl           - Controls module  
**:        ftp_point      - FTPC Space Points
**:      OUT:
**:        tpf_track      - FTPC Tracks
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
//
  float Todeg = 57.29577951 ;
//
//   Check there is something coming in
//
  if ( ctrl_h->nok < 1 ) {
     printf ( " \n r_ftpc: Control table is empty " ) ;
     return STAFCV_BAD ;
  }
//
//   Check there is something coming in
//
  if ( point_h->nok < 1 ) {
     printf ( " \n r_tpc: Point table is empty " ) ;
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
  int max_tracks     = (int)fptrack_h->maxlen ;
  int max_mc_tracks  = 10000 ;
  if ( tracker.hit   != 0 ) delete []tracker.hit ;
  tracker.hit        = new FTF_Hit[point_h->nok] ;
  if ( tracker.track != 0 ) delete []tracker.track ;
  tracker.max_tracks = max_tracks ;
  tracker.track      = new FTF_Track[max_tracks] ;
  tracker.mc_track   = new FTF_Mc_Track[max_mc_tracks] ;

  int i, j, k  ;
  int i_mc = 0 ;
  int max_index = 1000000 ;
//
//  Temporary index
//
  int *index = new int[max_index] ;
  for ( i = 0 ; i< max_index ; i++ ) index[i] = 0 ;
//
  short which ;
  for ( i = 0, j = -1  ; i < point_h->nok ; i++ ) {
//
//    Check whether hit belongs to selected FTPC
//
     which = point[i].row/100 ;
     if ( which != ctrl[0].which_ftpc ) continue ;
//
     j++ ;
     tracker.hit[j].id       = point[i].id ;
     tracker.hit[j].i_r      = (short)fmod(point[i].row,100)   ;
     tracker.hit[j].x        = point[i].x ;
     tracker.hit[j].y        = point[i].y ;
     tracker.hit[j].z        = point[i].z ;
//     tracker.hit[j].dx     = point[i].dx ;
//     tracker.hit[j].dy     = point[i].dy ;
//     tracker.hit[j].dz     = point[i].dz ;
     tracker.hit[j].dx       = 0.05 ;
     tracker.hit[j].dy       = 0.05 ;
     tracker.hit[j].dz       = 0.05 ;
     tracker.hit[j].nxmhit   = 0    ;
     k                       = point[i].ge_track_p ;

     if ( k < 0 || k >= max_index ) {
        printf ( "\n r_ftpc: GEANT index %d too large ",k ) ;
        continue ;
     } 
//
     if ( index[k] == 0 ) {
        i_mc++ ;
        if ( i_mc > max_mc_tracks ) {
           printf ( " \n r_ftpc: Max number of MC tracks reached " ) ; 
           continue ;
        }
        index[k] = i_mc ;
        tracker.mc_track[i_mc].first_hit =
        tracker.mc_track[i_mc].last_hit = &(tracker.hit[j]) ;
     }
     else {
       i_mc = index[k] ;
       tracker.mc_track[i_mc].last_hit->nxmhit = &(tracker.hit[j]) ; 
       tracker.mc_track[i_mc].last_hit         = &(tracker.hit[j]) ;
     }
     tracker.mc_track[i_mc].pid  = point[i].ge_pid ;
     tracker.mc_track[i_mc].p[0] = point[i].px_g;
     tracker.mc_track[i_mc].p[1] = point[i].py_g;
     tracker.mc_track[i_mc].p[2] = point[i].pz_g;
     tracker.mc_track[i_mc].pt   = sqrt ( point[i].px_g*point[i].px_g+
                                          point[i].py_g*point[i].py_g );
     tracker.hit[j].mc_track = index[k] ;
  } 
  tracker.n_hits        = j + 1  ;
  tracker.n_mc_tracks   = i_mc + 1 ;
  delete []index ;
//
//  Call tracker
//
  tracker.FTF ( ) ;
//
//    Move info to tpt tracks
//
 fptrack_h->nok = tracker.n_tracks ;

 for ( i = 0 ; i < tracker.n_tracks ; i++ ) {
    fptrack[i].id       = tracker.track[i].id + 1  ; 
    fptrack[i].flag     = tracker.track[i].flag    ; 
    fptrack[i].nrec     = 
    fptrack[i].nfit     = tracker.track[i].n_hits  ; 
    fptrack[i].q        = tracker.track[i].q       ; 
    fptrack[i].chisq[0] = tracker.track[i].chi2[0] ; 
    fptrack[i].chisq[1] = tracker.track[i].chi2[1] ; 
    fptrack[i].invp     = 1. / tracker.track[i].pt ; 
    fptrack[i].phi0     = tracker.track[i].phi0 * Todeg ; 
    fptrack[i].psi      = tracker.track[i].psi  * Todeg ; 
    fptrack[i].r0       = tracker.track[i].r0      ; 
    fptrack[i].tanl     = tracker.track[i].tanl    ; 
    fptrack[i].z0       = tracker.track[i].z0      ; 
    fptrack[i].hitid    = 0      ; 
    fptrack[i].dedx[0]  = tracker.track[i].dedx ;
    fptrack[i].dedx[1]  = 0.F ;
 }
//
//    Move hit assignment info
//
/*   for ( i = 0 ; i < point_h->nok ; i++ ){
     if ( tracker.hit[i].track != 0 )
        point[i].track = 1000 * ( tracker.hit[i].track->id + 1 ) ;
     else 
        point[i].track = 0 ;
   }
*/

  return STAFCV_OK;
}

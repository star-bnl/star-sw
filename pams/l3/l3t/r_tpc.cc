/*:>-------------------------------------------------------------------
**: FILE:       r_tpc.cc

**: HISTORY:
**:   
**:  
**:<------------------------------------------------------------------*/
#include "PAM.h"
#include "r_tpc.h"
#include "TTracker.hpp"
#include "TrackFrame.hpp"
#include "_memory.hpp"



extern "C" long r_tpc_(
  TABLE_HEAD_ST     *para_h,       L3T_TPC_PARA_ST    *para,
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
**:    INOUT:
**:        l3t_tpc_para   - Level 3 TPC tracking parameters
**:       IN:
**:        tcl_tphit      - TPC Space Points
**:      OUT:
**:        tpt_track      - TPC Tracks
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

  memory_manager_init init_my_memory;
  TTrackFrame TrackFrame;
  TTrackerFFT Tracker;

  // setup tracker
  TrackFrame.SetTracker((VTracker*)&Tracker);
  TrackFrame.Init();

//
//   Check para table
//
  if ( para_h->nok < 1 ) {
     printf ( " \n rft: empty para table " ) ;
     return STAFCV_BAD ;
  }
//
//   Check there is something coming in
//
  if ( tphit_h->nok < 1 ) {
     printf ( " \n rft: tphit table is empty " ) ;
     return STAFCV_BAD ;
  }
//
// setup tracking
//
  printf ( " \n rft: initializing " ) ;
  TrackFrame.Initialize(para);
//
// fill in hits (divided into s4 sectors)
//
  printf ( " \n rft: filling hit structures " ) ;
  TrackFrame.FillEvent(tphit_h, tphit, para->FirstSector, para->LastSector);
//
// process event
//
  printf ( " \n rft: processing event...\n " ) ;
  TrackFrame.ProcessEvent();
//
// fill result into table
//
  printf ( " \n rft: filling result tables " ) ;
  TrackFrame.FillResultTable(tphit_h, tphit, tptrack_h, tptrack);
//
// clear memory...
//
  printf ( " \n rft: exiting \n" ) ;
  TrackFrame.Done();

  return STAFCV_OK;
}

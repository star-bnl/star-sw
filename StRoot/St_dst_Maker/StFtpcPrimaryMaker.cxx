// $Id: StFtpcPrimaryMaker.cxx,v 1.10 2002/08/02 11:22:34 oldi Exp $
// $Log: StFtpcPrimaryMaker.cxx,v $
// Revision 1.10  2002/08/02 11:22:34  oldi
// MaxDCA is taken from StFtpcTrackingParams, now (it was hardcoded before).
//
// Revision 1.9  2002/04/05 16:52:51  oldi
// Minor changes:
// Global refit was removed, because TPC vertex is known at tracking time already.
// Chi2 calculation was fixed.
//
// Revision 1.8  2001/12/12 16:35:22  jcs
// increase max_Dca from 1. to 2. for refit to primary vertex
//
// Revision 1.7  2001/05/29 12:20:33  jcs
// increase n_fit_point by 1 for FTPC primary tracks to include vertex point
//
// Revision 1.6  2001/05/08 22:01:31  jcs
// set dst_track.iflag x=8 for FTPC primary tracks
//
// Revision 1.5  2001/05/03 15:02:57  jcs
// change number of refitter arguments
//
// Revision 1.4  2001/04/25 17:23:50  perev
// HPcorrs
//
// Revision 1.3  2001/03/30 13:30:12  jcs
// correct Id and Log
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcPrimaryMaker class                                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include "TMath.h"
#include "StFtpcPrimaryMaker.h"

#include "math_constants.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StVertexId.h"
#include "StThreeVectorD.hh"
#include "StHelixD.hh"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "StFtpcTrackMaker/StFtpcVertex.hh"
#include "StFtpcTrackMaker/StFtpcTracker.hh"
#include "StFtpcTrackMaker/StFtpcTrackingParams.hh"

#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_vertex_Table.h"

#include "math_constants.h"

ClassImp(StFtpcPrimaryMaker)
  
//_____________________________________________________________________________
  StFtpcPrimaryMaker::StFtpcPrimaryMaker(const char *name):StMaker(name){
}
//_____________________________________________________________________________
  StFtpcPrimaryMaker::~StFtpcPrimaryMaker(){
}
//_____________________________________________________________________________
Int_t StFtpcPrimaryMaker::Init(){
  // Create tables
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFtpcPrimaryMaker::Make(){
  PrintInfo();  
  
  St_DataSet *primary = GetDataSet("primary"); 
  if (!primary) {
     gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): primary is missing" << endm;
     return kStWarn;
  }

   St_dst_vertex *vertex = (St_dst_vertex *) primary->Find("vertex");
   if (!vertex) {
     gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): vertex is missing" << endm;
     return kStWarn;
   }

    dst_vertex_st *primvtx = vertex->GetTable();

 if( primvtx->vtx_id != kEventVtxId || primvtx->iflag != 1){
    for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,primvtx++){
      if( primvtx->vtx_id == kEventVtxId && primvtx->iflag == 1 ) break;
    }
  }
 if( primvtx->vtx_id != kEventVtxId || primvtx->iflag != 1){
     gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): primary vertex is missing" << endm;
     return kStWarn;
   }

  St_DataSet *match = GetDataSet("match");
  if (!match) {
     gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): match is missing" << endm;
     return kStWarn;
  }

  St_dst_track *globtrk = (St_dst_track *) match->Find("globtrk");
  if (!globtrk) {
     gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): globtrk is missing" << endm;
     return kStWarn;
  }

    St_DataSet *track_data = GetDataSet("ftpc_tracks");
    if (!track_data) {
       gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): ftpc_tracks is missing" << endm;
     return kStWarn;
  }
    St_fpt_fptrack *tracks = (St_fpt_fptrack *)track_data->Find("fpt_fptrack");
    if (!tracks) {
       gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): tracks is missing" << endm;
       return kStWarn;
    }

    St_DataSet *hit_data = GetDataSet("ftpc_hits");
    if (!hit_data) {
       gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): ftpc_hits is missing" << endm;
     return kStWarn;
  }
    St_fcl_fppoint *points = (St_fcl_fppoint *)hit_data->Find("fcl_fppoint");
    if (!points) {
       gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): points is missing" << endm;
       return kStWarn;
    }

// Refit FTPC tracks with primary vertex
    StFtpcTrackingParams *params = StFtpcTrackingParams::Instance();  
    StFtpcVertex *refit_vertex = new StFtpcVertex(primvtx);
    Bool_t bench = (Bool_t)false;
    StFtpcTracker *refitter = new StFtpcTracker(refit_vertex, points, tracks, bench, params->MaxDca(0));
    refitter->FitAndWrite(tracks, primvtx->id);
    delete refitter;
    delete refit_vertex;

// Increase size or create  primtrk to hold all FTPC primary tracks

   Int_t No_of_Tracks = 0;
   No_of_Tracks += tracks->GetNRows();
   
   St_dst_track *primtrk=0;
   Int_t No_of_primary_tracks = 0;
   if (primary) {
      primtrk = (St_dst_track *) primary->Find("primtrk");
   }
   if (primtrk) {
      No_of_primary_tracks = primtrk->GetNRows();
      primtrk->ReAllocate(primtrk->GetNRows() + No_of_Tracks);
   }
   if (!primtrk) {
      primtrk     = new St_dst_track("primtrk", No_of_Tracks);
      AddData(primtrk);
   }

// Create pointers to all FTPC track tables

   fpt_fptrack_st *trk = tracks->GetTable();
   dst_track_st *ptrk = primtrk->GetTable();
   ptrk = ptrk + No_of_primary_tracks;
   dst_track_st *gtrk = globtrk->GetTable();
  
// Loop over FTPC tracks and store all primary tracks in primtrk

 Int_t nrows=0;
 Int_t iglobtrk=0;
 for( Int_t no_rows=0; no_rows<No_of_Tracks; no_rows++,trk++){

  if ( trk->flag == 1) { 

    ptrk->r0    = sqrt(trk->v[0]*trk->v[0] + trk->v[1]*trk->v[1]);
    ptrk->phi0  = atan2(trk->v[1],trk->v[0]) * C_DEG_PER_RAD;
    ptrk->z0    = trk->v[2];
    ptrk->psi   = atan2(trk->p[1],trk->p[0]);
    if ( ptrk->psi < 0.0 ) {
       ptrk->psi = ptrk->psi + C_2PI;
    }
    ptrk->psi = ptrk->psi * C_DEG_PER_RAD;
    ptrk->invpt = 1./sqrt(trk->p[0]*trk->p[0]+trk->p[1]*trk->p[1]);
    ptrk->tanl  = trk->p[2] * ptrk->invpt;
    ptrk->curvature = trk->curvature;

    Int_t i = 0;
    for (i=0; i<15; i++) { ptrk->covar[i] = 0; }
    
    iglobtrk=trk->id_globtrk-1;

    for (i=0; i<3; i++) {
       ptrk->x_first[i] = gtrk[iglobtrk].x_first[i];
       ptrk->x_last[i] = gtrk[iglobtrk].x_last[i];
    }

    ptrk->length    = trk->length;
    ptrk->impact    = trk->impact;
        
    ptrk->map[0]    = gtrk[iglobtrk].map[0] + 1 ;
    ptrk->map[1]    = gtrk[iglobtrk].map[1];

    ptrk->id        =  trk->id_globtrk;
    ptrk->iflag     =  800 + trk->flag;
    ptrk->det_id    = gtrk[iglobtrk].det_id;
    ptrk->method    = gtrk[iglobtrk].method;
    ptrk->pid       = gtrk[iglobtrk].pid;
   
    ptrk->n_point     = gtrk[iglobtrk].n_point;
    ptrk->n_max_point = gtrk[iglobtrk].n_max_point;
    ptrk->n_fit_point = gtrk[iglobtrk].n_fit_point + 1;

    ptrk->chisq[0]  = trk->chisq[0]/(ptrk->n_fit_point-3);
    ptrk->chisq[1]  = trk->chisq[1]/(ptrk->n_fit_point-2);

    ptrk->icharge  = trk->q;
    ptrk->id_start_vertex =  10*trk->id_start_vertex;

    ptrk++;
    nrows++;
  }
}
 
  primtrk->SetNRows(No_of_primary_tracks + nrows);

  return kStOK;
}
//_____________________________________________________________________________



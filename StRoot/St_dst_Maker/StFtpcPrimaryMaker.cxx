// $Id: StFtpcPrimaryMaker.cxx,v 1.17 2004/02/12 18:38:21 oldi Exp $
// $Log: StFtpcPrimaryMaker.cxx,v $
// Revision 1.17  2004/02/12 18:38:21  oldi
// Removal of intermediate tables to store FTPC hits and tracks.
// Now the TObjArray's of hits and tracks are passed directly to
// StFtpcGlobalMaker.cxx and StFtpcPrimaryMaker.cxx where they are (still)
// copied into the dst tables.
//
// Revision 1.16  2003/12/04 14:49:30  jcs
// activate Markus' code to fill values at outermost point on ftpc tracks
//
// Revision 1.15  2003/09/02 17:59:26  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.14  2002/11/28 09:42:10  oldi
// Code was prepared to fill momentum values at outermost points on tracks.
// This feature is not used up to now.
//
// Revision 1.13  2002/11/06 13:48:29  oldi
// Vertex handling simplifed.
// Global/primary fit handling simplified.
//
// Revision 1.12  2002/10/11 15:47:35  oldi
// Code cleanup (several lines of code changed due to *params -> Instance()).
//
// Revision 1.11  2002/10/03 10:34:24  oldi
// Usage of gufld removed.
// Magnetic field is read by StMagUtilities, now.
//
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

#include <Stiostream.h>
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
#include "StFtpcTrackMaker/StFtpcTrack.hh"
#include "StFtpcTrackMaker/StFtpcPoint.hh"
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

  TObjectSet* objSetClusters = (TObjectSet*)GetDataSet("ftpcClusters");
  TObjArray *ftpcHits = (TObjArray*)objSetClusters->GetObject();
  if (!ftpcHits) {
    gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): TObjArray of ftpc hits is missing" << endm;
    return kStWarn;
  }

  TObjectSet* objSetTracks = (TObjectSet*)GetDataSet("ftpcTracks");
  if (!objSetTracks) { // this had to be introduced since in cases of a bad vertex there's not even an empty TObjArray written 
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): TObjectSet of ftpc tracks is missing" << endm;
    return kStWarn;
  }
  TObjArray *ftpcTracks = (TObjArray*)objSetTracks->GetObject();
  if (!ftpcTracks) {
    gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): TObjArray of ftpc tracks is missing" << endm;
    return kStWarn;
  }

  // assume found primary vertex earlier already
  TObjectSet* objSetVertex = (TObjectSet*)GetDataSet("ftpcVertex");
  StFtpcVertex *ftpcVertex = (StFtpcVertex*)objSetVertex->GetObject();
  if (!ftpcVertex) {
    gMessMgr->Warning() << "StFtpcPrimaryMaker::Make(): StFtpcVertex is missing" << endm;
    return kStWarn;
  }
  
  // Refit FTPC tracks with primary vertex
  Bool_t bench = (Bool_t)false;
  StFtpcTracker *refitter = new StFtpcTracker(ftpcVertex, ftpcHits, ftpcTracks, bench, 
					      StFtpcTrackingParams::Instance()->MaxDca(0));
  refitter->PrimaryFit();
  delete refitter;

  // Increase size or create primtrk to hold all FTPC primary tracks
  
  Int_t No_of_Tracks = ftpcTracks->GetEntriesFast();
  
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

  dst_track_st *ptrk = primtrk->GetTable();
  ptrk = ptrk + No_of_primary_tracks;
  dst_track_st *gtrk = globtrk->GetTable();
  
  // Loop over FTPC tracks and store all primary tracks in primtrk
  
  Int_t iglobtrk=0;
  Int_t itrk = 0;
  for (itrk=0; itrk<ftpcTracks->GetEntriesFast(); itrk++) {
    
    StFtpcTrack *track = (StFtpcTrack*)ftpcTracks->At(itrk);

    if (track->ComesFromMainVertex()) { 
      
      ptrk->r0    = ::sqrt(track->GetFirstPointOnTrack().X()*track->GetFirstPointOnTrack().X() 
			   + track->GetFirstPointOnTrack().Y()*track->GetFirstPointOnTrack().Y());
      ptrk->phi0  = atan2(track->GetFirstPointOnTrack().Y(),track->GetFirstPointOnTrack().X()) * C_DEG_PER_RAD;
      ptrk->z0    = track->GetFirstPointOnTrack().Z();
      ptrk->psi   = atan2(track->GetPy(),track->GetPx());
      if ( ptrk->psi < 0.0 ) {
	ptrk->psi = ptrk->psi + C_2PI;
      }
      ptrk->psi = ptrk->psi * C_DEG_PER_RAD;
      ptrk->invpt = 1./track->GetPt();
      ptrk->tanl  = track->GetPz() * ptrk->invpt;
      ptrk->curvature = track->curvature();


      Int_t i = 0;
      for (i=0; i<15; i++) { ptrk->covar[i] = 0; }
      
      iglobtrk = track->GetGlobalTrackId()-1;
      
      for (i=0; i<3; i++) {
	ptrk->x_first[i] = gtrk[iglobtrk].x_first[i];
	ptrk->x_last[i] = gtrk[iglobtrk].x_last[i];
      }

      ptrk->r0out   = 
	::sqrt(track->GetLastPointOnTrack().X()*track->GetLastPointOnTrack().X()
	       + track->GetLastPointOnTrack().Y()*track->GetLastPointOnTrack().Y());
      ptrk->phi0out = 
	atan2(track->GetLastPointOnTrack().Y(),track->GetLastPointOnTrack().X())
	* C_DEG_PER_RAD;
      ptrk->z0out = track->GetLastPointOnTrack().Z();

      // For Kalman fitting 'inner' and 'outer' momenta differ.
      // For FTPC fitting they are the same,
      // so fill with the 'inner' values.
      ptrk->psiout   = atan2(track->GetPy(), track->GetPx());
      if ( ptrk->psiout < 0.0 ) {
	ptrk->psiout = ptrk->psiout + C_2PI;
      }
      ptrk->psiout = ptrk->psiout * C_DEG_PER_RAD;
      ptrk->invptout = 1./track->GetPt();
      ptrk->tanlout  = track->GetPz() * ptrk->invptout;
      
      ptrk->length    = track->GetTrackLength();
      ptrk->impact    = track->GetDca();
      
      ptrk->map[0]    = gtrk[iglobtrk].map[0] + 1 ;
      ptrk->map[1]    = gtrk[iglobtrk].map[1];
      
      ptrk->id        = track->GetGlobalTrackId();
      ptrk->iflag     = 800 + 1; // this is a primary track
      ptrk->det_id    = gtrk[iglobtrk].det_id;
      ptrk->method    = gtrk[iglobtrk].method;
      ptrk->pid       = gtrk[iglobtrk].pid;
      
      ptrk->n_point     = gtrk[iglobtrk].n_point;
      ptrk->n_max_point = gtrk[iglobtrk].n_max_point;
      ptrk->n_fit_point = gtrk[iglobtrk].n_fit_point + 1;
      
      ptrk->chisq[0]  = track->GetChiSq()[0]/(ptrk->n_fit_point-3);
      ptrk->chisq[1]  = track->GetChiSq()[1]/(ptrk->n_fit_point-2);
      
      ptrk->icharge  = track->GetCharge();
      ptrk->id_start_vertex =  10*ftpcVertex->GetId();
      
      ptrk++;
    }
  }
  
  primtrk->SetNRows(No_of_primary_tracks + itrk);

  gMessMgr->Info() << "StFtpcPrimaryMaker: " << itrk << " primary Ftpc tracks written to DST tables." << endm;
  
  return kStOK;
}
//_____________________________________________________________________________



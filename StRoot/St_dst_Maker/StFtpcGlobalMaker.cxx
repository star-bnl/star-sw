// $Id: StFtpcGlobalMaker.cxx,v 1.18 2004/03/02 15:56:41 jcs Exp $
// $Log: StFtpcGlobalMaker.cxx,v $
// Revision 1.18  2004/03/02 15:56:41  jcs
// Fill dst_mon-soft_ftpc table
//
// Revision 1.17  2004/02/13 21:13:13  oldi
// Protection against missing FTPC DAQ data added.
//
// Revision 1.16  2004/02/12 18:38:21  oldi
// Removal of intermediate tables to store FTPC hits and tracks.
// Now the TObjArray's of hits and tracks are passed directly to
// StFtpcGlobalMaker.cxx and StFtpcPrimaryMaker.cxx where they are (still)
// copied into the dst tables.
//
// Revision 1.15  2003/12/04 14:49:30  jcs
// activate Markus' code to fill values at outermost point on ftpc tracks
//
// Revision 1.14  2003/09/02 17:59:25  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.13  2002/11/28 10:08:12  jcs
// simplify id_start_vertex comment and code
//
// Revision 1.12  2002/11/28 09:42:05  oldi
// Code was prepared to fill momentum values at outermost points on tracks.
// This feature is not used up to now.
//
// Revision 1.11  2002/11/25 12:06:29  jcs
// set bit 0 of map[0]=0 for all ftpc global tracks (unconstrained fit)
//
// Revision 1.10  2002/11/06 13:48:25  oldi
// Vertex handling simplifed.
// Global/primary fit handling simplified.
//
// Revision 1.9  2002/10/31 13:43:24  oldi
// dE/dx parameters read from database, now.
// Code cleanup (indention).
//
// Revision 1.8  2002/10/29 15:57:56  jcs
// inactivate all code necessary for redoing track fit with primary vertex
//
// Revision 1.7  2002/10/11 15:47:33  oldi
// Code cleanup (several lines of code changed due to *params -> Instance()).
//
// Revision 1.6  2002/08/02 11:22:31  oldi
// MaxDCA is taken from StFtpcTrackingParams, now (it was hardcoded before).
//
// Revision 1.5  2002/04/05 16:52:47  oldi
// Minor changes:
// Global refit was removed, because TPC vertex is known at tracking time already.
// Chi2 calculation was fixed.
//
// Revision 1.4  2002/02/01 01:59:25  jcs
// redo unconstrained fit for FTPC global tracks with primary vertex
// (done in StFtpcTrackMaker with preVertex)
//
// Revision 1.3  2002/01/30 15:14:02  jcs
// incorporate fill_ftpc_dst.cc
// write out all FTPC hits, not just those on tracks
//
// Revision 1.2  2001/03/30 13:30:11  jcs
// correct Id and Log
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcGlobalMaker class                                              //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include "TMath.h"
#include "StFtpcGlobalMaker.h"
#include "StFtpcTrackMaker/StFtpcVertex.hh"
#include "StFtpcTrackMaker/StFtpcTracker.hh"
#include "StFtpcTrackMaker/StFtpcTrack.hh"
#include "StFtpcTrackMaker/StFtpcPoint.hh"
#include "StFtpcTrackMaker/StFtpcTrackingParams.hh"

#include "tables/St_dst_mon_soft_ftpc_Table.h"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "StDetectorId.h"
#include "StVertexId.h"
#include "StTrackMethod.h"
#include "StDedxMethod.h"
#include "math_constants.h"

ClassImp(StFtpcGlobalMaker)
  
//_____________________________________________________________________________
StFtpcGlobalMaker::StFtpcGlobalMaker(const char *name):StMaker(name){
}

//_____________________________________________________________________________
StFtpcGlobalMaker::~StFtpcGlobalMaker(){
}

//_____________________________________________________________________________
Int_t StFtpcGlobalMaker::Init(){
  // Create tables
  
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StFtpcGlobalMaker::Make(){
  PrintInfo();  
  int iMake = kStOK;
  Int_t iftpc;

#ifdef REFIT_FTPC_TRACKS
  // if FTPC tracking is done before the primary vertex is found, 
  // i.e. if fpt is in ftpcChain instead of in globalChain,
  // the  tracks must be refit 
  St_DataSet *primary = GetDataSet("primary");
  if (!primary) {
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): primary is missing." << endm;
    return kStWarn;
  }

  St_dst_vertex *vertex = (St_dst_vertex *) primary->Find("vertex");
  if (!vertex) {
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): vertex is missing." << endm;
    return kStWarn;
  }

  dst_vertex_st *primvtx = vertex->GetTable();

  if( primvtx->vtx_id != kEventVtxId || primvtx->iflag != 1){
    for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,primvtx++){
      if( primvtx->vtx_id == kEventVtxId && primvtx->iflag == 1 ) break;
    }
  }
  if( primvtx->vtx_id != kEventVtxId || primvtx->iflag != 1){
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): primary vertex is missing." << endm;
    return kStWarn;
  }
#endif

  TObjectSet* objSetClusters = (TObjectSet*)GetDataSet("ftpcClusters");
  if (!objSetClusters) {
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): TObjectSet of ftpc clusters is missing." << endm;
    return kStWarn;
  }
  TObjArray *ftpcHits = (TObjArray*)objSetClusters->GetObject();
  if (!ftpcHits) {
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): TObjArray of ftpc hits is missing." << endm;
    return kStWarn;
  }

  TObjectSet* objSetTracks = (TObjectSet*)GetDataSet("ftpcTracks");
  if (!objSetTracks) {
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): TObjectSet of ftpc tracks is missing." << endm;
    return kStWarn;
  }
  TObjArray *ftpcTracks = (TObjArray*)objSetTracks->GetObject();
  if (!ftpcTracks) {
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): TObjArray of ftpc tracks is missing." << endm;
    return kStWarn;
  }

  TObjectSet* objSetVertex = (TObjectSet*)GetDataSet("ftpcVertex");
  if (!objSetVertex) {
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): TObjectSet of ftpc vertex is missing." << endm;
    return kStWarn;
  }
  StFtpcVertex *ftpcVertex = (StFtpcVertex*)objSetVertex->GetObject();
  if (!ftpcVertex) {
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): StFtpcVertex is missing." << endm;
    return kStWarn;
  }
  
  Int_t iglobtrk = 0;
  Int_t nfptrack = ftpcTracks->GetEntriesFast();
  Int_t nfppoint = ftpcHits->GetEntriesFast();
  St_dst_track *dst_track=0;
  St_DataSet *match = GetDataSet("match");
  if (match) {
    dst_track = (St_dst_track *) match->Find("globtrk");
    if (dst_track) {
      iglobtrk = dst_track->GetNRows();
      dst_track->ReAllocate(iglobtrk + nfptrack);
    }
  }
  if (!dst_track) {
    dst_track = new St_dst_track("globtrk", nfptrack); 
    AddData(dst_track);
  }
  dst_track_st *globtrk = dst_track->GetTable();

  St_dst_point *dst_point = new St_dst_point("point",nfppoint);  
  AddData(dst_point);
  dst_point_st *point = dst_point->GetTable();

  St_dst_mon_soft_ftpc *dst_mon_soft_ftpc = new St_dst_mon_soft_ftpc("mon_soft_ftpc",1);
  AddData(dst_mon_soft_ftpc);
  dst_mon_soft_ftpc->SetNRows(1);
  // Initialize dst_mon_soft_ftpc table
  // mon_soft_ftpc[1].n_clus_ftpc[iftpc] is not used
  dst_mon_soft_ftpc_st *mon_soft_ftpc = dst_mon_soft_ftpc->GetTable();
  for (iftpc=0;iftpc<2;iftpc++) {
    mon_soft_ftpc[0].n_clus_ftpc[iftpc] = 0;	  
    mon_soft_ftpc[0].n_pts_ftpc[iftpc] = 0;   
    mon_soft_ftpc[0].n_trk_ftpc[iftpc] = 0;
    mon_soft_ftpc[0].chrg_ftpc_tot[iftpc] = 0.;   
    mon_soft_ftpc[0].hit_frac_ftpc[iftpc] = 0.; 
    mon_soft_ftpc[0].avg_trkL_ftpc[iftpc] = 0.;
    mon_soft_ftpc[0].res_pad_ftpc[iftpc] = 0.;   
    mon_soft_ftpc[0].res_drf_ftpc[iftpc] = 0.;
  }  

  St_dst_dedx *dst_dedx = new St_dst_dedx("dst_dedx",nfptrack); 
  AddData(dst_dedx);
  dst_dedx_st *dedx = dst_dedx->GetTable();
  Int_t idedx = 0;
  St_DataSet *ftpcpars = GetInputDB("ftpc");
  assert(ftpcpars);
  St_DataSetIter gime(ftpcpars);
  m_fdepar = (St_fde_fdepar *) gime("fdepars/fdepar");
  fde_fdepar_st *fdepar = m_fdepar->GetTable();

#ifdef REFIT_FTPC_TRACKS
  // Redo unconstrained fit with primary vertex instead of preVertex
  StFtpcVertex *refit_vertex = new StFtpcVertex(primvtx);
  gMessMgr->Info() << "Using primary vertex: "<< *refit_vertex << endm;
  Bool_t bench = (Bool_t)false;
  StFtpcTracker *refitter = new StFtpcTracker(refit_vertex, ftpcHits, ftpcTracks, bench, 
					      StFtpcTrackingParams::Instance()->MaxDca(0));
  refitter->GlobalFitAnddEdx();
  ftpcVertex = refit_vertex;
  delete refitter;
  delete refit_vertex;
#endif

  Int_t ihit, iPoint, itrk;

  // Loop over all tracks in FTPC track table
  for (itrk=0; itrk<ftpcTracks->GetEntriesFast(); itrk++, iglobtrk++, idedx++) {

    StFtpcTrack* track = (StFtpcTrack*)ftpcTracks->At(itrk);
    
    //  Primary key
    globtrk[iglobtrk].id      = iglobtrk + 1;
    track->SetGlobalTrackId(globtrk[iglobtrk].id); 

    //  initialize map (=0 for global tracks = unconstrained fit)
    globtrk[iglobtrk].map[0] = 0;
    
    //  initialize map[1] = Format interpreter -  set bit 31 for FTPC
    globtrk[iglobtrk].map[1]   =  (1<<31);

    //  initialize det_id 
    globtrk[iglobtrk].det_id   = 0;

    //  Loop over all hits on track 
    // Note that hits are counted from inside to outside => inner = last, outer = last
    for (ihit=track->GetHits()->GetEntriesFast()-1; ihit>=0; ihit--) {
      StFtpcPoint *point = (StFtpcPoint*)track->GetHits()->At(ihit);
	
      if (globtrk[iglobtrk].det_id == 0 ) { 
	//                 Save first hit on current track and determine detector id
	globtrk[iglobtrk].x_first[0]    = point->GetX();
	globtrk[iglobtrk].x_first[1]    = point->GetY();
	globtrk[iglobtrk].x_first[2]    = point->GetZ();

	globtrk[iglobtrk].det_id  = point->GetDetectorId();
      }
      
      globtrk[iglobtrk].map[0] |= (1<<point->GetPadRow());
      
      point->SetPadRow(point->GetPadRow() + 100*globtrk[iglobtrk].id);
      // end of processing current hit
    }  // end of processing all hits on track
    
      if (globtrk[iglobtrk].det_id == 5 ) iftpc = 0;
      if (globtrk[iglobtrk].det_id == 4 ) iftpc = 1;
      mon_soft_ftpc[0].n_trk_ftpc[iftpc]++;

    
    //  Track finding and track fitting method 
    //   (Method: FTPC Conformal Mapping - set bit 10 )          
    //   (Fitter: kHelix2StepId)
    globtrk[iglobtrk].method = (1<<10) + (1<<kHelix2StepId);

    //  Geant particle ID number for mass hypothesis used in tracking
    //   (Currently not set for FTPC)                               
    globtrk[iglobtrk].pid = 0;

    //  Number of points 
    globtrk[iglobtrk].n_point = track->GetHits()->GetEntriesFast();

    //  Number of points used in fit
    globtrk[iglobtrk].n_fit_point  = track->GetHits()->GetEntriesFast();


    //  Charge 
    globtrk[iglobtrk].icharge = track->GetCharge();

    //  If this is a primary track candidate
    globtrk[iglobtrk].id_start_vertex  = 10*ftpcVertex->GetId();


    //  radius at start of track (cm) 
    globtrk[iglobtrk].r0   = 
      ::sqrt(track->GetFirstPointOnTrack().X()*track->GetFirstPointOnTrack().X()
	   + track->GetFirstPointOnTrack().Y()*track->GetFirstPointOnTrack().Y());

    //  azimuthal angle at start of track (deg)
    globtrk[iglobtrk].phi0 = 
      atan2(track->GetFirstPointOnTrack().Y(),track->GetFirstPointOnTrack().X())
      * C_DEG_PER_RAD;

    //  z-coordinate at start of track 
    globtrk[iglobtrk].z0 = track->GetFirstPointOnTrack().Z();

    //  momentum angle at start 
    globtrk[iglobtrk].psi = 
      atan2(track->GetPy(),track->GetPx());
    if (globtrk[iglobtrk].psi < 0.0) {
      globtrk[iglobtrk].psi = 
	globtrk[iglobtrk].psi + C_2PI;
    }
    globtrk[iglobtrk].psi = 
      globtrk[iglobtrk].psi * C_DEG_PER_RAD; 

    //  1/pt at start 
    globtrk[iglobtrk].invpt = 1./track->GetPt();

    //  tan(dip) = pz/pt at start
    globtrk[iglobtrk].tanl  = track->GetPz()  
      *  globtrk[iglobtrk].invpt;

    //  curvature 
    globtrk[iglobtrk].curvature =  track->curvature();

    //  covariance matrix 
    //  (currently not set for FTPC) 
    globtrk[iglobtrk].covar[0] = 0;
    globtrk[iglobtrk].covar[1] = 0;
    globtrk[iglobtrk].covar[2] = 0;
    globtrk[iglobtrk].covar[3] = 0;
    globtrk[iglobtrk].covar[4] = 0;
    globtrk[iglobtrk].covar[5] = 0;
    globtrk[iglobtrk].covar[6] = 0;
    globtrk[iglobtrk].covar[7] = 0;
    globtrk[iglobtrk].covar[8] = 0;
    globtrk[iglobtrk].covar[9] = 0;
    globtrk[iglobtrk].covar[10] = 0;
    globtrk[iglobtrk].covar[11] = 0;
    globtrk[iglobtrk].covar[12] = 0;
    globtrk[iglobtrk].covar[13] = 0;
    globtrk[iglobtrk].covar[14] = 0;

    //  chi-square fit
    globtrk[iglobtrk].chisq[0]      = track->GetChiSq()[0]
      / (globtrk[iglobtrk].n_fit_point - 3);
    globtrk[iglobtrk].chisq[1]      = track->GetChiSq()[1]
      / (globtrk[iglobtrk].n_fit_point - 2);

     mon_soft_ftpc[0].res_pad_ftpc[iftpc] += globtrk[iglobtrk].chisq[0];
     mon_soft_ftpc[0].res_drf_ftpc[iftpc] += globtrk[iglobtrk].chisq[1];

    //  Locate last (outer) hit on current track
    StFtpcPoint *outer = (StFtpcPoint *)track->GetHits()->First();
    globtrk[iglobtrk].x_last[0] = outer->GetX();
    globtrk[iglobtrk].x_last[1] = outer->GetY();
    globtrk[iglobtrk].x_last[2] = outer->GetZ();

    //  radius at end of track (cm) 
    globtrk[iglobtrk].r0out   = 
      ::sqrt(track->GetLastPointOnTrack().X()*track->GetLastPointOnTrack().X()
	   + track->GetLastPointOnTrack().Y()*track->GetLastPointOnTrack().Y());

    //  azimuthal angle at end of track (deg)
    globtrk[iglobtrk].phi0out = 
      atan2(track->GetLastPointOnTrack().Y(),track->GetLastPointOnTrack().X())
      * C_DEG_PER_RAD;

    //  z-coordinate at end of track 
    globtrk[iglobtrk].z0out = track->GetLastPointOnTrack().Z();

    // For Kalman fitting 'inner' and 'outer' momenta differ.
    // For FTPC fitting they are the same,
    // so fill with the 'inner' values.

    //  momentum angle at end 
    globtrk[iglobtrk].psiout = 
      atan2(track->GetPy(),track->GetPx());
    if (globtrk[iglobtrk].psiout < 0.0) {
      globtrk[iglobtrk].psiout = 
	globtrk[iglobtrk].psiout + C_2PI;
    }
    globtrk[iglobtrk].psiout = 
      globtrk[iglobtrk].psiout * C_DEG_PER_RAD; 

    //  1/pt at end 
    globtrk[iglobtrk].invptout =  
      1./::sqrt(track->GetPx()*track->GetPx()
	      +track->GetPy()*track->GetPy());

    //  tan(dip) = pz/pt at end
    globtrk[iglobtrk].tanlout  = track->GetPz()  
      *  globtrk[iglobtrk].invptout;

    globtrk[iglobtrk].length  = track->GetTrackLength();
    mon_soft_ftpc[0].avg_trkL_ftpc[iftpc] += globtrk[iglobtrk].length;

    globtrk[iglobtrk].impact  = track->GetDca();

    //  Maximum number of points 
    globtrk[iglobtrk].n_max_point  = track->GetNMax();

    /// assume global tracking and vertex was found
    Int_t flag;
    if (track->ComesFromMainVertex()) flag = 1;
    else flag = 0;

    // bitmask quality information
    globtrk[iglobtrk].iflag = 
      700 + flag;
    if (fabs((float) globtrk[iglobtrk].icharge) != 1. ) {
      globtrk[iglobtrk].iflag   =  
	-globtrk[iglobtrk].iflag + 20;
    }
    if (fabs((float) globtrk[iglobtrk].invpt) >= 999999.)  {
      globtrk[iglobtrk].iflag   = -799;
    }

    //  Fill dst_dedx table  

    dedx[idedx].id_track = globtrk[iglobtrk].id;
    dedx[idedx].det_id = globtrk[iglobtrk].det_id;

    if(fdepar->id_method == 0)
      dedx[idedx].method = kTruncatedMeanId;
    else if (fdepar->id_method == 1)
      dedx[idedx].method = kEnsembleTruncatedMeanId;
    else
      dedx[idedx].method = kUndefinedMethodId;

    dedx[idedx].ndedx = track->GetNumdEdxHits();
    dedx[idedx].dedx[0] = track->GetdEdx();
    dedx[idedx].dedx[1] = 0;

  }    // End of processing current track

  dst_track->SetNRows(iglobtrk);
  dst_dedx->SetNRows(idedx);

  gMessMgr->Info() << "StFtpcGlobalMaker: " << itrk << " global Ftpc tracks written to DST tables." << endm;
 
  // Now save all hits, but only if tracks were found.
  // (If the tracker didn't run at all, no tracks neither hits are written, anyway.
  //  In the case where the tracker DID run, but didn't find tracks, 
  //  we don't want to write the hits either.)

  if (ftpcTracks->GetEntriesFast() > 0) {
    Int_t ipnt = 0;
    
    const float FTPC_FAC = 2380.0; // Multiplication factor to achieve 4 micron accuracy
    const float FTPC_MIN = -270.0;   // Minimum FTPC z-coordinate
    const float FTPC_MAX =  270.0;   // Maximum FTPC z-coordinate
    
    const int two10 =    1024;    // 2**10
    const int two17 =  131072;    // 2**17
    const int two20 = 1048576;    // 2**20
    
    unsigned int ftpcx, ftpcy, ftpcz;
    unsigned int ftpcy10, ftpcy11;
    
    //  Loop over all hits
    
    for (iPoint=0; iPoint<ftpcHits->GetEntriesFast(); iPoint++,ipnt++) {
      StFtpcPoint *hit = (StFtpcPoint*)ftpcHits->At(iPoint);
      
      if (hit->GetPadRow() >= 101) {
	point[ipnt].id_track    = hit->GetPadRow()/100;
	hit->SetPadRow(hit->GetPadRow()%100);   
      }
      else {
	point[ipnt].id_track    = 0;
      }

      //   Sum for dst_mon_soft_ftpc table
      // Historically Ftpc West was the first FTPC but since the TPC fills east first
      // the FTPC must do the same
      if (hit->GetDetectorId() == 5) iftpc=0;
      if (hit->GetDetectorId() == 4) iftpc=1;
      if ( point[ipnt].id_track != 0) mon_soft_ftpc[0].hit_frac_ftpc[iftpc]++;
      mon_soft_ftpc[0].n_pts_ftpc[iftpc]++;
      
      point[ipnt].hw_position = hit->GetHardwarePosition();
      
      //         Fill space point position coordinates
      if (hit->GetX() > FTPC_MIN && hit->GetX() < FTPC_MAX){
	ftpcx = (int) (FTPC_FAC*(hit->GetX() + FTPC_MAX));
      }
      else {
	ftpcx = 0;
      }
      if (hit->GetY() > FTPC_MIN && hit->GetY() < FTPC_MAX){
	ftpcy = (int) (FTPC_FAC*(hit->GetY() + FTPC_MAX));
      }
      else {
	ftpcy = 0;
      }
      if (hit->GetZ() > FTPC_MIN && hit->GetZ() < FTPC_MAX){
	ftpcz = (int) (FTPC_FAC*(hit->GetZ() + FTPC_MAX));
      }
      else {
	ftpcz = 0;
      }
      ftpcy10 = ftpcy/two10;
      ftpcy11 = ftpcy - two10*ftpcy10;
      point[ipnt].position[0] = ftpcx + (two20*ftpcy11);
      point[ipnt].position[1] = ftpcy10 + (two10*ftpcz);
      
      
      //         Fill space point position errors (0.0 <= error < 8.0)
      if (hit->GetXerr() >= 0.0 && hit->GetXerr() < 8.0){
	ftpcx =  (long) (two17*hit->GetXerr());
      }
      else {
	ftpcx = 0;
      }
      if (hit->GetYerr() >= 0.0 && hit->GetYerr() < 8.0){
	ftpcy = (long) (two17*hit->GetYerr());
      }
      else {
	ftpcy = 0;
      }
      if (hit->GetZerr() >= 0.0 && hit->GetZerr() < 8.0){
	ftpcz =  (long) (two17*hit->GetZerr());
      }
      else {
	ftpcz = 0;
      }
      ftpcy10 = ftpcy/two10;
      ftpcy11 = ftpcy - (two10*ftpcy10);
      point[ipnt].pos_err[0] = ftpcx + (two20*ftpcy11);
      point[ipnt].pos_err[1] = ftpcy10 + (two10*ftpcz);
      
      //        Fill charge and flags for cluster
      //                     bits 0-15    charge (sum of adc channels)
      //                     bits 16-31   flags  (see fcl_fppoint.idl)
      point[ipnt].charge  =
	(hit->GetFlags()<<16)
	+ hit->GetCharge();
      mon_soft_ftpc[0].chrg_ftpc_tot[iftpc] += hit->GetCharge();
      
    }  // end of loop over all hits
    
    dst_point->SetNRows(ipnt);

  gMessMgr->Info() << "StFtpcGlobalMaker: " << iPoint << " Ftpc hits written to DST tables." << endm;
 

  } // end of if (ftpcTracks->GetEntriesFast >0) [only write hits if some tracks were found]


    // Compute dst_mon_soft_ftpc table averages if tracks are found
    for (iftpc=0;iftpc<2;iftpc++) {
       if ( mon_soft_ftpc[0].n_pts_ftpc[iftpc] != 0 ){
	  mon_soft_ftpc[0].hit_frac_ftpc[iftpc] = mon_soft_ftpc[0].hit_frac_ftpc[iftpc]/mon_soft_ftpc[0].n_pts_ftpc[iftpc];
       }	       
       if ( mon_soft_ftpc[0].n_trk_ftpc[iftpc] != 0 ){
	   mon_soft_ftpc[0].avg_trkL_ftpc[iftpc] = mon_soft_ftpc[0].avg_trkL_ftpc[iftpc]/mon_soft_ftpc[0].n_trk_ftpc[iftpc];
           mon_soft_ftpc[0].res_pad_ftpc[iftpc] = mon_soft_ftpc[0].res_pad_ftpc[iftpc]/mon_soft_ftpc[0].n_trk_ftpc[iftpc];
           mon_soft_ftpc[0].res_drf_ftpc[iftpc] = mon_soft_ftpc[0].res_drf_ftpc[iftpc]/mon_soft_ftpc[0].n_trk_ftpc[iftpc];
       }
    }       
       	    
  return iMake;
}
//_____________________________________________________________________________

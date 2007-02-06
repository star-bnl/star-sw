// $Id: StFtpcTracker.cc,v 1.37 2007/02/06 11:42:16 jcs Exp $
// $Log: StFtpcTracker.cc,v $
// Revision 1.37  2007/02/06 11:42:16  jcs
// move unessential output messages from INFO to DEBUG
//
// Revision 1.36  2007/01/15 08:23:02  jcs
// replace printf, cout and gMesMgr with Logger commands
//
// Revision 1.35  2005/02/05 01:04:38  perev
// test for 1/0
//
// Revision 1.34  2004/09/03 20:36:23  perev
// Big LeakOff + mem optimisation
//
// Revision 1.33  2004/05/07 14:58:22  oldi
// Deletion of track array added again, since it doesn't go into a DataSet anymore.
// Minor clean-ups.
//
// Revision 1.32  2004/02/27 21:24:09  oldi
// Unnecessary check for number of tracks removed.
//
// Revision 1.31  2004/02/12 19:37:11  oldi
// *** empty log message ***
//
// Revision 1.30  2004/01/28 01:41:32  jeromel
// *** empty log message ***
//
// Revision 1.29  2003/12/05 04:08:49  perev
// technical cleanup
//
// Revision 1.28  2003/09/16 20:49:34  oldi
// One more pointer initialized to zero. Code clean-up.
//
// Revision 1.27  2003/09/16 16:52:48  jeromel
// Multiple constructor entry, zeroing mBench everywhere + doxygenized
//
// Revision 1.26  2003/09/16 15:27:02  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.25  2002/11/28 09:39:33  oldi
// Problem in momentum fit eliminated. Negative vertex Id is not used anymore.
// It was used do decide for global or primary fit.
// Code was prepared to fill momentum values at outermost points on tracks.
// This feature is not used up to now.
// Code cleanups.
//
// Revision 1.24  2002/11/06 13:47:11  oldi
// Vertex handling simplifed.
// Global/primary fit handling simplified.
// Code clean ups.
//
// Revision 1.23  2002/10/31 13:41:46  oldi
// dE/dx parameters read from database, now.
// Vertex estimation for different sectors added.
// Vertex estimation for different areas (angle, radius) added.
//
// Revision 1.22  2002/10/11 15:45:35  oldi
// Get FTPC geometry and dimensions from database.
// No field fit activated: Returns momentum = 0 but fits a helix.
// Bug in TrackMaker fixed (events with z_vertex > outer_ftpc_radius were cut).
// QA histograms corrected (0 was supressed).
// Code cleanup (several lines of code changed due to *params -> Instance()).
// cout -> gMessMgr.
//
// Revision 1.21  2002/06/04 13:41:35  oldi
// Minor change: 'west' -> 'hemisphere' (just a naming convention)
//
// Revision 1.20  2002/04/29 15:50:16  oldi
// All tracking parameters moved to StFtpcTrackingParameters.cc/hh.
// In a future version the actual values should be moved to an .idl file (the
// interface should be kept as is in StFtpcTrackingParameters.cc/hh).
//
// Revision 1.19  2002/04/05 16:51:09  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.18  2002/02/21 22:57:57  oldi
// Fixes to avoid warnings during optimized compilation.
//
// Revision 1.17  2002/02/14 22:05:12  oldi
// Typo removed.
//
// Revision 1.16  2002/01/29 11:08:20  oldi
// Write() renamed to WriteCluster() resp. WriteTrack() to avoid compiler warnings.
// As a result the functions TObject::Write() are available again (directly).
//
// Revision 1.15  2001/05/04 09:21:01  oldi
// Changed TMath::TMath:: to TMath::
//
// Revision 1.14  2001/04/25 17:54:12  perev
// HPcorrs
//
// Revision 1.13  2001/04/02 14:20:15  oldi
// Some minor changes due to Insure++ was reporting problems.
// These changes do not affect the physical output of StFtpcTrackMaker!
//
// Revision 1.12  2001/01/30 13:31:48  oldi
// New variable mTime introduced to count total time consumption.
//
// Revision 1.11  2001/01/25 15:22:31  oldi
// Review of the complete code.
// Fix of several bugs which caused memory leaks:
//  - Tracks were not allocated properly.
//  - Tracks (especially split tracks) were not deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way). I changed all occurences to TObjArray which makes the
//    program slightly slower but much more save (in terms of memory usage).
// Speed up of HandleSplitTracks() which is now 12.5 times faster than before.
// Cleanup.
//
// Revision 1.10  2000/11/28 14:10:11  jcs
// set a_large_number frp fdepar
//
// Revision 1.9  2000/11/23 01:33:16  oldi
// Proper initialization of some variables to avoid Insure++ error messages.
//
// Revision 1.8  2000/11/10 18:39:09  oldi
// TBenchmark object 'mBech' moved from StFtpcConfMapper to here. This implied changes in the constructors.
// New function CalcEnergyLoss(FDE_FDEPAR_ST *fdepar) which replaces the pams/fde modul.
// New function FitAnddEdxAndWrite() introduced which replaces CalcEnergyLoss() and FitAndWrite().
//
// Revision 1.7  2000/07/18 21:22:17  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.6  2000/07/03 12:48:14  jcs
// use (pre)Vertex id to access vertex coordinates for unconstrained fit and
// for constrained fit
//
// Revision 1.5  2000/06/13 14:28:23  oldi
// Changed cout to gMessMgr->Message().
// Printed output changed (slightly).
//
// Revision 1.4  2000/05/15 14:28:13  oldi
// problem of preVertex solved: if no main vertex is found (z = NaN) StFtpcTrackMaker stops with kStWarn,
// refitting procedure completed and included in StFtpcTrackMaker (commented),
// new constructor of StFtpcVertex due to refitting procedure,
// minor cosmetic changes
//
// Revision 1.3  2000/05/12 12:59:17  oldi
// removed delete operator for mSegment in StFtpcConfMapper (mSegment was deleted twice),
// add two new constructors for StFtpcTracker to be able to refit already existing tracks,
// minor cosmetics
//
// Revision 1.2  2000/05/11 15:14:53  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
// Revision 1.1  2000/05/10 13:39:31  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Holm G. H&uuml;mmler, Markus D. Oldenburg
//----------Last Modified: 10.11.2000
//----------Copyright:     &copy MDO Production 1999

#include <math.h>
#include "TArrayD.h"
#include "TArrayI.h"
#include "StFtpcTracker.hh"
#include "StFtpcPoint.hh"
#include "StFtpcTrack.hh"

#include "StMessMgr.h"

///////////////////////////////////////////////////////////////////////////////////
//                                                                               //
// StFtpcTracker class - interface class for the different Ftpc track algorithms //
//                                                                               //
// This class contains the pointers needed to do tracking in the Ftpc i.e. a     // 
// pointer to the vertex, pointers to clusters and tracks.                       //
//                                                                               //
///////////////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcTracker)


  
/// Default constructor. Sets the pointers to 0 an cut for momentum fit loosely.
StFtpcTracker::StFtpcTracker()
{
  mBench  = 0;
  mTime = 0.;

  mVertex = 0;
  mVertexEast = 0;
  mVertexWest = 0;

  mHit    = 0;
  mTrack  = 0;

  mHitsCreated   = (Bool_t)kFALSE;
  mVertexCreated = (Bool_t)kFALSE;
  mTrackCreated  = (Bool_t)kFALSE;

  mMaxDca = 100.;
}


/// Constructor to take care of arbitrary hits.
StFtpcTracker::StFtpcTracker(TObjArray *hits, StFtpcVertex *vertex, Bool_t bench, Double_t max_Dca)
{
  mBench  = 0;
  if (bench) {
    mBench = new TBenchmark();
  }

  mTime = 0.;

  mHit = hits;
  mHitsCreated = (Bool_t)kFALSE;

  mMaxDca = max_Dca;
  mTrack = new TObjArray(2000);
  mTrack->SetOwner(kTRUE);
  mTrackCreated = 1;
  mVertex = vertex;
  mVertexCreated = (Bool_t)kFALSE;

  mVertexEast = new StFtpcVertex();
  mVertexWest = new StFtpcVertex();
}

/// Constructor to handle the case where everything is there already.
StFtpcTracker::StFtpcTracker(StFtpcVertex *vertex, TObjArray *hit, TObjArray *track, Bool_t bench, Double_t max_Dca)
{
  mBench  = 0;
  if (bench) {
    mBench = new TBenchmark();
  }

  mTime = 0.;

  mVertex = vertex;
  mHit = hit;
  mHitsCreated   = (Bool_t) kFALSE;
  mVertexCreated = (Bool_t) kFALSE;
  mTrackCreated  = (Bool_t) kFALSE;
  mTrack = track;
  mMaxDca = max_Dca;

  mVertexEast = new StFtpcVertex();
  mVertexWest = new StFtpcVertex();
}


/// Destructor.
StFtpcTracker::~StFtpcTracker()
{
  // delete everything

  if (mTrack && mTrackCreated) {	 
    mTrack->Delete();	 
    delete mTrack; mTrack=0;	 
  }

  if (mHit && mHitsCreated) {
    mHit->Delete();
    delete mHit; mHit=0;
  }

  if (mVertex && mVertexCreated) {
    delete mVertex; mVertex=0;
  }

  if (mBench) {
    delete mBench; mBench=0;
  }
 
  delete mVertexEast;
  delete mVertexWest;

  return;
}


/// Vertex estimation with fit tracks for FTPC east and west.
void StFtpcTracker::EstimateVertex(StFtpcVertex *vertex, UChar_t iterations)
{
  EstimateVertex(vertex, -1, iterations);
  EstimateVertex(vertex, +1, iterations);

  return;
}


/// Vertex estimation with fit tracks.
void StFtpcTracker::EstimateVertex(StFtpcVertex *vertex, Char_t hemisphere, UChar_t iterations)
{
  StFtpcVertex v = *vertex;

  for (Int_t i = 0; i < iterations; i++) {
    StFtpcVertex v_new(mTrack, &v, hemisphere);
    v = v_new;
  }

  if (hemisphere == 1) {
    *mVertexWest = v;
  }

  else {
    // hemisphere == -1
    *mVertexEast = v;
  }

  return;
}


/// Vertex estimation with fit tracks for different sectors.
StFtpcVertex StFtpcTracker::EstimateVertex(StFtpcVertex *vertex, Char_t hemisphere, 
					   Char_t sector, UChar_t iterations)
{
  StFtpcVertex v = *vertex;
  TObjArray tracks(GetNumberOfTracks());

  for (Int_t tt = 0; tt < GetNumberOfTracks(); tt++) {
    // loop over all tracks

    StFtpcTrack *track = (StFtpcTrack*)mTrack->At(tt);

    if (track->GetSector() == sector) {
      // fill matching tracks to new array

      tracks.AddLast(track);
    }
  }

  for (Int_t i = 0; i < iterations; i++) {

    StFtpcVertex v_new(&tracks, &v, hemisphere);
    v = v_new;
  }

  return v;
}



/// Vertex estimation with fit tracks for different areas.
StFtpcVertex StFtpcTracker::EstimateVertex(StFtpcVertex *vertex, Char_t hemisphere,
					   Double_t lowAngle, Double_t highAngle,
					   Double_t lowRadius, Double_t highRadius, 
					   UChar_t iterations)
{
  StFtpcVertex v = *vertex;

  TObjArray tracks(GetNumberOfTracks());

  for (Int_t tt = 0; tt < GetNumberOfTracks(); tt++) {
    // loop over all tracks

    StFtpcTrack *track = (StFtpcTrack*)mTrack->At(tt);

    if (track->GetMeanR() >= lowRadius && track->GetMeanR() < highRadius &&
	track->GetMeanAlpha() >= lowAngle && track->GetMeanAlpha() < highAngle) {
      // fill matching tracks to new array

      tracks.AddLast(track);
    }
  }

  for (Int_t i = 0; i < iterations; i++) {
    StFtpcVertex v_new(&tracks, &v, hemisphere);
    v = v_new;
  }
    
  return v;
}


/*!
  Calculates dE/dx.
  This function replaces the old pams/ftpc/fde-module, but it is no 
  longer used as everything happens in FitAnddEdxAndWrite().
*/  
void StFtpcTracker::CalcEnergyLoss()
{
  Int_t itrk_ok ;                   // number of acepted tracks  
  Double_t total_charge = 0.0 ;     // total charges
  
  Int_t itrk;             // track counter
  Int_t icluster;         // cluster counter
  Int_t ihit;             // hit counter
  Int_t hit_p;            // pointer to next hit on a track
  Int_t all_hit;          // total number of hits
  Int_t acc_hit;          // accepted number of hits
  
  Double_t *dedx_arr=0;   // tmp array to store delta_E of hits on a track
  TArrayD dedx_arrT;
  Int_t *index_arr = 0;
  TArrayI index_arrT;
  StFtpcTrack *track;     // track
  StFtpcPoint *hit;       // hit
  
  Double_t xx, yy, rr, px, py, pz, pp, ftmp; // local coor. + mom
  Double_t cos_lambda, cos_alpha;     // cosines of dip and cross angles
  
  Double_t dedx_mean;
  
  // variables for full chamber truncation 
  Double_t *weighted=0;
  TArrayD weightedT;
  Double_t average_dedx;
  Int_t n_tracked;
  Int_t n_untracked;
  
  if (StFtpcTrackingParams::Instance()->DebugLevel() < 8) {      
    LOG_DEBUG << " No track = " << GetNumberOfTracks() << endm;
    LOG_DEBUG << " max_track = " << StFtpcTrackingParams::Instance()->MaxTrack() << endm;
    LOG_DEBUG << " max_hit = " << StFtpcTrackingParams::Instance()->MaxHit() << endm;
    LOG_DEBUG << " min_hit = " << StFtpcTrackingParams::Instance()->MinHit() << endm;;
    LOG_DEBUG << " ftrunc = " << StFtpcTrackingParams::Instance()->FracTrunc() << endm;
    
    LOG_DEBUG << " name= (no name), nok = (" << GetNumberOfClusters() 
				      << "), maxlen = (" << GetNumberOfClusters() << ")" << endm;
    LOG_DEBUG << " name= (no mane), nok = (" << GetNumberOfTracks() 
				      << "), maxlen = (" << GetNumberOfTracks() << ")" << endm;
    //LOG_DEBUG << " name= (" << fdepar_h->name << "), nok = (" << fdepar_h->nok 
    //				      << "), maxlen = (" << fdepar_h->maxlen << ")" << endm;
  }
  
  // initialize the dedx table counter
  itrk_ok   = 0; 
  n_tracked = 0;
  
  // tmp array to store delta_E of hits on a track 
  int nMaxHit = StFtpcTrackingParams::Instance()->MaxHit();
  if (dedx_arrT.GetSize()<nMaxHit) dedx_arrT.Set(nMaxHit);
  dedx_arrT.Reset();
  dedx_arr  = dedx_arrT.GetArray();;
  if (index_arrT.GetSize()<nMaxHit) index_arrT.Set(nMaxHit);
  index_arr = index_arrT.GetArray();

  // loop over all tracks inside the FTPC for each track 
  // possible to limit number of tracks for processing 
  for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), StFtpcTrackingParams::Instance()->MaxTrack()); itrk++) {

    all_hit = 0;       
    track = (StFtpcTrack*)mTrack->At(itrk);
    
    // we accumulate all the charges inside the sensitive volume
    for (icluster = 0; icluster < track->GetNumberOfPoints(); icluster++) {
      hit = (StFtpcPoint*)((TObjArray*)track->GetHits())->At(icluster);
      hit_p = hit->GetHitNumber();

      if(hit_p >= 0) {
	total_charge += hit->GetCharge();
	all_hit++;
	n_tracked++;

	if (StFtpcTrackingParams::Instance()->DebugLevel() < 2 ) {          // level=1 debugging
	  LOG_DEBUG << "total_charge = " << total_charge << ", hit_p = " << hit_p 
		   << ", de = " << hit->GetCharge() << endm;
	}
      }
    }

    if (all_hit < StFtpcTrackingParams::Instance()->MinHit() || all_hit > StFtpcTrackingParams::Instance()->MaxHit()) {
      
      if (StFtpcTrackingParams::Instance()->DebugLevel() < 5) {  // level = 10 debugging
	LOG_DEBUG << " number of hits = " << all_hit << endm;
      }
      
      continue;              // skip if unacceptable no. hits
    }

    // use vertex momenta instead of local momenta,
    // as angle to z-axis stays constant along the helix 
    // through the b-field
    px = track->GetPx();
    py = track->GetPy();
    pz = track->GetPz();
    pp = track->GetP();
       
    // fill the array after correcting the track length
    for (icluster = 0, ihit = 0; icluster < track->GetNumberOfPoints(); icluster++) {
      hit = (StFtpcPoint*)((TObjArray*)track->GetHits())->At(icluster);
      hit_p = hit->GetHitNumber();

      if(hit_p >= 0) {

	if (StFtpcTrackingParams::Instance()->NoAngle() != 0) {
	  cos_lambda = 1.0;
	  cos_alpha  = 1.0;
	} 

	else {	     
	  xx  = hit->GetX();
	  yy  = hit->GetY();
	  rr  = TMath::Sqrt(xx*xx + yy*yy);
	  xx  = xx/rr;                  // normalized
	  yy  = yy/rr;
	     
	  ftmp = (xx*px + yy*py)/pp; 
	  cos_lambda = TMath::Sqrt(1. - ftmp*ftmp);
	  ftmp = yy*px - xx*py;
	  cos_alpha  = fabs(pz) / TMath::Sqrt(pz*pz + ftmp*ftmp); 
	} 
	   
	if (StFtpcTrackingParams::Instance()->DebugLevel() < 2 ) {          // level=1 debugging
	  LOG_DEBUG << " ANGLES: dip= "<< 180./3.14159 * TMath::ACos(cos_lambda) << "(" 
		   << cos_lambda << ");  cross= " << 180./3.14159 * TMath::ACos(cos_alpha) 
		   << "(" << cos_alpha << ") [deg]; P=(" << px << ", " << py << ", " 
		   << pz << endm;
	}
	   
	if ( cos_alpha == 0. || cos_lambda == 0. ) {
	  dedx_arr[ihit] = StFtpcTrackingParams::Instance()->ALargeNumber();
	} 

	else {
	  dedx_arr[ihit] = hit->GetCharge() * cos_alpha*cos_lambda;

	  if(dedx_arr[ihit]<0) {
	    LOG_DEBUG << dedx_arr[ihit] << " " << hit->GetCharge() 
		     << " " << cos_alpha << " " << cos_lambda << endm;
	  }
	}
	
	ihit++;
      }
    }
    
    
    // sort hits on this track acording to dE/dx 
    Sorter(dedx_arr, index_arr, all_hit);
       
    // calculate the truncated mean of dE/dx 
    acc_hit   = 0;
    dedx_mean = 0.0;
    
    for (ihit = 0; ihit < (Int_t)(all_hit*StFtpcTrackingParams::Instance()->FracTrunc()); ihit++) {
      acc_hit++;
      dedx_mean += dedx_arr[ihit];
    }
       
    if (acc_hit < 1) continue;
    
    dedx_mean /= (Double_t)acc_hit;
       
    // fill the output table 
    track->SetdEdx(dedx_mean);
    track->SetNumdEdxHits(acc_hit);
    
    if(track->GetdEdx() == 0) {
      LOG_DEBUG << "track " << itrk << " dedx " << track->GetdEdx() 
	       << " ndedx " << track->GetNumdEdxHits() << endm;
    }
    
    itrk_ok++;
    
  } // end loop itrk 

  if(StFtpcTrackingParams::Instance()->IdMethod() == 1) {
    LOG_DEBUG << "Using truncated mean over whole chamber method by R. Witt." << endm;
    int nClusters = GetNumberOfClusters();
    if (weightedT.GetSize() <nClusters)  weightedT.Set(nClusters);
    weighted =weightedT.GetArray();
    if (index_arrT.GetSize()<nClusters) index_arrT.Set(nClusters);
    index_arr = index_arrT.GetArray();
    
    weightedT.Reset();index_arrT.Reset(-2);
    
    average_dedx=0;
    
    for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), StFtpcTrackingParams::Instance()->MaxTrack()); itrk++) {
      track = (StFtpcTrack*)mTrack->At(itrk);
      px  = track->GetPx();
      py  = track->GetPy();
      pz  = track->GetPz();
      pp  = track->GetP();
      
      average_dedx += track->GetdEdx();
      
      for (icluster = 0; icluster < track->GetNumberOfPoints(); icluster++) {
	hit = (StFtpcPoint*)((TObjArray*)track->GetHits())->At(icluster);
	hit_p = hit->GetHitNumber();
	
	if (hit_p >= 0) {
	  
	  if (StFtpcTrackingParams::Instance()->NoAngle() != 0) {
	    cos_lambda = 1.0;
	    cos_alpha  = 1.0;
	  }
		
	  else {
	    xx  = hit->GetX();
	    yy  = hit->GetY();
	    rr  = TMath::Sqrt(xx*xx + yy*yy);
	    xx  = xx/rr;                  // normalized
	    yy  = yy/rr;
	    
	    ftmp = (xx*px + yy*py)/pp; 
	    cos_lambda = TMath::Sqrt(1. - ftmp*ftmp);
	    ftmp = yy*px - xx*py;
	    cos_alpha  = fabs(pz) / TMath::Sqrt(pz*pz + ftmp*ftmp); 
	  } 
		
	  if (cos_alpha == 0. || cos_lambda == 0.) {
	    weighted[hit_p] = StFtpcTrackingParams::Instance()->ALargeNumber();
	  } 
	  
	  else {
	    weighted[hit_p] = hit->GetCharge() * cos_alpha*cos_lambda / track->GetdEdx();
	  }
		
	  index_arr[hit_p] = hit_p;
	}
      }
    }
    
    average_dedx /= TMath::Min(GetNumberOfTracks(), StFtpcTrackingParams::Instance()->MaxTrack());

    Sorter(weighted, index_arr, GetNumberOfClusters());
    
    // remove all hits over truncation threshold
    n_untracked = GetNumberOfClusters() - n_tracked;
    
    for(ihit = 0; ihit < GetNumberOfClusters(); ihit++) {
      
      if(index_arr[ihit]>=0) {
	
	if(ihit >= (n_untracked +(Int_t)(StFtpcTrackingParams::Instance()->FracTrunc()*(Double_t) n_tracked))) {
	  index_arr[ihit]=-1;
	}
      } 
    } 
    
    for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), StFtpcTrackingParams::Instance()->MaxTrack()); itrk++) {
      acc_hit = 0;
      dedx_mean = 0;
      
      track = (StFtpcTrack*)mTrack->At(itrk);
      
      for (icluster=0; icluster<track->GetNumberOfPoints(); icluster++) {
	// use vertex momenta instead of local momenta,
	// as angle to z-axis stays constant along the helix 
	// through the b-field
	px  = track->GetPx();
	py  = track->GetPy();
	pz  = track->GetPz();
	pp  = track->GetP();
	
	hit = (StFtpcPoint*)((TObjArray*)track->GetHits())->At(icluster);
	hit_p = hit->GetHitNumber();
	
	if(hit_p>=0) {
	  
	  for(ihit=n_untracked; ihit<GetNumberOfClusters(); ihit++) {
	    
	    if(hit_p == index_arr[ihit]) {
	      acc_hit++;
	      
	      if (StFtpcTrackingParams::Instance()->NoAngle() != 0) {
		cos_lambda = 1.0;
		cos_alpha  = 1.0;
	      }
	      
	      else {
		xx  = hit->GetX();
		yy  = hit->GetY();
		rr  = TMath::Sqrt(xx*xx + yy*yy);
		xx  = xx/rr;                  // normalized
		yy  = yy/rr;
		
		ftmp = (xx*px+yy*py)/pp; 
		cos_lambda = TMath::Sqrt(1. - ftmp*ftmp);
		ftmp = yy*px-xx*py;
		cos_alpha  = fabs(pz) / TMath::Sqrt(pz*pz + ftmp*ftmp); 
	      } 
	      
	      if (cos_alpha == 0. || cos_lambda == 0.) {
		acc_hit--;
	      } 
	      
	      else {
		dedx_mean += hit->GetCharge() * cos_alpha*cos_lambda;
	      }
	    }
	  }
	}
      }
      
      if (acc_hit < 1) continue;
      
      dedx_mean /= (Double_t)acc_hit;
      track->SetdEdx(dedx_mean);
      track->SetNumdEdxHits(acc_hit);
    }
    
  } 

  if (StFtpcTrackingParams::Instance()->DebugLevel() < 11) {
    LOG_DEBUG << " total charges in 2 FTPCs " << total_charge << endm;
    LOG_DEBUG << " processed tracks = " << itrk_ok << endm;
  }
      
  return;
}



/*!
  Sorts hits in ascending order (depending on dE/dx).
  This function is needed to replace pams/ftpc/fde.
*/
void StFtpcTracker::Sorter(Double_t *arr, Int_t *index, Int_t len)
{
  Int_t i, j;
  Double_t temp;
  Int_t itemp;

  for (i=0; i<len-2; ++i) {
    
    for (j = len-1; j > i; --j) {

      if (arr[i] > arr[j]) {
	temp = arr[i];       // swap
	arr[i] = arr[j];
	arr[j]= temp;
	itemp = index[i];
	index[i] = index[j];
	index[j] = itemp;
      }
    }
  }
}


/*!
  Fits tracks.
*/
Int_t StFtpcTracker::Fit(Bool_t primary_fit)
{
  if (mTrack) {
    StFtpcTrack *track=0;
    
    for (Int_t i=0; i<mTrack->GetEntriesFast(); i++) {
      track = (StFtpcTrack *)mTrack->At(i);
      track->Fit(mVertex, mMaxDca, primary_fit);
    }

    return -1;
  }
   
  return 0;
 }


// Calculates the momentum fit and dE/dx (no writing!).
Int_t StFtpcTracker::FitAnddEdx(Bool_t primary_fit)
{
  if (mBench) {
    mBench->Start("fit");
  }
  
  if (mTrack) {
    
    Int_t itrk_ok ;                   // number of acepted tracks  
    Double_t total_charge = 0.0 ;     // total charges
  
    Int_t itrk;             // track counter
    Int_t icluster;         // cluster counter
    Int_t ihit;             // hit counter
    Int_t hit_p;            // pointer to next hit on a track
    Int_t all_hit;          // total number of hits
    Int_t acc_hit;          // accepted number of hits
  
    Double_t *dedx_arr=0;     // tmp array to store delta_E of hits on a track
    TArrayD dedx_arrT;
    Int_t *index_arr;
    TArrayI  index_arrT; 
    StFtpcTrack *track;     // track
    StFtpcPoint *hit;       // hit
  
    Double_t xx, yy, rr, px, py, pz, pp, ftmp; // local coor. + mom
    Double_t cos_lambda, cos_alpha;     // cosines of dip and cross angles

    Double_t dedx_mean;
  
    // variables for full chamber truncation 
    TArrayD weightedT;
    Double_t *weighted=0;
    Double_t average_dedx;
    Int_t n_tracked;
    Int_t n_untracked;
  
    // initialize the dedx table counter
    itrk_ok   = 0; 
    n_tracked = 0;
  
    // tmp array to store delta_E of hits on a track 
    int nMaxHit = StFtpcTrackingParams::Instance()->MaxHit();
    if (dedx_arrT.GetSize()<nMaxHit) dedx_arrT.Set(nMaxHit);
    dedx_arrT.Reset();
    dedx_arr = dedx_arrT.GetArray();;
    if (index_arrT.GetSize()<nMaxHit) index_arrT.Set(nMaxHit);
    index_arr = index_arrT.GetArray();
    
    // loop over all tracks inside the FTPC for each track 
    // possible to limit number of tracks for processing 
    for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), StFtpcTrackingParams::Instance()->MaxTrack()); itrk++) {
      all_hit = 0;       
      track = (StFtpcTrack*)mTrack->At(itrk);
      track->Fit(mVertex, mMaxDca, primary_fit);

      // we accumulate all the charges inside the sensitive volume
      for (icluster = 0; icluster < track->GetNumberOfPoints(); icluster++) {
	hit = (StFtpcPoint*)((TObjArray*)track->GetHits())->At(icluster);
	hit_p = hit->GetHitNumber();

	if (hit_p >= 0) {
	  total_charge += hit->GetCharge();
	  all_hit++;
	  n_tracked++;
	}
      }

      if (all_hit < StFtpcTrackingParams::Instance()->MinHit() || all_hit > StFtpcTrackingParams::Instance()->MaxHit()) {      
	continue;              // skip if unacceptable no. hits
      }

      // use vertex momenta instead of local momenta,
      // as angle to z-axis stays constant along the helix 
      // through the b-field
      px = track->GetPx();
      py = track->GetPy();
      pz = track->GetPz();
      pp = track->GetP();
      if (pp<1.e-3) continue;
             
      // fill the array after correcting the track length
      for (icluster = 0, ihit = 0; icluster < track->GetNumberOfPoints(); icluster++) {
	hit = (StFtpcPoint*)((TObjArray*)track->GetHits())->At(icluster);
	hit_p = hit->GetHitNumber();

	if(hit_p >= 0) {

	  if (StFtpcTrackingParams::Instance()->NoAngle() != 0) {
	    cos_lambda = 1.0;
	    cos_alpha  = 1.0;
	  } 

	  else {	     
	    xx  = hit->GetX();
	    yy  = hit->GetY();
	    rr  = TMath::Sqrt(xx*xx + yy*yy);
	    xx  = xx/rr;                  // normalized
	    yy  = yy/rr;
	     
	    ftmp = (xx*px + yy*py)/pp; 
	    cos_lambda = TMath::Sqrt(1. - ftmp*ftmp);
	    ftmp = yy*px - xx*py;
	    cos_alpha  = fabs(pz) / TMath::Sqrt(pz*pz + ftmp*ftmp); 
	  } 
	   
	  if ( cos_alpha == 0. || cos_lambda == 0. ) {
	    dedx_arr[ihit] = StFtpcTrackingParams::Instance()->ALargeNumber();
	  } 

	  else {
	    dedx_arr[ihit] = hit->GetCharge() * cos_alpha*cos_lambda;
	  }
	
	  ihit++;
	}
      }
    
    
      // sort hits on this track acording to dE/dx 
      Sorter(dedx_arr, index_arr, all_hit);
       
      // calculate the truncated mean of dE/dx 
      acc_hit   = 0;
      dedx_mean = 0.0;
    
      for (ihit = 0; ihit < (Int_t)(all_hit*StFtpcTrackingParams::Instance()->FracTrunc()); ihit++) {
	acc_hit++;
	dedx_mean += dedx_arr[ihit];
      }
       
      if (acc_hit < 1) continue;
    
      dedx_mean /= (Double_t)acc_hit;
       
      // fill the output table 
      track->SetdEdx(dedx_mean);
      track->SetNumdEdxHits(acc_hit);
	    
      itrk_ok++;

      if (itrk > GetNumberOfTracks()) {
	break;
      }
    
    } // end loop itrk 

    if(StFtpcTrackingParams::Instance()->IdMethod() == 1) {
      LOG_DEBUG << "Using truncated mean over whole chamber method by R. Witt." << endm;
      int nClusters=GetNumberOfClusters();
      if (weightedT.GetSize()<nClusters) weightedT.Set(nClusters);
      weighted = weightedT.GetArray();
      if (index_arrT.GetSize()<nClusters) index_arrT.Set(nClusters);
      index_arr = index_arrT.GetArray();
    
      for(ihit=0; ihit<GetNumberOfClusters(); ihit++) {
	weighted[ihit] = 0;
	index_arr[ihit] = -2;
      }
    
      average_dedx=0;
    
      for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), StFtpcTrackingParams::Instance()->MaxTrack()); itrk++) {
	track = (StFtpcTrack*)mTrack->At(itrk);
	px  = track->GetPx();
	py  = track->GetPy();
	pz  = track->GetPz();
	pp  = track->GetP();
        if (pp <1.e-3) continue;
	average_dedx += track->GetdEdx();
      
	for (icluster = 0; icluster < track->GetNumberOfPoints(); icluster++) {
	  hit = (StFtpcPoint*)((TObjArray*)track->GetHits())->At(icluster);
	  hit_p = hit->GetHitNumber();
	
	  if (hit_p >= 0) {
	  
	    if (StFtpcTrackingParams::Instance()->NoAngle() != 0) {
	      cos_lambda = 1.0;
	      cos_alpha  = 1.0;
	    }
		
	    else {
	      xx  = hit->GetX();
	      yy  = hit->GetY();
	      rr  = TMath::Sqrt(xx*xx + yy*yy);
	      xx  = xx/rr;                  // normalized
	      yy  = yy/rr;
	    
	      ftmp = (xx*px + yy*py)/pp; 
	      cos_lambda = TMath::Sqrt(1. - ftmp*ftmp);
	      ftmp = yy*px - xx*py;
	      cos_alpha  = fabs(pz) / TMath::Sqrt(pz*pz + ftmp*ftmp); 
	    } 
		
	    if (cos_alpha == 0. || cos_lambda == 0.) {
	      weighted[hit_p] = StFtpcTrackingParams::Instance()->ALargeNumber();
	    } 
	  
	    else {
	      weighted[hit_p] = hit->GetCharge() * cos_alpha*cos_lambda / track->GetdEdx();
	    }
		
	    index_arr[hit_p] = hit_p;
	  }
	}
      }
    
      if (average_dedx) average_dedx /= TMath::Min(GetNumberOfTracks(), StFtpcTrackingParams::Instance()->MaxTrack());
    
      Sorter(weighted, index_arr, GetNumberOfClusters());
    
      // remove all hits over truncation threshold
      n_untracked = GetNumberOfClusters() - n_tracked;
    
      for(ihit = 0; ihit < GetNumberOfClusters(); ihit++) {
      
	if(index_arr[ihit]>=0) {
	
	  if(ihit >= (n_untracked +(Int_t)(StFtpcTrackingParams::Instance()->FracTrunc()*(Double_t) n_tracked))) {
	    index_arr[ihit]=-1;
	  }
	} 
      } 
    
      for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), StFtpcTrackingParams::Instance()->MaxTrack()); itrk++) {
	acc_hit = 0;
	dedx_mean = 0;
      
	track = (StFtpcTrack*)mTrack->At(itrk);

	for (icluster=0; icluster<track->GetNumberOfPoints(); icluster++) {
	  // use vertex momenta instead of local momenta,
	  // as angle to z-axis stays constant along the helix 
	  // through the b-field
	  px  = track->GetPx();
	  py  = track->GetPy();
	  pz  = track->GetPz();
	  pp  = track->GetP();
	  if (pp <1.E-3) continue;
	  hit = (StFtpcPoint*)((TObjArray*)track->GetHits())->At(icluster);
	  hit_p = hit->GetHitNumber();
	
	  if(hit_p>=0) {
	  
	    for(ihit=n_untracked; ihit<GetNumberOfClusters(); ihit++) {
	    
	      if(hit_p == index_arr[ihit]) {
		acc_hit++;
	      
		if (StFtpcTrackingParams::Instance()->NoAngle() != 0) {
		  cos_lambda = 1.0;
		  cos_alpha  = 1.0;
		}
	      
		else {
		  xx  = hit->GetX();
		  yy  = hit->GetY();
		  rr  = TMath::Sqrt(xx*xx + yy*yy);
		  xx  = xx/rr;                  // normalized
		  yy  = yy/rr;
		
		  ftmp = (xx*px+yy*py)/pp; 
		  cos_lambda = TMath::Sqrt(1. - ftmp*ftmp);
		  ftmp = yy*px-xx*py;
		  cos_alpha  = fabs(pz) / TMath::Sqrt(pz*pz + ftmp*ftmp); 
		} 
	      
		if (cos_alpha == 0. || cos_lambda == 0.) {
		  acc_hit--;
		} 
	      
		else {
		  dedx_mean += hit->GetCharge() * cos_alpha*cos_lambda;
		}
	      }
	    }
	  }
	}
      
	if (acc_hit < 1) {
	  continue;
	}

	dedx_mean /= (Double_t)acc_hit;
	track->SetdEdx(dedx_mean);
	track->SetNumdEdxHits(acc_hit);
      }
    
    } 


    if(mBench) {
      mBench->Stop("fit");
      LOG_DEBUG << "Fit and dE/dx calc. finished  (" << mBench->GetCpuTime("fit") << " s)." << endm;
      mTime += mBench->GetCpuTime("fit");
    }

    return 0;
  }
    
  else {
    
    if(mBench) {
      mBench->Stop("fit");
      LOG_DEBUG << "Fit, dE/dx, writing finished  (" << mBench->GetCpuTime("fit") << " s)." << endm;
      mTime += mBench->GetCpuTime("fit");
    }
    
    LOG_WARN << "Tracks not written (No tracks found!)." << endm;
    return -1;
  }
}

// $Id: StFtpcTracker.cc,v 1.15 2001/05/04 09:21:01 oldi Exp $
// $Log: StFtpcTracker.cc,v $
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


StFtpcTracker::StFtpcTracker()
{
  // Default constructor.
  // Sets the pointers to 0 an cut for momnetum fit loosely.

  mBench  = NULL;
  mTime = 0.;

  mVertex = NULL;
  mHit    = NULL;
  mTrack  = NULL;

  mHitsCreated = (Bool_t)false;
  mVertexCreated = (Bool_t)false;

  mMaxDca = 100.;
}


StFtpcTracker::StFtpcTracker(St_fcl_fppoint *fcl_fppoint, Double_t vertexPos[3], Bool_t bench, Double_t max_Dca)
{
  // Usual used constructor.
  // Sets up the pointers and the cut value for the momentum fit.

  if (bench) {
    mBench = new TBenchmark();
  }

  mTime = 0.;

  mHitsCreated = (Bool_t)false;
  mMaxDca = max_Dca;
  mTrack = new TObjArray(2000);

  Int_t n_clusters = fcl_fppoint->GetNRows();          // number of clusters
  fcl_fppoint_st *point_st = fcl_fppoint->GetTable();  // pointer to first cluster structure

  if(vertexPos == NULL) {
    mVertex = new StFtpcVertex(point_st, n_clusters);
  }
  
  else {
    mVertex = new StFtpcVertex(vertexPos);
  }

  mVertexCreated = (Bool_t)true;
}


StFtpcTracker::StFtpcTracker(TObjArray *hits, StFtpcVertex *vertex, Bool_t bench, Double_t max_Dca)
{
  // Constructor to take care of arbitrary hits.

  if (bench) {
    mBench = new TBenchmark();
  }

  mTime = 0.;

  mHit = hits;
  mHitsCreated = (Bool_t)false;

  mMaxDca = max_Dca;
  mTrack = new TObjArray(2000);

  mVertex = vertex;
  mVertexCreated = (Bool_t)false;
}


StFtpcTracker::StFtpcTracker(StFtpcVertex *vertex, TObjArray *hit, TObjArray *track, Bool_t bench, Double_t max_Dca)
{
  // Constructor to handle the case where everything is there already.

  if (bench) {
    mBench = new TBenchmark();
  }

  mTime = 0.;

  mVertex = vertex;
  mHit = hit;
  mHitsCreated = (Bool_t) false;
  mVertexCreated = (Bool_t) false;
  mTrack = track;
  mMaxDca = max_Dca;
}


StFtpcTracker::StFtpcTracker(StFtpcVertex *vertex, St_fcl_fppoint *fcl_fppoint, St_fpt_fptrack *fpt_fptrack, Bool_t bench, Double_t max_Dca)
{
  // Constructor to handle the case where everything is there already but only in StAF tables.

  if (bench) {
    mBench = new TBenchmark();
  }

  mTime = 0.;

  mVertex = vertex;
  mVertexCreated = (Bool_t)false;

  // Copy clusters into ObjArray.
  Int_t n_clusters = fcl_fppoint->GetNRows();          // number of clusters
  fcl_fppoint_st *point_st = fcl_fppoint->GetTable();  // pointer to first cluster structure

  mHit = new TObjArray(n_clusters);    // create TObjArray
  mHitsCreated = (Bool_t)true;

  {for (Int_t i = 0; i < n_clusters; i++) {
    mHit->AddAt(new StFtpcPoint(point_st++), i);
    ((StFtpcPoint *)mHit->At(i))->SetHitNumber(i);
  }}

  // Copy tracks into ObjArray.
  Int_t n_tracks = fpt_fptrack->GetNRows();  // number of tracks
  fpt_fptrack_st *track_st = fpt_fptrack->GetTable();  // pointer to first track structure

  mTrack = new TObjArray(n_tracks);    // create TObjArray

  {for (Int_t i = 0; i < n_tracks; i++) {
    mTrack->AddAt(new StFtpcTrack(track_st++, mHit, i), i);
  }}

  mMaxDca = max_Dca;
}


StFtpcTracker::~StFtpcTracker()
{
  // Destructor.

  if (mTrack) {
    mTrack->Delete();
    delete mTrack;
  }
  
  if (mHitsCreated) {
    mHit->Delete();
    delete mHit;
  }

  if (mVertex && mVertexCreated) {
    delete mVertex;
  }

  if (mBench) {
    delete mBench;
  }
 
  return;
}


void StFtpcTracker::CalcEnergyLoss(FDE_FDEPAR_ST *fdepar)
{
  // Calculates dE/dx.
  // This function replaces the old pams/ftpc/fde-module, but it is no 
  // longer used as everything happens in FitAnddEdxAndWrite().
    
  Int_t itrk_ok ;                   // number of acepted tracks  
  Double_t total_charge = 0.0 ;     // total charges
  
  Int_t itrk;             // track counter
  Int_t icluster;         // cluster counter
  Int_t ihit;             // hit counter
  Int_t hit_p;            // pointer to next hit on a track
  Int_t all_hit;          // total number of hits
  Int_t acc_hit;          // accepted number of hits
  
  Double_t *dedx_arr;     // tmp array to store delta_E of hits on a track
  Int_t *index_arr;
  
  StFtpcTrack *track;     // track
  StFtpcPoint *hit;       // hit
  
  Int_t debug_level = 100;
  Int_t no_angle;
  Int_t max_hit;
  Int_t min_hit;
  
  Double_t xx, yy, rr, px, py, pz, pp, ftmp; // local coor. + mom
  Double_t cos_lambda, cos_alpha;     // cosines of dip and cross angles
  Double_t a_large_number;
  
  Double_t ftrunc;
  Double_t dedx_mean;
  Double_t pad_length;
  
  // variables for full chamber truncation 
  Double_t *weighted;
  Double_t average_dedx;
  Int_t n_tracked;
  Int_t n_untracked;
  
  // get the preset parameters 
  debug_level = fdepar[0].debug_level;      // level of debugging
  no_angle    = fdepar[0].no_angle;         // switch for dip/cross angles
  max_hit     = fdepar[0].max_hit;          // max. allowable hits per track 
  min_hit     = fdepar[0].min_hit;          // min. no. hit required 
  pad_length  = fdepar[0].pad_length/100.;  // from cm to um/keV
  ftrunc      = fdepar[0].frac_trun;        // fraction for trunc. mean 
  a_large_number = fdepar[0].a_large_number; // 1e+10
  
  if (debug_level < 8 ) {      
    gMessMgr->Message("", "I", "OST") << " No track = " << (Int_t)GetNumberOfTracks() << endm;
    gMessMgr->Message("", "I", "OST") << " max_track = " << (Int_t)fdepar[0].max_track << endm;
    gMessMgr->Message("", "I", "OST") << " max_hit = " << max_hit << endm;
    gMessMgr->Message("", "I", "OST") << " min_hit = " << min_hit << endm;;
    gMessMgr->Message("", "I", "OST") << " ftrunc = " << ftrunc << endm;
    
    gMessMgr->Message("", "I", "OST") << " name= (no name), nok = (" << GetNumberOfClusters() 
				      << "), maxlen = (" << GetNumberOfClusters() << ")" << endm;
    gMessMgr->Message("", "I", "OST") << " name= (no mane), nok = (" << GetNumberOfTracks() 
				      << "), maxlen = (" << GetNumberOfTracks() << ")" << endm;
    //gMessMgr->Message("", "I", "OST") << " name= (" << fdepar_h->name << "), nok = (" << fdepar_h->nok 
    //				      << "), maxlen = (" << fdepar_h->maxlen << ")" << endm;
  }
  
  // initialize the dedx table counter
  itrk_ok   = 0; 
  n_tracked = 0;
  
  // tmp array to store delta_E of hits on a track 
  dedx_arr = new Double_t[max_hit];
  index_arr = new Int_t[max_hit];
  
  for (int go = 0; go < max_hit; go++) {
    dedx_arr[go] = 0.;
  }

  // loop over all tracks inside the FTPC for each track 
  // possible to limit number of tracks for processing 
  for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), (Int_t)fdepar[0].max_track); itrk++) {

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

	if (debug_level < 2 ) {          // level=1 debugging
	  gMessMgr->Message("", "I", "OST") << "total_charge = " << total_charge << ", hit_p = " << hit_p 
					    << ", de = " << hit->GetCharge() << endm;
	}
      }
    }

    if (all_hit < min_hit || all_hit > max_hit) {
      
      if (debug_level < 5) {  // level = 10 debugging
	gMessMgr->Message("", "I", "OST") << " number of hits = " << all_hit << endm;
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

	if (no_angle != 0) {
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
	   
	if (debug_level < 2 ) {          // level=1 debugging
	  gMessMgr->Message("", "I", "OST") << " ANGLES: dip= "<< 180./3.14159 * TMath::ACos(cos_lambda) << "(" 
					    << cos_lambda << ");  cross= " << 180./3.14159 * TMath::ACos(cos_alpha) 
					    << "(" << cos_alpha << ") [deg]; P=(" << px << ", " << py << ", " 
					    << pz << endm;
	}
	   
	if ( cos_alpha == 0. || cos_lambda == 0. ) {
	  dedx_arr[ihit] = a_large_number;
	} 

	else {
	  dedx_arr[ihit] = hit->GetCharge() * cos_alpha*cos_lambda;

	  if(dedx_arr[ihit]<0) {
	    gMessMgr->Message("", "I", "OST") << dedx_arr[ihit] << " " << hit->GetCharge() 
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
    
    for (ihit = 0; ihit < (Int_t)(all_hit*ftrunc); ihit++) {
      acc_hit++;
      dedx_mean += dedx_arr[ihit];
    }
       
    if (acc_hit < 1) continue;
    
    dedx_mean /= (Double_t)acc_hit;
       
    // fill the output table 
    track->SetdEdx(dedx_mean);
    track->SetNumdEdxHits(acc_hit);
    
    if(track->GetdEdx() == 0) {
      gMessMgr->Message("", "I", "OST") << "track " << itrk << " dedx " << track->GetdEdx() 
					<< " ndedx " << track->GetNumdEdxHits() << endm;
    }
    
    itrk_ok++;

    if (itrk > GetNumberOfTracks()) {
      gMessMgr->Message("", "I", "OST") << " itrk ("<< itrk <<") > fpt_fptrack_h->maxlen (" 
					<< GetNumberOfTracks() << ")" << endm;
      break;
    }
    
  } // end loop itrk 

  if(fdepar->id_method == 1) {
    gMessMgr->Message("", "I", "OST") << "Using truncated mean over whole chamber method by R. Witt." << endm;
    
    weighted = new Double_t[GetNumberOfClusters()];
    if (index_arr) delete[] index_arr; // This line was missing; new index_arr was invoked twice
    index_arr = new Int_t[GetNumberOfClusters()];
    
    for(ihit=0; ihit<GetNumberOfClusters(); ihit++) {
      weighted[ihit] = 0;
      index_arr[ihit] = -2;
    }
    
    average_dedx=0;
    
    for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), (Int_t)fdepar[0].max_track); itrk++) {
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
	  
	  if (no_angle != 0) {
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
	    weighted[hit_p] = a_large_number;
	  } 
	  
	  else {
	    weighted[hit_p] = hit->GetCharge() * cos_alpha*cos_lambda / track->GetdEdx();
	  }
		
	  index_arr[hit_p] = hit_p;
	}
      }
    }
    
    average_dedx /= TMath::Min(GetNumberOfTracks(), (Int_t)fdepar[0].max_track);

    Sorter(weighted, index_arr, GetNumberOfClusters());
    
    // remove all hits over truncation threshold
    n_untracked = GetNumberOfClusters() - n_tracked;
    
    for(ihit = 0; ihit < GetNumberOfClusters(); ihit++) {
      
      if(index_arr[ihit]>=0) {
	
	if(ihit >= (n_untracked +(Int_t)(ftrunc*(Double_t) n_tracked))) {
	  index_arr[ihit]=-1;
	}
      } 
    } 
    
    for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), (Int_t)fdepar[0].max_track); itrk++) {
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
	      
	      if (no_angle != 0) {
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
    
    delete[] weighted;
  } 

  if (debug_level < 11) {
    gMessMgr->Message("", "I", "OST") << " total charges in 2 FTPCs " << total_charge << endm;
    gMessMgr->Message("", "I", "OST") << " processed tracks = " << itrk_ok << endm;
  }
  
  delete[] dedx_arr;    // release the dedx_arr array
  delete[] index_arr;   // release the dedx_arr array
    
  return;
}


void StFtpcTracker::Sorter(Double_t *arr, Int_t *index, Int_t len)
{
  // Sorts hits in ascending order (depending on dE/dx).
  // This function is needed to replace pams/ftpc/fde.
 
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



Int_t StFtpcTracker::FitAndWrite(St_fpt_fptrack *trackTableWrapper, Int_t id_start_vertex)
{
  // Writes tracks to STAF table.
  // This function is no longer used. Everything is done now in FitAnddEdexAndWrite().
  
  fpt_fptrack_st *trackTable= trackTableWrapper->GetTable();

  if (mTrack) {
    Int_t num_tracks = mTrack->GetEntriesFast();
    
    if(num_tracks > trackTableWrapper->GetTableSize()) {
      num_tracks = trackTableWrapper->GetTableSize();
    }

    StFtpcTrack *track;
    
    for (Int_t i=0; i<num_tracks; i++) {
      track = (StFtpcTrack *)mTrack->At(i);
      track->Fit(mVertex, mMaxDca, id_start_vertex);
      track->Write(&(trackTable[i]), id_start_vertex);
    }
   
    trackTableWrapper->SetNRows(num_tracks);
    gMessMgr->Message("", "I", "OST") << "Writing " << num_tracks << " found tracks.";
    
    if (num_tracks == 1) {
      *gMessMgr << "." << endm;
    }
    
    else {
      *gMessMgr << "s." << endm;
    }

    return 0;
  }

  else {
    gMessMgr->Message("", "W", "OST") << "Tracks not written (No tracks found!)." << endm;
    return -1;
  }
}


Int_t StFtpcTracker::FitAnddEdxAndWrite(St_fpt_fptrack *trackTableWrapper, FDE_FDEPAR_ST *fdepar, Int_t id_start_vertex)
{
  // Calculates the momentum fit, the dE/dx, and writes the tracks to their STAF table, finally.
    
  if (mBench) {
    mBench->Start("fit");
  }
  
  fpt_fptrack_st *trackTable= trackTableWrapper->GetTable();
  
  if (mTrack) {
    Int_t num_tracks = mTrack->GetEntriesFast();
    
    if(num_tracks > trackTableWrapper->GetTableSize()) {
      num_tracks = trackTableWrapper->GetTableSize();
    }

    Int_t itrk_ok ;                   // number of acepted tracks  
    Double_t total_charge = 0.0 ;     // total charges
  
    Int_t itrk;             // track counter
    Int_t icluster;         // cluster counter
    Int_t ihit;             // hit counter
    Int_t hit_p;            // pointer to next hit on a track
    Int_t all_hit;          // total number of hits
    Int_t acc_hit;          // accepted number of hits
  
    Double_t *dedx_arr;     // tmp array to store delta_E of hits on a track
    Int_t *index_arr;
  
    StFtpcTrack *track;     // track
    StFtpcPoint *hit;         // hit
  
    Int_t debug_level = 100;
    Int_t no_angle;
    Int_t max_hit;
    Int_t min_hit;
  
    Double_t xx, yy, rr, px, py, pz, pp, ftmp; // local coor. + mom
    Double_t cos_lambda, cos_alpha;     // cosines of dip and cross angles
    Double_t a_large_number;
  
    Double_t ftrunc;
    Double_t aip;
    Double_t dedx_mean;
    Double_t pad_length;
  
    // variables for full chamber truncation 
    Double_t *weighted;
    Double_t average_dedx;
    Int_t n_tracked;
    Int_t n_untracked;
  
    // get the preset parameters 
    debug_level = fdepar[0].debug_level;      // level of debugging
    no_angle    = fdepar[0].no_angle;         // switch for dip/cross angles
    max_hit     = fdepar[0].max_hit;          // max. allowable hits per track 
    min_hit     = fdepar[0].min_hit;          // min. no. hit required 
    pad_length  = fdepar[0].pad_length/100.;  // from cm to um/keV
    ftrunc      = fdepar[0].frac_trun;        // fraction for trunc. mean 
    aip         = fdepar[0].a_i_p * 1.0e-9;   // in GeV 
  
    // initialize the dedx table counter
    itrk_ok   = 0; 
    n_tracked = 0;
  
    // tmp array to store delta_E of hits on a track 
    dedx_arr = new Double_t[max_hit];
    index_arr = new Int_t[max_hit];
    
    for (int go = 0; go < max_hit; go++) {
      dedx_arr[go] = 0.;
    }

    // loop over all tracks inside the FTPC for each track 
    // possible to limit number of tracks for processing 
    for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), (Int_t)fdepar[0].max_track); itrk++) {
      all_hit = 0;       
      track = (StFtpcTrack*)mTrack->At(itrk);
      track->Fit(mVertex, mMaxDca, id_start_vertex);

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

      if (all_hit < min_hit || all_hit > max_hit) {      
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

	  if (no_angle != 0) {
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
	    dedx_arr[ihit] = a_large_number;
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
    
      for (ihit = 0; ihit < (Int_t)(all_hit*ftrunc); ihit++) {
	acc_hit++;
	dedx_mean += dedx_arr[ihit];
      }
       
      if (acc_hit < 1) continue;
    
      dedx_mean /= (Double_t)acc_hit;
       
      // fill the output table 
      track->SetdEdx(dedx_mean);
      track->SetNumdEdxHits(acc_hit);
	
      if (fdepar->id_method != 1) { 
	// calculations done, write track
	// if id_method == 1 the calculaations go on and the track is writtem later
	track->Write(&(trackTable[itrk]), id_start_vertex);
      }
    
      itrk_ok++;

      if (itrk > GetNumberOfTracks()) {
	break;
      }
    
    } // end loop itrk 

    if(fdepar->id_method == 1) {
      gMessMgr->Message("", "I", "OST") << "Using truncated mean over whole chamber method by R. Witt." << endm;
    
      weighted = new Double_t[GetNumberOfClusters()];
      if (index_arr) delete[] index_arr; // This line was missing; new index_arr was invoked twice
      index_arr = new Int_t[GetNumberOfClusters()];
    
      for(ihit=0; ihit<GetNumberOfClusters(); ihit++) {
	weighted[ihit] = 0;
	index_arr[ihit] = -2;
      }
    
      average_dedx=0;
    
      for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), (Int_t)fdepar[0].max_track); itrk++) {
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
	  
	    if (no_angle != 0) {
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
	      weighted[hit_p] = a_large_number;
	    } 
	  
	    else {
	      weighted[hit_p] = hit->GetCharge() * cos_alpha*cos_lambda / track->GetdEdx();
	    }
		
	    index_arr[hit_p] = hit_p;
	  }
	}
      }
    
      average_dedx /= TMath::Min(GetNumberOfTracks(), (Int_t)fdepar[0].max_track);
    
      Sorter(weighted, index_arr, GetNumberOfClusters());
    
      // remove all hits over truncation threshold
      n_untracked = GetNumberOfClusters() - n_tracked;
    
      for(ihit = 0; ihit < GetNumberOfClusters(); ihit++) {
      
	if(index_arr[ihit]>=0) {
	
	  if(ihit >= (n_untracked +(Int_t)(ftrunc*(Double_t) n_tracked))) {
	    index_arr[ihit]=-1;
	  }
	} 
      } 
    
      for (itrk = 0; itrk < TMath::Min(GetNumberOfTracks(), (Int_t)fdepar[0].max_track); itrk++) {
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
	      
		if (no_angle != 0) {
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

	// write track
	track->Write(&(trackTable[itrk]), id_start_vertex);
      }
    
      delete[] weighted;
    } 

    delete[] dedx_arr;    // release the dedx_arr array
    delete[] index_arr;   // release the dedx_arr array
    
    trackTableWrapper->SetNRows(num_tracks);

    if(mBench) {
      mBench->Stop("fit");
      gMessMgr->Message("", "I", "OST") << "Fit, dE/dx, writing finished  (" << mBench->GetCpuTime("fit") << " s)." << endm;
      mTime += mBench->GetCpuTime("fit");
    }

    return 0;
  }
    
  else {
    
    if(mBench) {
      mBench->Stop("fit");
      gMessMgr->Message("", "I", "OST") << "Fit, dE/dx, writing finished  (" << mBench->GetCpuTime("fit") << " s)." << endm;
      mTime += mBench->GetCpuTime("fit");
    }
    
    gMessMgr->Message("", "W", "OST") << "Tracks not written (No tracks found!)." << endm;
    return -1;
  }
}

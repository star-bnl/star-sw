// $Id: StFtpcConfMapper.cc,v 1.13 2001/04/25 17:53:06 perev Exp $
// $Log: StFtpcConfMapper.cc,v $
// Revision 1.13  2001/04/25 17:53:06  perev
// HPcorrs
//
// Revision 1.12  2001/03/30 21:30:27  jeromel
// Constructor #2 argument 1 had an implicit/hidden copy of object.
// Removed const  in declaration.
//
// Revision 1.11  2001/01/30 13:31:27  oldi
// New variable mTime introduced to count total time consumption.
//
// Revision 1.10  2001/01/25 15:21:33  oldi
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
// Revision 1.9  2000/11/10 18:37:01  oldi
// New functions introduced to be able to extend tracks after main vertex tracking.
// This implied many changes in other parts of the class (loop over clusters can run backwards now).
// TBenchmark object ('mBench') moved to StFtpcTracker.
// Cleanup.
//
// Revision 1.8  2000/07/24 02:42:47  oldi
// Problem of memory exhaustion solved. This was introduced during the last changes.
//
// Revision 1.7  2000/07/18 21:22:15  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.6  2000/07/03 12:39:21  jcs
// correct comment
//
// Revision 1.5  2000/06/13 14:34:25  oldi
// Excluded function call to ExtendTracks().
// Changed cout to gMessMgr->Message().
// Limits for pseudorapidity eta changed. They are now calculated from the
// z-position of the main vertex directly. This solves bug #574.
//
// Revision 1.4  2000/06/07 10:04:18  oldi
// Changed 0 pointers to NULL pointers.
// Constructor changed. Setting of mNumRowSegment is not possible anymore to
// avoid that it is set to a value != 20. This was necessary to avoid the
// exit(-20) function call.
// New funtion CalcChiSquared(StFtpcTrack *track, StFtpcConfMapPoint *point,
// Double_t *chi2) introduced.
// StraightLineFit(StFtpcTrack *track, Double_t *a, Int_t n) now calculates the
// chi squared. Therefore the pointer to 'a' has to be a pointer to an 6-dim array
// (a[6]). Also the code of this function was modified to take care of the first
// point which is not allowed to be used in the circle fit (to avoid infinities).
// Introduction of new function CreateTrack() to cleanup ClusterLoop().
// Introduction of new function ExtendTracks() and TrackExtension() to get cleaner
// tracks.
// Cleanup.
//
// Revision 1.3  2000/05/12 12:59:12  oldi
// removed delete operator for mSegment in StFtpcConfMapper (mSegment was deleted twice),
// add two new constructors for StFtpcTracker to be able to refit already existing tracks,
// minor cosmetics
//
// Revision 1.2  2000/05/11 15:14:41  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
// Revision 1.1  2000/05/10 13:39:09  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 10.11.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFtpcConfMapper.hh"
#include "StFtpcConfMapPoint.hh"
#include "StFtpcTrack.hh"
#include "StFormulary.hh"
#include "MIntArray.h"

#include "TMath.h"
#include "TBenchmark.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TPolyMarker.h"
#include "TFile.h"
#include "TLine.h"
#include "TGraph.h"
#include "TMarker.h"
#include "TPolyLine3D.h"
#include "TPolyMarker3D.h"
#include "TTUBE.h"
#include "TBRIK.h"

#include "StMessMgr.h"

////////////////////////////////////////////////////////////////////////////////////
//                                                                                //
// StFtpcConfMapper class - tracking class to do tracking with conformal mapping. //
//                                                                                //
////////////////////////////////////////////////////////////////////////////////////


ClassImp(StFtpcConfMapper)


StFtpcConfMapper::StFtpcConfMapper() 
{
  // Default constructor.

  mLaser = (Bool_t)false;

  mHit     = NULL;
  mVolume  = NULL;

  mVertexConstraint = (Bool_t)true;
}


StFtpcConfMapper::StFtpcConfMapper(St_fcl_fppoint *fcl_fppoint, Double_t vertexPos[3], Bool_t bench, 
				   Int_t phi_segments, Int_t eta_segments) 
  : StFtpcTracker(fcl_fppoint, vertexPos, bench)
{
  // Constructor.

  if (bench) {
    mBench->Start("init");
  }

  mLaser = (Bool_t)false;

  mNumRowSegment = 20;   // The number of rows has to be fixed to 20 (because this is the number of rows in both Ftpc's)!
  mNumPhiSegment = phi_segments; 
  mNumEtaSegment = eta_segments; 
  mBounds = mNumRowSegment * mNumPhiSegment * mNumEtaSegment;
  mMaxFtpcRow = mNumRowSegment/2;

  CalcEtaMinMax();

  mMergedTracks = 0;
  mMergedTracklets = 0;
  mMergedSplits = 0;
  mExtendedTracks = 0;
  mDiffHits = 0;
  mDiffHitsStill = 0;
  mLengthFitNaN  = 0;

  Int_t n_clusters = fcl_fppoint->GetNRows();          // number of clusters
  mClustersUnused = n_clusters;

  fcl_fppoint_st *point_st = fcl_fppoint->GetTable();  // pointer to first cluster structure

  mHit = new TObjArray(n_clusters);    // create TObjArray
  mHitsCreated = (Bool_t)true;

  {for (Int_t i = 0; i < n_clusters; i++) {
    mHit->AddAt(new StFtpcConfMapPoint(point_st++, mVertex), i);
    ((StFtpcConfMapPoint *)mHit->At(i))->SetHitNumber(i);
  }}

  mVolume = new TObjArray(mBounds);  // create ObjArray for volume cells (of size bounds)

  {for (Int_t i = 0; i < mBounds; i++) {
    mVolume->AddAt(new TObjArray(0), i);     // Fill ObjArray with empty ObjArrays
  }}

  StFtpcConfMapPoint *h;

  {for (Int_t i = 0; i < mHit->GetEntriesFast(); i++) {
    h = (StFtpcConfMapPoint *)mHit->At(i);   
    h->Setup(mVertex);
    ((TObjArray *)mVolume->At(GetSegm(GetRowSegm(h), GetPhiSegm(h), GetEtaSegm(h))))->AddLast(h);
  }}

  if (mBench) {
    mBench->Stop("init");
    gMessMgr->Message("", "I", "OST") << "Setup finished                (" << mBench->GetCpuTime("init") << " s)." << endm;
    mTime += mBench->GetCpuTime("init");
  }
}


StFtpcConfMapper::StFtpcConfMapper(TObjArray *hits, StFtpcVertex *vertex, Bool_t bench, 
				   Int_t phi_segments, Int_t eta_segments) 
  : StFtpcTracker(hits, vertex, bench)
{
  // Constructor which needs a ClonesArray of hits as an input.
  // So it is possible to fill in arbitrary hits.

  if (bench) { 
    mBench = new TBenchmark();
    mBench->Start("init");
  }

  mLaser = (Bool_t)false;

  mNumRowSegment = 20;   // The number of rows has to be fixed to 20 (because this is the number of rows in both Ftpc's)!
  mNumPhiSegment = phi_segments; 
  mNumEtaSegment = eta_segments; 
  mBounds = mNumRowSegment * mNumPhiSegment * mNumEtaSegment;
  mMaxFtpcRow = mNumRowSegment/2;

  CalcEtaMinMax();

  mMergedTracks = 0;
  mMergedTracklets = 0;
  mMergedSplits = 0;
  mExtendedTracks = 0;
  mDiffHits = 0;
  mDiffHitsStill = 0;
  mLengthFitNaN  = 0;

  mClustersUnused = mHit->GetEntriesFast();

  mVolume = new TObjArray(mBounds);  // create ObjArray for volume cells (of size bounds)

  {for (Int_t i = 0; i < mBounds; i++) {
    mVolume->AddAt(new TObjArray(0), i);     // Fill ObjArray with empty ObjArrays
  }}

  StFtpcConfMapPoint *h;

  {for (Int_t i = 0; i < mHit->GetEntriesFast(); i++) {
    h = (StFtpcConfMapPoint *)mHit->At(i);
    ((TObjArray *)mVolume->At(GetSegm(GetRowSegm(h), GetPhiSegm(h), GetEtaSegm(h))))->AddLast(h);
  }}

  if (mBench) {
    mBench->Stop("init");
    gMessMgr->Message("", "I", "OST") << "Setup finished                (" << mBench->GetCpuTime("init") << " s)." << endm;
    mTime += mBench->GetCpuTime("init");
  }
}


StFtpcConfMapper::~StFtpcConfMapper()
{
  // Destructor.

  if (mVolume) {
    mVolume->Delete();
    delete mVolume;
  }
}


void StFtpcConfMapper::CompleteTrack(StFtpcTrack *track)
{
  // Do everything which is still left to be done for the given track.
  // Updates the statistics.

  track->SetPointDependencies();
  track->ComesFromMainVertex(mVertexConstraint);
  track->CalculateNMax();

  mClustersUnused -= track->GetNumberOfPoints();
  if (mVertexConstraint) mMainVertexTracks++;
  
  return;
}


void StFtpcConfMapper::MainVertexTracking()
{
  // Tracking with vertex constraint.

  if (mBench) {
    mBench->Start("main_vertex");
  }
  
  // settings
  SetMaxDca(1.);
  MainVertexSettings(3, 5, 1, 2, 1, 1);
  SetVertexConstraint(true);

  // cuts
  SetTrackletCuts(0.007, true);
  SetTrackCuts(0.007, 0.03, 30, true);

  ClusterLoop();
 
  if (mBench) {
    mBench->Stop("main_vertex");
    gMessMgr->Message("", "I", "OST") << "Main vertex tracking finished (" << mBench->GetCpuTime("main_vertex") << " s)." << endm;
    mTime += mBench->GetCpuTime("main_vertex");

    mBench->Start("extend");
  }

  ExtendTracks();

  if (mBench) {
    mBench->Stop("extend");
    gMessMgr->Message("", "I", "OST") << "Track extension finished      (" << mBench->GetCpuTime("extend") << " s)." << endm;
    mTime += mBench->GetCpuTime("extend");

    mBench->Start("splits");
  }

  HandleSplitTracks(0.11, 0.5, 0.5);

  if (mBench) {
    mBench->Stop("splits");
    gMessMgr->Message("", "I", "OST") << "Split track merging finished  (" << mBench->GetCpuTime("splits") << " s)." << endm;
    mTime += mBench->GetCpuTime("splits");
  }
  
  return;
}


void StFtpcConfMapper::FreeTracking()
{
  // Tracking without vertex constraint.

  if (mBench) {
    mBench->Start("non_vertex");
  }

  // settings
  SetMaxDca(1.);
  NonVertexSettings (3, 5, 1, 2, 1, 1);
  SetVertexConstraint(false);

  // cuts
  SetTrackletCuts(0.007, false);
  SetTrackCuts(0.007, 0.03, 70., false);

  ClusterLoop();

  if (mBench) {
    mBench->Stop("non_vertex");
    gMessMgr->Message("", "I", "OST") << "Non vertex tracking finished  (" << mBench->GetCpuTime("non_vertex") << " s)." << endm;
    mTime += mBench->GetCpuTime("non_vertex");
  }
  
  return;
}


void StFtpcConfMapper::LaserTracking()
{
  // Tracking of straight (laser) tracks.
  // Cuts and settings are optimized to find straight tracks.

  if (mBench) {
    mBench->Start("laser");
  }

  // settings
  SetLaser((Bool_t)true);
  SetMaxDca(1.);
  NonVertexSettings(10, 5, 2, 2, 2, 15);
  SetVertexConstraint(false);
  
  // cuts
  SetTrackletCuts(0.0001, false);
  
  ClusterLoop();
  ExtendTracks();
  
  if (mBench) {
    mBench->Stop("laser");
    gMessMgr->Message("", "I", "OST") << "Laser tracking finished       (" << mBench->GetCpuTime("laser") << " s)." << endm;
    mTime += mBench->GetCpuTime("laser");
  }
  
  return;
}


void StFtpcConfMapper::Settings(Int_t trackletlength1, Int_t trackletlength2, Int_t tracklength1, Int_t tracklength2, Int_t rowscopetracklet1, Int_t rowscopetracklet2, Int_t rowscopetrack1, Int_t rowscopetrack2, Int_t phiscope1, Int_t phiscope2, Int_t etascope1, Int_t etascope2)
{
  // Sets all settings of the tracker.
  // 
  // This is the order of settings given to this function:
  //
  //   - number of points to perform 'tracklet' search for main vertex tracks
  //   - number of points to perform 'tracklet' search for non vertex tracks
  //     These two mean no fitting but just looking for the nearest point 
  //     in the direction to the main vertex (also for non vertex tracks!).
  //
  //   - minimum number of points on a main vertex track
  //   - minimum number of points on a non vertex track
  //     These remove tracks and release points again already during the tracking
  //     if the tracker hasn't found enough hits on the track.
  //     
  //   - number of row segments to look on both sides for main vertex tracklets
  //   - number of row segments to look on both sides for non vertex tracklets
  //   - number of row segments to look on both sides for main vertex tracks
  //   - number of row segments to look on both sides for non vertex tracks
  //     These should be set to 1 for tracklets and to a value not less than 1 for track. 
  //     Otherwise (if the value is set to 0) the tracker looks for the next point
  //     only in the same row. It should be set to a value higher than 1 if you
  //     want to be able to miss a cluster (because it is not there) but still
  //     to extend the track. This is (may be) not a good idea for tracklets.
  //     The tracker will extend the search in the next to the following padrow only
  //     if it has not found a cluster in the following row.
  //
  //   - number of phi segments to look on both sides for main vertex tracks
  //   - number of phi segments to look on both sides for non vertex tracks
  //     These values have the same meaning as the values above for the row segemnts
  //     but now for the azimuth angle phi.
  //     The diffences are that the search is performed in any case over all ofthese 
  //     segemnts (not only if in the nearest segments nothing was found) and that the 
  //     reason to be able to set these values differential for main and non vertex tracks 
  //     is the fact that non vertex tracks may be bent more than the high momentum 
  //     main vertex tracks.
  //
  //   - number of eta segments to look on both sides for main vertex tracks
  //   - number of eta segments to look on both sides for non vertex tracks
  //     Same as for the phi segments (above but now in eta (pseudorapidity) space).

  SetTrackletLength(trackletlength1, trackletlength2);
  SetRowScopeTracklet(rowscopetracklet1, rowscopetracklet2);
  SetRowScopeTrack(rowscopetrack1, rowscopetrack2);
  SetPhiScope(phiscope1, phiscope2);
  SetEtaScope(etascope1, etascope2);
  SetMinPoints(tracklength1, tracklength2);
} 


void StFtpcConfMapper::Settings(Int_t trackletlength, Int_t tracklength, Int_t rowscopetracklet, Int_t rowscopetrack, Int_t phiscope, Int_t etascope)
{
  // Sets settings for the given vertex constraint.
  // See Begin_Html<a href="#settings">above</a>End_Html for details on the settings.

  SetTrackletLength(trackletlength, mVertexConstraint);
  SetRowScopeTracklet(rowscopetracklet, mVertexConstraint);
  SetRowScopeTrack(rowscopetrack, mVertexConstraint);
  SetPhiScope(phiscope, mVertexConstraint);
  SetEtaScope(etascope, mVertexConstraint);
  SetMinPoints(tracklength, mVertexConstraint);
}


void StFtpcConfMapper::MainVertexSettings(Int_t trackletlength, Int_t tracklength, Int_t rowscopetracklet, Int_t rowscopetrack, Int_t phiscope, Int_t etascope)
{
  // Sets settings for vertex constraint on.
  // See Begin_Html<a href="#settings">above</a>End_Html for details on the settings.

  SetTrackletLength(trackletlength, (Bool_t) true);
  SetRowScopeTracklet(rowscopetracklet, (Bool_t) true);
  SetRowScopeTrack(rowscopetrack, (Bool_t) true);
  SetPhiScope(phiscope, (Bool_t) true);
  SetEtaScope(etascope, (Bool_t) true);
  SetMinPoints(tracklength, (Bool_t) true);
}


void StFtpcConfMapper::NonVertexSettings(Int_t trackletlength, Int_t tracklength, Int_t rowscopetracklet, Int_t rowscopetrack, Int_t phiscope, Int_t etascope)
{
  // Sets settings for vertex constraint off.
  // Begin_Html
  // See <a href="#settings">above</a> for details on the settings.<a name="cuts"></a>End_Html

  SetTrackletLength(trackletlength, (Bool_t) false);
  SetRowScopeTracklet(rowscopetracklet, (Bool_t) false);
  SetRowScopeTrack(rowscopetrack, (Bool_t) false);
  SetPhiScope(phiscope, (Bool_t) false);
  SetEtaScope(etascope, (Bool_t) false);
  SetMinPoints(tracklength, (Bool_t) false);
}


void StFtpcConfMapper::SetCuts(Double_t maxangletracklet1, Double_t maxangletracklet2, Double_t maxangletrack1,  Double_t maxangletrack2, Double_t maxcircletrack1, Double_t maxcircletrack2, Double_t maxlengthtrack1, Double_t maxlengthtrack2)
{
  // Sets all cuts for the tracking.
  // 
  // This is the order of settings given to this function:
  //
  //   - maximum angle of main vertex tracklets
  //   - maximum angle of non vertex tracklets
  //   - maximum angle of main vertex tracks
  //   - maximum angle of non vertex tracks 
  //   - maximal distance from circle fit for main vertex tracks
  //   - maximal distance from circle fit for non vertex tracks
  //   - maximal distance from length fit for main vertex tracks
  //   - maximal distance from length fit for non vertex tracks

  SetMaxAngleTracklet(maxangletracklet1, maxangletracklet2);
  SetMaxAngleTrack(maxangletrack1, maxangletrack2);
  SetMaxCircleDistTrack(maxcircletrack1, maxcircletrack2);
  SetMaxLengthDistTrack(maxlengthtrack1, maxlengthtrack2);
}


void StFtpcConfMapper::SetCuts(Double_t maxangletracklet, Double_t maxangletrack, Double_t maxcircletrack, Double_t maxlengthtrack) 
{
  // Sets cuts for vertex constraint on or off.
  // See Begin_Html<a href="#cuts">above</a>End_Html for details on the cuts.

  SetMaxAngleTracklet(maxangletracklet, mVertexConstraint);
  SetMaxAngleTrack(maxangletrack, mVertexConstraint);
  SetMaxCircleDistTrack(maxcircletrack, mVertexConstraint);
  SetMaxLengthDistTrack(maxlengthtrack, mVertexConstraint);
}


void StFtpcConfMapper::SetTrackCuts(Double_t maxangle, Double_t maxcircletrack, Double_t maxlengthtrack, Bool_t vertex_constraint)
{
  // Sets cuts of tracks for the given vertex constraint.
  // See Begin_Html<a href="#cuts">above</a>End_Html for details on the cuts.

  SetMaxAngleTrack(maxangle, vertex_constraint);
  SetMaxCircleDistTrack(maxcircletrack, vertex_constraint);
  SetMaxLengthDistTrack(maxlengthtrack, vertex_constraint);
}


void StFtpcConfMapper::SetTrackletCuts(Double_t maxangle, Bool_t vertex_constraint)
{
  // Sets cuts of tracklets for the given vertex constraint.
  // See Begin_Html<a href="#cuts">above</a>End_Html for details on the cuts.

  SetMaxAngleTracklet(maxangle, vertex_constraint);
}


void StFtpcConfMapper::LoopUpdate(Int_t *sub_row_segm, Bool_t backward)
{
  // Increments or decrements *sub_row_segm, depending on backward.
  
  if (backward) {
    (*sub_row_segm)--;
  }
  
  else { // forward
    (*sub_row_segm)++;
  }
}


Bool_t const StFtpcConfMapper::TestExpression(Int_t sub_row_segm, Int_t end_row, Bool_t backward)
{
  // Tests if loop should be continued or not.
  
  if (backward) {
    
    if (sub_row_segm >= end_row) {
      return (Bool_t)true;
    }
    
    else {
      return (Bool_t) false;
    }
  }
  
  else { // forward
    
    if (sub_row_segm <= end_row) {
      return (Bool_t)true;
    }
    
    else {
      return (Bool_t) false;
    }
  }
 
  return (Bool_t) false;
}


Double_t const StFtpcConfMapper::CalcDistance(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2, Bool_t laser)
{
  // Returns the distance of two given clusters. The distance in this respect (conformal mapping)
  // is defined in the paper "A Fast track pattern recognition" by Pablo Yepes, NIM A 380 (1996) 585-585.
  // If laser == true I expect straight tracks and the returned value will be the ordinary 3d distance.
  
  if (!laser) {
 
    Double_t phi_diff = TMath::Abs( hit1->GetPhi() - hit2->GetPhi() );
    if (phi_diff > TMath::Pi()) phi_diff = 2*TMath::Pi() - phi_diff;
    
    return TMath::Abs(hit1->GetPadRow() - hit2->GetPadRow()) *  (phi_diff + TMath::Abs( hit1->GetEta() - hit2->GetEta() ));
  }

  else {
    return TMath::Sqrt(TMath::Power(hit1->GetX() - hit2->GetX(), 2.) + TMath::Power(hit1->GetY() - hit2->GetY(), 2.) + TMath::Power(hit1->GetZ() - hit2->GetZ(), 2.));
  }
}


Double_t const StFtpcConfMapper::CalcDistance(const StFtpcConfMapPoint *hit, Double_t *coeff) 
{
  // Returns the distance of a point to a straight line.
  // The point is given by the to conformal coordinates of a cluster and the
  // straight line is given by its to coefficients: y = coeff[0]*x + coeff[1].

  Double_t x = (coeff[0] / (1 + coeff[0]*coeff[0])) * (1/coeff[0] * hit->GetXprime() + hit->GetYprime() - coeff[1]);

  return TMath::Sqrt(TMath::Power(x - hit->GetXprime(), 2) + TMath::Power(coeff[0]*x + coeff[1] - hit->GetYprime(), 2));
} 


Bool_t const StFtpcConfMapper::VerifyCuts(const StFtpcConfMapPoint *lasttrackhit, const StFtpcConfMapPoint *newhit, Bool_t backward)
{
  // Returns true if circle, length, and angle cut holds.

  if (newhit->GetCircleDist() < mMaxCircleDist[mVertexConstraint] &&
      newhit->GetLengthDist() < mMaxLengthDist[mVertexConstraint] &&
      TrackAngle(lasttrackhit, newhit, backward) < mMaxAngleTrack[mVertexConstraint]) {
    return true;
  }
  
  else {
    return false;
  }
}


Double_t const StFtpcConfMapper::TrackAngle(const StFtpcPoint *lasthitoftrack, const StFtpcPoint *hit, Bool_t backward = (Bool_t)true)
{
  // Returns the 'angle' between the last two points on the track (of which the last point is
  // given as input) and the second given point.
  
  Double_t x1[3];
  Double_t x2[3];

  StFtpcTrack *track = lasthitoftrack->GetTrack(mTrack);
  TObjArray *hits = track->GetHits();
  Int_t n = track->GetNumberOfPoints();
  
  if (n<2) {
    gMessMgr->Message("StFtpcConfMapper::TrackAngle(StFtpcPoint *lasthitoftrack, StFtpcPoint *hit)", "E", "OST");    
    gMessMgr->Message(" - Call this function only if you are sure to have at least two points on the track already!", "E", "OST");
    
    return 0.;
  }

  if (backward) {
    x1[0] = ((StFtpcPoint *)hits->At(n-1))->GetX() - ((StFtpcPoint *)hits->At(n-2))->GetX();
    x1[1] = ((StFtpcPoint *)hits->At(n-1))->GetY() - ((StFtpcPoint *)hits->At(n-2))->GetY();
    x1[2] = ((StFtpcPoint *)hits->At(n-1))->GetZ() - ((StFtpcPoint *)hits->At(n-2))->GetZ();
    
    x2[0] = hit->GetX() - ((StFtpcPoint *)hits->At(n-1))->GetX();
    x2[1] = hit->GetY() - ((StFtpcPoint *)hits->At(n-1))->GetY();
    x2[2] = hit->GetZ() - ((StFtpcPoint *)hits->At(n-1))->GetZ();
  }

  else { // forward
    x1[0] = ((StFtpcPoint *)hits->At(0))->GetX() - ((StFtpcPoint *)hits->At(1))->GetX();
    x1[1] = ((StFtpcPoint *)hits->At(0))->GetY() - ((StFtpcPoint *)hits->At(1))->GetY();
    x1[2] = ((StFtpcPoint *)hits->At(0))->GetZ() - ((StFtpcPoint *)hits->At(1))->GetZ();
    
    x2[0] = hit->GetX() - ((StFtpcPoint *)hits->At(0))->GetX();
    x2[1] = hit->GetY() - ((StFtpcPoint *)hits->At(0))->GetY();
    x2[2] = hit->GetZ() - ((StFtpcPoint *)hits->At(0))->GetZ();
  }

  return StFormulary::Angle(x1, x2, 3);
}


Double_t const StFtpcConfMapper::TrackletAngle(StFtpcTrack *track, Int_t n)
{
  // Returns the angle 'between' the last three points (started at point number n) on this track.

  Double_t x1[3];
  Double_t x2[3];  

  TObjArray *hits = track->GetHits();  
  if (n > track->GetNumberOfPoints() || n == 0) {
    n = track->GetNumberOfPoints();
  }

  if (n<3) {
    gMessMgr->Message("StFtpcConfMapper::TrackletAngle(StFtpcTrack *track, Int_t n)", "E", "OST");
    gMessMgr->Message(" - Call this function only if you are sure to have at least three points on this track already!", "E", "OST");

    return 0.;
  }

  x1[0] = ((StFtpcPoint *)hits->At(n-2))->GetX() - ((StFtpcPoint *)hits->At(n-3))->GetX();
  x1[1] = ((StFtpcPoint *)hits->At(n-2))->GetY() - ((StFtpcPoint *)hits->At(n-3))->GetY();
  x1[2] = ((StFtpcPoint *)hits->At(n-2))->GetZ() - ((StFtpcPoint *)hits->At(n-3))->GetZ();
  
  x2[0] = ((StFtpcPoint *)hits->At(n-1))->GetX() - ((StFtpcPoint *)hits->At(n-2))->GetX();
  x2[1] = ((StFtpcPoint *)hits->At(n-1))->GetY() - ((StFtpcPoint *)hits->At(n-2))->GetY();
  x2[2] = ((StFtpcPoint *)hits->At(n-1))->GetZ() - ((StFtpcPoint *)hits->At(n-2))->GetZ();  

  return StFormulary::Angle(x1, x2, 3);
}


void StFtpcConfMapper::CalcChiSquared(StFtpcTrack *track, StFtpcConfMapPoint *point, Double_t *chi2)
{
  // Calculates the chi squared for the circle and the length fit for a given point 
  // as an extension for the also given track.

  TObjArray *trackpoint = track->GetHits();
  StFtpcConfMapPoint *last_point = (StFtpcConfMapPoint *)trackpoint->Last();

  if (!mVertexConstraint) {
    point->SetAllCoord(last_point);
  }

  trackpoint->AddLast(point);
  
  Double_t a[6];
  StraightLineFit(track, a);
  chi2[0] = a[4];
  chi2[1] = a[5];

  trackpoint->Remove(point);

  return;
}

/*
Double_t StFtpcConfMapper::CalcLength(StFtpcTrack *track, StFtpcTrackPoint* point)
{

  Double_t s;

  asin_arg = (point->GetY() - track->GetCenterY()) / track->GetRadius();
  
  // The following lines were inserted because ~1% of all tracks produce arguments of arcsin 
  // which are above the |1| limit. But they were differing only in the 5th digit after the point.
  if (TMath::Abs(asin_arg) > 1.) {
    asin_arg = (asin_arg >= 0) ? +1. : -1.;
    //mLengthFitNaN++;
  }
  
  if (point->GetX() >= track->GetCenterX() && point->GetY() > track->GetCenterY()) {
    angle = TMath::ASin(asin_arg);
  }
  
  else if (point->GetX() < track->GetCenterX() && point->GetY() >= track->GetCenterY()) {
    angle = TMath::ASin(-asin_arg) + TMath::Pi();
  }
  
  else if (point->GetX() <= track->GetCenterX() && point->GetY() < track->GetCenterY()) {
    angle = TMath::ASin(-asin_arg) +  TMath::Pi();
  }
  
  else if (point->GetX() > track->GetCenterX() && point->GetY() <= track->GetCenterY()) {
    angle = TMath::ASin(asin_arg) + 2 * TMath::Pi();
  }
  
  s = (angle - track->GetAlpha0()) * 
    TMath::Sqrt(TMath::Power(track->GetRadius(), 2.) + TMath::Power(point->GetZv() , 2.)); 
  
  return s;
}


void StFtcpConfMapper::CalcAngle(StFtpcTrack *track, StFtpcTrackPoint* point)
{
  // Calculates the the angle of the given point of a track with respect to the vertex of this point.

  Double_t asin_arg;

  asin_arg = (point->GetY() - track->GetCenterY()) / track->GetRadius();
  
  // The following lines were inserted because ~1% of all tracks produce arguments of arcsin 
  // which are above the |1| limit. But they were differing only in the 5th digit after the point.
  if (TMath::Abs(asin_arg) > 1.) {
    asin_arg = (asin_arg >= 0) ? +1. : -1.;
    //mLengthFitNaN++;
  }
  
  if (point->GetX() >= track->GetCenterX() && point->GetY() > track->GetCenterY()) {
    point->SetPhiv(TMath::ASin(asin_arg));
  }
  
  else if (point->GetX() < track->GetCenterX() && point->GetY() >= track->GetCenterY()) {
    point->SetPhiv(-TMath::ASin(asin_arg) + TMath::Pi());
  }
  
  else if (point->GetX() <= track->GetCenterX() && point->GetY() < track->GetCenterY()) {
    point->SetPhiv(TMath::ASin(-asin_arg) +  TMath::Pi());
  }
  
  else if (point->GetX() > track->GetCenterX() && point->GetY() <= track->GetCenterY()) {
    point->SetPhiv(-TMath::ASin(-asin_arg) + 2 * TMath::Pi());
  }

  return;
}
*/

void StFtpcConfMapper::StraightLineFit(StFtpcTrack *track, Double_t *a, Int_t n)
{
  // Calculates two straight line fits with the given clusters
  //
  // The first calculation is performed in the conformal mapping space (Xprime, Yprime):
  // Yprime(Xprime) = a[0]*Xprime + a[1].
  // The second calculates the fit for length s vs. z:
  // s(z) = a[2]*z +a[3].
  // a[4] is the chi squared per degrees of freedom for the circle fit.
  // a[5] is the chi squared per degrees of freedom for the length fit.

  TObjArray *trackpoints = track->GetHits();
  
  if (n>0) {
    
    if (n > trackpoints->GetEntriesFast()) {
      n = trackpoints->GetEntriesFast();
    }
  }
  
  else {
    n = trackpoints->GetEntriesFast();
  }

  Double_t L11 = 0.;
  Double_t L12 = 0.;
  Double_t L22 = 0.;
  Double_t  g1 = 0.;
  Double_t  g2 = 0.;

  Double_t asin_arg, asin_arg2;
  Int_t start_counter = 0;
  
  if (!mVertexConstraint) {
    start_counter = 1;
  }

  // Circle Fit
  StFtpcConfMapPoint *trackpoint;

  {for (Int_t i = start_counter; i < n; i++) {
    trackpoint = (StFtpcConfMapPoint *)trackpoints->At(i);

    L11 += 1./* / (trackpoint->GetYprimeerr() * trackpoint->GetYprimeerr())*/;
    L12 +=  trackpoint->GetXprime()/* / (trackpoint->GetYprimeerr() * trackpoint->GetYprimeerr())*/;
    L22 += (trackpoint->GetXprime() * trackpoint->GetXprime())/* / (trackpoint->GetYprimeerr() * trackpoint->GetYprimeerr())*/;
    g1  +=  trackpoint->GetYprime()/* / (trackpoint->GetYprimeerr() * trackpoint->GetYprimeerr())*/;
    g2  += (trackpoint->GetXprime() * trackpoint->GetYprime())/* / (trackpoint->GetYprimeerr() * trackpoint->GetYprimeerr())*/;
  }}

  Double_t D = L11*L22 - L12*L12;
  
  a[0] = (g2*L11 - g1*L12)/D;
  a[1] = (g1*L22 - g2*L12)/D;

  // Set circle parameters
  track->SetCenterX(- a[0] / (2. * a[1]) + trackpoint->GetXt());
  track->SetCenterY(-  1.  / (2. * a[1]) + trackpoint->GetYt());
  track->SetRadius(TMath::Sqrt(a[0]*a[0] + 1.) / (2. * TMath::Abs(a[1])));
  track->CalcAndSetAlpha0();

  //cout << track->GetRadius() << " " << track->GetAlpha0() << endl;

  // Tracklength Fit
  Double_t s;
  Double_t angle;
  Double_t angle_diff;

  // Set variables again
  // first track point is main vertex or first track point (both at (0, 0, 0) [shifted coordinates])
  L11 = 1.;
  L12 = L22 = g1 = g2 = 0.;

  {for (Int_t i = start_counter; i < n; i++ ) {
    
    trackpoint= (StFtpcConfMapPoint *)trackpoints->At(i);
    
    // calculate angle

    // The following lines were inserted because ~1% of all tracks produce arguments of arcsin 
    // which are above the |1| limit. But they were differing only in the 5th digit after the point.
    asin_arg = StFormulary::CheckASinArg(asin_arg2 = (trackpoint->GetY() - track->GetCenterY()) / track->GetRadius());
    if (asin_arg != asin_arg2) mLengthFitNaN++;

    if (trackpoint->GetX() >= track->GetCenterX() && trackpoint->GetY() > track->GetCenterY()) {
      angle = TMath::ASin(asin_arg);
    }
    
    else if (trackpoint->GetX() < track->GetCenterX() && trackpoint->GetY() >= track->GetCenterY()) {
      angle = TMath::ASin(-asin_arg) + TMath::Pi();
    }
    
    else if (trackpoint->GetX() <= track->GetCenterX() && trackpoint->GetY() < track->GetCenterY()) {
      angle = TMath::ASin(-asin_arg) +  TMath::Pi();
    }
    
    else if (trackpoint->GetX() > track->GetCenterX() && trackpoint->GetY() <= track->GetCenterY()) {
      angle = TMath::ASin(asin_arg) + 2 * TMath::Pi();
    }
    
    angle_diff = angle - track->GetAlpha0();
    
    if (TMath::Abs(angle_diff) > TMath::Pi()) {
      if (angle_diff > 0.) {
	angle_diff -= 2*TMath::Pi();
      }

      else {
	angle_diff += 2*TMath::Pi();
      }
    }

    s = TMath::Sqrt(TMath::Power(track->GetRadius() * angle_diff, 2.) 
		    + TMath::Power(trackpoint->GetZv() , 2.));

    //cout << angle << " " << angle - track->GetAlpha0() << " " << s << endl;

    L11 += 1;
    L12 += trackpoint->GetZv();
    L22 += (trackpoint->GetZv() * trackpoint->GetZv());
    g1  += s;
    g2  += (s * trackpoint->GetZv());
  }}
    
  D = L11*L22 - L12*L12;

  //cout <<  endl;

  a[2] = (g2*L11 - g1*L12)/D;
  a[3] = (g1*L22 - g2*L12)/D;

  // Set chi squared
  Double_t chi2circle = 0.;
  Double_t chi2length = 0.;
  
  {for (Int_t i = start_counter; i < n; i++) {
    trackpoint = (StFtpcConfMapPoint *)trackpoints->At(i);
    asin_arg = StFormulary::CheckASinArg((trackpoint->GetYv() - track->GetCenterY()) / track->GetRadius());
 
    s = TMath::Sqrt(TMath::Power(track->GetRadius() * angle_diff, 2.) 
		    + TMath::Power(trackpoint->GetZv() , 2.));

    chi2circle += TMath::Power(trackpoint->GetYprime() - a[0]*trackpoint->GetXprime()+a[1], 2.) / (a[0]*trackpoint->GetXprime()+a[1]);    
    chi2length += TMath::Power(s - a[2]*trackpoint->GetZv()+a[3], 2.) / (a[2]*trackpoint->GetZv()+a[3]);
  }}


  if (mVertexConstraint) {
    n++;
  } 

  a[4] = chi2circle/(n-3.);
  a[5] = chi2length/(n-2.);

  //track->SetChi2Circle(chi2circle/(n-2.));
  //track->SetChi2Length(chi2length/(n-2.));

  return;
}


void StFtpcConfMapper::HandleSplitTracks(Double_t max_dist, Double_t ratio_min, Double_t ratio_max)
{
  // Looks for split tracks and passes them to the track merger.
  Double_t ratio;
  Double_t dist;

  Double_t r1;
  Double_t r2;
  Double_t R1;
  Double_t R2;
  Double_t phi1;
  Double_t phi2;
  Double_t Phi1;
  Double_t Phi2;

  Int_t first_split = -1;
 
  StFtpcTrack *t1;
  StFtpcTrack *t2;

  Int_t entries = mTrack->GetEntriesFast();
      
  for (Int_t i = 0; i < entries; i++) {
    t1 = (StFtpcTrack *)mTrack->At(i);

    if (!t1) { 
      // track was removed before (already merged)
      continue;
    }

    for (Int_t j = i+1; j < entries; j++) {
      t2 = (StFtpcTrack *)mTrack->At(j);
     
      if (!t2) {
	// track was removed before (already merged)
	continue;
      }

      if (!t1) {
	// track t1 was removed before (already merged) - leave inner loop
	break;
      }
      
      if (!(t1->GetRowsWithPoints() & t2->GetRowsWithPoints()) && 
	  (t1->GetHemisphere() == t2->GetHemisphere())) {
	
	r1 = t1->GetRLast();
	r2 = t2->GetRLast();
	phi1 = t1->GetAlphaLast();
	phi2 = t2->GetAlphaLast();
	R1 = t1->GetRFirst();
	R2 = t2->GetRFirst();
	Phi1 = t1->GetAlphaFirst();
	Phi2 = t2->GetAlphaFirst();
	
	dist = (TMath::Sqrt(r2*r2+r1*r1-2*r1*r2*(TMath::Cos(phi1)*TMath::Cos(phi2)+TMath::Sin(phi1)*TMath::Sin(phi2))) +
		TMath::Sqrt(R2*R2+R1*R1-2*R1*R2*(TMath::Cos(Phi1)*TMath::Cos(Phi2)+TMath::Sin(Phi1)*TMath::Sin(Phi2)))) / 2.;
	ratio = (Double_t)(t1->GetNumberOfPoints() + t2->GetNumberOfPoints()) / (Double_t)(t1->GetNMax() + t2->GetNMax());
	
	if (dist <= max_dist && ratio <= ratio_max && ratio >= ratio_min) {
	  
	  if (first_split == -1) {
	    first_split = i;
	  }
	  
	  MergeSplitTracks(t1, t2);
	  t1 = t2 = NULL;
	}      
      }
    }
  }
  
  if (first_split != -1) {
    // adjust track numbers only if something has changed
    AdjustTrackNumbers(first_split);
  }

  return;
}


void StFtpcConfMapper::MergeSplitTracks(StFtpcTrack *t1, StFtpcTrack *t2)
{
  // Merges two tracks which are split.

  Int_t new_track_number = mTrack->GetEntriesFast();
  Int_t num_t1_points = t1->GetNumberOfPoints();
  Int_t num_t2_points = t2->GetNumberOfPoints();
  TObjArray *t1_points = t1->GetHits();
  TObjArray *t2_points = t2->GetHits();

  mTrack->AddAt(new StFtpcTrack(new_track_number), new_track_number);
  StFtpcTrack *track = (StFtpcTrack *)mTrack->At(new_track_number);

  TObjArray *trackpoint = track->GetHits();
  trackpoint->Expand(mMaxFtpcRow);

  MIntArray *trackhitnumber = track->GetHitNumbers();  

  {for (Int_t i = 0; i < mMaxFtpcRow; i++) {

    if (i < num_t1_points) {
      trackpoint->AddAt(t1_points->At(i), mMaxFtpcRow - 1 -(((StFtpcPoint *)t1_points->At(i))->GetPadRow()-1)%mMaxFtpcRow);
    } 
   
    if (i < num_t2_points) {
      trackpoint->AddAt(t2_points->At(i), mMaxFtpcRow - 1 -(((StFtpcPoint *)t2_points->At(i))->GetPadRow()-1)%mMaxFtpcRow);
    } 
  }}
 
  trackpoint->Compress();
  trackpoint->Expand(trackpoint->GetLast()+1);

  {for (Int_t i = 0; i < trackpoint->GetEntriesFast(); i++) {
    trackhitnumber->AddLast(((StFtpcPoint *)trackpoint->At(i))->GetHitNumber());
  }}
  
  track->SetProperties(true, new_track_number);
  track->ComesFromMainVertex(mVertexConstraint);
  track->CalculateNMax();

  if (mVertexConstraint) mMainVertexTracks++;
  if (t1->ComesFromMainVertex()) mMainVertexTracks--;
  if (t2->ComesFromMainVertex()) mMainVertexTracks--;

  mTrack->Remove(t1);
  mTrack->Remove(t2);
  delete t1;
  delete t2;

  mMergedSplits++;

  return;
}


void StFtpcConfMapper::ClusterLoop()
{
  // This function loops over all clusters to do the tracking.
  // These clusters act as starting points for new tracks.
  
  Int_t row_segm;
  Int_t phi_segm;
  Int_t eta_segm;
  Int_t entries;
  Int_t hit_num;

  TObjArray *segment;  
  StFtpcConfMapPoint *hit;

  // loop over two Ftpcs
  for (mFtpc = 1; mFtpc <= 2; mFtpc++) {

    // loop over the respective 10 RowSegments ("layers") per Ftpc
    // loop only so far to where you can still put a track in the remaining padrows (due to length)
    for (row_segm = mFtpc * mMaxFtpcRow - 1; row_segm >= (mFtpc-1) * mMaxFtpcRow + mMinPoints[mVertexConstraint] - 1; row_segm--) {

      // loop over phi segments
      for (Int_t phi_segm_counter = 0; phi_segm_counter < mNumPhiSegment; phi_segm_counter++) {
	
	// go over phi in two directions, one segment in each direction alternately
	if(phi_segm_counter%2) {
	  phi_segm = mNumPhiSegment - phi_segm_counter - mNumPhiSegment%2;
	}

	else {
	  phi_segm = phi_segm_counter;
	}

	// loop over eta segments
	for(eta_segm = 0; eta_segm < mNumEtaSegment; eta_segm++) { 
	  
	  // loop over entries in one segment 
	  if ((entries = (segment = (TObjArray *)mVolume->At(GetSegm(row_segm, phi_segm, eta_segm)))->GetEntriesFast())) {    
	    
	    for (hit_num = 0; hit_num < entries; hit_num++) {  
	      hit = (StFtpcConfMapPoint *)segment->At(hit_num);
	      
	      if (hit->GetUsage() == true) { // start hit was used before 
		continue;
	      }
	      
	      else { // start hit was not used before
		CreateTrack(hit);
	      }
	    }
	  }

	  else continue;  // no entries in this segment
	}
      }
    }
  }
  
  return; // end of track finding
}  


void StFtpcConfMapper::CreateTrack(StFtpcConfMapPoint *hit)
{
  // This function takes as input a point in the Ftpc which acts as starting point for a new track.
  // It forms tracklets then extends them to tracks.

  Double_t *coeff = NULL;
  //Double_t chi2[2];

  StFtpcConfMapPoint *closest_hit = NULL;
  StFtpcTrack *track = NULL;

  Int_t point;
  Int_t tracks = GetNumberOfTracks();
  if (tracks >= mTrack->GetSize()) mTrack->Expand(mTrack->GetSize()+1000);

  mTrack->AddAt(new StFtpcTrack(tracks), tracks);
  track = (StFtpcTrack *)mTrack->At(tracks);
  TObjArray *trackpoint = track->GetHits();

  track->AddPoint(hit);

  // set conformal mapping coordinates if looking for non vertex tracks
  if (!mVertexConstraint) {
    hit->SetAllCoord(hit);
  }
  
  // create tracklets
  for (point = 1; point < mTrackletLength[mVertexConstraint]; point++) {

    if ((closest_hit = GetNextNeighbor(hit, coeff, (Bool_t)true))) {
      // closest_hit for hit exists
      track->AddPoint(closest_hit);
      hit = closest_hit;

      if (GetLaser() && track->GetNumberOfPoints() == mTrackletLength[mVertexConstraint]) {
	tracks++;
	CompleteTrack(track);

	return; // no additional track search
      }
    }
    
    else { // closest hit does not exist
      
      if (!GetLaser()) { // this is not a laser event
	RemoveTrack(track);
	point = mTrackletLength[mVertexConstraint];  // continue with next hit in segment
      }

      else { // this is a laser event
	
	if (track->GetNumberOfPoints() < mMinPoints[mVertexConstraint]) { // not enough points
	  RemoveTrack(track);
	  point = mTrackletLength[mVertexConstraint];  // continue with next hit in segment
	}
	
	else { // enough points
	  tracks++;
	  CompleteTrack(track);
	  
	  return; // no additional track search
	}
      }
    }
  }

  // tracklet is long enough to be extended to a track
  if (trackpoint->GetEntriesFast() == mTrackletLength[mVertexConstraint]) {
    
    if (TrackletAngle(track) > mMaxAngleTracklet[mVertexConstraint]) { // proof if the first points seem to be a beginning of a track
      RemoveTrack(track);
    }
    
    else { // good tracklet -> proceed
      tracks++;
      
      // create tracks 
      for (point = mTrackletLength[mVertexConstraint]; point < mMaxFtpcRow; point++) {
	
	if (!coeff) coeff = new Double_t[6];
	//Double_t chi_circ = track->GetChi2Circle();
	//Double_t chi_len  = track->GetChi2Length();
	
	StraightLineFit(track, coeff);
	closest_hit = GetNextNeighbor((StFtpcConfMapPoint *)trackpoint->Last(), coeff, (Bool_t)true);
	
	if (closest_hit) {
	  
	  /*
	    CalcChiSquared(track, closest_hit, chi2);
	    
	    if (coeff[4]-chi2[0]>1.) {
	    //cout << coeff[4] << " - " << chi2[0] << " = " << coeff[4]-chi2[0] << endl;
	    //cout << coeff[5] << " - " << chi2[1] << " = " << coeff[5]-chi2[1] << endl << endl;
	    point = mMaxFtpcRow;
	    }
	    
	    else {
	  */
	  
	  // add closest hit to track
	  track->AddPoint(closest_hit);

	  // }
	}
	
	else { 
	  // closest hit does not exist
	  
	  /*
	    probably switch off vertexconstraint!
	    
	    if (point.PadRow() > limit) {
	    
	    }
	    
	    else
	  */
	  point = mMaxFtpcRow; // continue with next hit in segment
	}
      }
      
      // remove tracks with not enough points already now
      if (track->GetNumberOfPoints() < mMinPoints[mVertexConstraint]) {
	RemoveTrack(track);
	tracks--;
      }      
      
      else {
	//cout << coeff[2] << endl;
	CompleteTrack(track);
      }
      
      // cleanup
      delete[] coeff; 
      coeff = NULL;
    } 
  } 	

  return;
}


StFtpcConfMapPoint *StFtpcConfMapper::GetNextNeighbor(StFtpcConfMapPoint *start_hit, Double_t *coeff = NULL, Bool_t backward = (Bool_t)true)
{ 
  // Returns the nearest cluster to a given start_hit. 
  
  Double_t dist, closest_dist = 1.e7;
  Double_t closest_circle_dist = 1.e7;
  Double_t closest_length_dist = 1.e7;

  StFtpcConfMapPoint *hit = NULL;
  StFtpcConfMapPoint *closest_hit = NULL;
  StFtpcConfMapPoint *closest_circle_hit = NULL;
  StFtpcConfMapPoint *closest_length_hit = NULL;
  
  TObjArray *sub_segment;
  Int_t sub_entries;

  Int_t sub_row_segm;
  Int_t sub_phi_segm;
  Int_t sub_eta_segm;
  Int_t sub_hit_num;

  Int_t start_row;
  Int_t end_row;
  
  if (backward) {
    start_row = GetRowSegm(start_hit) - 1;

    if (coeff) {
      end_row = GetRowSegm(start_hit) - mRowScopeTrack[mVertexConstraint];
    }
    
    else {
      end_row = GetRowSegm(start_hit) - mRowScopeTracklet[mVertexConstraint];
    }
    
    while (end_row < (mFtpc-1) * mMaxFtpcRow) {
      end_row++;
    }
  
    if (start_row < end_row) return 0;
  }

  else { // forward
     start_row = GetRowSegm(start_hit) + 1;

    if (coeff) {
      end_row = GetRowSegm(start_hit) + mRowScopeTrack[mVertexConstraint];
    }
    
    else {
      end_row = GetRowSegm(start_hit) + mRowScopeTracklet[mVertexConstraint];
    }
    
    while (end_row > mFtpc * mMaxFtpcRow - 1) {
      end_row--;
    }
  
    if (start_row > end_row) return 0;
  }
  //if (end_row == 20) 
  //cout << start_row << " " << end_row << endl;
  
  // loop over sub rows
  //  for (sub_row_segm = start_row; sub_row_segm >= end_row; sub_row_segm--) {

  for (sub_row_segm = start_row; TestExpression(sub_row_segm, end_row, backward); LoopUpdate(&sub_row_segm, backward)) {    

    //  loop over sub phi segments
    for (Int_t i = -(mPhiScope[mVertexConstraint]); i <= mPhiScope[mVertexConstraint]; i++) {
      sub_phi_segm = GetPhiSegm(start_hit) + i;  // neighboring phi segment 
      
      if (sub_phi_segm < 0) {  // find neighboring segment if #segment < 0
	sub_phi_segm += mNumPhiSegment;
      }
      
      else if (sub_phi_segm >= mNumPhiSegment) { // find neighboring segment if #segment > fNum_phi_segm
	sub_phi_segm -= mNumPhiSegment;
      }
      
      // loop over sub eta segments
      for (Int_t j = -(mEtaScope[mVertexConstraint]); j <= mEtaScope[mVertexConstraint]; j++) {
	sub_eta_segm = GetEtaSegm(start_hit) + j;   // neighboring eta segment 
	
	if (sub_eta_segm < 0 || sub_eta_segm >= mNumEtaSegment) {  
	  continue;  // #segment exceeds bounds -> skip
	}
	
	// loop over entries in one sub segment
	if ((sub_entries = ((sub_segment = (TObjArray *)mVolume->At(GetSegm(sub_row_segm, sub_phi_segm, sub_eta_segm)))->GetEntriesFast()))) {  		

	  for (sub_hit_num = 0; sub_hit_num < sub_entries; sub_hit_num++) {  
	    hit = (StFtpcConfMapPoint *)sub_segment->At(sub_hit_num);
	    
	    if (!(hit = (StFtpcConfMapPoint *)sub_segment->At(sub_hit_num))->GetUsage()) {
	      // hit was not used before
	      
	      // set conformal mapping coordinates if looking for non vertex tracks
	      if (!mVertexConstraint) {
		hit->SetAllCoord(start_hit);
	      }
	      
	      if (coeff) { // track search - look for nearest neighbor to extrapolated track
		
		// test distance
		hit->SetDist(CalcDistance(hit, coeff+0), CalcDistance(hit, coeff+2));

		if (hit->GetCircleDist() < closest_circle_dist) {
		  closest_circle_dist = hit->GetCircleDist();
		  closest_circle_hit = hit;
		}
		
		if (hit->GetLengthDist() < closest_length_dist) {   
		  closest_length_dist = hit->GetLengthDist();
		  closest_length_hit = hit;
		}
	      }
	      
	      else {  
		// tracklet search - just look for the nearest neighbor (distance defined by Pablo Jepes)
		
		// test distance
		if ((dist = CalcDistance(start_hit, hit, GetLaser())) < closest_dist) { // hit found that is closer than the hits before
		  
		  if (GetLaser() && ((StFtpcTrack*)start_hit->GetTrack(mTrack))->GetNumberOfPoints() > 1) { // laser tracking mode and already 2 hits on this track

		    if (TrackAngle(start_hit, hit, backward) <= mMaxAngleTracklet[mVertexConstraint]) {  // angle between last two hits and new within limits
		      
		      closest_dist = dist;
		      closest_hit = hit;
		    }
		    
		    else {  // angle not in limits
		      continue;
		    }
		  }
		  
		  else { // no laser mode or laser mode but less the two points on the track up to now
		    closest_dist = dist;
		    closest_hit = hit;
		  }
		}
		
		else {  // sub hit was farther away than a hit before
		  continue;
		} 
	      }
	    }
	    
	    else continue;  // sub hit was used before
	  }
	}
	
	else continue;  // no sub hits
      }
    }
    
    
    if ((coeff && (closest_circle_hit || closest_length_hit)) || ((!coeff) && closest_hit)) {
      
      if ((start_row - sub_row_segm) >= 1) {
	
	if (coeff) {
	  mMergedTracks++;
	}
	
	else {
	  mMergedTracklets++;
	}
      }
      
      // found a hit in a sub layer - don't look in other sub layers
      break;
    }
    
    else {
      // didn't find a hit in this sub layer - try next sub layer
      continue;
    }
  }		
  
  if (coeff) {
    
    if (closest_circle_hit && closest_length_hit) { // hits are not zero
      
      if (closest_circle_hit == closest_length_hit) { // both found hits are identical
	
	if (VerifyCuts(start_hit, closest_circle_hit, backward)) {

	  // closest hit within limits found
	  return closest_circle_hit;
	}
	
	else {  // limits exceeded
	  return 0;
	}
      }
      
      else {  // found hits are different
	mDiffHits++;

	Bool_t cut_circle = VerifyCuts(start_hit, closest_circle_hit, backward);
	Bool_t cut_length = VerifyCuts(start_hit, closest_length_hit, backward);

	if (cut_circle && cut_length) { // both hits are within the limit

	  if (GetDistanceFromFit(closest_circle_hit) < GetDistanceFromFit(closest_length_hit)) { // circle_hit is closer
	    return closest_circle_hit;
	  }

	  else { // length_hit is closer
	    return closest_length_hit;
	  }
	}

	else if (!(cut_circle || cut_length)) { // both hits exceed limits
	  return 0;
	}
	  
	else if (cut_circle) { // closest_circle_hit is the only one within limits
	  return closest_circle_hit;
	}

	else { // closest_length_hit is the only one within limits
	  return closest_length_hit;
	} 
      }
    }

    else { // no hits found
      return 0;
    }
  }
  
  
  else { // closest hit for tracklet found

    if (closest_hit) { // closest hit exists
      return closest_hit;
    }
      
    else { // hit does not exist
      return 0;
    }
  }
}


void StFtpcConfMapper::ExtendTracks()
{
  // Loops over all found tracks and passes them to the part of the program where each track is tried to be extended.
  
  for (Int_t t = 0; t < mTrack->GetEntriesFast(); t++) {
    
    StFtpcTrack *track = (StFtpcTrack *)mTrack->At(t);
  
    if (track->GetHemisphere() == 1) {
      mFtpc = (Char_t)1;
    }
    
    else {
      mFtpc = (Char_t)2;
    }
    
    if (TrackExtension(track)) {
      mExtendedTracks++;
    }
  }

  return;
}


Bool_t StFtpcConfMapper::TrackExtension(StFtpcTrack *track)
{
  // Trys to extend a given track.

  Int_t point;
  Int_t number_of_points = track->GetNumberOfPoints();
  Double_t *coeff = NULL;

  StFtpcConfMapPoint *closest_hit;
  StFtpcConfMapPoint *hit;
  TObjArray *trackpoint = track->GetHits();

  for (Int_t direction = 0; direction < 2; direction ++) {
    
    for (point = number_of_points; point < mMaxFtpcRow; point++) {
   
      if (direction == 1) { // backward
	hit = (StFtpcConfMapPoint *)trackpoint->Last();
      }
      
      else { // forward
	hit = (StFtpcConfMapPoint *)trackpoint->First();
      }
      
      Int_t padrow = hit->GetPadRow()%10;

      if ((padrow !=  1 && direction == 1) ||
	  (padrow !=  0 && direction == 0)) {
	
	if (GetLaser()) { // Laser event

	  if ((closest_hit = GetNextNeighbor(hit, coeff, (Bool_t)direction))) {
	   
	    // add closest hit to track
	    if(direction == 1) { // backward
	      track->AddPoint(closest_hit);
	    }
	    
	    else { // forward
	      track->AddForwardPoint(closest_hit);
	    }
	  }
	}

	else { // usual event

	  if (!coeff) coeff = new Double_t[6];
	  StraightLineFit(track, coeff);
	  
	  if ((closest_hit = GetNextNeighbor(hit, coeff, (Bool_t)direction))) {
	    
	    // add closest hit to track
	    if(direction == 1) { // backward
	      track->AddPoint(closest_hit);
	    }
	    
	    else { // forward
	      track->AddForwardPoint(closest_hit);
	    }
	  }

	  else { 
	    point = mMaxFtpcRow; // continue with next hit in segment
	  }
	}
      }
    }
  }    

  // cleanup
  if (coeff) {
    delete[] coeff; 
    coeff = NULL;
  }

  if (track->GetNumberOfPoints() - number_of_points) {
    
    track->SetPointDependencies();
    track->ComesFromMainVertex(mVertexConstraint);
    track->CalculateNMax();

    mClustersUnused -= (track->GetNumberOfPoints() - number_of_points);

    return (Bool_t)true;
  }
  
  else {
    return (Bool_t)false;
  } 
}


void StFtpcConfMapper::TrackingInfo()
{
  // Information about the tracking process.
  
  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("Tracking information", "I", "OST");
  gMessMgr->Message("--------------------", "I", "OST");
  
  gMessMgr->Message("", "I", "OST");
  gMessMgr->width(5);
  *gMessMgr << GetNumberOfTracks() << " (";
  gMessMgr->width(5);
  *gMessMgr << GetNumMainVertexTracks() << "/";
  gMessMgr->width(5);
  *gMessMgr << GetNumberOfTracks() - GetNumMainVertexTracks() << ") tracks (main vertex/non vertex) found." << endm;
  
  gMessMgr->Message("", "I", "OST");
  gMessMgr->width(5);
  *gMessMgr << GetNumberOfClusters() << " (";
  gMessMgr->width(5);
  *gMessMgr << GetNumberOfClusters() - GetNumClustersUnused() << "/";
  gMessMgr->width(5);
  *gMessMgr << GetNumClustersUnused() << ") clusters (used/unused)." << endm;

  gMessMgr->Message("", "I", "OST") << "       ";
  gMessMgr->width(5);
  *gMessMgr << GetNumMergedTracks() << "/";
  gMessMgr->width(5);
  *gMessMgr << GetNumMergedTracklets() << "  tracks/tracklets merged." << endm;

  gMessMgr->Message("", "I", "OST");
  gMessMgr->width(18);
  *gMessMgr << GetNumDiffHits() << "  times different hits for circle and length fit found." << endm;

  gMessMgr->Message("", "I", "OST");
  gMessMgr->width(18);
  *gMessMgr << GetNumLengthFitNaN() << "  times argument of arcsin set to +/-1." << endm;

  gMessMgr->Message("", "I", "OST");
  gMessMgr->width(18);
  *gMessMgr << GetNumExtendedTracks() << "  tracks extended." << endm;

  gMessMgr->Message("", "I", "OST");
  gMessMgr->width(18);
  *gMessMgr << GetNumMergedSplits() << "  split tracks merged." << endm;

  return;
}


void StFtpcConfMapper::CutInfo()
{
  // Information about cuts.

  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "Cuts for main vertex constraint on / off" << endm;
  gMessMgr->Message("", "I", "OST") << "----------------------------------------" << endm;
  gMessMgr->Message("", "I", "OST") << "Max. angle between last three points of tracklets:  "; 
  
  gMessMgr->width(6);
  *gMessMgr <<  mMaxAngleTracklet[1] << " / "; 
  gMessMgr->width(6);
  *gMessMgr <<  mMaxAngleTracklet[0] << endm;
  
  gMessMgr->Message("", "I", "OST") << "Max. angle between last three points of tracks:     "; 
  gMessMgr->width(6);
  *gMessMgr <<  mMaxAngleTrack[1] << " / "; 
  gMessMgr->width(6);
  *gMessMgr <<  mMaxAngleTrack[0] << endm;
                      
  gMessMgr->Message("", "I", "OST") << "Max. distance between circle fit and trackpoint:    "; 
  gMessMgr->width(6);
  *gMessMgr <<  mMaxCircleDist[1] << " / "; 
  gMessMgr->width(6);
  *gMessMgr <<  mMaxCircleDist[0] << endm;

  gMessMgr->Message("", "I", "OST") << "Max. distance between length fit and trackpoint:    "; 
  gMessMgr->width(6);
  *gMessMgr <<  mMaxLengthDist[1] << " / "; 
  gMessMgr->width(6);
  *gMessMgr <<  mMaxLengthDist[0] << endl;

  return;
}


void StFtpcConfMapper::SettingInfo()
{
  // Information about settings.

  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "Settings for main vertex constraint on / off" << endm;
  gMessMgr->Message("", "I", "OST") << "--------------------------------------------" << endm;
  gMessMgr->Message("", "I", "OST") << "Points required to create a tracklet:                " << mTrackletLength[1] << " / " << mTrackletLength[0] << endm;
  gMessMgr->Message("", "I", "OST") << "Points required for a track:                         " << mMinPoints[1] << " / " << mMinPoints[0] << endm;  
  gMessMgr->Message("", "I", "OST") << "Subsequent padrows to look for next tracklet point:  " << mRowScopeTracklet[1] << " / " << mRowScopeTracklet[0] << endm; 
  gMessMgr->Message("", "I", "OST") << "Subsequent padrows to look for next track point:     " << mRowScopeTrack[1] << " / " << mRowScopeTrack[0] << endm;
  gMessMgr->Message("", "I", "OST") << "Adjacent phi segments to look for next point:        " << mPhiScope[1] << " / " << mPhiScope[0] << endm;
  gMessMgr->Message("", "I", "OST") << "Adjacent eta segments to look for next point:        " << mEtaScope[1] << " / " << mEtaScope[0] << endm;

  return;
}


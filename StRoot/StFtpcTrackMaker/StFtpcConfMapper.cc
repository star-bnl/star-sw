// $Id: StFtpcConfMapper.cc,v 1.6 2000/07/03 12:39:21 jcs Exp $
// $Log: StFtpcConfMapper.cc,v $
// Revision 1.6  2000/07/03 12:39:21  jcs
// correct comment
//
// Revision 1.5  2000/06/13 14:34:25  oldi
// Excluded function call to TrackLoop().
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
// Introduction of new function TrackLoop() and ExtendTrack() to get cleaner
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
//----------Last Modified: 13.06.2000
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

  mBench = NULL;
  mHit = NULL;
  mVolume = NULL;
  mSegment = NULL;

  mVertexConstraint = (Bool_t)true;
}


StFtpcConfMapper::StFtpcConfMapper(St_fcl_fppoint *const fcl_fppoint, Double_t vertexPos[3], Bool_t bench, 
				   Int_t phi_segments, Int_t eta_segments) 
  : StFtpcTracker(fcl_fppoint, vertexPos)
{
  // Constructor.

  if (bench) { 
    mBench = new TBenchmark();
    mBench->Start("init");
  }

  mNumRowSegment = 20;   // The number of rows has to be fixed to 20 (because this is the number of rows in both Ftpc's)!
  mNumPhiSegment = phi_segments; 
  mNumEtaSegment = eta_segments; 
  mBounds = mNumRowSegment * mNumPhiSegment * mNumEtaSegment;
  mMaxFtpcRow = mNumRowSegment/2;

  CalcEtaMinMax();

  mMergedTracks = 0;
  mMergedTracklets = 0;
  mMergedSplits = 0;
  mDiffHits = 0;
  mDiffHitsStill = 0;
  mLengthFitNaN  = 0;

  Int_t n_clusters = fcl_fppoint->GetNRows();          // number of clusters
  mClustersUnused = n_clusters;

  fcl_fppoint_st *point_st = fcl_fppoint->GetTable();  // pointer to first cluster structure

  mHit = new TClonesArray("StFtpcConfMapPoint", n_clusters);    // create TClonesArray

  TClonesArray &hit = *mHit;
  
  for (Int_t i = 0; i < n_clusters; i++) {
    new(hit[i]) StFtpcConfMapPoint(point_st++, mVertex);
    ((StFtpcConfMapPoint *)mHit->At(i))->SetHitNumber(i);
  }

  mVolume = new TObjArray(mBounds, 0);  // create ObjArray for volume cells (of size bounds)

  for (Int_t i = 0; i < mBounds; i++) {
    mSegment = new TObjArray(0, 0);     // Fill ObjArray with empty ObjArrays
    mVolume->AddLast(mSegment);
  }

  StFtpcConfMapPoint *h;

  for (Int_t i = 0; i < mHit->GetEntriesFast(); i++) {
    h = (StFtpcConfMapPoint *)mHit->At(i);   
    ((TObjArray *)mVolume->At(GetSegm(GetRowSegm(h), GetPhiSegm(h), GetEtaSegm(h))))->AddLast(h);
  }

  if (mBench) {
    mBench->Stop("init");
    gMessMgr->Message("", "I", "OST") << "Setup finished                (" << mBench->GetCpuTime("init") << " s)." << endm;
  }
}


StFtpcConfMapper::~StFtpcConfMapper()
{
  // Destructor.

  if (mVolume) {
    mVolume->Delete();
    delete mVolume;
  }
    
  if (mHit) {
    mHit->Delete();
    delete mHit;
  }

  if (mBench) {
    delete mBench;
  }
}


void StFtpcConfMapper::MainVertexTracking()
{
  // Tracking with vertex constraint.

  if (mBench) {
    mBench->Start("main_vertex");
  }

  SetVertexConstraint(true);
  ClusterLoop();
  //TrackLoop();
  
  if (mBench) {
    mBench->Stop("main_vertex");
    gMessMgr->Message("", "I", "OST") << "Main vertex tracking finished (" << mBench->GetCpuTime("main_vertex") << " s)." << endm;
  }
  
  return;
}


void StFtpcConfMapper::FreeTracking()
{
  // Tracking without vertex constraint.

  if (mBench) {
    mBench->Start("non_vertex");
  }

  SetVertexConstraint(false);
  ClusterLoop();

  if (mBench) {
    mBench->Stop("non_vertex");
    gMessMgr->Message("", "I", "OST") << "Non vertex tracking finished  (" << mBench->GetCpuTime("non_vertex") << " s)." << endm;
  }
  
  return;
}


void StFtpcConfMapper::TwoCycleTracking()
{
  // Tracking in 2 cycles:
  // 1st cycle: tracking with vertex constraint
  // 2nd cycle: without vertex constraint (of remaining clusters)Begin_Html<a name="settings"></a>End_Html

  MainVertexTracking();
  FreeTracking();
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


void StFtpcConfMapper::CalcEtaMinMax()
{
  // Calculates the min. and max. value of eta (pseudorapidity) with the given main vertex.
  // The FTPCs are placed in a distance to the point of origin between 162.75 and 256.45 cm.
  // Their inner radius is 8, the outer one 30 cm. This means they are seen from (0, 0, 0) 
  // under an angle between 1.79 and 10.62 degrees which translates directly into max. eta and min. eta. 
  // Due to the fact that the main vertex is shifted, the values of min./max. eta is calculated for each 
  // event. To be save, 0.01 is substracted/added.
  
  mEtaMin = -TMath::Log(TMath::Tan( TMath::ASin(30./ (162.75 - TMath::Abs(mVertex->GetZ()) ) ) /2.) ) - 0.01;
  mEtaMax = -TMath::Log(TMath::Tan( TMath::ASin( 8./ (256.45 + TMath::Abs(mVertex->GetZ()) ) ) /2.) ) + 0.01;

  return;
}


Int_t StFtpcConfMapper::GetRowSegm(StFtpcConfMapPoint *hit)
{
 // Returns number of pad segment of a specific hit.

  return hit->GetPadRow() - 1;  // fPadRow (1-20) already segmented, only offset substraction
}


Int_t StFtpcConfMapper::GetPhiSegm(StFtpcConfMapPoint *hit)
{
  // Returns number of phi segment of a specific hit.
  
  return (Int_t)(hit->GetPhi()  * mNumPhiSegment / (2.*TMath::Pi())); // fPhi has no offset but needs to be segmented (this is done by type conversion to Int_t)
}


Int_t StFtpcConfMapper::GetEtaSegm(StFtpcConfMapPoint *hit)
{
  // Returns number of eta segment of a specific hit.

  Double_t eta;
  Int_t eta_segm;
  
  if ((eta = hit->GetEta()) > 0.) {  // positive values
    eta_segm = (Int_t)((eta - mEtaMin) * mNumEtaSegment/(mEtaMax - mEtaMin) /2.); // Only use n_eta_segm/2. bins because of negative eta values.
  }

  else {                             // negative eta values
    eta_segm = (Int_t)((-eta -  mEtaMin) * mNumEtaSegment/(mEtaMax - mEtaMin) /2. + mNumEtaSegment/2.);
  }
  
  return eta_segm;
}


Int_t StFtpcConfMapper::GetSegm(Int_t row_segm, Int_t phi_segm, Int_t eta_segm)
{
  // Calculates the volume segment number from the segmented volumes (segm = segm(pad,phi,eta)).

  return row_segm * (mNumPhiSegment * mNumEtaSegment) + phi_segm * (mNumEtaSegment) + eta_segm;
}


Int_t StFtpcConfMapper::GetRowSegm(Int_t segm)
{
  // Returns number of pad segment of a specifiv segment.

  return (segm - GetEtaSegm(segm) - GetPhiSegm(segm)) / (mNumPhiSegment * mNumEtaSegment);
}


Int_t StFtpcConfMapper::GetPhiSegm(Int_t segm)
{
  // Returns number of phi segment of a specifiv segment.

  return (segm - GetEtaSegm(segm)) % (mNumPhiSegment * mNumEtaSegment) / (mNumEtaSegment);
}


Int_t StFtpcConfMapper::GetEtaSegm(Int_t segm)
{
  // Returns number of eta segment of a specifiv segment.

  return (segm % (mNumPhiSegment * mNumEtaSegment)) % (mNumEtaSegment);
}


Double_t const StFtpcConfMapper::CalcDistance(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2)
{
  // Returns the distance of two given clusters. The distance in this respect (conformal mapping)
  // is defined in the paper "A Fast track pattern recognition" by Pablo Yepes, NIM A 380 (1996) 585-585.
  
  Double_t phi_diff = TMath::Abs( hit1->GetPhi() - hit2->GetPhi() );
  if (phi_diff > TMath::Pi()) phi_diff = 2*TMath::Pi() - phi_diff;
  
  return TMath::Abs(hit1->GetPadRow() - hit2->GetPadRow()) *  (phi_diff + TMath::Abs( hit1->GetEta() - hit2->GetEta() ));
}


Double_t const StFtpcConfMapper::CalcDistance(const StFtpcConfMapPoint *hit, Double_t *coeff) 
{
  // Returns the distance of a point to a straight line.
  // The point is given by the to conformal coordinates of a cluster and the
  // straight line is given by its to coefficients: y = coeff[0]*x + coeff[1].

  Double_t x = (coeff[0] / (1 + coeff[0]*coeff[0])) * (1/coeff[0] * hit->GetXprime() + hit->GetYprime() - coeff[1]);

  return TMath::Sqrt(TMath::Power(x - hit->GetXprime(), 2) + TMath::Power(coeff[0]*x + coeff[1] - hit->GetYprime(), 2));
} 


Bool_t const StFtpcConfMapper::VerifyCuts(const StFtpcConfMapPoint *lasttrackhit, const StFtpcConfMapPoint *newhit)
{
  // Returns true if circle, length, and angle cut holds.
  
  if (newhit->GetCircleDist() < mMaxCircleDist[mVertexConstraint] &&
      newhit->GetLengthDist() < mMaxLengthDist[mVertexConstraint] &&
      TrackAngle(lasttrackhit, newhit) < mMaxAngleTrack[mVertexConstraint]) {
    return true;
  }
  
  else {
    return false;
  }
}


Double_t const StFtpcConfMapper::TrackAngle(const StFtpcPoint *lasthitoftrack, const StFtpcPoint *hit)
{
  // Returns the 'angle' between the last two points on the track (of which the last point is
  // given as input) and the second given point.
  
  StFormulary f;

  Double_t x1[3];
  Double_t x2[3];
  StFtpcTrack *track = lasthitoftrack->GetTrack(mTrack);
  TObjArray *hits = track->GetHits();
  Int_t n = track->GetNumberOfPoints();
  
  if (n<2) {
    gMessMgr->Message("StFtpcConfMapper::TrackAngle(StFtpcPoint *lasthitoftrack, StFtpcPoint *hit)", "E", "OST");    
    gMessMgr->Message(" - Call this function only if you are sure to have at least two points on the track already!", "E", "OST");

    return false;
  }

  x1[0] = ((StFtpcPoint *)hits->At(n-1))->GetX() - ((StFtpcPoint *)hits->At(n-2))->GetX();
  x1[1] = ((StFtpcPoint *)hits->At(n-1))->GetY() - ((StFtpcPoint *)hits->At(n-2))->GetY();
  x1[2] = ((StFtpcPoint *)hits->At(n-1))->GetZ() - ((StFtpcPoint *)hits->At(n-2))->GetZ();

  x2[0] = hit->GetX() - ((StFtpcPoint *)hits->At(n-1))->GetX();
  x2[1] = hit->GetY() - ((StFtpcPoint *)hits->At(n-1))->GetY();
  x2[2] = hit->GetZ() - ((StFtpcPoint *)hits->At(n-1))->GetZ();

  return f.Angle(x1, x2, 3);
}


Double_t const StFtpcConfMapper::TrackletAngle(StFtpcTrack *track, Int_t n)
{
  // Returns the angle 'between' the last three points (started at point number n) on this track.

  StFormulary f;

  Double_t x1[3];
  Double_t x2[3];  
  TObjArray *hits = track->GetHits();  
  if (n > track->GetNumberOfPoints()) {
    n = track->GetNumberOfPoints();
  }

  if (n<3) {
    gMessMgr->Message("StFtpcConfMapper::TrackletAngle(StFtpcTrack *track, Int_t n)", "E", "OST");
    gMessMgr->Message(" - Call this function only if you are sure to have at least three points on this track already!", "E", "OST");

    return false;
  }
    
  x1[0] = ((StFtpcPoint *)hits->At(n-2))->GetX() - ((StFtpcPoint *)hits->At(n-3))->GetX();
  x1[1] = ((StFtpcPoint *)hits->At(n-2))->GetY() - ((StFtpcPoint *)hits->At(n-3))->GetY();
  x1[2] = ((StFtpcPoint *)hits->At(n-2))->GetZ() - ((StFtpcPoint *)hits->At(n-3))->GetZ();

  x2[0] = ((StFtpcPoint *)hits->At(n-1))->GetX() - ((StFtpcPoint *)hits->At(n-2))->GetX();
  x2[1] = ((StFtpcPoint *)hits->At(n-1))->GetY() - ((StFtpcPoint *)hits->At(n-2))->GetY();
  x2[2] = ((StFtpcPoint *)hits->At(n-1))->GetZ() - ((StFtpcPoint *)hits->At(n-2))->GetZ();  
  
  return f.Angle(x1, x2, 3);
}


Double_t const StFtpcConfMapper::GetPhiDiff(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2)
{
  // Returns the difference in angle phi of the two given clusters.
  // Normalizes the result to the arbitrary angle between two subsequent padrows.

  Double_t angle = TMath::Abs(hit1->GetPhi() - hit2->GetPhi());
  if (angle > TMath::Pi()) angle = 2*TMath::Pi() - angle;
  
  Double_t row_diff = TMath::Abs(hit1->GetPadRow() - hit2->GetPadRow());

  return angle/row_diff;
}


Double_t const StFtpcConfMapper::GetEtaDiff(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2)
{
  // Returns the difference in pseudrapidity eta of the two given clusters.
  // Normalizes the result to the arbitrary pseudorapidity between two subsequent padrows.

  Double_t eta = TMath::Abs(hit1->GetEta() - hit2->GetEta());
  Double_t row_diff = TMath::Abs(hit1->GetPadRow() - hit2->GetPadRow());

  return eta/row_diff;
}


Double_t const StFtpcConfMapper::GetClusterDistance(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2)
{
  // Returns the distance of two clusters measured in terms of angle phi and pseudorapidity eta weighted by the
  // maximal allowed values for phi and eta.

  return TMath::Sqrt(TMath::Power(GetPhiDiff(hit1, hit2)/mMaxCircleDist[mVertexConstraint], 2) + TMath::Power(GetEtaDiff(hit1, hit2)/mMaxLengthDist[mVertexConstraint], 2));
}


Double_t const StFtpcConfMapper::GetDistanceFromFit(const StFtpcConfMapPoint *hit)
{
  // Returns the distance of the given cluster to the track to which it probably belongs.
  // The distances to the circle and length fit are weighted by the cuts on these values.
  // Make sure that the variables mCircleDist and mLengthDist for the hit are set already.

  return TMath::Sqrt(TMath::Power((hit->GetCircleDist() / mMaxCircleDist[mVertexConstraint]), 2) + TMath::Power((hit->GetLengthDist() / mMaxLengthDist[mVertexConstraint]), 2));
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

  Int_t start_counter = 0;
  
  if (!mVertexConstraint) {
    start_counter = 1;
  }

  // Circle Fit
  StFtpcConfMapPoint *trackpoint;

  for (Int_t i = start_counter; i < n; i++) {
    trackpoint = (StFtpcConfMapPoint *)trackpoints->At(i);

    L11 += 1./* / (trackpoint->GetYprimeerr() * trackpoint->GetYprimeerr())*/;
    L12 +=  trackpoint->GetXprime()/* / (trackpoint->GetYprimeerr() * trackpoint->GetYprimeerr())*/;
    L22 += (trackpoint->GetXprime() * trackpoint->GetXprime())/* / (trackpoint->GetYprimeerr() * trackpoint->GetYprimeerr())*/;
    g1  +=  trackpoint->GetYprime()/* / (trackpoint->GetYprimeerr() * trackpoint->GetYprimeerr())*/;
    g2  += (trackpoint->GetXprime() * trackpoint->GetYprime())/* / (trackpoint->GetYprimeerr() * trackpoint->GetYprimeerr())*/;
  }

  Double_t D = L11*L22 - L12*L12;
  
  a[0] = (g2*L11 - g1*L12)/D;
  a[1] = (g1*L22 - g2*L12)/D;

  // Set circle parameters
  track->SetCenterX(- a[0] / (2. * a[1]) + trackpoint->GetXt());
  track->SetCenterY(-  1.  / (2. * a[1]) + trackpoint->GetYt());
  track->SetRadius(TMath::Sqrt(a[0]*a[0] + 1.) / (2. * TMath::Abs(a[1])));
  track->SetAlpha0(TMath::ASin((trackpoint->GetYt() - track->GetCenterY()) / track->GetRadius()));
    
  // Tracklength Fit
  Double_t s;
  Double_t asin_arg;

  // Set variables again
  // first track point is main vertex or first track point (both at (0, 0, 0) [shifted coordinates])
  L11 = 1.;
  L12 = L22 = g1 = g2 = 0.;

  for (Int_t i = start_counter; i < n; i++ ) {
    
    trackpoint= (StFtpcConfMapPoint *)trackpoints->At(i);
    
    asin_arg = (trackpoint->GetYv() - track->GetCenterY()) / track->GetRadius();

    // The following lines were inserted because ~1% of all tracks produce arguments of arcsin 
    // which are above the |1| limit. But they were differing only in the 5th digit after the point.
    if (TMath::Abs(asin_arg) > 1.) {
      asin_arg = (asin_arg >= 0) ? +1. : -1.;
      mLengthFitNaN++;
    }

    s = TMath::Sqrt(TMath::Power(track->GetRadius() * (TMath::ASin(asin_arg) - track->GetAlpha0()), 2) + 
		     TMath::Power(trackpoint->GetZv() - trackpoint->GetZt(), 2));
    
    L11 += 1;
    L12 += trackpoint->GetZv();
    L22 += (trackpoint->GetZv() * trackpoint->GetZv());
    g1  += s;
    g2  += (s * trackpoint->GetZv());
  }
    
  D = L11*L22 - L12*L12;

  a[2] = (g2*L11 - g1*L12)/D;
  a[3] = (g1*L22 - g2*L12)/D;

  // Set chi squared
  Double_t chi2circle = 0.;
  Double_t chi2length = 0.;
  
  for (Int_t i = start_counter; i < n; i++) {
    trackpoint = (StFtpcConfMapPoint *)trackpoints->At(i);
    asin_arg = (trackpoint->GetYv() - track->GetCenterY()) / track->GetRadius();

    if (TMath::Abs(asin_arg) > 1.) {
      asin_arg = (asin_arg >= 0) ? +1. : -1.;
    }
    
    s = TMath::Sqrt(TMath::Power(track->GetRadius() * (TMath::ASin(asin_arg) - track->GetAlpha0()), 2) + 
		    TMath::Power(trackpoint->GetZv() - trackpoint->GetZt(), 2));

    chi2circle += TMath::Power(trackpoint->GetYprime() - a[0]*trackpoint->GetXprime()+a[1], 2.) / (a[0]*trackpoint->GetXprime()+a[1]);    
    chi2length += TMath::Power(s - a[2]*trackpoint->GetZv()+a[3], 2.) / (a[2]*trackpoint->GetZv()+a[3]);
  }


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

      if (!(t1->GetRowsWithPoints() & t2->GetRowsWithPoints()) && 
	  (t1->GetHemisphere() == t2->GetHemisphere()) &&
	  dist <= max_dist && ratio <= ratio_max && ratio >= ratio_min) {
	MergeSplitTracks(t1, t2);
      }      
    }
  }

  mTrack->Compress();
  mTrack->Expand(mTrack->GetLast()+1);
    
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

  TClonesArray &track_init = *mTrack;
  new(track_init[new_track_number]) StFtpcTrack();
  StFtpcTrack *track = (StFtpcTrack *)mTrack->At(new_track_number);

  TObjArray *trackpoint = track->GetHits();
  trackpoint->Expand(mMaxFtpcRow);

  MIntArray *trackhitnumber = track->GetHitNumbers();  

  for (Int_t i = 0; i < mMaxFtpcRow; i++) {

    if (i < num_t1_points) {
      trackpoint->AddAt(t1_points->At(i), mMaxFtpcRow - 1 -(((StFtpcPoint *)t1_points->At(i))->GetPadRow()-1)%mMaxFtpcRow);
    } 
   
    if (i < num_t2_points) {
      trackpoint->AddAt(t2_points->At(i), mMaxFtpcRow - 1 -(((StFtpcPoint *)t2_points->At(i))->GetPadRow()-1)%mMaxFtpcRow);
    } 
  }
 
  trackpoint->Compress();
  trackpoint->Expand(trackpoint->GetLast()+1);

  for (Int_t i = 0; i < trackpoint->GetEntriesFast(); i++) {
    trackhitnumber->AddLast(((StFtpcPoint *)trackpoint->At(i))->GetHitNumber());
  }
  
  track->SetProperties(true, new_track_number);
  track->ComesFromMainVertex(mVertexConstraint);
  track->CalculateNMax();

  if (mVertexConstraint) mMainVertexTracks++;
  if (t1->ComesFromMainVertex()) mMainVertexTracks--;
  if (t2->ComesFromMainVertex()) mMainVertexTracks--;

  mTrack->Remove(t1);
  mTrack->Remove(t2);

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

  TClonesArray &track_init = *mTrack;
  new(track_init[tracks]) StFtpcTrack();
  track = (StFtpcTrack *)mTrack->At(tracks);
  
  TObjArray *trackpoint = track->GetHits();
  MIntArray *trackhitnumber = track->GetHitNumbers();
  trackpoint->AddLast(hit);                // add address of first cluster to cluster list
  trackhitnumber->AddLast(hit->GetHitNumber()); // add number of first cluster to number list
  
  // set conformal mapping coordinates if looking for non vertex tracks
  if (!mVertexConstraint) {
    hit->SetAllCoord(hit);
  }
  
  // create tracklets
  for (point = 1; point < mTrackletLength[mVertexConstraint]; point++) {
    
    if ((closest_hit = GetNextNeighbor(hit, coeff))) {
      
      // closest_hit for hit exists
      trackhitnumber->AddLast(closest_hit->GetHitNumber()); 
      trackpoint->AddLast(closest_hit);
      hit = closest_hit;
    }
    
    else {  
      
      // closest hit does not exist
      mTrack->Remove(track);  // remove track
      point = mTrackletLength[mVertexConstraint];  // continue with next hit in segment
    }
  }
  
  // tracklet is long enough to be extended to a track
  if (trackpoint->GetEntriesFast() == mTrackletLength[mVertexConstraint]) {
    
    track->SetProperties(true, tracks); // set properties for tracklet 
    // (otherwise TrackletAngle() does not work properly)
    
    if (TrackletAngle(track) > mMaxAngleTracklet[mVertexConstraint]) { // proof if the first points seem to be a beginning of a track
      track->SetProperties(false, tracks); // set usage of the clusters to "unused"
      mTrack->Remove(track);  // remove track
    }
    
    else { // good tracklet -> proceed
      tracks++;
      
      // create tracks 
      for (point = mTrackletLength[mVertexConstraint]; point < mMaxFtpcRow; point++) {
	
	if (!coeff) coeff = new Double_t[6];
	//Double_t chi_circ = track->GetChi2Circle();
	//Double_t chi_len  = track->GetChi2Length();
	
	StraightLineFit(track, coeff);
	closest_hit = GetNextNeighbor((StFtpcConfMapPoint *)trackpoint->Last(), coeff);
	
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
	  trackhitnumber->AddLast(closest_hit->GetHitNumber());
	  trackpoint->AddLast(closest_hit);
	  closest_hit->SetUsage(true);
	  closest_hit->SetTrackNumber(tracks-1);
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
	track->SetProperties(false, tracks-1); // set usage of the clusters to "unused"
	mTrack->Remove(track);    // remove track
	tracks--;
      }      
      
      else {
	mClustersUnused -= track->GetNumberOfPoints();
	track->ComesFromMainVertex(mVertexConstraint); // mark track as main vertex track or not
	track->CalculateNMax();
	if (mVertexConstraint) mMainVertexTracks++;
      }
      
      // cleanup
      delete[] coeff; 
      coeff = NULL;
    } 
  } 	

  return;
}


StFtpcConfMapPoint *StFtpcConfMapper::GetNextNeighbor(StFtpcConfMapPoint *start_hit, Double_t *coeff = NULL)
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

  Int_t max_row = GetRowSegm(start_hit) - 1;
  Int_t min_row;
  
  if (coeff) {
    min_row = GetRowSegm(start_hit) - mRowScopeTrack[mVertexConstraint];
  }
  
  else {
    min_row = GetRowSegm(start_hit) - mRowScopeTracklet[mVertexConstraint];
  }
  
  // loop over sub volume
  while (min_row < (mFtpc-1) * mMaxFtpcRow) {
    min_row++;
  }
  
  if (max_row < min_row) return 0;
  
  else {
    
    // loop over sub rows
    for (sub_row_segm = max_row; sub_row_segm >= min_row; sub_row_segm--) {
      
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
		  if ((dist = CalcDistance(start_hit, hit)) < closest_dist) { 
		    closest_dist = dist;
		    closest_hit = hit;
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
	
	if ((max_row - sub_row_segm) >= 1) {
	  
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
  }
    
  if (coeff) {
    
    if (closest_circle_hit && closest_length_hit) { // hits are not zero
      
      if (closest_circle_hit == closest_length_hit) { // both found hits are identical
	
	if (VerifyCuts(start_hit, closest_circle_hit)) {

	  // closest hit within limits found
	  return closest_circle_hit;
	}
	
	else {  // limits exceeded
	  return 0;
	}
      }
      
      else {  // found hits are different
	mDiffHits++;

	Bool_t cut_circle = VerifyCuts(start_hit, closest_circle_hit);
	Bool_t cut_length = VerifyCuts(start_hit, closest_length_hit);

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


void StFtpcConfMapper::TrackLoop()
{
  // Loops over all found tracks and passes them to the part of the program where each track is tried to be extended.
  
  Int_t num_extended = 0;

  for (Int_t t = 0; t < mTrack->GetEntriesFast(); t++) {
    
    if (ExtendTrack((StFtpcTrack *)mTrack->At(t))) {
      num_extended++;
    }
  }
  
  gMessMgr->Message("", "I", "OST") << num_extended << " tracks extended" << endm;

  return;
}


Bool_t StFtpcConfMapper::ExtendTrack(StFtpcTrack *track)
{
  // Trys to extend a given track.

  Int_t point;
  Int_t number_of_points = track->GetNumberOfPoints();
  Int_t tracks = mTrack->IndexOf(track);
  Double_t *coeff = NULL;

  StFtpcConfMapPoint *closest_hit;
  TObjArray *trackpoint = track->GetHits();
  MIntArray *trackhitnumber = track->GetHitNumbers();

  for (point = number_of_points; point < mMaxFtpcRow; point++) {
    
    if (!coeff) coeff = new Double_t[6];
    StraightLineFit(track, coeff);
    closest_hit = GetNextNeighbor((StFtpcConfMapPoint *)trackpoint->Last(), coeff);
    
    if (closest_hit) {
      
      // add closest hit to track
      trackhitnumber->AddLast(closest_hit->GetHitNumber());
      trackpoint->AddLast(closest_hit);
      closest_hit->SetUsage(true);
      closest_hit->SetTrackNumber(tracks);
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
  
  // cleanup
  delete[] coeff; 
  coeff = NULL;

  if (track->GetNumberOfPoints() - number_of_points) {
    
    mClustersUnused -= (track->GetNumberOfPoints() - number_of_points);
    track->CalculateNMax();

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


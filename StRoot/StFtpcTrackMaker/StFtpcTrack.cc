// $Id: StFtpcTrack.cc,v 1.39 2014/07/24 23:07:59 jeromel Exp $
// $Log: StFtpcTrack.cc,v $
// Revision 1.39  2014/07/24 23:07:59  jeromel
// Fix for C++11 compliance
//
// Revision 1.38  2012/06/17 19:13:36  fisyak
// Resolve  ambiguity in TMath::Power
//
// Revision 1.37  2008/07/03 07:22:35  jcs
// improved LOG_WARN message
//
// Revision 1.36  2008/07/03 05:25:44  jcs
// exit momentum fit if plength >= NoSolution/2 for any hit
//
// Revision 1.35  2008/06/24 08:12:48  jcs
// If NoSolution found for MomentumFit with primary vertex, set mFromMainVertex = kFALSE for the track to avoid looping in StarMagField  3D field interpolation
//
// Revision 1.34  2008/06/11 18:41:31  fine
// Add FATAL_ERROR message
//
// Revision 1.33  2007/01/15 08:23:02  jcs
// replace printf, cout and gMesMgr with Logger commands
//
// Revision 1.32  2004/02/12 19:37:10  oldi
// *** empty log message ***
//
// Revision 1.31  2004/01/28 01:41:32  jeromel
// *** empty log message ***
//
// Revision 1.30  2003/11/26 11:34:45  jcs
// change Sign(Float_t, Double_t) to Sign(Double_t,Double_t)
//
// Revision 1.29  2003/09/16 15:27:02  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.28  2003/09/16 14:08:04  jeromel
// Removed inline to resolve undefined symbol in lib
//
// Revision 1.27  2003/09/02 17:58:16  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.26  2003/01/20 09:16:30  oldi
// Calculation of residuals simplified.
//
// Revision 1.25  2003/01/16 18:04:33  oldi
// Bugs eliminated. Now it compiles on Solaris again.
// Split residuals for global and primary fit.
//
// Revision 1.24  2002/11/28 09:39:25  oldi
// Problem in momentum fit eliminated. Negative vertex Id is not used anymore.
// It was used do decide for global or primary fit.
// Code was prepared to fill momentum values at outermost points on tracks.
// This feature is not used up to now.
// Code cleanups.
//
// Revision 1.23  2002/11/06 13:45:59  oldi
// Global/primary fit handling simplified.
// Code clean ups.
//
// Revision 1.22  2002/10/31 13:40:01  oldi
// Method GetSector() added.
// Method GetMeanR() and GetMeanAlpha() added.
// dca set to zero if no vertex was found (tracking done with arbitrary vertex at (0., 0., 0.))
// Code cleanup.
//
// Revision 1.21  2002/10/24 16:37:41  oldi
// dca (impact parameter) is calculated using StHelix::distance(vertexPos), now.
// Therefore it is the smallest three dimensional distance of the helix to the
// primary vertex.
// dca for primary tracks is filled correctly, now. (Even though this value
// shouldn't be of great use.)
// Code clean-ups.
//
// Revision 1.20  2002/10/11 15:45:14  oldi
// Get FTPC geometry and dimensions from database.
// No field fit activated: Returns momentum = 0 but fits a helix.
// Bug in TrackMaker fixed (events with z_vertex > outer_ftpc_radius were cut).
// QA histograms corrected (0 was supressed).
// Code cleanup (several lines of code changed due to *params -> Instance()).
// cout -> gMessMgr.
//
// Revision 1.19  2002/10/03 10:33:59  oldi
// Usage of gufld removed.
// Magnetic field is read by StMagUtilities, now.
//
// Revision 1.18  2002/04/29 15:50:01  oldi
// All tracking parameters moved to StFtpcTrackingParameters.cc/hh.
// In a future version the actual values should be moved to an .idl file (the
// interface should be kept as is in StFtpcTrackingParameters.cc/hh).
//
// Revision 1.17  2002/04/22 14:18:17  oldi
// Assume hit resolution to be 0.1mm if it is equal to 0.
//
// Revision 1.16  2002/04/08 15:37:59  oldi
// Switch for magnetic field factor installed.
// Minor corrections/improvements.
//
// Revision 1.15  2002/04/05 16:50:42  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.14  2002/02/21 22:57:57  oldi
// Fixes to avoid warnings during optimized compilation.
//
// Revision 1.13  2002/01/29 11:08:10  oldi
// Write() renamed to WriteCluster() resp. WriteTrack() to avoid compiler warnings.
// As a result the functions TObject::Write() are available again (directly).
//
// Revision 1.12  2001/05/04 09:19:58  oldi
// For non main vertex tracks the momentum of the fit with vertex constraint was
// stored. To fix this  Fit-> was changed to looseFit-> at the appropriate
// locations.
//
// Revision 1.11  2001/01/25 15:21:57  oldi
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
// Revision 1.10  2000/11/10 18:38:02  oldi
// Changes due to replacement of StThreeVector by TVector3.
// Points can be added to a track on either end now.
// New data members for dE/dx information.
// Cleanup.
//
// Revision 1.9  2000/09/07 11:35:39  jcs
// Set flag,id_start_vertex,nrec,nmax,nfit for FTPC primary tracks
//
// Revision 1.8  2000/07/18 21:22:16  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.7  2000/07/17 14:54:22  jcs
// save results of constrained fit
//
// Revision 1.6  2000/07/12 11:58:40  jcs
// calculate and save FTPC track parameters for unconstrained fit
//
// Revision 1.5  2000/07/03 12:42:57  jcs
// save (pre)Vertex id and unconstrained fit results
//
// Revision 1.4  2000/06/07 11:48:56  oldi
// Added function GetEta().
// In SetProperties(Bool_t usage, Int_t tracknumber): calculation of
// mRowsWithPoints added.
// CalculateNMax() changed. It calculates now the (arbitrary) angle and radius
// in the inner and outer padrow.
//
// Revision 1.3  2000/05/12 12:59:15  oldi
// removed delete operator for mSegment in StFtpcConfMapper (mSegment was deleted twice),
// add two new constructors for StFtpcTracker to be able to refit already existing tracks,
// minor cosmetics
//
// Revision 1.2  2000/05/11 15:14:50  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
// Revision 1.1  2000/05/10 13:39:22  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Holm G. H&uuml;mmler, Markus D. Oldenburg
//----------Last Modified: 10.11.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFtpcTrack.hh"
#include "StFtpcTrackingParams.hh"
#include "StFormulary.hh"
#include "StFtpcVertex.hh"
#include "StFtpcPoint.hh"
#include "StFtpcConfMapPoint.hh"

#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StMessMgr.h"

#include <math.h>

////////////////////////////////////////////////////////////////////////////////////
//                                                                                //
// StFtpcTrack class - representation of one track for the FTPC trackers.         //
//                                                                                //
// This class contains all data members which are the output of the FTPC tracker. //
//                                                                                //
////////////////////////////////////////////////////////////////////////////////////


ClassImp(StFtpcTrack)


StFtpcTrack::StFtpcTrack()
{
  // Default constructor.
  // Creates a ObjArray of the hits belonging to the track.

  SetDefaults();
}


StFtpcTrack::StFtpcTrack(Int_t tracknumber)
{
  // Same as default constructor except that the track number is set.
  
  SetDefaults();
  SetTrackNumber(tracknumber);
}


StFtpcTrack::~StFtpcTrack()
{
  // Destructor.

  delete mPoints;
  delete mPointNumbers;
}


void StFtpcTrack::SetDefaults()
{
  // Executes the default setup for the track.

  mPoints = new TObjArray(0);
  mPointNumbers = new MIntArray();

  mTrackNumber = -1;
  mGlobTrackId = -1;

  SetRadius(0.);
  SetCenterX(0.);
  SetCenterY(0.);
  SetAlpha0(0.);
  SetPid(0);
  SetNMax(0);

  ComesFromMainVertex(kFALSE);

  mP.SetX(0.);
  mP.SetY(0.);
  mP.SetZ(0.);

  mV.SetX(0.);
  mV.SetY(0.);
  mV.SetZ(0.);

  mL.SetX(0.);
  mL.SetY(0.);
  mL.SetZ(0.);

  mQ = 0;
  mChiSq[0] = 0.;
  mChiSq[1] = 0.;
  mTheta = 0.;
  mDca = 0.;

  mTrackLength = 0.;

  mdEdx = 0.;
  mNumdEdxHits = 0;

  return;
}


void StFtpcTrack::SetTrackNumber(Int_t number) 
{
  // Sets the tracknumber.
  // If the track has already some hits assigned the track number of the hits is also set.

  mTrackNumber = number;

  for (Int_t i = 0; i < mPoints->GetEntriesFast(); i++) {
    ((StFtpcPoint*)mPoints->At(i))->SetTrackNumber(number);    
  }

  return;
}


void StFtpcTrack::AddPoint(StFtpcPoint* point)
{
  // Adds a given point to the track.

  mPointNumbers->AddLast(point->GetHitNumber());
  mPoints->AddLast(point);
  point->SetUsage(kTRUE);
  point->SetTrackNumber(GetTrackNumber());

  return;
}


void StFtpcTrack::AddForwardPoint(StFtpcPoint* point)
{
  // Shifts all found points by one. Adds a given point in the (now) empty first slot.

  Int_t num = mPoints->GetEntriesFast();
  mPoints->Expand(num+1);

  for (Int_t i = num-1; i >= 0; i--) {
    mPoints->AddAt(mPoints->At(i), i+1);
  }
  
  mPoints->AddFirst(point);
  mPointNumbers->ShiftByOneAndAddAtFirst(point->GetHitNumber());
  point->SetUsage(kTRUE);
  point->SetTrackNumber(GetTrackNumber());

  return;
}


Int_t StFtpcTrack::GetHemisphere() const
{
  // Returns +1 if z of track is positiv, -1 otherwise.
  return (Int_t)TMath::Sign(1., ((StFtpcPoint *)(GetHits()->First()))->GetZ());  
}


Int_t StFtpcTrack::GetSector() const
{
  // Returns sector of track. Assumes that the track doesn't cross more than one sector.
  return ((StFtpcPoint *)GetHits()->First())->GetSector();  
}


Double_t StFtpcTrack::CalcAlpha0()
{
  // Calculates the starting angle (with respect to the x-axis) of xt.
  
  StFtpcConfMapPoint *trackpoint = (StFtpcConfMapPoint *)mPoints->First();
  Double_t asin_arg = StFormulary::CheckASinArg((trackpoint->GetYt() - GetCenterY())/GetRadius());
  Double_t alpha0 = 0.;

  if (trackpoint->GetXt() >= GetCenterX() && trackpoint->GetYt() > GetCenterY()) {
    alpha0 = TMath::ASin(asin_arg);
  }

  else if (trackpoint->GetXt() < GetCenterX() && trackpoint->GetYt() >= GetCenterY()) {
    alpha0 = -TMath::ASin(asin_arg) + TMath::Pi();
  }

  else if (trackpoint->GetXt() <= GetCenterX() && trackpoint->GetYt() < GetCenterY()) {
    alpha0 = TMath::ASin(-asin_arg) +  TMath::Pi();
  }

  else if (trackpoint->GetXt() > GetCenterX() && trackpoint->GetYt() <= GetCenterY()) {
    alpha0 = -TMath::ASin(-asin_arg) + 2 * TMath::Pi();
  }

  return alpha0;
}


void StFtpcTrack::SetProperties(Bool_t usage, Int_t tracknumber) 
{
  // Sets number of next hit. Counting is started from the vertex. (The tracker finds hits on tracks vice versa!)
  // Sets the usage of all points belonging to this track to the value of mUsage and
  // sets the track number of all points belonging to this track to the value of fTrackNumber.

  mRowsWithPoints = 0;
  
  for (Int_t i = 0; i < mPoints->GetEntriesFast(); i++) {    
    StFtpcConfMapPoint *p = (StFtpcConfMapPoint *)mPoints->At(i);

    if (usage == kTRUE) {
      mRowsWithPoints += (Int_t)TMath::Power(2., (Int_t)(((p->GetPadRow()-1)%StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide())+1));

      if (i != 0) {
	p->SetNextHitNumber(((StFtpcConfMapPoint *)mPoints->At(i-1))->GetHitNumber());
      }

      else {
	p->SetNextHitNumber(-1);
      }
    }    

    else {
      p->SetNextHitNumber(-1);
    }
    
    p->SetUsage(usage);
    p->SetTrackNumber(tracknumber);
  }

  return;
}


void StFtpcTrack::SetPointDependencies()
{
  // Sets number of next hit. Counting is started from the vertex. (The tracker finds hits on tracks vice versa!)

  mRowsWithPoints = 0;
  
  for (Int_t i = 0; i < mPoints->GetEntriesFast(); i++) {    
    StFtpcPoint *p = (StFtpcPoint *)mPoints->At(i);

    mRowsWithPoints += (Int_t)TMath::Power(2., (Int_t) (((p->GetPadRow()-1)%StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide())+1));
    
    if (i != 0) {
      p->SetNextHitNumber(((StFtpcPoint *)mPoints->At(i-1))->GetHitNumber());
    }
    
    else {
      p->SetNextHitNumber(-1);
    }
  }    



  return;
}


void StFtpcTrack::CalculateNMax()
{
  // Calculates the max. possible number of points on this track.
  // Up to now this is only a approximation. The calculated value would be right if:
  //   - the track would be a straight line
  //
  // In addition this funtion calculates the radius and the angle of a potential 
  // track point in the first (inner) and the last (outer) pad row.

  Short_t nmax = 0;
  
  StFtpcConfMapPoint *lastpoint  = (StFtpcConfMapPoint *)this->GetHits()->Last();
  StFtpcConfMapPoint *firstpoint = (StFtpcConfMapPoint *)this->GetHits()->First();
  
  Double_t z2 = firstpoint->GetZ();
  Double_t z1 = lastpoint->GetZ();
  Double_t x2 = firstpoint->GetX();
  Double_t x1 = lastpoint->GetX();
  Double_t r2 = TMath::Sqrt((firstpoint->GetX() * firstpoint->GetX()) + (firstpoint->GetY() * firstpoint->GetY())); 
  Double_t r1 = TMath::Sqrt((lastpoint->GetX() * lastpoint->GetX()) + (lastpoint->GetY() * lastpoint->GetY())); 

  Double_t r, x;
    
  for (Int_t i = 0; i < StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(); i++) {
    r = (r2 - r1) / (z2 - z1) * (TMath::Sign(Double_t(StFtpcTrackingParams::Instance()->PadRowPosZ(i)), z1) - z1) + r1;
    x = (x2 - x1) / (z2 - z1) * (TMath::Sign(Double_t(StFtpcTrackingParams::Instance()->PadRowPosZ(i)), z1) - z1) + x1;
    
    if (i == 0) {
      mRFirst = r;
      
      Double_t ratio = x/r;
      if (TMath::Abs(ratio) > 1.) {
	if (ratio > 0) ratio = 1.;
	else ratio = -1.;
      }

      mAlphaFirst = TMath::ACos(ratio);
   }
   
    if (i == 9) {
      mRLast = r;

      Double_t ratio = x/r;
      if (TMath::Abs(ratio) > 1.) {
	if (ratio > 0) ratio = 1.;
	else ratio = -1.;
      }

      mAlphaLast = TMath::ACos(ratio);
    }
    
    if (r < StFtpcTrackingParams::Instance()->OuterRadius() && r > StFtpcTrackingParams::Instance()->InnerRadius()) {
      nmax++;
    }
  }
  
  mNMax = nmax;

  return;
}


Double_t StFtpcTrack::CalcDca(StFtpcVertex *vertex, Bool_t primaryFit)
{
  // Calculates distance of closest approach to vertex.

  // call fit class
  MomentumFit(primaryFit ? vertex : 0);
  StThreeVector<Double_t> vertexPos(vertex->GetX(), vertex->GetY(), vertex->GetZ());
  return distance(vertexPos);
}


void StFtpcTrack::CalcResiduals(Bool_t primaryFit) {
  // Calculates the residuals to the momentum fit helix for each point.
  
  StThreeVector<Double_t> nv(0., 0., 1.);

  for (Int_t i = 0; i < mPoints->GetEntriesFast(); i++) {
    // loop over all points on track
    StFtpcPoint *hit = (StFtpcPoint*)mPoints->At(i);
    StThreeVector<Double_t> hitpos(hit->GetX(), hit->GetY(), hit->GetZ());

    Double_t pl = pathLength(hitpos, nv);
    Double_t x_hel = x(pl);
    Double_t y_hel = y(pl);
    Double_t x_hit = hit->GetX();
    Double_t y_hit = hit->GetY();

    if (primaryFit) {
      hit->SetXPrimResidual(x_hit - x_hel);
      hit->SetYPrimResidual(y_hit - y_hel);
      hit->SetRPrimResidual(TMath::Sqrt(x_hit*x_hit + y_hit*y_hit) - TMath::Sqrt(x_hel*x_hel + y_hel*y_hel));
      hit->SetPhiPrimResidual(TMath::ATan2(y_hit, x_hit) - TMath::ATan2(y_hel, x_hel));
    }

    else { // global fit
      hit->SetXGlobResidual(x_hit - x_hel);
      hit->SetYGlobResidual(y_hit - y_hel);
      hit->SetRGlobResidual(TMath::Sqrt(x_hit*x_hit + y_hit*y_hit) - TMath::Sqrt(x_hel*x_hel + y_hel*y_hel));
      hit->SetPhiGlobResidual(TMath::ATan2(y_hit, x_hit) - TMath::ATan2(y_hel, x_hel));
    }
  }

  return;
}


void StFtpcTrack::Fit()
{
  // call fit class
  MomentumFit();

  mP.SetX(momentum().x());
  mP.SetY(momentum().y());
  mP.SetZ(momentum().z());
  mChiSq[0] = chi2Rad();
  mChiSq[1] = chi2Lin();
  mTheta = momentum().theta();

  return;
}


void StFtpcTrack::Fit(StFtpcVertex *vertex, Double_t max_Dca, Bool_t primary_fit)
{
  // Fitting.
  
  // call fit class
  MomentumFit();
  CalcGlobResiduals();

  // tracks are treated as the particles fly
  StFtpcPoint *firstP = (StFtpcPoint *)mPoints->At(GetNumberOfPoints()-1);
  StFtpcPoint *lastP  = (StFtpcPoint *)mPoints->At(0);

  StThreeVector<Double_t> vertexPos(vertex->GetX(), vertex->GetY(), vertex->GetZ());
  StThreeVector<Double_t> lastPoint(lastP->GetX(), lastP->GetY(), lastP->GetZ());
  StThreeVector<Double_t> firstPoint;
  StThreeVector<Double_t> nv(0., 0., 1.);

  mDca = distance(vertexPos);

  if (!primary_fit) { // global fit
    firstPoint = StThreeVector<Double_t>(firstP->GetX(), firstP->GetY(), firstP->GetZ());
    
    if (mDca > max_Dca) {
      mFromMainVertex = (Bool_t)kFALSE;
    }
    
    else {
      mFromMainVertex = (Bool_t)kTRUE;
    }
  }
  
  else { // primary fit
    
    if (mDca > max_Dca) { // no refit
      firstPoint = StThreeVector<Double_t>(firstP->GetX(), firstP->GetY(), firstP->GetZ());
      mFromMainVertex = (Bool_t)kFALSE;
    }
    
    else {
      MomentumFit(vertex); // refit
      if (pathLength(lastPoint, nv)  >= NoSolution/2){
        LOG_WARN << "NoSolution found for MomentumFit with primary vertex - set mFromMainVertex = kFALSE for track " <<mTrackNumber<< endm;
        mFromMainVertex = (Bool_t)kFALSE;
      }
      else {
         CalcPrimResiduals();
      
         firstPoint = StThreeVector<Double_t>(vertexPos.x(), vertexPos.y(), vertexPos.z());
         mFromMainVertex = (Bool_t)kTRUE;
         mDca = distance(vertexPos); // change dca (even if this dca isn't very useful)
      }
    }
  }

  Double_t pl =  pathLength(firstPoint, nv);
  mTrackLength = pathLength(lastPoint, nv);
  
  mP.SetX(momentum().x());
  mP.SetY(momentum().y());
  mP.SetZ(momentum().z());
  mV.SetX(x(pl));
  mV.SetY(y(pl));
  mV.SetZ(z(pl));
  mL.SetX(x(mTrackLength));
  mL.SetY(y(mTrackLength));
  mL.SetZ(z(mTrackLength));

  mChiSq[0] = chi2Rad();
  mChiSq[1] = chi2Lin();
  mFitRadius = 1./curvature();
  mTheta = mP.Theta();

  if (vertex->GetId() == 0) { // no vertex found, tracking done with arbitray vertex at 0., 0., 0.
    mDca = 0.; // set dca to zero
  }

  return;
}


void StFtpcTrack::MomentumFit(StFtpcVertex *vertex)
{
  Double_t xWeight[11], yWeight[11];
  Double_t xval[11], yval[11], zval[11];
  Double_t xhelix[11], yhelix[11], zhelix[11];
  Int_t i, j;
  
  mIterSteps = 10;
  mYCenter = 0.;
  mXCenter = 0.;

  if (vertex) {
    //additional point needed
    mVertexPointOffset = 1;
    
    //vertex used as additional point on track
    xval[0] = vertex->GetX() * centimeter;
    yval[0] = vertex->GetY() * centimeter;
    zval[0] = vertex->GetZ() * centimeter;
    
    // use vertex error as weight (if it exists)
    xWeight[0] = (vertex->GetXerr() == 0.) ? 100. : 1./(vertex->GetXerr() * centimeter);
    yWeight[0] = (vertex->GetYerr() == 0.) ? 100. : 1./(vertex->GetYerr() * centimeter);
  }

  else {
    //no additional point needed
    mVertexPointOffset = 0;
  }
  
  Int_t backw_counter = 0;
  // initialize position arrays
  // these values will later be manipulated, mPoint array will not be touched   
  for(i = 0; i < GetNumberOfPoints(); i++) {
    backw_counter = GetNumberOfPoints() - 1 - i + mVertexPointOffset;
    StFtpcPoint *point = (StFtpcPoint*)mPoints->At(i);

    xval[backw_counter] = point->GetX() * centimeter;
    yval[backw_counter] = point->GetY() * centimeter;
    zval[backw_counter] = point->GetZ() * centimeter;
    
    // hit resolution taken from hit error (or set to 100)
    xWeight[backw_counter] = (point->GetXerr() == 0.) ? 100. : 1./(point->GetXerr() * centimeter);
    yWeight[backw_counter] = (point->GetYerr() == 0.) ? 100. : 1./(point->GetYerr() * centimeter);
  }

  /////////////////////////////////////////////////////////////////////
  // calculate first guess momentum from helix fit
  /////////////////////////////////////////////////////////////////////
  
  CircleFit(xval, yval, xWeight, yWeight, GetNumberOfPoints() + mVertexPointOffset);
  LineFit(xval, yval, zval, xWeight, yWeight, GetNumberOfPoints() + mVertexPointOffset);

  // determine helix parameters
  Double_t dipAngle = fabs(atan(1/(mFitRadius*mArcSlope)));
  
  if (zval[1] < 0) {
    dipAngle *= -1;
  }
  
  Double_t startPhase = atan((yval[0]-mYCenter)/(xval[0]-mXCenter));
  
  if (xval[0]-mXCenter < 0) {
    startPhase += pi;
  }
  
  else if (yval[0]-mYCenter < 0) {
    startPhase += twopi;
  }
  
  Int_t orientation = 1;

  if (mArcSlope * zval[1] < 0) {
    orientation = -1;
  }

  // create helix
  Double_t startAngle = mArcOffset + mArcSlope * zval[0];
  Double_t startX = mXCenter + mFitRadius * cos(startAngle);
  Double_t startY = mYCenter + mFitRadius * sin(startAngle);

  StThreeVector<Double_t> startHit(startX, startY, zval[0]);
  setParameters(1/mFitRadius, dipAngle, startPhase, startHit, orientation);

  // get z-component of B-field at 0,0,0 for first momentum guess
  Float_t pos[3] = {0., 0., 0.};
  Float_t centralField[3];
  StFtpcTrackingParams::Instance()->MagField()->B3DField(pos, centralField);
  centralField[0] *= kilogauss;
  centralField[1] *= kilogauss;
  centralField[2] *= kilogauss;
  mZField = (Double_t) centralField[2];
  
  // get momentum at track origin and charge
  StThreeVector<Double_t> rv(0., 0., zval[0]);
  StThreeVector<Double_t> nv(0., 0., 1.);
  Double_t pl = pathLength(rv, nv);
  mHelixMomentum = momentumAt(pl, mZField);
  SetCharge(charge(mZField));

  // store helix fitted hit positions
  for(i = 0; i < GetNumberOfPoints() + mVertexPointOffset; i++) {
    StThreeVector<Double_t> rvec(0., 0., zval[i]);
    StThreeVector<Double_t> nvec(0., 0., 1.);
    Double_t plength = pathLength(rvec, nvec);
    if (plength >= NoSolution/2) {
       LOG_WARN << "Helix Fit found NoSolution for hit  " << i <<" - not possible to track helix momentum through measured field for this track"<< endm;
       return;
    }
    xhelix[i] = x(plength);
    yhelix[i] = y(plength);
    zhelix[i] = z(plength);
  }

  ///////////////////////////////////////////////////////////////////////
  // track helix momentum through measured field:
  ///////////////////////////////////////////////////////////////////////

  // initialize position and momentum
  StThreeVector<Double_t> currentPosition(xhelix[0 + mVertexPointOffset], 
					  yhelix[0 + mVertexPointOffset],
					  zhelix[0 + mVertexPointOffset]);
  pl = pathLength(currentPosition, nv);
  StThreeVector<Double_t> currentMomentum(momentumAt(pl, mZField));


  if (StFtpcTrackingParams::Instance()->MagFieldFactor()) {

    // iterate over points
    Double_t stepSize;
    
    for(i = 1 + mVertexPointOffset; i < GetNumberOfPoints() + mVertexPointOffset; i++) {
      stepSize = (zval[i] - zval[i-1]) / mIterSteps;
      
      // iterate between points
      for(j = 0; j < mIterSteps; j++) {
	// store momentum for position propagation
	Double_t propagateXMomentum = currentMomentum.x();
	Double_t propagateYMomentum = currentMomentum.y();
	Double_t propagateZMomentum = currentMomentum.z();
	
	// get local magnetic field
	Float_t positionArray[3] = {(Float_t) (currentPosition.x()), 
				    (Float_t) (currentPosition.y()), 
				    (Float_t) (currentPosition.z() + stepSize/2)};
	Float_t localField[3];
	StFtpcTrackingParams::Instance()->MagField()->B3DField(positionArray, localField);
	
	StThreeVector<Double_t> fieldVector
	  ((Double_t) localField[0] * kilogauss/tesla*c_light*nanosecond/meter, 
	   (Double_t) localField[1] * kilogauss/tesla*c_light*nanosecond/meter, 
	   (Double_t) localField[2] * kilogauss/tesla*c_light*nanosecond/meter); 
	
	// calculate new momentum as helix segment
	Double_t absMomentum = abs(currentMomentum);
	StThreeVector<Double_t> perpField = 
	  currentMomentum.cross(fieldVector) * (Double_t)mQ/absMomentum;
	Double_t twistRadius = (absMomentum/abs(perpField)) * meter/GeV;
	
	Double_t stepLength = stepSize/cos(currentMomentum.theta());
	
	Double_t newMomentumCross = absMomentum*stepLength/twistRadius;
	Double_t newMomentumParallel = 
	  ::sqrt(absMomentum * absMomentum - newMomentumCross * newMomentumCross);
	currentMomentum.setMagnitude(newMomentumParallel);
	StThreeVector<Double_t> momentumChange(perpField);
	momentumChange.setMagnitude(newMomentumCross);
	currentMomentum = currentMomentum+momentumChange;
	
	// propagate position
	propagateXMomentum = (propagateXMomentum + currentMomentum.x())/2;
	propagateYMomentum = (propagateYMomentum + currentMomentum.y())/2;
	propagateZMomentum = (propagateZMomentum + currentMomentum.z())/2;
	currentPosition.setX(currentPosition.x() + stepSize *
			     (propagateXMomentum / propagateZMomentum));
	currentPosition.setY(currentPosition.y() + stepSize *
			     (propagateYMomentum / propagateZMomentum));
	currentPosition.setZ(currentPosition.z() + stepSize);
      }
      
      // change position array to compensate for distortion
      StThreeVector<Double_t> rvec(0., 0., zval[i]);
      StThreeVector<Double_t> nvec(0., 0., 1.);
      Double_t plength=pathLength(rvec, nvec);
      
      if (zval[1] > 0) {
	xval[i] += (x(plength) - currentPosition.x());
	yval[i] += (y(plength) - currentPosition.y());
      }
      
      else {
	xval[i] -= (x(plength) - currentPosition.x());
	yval[i] -= (y(plength) - currentPosition.y());
      }
      
      // calculate fit quality indicators only if needed
      //       Double_t distHitHelix=::sqrt((xval[i]-x(plength))*(xval[i]-x(plength))+(yval[i]-y(plength))*(yval[i]-y(plength)));
      //       Double_t distHelixFit=::sqrt((x(plength)-currentPosition.x())*(x(plength)-currentPosition.x())+(y(plength)-currentPosition.y())*(y(plength)-currentPosition.y()));
      
    }
    
    //////////////////////////////////////////////////////////////////////
    // refit helix
    //////////////////////////////////////////////////////////////////////
    
    CircleFit(xval, yval, xWeight, yWeight, GetNumberOfPoints()+mVertexPointOffset);
    LineFit(xval, yval, zval, xWeight, yWeight, GetNumberOfPoints()+mVertexPointOffset);
    
    // determine helix parameters
    dipAngle = fabs(atan(1/(mFitRadius*mArcSlope)));
    
    if (zval[1] < 0) {
      dipAngle*=-1;
    }
    
    startPhase = atan((yval[0]-mYCenter)/(xval[0]-mXCenter));
    
    if (xval[0] - mXCenter < 0) {
      startPhase+=pi;
    }
    
    else if (yval[0]-mYCenter<0) {
      startPhase+=twopi;
    }
    
    orientation = 1;
    
    if (mArcSlope * zval[1] < 0) {
      orientation = -1;
    }
    
    // set helix parameters to new values
    startAngle = mArcOffset + mArcSlope * zval[0];
    startX = mXCenter + mFitRadius * cos(startAngle);
    startY = mYCenter + mFitRadius * sin(startAngle);
    startHit.setX(startX);
    startHit.setY(startY);
    
    setParameters(1/mFitRadius, dipAngle, startPhase, startHit, orientation);
  }
  
  // set final momentum value
  pl = pathLength(rv, nv);
  mFullMomentum = momentumAt(pl, mZField);
}


//////////////////////////////////////////////////////////////////
// Circle fitting program
//
// This function will fit a circle to the points in the matrix x and y.
// 'num' is the number of points in x and y
// 'xc' is the found center in x
// 'yc' is the found center in y
// 'R' is the radius of the fitted circle
// 'chi2' error in fit
//
// Written by Mike Heffner Sept 21 1998
// error calculation added Oct 3 1998, Mike Heffner
// fit with point errors added May 1999 Holm Huemmler
//
// Fitting algorithm by: N.Chernov,G.Ososkov, Computer Physics 
//          Communications 33(1984) 329-333
//////////////////////////////////////////////////////////////////  


Int_t StFtpcTrack::CircleFit(Double_t x[],Double_t y[], Double_t xw[], Double_t yw[], Int_t num)
{
#ifndef __IOSTREAM__
#include <Stiostream.h>
#endif
  
  Int_t i;
  Int_t debug = 0; //set to 1 for debug messages
  
  if (num ==0 ) {
    // added to remove error from zero input
    return 0;
  }

  //-------------------------------------------------
  ////////////////////////////////////////////////
  // calculate the center of gravity of the points.
  // then transform to that origin
  Double_t xav = 0;
  Double_t yav = 0;
  Double_t xwav = 0;
  Double_t ywav = 0;
  Double_t wei[11];

  for(i=0;i<num;i++) {
    wei[i] = xw[i] + yw[i];
    xav += x[i] * wei[i];
    yav += y[i] * wei[i];
    xwav += wei[i];
    ywav += wei[i];
  }
  
  xav = xav/xwav;
  yav = yav/ywav;

  //gMessMgr->precision(16);
    
  if (debug) {
    LOG_INFO << "from circle fitting program" << endm;
  }
  
  for(i=0;i<num;i++) {
      
    if (debug) { 
      LOG_INFO << "x: " << x[i] << " y: " << y[i] << "xw: " << xw[i] << " yw: " << yw[i] << endm;
    }

    x[i] = x[i] - xav;
    y[i] = y[i] - yav;
  }
  
  /////////////////////////////////////////////////
  // calculate some moments of the points
  
  Double_t F = 0;
  Double_t G = 0;
  Double_t H = 0;
  Double_t P = 0;
  Double_t Q = 0;
  Double_t T = 0;
  Double_t gamma0 = 0;
  Double_t wF = 0;
  Double_t wG = 0;
  Double_t wH = 0;
  Double_t wP = 0;
  Double_t wQ = 0;
  Double_t wT = 0;
  Double_t wgamma0 = 0;
  
  // change error parameters to 1d, 2d errors not usable in this fit
  for(i = 0; i < num; i++) {
    F += wei[i] * (3 * x[i] * x[i] + y[i] * y[i]);
    wF += wei[i];
    G += wei[i] * (x[i] * x[i] + 3 * y[i] * y[i]);
    wG += wei[i];
    H += wei[i] * 2 * x[i] *y[i];
    wH += wei[i];
    P += wei[i] * x[i] * (x[i] * x[i] + y[i] * y[i]);
    wP += wei[i];
    Q += wei[i] * y[i] * (x[i] * x[i] + y[i] * y[i]);
    wQ += wei[i];
    T += wei[i] * (x[i] * x[i] + y[i] * y[i]) * (x[i] * x[i] + y[i] * y[i]);
    wT += wei[i];
    gamma0 += wei[i] * (x[i] * x[i] + y[i] * y[i]);
    wgamma0 += wei[i];
  }

  gamma0 = gamma0/wgamma0;
  F = F/wF;
  G = G/wG;
  H = H/wH;
  P = P/wP;
  Q = Q/wQ;
  T = T/wT;
  
  Double_t  A = -F -G;
  Double_t  B =  F * G - T - H * H;
  Double_t  C =  T * (F + G) - 2 * (P *P + Q * Q);
  Double_t  D =  T * (H * H - F * G) + 2 * (P * P * G + Q * Q * F) - 4 * P * Q *H;
  
  Double_t A0 = A / gamma0;
  Double_t B0 = B / (gamma0*gamma0);
  Double_t C0 = C / (gamma0*gamma0*gamma0);
  Double_t D0 = D / (gamma0*gamma0*gamma0*gamma0);

  ///////////////////////////////////////////////////////
  // now solve the equation by Newton method

  Int_t MaxIter = 100;
  Double_t w = 1.;
  Double_t wNew = 0.;
  Double_t f = 0., fp = 0.;
  Double_t xc = 0., yc = 0.;
  
  if (debug) { 
    LOG_INFO << "Solving by Newton method" << endm;
  }
  
  for(i = 0; i < MaxIter; i++) {
    f = w*w*w*w + A0 * w*w*w + B0 * w*w + C0 * w + D0;
    fp = 4 * w*w*w + 3 * A0 * w*w + 2 * B0 * w + C0;
    wNew = w - f / fp;
    
    if (debug) { 
      LOG_INFO << "Iteration Number" << i << endm;
    }
    
    if ((wNew-w) < 10e-16 && (w-wNew) < 10e-16) {
      break;
    }
    
    w = wNew;
  }
  
  ////////////////////////////////////////////
  // compute the output variables
  Double_t gamma = gamma0 * wNew;
  Double_t b = (Q - H * P / (F - gamma)) / (G - gamma - H * H / (F - gamma));
  Double_t a = (P - H * b) / (F - gamma);
  
  Double_t R = 0;
  if ((wNew-w) < 10e-16 && (w-wNew) < 10e-16) {
    R = ::sqrt(a * a + b * b + gamma);
    xc = a + xav;
    yc = b + yav;
  }
  
  // compute chi2
  Double_t chi2 = 0.;
  Double_t variance = 0.;
  //   Double_t wchi2=0;
  //   for(i=0;i<num;i++)
  //     {
  //       x[i] = x[i] + xav;
  //       y[i] = y[i] + yav;
  
  //       Double_t err = R - ::sqrt(xw[i]*(x[i]-xc)*xw[i]*(x[i]-xc) 
  // 			    + yw[i]*(y[i]-yc)*yw[i]*(y[i]-yc));
  //       chi2 += err*err;
  //       wchi2 += xw[i]*xw[i]+yw[i]*yw[i];
  //     }
  //   chi2 *= num / wchi2;
  
  
  for (i = 0; i < num; i++) {
    x[i] = x[i] + xav;
    y[i] = y[i] + yav;
    
    Double_t err = R - ::sqrt((x[i] - xc) * (x[i] - xc) + (y[i] - yc) * (y[i] - yc));
    chi2 += err * err;
    
    if (i>0) {
      // approximation for sigma r, more precise using atan...
      variance += 1 / (xw[i] * xw[i]) + 1 / (yw[i] * yw[i]);
    }
  }
  
  variance /= (num-1);
  chi2 /= variance;
  
  mXCenter = xc;
  mYCenter = yc;
  mFitRadius = R;
  mChi2Rad = chi2;

  return 1;
}

void StFtpcTrack::LineFit(Double_t *xval, Double_t *yval, Double_t *zval, Double_t *xw, Double_t *yw, Int_t num)
{
  Double_t x_ss = 0., x_sang = 0., x_sz = 0., x_szang = 0., x_szz = 0.;
  Double_t weight, t;
  Int_t i;
  Double_t angle = 0., lastangle = 0.;
  
  for(i = 0; i < num; i++) {
    // calculate angle and eliminate steps in atan function
    angle = atan((yval[i]-mYCenter)/(xval[i]-mXCenter));
    
    if (xval[i] - mXCenter < 0) {
      angle += pi;
    }
    
    else if (yval[i] - mYCenter < 0) {
      angle += twopi;
    }
    
    // shift into same phase
    if (i != 0) {
      if (angle>lastangle+pi) angle -= twopi;
      if (angle<lastangle-pi) angle += twopi;
    }
    
    lastangle = angle;
    
    // do sums
    weight = ::sqrt(xw[i] * xw[i] * cos(angle) * cos(angle)
		  + yw[i] * yw[i] * sin(angle) * sin(angle));
    x_ss += weight;
    x_sang += weight * angle;
    x_sz += weight * zval[i];
    x_szang += weight * zval[i] * angle;
    x_szz += weight * zval[i] * zval[i];
  }
  
  t = x_ss * x_szz - x_sz * x_sz;
  
  if (t != 0) {
    mArcOffset = ((x_szz * x_sang) - (x_sz * x_szang)) / t;
    mArcSlope = ((x_ss * x_szang) - (x_sz * x_sang)) / t;
  }
  
  else {
    mArcOffset = 0;
    mArcSlope = 0;
  }
  
  Double_t chi2 = 0., variance = 0.;
  
  for(i = 0; i < num; i++) {      
    Double_t angle = atan((yval[i] - mYCenter) / (xval[i] - mXCenter));
    
    if (xval[i]-mXCenter<0) {
      angle+=pi;
    }
    
    else if (yval[i]  -mYCenter < 0) {
      angle += twopi;
    }

    Double_t lastangle = mArcOffset + mArcSlope*zval[i];
    
    // shift into same phase
    if (i != 0){
      if (angle>lastangle+pi) angle-=twopi;
      if (angle<lastangle-pi) angle+=twopi;
    }
    
    Double_t err = (angle - (mArcOffset + mArcSlope*zval[i]));
    chi2 += err*err;
    
    if (i>0) {
      // approximation for sigma r, more precise using atan...
      Double_t temp= ((1 / xw[i] * 1 / xw[i]) + (1 / yw[i] * 1 / yw[i]))
	* ((mArcSlope * (zval[i] - zval[0])) * (mArcSlope * (zval[i] - zval[0])))
	/ ((xval[i] - xval[0]) * (xval[i] - xval[0]) + (yval[i] - yval[0]) * (yval[i] - yval[0]));
      variance += temp;
    }
  }
  
  variance /= (num-1);
  chi2 /= variance;
  mChi2Lin = chi2;
}

void StFtpcTrack::CalcGlobResiduals()
{
  // calulates the global fit residuals for each point on track

  CalcResiduals((Bool_t)kFALSE);
}


void StFtpcTrack::CalcPrimResiduals()
{
  // calulates the primary fit residuals for each point on track

  CalcResiduals((Bool_t)kTRUE);
}


Double_t StFtpcTrack::GetMeanAlpha()
{
  // Returns mean phi angle of track.

  Double_t phi = mAlphaFirst+mAlphaLast;
  
  if (phi >= 2*TMath::Pi()) phi -= 2*TMath::Pi();
  else if (phi <= -2*TMath::Pi())  phi += 2*TMath::Pi();

  return phi/2.;
}


Double_t StFtpcTrack::GetPt() const
{
  // Returns transverse momentum.

  return TMath::Sqrt(mP.X() * mP.X() + mP.Y() * mP.Y());
}


Double_t StFtpcTrack::GetP() const
{
  // Returns total momentum.

  return TMath::Sqrt(mP.X() * mP.X() + mP.Y() * mP.Y() + mP.Z() * mP.Z());
}


Double_t StFtpcTrack::GetPseudoRapidity() const
{
  // Returns the pseudo rapidity of the particle.

  return 0.5 * TMath::Log((GetP() + GetPz()) / (GetP() - GetPz()));  
}


Double_t StFtpcTrack::GetEta() const
{
  // This function returns the value of GetPseudoRapidity().
  
  return GetPseudoRapidity();
}


Double_t StFtpcTrack::GetRapidity() const
{
  // Returns the rapidity of the particle with the assumption that the particle is a pion (+/-).

  return 0.5 * TMath::Log((M_PION_PLUS + GetPz()) / (M_PION_PLUS - GetPz()));
}


// momentum fit functions

StThreeVector<Double_t> StFtpcTrack::helixMomentum() const
{
  return mHelixMomentum;
}


StThreeVector<Double_t> StFtpcTrack::momentum() const
{
  return mFullMomentum;
}


Double_t StFtpcTrack::chi2Rad() const
{
  return mChi2Rad;
}


Double_t StFtpcTrack::chi2Lin() const
{
  return mChi2Lin;
}


StThreeVector<Double_t> StFtpcTrack::localMomentum(Double_t s)
{
  return momentumAt(s, mZField);
}



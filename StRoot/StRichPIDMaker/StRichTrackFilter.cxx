/**********************************************************
 * $Id: StRichTrackFilter.cxx,v 1.3 2000/05/19 19:06:11 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrackFilter.cxx,v $
 *  Revision 1.3  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.2  2000/04/04 14:14:48  horsley
 *  modified StRichTrackFilter to use StRichGeometryDb.
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 **********************************************************/

#include "StRichTrackFilter.h"
#include "StRichTrack.h"
#include "StRrsMaker/StRichGeometryDb.h"
#include "StEventTypes.h"
#include "SystemOfUnits.h"


#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichTrackFilter::StRichTrackFilter()  {
  mMomCut      = 0.25*GeV;
  mImpactPar   = .33*centimeter;
  mChi2Cut     = 2.0;
  mMinNTPCHits = 20.0;
  myGeometryDb = StRichGeometryDb::getDb();
}

StRichTrackFilter::~StRichTrackFilter()  {}

void StRichTrackFilter::setTrack(StRichTrack* track)  {
  mStRichTrack      = track;
  mAngleOfIncidence = mStRichTrack->getTheta();
  mMomentum         = mStRichTrack->getMomentum();
  mMIP              = mStRichTrack->getMIP();
  
  mFlag = mStRichTrack->getStTrack()->flag();
  mImpactParameter  = (double)mStRichTrack->getStTrack()->impactParameter();
  mNumberOfTPCHits  = mStRichTrack->getStTrack()->fitTraits().numberOfFitPoints();
  mChiSqr           = (double)mStRichTrack->getStTrack()->fitTraits().chi2();

}


bool StRichTrackFilter::trackAcceptable() {
  
  if (abs(mMIP.x()) > myGeometryDb->radiatorDimension().x() ||
       abs(mMIP.y()) > myGeometryDb->radiatorDimension().y()) {
    return false;
  }
  
 
  if ( (mMomentum.mag()   > mMomCut)       &&
        (mImpactParameter  < mImpactPar)   &&
        (mAngleOfIncidence < 35.0*degree)  &&
        (mNumberOfTPCHits  > mMinNTPCHits) &&
        (mFlag > 0) ) {
    return true;
  }

  return false;
}

void StRichTrackFilter::setChi2(double chiCut) {
  mChi2Cut = chiCut;
}

void StRichTrackFilter::setNTPCPoints(int n) {
  mNTPCPoints = n;
}

void StRichTrackFilter::setImpactParameter(double ip) {
  mImpactPar = ip;
}


void StRichTrackFilter::setMomentum(double p) {
  mMomCut = p;
}







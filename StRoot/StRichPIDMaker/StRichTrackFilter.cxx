/**********************************************************
 * $Id: StRichTrackFilter.cxx,v 2.1 2000/09/29 01:35:38 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrackFilter.cxx,v $
 *  Revision 2.1  2000/09/29 01:35:38  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.4  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.3  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.2  2000/04/04 14:14:48  horsley
 *  modified StRichTrackFilter to use StRichGeometryDb.
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 **********************************************************/
#include "StRichTrackingControl.h"
#include "StRichTrackFilter.h"
#include "StRichTrack.h"
#include "StRrsMaker/StRichGeometryDb.h"
#include "StEvent/StEventTypes.h"
#include "SystemOfUnits.h"


#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichTrackFilter::StRichTrackFilter()  {
 
  mDCA              = 3*centimeter;
  mMinNTPCFitPoints = 9.0;
  mEtaCut           = 0.1;
  myGeometryDb = StRichGeometryDb::getDb();
}

StRichTrackFilter::~StRichTrackFilter()  {}

void StRichTrackFilter::setTrack(StRichTrack* track)  {
  
  mStRichTrack      = track;
  mAngleOfIncidence = mStRichTrack->getTheta();
  mMomentum         = mStRichTrack->getMomentum();
  mMIP              = mStRichTrack->getProjectedMIP();
  if( mStRichTrack->getStTrack()){
      mFlag = mStRichTrack->getStTrack()->flag();
      mNumberOfTPCFitPoints  = mStRichTrack->getStTrack()->fitTraits().numberOfFitPoints();
  }
}

void StRichTrackFilter::setVertex(StThreeVectorF& Vertex){
    mVertex = Vertex;}


bool StRichTrackFilter::trackAcceptable() {

    if (abs(mMIP.x()) > myGeometryDb->radiatorDimension().x() ||
	abs(mMIP.y()) > myGeometryDb->radiatorDimension().y())
	{
	    return false;
	}
    

    if (mStRichTrack && mStRichTrack->getStTrack() &&
	mStRichTrack->getStTrack()->geometry() &&
    	(mAngleOfIncidence < 85.0*degree)  &&
	(mNumberOfTPCFitPoints  > mMinNTPCFitPoints) &&
	(mFlag >= 0) &&
	(mStRichTrack->getStTrack()->geometry()->helix().distance(mVertex) < mDCA) &&
	(fabs(mStRichTrack->getStTrack()->geometry()->momentum().pseudoRapidity()) < mEtaCut) )
	{
	    return true;
	}
    
    return false;
    
}



bool StRichTrackFilter::fromPrimaryVertex(int & nTracksPrim){
#ifdef RICH_WITH_L3_TRACKS

    if(mStRichTrack && mStRichTrack->getL3Track()){
      //if(mStRichTrack->getZVertex() < mZVertex + mPrimaryTrackSigma &&
      //   mStRichTrack->getZVertex() > mZVertex - mPrimaryTrackSigma){
      //    nTracksPrim++;
          return true;
	}
    else return false;
    }
#endif
    return false;

}



void StRichTrackFilter::setNTPCPoints(int n) {
//mNTPCPoints = n;
}

void StRichTrackFilter::setImpactParameter(double ip) {
//mImpactPar = ip;
}


void StRichTrackFilter::setMomentum(double p) {
//mMomCut = p;
}







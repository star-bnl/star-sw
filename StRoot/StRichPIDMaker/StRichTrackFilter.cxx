/**********************************************************
 * $Id: StRichTrackFilter.cxx,v 2.0 2000/08/09 16:26:22 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrackFilter.cxx,v $
 *  Revision 2.0  2000/08/09 16:26:22  gans
 *  Naming Convention for TDrawable Ojects. All drawable objects now in StRichDisplayMaker
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
    // mMomCut      = 0.5*GeV;
  mMomCut      = -0.1*GeV;
  mImpactPar   = 1*centimeter;
  mChi2Cut     = 20.0;
  mMinNTPCHits = 5.0;
  mPrimaryTrackSigma = 5.0;
  myGeometryDb = StRichGeometryDb::getDb();

  mZVertex = 0; // Initialize
}

StRichTrackFilter::~StRichTrackFilter()  {}

void StRichTrackFilter::setTrack(StRichTrack* track)  {
  mStRichTrack      = track;
  
  mAngleOfIncidence = mStRichTrack->getTheta();
  
  mMomentum         = mStRichTrack->getMomentum();
    
  mMIP              = mStRichTrack->getProjectedMIP();
  
  if( mStRichTrack->getStTrack()){
      mFlag = mStRichTrack->getStTrack()->flag();
      mImpactParameter  = (double)mStRichTrack->getStTrack()->impactParameter();
      mNumberOfTPCHits  = mStRichTrack->getStTrack()->fitTraits().numberOfFitPoints();
      mChiSqr           = (double)mStRichTrack->getStTrack()->fitTraits().chi2();
  }


#ifdef RICH_WITH_L3_TRACKS
  if(mStRichTrack->getL3Track())
  {
      mFlag = mStRichTrack->getL3Track()->flag;
      mImpactParameter = mStRichTrack->getL3Track()->r0 ; // is it?

      mNumberOfTPCHits = mStRichTrack->getL3Track()->nHits;
      mChiSqr =  mStRichTrack->getL3Track()->chi2[0];
      
  }
#endif
}

void StRichTrackFilter::setZVertex(double zVertex){
    mZVertex = zVertex;}


bool StRichTrackFilter::trackAcceptable() {

    
    if (abs(mMIP.x()) > myGeometryDb->radiatorDimension().x() ||
	abs(mMIP.y()) > myGeometryDb->radiatorDimension().y())
	{
	    return false;
	}


    /*   if( ! mStRichTrack->isGood())
	{
	    cout << "mStRichTrack is False " << endl;
	    return false;
	}
    */
#ifdef RICH_WITH_L3_TRACKS

    if ( mStRichTrack && mStRichTrack->getL3Track()){
	if( mMomentum.mag() > mMomCut
	    && mStRichTrack->getPathLength() > 0
	    && mNumberOfTPCHits  > mMinNTPCHits )
	    {
		return true;
	    }
    }
    
    
#endif
    
    if (mStRichTrack && mStRichTrack->getStTrack() &&
	(mMomentum.mag()   > mMomCut)       &&
	(mStRichTrack->getPathLength() > 0) &&
	//(mImpactParameter  < mImpactPar)   &&
	(mAngleOfIncidence < 85.0*degree)  &&
	(mNumberOfTPCHits  > mMinNTPCHits) &&
	(mFlag > 0) 
	)
	{
	    return true;
	}
    
    return false;
    
}

bool StRichTrackFilter::fromPrimaryVertex(int & nTracksPrim){
#ifdef RICH_WITH_L3_TRACKS

    if(mStRichTrack && mStRichTrack->getL3Track()){
	if(mStRichTrack->getZVertex() < mZVertex + mPrimaryTrackSigma &&
	   mStRichTrack->getZVertex() > mZVertex - mPrimaryTrackSigma){
	    nTracksPrim++;
	    return true;
	}
    }
#endif
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







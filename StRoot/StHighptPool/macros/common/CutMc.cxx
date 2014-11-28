#include "CutMc.h"
#include "StMiniMcEvent/StMiniMcEvent.h"

bool
CutMc::AcceptTrackHalf(StTinyRcTrack* track, float vtxZ)
{

  if(VertexSkipOn() && 
     !IsOutSide(track->mFirstZ,track->mLastZ,mVertexZSkip)) return false;
  if(mHitAvoid &&
     !IsOutSide(track->mFirstZ,track->mLastZ,mHitAvoid)) return false;
  if(Half() && !IsSameSide(vtxZ,track->mFirstZ,track->mLastZ)) return false;
  if(HitHalf() && !IsHitSameSide(track->mFirstZ,track->mLastZ)) 
    return false;
  if(GeomHalf() && !IsGeomSameSide(vtxZ,atan2(track->mPzGl,track->mPtGl)))
     return false;
  
  return true;
}

bool 
CutMc::AcceptCent(StMiniMcEvent* event)
{
  bool acceptCent = false;
  if(mDoFlowCent) acceptCent=AcceptFlowCent(event);
  else if(mDoZdcCtbCent) acceptCent=AcceptZdcCtbCent(event);
  else if(mDoHMinusCent) acceptCent=AcceptHMinusCent(event);
  else if(mDoNchCent || mDoNchCentKludge) acceptCent=AcceptNchCent(event);

  return acceptCent;
}

bool 
CutMc::Accept(StMiniMcEvent* event)
{

  return(
	 AcceptVertexZ(event) && AcceptCent(event)
	 );    
}

bool
CutMc::AcceptFlowCent(StMiniMcEvent* event)
{
  return (
	  event->mCentrality >= mFlowCent[0] &&
	  event->mCentrality <= mFlowCent[1]
	 );
}

bool
CutMc::AcceptZdcCtbCent(StMiniMcEvent* event)
{
  double zdcSum = event->mZDCe + event->mZDCw;
  double ctb    = event->mCTB;
  NchCentrality zdcCent = centrality(zdcSum,ctb);

  return(
	 zdcCent >= mZdcCtbCent[1] &&  // less central has higher value
	 zdcCent <= mZdcCtbCent[0] 
	 );
}

bool 
CutMc::AcceptHMinusCent(StMiniMcEvent* event)
{
  int nHMinus = event->mNUncorrectedNegativePrimaries;
  NchCentrality cent = centralityHMinus(nHMinus);

  return(
	 cent >= mHMinusCent[1] && // less central has higher value
	 cent <= mHMinusCent[0]
	 ); 
}

bool 
CutMc::AcceptNchCent(StMiniMcEvent* event)
{
  int nCh = event->mNUncorrectedPrimaries;
  NchCentrality cent = centralityNch(nCh);
  if(mDoNchCentKludge) cent = centralityNchKludge(nCh);

  return(
	 cent >= mNchCent[1] && // less central has higher value
	 cent <= mNchCent[0]
	 ); 
}


bool
CutMc::AcceptVertexZ(StMiniMcEvent* event)
{

  return(
	 event->mVertexZ >= mVertexZ[0] &&
	 event->mVertexZ <= mVertexZ[1] &&
	 ( (!mVertexZSkip) ? true : fabs(event->mVertexZ)>mVertexZSkip )
	 );  
}



//
// uses the mc eta cut
//

bool
CutMc::Accept(StMiniMcPair* track)
{
  return (
	  track &&
	  AcceptFitPts(track) &&
	  AcceptEtaMc(track)    &&
	  AcceptSDcaGl(track)
	  );
}

bool
CutMc::AcceptNoEta(StTinyRcTrack* track)
{
  return (
	  track &&
	  AcceptFitPts(track) &&
	  AcceptSDcaGl(track)
	  );
}

bool
CutMc::AcceptFitPts(StTinyRcTrack* track)
{
  return(
	 track->mFitPts >= mFitPts[0] &&
	 track->mFitPts <= mFitPts[1] 
	 );
}

bool
CutMc::AcceptMcPts(StTinyMcTrack* track)
{
  return(
	 track->mNHitMc >= mMcPts[0] &&
	 track->mNHitMc <= mMcPts[1] 
	 );
}

bool
CutMc::AcceptEtaPr(StTinyRcTrack* track)
{
  return(
	 track->mEtaPr    >= mEta[0]    &&
	 track->mEtaPr    <= mEta[1]    
	 );
}


bool
CutMc::AcceptEtaMc(StTinyMcTrack* track)
{
  return(
	 track->mEtaMc    >= mEta[0]    &&
	 track->mEtaMc    <= mEta[1]    
	 );
}

bool
CutMc::AcceptEtaMcTight(StTinyMcTrack* track)
{
  return(
	 track->mEtaMc    >= mEtaTight[0]    &&
	 track->mEtaMc    <= mEtaTight[1]    
	 );
}


bool
CutMc::AcceptSDcaGl(StTinyRcTrack* track)
{
  return(
	 track->mDcaGl >= mSDcaGl[0]    &&
	 track->mDcaGl <= mSDcaGl[1]    
	 );
}

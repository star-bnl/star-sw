#include "CutRc.h"

bool
CutRc::AcceptTrackHalf(StHiMicroTrack* track, float vtxZ)
{
  if(VertexSkipOn() && 
     !IsOutSide(track->mFirstZ,track->mLastZ,mVertexZSkip)) return false;
  if(mHitAvoid &&
     !IsOutSide(track->mFirstZ,track->mLastZ,mHitAvoid)) return false;
  if(Half() && !IsSameSide(vtxZ,track->mFirstZ,track->mLastZ)) return false;
  if(HitHalf() && !IsHitSameSide(track->mFirstZ,track->mLastZ)) 
    return false;
  if(GeomHalf() && !IsGeomSameSide(vtxZ,track->mDipAngleGl))
     return false;
  
  return true;
}

bool 
CutRc::AcceptCent(StHiMicroEvent* event)
{
  bool acceptCent = false;
  if(mDoFlowCent) acceptCent=AcceptFlowCent(event);
  else if(mDoZdcCtbCent) acceptCent=AcceptZdcCtbCent(event);
  else if(mDoHMinusCent) acceptCent=AcceptHMinusCent(event);
  else if(mDoNchCent || mDoNchCentKludge) acceptCent=AcceptNchCent(event);
  else acceptCent = false;

  return acceptCent;
}

bool 
CutRc::Accept(StHiMicroEvent* event)
{
  

  return(
	 AcceptVertexZ(event) && AcceptCent(event)
	 );    
}

bool
CutRc::AcceptFlowCent(StHiMicroEvent* event)
{
  return (
	  event->mCentrality >= mFlowCent[0] &&
	  event->mCentrality <= mFlowCent[1]
	 );
}

bool
CutRc::AcceptZdcCtbCent(StHiMicroEvent* event)
{
  float zdcSum = event->mZDCe + event->mZDCw;
  float ctb    = event->mCTB;
  NchCentrality zdcCent = centrality(zdcSum,ctb);

  return(
	 zdcCent >= mZdcCtbCent[1] &&  // less central has higher value
	 zdcCent <= mZdcCtbCent[0] 
	 );
}

bool 
CutRc::AcceptHMinusCent(StHiMicroEvent* event)
{
  int nHMinus = event->mNUncorrectedNegativePrimaries;
  NchCentrality cent = centralityHMinus(nHMinus);

  return(
	 cent >= mHMinusCent[1] && // less central has higher value
	 cent <= mHMinusCent[0]
	 );	 
}

bool 
CutRc::AcceptNchCent(StHiMicroEvent* event)
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
CutRc::AcceptVertexZ(StHiMicroEvent* event)
{

  return(
	 event->mVertexZ >= mVertexZ[0] &&
	 event->mVertexZ <= mVertexZ[1] &&
	 ( (!mVertexZSkip) ? true : fabs(event->mVertexZ)>mVertexZSkip )
	 );    
}

bool
CutRc::Accept(StHiMicroTrack* track)
{
  return (
	  track &&
	  AcceptFitPts(track) &&
	  AcceptEta(track)    &&
	  AcceptSDcaGl(track)
	  );
}

bool
CutRc::AcceptNoEta(StHiMicroTrack* track)
{
  return (
	  track &&
	  AcceptFitPts(track) &&
	  AcceptSDcaGl(track)
	  );
}

bool
CutRc::AcceptFitPts(StHiMicroTrack* track)
{
  return(
	 track->mFitPts >= mFitPts[0] &&
	  track->mFitPts <= mFitPts[1] 
	 );
}

bool
CutRc::AcceptEta(StHiMicroTrack* track)
{
  return(
	 track->mEtaPr    >= mEta[0]    &&
	 track->mEtaPr    <= mEta[1]    
	 );
}

bool
CutRc::AcceptSDcaGl(StHiMicroTrack* track)
{
  return(
	 track->mDcaGl >= mSDcaGl[0]    &&
	 track->mDcaGl <= mSDcaGl[1]    
	 );
}

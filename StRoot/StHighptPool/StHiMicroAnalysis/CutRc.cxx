/***************************************************************************  
 *  
 * $Id: CutRc.cxx,v 1.2 2002/04/03 00:23:27 jklay Exp $  
 *  
 * Author: Bum Choi, UT Austin, Apr 2002  
 *  
 ***************************************************************************  
 *  
 * Description:  Class for making standardized cuts in highpt analysis  
 *               
 *               
 ***************************************************************************
 *
 * $Log: CutRc.cxx,v $
 * Revision 1.2  2002/04/03 00:23:27  jklay
 * Fixed private member access bugs in analysis code
 *
 * Revision 1.1  2002/04/02 20:05:17  jklay
 * Bums analysis tools for highpt uDSTs
 *
 * 
 **************************************************************************/
#include "CutRc.h"

bool
CutRc::AcceptTrackHalf(StHiMicroTrack* track, float vtxZ)
{
  if(VertexSkipOn() && 
     !IsOutSide(track->FirstZ(),track->LastZ(),mVertexZSkip)) return false;
  if(mHitAvoid &&
     !IsOutSide(track->FirstZ(),track->LastZ(),mHitAvoid)) return false;
  if(Half() && !IsSameSide(vtxZ,track->FirstZ(),track->LastZ())) return false;
  if(HitHalf() && !IsHitSameSide(track->FirstZ(),track->LastZ())) 
    return false;
  if(GeomHalf() && !IsGeomSameSide(vtxZ,track->DipAngleGl()))
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
	  event->Centrality() >= mFlowCent[0] &&
	  event->Centrality() <= mFlowCent[1]
	 );
}

bool
CutRc::AcceptZdcCtbCent(StHiMicroEvent* event)
{
  float zdcSum = event->ZDCe() + event->ZDCw();
  float ctb    = event->CTB();
  NchCentrality zdcCent = centrality(zdcSum,ctb);

  return(
	 zdcCent >= mZdcCtbCent[1] &&  // less central has higher value
	 zdcCent <= mZdcCtbCent[0] 
	 );
}

bool 
CutRc::AcceptHMinusCent(StHiMicroEvent* event)
{
  int nHMinus = event->NUncorrectedNegativePrimaries();
  NchCentrality cent = centrality(nHMinus);

  return(
	 cent >= mHMinusCent[1] && // less central has higher value
	 cent <= mHMinusCent[0]
	 );
	 
}

bool
CutRc::AcceptVertexZ(StHiMicroEvent* event)
{

  return(
	 event->VertexZ() >= mVertexZ[0] &&
	 event->VertexZ() <= mVertexZ[1] &&
	 ( (!mVertexZSkip) ? true : fabs(event->VertexZ())>mVertexZSkip )
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
	 track->FitPts() >= mFitPts[0] &&
	  track->FitPts() <= mFitPts[1] 
	 );
}

bool
CutRc::AcceptEta(StHiMicroTrack* track)
{
  return(
	 track->EtaPr()    >= mEta[0]    &&
	 track->EtaPr()    <= mEta[1]    
	 );
}

bool
CutRc::AcceptSDcaGl(StHiMicroTrack* track)
{
  return(
	 track->DcaGl() >= mSDcaGl[0]    &&
	 track->DcaGl() <= mSDcaGl[1]    
	 );
}

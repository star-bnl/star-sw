#include "CutRc.h"

//------------------------------------------------------------
//This is the STANDARD Event Cut - Should always be used
//definitions of internal cuts are listed in order beneath
//------------------------------------------------------------
bool 
CutRc::Accept(StHiMicroEvent* event)
{
  
  return(
	 AcceptVertexZ(event) && AcceptCent(event) && AcceptTrgWord(event)
	 );    
}
//--------------

bool
CutRc::AcceptVertexZ(StHiMicroEvent* event)
{

  return(
	 event->VertexZ() >= mVertexZ[0] &&
	 event->VertexZ() <= mVertexZ[1]
	 );    
}
//--------------

bool
CutRc::AcceptEastSideVertexZ(StHiMicroEvent* event)
{
  //East is negative
  return( event->VertexZ() < 0 );    
}
//--------------

bool
CutRc::AcceptWestSideVertexZ(StHiMicroEvent* event)
{
  //West is positive
  return( event->VertexZ() > 0 );    
}
//--------------

bool 
CutRc::AcceptCent(StHiMicroEvent* event)
{
  bool acceptCent = false;
  if (mFlowCent[0] >= 0 && mFlowCent[1] >= 0)
   { acceptCent=AcceptFlowCent(event); }
  else
   { acceptCent=AcceptZDCCent(event); }

  return acceptCent;
}
//--------------

bool
CutRc::AcceptFlowCent(StHiMicroEvent* event)
{
  return (
	  flowCentrality(event->NUncorrectedPrimaries()) >= mFlowCent[0] &&
	  flowCentrality(event->NUncorrectedPrimaries()) <= mFlowCent[1]
	 );
}
//----------------

bool
CutRc::AcceptZDCCent(StHiMicroEvent* event)
{
  return (
	  event->ZDCe()+event->ZDCw() >= mZDCSum[0] &&
	  event->ZDCe()+event->ZDCw() <= mZDCSum[1]
	 );
}
//----------------

bool 
CutRc::AcceptTrgWord(StHiMicroEvent* event)
{
  bool acceptTrgWord = false;
 
 if (event->L3UnbiasedTrigger()) {	//First check for bias, then check the word
//   if (mFlowCent[0] >= 0 && mFlowCent[1] >= 0) { 
     if (event->L0TriggerWord() == 4096) acceptTrgWord = true; //Hadronic Minbias
//     } else { 
     if (event->L0TriggerWord() == 4352) acceptTrgWord = true; //Hadronic Central
//     }
 }

  return acceptTrgWord;

}
//---------------

//--------------------------------
//Composite Track Cuts
//Individual cuts listed beneath
//--------------------------------
bool
CutRc::Accept(StHiMicroTrack* track)
{
  return (
	  track 
	&&  AcceptFitPts(track)
	&&  AcceptEta(track)
	&&  AcceptSDcaGl(track)
	&&  AcceptFirstPadrow(track)
        && AcceptSameSector(track)
	  );
}
//--------------

bool
CutRc::AcceptNoDca(StHiMicroTrack* track)
{
  return (
	  track 
	&& AcceptFitPts(track) 
	&& AcceptEta(track)
	&& AcceptFirstPadrow(track)
        && AcceptSameSector(track)
	  );
}
//----------------

bool
CutRc::AcceptNoEta(StHiMicroTrack* track)
{
  return (
	  track 
	&& AcceptFitPts(track) 
	&& AcceptSDcaGl(track) 
	&& AcceptFirstPadrow(track)
        && AcceptSameSector(track)
	  );
}
//----------------

//-------------------------
//Individual Track Cuts
//-------------------------
bool
CutRc::AcceptFitPts(StHiMicroTrack* track)
{
  return(
	 track->FitPts() >= mFitPts[0] &&
	  track->FitPts() <= mFitPts[1] 
	 );
}
//---------------

bool
CutRc::AcceptEta(StHiMicroTrack* track)
{
  return(
	 track->EtaPr()    >= mEta[0]    &&
	 track->EtaPr()    <= mEta[1]    
	 );
}
//---------------

bool
CutRc::AcceptSDcaGl(StHiMicroTrack* track)
{
  return(
	 track->DcaGl() >= mSDcaGl[0]    &&
	 track->DcaGl() <= mSDcaGl[1]    &&
	 track->DcaXYGl() >= mDcaXYGl[0] &&
	 track->DcaXYGl() <= mDcaXYGl[1]
	 );
}
//---------------

bool 
CutRc::AcceptFirstPadrow(StHiMicroTrack* track)
{
  int firstPadrow = track->FirstPadRow();

  return (track && firstPadrow>=mFirstPadrow[0] 
	  && firstPadrow<=mFirstPadrow[1]);
  
}
//----------------

bool 
CutRc::AcceptSameSector(StHiMicroTrack* track)
{

  //The track begins and ends in the same sector
  //but only if we select for that in the cuts, 
  //otherwise, pass on through...
  if (mSameSector) { 
    return (track && track->FirstSector() == track->LastSector());
  } else {
    return true;
  }

}
//----------------

bool 
CutRc::AcceptWestSideTrack(StHiMicroTrack* track)
{
  //The track was on the west side only
  //Sectors 1-12 are on the West side
  return (track && track->FirstSector() <= 12 && track->LastSector() <= 12);
  
}
//----------------

bool 
CutRc::AcceptEastSideTrack(StHiMicroTrack* track)
{
  //The track was on the east side only
  //Sectors 13-24 are on the East side
  return (track && track->FirstSector() >= 13 && track->LastSector() >= 13);
  
}
//----------------

bool
CutRc::AcceptTrackVtxZHalf(StHiMicroTrack* track, Float_t vtxZ)
{   
   //Track and vertex on the same side of the central membrane
   if ((AcceptWestSideTrack(track) && vtxZ > 0) || (AcceptEastSideTrack(track) && vtxZ < 0))
     { return true; } else { return false; }

}  

bool
CutRc::AcceptTrackHalf(StHiMicroTrack* track)
{   
   //Track only on one side of the central membrane
   if (mCrossCM) {
     return true;
   } else {
     if (AcceptWestSideTrack(track) || AcceptEastSideTrack(track))
       { return true; } else { return false; }
   }
}  


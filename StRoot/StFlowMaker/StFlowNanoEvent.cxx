////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowNanoEvent.cxx,v 1.6 2000/05/26 21:29:30 posk Exp $
//
// Author: Sergei Voloshin and Raimond Snellings, March 2000
//
// Description:  A persistent Flow nano DST
//
//  The StFlowNanoEvent class has a simple event structure:
//        TClonesArray    *fTracks;
//
//        Int_t           mNtrack;                  // track number
//        Int_t           mEventID;                 // event ID
//        UInt_t          mOrigMult;                // number of StEvent tracks
//        UInt_t          mCentrality;              // centrality bin
//        StThreeVectorF  mVertexPos;               // primary vertex position
//
//   The StFlowNanoEvent data member fTracks is a pointer to a TClonesArray.
//   It is an array of a variable number of tracks per event.
//   Each element of the array is an object of class StFlowTrack 
//
////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowNanoEvent.cxx,v $
// Revision 1.6  2000/05/26 21:29:30  posk
// Protected Track data members from overflow.
//
// Revision 1.5  2000/05/20 00:55:17  posk
// Condensed flownanoevent.root somewhat.
//
// Revision 1.4  2000/05/16 20:59:33  posk
// Voloshin's flownanoevent.root added.
//
// Revision 1.2  2000/03/08 15:10:48  posk
//
// 
//////////////////////////////////////////////////////////////////////////

#include "StFlowNanoEvent.h"
#include "StFlowTrack.h"
#define PR(x) cout << "##### FlowNanoEvent: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowNanoEvent)

TClonesArray *StFlowNanoEvent::fgTracks = 0;

//-----------------------------------------------------------------------
StFlowNanoEvent::StFlowNanoEvent()
{
  // Create an StFlowNanoEvent object.
  // When the constructor is invoked for the first time, the class static
  // variable fgTracks is 0 and the TClonesArray fgTracks is created.
  
  if (!fgTracks) fgTracks = new TClonesArray("StFlowTrack", 4000);
  fTracks = fgTracks;
  mNtrack = 0;
}

//-----------------------------------------------------------------------
void StFlowNanoEvent::AddTrack(StFlowTrack* pFlowTrack )
{
  // Add a new track to the list of tracks for this StFlowNanoEvent.
  // To avoid calling the very time consuming operator new for each track,
  // the standard but not well know C++ operator "new with placement"
  // is called. If tracks[i] is 0, a new Track object will be created
  // otherwise the previous Track[i] will be overwritten.
  
  TClonesArray &tracks = *fTracks;
  tracks[mNtrack++] = pFlowTrack;
}

//-----------------------------------------------------------------------
void StFlowNanoEvent::Clear(Option_t *option)
{
  fTracks->Clear(option);
  mNtrack=0;
}









////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowPicoEvent.cxx,v 1.6 2000/09/15 22:51:31 posk Exp $
//
// Author: Sergei Voloshin and Raimond Snellings, March 2000
//
// Description:  A persistent Flow Pico DST
//
//  The StFlowPicoEvent class has a simple event structure:
//        TClonesArray    *fTracks;
//
//   The StFlowPicoEventHeader class:
//        Int_t           mNtrack;                  // track number
//        Int_t           mEventID;                 // event ID
//        UInt_t          mOrigMult;                // number of StEvent tracks
//        UInt_t          mCentrality;              // centrality bin
//        Float_t         mVertexX, Y, Z;           // primary vertex position
//
//   The StFlowPicoEvent data member fTracks is a pointer to a TClonesArray.
//   It is an array of a variable number of tracks per Event.
//   Each element of the array is an object of class StFlowTrack 
//
////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPicoEvent.cxx,v $
// Revision 1.6  2000/09/15 22:51:31  posk
// Added pt weighting for event plane calcualtion.
//
// Revision 1.5  2000/09/05 16:11:34  snelling
// Added global DCA, electron and positron
//
// Revision 1.4  2000/08/09 21:38:23  snelling
// PID added
//
// Revision 1.3  2000/06/01 18:26:39  posk
// Increased precision of Track integer data members.
//
// Revision 1.2  2000/05/26 21:29:31  posk
// Protected Track data members from overflow.
//
// Revision 1.1  2000/05/23 20:09:48  voloshin
// added StFlowPicoEvent, persistent FlowEvent as plain root TTree
//
// Revision 1.4  2000/05/16 20:59:33  posk
// Voloshin's flowPicoevent.root added.
//
// 
//////////////////////////////////////////////////////////////////////////

#include "StFlowPicoEvent.h"

ClassImp(StFlowPicoEvent)

TClonesArray *StFlowPicoEvent::fgTracks = 0;

//-----------------------------------------------------------------------

StFlowPicoEvent::StFlowPicoEvent()
{
  // Create an StFlowPicoEvent object.
  // When the constructor is invoked for the first time, the class static
  // variable fgTracks is 0 and the TClonesArray fgTracks is created.
  
  if (!fgTracks) fgTracks = new TClonesArray("StFlowPicoTrack", 4000);
  fTracks = fgTracks;
  mNtrack = 0;
  mVersion = 0;
}

//-----------------------------------------------------------------------

void StFlowPicoEvent::Clear(Option_t *option)
{
  fTracks->Clear(option);
  mNtrack=0;
}

//---------------------------------------------------------------------

void StFlowPicoEvent::AddTrack(StFlowPicoTrack* inputTrack) {
 
  TClonesArray &tracks = *fTracks;
  new(tracks[mNtrack++]) StFlowPicoTrack(inputTrack);
}
 






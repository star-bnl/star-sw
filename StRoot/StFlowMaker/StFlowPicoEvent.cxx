////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowPicoEvent.cxx,v 1.10 2002/03/15 16:43:22 snelling Exp $
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

#include <iostream.h>
#include "StFlowPicoEvent.h"
#include "TClonesArray.h"

ClassImp(StFlowPicoEvent)

TClonesArray *StFlowPicoEvent::fgTracks = 0;

//-----------------------------------------------------------------------

StFlowPicoEvent::StFlowPicoEvent() {
  // Create an StFlowPicoEvent object.
  // When the constructor is invoked for the first time, the class static
  // variable fgTracks is 0 and the TClonesArray fgTracks is created.
  
  if (!fgTracks) fgTracks = new TClonesArray("StFlowPicoTrack", 4000);
  fTracks  = fgTracks;
  mNtrack  = 0;
  mVersion = 0;
}

//-----------------------------------------------------------------------

void StFlowPicoEvent::Clear(Option_t *option) {
  fTracks->Clear(option);
  mNtrack=0;
}

//---------------------------------------------------------------------

void StFlowPicoEvent::AddTrack(StFlowPicoTrack* inputTrack) {
 
  TClonesArray &tracks = *fTracks;
  new(tracks[mNtrack++]) StFlowPicoTrack(inputTrack);
}
//-----------------------------------------------------------------------

UInt_t StFlowPicoEvent::CalcCentrality() {

  if (this->CenterOfMassEnergy() >= 199.) {
    if (fabs(this->MagneticField()) >= 4.) {
      int nhmin = this->UncorrNegMult();
      int nhplus = this->UncorrPosMult();
      int tracks = nhmin + nhplus;
      cout << "pFlowPicoEvent->SetCentralityYear2AuAuFull: " << tracks << endl;
      
      // Centrality for year=2, Au+Au and Full Field 

      int cent[] = {14,33,59,98,150,221,311,428,500};
      if (tracks < cent[0])       { mCentrality = 0; }
      else if (tracks < cent[1])  { mCentrality = 1; }
      else if (tracks < cent[2])  { mCentrality = 2; }
      else if (tracks < cent[3])  { mCentrality = 3; }
      else if (tracks < cent[4])  { mCentrality = 4; }
      else if (tracks < cent[5])  { mCentrality = 5; }
      else if (tracks < cent[6])  { mCentrality = 6; }
      else if (tracks < cent[7])  { mCentrality = 7; }
      else if (tracks < cent[8])  { mCentrality = 8; }
      else                        { mCentrality = 9; }
    }
    else {
      int nhmin = this->UncorrNegMult();
      int nhplus = this->UncorrPosMult();
      int tracks = nhmin + nhplus;
      cout << "pFlowPicoEvent->SetCentralityYear2AuAuHalf: " << tracks << endl;  

      // Centrality for year=2, Au+Au and Half Field

      int cent[] = {14,32,59,98,149,216,302,409,474};
      if (tracks < cent[0])       { mCentrality = 0; }
      else if (tracks < cent[1])  { mCentrality = 1; }
      else if (tracks < cent[2])  { mCentrality = 2; }
      else if (tracks < cent[3])  { mCentrality = 3; }
      else if (tracks < cent[4])  { mCentrality = 4; }
      else if (tracks < cent[5])  { mCentrality = 5; }
      else if (tracks < cent[6])  { mCentrality = 6; }
      else if (tracks < cent[7])  { mCentrality = 7; }
      else if (tracks < cent[8])  { mCentrality = 8; }
      else                        { mCentrality = 9; }
    }
  }
  else {
    int tracks = this->MultEta();
    cout << "pFlowPicoEvent->SetCentrality: " << tracks << endl;

    // Centrality for year=1

    int cent[] = {20,100,180,270,360,460,560,660,870};
    if (tracks < cent[0])       { mCentrality = 0; }
    else if (tracks < cent[1])  { mCentrality = 1; }
    else if (tracks < cent[2])  { mCentrality = 2; }
    else if (tracks < cent[3])  { mCentrality = 3; }
    else if (tracks < cent[4])  { mCentrality = 4; }
    else if (tracks < cent[5])  { mCentrality = 5; }
    else if (tracks < cent[6])  { mCentrality = 6; }
    else if (tracks < cent[7])  { mCentrality = 7; }
    else if (tracks < cent[8])  { mCentrality = 8; }
    else                        { mCentrality = 9; }
  }

  return mCentrality;
}
 
//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPicoEvent.cxx,v $
// Revision 1.10  2002/03/15 16:43:22  snelling
// Added a method to recalculate the centrality in StFlowPicoEvent
//
// Revision 1.9  2001/07/24 22:29:31  snelling
// First attempt to get a standard root pico file again, added variables
//
// Revision 1.8  2001/05/22 20:17:51  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.7  2000/12/12 20:22:06  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
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
//////////////////////////////////////////////////////////////////////////





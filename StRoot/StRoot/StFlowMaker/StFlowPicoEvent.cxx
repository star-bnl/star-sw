////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowPicoEvent.cxx,v 1.15 2005/07/06 19:39:26 fisyak Exp $
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

#include <Stiostream.h>
#include <math.h>
#include "StFlowPicoEvent.h"
#include "StFlowConstants.h"
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

  Int_t* cent = 0;
  Int_t  tracks = mMultEta; // converts UInt_t to Int_t

  if (mCenterOfMassEnergy == 0.) { // year=1
    cent = Flow::cent130;
  } else if (mCenterOfMassEnergy >= 199.) {
    if (fabs(mMagneticField) >= 4.) { // year=2, Au+Au, Full Field
      cent = Flow::cent200Full;
    } else { // year=2, Au+Au, Half Field
      cent = Flow::cent200Half;
    }
  } else if (mCenterOfMassEnergy <= 25.) { // year=2, 22 GeV
    cent = Flow::cent22;
  } else if (mCenterOfMassEnergy > 60. && mCenterOfMassEnergy < 65.) { // 62 GeV
    cent = Flow::cent62;
  }

  if      (tracks < cent[0])  { mCentrality = 0; }
  else if (tracks < cent[1])  { mCentrality = 1; }
  else if (tracks < cent[2])  { mCentrality = 2; }
  else if (tracks < cent[3])  { mCentrality = 3; }
  else if (tracks < cent[4])  { mCentrality = 4; }
  else if (tracks < cent[5])  { mCentrality = 5; }
  else if (tracks < cent[6])  { mCentrality = 6; }
  else if (tracks < cent[7])  { mCentrality = 7; }
  else if (tracks < cent[8])  { mCentrality = 8; }
  else                        { mCentrality = 9; }

  return mCentrality;
}

//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPicoEvent.cxx,v $
// Revision 1.15  2005/07/06 19:39:26  fisyak
// use templated version of StThreeVectorF and StPhysicalHelixD
//
// Revision 1.14  2004/08/24 20:24:36  oldi
// Minor modifications to avoid compiler warnings.
// Small bug fix (didn't affect anyone yet).
//
// Revision 1.13  2004/05/31 20:09:39  oldi
// PicoDst format changed (Version 7) to hold ZDC SMD information.
// Trigger cut modified to comply with TriggerCollections.
// Centrality definition for 62 GeV data introduced.
// Minor bug fixes.
//
// Revision 1.12  2003/09/02 17:58:12  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.11  2002/05/23 18:54:13  posk
// Moved centrality cuts into StFlowConstants
//
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





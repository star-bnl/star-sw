/***************************************************************************
 *
 * $Id: StHiMicroEvent.cxx,v 1.4 2007/07/12 19:41:00 fisyak Exp $
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  This is a uDST for highpt Analysis.
 *
 ***************************************************************************
 *
 * $Log: StHiMicroEvent.cxx,v $
 * Revision 1.4  2007/07/12 19:41:00  fisyak
 * Add includes for ROOT 5.16
 *
 * Revision 1.3  2003/09/02 17:58:36  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2002/06/05 02:31:51  jklay
 * New Nch centrality limits from Zhangbu added
 *
 * Revision 1.1  2002/04/02 19:36:15  jklay
 * Bums highpt uDST format
 *
 *
 **************************************************************************/
#include "StHiMicroEvent.h"
#include "Stiostream.h"
#include <math.h>
#define DEBUG 1
// set the static pointers to zero.
TClonesArray* StHiMicroEvent::mSTracks = 0;
TClonesArray* StHiMicroEvent::mSHits = 0;

//______________________

StHiMicroEvent::StHiMicroEvent()
  : mNTrack(0)
{
  //
  // create the clones arrays the first time any
  // StHiMicroEvent object is instantiated
  //
  Int_t trackSZ = 1000; 
  Int_t hitSZ = trackSZ*50;

  if(DEBUG)
    cout << "StHiMicroEvent::StHiMicroEvent" << endl
	 << "\tCreating the clonesarrays..." << endl;

  if(!mSTracks) 
    mTracks = new TClonesArray("StHiMicroTrack",trackSZ);
  if(!mSHits) 
    mHits = new TClonesArray("StHiMicroHit",hitSZ);

  if(DEBUG) cout << "\t...done" <<endl;

  //
  // set the static pointers so that we dont create the
  // the clonesarrays again
  //
  mSTracks = mTracks;
  mSHits = mHits;

}
//_______________________

StHiMicroEvent::~StHiMicroEvent()
{ 
  if(DEBUG) cout << "StHiMicroEvent::~StHiMicroEvent" << endl;

  
}

//______________________
//
// add a high pt track to the clones array
//
void
StHiMicroEvent::AddTrack(StHiMicroTrack* track)
{
  TClonesArray &tracks = *mTracks;
  //
  // use memberwise copy
  //
  new(tracks[mNTrack++]) StHiMicroTrack(*track);

}
//_______________________

void
StHiMicroEvent::AddHit(StHiMicroHit* hit)
{
  TClonesArray &hits = *mHits;
  //
  // memberwise copy...
  //
  new(hits[mNHit++]) StHiMicroHit(*hit);
  
}

//_______________________
//
// centrality definition as determined by the flow team
//
void
StHiMicroEvent::SetCentrality(Int_t N)
{
//Ok, this method is rewritten to be very similar to the FLOW version, incorporating different
//B-fields, etc.

  if (this->CenterOfMassEnergy() >= 199.) {
    if (fabs(this->MagneticField()) >= 4.) {
      cout << "StHiMicroEvent->SetCentralityYear2AuAuFull: " << N << endl;
      
      // Centrality for year=2, Au+Au and Full Field 
      // Zhangbu's corrected numbers as of 4-Jun-2002:
      int cent[] = {14,30,56,94,146,217,312,431,510};
      if (N < cent[0])       { mCentrality = 0; }
      else if (N < cent[1])  { mCentrality = 1; }
      else if (N < cent[2])  { mCentrality = 2; }
      else if (N < cent[3])  { mCentrality = 3; }
      else if (N < cent[4])  { mCentrality = 4; }
      else if (N < cent[5])  { mCentrality = 5; }
      else if (N < cent[6])  { mCentrality = 6; }
      else if (N < cent[7])  { mCentrality = 7; }
      else if (N < cent[8])  { mCentrality = 8; }
      else                        { mCentrality = 9; }
    }
    else {
      cout << "StHiMicroEvent->SetCentralityYear2AuAuHalf: " << N << endl;  

      // Centrality for year=2, Au+Au and Half Field

      int cent[] = {14,32,59,98,149,216,302,409,474};
      if (N < cent[0])       { mCentrality = 0; }
      else if (N < cent[1])  { mCentrality = 1; }
      else if (N < cent[2])  { mCentrality = 2; }
      else if (N < cent[3])  { mCentrality = 3; }
      else if (N < cent[4])  { mCentrality = 4; }
      else if (N < cent[5])  { mCentrality = 5; }
      else if (N < cent[6])  { mCentrality = 6; }
      else if (N < cent[7])  { mCentrality = 7; }
      else if (N < cent[8])  { mCentrality = 8; }
      else                        { mCentrality = 9; }
    }
  }
  else {
    cout << "StHiMicroEvent->SetCentrality: " << N << endl;

    cout << "WARNING:  MAKE SURE THE RIGHT ETA RANGE IS DEFINED BEFORE USING THESE DEFINITIONS!!!" << endl;
    //year 1, |eta|<0.75, year 2, |eta|<0.5

    // Centrality for year=1

    int cent[] = {20,100,180,270,360,460,560,660,870};
    if (N < cent[0])       { mCentrality = 0; }
    else if (N < cent[1])  { mCentrality = 1; }
    else if (N < cent[2])  { mCentrality = 2; }
    else if (N < cent[3])  { mCentrality = 3; }
    else if (N < cent[4])  { mCentrality = 4; }
    else if (N < cent[5])  { mCentrality = 5; }
    else if (N < cent[6])  { mCentrality = 6; }
    else if (N < cent[7])  { mCentrality = 7; }
    else if (N < cent[8])  { mCentrality = 8; }
    else                        { mCentrality = 9; }
  }

}

//_______________________

void
StHiMicroEvent::Clear(Option_t* option)
{
  mTracks->Clear(option);
  mHits->Clear(option);
  mNTrack = 0;
  mNHit = 0;
}

ClassImp(StHiMicroEvent)


/**********************************************************************
 *
 * $Id: StEbyeEvent.h,v 1.3 2000/09/01 22:59:11 jgreid Exp $
 *
 * Author: Jeff Reid, UW, July 2000
 *         incorporates elements of code by
 *         Poskanzer, Snellings, & Voloshin
 *
 **********************************************************************
 *
 * Description:  This maker defines the event structure for the
 *               event-by-event DST.
 *
 **********************************************************************
 *
 * $Log: StEbyeEvent.h,v $
 * Revision 1.3  2000/09/01 22:59:11  jgreid
 * version 1 revision ; multiple file handling + additional data members added
 *
 * Revision 1.2  2000/08/03 20:12:13  jgreid
 * added CTBm() convenience function
 *
 * Revision 1.1.1.1  2000/08/01 13:57:55  jgreid
 * EbyE DST creation and access tools
 *
 *
 *********************************************************************/

#ifndef _StEbyeEvent
#define _StEbyeEvent

#include <iostream.h>
#include "TObject.h"
#include "TClonesArray.h"
#include "StEbyeTrack.h"

class StEbyeEvent : public TObject {

 private:

  static const Int_t mVersion = 1;   // DST version number is 1

  Int_t mNtrack;                     // track number
  Int_t mNtrackG;                    // -"- && flag>0
  Int_t mNtrackG1;                   // -"- && |eta| < 1

  Int_t mEventID;                    // event ID
  Int_t mRunID;                      // run ID
  Int_t mOrigMult;                   // number of StEvent tracks
  Int_t mCentMult;                   // multiplicity used to determine centrality

  Float_t mCentrality;               // centrality measure

  Float_t mVx;                       // primary vertex position
  Float_t mVy;
  Float_t mVz;

  Int_t mCTBarray[32];               // coarse CTB array

  Float_t mZDCe;                     // ZDC East
  Float_t mZDCw;                     // ZDC West

  TClonesArray *fTracks;
  static TClonesArray *fgTracks;
    
 public:
  StEbyeEvent();
  virtual ~StEbyeEvent() { Clear(); }

  void Clear(Option_t *option ="");

  Int_t Version() const { return mVersion; };

  Int_t EventID() const { return mEventID; }; 
  Int_t RunID() const { return mRunID; };
  Int_t OrigMult() const { return mOrigMult; };
  Int_t CentMult() const { return mCentMult; };

  Float_t Centrality() const { return mCentrality; };

  Float_t Vx() const { return mVx; }
  Float_t Vy() const { return mVy; }
  Float_t Vz() const { return mVz; }

  Int_t CTBarray(Int_t i) const { return mCTBarray[i]; } 
  Int_t CTBm() const { Int_t m=0; for(Int_t i=0; i<32; i++) m+=mCTBarray[i]; return m; }

  Float_t ZDCe() const { return mZDCe; }
  Float_t ZDCw() const { return mZDCw; }
  
  void AddTrack(StEbyeTrack* pEbyeTrack);

  void SetNtrack(const Int_t ntrk) { mNtrack = ntrk; }
  void SetNtrackG(const Int_t ntrkg) { mNtrackG = ntrkg; }
  void SetNtrackG1(const Int_t ntrkg1) { mNtrackG1 = ntrkg1; }

  void SetEventID(const Int_t id) { mEventID = id; }
  void SetRunID(const Int_t id) { mRunID = id; }
  void SetOrigMult(const Int_t tracks) { mOrigMult = tracks; }
  void SetCentMult(const Int_t tracks) { mCentMult = tracks; }

  void SetCentrality(const UInt_t N);

  void SetVx(const Float_t vx) { mVx = vx; }
  void SetVy(const Float_t vy) { mVy = vy; }
  void SetVz(const Float_t vz) { mVz = vz; }

  void SetVertex(const Float_t vx, const Float_t vy, const Float_t vz) {
    mVx = vx; mVy = vy; mVz = vz; }

  void SetCTBarray(const Int_t i, const Int_t ctbav) { mCTBarray[i] = ctbav; }

  void SetZDCe(const Float_t zdce) { mZDCe = zdce; }
  void SetZDCw(const Float_t zdcw) { mZDCw = zdcw; }

  Int_t Ntrack() { return mNtrack; }
  TClonesArray *Tracks() { return fTracks; }
  
  ClassDef(StEbyeEvent,1)
};

#endif

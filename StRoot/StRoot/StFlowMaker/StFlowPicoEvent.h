//////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowPicoEvent.h,v 1.15 2004/05/31 20:09:40 oldi Exp $
//
// Author: Sergei Voloshin and Raimond Snellings, March 2000
//
// Description:  A persistent Flow Pico DST
// 
//////////////////////////////////////////////////////////////////////////

#ifndef StFlowPicoEvent__h
#define StFlowPicoEvent__h

#include "TObject.h"
#include "StFlowPicoTrack.h"

class TClonesArray;
//-----------------------------------------------------------

class StFlowPicoEvent : public TObject {
  
 public:

                StFlowPicoEvent();
  virtual       ~StFlowPicoEvent() { Clear(); }
  void          Clear(Option_t *option ="");
  void          AddTrack(StFlowPicoTrack* pFlowPicoTrack);
  TClonesArray* Tracks()     const { return fTracks; }
  Int_t         GetNtrack()  const { return mNtrack; }

  Int_t         Version()    const { return mVersion; }
  UInt_t        L0TriggerWord() const { return mL0TriggerWord; }
  UInt_t        OrigMult()   const { return mOrigMult; }
  UInt_t        UncorrNegMult() const { return mUncorrNegMult; }
  UInt_t        UncorrPosMult() const { return mUncorrPosMult; }
  UInt_t        MultEta()    const { return mMultEta; }

  UInt_t        Centrality() const { return mCentrality; }
  UInt_t        CalcCentrality(); 
  Float_t       VertexX()    const { return mVertexX; }
  Float_t       VertexY()    const { return mVertexY; }
  Float_t       VertexZ()    const { return mVertexZ; }
  Int_t         EventID()    const { return mEventID; }
  Int_t         RunID()      const { return mRunID; }
  Double_t      CenterOfMassEnergy() const { return mCenterOfMassEnergy; }
  Double_t      MagneticField() const { return mMagneticField; }
  Short_t       BeamMassNumberEast() const { return mBeamMassNumberEast; }
  Short_t       BeamMassNumberWest() const { return mBeamMassNumberWest; } 
  Float_t       CTB()        const { return mCTB; }
  Float_t       ZDCe()       const { return mZDCe; }
  Float_t       ZDCw()       const { return mZDCw; }
  Float_t       ZDCSMD(int eastwest,int verthori,int strip) const;
  
  void SetVersion(const Int_t ver)      { mVersion = ver; }
  void SetEventID(const Int_t id)       { mEventID = id; }
  void SetRunID(const Int_t id)         { mRunID = id; }
  void SetCenterOfMassEnergy(const Double_t cms) { mCenterOfMassEnergy = cms; }
  void SetMagneticField(const Double_t mf) { mMagneticField = mf; }
  void SetBeamMassNumberEast(const Short_t bme) { mBeamMassNumberEast = bme; }
  void SetBeamMassNumberWest(const Short_t bmw) { mBeamMassNumberWest = bmw; }
  void SetNtrack(const Int_t ntrk)      { mNtrack = ntrk; }
  void SetOrigMult(const UInt_t mult)   { mOrigMult = mult; }
  void SetL0TriggerWord(const UInt_t trigger) { mL0TriggerWord = trigger; }
  void SetUncorrNegMult(const UInt_t mult)   { mUncorrNegMult = mult; }
  void SetUncorrPosMult(const UInt_t mult)   { mUncorrPosMult = mult; }
  void SetMultEta(const UInt_t goodtracks) { mMultEta = goodtracks; }
  void SetCentrality(const UInt_t cent) { mCentrality = cent; }
  void SetVertexPos(const Float_t x, const Float_t y, const Float_t z) { 
    mVertexX=x; mVertexY=y; mVertexZ=z; }
  void SetCTB(const Float_t ctb)   {mCTB  = ctb; }
  void SetZDCe(const Float_t zdce) {mZDCe = zdce; }
  void SetZDCw(const Float_t zdcw) {mZDCw = zdcw; }
  void SetZDCSMD(int eastwest,int verthori,int strip,const Float_t zdcsmd);

 private:

  Int_t          mVersion;              // pico version
  Int_t          mNtrack;               // track number
  Int_t          mEventID;              // event ID
  Int_t          mRunID;                // run ID
  Double_t       mMagneticField;        // Magnetic Field 
  Double_t       mCenterOfMassEnergy;   // CMS Energy 
  Short_t        mBeamMassNumberEast;   // Mass Number of East Beam
  Short_t        mBeamMassNumberWest;   // Mass Number of West Beam
  UInt_t         mOrigMult;             // number of tracks
  UInt_t         mL0TriggerWord;        // L0 Trigger Word
  UInt_t         mUncorrNegMult;        // number of h-
  UInt_t         mUncorrPosMult;        // number of h+
  UInt_t         mMultEta;              // number of tracks with 
                                        // positive flag in 1.5 units of eta
  UInt_t         mCentrality;           // centrality bin
  Float_t        mVertexX;              // primary vertex position
  Float_t        mVertexY;              // primary vertex position
  Float_t        mVertexZ;              // primary vertex position
  Float_t        mCTB;                  // CTB value sum
  Float_t        mZDCe;                 // ZDC east
  Float_t        mZDCw;                 // ZDC west
  Float_t	 mZDCSMD[2][2][8];      // ZDCSMD calibrated, diff. from MuDst. 
  
  TClonesArray*        fTracks;
  static TClonesArray* fgTracks;
  
  ClassDef(StFlowPicoEvent,5)
};

inline void StFlowPicoEvent::SetZDCSMD(int eastwest,int verthori,int strip,const Float_t zdcsmd) {mZDCSMD[eastwest][verthori][strip-1] = zdcsmd;}

inline Float_t  StFlowPicoEvent::ZDCSMD(int eastwest,int verthori,int strip) const {return mZDCSMD[eastwest][verthori][strip-1];}


#endif

//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPicoEvent.h,v $
// Revision 1.15  2004/05/31 20:09:40  oldi
// PicoDst format changed (Version 7) to hold ZDC SMD information.
// Trigger cut modified to comply with TriggerCollections.
// Centrality definition for 62 GeV data introduced.
// Minor bug fixes.
//
// Revision 1.14  2002/03/15 16:43:22  snelling
// Added a method to recalculate the centrality in StFlowPicoEvent
//
// Revision 1.13  2002/02/05 07:19:38  snelling
// Quick fix for problems with backward compatibility (changed ClassDef back)
//
// Revision 1.12  2002/02/01 23:06:59  snelling
// Added entries for header information in flowPico (not everthing is available yet)
//
// Revision 1.11  2001/08/01 19:39:50  snelling
// Added the trigger word
//
// Revision 1.10  2001/07/27 01:26:35  snelling
// Added and changed variables for picoEvent. Changed trackCut class to StTrack
//
// Revision 1.9  2001/07/24 22:29:32  snelling
// First attempt to get a standard root pico file again, added variables
//
// Revision 1.8  2001/05/22 20:17:54  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.7  2000/12/12 20:22:06  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.6  2000/09/05 16:11:35  snelling
// Added global DCA, electron and positron
//
// Revision 1.5  2000/08/31 18:58:25  posk
// For picoDST, added version number, runID, and multEta for centrality.
// Added centrality cut when reading picoDST.
// Added pt and eta selections for particles corr. wrt event plane.
//
// Revision 1.4  2000/08/09 21:38:23  snelling
// PID added
//
// Revision 1.3  2000/06/01 18:26:39  posk
// Increased precision of Track integer data members.
//
// Revision 1.2  2000/05/26 21:29:32  posk
// Protected Track data members from overflow.
//
// Revision 1.1  2000/05/23 20:09:50  voloshin
// added StFlowPicoEvent, persistent FlowEvent as plain root TTree
//
// Revision 1.5  2000/05/16 20:59:34  posk
// Voloshin's flowPicoevent.root added.
//
//////////////////////////////////////////////////////////////////////////







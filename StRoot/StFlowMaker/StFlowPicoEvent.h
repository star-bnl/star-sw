//////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowPicoEvent.h,v 1.3 2000/06/01 18:26:39 posk Exp $
//
// Author: Sergei Voloshin and Raimond Snellings, March 2000
//
// Description:  A persistent Flow Pico DST
// 
//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPicoEvent.h,v $
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
// 
//////////////////////////////////////////////////////////////////////////

#ifndef StFlowPicoEvent__h
#define StFlowPicoEvent__h
#include <iostream.h>
#include "TObject.h"
#include "TClonesArray.h"

class StFlowPicoEvent : public TObject {
  
 public:

                StFlowPicoEvent();
  virtual       ~StFlowPicoEvent() { Clear(); }
  void          Clear(Option_t *option ="");
  TClonesArray* Tracks() const { return fTracks; }
  Int_t         GetNtrack()  const { return mNtrack; }

  UInt_t        OrigMult()   const { return mOrigMult; }
  UInt_t        Centrality() const { return mCentrality; }
  Float_t       VertexX()    const { return mVertexX; }
  Float_t       VertexY()    const { return mVertexY; }
  Float_t       VertexZ()    const { return mVertexZ; }
  Int_t         EventID()    const { return mEventID; }
  
  void SetEventID(const Int_t id)       { mEventID = id; }
  void SetNtrack(const Int_t ntrk)      { mNtrack = ntrk; }
  void SetOrigMult(const UInt_t mult)   { mOrigMult = mult; }
  void SetCentrality(const UInt_t cent) { mCentrality = cent; }
  void SetVertexPos(const Float_t x, const Float_t y, const Float_t z) { 
    mVertexX=x; mVertexY=y; mVertexZ=z; }
  
 private:

  Int_t          mNtrack;                     // track number
  Int_t          mEventID;                    // event ID
  UInt_t         mOrigMult;                   // number of tracks
  UInt_t         mCentrality;                 // centrality bin
  Float_t        mVertexX;                    // primary vertex position
  Float_t        mVertexY;                    // primary vertex position
  Float_t        mVertexZ;                    // primary vertex position
  
  TClonesArray*        fTracks;
  static TClonesArray* fgTracks;
  
  ClassDef(StFlowPicoEvent,1)
};


class StFlowPicoTrack : public TObject {

public:

            StFlowPicoTrack() { }
            StFlowPicoTrack(Float_t pt, 
                            Float_t eta, 
                            Float_t phi,
          		    Short_t charge,
                            Float_t dca,
                            Float_t chi2,
                            Int_t   fitPts,
                            Int_t   maxPts,
                            Float_t pidPiPlus,
                            Float_t pidPiMinus,
                            Float_t pidProton);
   virtual  ~StFlowPicoTrack() { }

   Float_t  Pt()         const { return mPt; }
   Float_t  Eta()        const { return mEta; }
   Float_t  Phi()        const { return mPhi; }
   Short_t  Charge()     const { return (Short_t)mCharge; }
   Float_t  Dca()        const { return mDca/10000.; }
   Float_t  Chi2()       const { return mChi2/10000.; }
   Int_t    FitPts()     const { return (Int_t)mFitPts; }
   Int_t    MaxPts()     const { return (Int_t)mMaxPts; }
   Float_t  PidPiPlus()  const { return mPidPiPlus/1000.; }
   Float_t  PidPiMinus() const { return mPidPiMinus/1000.; }
   Float_t  PidProton()  const { return mPidProton/1000.; }

private:
   Float_t   mPt;
   Float_t   mEta;
   Float_t   mPhi;
   Char_t    mCharge;
   UShort_t  mDca;
   UShort_t  mChi2;
   UChar_t   mFitPts;
   UChar_t   mMaxPts;
   Short_t   mPidPiPlus;      
   Short_t   mPidPiMinus;
   Short_t   mPidProton;

   ClassDef(StFlowPicoTrack,1)
};

#endif








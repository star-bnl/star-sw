//////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowPicoTrack.h,v 1.2 2000/09/15 22:51:33 posk Exp $
//
// Author: Raimond Snellings, March 2000
//
// Description:  A persistent Flow Pico DST
// 
//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPicoTrack.h,v $
// Revision 1.2  2000/09/15 22:51:33  posk
// Added pt weighting for event plane calcualtion.
//
// Revision 1.1  2000/09/05 16:11:38  snelling
// Added global DCA, electron and positron
//
// 
//////////////////////////////////////////////////////////////////////////

#ifndef StFlowPicoTrack__h
#define StFlowPicoTrack__h
#include "TObject.h"

//-----------------------------------------------------------

class StFlowPicoTrack : public TObject {

public:

            StFlowPicoTrack() {}
	    StFlowPicoTrack(StFlowPicoTrack* track);
   virtual  ~StFlowPicoTrack() {}

   Float_t  Pt()            const { return mPt; }
   Float_t  Eta()           const { return mEta; }
   Float_t  Phi()           const { return mPhi; }
   Short_t  Charge()        const { return mCharge; }
   Float_t  Dca()           const { return mDca; }
   Float_t  DcaGlobal()     const { return mDcaGlobal; }
   Float_t  Chi2()          const { return mChi2; }
   Int_t    FitPts()        const { return mFitPts; }
   Int_t    MaxPts()        const { return mMaxPts; }
   Float_t  PidPion()       const { return mPidPion/1000.; }
   Float_t  PidProton()     const { return mPidProton/1000.; }
   Float_t  PidKaon()       const { return mPidKaon/1000.; }
   Float_t  PidDeuteron()   const { return mPidDeuteron/1000.; }
   Float_t  PidElectron()   const { return mPidElectron/1000.; }
   Float_t  Dedx()          const { return mDedx; }

   void  SetPt(Float_t pt)           { mPt = pt; }
   void  SetEta(Float_t eta)         { mEta = eta; }
   void  SetDedx(Float_t Dedx)       { mDedx = Dedx; }
   void  SetPhi(Float_t phi)         { mPhi = phi; }
   void  SetCharge(Short_t charge)   { mCharge = charge; }
   void  SetDca(Float_t dca)         { mDca = dca; }
   void  SetDcaGlobal(Float_t gdca)  { mDcaGlobal = gdca; }
   void  SetChi2(Float_t chi2)       { mChi2 = chi2; }
   void  SetFitPts(Int_t fitPts)     { mFitPts = fitPts; }
   void  SetMaxPts(Int_t maxPts)     { mMaxPts = maxPts; }
   void  SetPidPion(Float_t pid)     { mPidPion = (Int_t)(pid*1000.); }
   void  SetPidProton(Float_t pid)   { mPidProton = (Int_t)(pid*1000.); }
   void  SetPidKaon(Float_t pid)     { mPidKaon = (Int_t)(pid*1000.); }
   void  SetPidDeuteron(Float_t pid) { mPidDeuteron = (Int_t)(pid*1000.); }
   void  SetPidElectron(Float_t pid) { mPidElectron = (Int_t)(pid*1000.); }

private:

   Float_t   mPt;
   Float_t   mEta;
   Float_t   mDedx;
   Float_t   mPhi;
   Short_t   mCharge;
   Float_t   mDca;
   Float_t   mDcaGlobal;
   Float_t   mChi2;
   Int_t     mFitPts;
   Int_t     mMaxPts;
   Int_t     mPidPion; 
   Int_t     mPidProton;
   Int_t     mPidKaon;
   Int_t     mPidDeuteron;
   Int_t     mPidElectron;

   ClassDef(StFlowPicoTrack,1)
};

#endif


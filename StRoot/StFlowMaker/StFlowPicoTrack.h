//////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowPicoTrack.h,v 1.3 2000/10/12 22:46:39 snelling Exp $
//
// Author: Raimond Snellings, March 2000
//
// Description:  A persistent Flow Pico DST
// 
//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPicoTrack.h,v $
// Revision 1.3  2000/10/12 22:46:39  snelling
// Added support for the new pDST's and the probability pid method
//
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
   Float_t  PtGlobal()      const { return mPtGlobal; }
   Float_t  Eta()           const { return mEta; }
   Float_t  EtaGlobal()     const { return mEtaGlobal; }
   Float_t  Phi()           const { return mPhi; }
   Float_t  PhiGlobal()     const { return mPhiGlobal; }
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
   Int_t    MostLikelihoodPID()  const{ return mMostLikelihoodPID; }
   Float_t  MostLikelihoodProb() const{ return mMostLikelihoodProb; }
   Int_t    ExtrapTag()     const{ return mExtrapTag; } 

   void  SetPt(Float_t pt)           { mPt = pt; }
   void  SetPtGlobal(Float_t gpt)    { mPtGlobal = gpt; }
   void  SetEta(Float_t eta)         { mEta = eta; }
   void  SetEtaGlobal(Float_t geta)  { mEtaGlobal = geta; }
   void  SetDedx(Float_t Dedx)       { mDedx = Dedx; }
   void  SetPhi(Float_t phi)         { mPhi = phi; }
   void  SetPhiGlobal(Float_t gphi)  { mPhiGlobal = gphi; }
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
   void  SetMostLikelihoodPID(Int_t val){ mMostLikelihoodPID=val; } 
   void  SetMostLikelihoodProb(Float_t val){ mMostLikelihoodProb=val; } 
   void  SetExtrapTag(Int_t val){ mExtrapTag=val; } 

private:

   Float_t   mPt;
   Float_t   mPtGlobal;
   Float_t   mEta;
   Float_t   mEtaGlobal;
   Float_t   mDedx;
   Float_t   mPhi;
   Float_t   mPhiGlobal;
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
   Int_t   mMostLikelihoodPID;  
   Float_t mMostLikelihoodProb;
   Int_t   mExtrapTag; //merging area tag.

   ClassDef(StFlowPicoTrack,1)
};

#endif


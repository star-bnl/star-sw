//////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowPicoTrack.h,v 1.7 2001/07/24 22:29:39 snelling Exp $
//
// Author: Raimond Snellings, March 2000
//
// Description:  A persistent Flow Pico DST
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
   Float_t  DcaSigned()     const { return mDcaSigned; }
   Float_t  DcaGlobal()     const { return mDcaGlobal; }
   Float_t  Chi2()          const { return mChi2; }
   Int_t    FitPts()        const { return mFitPts; }
   Int_t    MaxPts()        const { return mMaxPts; }
   Int_t    Nhits()         const { return mNhits; }
   Float_t  TrackLength()   const { return mTrackLength; }
   Float_t  PidPion()       const { return mPidPion/1000.; }
   Float_t  PidProton()     const { return mPidProton/1000.; }
   Float_t  PidKaon()       const { return mPidKaon/1000.; }
   Float_t  PidDeuteron()   const { return mPidDeuteron/1000.; }
   Float_t  PidElectron()   const { return mPidElectron/1000.; }
   Float_t  Dedx()          const { return mDedx; }
   Int_t    MostLikelihoodPID()  const { return mMostLikelihoodPID; }
   Float_t  MostLikelihoodProb() const { return mMostLikelihoodProb; }
   Int_t    ExtrapTag()     const { return mExtrapTag; }
   Float_t  ElectronPositronProb()  const { return mElectronPositronProb; }
   Float_t  PionPlusMinusProb()     const { return mPionPlusMinusProb; }
   Float_t  KaonPlusMinusProb()     const { return mKaonPlusMinusProb; }
   Float_t  ProtonPbarProb()        const { return mProtonPbarProb; }
   Float_t  FirstPointX()           const { return mFirstPointX; }
   Float_t  FirstPointY()           const { return mFirstPointY; }
   Float_t  FirstPointZ()           const { return mFirstPointZ; }
   UInt_t   TopologyMap0()          const { return mTopologyMap0; }
   UInt_t   TopologyMap1()          const { return mTopologyMap1; }

   void  SetPt(Float_t pt)           { mPt = pt; }
   void  SetPtGlobal(Float_t gpt)    { mPtGlobal = gpt; }
   void  SetEta(Float_t eta)         { mEta = eta; }
   void  SetEtaGlobal(Float_t geta)  { mEtaGlobal = geta; }
   void  SetDedx(Float_t Dedx)       { mDedx = Dedx; }
   void  SetPhi(Float_t phi)         { mPhi = phi; }
   void  SetPhiGlobal(Float_t gphi)  { mPhiGlobal = gphi; }
   void  SetCharge(Short_t charge)   { mCharge = charge; }
   void  SetDca(Float_t dca)         { mDca = dca; }
   void  SetDcaSigned(Float_t sdca)  { mDcaSigned = sdca; }
   void  SetDcaGlobal(Float_t gdca)  { mDcaGlobal = gdca; }
   void  SetChi2(Float_t chi2)       { mChi2 = chi2; }
   void  SetFitPts(Int_t fitPts)     { mFitPts = fitPts; }
   void  SetMaxPts(Int_t maxPts)     { mMaxPts = maxPts; }
   void  SetNhits(Int_t nhits)       { mNhits = nhits; }
   void  SetTrackLength(Float_t tl)  { mTrackLength = tl; }
   void  SetPidPion(Float_t pid)     { mPidPion = (Int_t)(pid*1000.); }
   void  SetPidProton(Float_t pid)   { mPidProton = (Int_t)(pid*1000.); }
   void  SetPidKaon(Float_t pid)     { mPidKaon = (Int_t)(pid*1000.); }
   void  SetPidDeuteron(Float_t pid) { mPidDeuteron = (Int_t)(pid*1000.); }
   void  SetPidElectron(Float_t pid) { mPidElectron = (Int_t)(pid*1000.); }
   void  SetMostLikelihoodPID(Int_t val){ mMostLikelihoodPID=val; } 
   void  SetMostLikelihoodProb(Float_t val){ mMostLikelihoodProb=val; } 
   void  SetExtrapTag(Int_t val){ mExtrapTag=val; }
   void  SetElectronPositronProb(Float_t val) { mElectronPositronProb = val; }
   void  SetPionPlusMinusProb(Float_t val) { mPionPlusMinusProb = val; }
   void  SetKaonPlusMinusProb(Float_t val) { mKaonPlusMinusProb = val; }
   void  SetProtonPbarProb(Float_t val) { mProtonPbarProb = val; }
   void  SetFirstPoint(const Float_t x, const Float_t y, const Float_t z) {
     mFirstPointX = x; mFirstPointY = y; mFirstPointZ = z; }
   void  SetTopologyMap(const UInt_t map0, const UInt_t map1) { 
     mTopologyMap0 = map0; mTopologyMap1 = map1; }

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
   Float_t   mDcaSigned;
   Float_t   mDcaGlobal;
   Float_t   mChi2;
   Int_t     mFitPts;
   Int_t     mMaxPts;
   Int_t     mNhits;
   Float_t   mTrackLength;
   Int_t     mPidPion; 
   Int_t     mPidProton;
   Int_t     mPidKaon;
   Int_t     mPidDeuteron;
   Int_t     mPidElectron;
   Int_t     mMostLikelihoodPID;  
   Float_t   mMostLikelihoodProb;
   Int_t     mExtrapTag;                  //merging area tag.
   Float_t   mElectronPositronProb;
   Float_t   mPionPlusMinusProb;
   Float_t   mKaonPlusMinusProb;
   Float_t   mProtonPbarProb;
   Float_t   mFirstPointX;
   Float_t   mFirstPointY;
   Float_t   mFirstPointZ;
   UInt_t    mTopologyMap0;
   UInt_t    mTopologyMap1;

   ClassDef(StFlowPicoTrack,1)
};

#endif

//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPicoTrack.h,v $
// Revision 1.7  2001/07/24 22:29:39  snelling
// First attempt to get a standard root pico file again, added variables
//
// Revision 1.6  2001/05/22 20:17:57  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.5  2000/12/12 20:22:06  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.4  2000/12/10 02:01:13  oldi
// A new member (StTrackTopologyMap mTopology) was added to StFlowPicoTrack.
// The evaluation of either a track originates from the FTPC or not is
// unambiguous now. The evaluation itself is easily extendible for other
// detectors (e.g. SVT+TPC). Old flowpicoevent.root files are treated as if
// they contain TPC tracks only (backward compatibility).
//
// Revision 1.3  2000/10/12 22:46:39  snelling
// Added support for the new pDST's and the probability pid method
//
// Revision 1.2  2000/09/15 22:51:33  posk
// Added pt weighting for event plane calcualtion.
//
// Revision 1.1  2000/09/05 16:11:38  snelling
// Added global DCA, electron and positron
//
//////////////////////////////////////////////////////////////////////////

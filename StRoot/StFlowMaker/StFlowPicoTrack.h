//////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowPicoTrack.h,v 1.9 2003/01/08 19:26:52 posk Exp $
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

   Float_t   Pt()            const { return mPt; }
   Float_t   PtGlobal()      const { return mPtGlobal; }
   Float_t   Eta()           const { return mEta; }
   Float_t   EtaGlobal()     const { return mEtaGlobal; }
   Float_t   Phi()           const { return mPhi; }
   Float_t   PhiGlobal()     const { return mPhiGlobal; }
   Short_t   Charge()        const { return mCharge; }
   Float_t   Dca()           const { return mDca; }
   Float_t   DcaSigned()     const { return mDcaSigned; }
   Float_t   DcaGlobal()     const { return mDcaGlobal; }
   Float_t   ZFirstPoint()   const { return mZFirstPoint; }
   Float_t   ZLastPoint()    const { return mZLastPoint; }
   Float_t   Chi2()          const { return mChi2; }
   Int_t     FitPts()        const { return mFitPts; }
   Int_t     MaxPts()        const { return mMaxPts; }
   Int_t     Nhits()         const { return mNhits; }
   Int_t     NdedxPts()      const { return mNdedxPts; }
   Float_t   TrackLength()   const { return mTrackLength; }
   Float_t   PidPion()       const { return mPidPion/1000.; }
   Float_t   PidProton()     const { return mPidProton/1000.; }
   Float_t   PidKaon()       const { return mPidKaon/1000.; }
   Float_t   PidDeuteron()   const { return mPidDeuteron/1000.; }
   Float_t   PidElectron()   const { return mPidElectron/1000.; }
   Float_t   Dedx()          const { return mDedx; }
   Int_t     MostLikelihoodPID()    const { return mMostLikelihoodPID; }
   Float_t   MostLikelihoodProb()   const { return mMostLikelihoodProb; }
   Int_t     ExtrapTag()     const { return mExtrapTag; }
   Float_t   ElectronPositronProb() const { return mElectronPositronProb; }
   Float_t   PionPlusMinusProb()    const { return mPionPlusMinusProb; }
   Float_t   KaonPlusMinusProb()    const { return mKaonPlusMinusProb; }
   Float_t   ProtonPbarProb()       const { return mProtonPbarProb; }
   Double_t  DcaGlobalX()           const { return (Double_t)mDcaGlobalX; }
   Double_t  DcaGlobalY()           const { return (Double_t)mDcaGlobalY; }
   Double_t  DcaGlobalZ()           const { return (Double_t)mDcaGlobalZ; }
   UInt_t    TopologyMap0()         const { return mTopologyMap0; }
   UInt_t    TopologyMap1()         const { return mTopologyMap1; }

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
   void  SetZFirstPoint(Float_t zFirst) { mZFirstPoint = zFirst; }
   void  SetZLastPoint(Float_t zLast) { mZLastPoint = zLast; }
   void  SetChi2(Float_t chi2)       { mChi2 = chi2; }
   void  SetFitPts(Int_t fitPts)     { mFitPts = fitPts; }
   void  SetMaxPts(Int_t maxPts)     { mMaxPts = maxPts; }
   void  SetNhits(Int_t nhits)       { mNhits = nhits; }
   void  SetNdedxPts(Int_t ndedxPts) { mNdedxPts = ndedxPts; }
   void  SetTrackLength(Float_t tl)  { mTrackLength = tl; }
   void  SetPidPion(Float_t pid)     { mPidPion = (Int_t)(pid*1000.); }
   void  SetPidProton(Float_t pid)   { mPidProton = (Int_t)(pid*1000.); }
   void  SetPidKaon(Float_t pid)     { mPidKaon = (Int_t)(pid*1000.); }
   void  SetPidDeuteron(Float_t pid) { mPidDeuteron = (Int_t)(pid*1000.); }
   void  SetPidElectron(Float_t pid) { mPidElectron = (Int_t)(pid*1000.); }
   void  SetMostLikelihoodPID(Int_t val){ mMostLikelihoodPID=val; } 
   void  SetMostLikelihoodProb(Float_t val) { mMostLikelihoodProb=val; } 
   void  SetExtrapTag(Int_t val){ mExtrapTag=val; }
   void  SetElectronPositronProb(Float_t val) { mElectronPositronProb = val; }
   void  SetPionPlusMinusProb(Float_t val) { mPionPlusMinusProb = val; }
   void  SetKaonPlusMinusProb(Float_t val) { mKaonPlusMinusProb = val; }
   void  SetProtonPbarProb(Float_t val) { mProtonPbarProb = val; }
   void  SetDcaGlobal3(const Double_t x, const Double_t y, const Double_t z) {
     mDcaGlobalX = (Float_t)x; mDcaGlobalY = (Float_t)y; mDcaGlobalZ = (Float_t)z; }
   void  SetTopologyMap(const UInt_t map0, const UInt_t map1) { 
     mTopologyMap0 = map0; mTopologyMap1 = map1; }

private:

   Float_t   mPt;                         // transverse momentum
   Float_t   mPtGlobal;                   // transverse momentum (global track)
   Float_t   mEta;                        // pseudorapidity
   Float_t   mEtaGlobal;                  // pseudorapidity (global track)
   Float_t   mDedx;                       // specific energy loss
   Float_t   mPhi;                        // azimuthal angle
   Float_t   mPhiGlobal;                  // azimuthal angle (global track)
   Short_t   mCharge;                     // charge
   Float_t   mDca;                        // distance of closest approach (? track model)
   Float_t   mDcaSigned;                  // 2D dca with sign (circle fit)
   Float_t   mDcaGlobal;                  // distance of closest approach for global track
   Float_t   mZFirstPoint;                // z of the first point
   Float_t   mZLastPoint;                 // z of the last point
   Float_t   mChi2;                       // chi squared
   Int_t     mFitPts;                     // number of hits used in fit
   Int_t     mMaxPts;                     // maximum possible number of hits 
   Int_t     mNhits;                      // number of hits on the track
   Int_t     mNdedxPts;                   // number of hits use for dE/dx
   Float_t   mTrackLength;                // lenght of the track (cm)
   Int_t     mPidPion;                    // deviant pid for pi+ and pi-
   Int_t     mPidProton;                  // deviant pid for p and pbar
   Int_t     mPidKaon;                    // deviant pid for K+ and K- 
   Int_t     mPidDeuteron;                // deviant pid for d and dbar 
   Int_t     mPidElectron;                // deviant pid for e+ and e-
   Int_t     mMostLikelihoodPID;          // pid with highest probability
   Float_t   mMostLikelihoodProb;         // probability for most likely pid
   Int_t     mExtrapTag;                  // merging area tag.
   Float_t   mElectronPositronProb;       // probability to be e+ or e-
   Float_t   mPionPlusMinusProb;          // probability to be pi+ or pi- 
   Float_t   mKaonPlusMinusProb;          // probability to be k+ or k-
   Float_t   mProtonPbarProb;             // probability to be p or pbar
   Float_t   mDcaGlobalX;                 // dca.x for global tracks (helix) 
   Float_t   mDcaGlobalY;                 // dca.y for global tracks (helix) 
   Float_t   mDcaGlobalZ;                 // dca.z for global tracks (helix) 
   UInt_t    mTopologyMap0;               // First 32 bits of Topology map
   UInt_t    mTopologyMap1;               // second 32 bits of Topology map

   ClassDef(StFlowPicoTrack,5)
};

#endif

//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowPicoTrack.h,v $
// Revision 1.9  2003/01/08 19:26:52  posk
// PhiWgt hists sorted on sign of z of first and last points.
// Version 6 of pico file.
//
// Revision 1.8  2001/07/27 01:26:40  snelling
// Added and changed variables for picoEvent. Changed trackCut class to StTrack
//
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

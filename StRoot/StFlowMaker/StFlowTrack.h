//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrack.h,v 1.28 2010/03/08 16:52:53 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//         FTPC added by Markus Oldenburg, MPI, Dec 2000
//
// Description: part of StFlowTrackCollection
//
///////////////////////////////////////////////////////////////////////

#ifndef StFlowTrack_h
#define StFlowTrack_h
#include <string.h>
#include <math.h>
#include "Rtypes.h"
#include "StObject.h"
#include "StFlowConstants.h"
#include "StTrackTopologyMap.h"
#include "StThreeVectorD.hh"

class StFlowTrack : public StObject {

public:

                StFlowTrack();
  virtual       ~StFlowTrack();

  Float_t       PidPiPlus()       const;
  Float_t       PidPiMinus()      const;
  Float_t       PidProton()       const;
  Float_t       PidKaonMinus()    const;
  Float_t       PidKaonPlus()     const;
  Float_t       PidAntiProton()   const;
  Float_t       PidDeuteron()     const;
  Float_t       PidAntiDeuteron() const;
  Float_t       PidElectron()     const;
  Float_t       PidPositron()     const;

  Float_t       Mass()        const; // direct cumulant maker
  Int_t         id()          const;
  Int_t         Flag()        const;

  const Char_t* Pid()         const;
  Float_t       Phi()         const;
  Float_t       PhiGlobal()   const;
  Float_t       Eta()         const;
  Float_t       EtaGlobal()   const;
  Float_t       ZFirstPoint() const;
  Float_t       ZLastPoint()  const;
  Float_t       Dedx()        const;
  Float_t       Pt()          const;
  Float_t       PtGlobal()    const;
  Float_t       P()           const;
  Float_t       PGlobal()     const;
  Float_t       Y()           const;
  Short_t       Charge()      const;
  Float_t       Dca()         const;
  Float_t       DcaSigned()   const;
  Float_t       DcaGlobal()   const;
  Float_t       Chi2()        const;
  Int_t         FitPts()      const;  // contains fit points in TPC xor FTPC only (SVT and/or SSD hits subtracted)
  Int_t         MaxPts()      const;  // contains possible hits in TPC xor FTPC only (SVT and/or SSD hits subtracted)
  Int_t         Nhits()       const;  // contains ALL hits on the track (TPC + SVT + SSD + FTPC east + FTPC west)
  Int_t         NdedxPts()    const;
  Float_t       TrackLength() const;
  Int_t Select(Int_t harmonic, Int_t selection, Int_t subevent= -1) const;
  Int_t         MostLikelihoodPID()    const; 
  Float_t       MostLikelihoodProb()   const; 
  Int_t         ExtrapTag()            const;
  Float_t       ElectronPositronProb() const;
  Float_t       PionPlusMinusProb()    const; 
  Float_t       KaonPlusMinusProb()    const; 
  Float_t       ProtonPbarProb()       const;
  StThreeVectorD DcaGlobal3()          const; 
  const StTrackTopologyMap& TopologyMap() const;

  void SetPidPiPlus(Float_t);
  void SetPidPiMinus(Float_t);
  void SetPidProton(Float_t);
  void SetPidKaonMinus(Float_t);
  void SetPidKaonPlus(Float_t);
  void SetPidAntiProton(Float_t);
  void SetPidDeuteron(Float_t);
  void SetPidAntiDeuteron(Float_t);
  void SetPidElectron(Float_t);
  void SetPidPositron(Float_t);
  void SetPid(const Char_t*);
  void SetPhi(Float_t);
  void SetPhiGlobal(Float_t);
  void SetEta(Float_t);
  void SetEtaGlobal(Float_t);
  void SetZFirstPoint(Float_t);
  void SetZLastPoint(Float_t);
  void SetDedx(Float_t);
  void SetPt(Float_t);
  void SetPtGlobal(Float_t);
  void SetCharge(Short_t);
  void SetDca(Float_t);
  void SetDcaSigned(Float_t);
  void SetDcaGlobal(Float_t);
  void SetChi2(Float_t);
  void SetFitPts(Int_t);
  void SetMaxPts(Int_t);
  void SetNhits(Int_t);
  void SetNdedxPts(Int_t);
  void SetTrackLength(Float_t);
  void SetSelect(Int_t harmonic, Int_t selection);
  void SetSubevent(Int_t harmonic, Int_t selection, Int_t subevent);
  void SetMostLikelihoodPID(Int_t); 
  void SetMostLikelihoodProb(Float_t); 
  void SetExtrapTag(Int_t); 
  void SetElectronPositronProb(Float_t);
  void SetPionPlusMinusProb(Float_t);
  void SetKaonPlusMinusProb(Float_t);
  void SetProtonPbarProb(Float_t);
  void SetDcaGlobal3(StThreeVectorD gdca3);
  void SetTopologyMap(StTrackTopologyMap map);
  
  void SetMass(Float_t); // direct cumulant maker
  void Setid(int);
  void SetFlag(int);

private:

  Int_t   mPidPiPlus;
  Int_t   mPidPiMinus;
  Int_t   mPidProton;
  Int_t   mPidKaonPlus;
  Int_t   mPidKaonMinus;
  Int_t   mPidAntiProton;
  Int_t   mPidDeuteron;
  Int_t   mPidAntiDeuteron;
  Int_t   mPidElectron;
  Int_t   mPidPositron;
  Char_t  mPid[12];
  Float_t mPhi;
  Float_t mPhiGlobal;
  Float_t mEta;
  Float_t mEtaGlobal;
  Float_t mZFirstPoint;
  Float_t mZLastPoint;
  Float_t mDedx;
  Float_t mPt;
  Float_t mPtGlobal;
  Short_t mCharge;
  Float_t mDca;
  Float_t mDcaSigned;
  Float_t mDcaGlobal;
  Float_t mChi2;
  Int_t   mFitPts; // contains fit points in TPC xor FTPC only (SVT and/or SSD hits subtracted)
  Int_t   mMaxPts; // contains possible hits in TPC xor FTPC only (SVT and/or SSD hits subtracted)
  Int_t   mNhits;  // contains ALL hits on the track (TPC + SVT + SSD + FTPC east + FTPC west)
  Int_t   mNdedxPts;
  Float_t mTrackLength;
  Int_t   mSelection;
  Short_t mSubevent[Flow::nHars][Flow::nSels];
  static  Float_t maxInt;
  Int_t   mMostLikelihoodPID;
  Float_t mMostLikelihoodProb;
  Int_t   mExtrapTag; //merging area tag.
  Float_t mElectronPositronProb;
  Float_t mPionPlusMinusProb;  
  Float_t mKaonPlusMinusProb;  
  Float_t mProtonPbarProb;
  StThreeVectorD mDcaGlobal3;  
  StTrackTopologyMap mTopology;

  Float_t mMass; // direct cumulant maker
  Int_t   mId;
  Int_t   mFlag;

  ClassDef(StFlowTrack, 1)                     // macro for rootcint
};

inline Float_t  StFlowTrack::PidPiPlus()    const { return mPidPiPlus/1000.; }
inline Float_t  StFlowTrack::PidPiMinus()   const { return mPidPiMinus/1000.; }
inline Float_t  StFlowTrack::PidProton()    const { return mPidProton/1000.; }
inline Float_t  StFlowTrack::PidKaonMinus() const { return mPidKaonMinus/1000.; }
inline Float_t  StFlowTrack::PidKaonPlus()  const { return mPidKaonPlus/1000.; }
inline Float_t  StFlowTrack::PidAntiProton() const { return mPidAntiProton/1000.; }
inline Float_t  StFlowTrack::PidDeuteron()  const { return mPidDeuteron/1000.; }
inline Float_t  StFlowTrack::PidAntiDeuteron() const { return mPidAntiDeuteron/1000.; }
inline Float_t  StFlowTrack::PidElectron()  const { return mPidElectron/1000.; }
inline Float_t  StFlowTrack::PidPositron()  const { return mPidPositron/1000.; }
inline const Char_t* StFlowTrack::Pid()     const { return mPid; }
inline Float_t  StFlowTrack::Phi()          const { return mPhi; }   
inline Float_t  StFlowTrack::PhiGlobal()    const { return mPhiGlobal; }   
inline Float_t  StFlowTrack::Eta()          const { return mEta; }     
inline Float_t  StFlowTrack::EtaGlobal()    const { return mEtaGlobal; }     
inline Float_t  StFlowTrack::ZFirstPoint()  const { return mZFirstPoint; }     
inline Float_t  StFlowTrack::ZLastPoint()   const { return mZLastPoint; }     
inline Float_t  StFlowTrack::Dedx()         const { return mDedx; }     
inline Float_t  StFlowTrack::Pt()           const { return mPt; }
inline Float_t  StFlowTrack::PtGlobal()     const { return mPtGlobal; }
inline Short_t  StFlowTrack::Charge()       const { return mCharge; }   
inline Float_t  StFlowTrack::Dca()          const { return mDca; }
inline Float_t  StFlowTrack::DcaSigned()    const { return mDcaSigned; }
inline Float_t  StFlowTrack::DcaGlobal()    const { return mDcaGlobal; }
inline Float_t  StFlowTrack::Chi2()         const { return mChi2; } 
inline Int_t    StFlowTrack::FitPts()       const { return mFitPts; }  
inline Int_t    StFlowTrack::MaxPts()       const { return mMaxPts; }  
inline Int_t    StFlowTrack::Nhits()        const { return mNhits; }
inline Int_t    StFlowTrack::NdedxPts()     const { return mNdedxPts; }  
inline Float_t  StFlowTrack::TrackLength()  const { return mTrackLength; }  
inline Int_t    StFlowTrack::MostLikelihoodPID() const
{ return mMostLikelihoodPID; } 
inline Float_t  StFlowTrack::MostLikelihoodProb() const 
{ return mMostLikelihoodProb; } 
inline Int_t    StFlowTrack::ExtrapTag()    const { return mExtrapTag; } 
inline Float_t  StFlowTrack::ElectronPositronProb() const { return mElectronPositronProb; }
inline Float_t  StFlowTrack::PionPlusMinusProb() const { return mPionPlusMinusProb; }
inline Float_t  StFlowTrack::KaonPlusMinusProb() const { return mKaonPlusMinusProb; }
inline Float_t  StFlowTrack::ProtonPbarProb() const { return mProtonPbarProb; }
inline StThreeVectorD StFlowTrack::DcaGlobal3() const { return mDcaGlobal3; }
inline const StTrackTopologyMap& StFlowTrack::TopologyMap() const { return mTopology; }

inline Float_t  StFlowTrack::Mass()         const { return mMass; }
inline Int_t  StFlowTrack::id()             const { return mId; }
inline Int_t  StFlowTrack::Flag()         const { return mFlag; }

inline Float_t StFlowTrack::P()             const { 
  float momentum = mPt/::sqrt(1-(tanh(mEta)*tanh(mEta)));
  return momentum; }

inline Float_t StFlowTrack::PGlobal()       const { 
  float momentum = mPtGlobal/::sqrt(1-(tanh(mEtaGlobal)*tanh(mEtaGlobal)));
  return momentum; }

inline Float_t StFlowTrack::Y()             const { 
  float M = 0.139; 
  if (strcmp(mPid, "none") == 0)          { M = 0.139; }
  else if (strcmp(mPid, "pi+") == 0)      { M = 0.139; }
  else if (strcmp(mPid, "pi-") == 0)      { M = 0.139; }
  else if (strcmp(mPid, "pr+") == 0)      { M = 0.938; }
  else if (strcmp(mPid, "pr-") == 0)      { M = 0.938; }
  else if (strcmp(mPid, "k+")  == 0)      { M = 0.494; }
  else if (strcmp(mPid, "k-")  == 0)      { M = 0.494; }
  else if (strcmp(mPid, "d+")  == 0)      { M = 1.876; }
  else if (strcmp(mPid, "d-")  == 0)      { M = 1.876; }
  else if (strcmp(mPid, "e-")  == 0)      { M = 0.0005; }
  else if (strcmp(mPid, "e+")  == 0)      { M = 0.0005; }
  double Pz = ::sqrt(this->P()*this->P() - mPt*mPt); 
  if (mEta < 0) { Pz = -Pz; }
  double E = ::sqrt(this->P()*this->P() + M*M);
  float rapidity = 0.5*::log((E + Pz)/(E - Pz));
  return rapidity;
}

inline Int_t StFlowTrack::Select(Int_t harmonic, Int_t selection,
 Int_t subevent) const {
  if (subevent == -1 || subevent == mSubevent[harmonic][selection]) {
    int bitShift = harmonic + Flow::nHars * selection;
    return (mSelection & (1 << bitShift)) ? kTRUE : kFALSE;
  }
  return kFALSE;         
}

inline void StFlowTrack::SetMostLikelihoodPID(Int_t val) {
         mMostLikelihoodPID=val; } 

inline void StFlowTrack::SetMostLikelihoodProb(Float_t val) {
         mMostLikelihoodProb=val; } 

inline void StFlowTrack::SetExtrapTag(Int_t val) {
         mExtrapTag=val; } 

inline void StFlowTrack::SetElectronPositronProb(Float_t val) {
  mElectronPositronProb = val; }

inline void StFlowTrack::SetPionPlusMinusProb(Float_t val) {
  mPionPlusMinusProb = val; }

inline void StFlowTrack::SetKaonPlusMinusProb(Float_t val) {
  mKaonPlusMinusProb = val; }

inline void StFlowTrack::SetProtonPbarProb(Float_t val) {
  mProtonPbarProb = val; }

inline void StFlowTrack::SetPidPiPlus(Float_t pid)  {
  if (fabs(pid) > maxInt) pid = maxInt; mPidPiPlus = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPidPiMinus(Float_t pid) {
  if (fabs(pid) > maxInt) pid = maxInt; mPidPiMinus = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPidProton(Float_t pid)  {
  if (fabs(pid) > maxInt) pid = maxInt; mPidProton = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPidKaonMinus(Float_t pid)  {
  if (fabs(pid) > maxInt) pid = maxInt; mPidKaonMinus = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPidKaonPlus(Float_t pid)  {
  if (fabs(pid) > maxInt) pid = maxInt; mPidKaonPlus = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPidAntiProton(Float_t pid)  {
  if (fabs(pid) > maxInt) pid = maxInt; mPidAntiProton = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPidDeuteron(Float_t pid)  {
  if (fabs(pid) > maxInt) pid = maxInt; mPidDeuteron = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPidAntiDeuteron(Float_t pid)  {
  if (fabs(pid) > maxInt) pid = maxInt; mPidAntiDeuteron = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPidElectron(Float_t pid)  {
  if (fabs(pid) > maxInt) pid = maxInt; mPidElectron = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPidPositron(Float_t pid)  {
  if (fabs(pid) > maxInt) pid = maxInt; mPidPositron = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPid(const Char_t* pid)  { strncpy(mPid, pid, 9);
                                                         mPid[9] = '\0'; }
inline void StFlowTrack::SetPhi(Float_t phi)        { mPhi = phi; }      

inline void StFlowTrack::SetPhiGlobal(Float_t gphi) { mPhiGlobal = gphi; }   

inline void StFlowTrack::SetEta(Float_t eta)        { mEta = eta; }       

inline void StFlowTrack::SetEtaGlobal(Float_t geta) { mEtaGlobal = geta; } 

inline void StFlowTrack::SetZFirstPoint(Float_t zFirst) { mZFirstPoint = zFirst; } 

inline void StFlowTrack::SetZLastPoint(Float_t zLast) { mZLastPoint = zLast; } 

inline void StFlowTrack::SetDedx(Float_t dedx)      { mDedx = dedx; }       

inline void StFlowTrack::SetPt(Float_t pt)          { mPt = pt; }              

inline void StFlowTrack::SetPtGlobal(Float_t gpt)   { mPtGlobal = gpt; }

inline void StFlowTrack::SetCharge(Short_t charge)  { mCharge = charge; }     

inline void StFlowTrack::SetDca(Float_t dca)        { mDca = dca; }

inline void StFlowTrack::SetDcaSigned(Float_t sdca) { mDcaSigned = sdca; }

inline void StFlowTrack::SetDcaGlobal(Float_t gdca) { mDcaGlobal = gdca; }

inline void StFlowTrack::SetChi2(Float_t chi2)      { mChi2 = chi2; }

inline void StFlowTrack::SetFitPts(Int_t fitPts)    { mFitPts = fitPts; }

inline void StFlowTrack::SetMaxPts(Int_t maxPts)    { mMaxPts = maxPts; }

inline void StFlowTrack::SetNhits(Int_t nhits)      { mNhits = nhits; }

inline void StFlowTrack::SetNdedxPts(Int_t ndedxPts) { mNdedxPts = ndedxPts; }

inline void StFlowTrack::SetTrackLength(Float_t tl) { mTrackLength = tl; }

inline void StFlowTrack::SetSelect(Int_t harmonic, Int_t selection) {
  int bitShift = harmonic + Flow::nHars * selection;
  mSelection |= 1 << bitShift; }

inline void StFlowTrack::SetSubevent(Int_t harmonic, Int_t selection,
 Int_t subevent) { mSubevent[harmonic][selection] = subevent; }

inline void StFlowTrack::SetDcaGlobal3(StThreeVectorD gdca3) { mDcaGlobal3 = gdca3; }

inline void StFlowTrack::SetTopologyMap(StTrackTopologyMap map) { mTopology = map; }

inline void StFlowTrack::SetMass(Float_t mass) { mMass = mass; }

inline void StFlowTrack::Setid(Int_t id) {mId = id;}

inline void StFlowTrack::SetFlag(Int_t flag) {mFlag = flag;}

#endif

//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTrack.h,v $
// Revision 1.28  2010/03/08 16:52:53  posk
// Added StFlowDirectCumulantMaker written by Dhevan Gangadharan.
//
// Revision 1.27  2004/08/18 00:19:21  oldi
// Several changes were necessary to comply with latest changes of MuDsts and StEvent:
//
// nHits, nFitPoints, nMaxPoints
// -----------------------------
// From now on
//  - the fit points used in StFlowMaker are the fit points within the TPC xor FTPC (vertex excluded).
//  - the max. possible points used in StFlowMAker are the max. possible points within the TPC xor FTPC (vertex excluded).
//  - the number of points (nHits; not used for analyses so far) are the total number of points on a track, i. e.
//    TPC + SVT + SSD + FTPCeast + FTPCwest [reading from HBT event gives a warning, but it seems like nobody uses it anyhow].
// - The fit/max plot (used to be (fit-1)/max) was updated accordingly.
// - The default cuts for fit points were changed (only for the FTPC, since TPC doesn't set default cuts).
// - All these changes are backward compatible, as long as you change your cuts for the fit points by 1 (the vertex used to
//   be included and is not included anymore). In other words, your results won't depend on old or new MuDst, StEvent,
//   PicoDsts as long as you use the new flow software (together with the latest MuDst and StEvent software version).
// - For backward compatibility reasons the number of fit points which is written out to the flowpicoevent.root file
//   includes the vertex. It is subtracted internally while reading back the pico files. This is completely hidden from the
//   user.
//
// zFirstPoint
// -----------
// The positions of the first point of tracks which have points in the TPC can lie outside of the TPC (the tracks can start in
// the SVT or SSD now). In this case, the first point of the track is obtained by extrapolating the track helix to the inner
// radius of the TPC.
//
// Revision 1.26  2003/09/02 17:58:13  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.25  2003/02/25 19:28:41  posk
// Changed a few unimportant default cuts.
//
// Revision 1.24  2003/01/08 19:26:52  posk
// PhiWgt hists sorted on sign of z of first and last points.
// Version 6 of pico file.
//
// Revision 1.23  2001/12/18 19:22:42  posk
// "proton" and "antiproton" changed to "pr+" and "pr-".
// Compiles on Solaris.
//
// Revision 1.22  2001/11/09 21:11:04  posk
// Switched from CERNLIB to TMath. Little q is now normalized.
//
// Revision 1.21  2001/07/27 01:26:43  snelling
// Added and changed variables for picoEvent. Changed trackCut class to StTrack
//
// Revision 1.20  2001/07/24 22:29:44  snelling
// First attempt to get a standard root pico file again, added variables
//
// Revision 1.19  2001/05/22 20:18:04  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.18  2000/12/12 20:22:06  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.17  2000/12/10 02:01:13  oldi
// A new member (StTrackTopologyMap mTopology) was added to StFlowPicoTrack.
// The evaluation of either a track originates from the FTPC or not is
// unambiguous now. The evaluation itself is easily extendible for other
// detectors (e.g. SVT+TPC). Old flowpicoevent.root files are treated as if
// they contain TPC tracks only (backward compatibility).
//
// Revision 1.16  2000/12/06 15:38:46  oldi
// Including FTPC.
//
// Revision 1.15  2000/10/12 22:46:40  snelling
// Added support for the new pDST's and the probability pid method
//
// Revision 1.13  2000/09/16 22:20:35  snelling
// Added selection on P and global DCA and fixed rapidity calulation
//
// Revision 1.12  2000/09/15 22:51:35  posk
// Added pt weighting for event plane calcualtion.
//
// Revision 1.11  2000/09/15 01:20:04  snelling
// Added methods for P and Y and added selection on Y
//
// Revision 1.10  2000/09/05 17:57:13  snelling
// Solaris needs math.h for fabs
//
// Revision 1.9  2000/09/05 16:11:39  snelling
// Added global DCA, electron and positron
//
// Revision 1.8  2000/08/09 21:38:23  snelling
// PID added
//
// Revision 1.7  2000/06/01 18:26:41  posk
// Increased precision of Track integer data members.
//
// Revision 1.6  2000/05/26 21:29:34  posk
// Protected Track data members from overflow.
//
// Revision 1.4  2000/05/16 20:59:35  posk
// Voloshin's flownanoevent.root added.
//
// Revision 1.3  2000/05/12 22:42:05  snelling
// Additions for persistency and minor fix
//
// Revision 1.1  2000/03/02 23:02:57  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.10  2000/02/29 22:00:56  posk
// Made SetPhiWeight inline, changed ImpactPar to Dca, etc.
//
// Revision 1.9  2000/02/18 22:49:57  posk
// Added PID and centrality.
//
// Revision 1.5  1999/12/15 22:01:29  posk
// Added StFlowConstants.hh
//
// Revision 1.4  1999/12/04 00:10:35  posk
// Works with the new StEvent
//
// Revision 1.3  1999/11/30 18:52:55  snelling
// First modification for the new StEvent
//
// Revision 1.2  1999/11/24 18:17:16  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
// Revision 1.1  1999/11/04 19:02:08  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//////////////////////////////////////////////////////////////////////

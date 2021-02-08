
/**
 * $Id $
 * \file  StTinyRcTrack.h
 * \brief   Persistent RC track class.
 * 
 *
 * \author Bum Choi, Manuel Calderon de la Barca
 * \date   March 2001
 *
 * These will be entered into the Matched and Contamination (split, merged, background)
 * collections as Pairs, with their corresponding MC track.
 * $Log $
*/

#ifndef StTinyRcTrack_H
#define StTinyRcTrack_H

#include "TObject.h"
#include <cmath>

class StTinyRcTrack {
 public:
  StTinyRcTrack();
  virtual ~StTinyRcTrack() {}
  void setValidGl() {mIsValidGl = 1;}
  void setValidPr() {mIsValidPr = 1;}
  void setPtPr(Float_t val){ mPtPr=val; }
  void setPzPr(Float_t val){ mPzPr=val; }
  void setEtaPr(Float_t val) { mEtaPr=val; }
  void setPhiPr(Float_t val) { mPhiPr=val; }
  void setDca(int idca) { mIsDca=idca; }
  void setDcaPr(Float_t val) { mDcaPr=val; }
//vp  void setDca00(Float_t val) { mDca00=val; }
  void setDcaXYPr(Float_t val) { mDcaXYPr=val; }
  void setDcaZPr(Float_t val) { mDcaZPrMcV=val; }
  void setDcaXYPrMcV(Float_t val) { mDcaXYPrMcV=val; }
  void setDcaZPrMcV(Float_t val) { mDcaZPr=val; }
  void setCurvPr(Float_t val) {mCurvPr=val; }
  void setTanLPr(Float_t val) {mTanLPr=val; }
  void setErrPr(Float_t val[5]) { for (short j=0; j<5; ++j) mErrP[j] = val[j]; }
    
  void setChi2Pr(Float_t val) { mChi2Pr=val; }
  void setFlag(Short_t val) { mFlag=val; }
  void setDedx(Float_t val) { mDedx=val; }

  void setPtGl(Float_t val){ mPtGl=val; }
  void setPzGl(Float_t val){ mPzGl=val; }
  void setEtaGl(Float_t val) { mEtaGl=val; }
  void setPhiGl(Float_t val) { mPhiGl=val; }
  void setDca00(Float_t val) { mDca00=val; }
  void setDcaGl(Float_t val) { mDcaGl=val; }
  void setDcaXYGl(Float_t val) { mDcaXYGl=val; }
  void setDcaZGl(Float_t val) { mDcaZGl=val; }
  void setDcaXYGlMcV(Float_t val) { mDcaXYGlMcV=val; }
  void setDcaZGlMcV(Float_t val) { mDcaZGlMcV=val; }
  void setCurvGl(Float_t val) { mCurvGl=val; }
  void setTanLGl(Float_t val) { mTanLGl=val; }
  void setErrGl(Float_t val[5]) { for (short j=0; j<5; ++j) mErrG[j] = val[j]; }

  void setPidPion(Float_t val) { mPidPion=val; }
  void setPidProton(Float_t val) { mPidProton=val; }
  void setPidKaon(Float_t val) { mPidKaon=val; }
  void setPidElectron(Float_t val) { mPidElectron=val; }
  
  void setFirstZ(Float_t val) { mFirstZ=val; }
  void setLastZ(Float_t val) { mLastZ=val; }
  void setFirstPadrow(Short_t val) { mFirstPadrow=val; }
  void setLastPadrow(Short_t val) { mLastPadrow=val; }
  void setFirstFitPadrow(Short_t val){ mFirstFitPadrow=val; }
  void setLastFitPadrow(Short_t val) { mLastFitPadrow=val; }

  void setRecoKey(Int_t val) { mRecoKey=val; }
  void setFirstSector(Short_t val) { mFirstSector=val; }
  void setLastSector(Short_t val) { mLastSector=val; }
  void setFitPts(Short_t val) { mFitPts=val; }
  void setFitFtpc(Short_t val) { mFitFtpc=val; }
  void setFitSvt(Short_t val) { mFitSvt=val; }
  void setFitSsd(Short_t val) { mFitSsd=val; }
  void setDedxPts(Short_t val) { mDedxPts=val; }
  void setAllPts(Short_t val) { mAllPts=val; }
  void setCharge(Short_t val) { mCharge=val; }
  void setNAssocMc(Short_t val) { mNAssocMc=val; }
  void setNPossible(Short_t val) { mNPossible=val; }

  void setEmcTowerAdc(Short_t val, size_t index) { if (index<3) mEmcTowerAdc[index]=val; }
  void setEmcEnergyRcHit(Float_t val, size_t index) { if (index<3) mEmcEnergyRcHit[index]=val; }
  void setEmcSoftIdHiTowerRc(Short_t val, size_t index) { if (index<3) mEmcSoftIdHiTowerRc[index]=val; }
  void setSeedQuality(UShort_t qa) {mSeedQA = qa;}
  float ptPr()const { return mPtPr; }
  float pxPr()const { return mPtPr*cos(mPhiPr); }
  float pyPr()const { return mPtPr*sin(mPhiPr); }
  float pzPr()const { return mPzPr; }
  float pPr()const { return ::sqrt((mPtPr*mPtPr) + (mPzPr*mPzPr)); }
  float etaPr() const { return mEtaPr; }
  float phiPr() const { return mPhiPr; }
  float dcaPr() const { return mDcaPr; }
  float dcaXYPr() const { return mDcaXYPr; }
  float dcaZPr() const { return mDcaZPr; }
  float curvPr() const { return mCurvPr; }
  float tanLPr() const { return mTanLPr; }
  float errPr(size_t i) const { return (i<5) ? mErrP[i] : 0; }
  	
  float chi2Pr() const { return mChi2Pr; }
  short flag() const { return mFlag; }
  float dedx() const { return mDedx; }
	
  float ptGl()const { return mPtGl; }
  float pxGl()const { return mPtGl*cos(mPhiGl); }
  float pyGl()const { return mPtGl*sin(mPhiGl); }
  float pzGl()const { return mPzGl; }
  float pGl()const { return ::sqrt((mPtGl*mPtGl) + (mPzGl*mPzGl)); }
  float etaGl() const { return mEtaGl; }
  float phiGl() const { return mPhiGl; }
  float dcaGl() const { return mDcaGl; }
  float dcaXYGl() const { return mDcaXYGl; }
  float dcaZGl() const { return mDcaZGl; }
  float curvGl() const { return mCurvGl; }
  float tanLGl() const { return mTanLGl; }
  float errGl(size_t i) const { return (i<5) ? mErrG[i] : 0; }
    
  float pidPion() const { return mPidPion; }
  float pidProton() const { return mPidProton; }
  float pidKaon() const { return mPidKaon; }
  float pidElectron() const { return mPidElectron; }
  	
  float firstZ() const { return mFirstZ; }
  float lastZ() const { return mLastZ; }
  short firstPadrow() const { return mFirstPadrow; }
  short lastPadrow() const { return mLastPadrow; }
  short firstFitPadrow()const { return mFirstFitPadrow; }
  short fastFitPadrow() const { return mLastFitPadrow; }

    int recoKey() const { return mRecoKey; }
  short firstSector() const { return mFirstSector; }
  short lastSector() const { return mLastSector; }
  short fitPts()     const { return mFitPts; }
  short fitPtsFtpc() const { return mFitFtpc; }
  short fitPtsSvt()  const { return mFitSvt; }
  short fitPtsSsd()  const { return mFitSsd; }
  short dedxPts() const { return mDedxPts; }
  short allPts() const { return mAllPts; }
  short charge() const { return mCharge; }
  short nAssocMc() const { return mNAssocMc; }
  short nPossiblePts() const { return mNPossible; }

  short emcTowerAdc(size_t index) const { if (index<3) return mEmcTowerAdc[index]; else return -999;}
  float emcEnergyRcHit(size_t index) const {if (index<3) return mEmcEnergyRcHit[index]; else return -999;}
  short emcSoftIdHiTowerRc(size_t index) const { if (index<3) return mEmcSoftIdHiTowerRc[index]; else return -999;}
  UShort_t seedQuality() {return mSeedQA;}
  
  virtual void Print(Option_t *option="") const;
private:
  // primary stuff
  Char_t     mIsValidGl;
  Char_t     mIsDca;
  Float_t    mPtPr;
  Float_t    mPzPr;
  Float_t    mEtaPr;
  Float_t    mPhiPr;
  Float_t    mDcaPr;     // 3D distance to primary vertex from primary track
  Float_t    mDcaXYPr;   // 2D dca with respect to primary vertex     
  Float_t    mDcaZPr;	 // Z distance to   -"-		       
//vp  Float_t    mDca00;	 // 2D dca with respect to x=0,y=0	       
  Float_t    mDcaXYPrMcV;// 2D dca with respect to MC vertex	       
  Float_t    mDcaZPrMcV; // Z  distance  -"-                          
  Float_t    mCurvPr;
  Float_t    mTanLPr;

    // the indices of the error matrix correspond to
    // 0 - error on y (track position along pad row direction)
    // 1 - error on z (track position along drift direction)
    // 2 - error on C*x0 where C is the track curvature and x0 is the helix center position along x
    // 3 - error on C, the curvature
    // 4 - error on tan(dipAngle)
  Float_t    mErrP[5];
    
  Float_t    mChi2Pr;
  Short_t    mFlag;
  Float_t    mDedx;
  // global stuff
  Float_t    mPtGl;
  Float_t    mPzGl;
  Float_t    mEtaGl;
  Float_t    mPhiGl;
  Float_t    mDca00;      // 2D DCA to x=y=0  
  Float_t    mDcaGl;      // 3D distance to primary vertex from global trazck 
  Float_t    mDcaXYGl;    // 2D dca with respect to primary vertex     
  Float_t    mDcaZGl;     // Z distance to   -"-		       
  Float_t    mDcaXYGlMcV; // 2D dca with respect to MC vertex	       
  Float_t    mDcaZGlMcV;  // Z  distance  -"-                          
  Float_t    mCurvGl;
  Float_t    mTanLGl;
  Float_t    mErrG[5];
  UShort_t   mSeedQA;
  

  // pid stuff from FlowMaker
  Float_t    mPidPion; 
  Float_t    mPidProton;
  Float_t    mPidKaon;
  Float_t    mPidElectron;
  
  
  //Short_t      mMostLikelihoodPID;  
  //Float_t    mMostLikelihoodProb;
  //Short_t      mExtrapTag; //merging area tag.
  
  
  // common stuff
  Float_t    mFirstZ;
  Float_t    mLastZ;
  Short_t    mFirstPadrow;
  Short_t    mLastPadrow;
  Short_t    mFirstFitPadrow;
  Short_t    mLastFitPadrow;
  Short_t    mFirstSector;
  Short_t    mLastSector;
  Int_t      mRecoKey;
    
  Short_t      mFitPts;
  Short_t      mFitSvt;
  Short_t      mFitSsd;
  Short_t      mFitFtpc;
  Short_t      mDedxPts;
  Short_t      mAllPts;
  Short_t      mCharge;
    
  Short_t      mNAssocMc;
  Short_t      mNPossible;
  Char_t       mIsValidPr;

  Short_t      mEmcTowerAdc[3];
  Float_t      mEmcEnergyRcHit[3];
  Short_t      mEmcSoftIdHiTowerRc[3];
  ClassDef(StTinyRcTrack,6)
};

#endif
//
// $Log: StTinyRcTrack.h,v $
// Revision 1.11  2018/01/03 18:18:09  genevb
// idTruths and keys moved from short to int
//
// Revision 1.10  2011/07/19 19:16:33  perev
// mDca00 added
//
// Revision 1.9  2010/08/31 20:16:48  fisyak
// Add track seedQuality
//
// Revision 1.8  2007/12/22 20:37:53  calderon
// Added EMC information to tracks.  MC info obtained from StMcTrack, Rec Info
// obtained from track extrapolation to BEMC of rec track.
//
// Revision 1.7  2007/02/23 17:07:01  fisyak
// Add Ssd and DCA
//
// Revision 1.6  2006/07/24 19:03:16  calderon
// Added parent key data member to StTinyMcTrack.
// Added reco key data member to StTinyRcTrack.
//
// Revision 1.5  2003/09/02 17:58:43  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2003/05/14 00:07:40  calderon
// Added members so that curvature and tan(lambda) pull plots can be made.
// Curvature and tan(lambda) are now stored.  The 5 diagonal elements of the
// error matrix are also stored.  The last two, for ittf tracks, are the error
// on curvature and error on tan(lambda).  This is done from both the global
// and the primary track.
//
// Revision 1.3  2003/05/08 02:09:20  calderon
// Added data members for svt and ftpc fit points for StTinyRcTrack.
// Added data members for svt and ftpc hits for StTinyMcTrack.
// Added methods to calculate px, py, and p from the available pt,  phi and pz, for
// global and primary momenta and also for monte carlo momentum.
// Cleaned up includes in StMiniMcEvent.
//
// Revision 1.2  2002/06/06 18:58:30  calderon
// Added $Log: StTinyRcTrack.h,v $
// Added Revision 1.11  2018/01/03 18:18:09  genevb
// Added idTruths and keys moved from short to int
// Added
// Added Revision 1.10  2011/07/19 19:16:33  perev
// Added mDca00 added
// Added
// Added Revision 1.9  2010/08/31 20:16:48  fisyak
// Added Add track seedQuality
// Added
// Added Revision 1.8  2007/12/22 20:37:53  calderon
// Added Added EMC information to tracks.  MC info obtained from StMcTrack, Rec Info
// Added obtained from track extrapolation to BEMC of rec track.
// Added
// Added Revision 1.7  2007/02/23 17:07:01  fisyak
// Added Add Ssd and DCA
// Added
// Added Revision 1.6  2006/07/24 19:03:16  calderon
// Added Added parent key data member to StTinyMcTrack.
// Added Added reco key data member to StTinyRcTrack.
// Added
// Added Revision 1.5  2003/09/02 17:58:43  perev
// Added gcc 3.2 updates + WarnOff
// Added
// Added Revision 1.4  2003/05/14 00:07:40  calderon
// Added Added members so that curvature and tan(lambda) pull plots can be made.
// Added Curvature and tan(lambda) are now stored.  The 5 diagonal elements of the
// Added error matrix are also stored.  The last two, for ittf tracks, are the error
// Added on curvature and error on tan(lambda).  This is done from both the global
// Added and the primary track.
// Added
// Added Revision 1.3  2003/05/08 02:09:20  calderon
// Added Added data members for svt and ftpc fit points for StTinyRcTrack.
// Added Added data members for svt and ftpc hits for StTinyMcTrack.
// Added Added methods to calculate px, py, and p from the available pt,  phi and pz, for
// Added global and primary momenta and also for monte carlo momentum.
// Added Cleaned up includes in StMiniMcEvent.
// Added
// Added mDedxPts data member, get and set methods, and updated ClassDef(StTinyRcTrack,1)
// to ClassDef(StTinyRcTrack,2) because of this change for schema evolution
//
// Revision 1.1  2002/05/30 01:20:58  calderon
// Classes for use in a general framework for extracting efficiencies
// from both embedding and full simulations
// (after GSTAR+TRS+StEvent+StMcEvent+StAssociationMaker)
// so that the information of the track matches gets stored persistently.
//
//

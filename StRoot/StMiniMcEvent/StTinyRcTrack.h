
/**
 * $Id $
 * \file  StTinyRcTrack.h
 * \brief   Persistent RC track class.
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 *
 * These will be entered into the Matched and Contamination (split, merged, background)
 * collections as Pairs, with their corresponding MC track.
 * $Log $
*/

#ifndef StTinyRcTrack_H
#define StTinyRcTrack_H

#include "TObject.h"

class StTinyRcTrack {
 public:
  StTinyRcTrack();
  virtual ~StTinyRcTrack();
  
  void setPtPr(Float_t val){ mPtPr=val; }
  void setPzPr(Float_t val){ mPzPr=val; }
  void setEtaPr(Float_t val) { mEtaPr=val; }
  void setPhiPr(Float_t val) { mPhiPr=val; }
  void setDcaPr(Float_t val) { mDcaPr=val; }
  void setDcaXYPr(Float_t val) { mDcaXYPr=val; }
  void setDcaZPr(Float_t val) { mDcaZPr=val; }
  
  void setChi2Pr(Float_t val) { mChi2Pr=val; }
  void setFlag(Short_t val) { mFlag=val; }
  void setDedx(Float_t val) { mDedx=val; }

  void setPtGl(Float_t val){ mPtGl=val; }
  void setPzGl(Float_t val){ mPzGl=val; }
  void setEtaGl(Float_t val) { mEtaGl=val; }
  void setPhiGl(Float_t val) { mPhiGl=val; }
  void setDcaGl(Float_t val) { mDcaGl=val; }
  void setDcaXYGl(Float_t val) { mDcaXYGl=val; }
  void setDcaZGl(Float_t val) { mDcaZGl=val; }

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

  void setFirstSector(Short_t val) { mFirstSector=val; }
  void setLastSector(Short_t val) { mLastSector=val; }
  void setFitPts(Short_t val) { mFitPts=val; }
  void setDedxPts(Short_t val) { mDedxPts=val; }
  void setAllPts(Short_t val) { mAllPts=val; }
  void setCharge(Short_t val) { mCharge=val; }
  void setNAssocMc(Short_t val) { mNAssocMc=val; }
  void setNPossible(Short_t val) { mNPossible=val; }

  float ptPr()const { return mPtPr; }
  float pzPr()const { return mPzPr; }
  float etaPr() const { return mEtaPr; }
  float phiPr() const { return mPhiPr; }
  float dcaPr() const { return mDcaPr; }
  float dcaXYPr() const { return mDcaXYPr; }
  float dcaZPr() const { return mDcaZPr; }
  	
  float chi2Pr() const { return mChi2Pr; }
  short flag() const { return mFlag; }
  float dedx() const { return mDedx; }
	
  float ptGl()const { return mPtGl; }
  float pzGl()const { return mPzGl; }
  float etaGl() const { return mEtaGl; }
  float phiGl() const { return mPhiGl; }
  float dcaGl() const { return mDcaGl; }
  float dcaXYGl() const { return mDcaXYGl; }
  float dcaZGl() const { return mDcaZGl; }
	
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
	
  short firstSector() const { return mFirstSector; }
  short lastSector() const { return mLastSector; }
  short fitPts() const { return mFitPts; }
  short dedxPts() const { return mDedxPts; }
  short allPts() const { return mAllPts; }
  short charge() const { return mCharge; }
  short nAssocMc() const { return mNAssocMc; }
  short nPossiblePts() const { return mNPossible; }
private:
  // primary stuff
  
  Float_t    mPtPr;
  Float_t    mPzPr;
  Float_t    mEtaPr;
  Float_t    mPhiPr;
  Float_t    mDcaPr;
  Float_t    mDcaXYPr;
  Float_t    mDcaZPr;
  
  Float_t    mChi2Pr;
  Short_t    mFlag;
  Float_t    mDedx;

  // global stuff
  Float_t    mPtGl;
  Float_t    mPzGl;
  Float_t    mEtaGl;
  Float_t    mPhiGl;
  Float_t    mDcaGl;
  Float_t    mDcaXYGl;
  Float_t    mDcaZGl;
  

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

  Short_t      mFitPts;
  Short_t      mDedxPts;
  Short_t      mAllPts;
  Short_t      mCharge;

  Short_t      mNAssocMc;
  Short_t      mNPossible;

  ClassDef(StTinyRcTrack,2)
};

#endif
//
// $Log: StTinyRcTrack.h,v $
// Revision 1.2  2002/06/06 18:58:30  calderon
// Added $Log$
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

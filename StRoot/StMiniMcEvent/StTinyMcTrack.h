/**
 * $Id: StTinyMcTrack.h,v 1.7 2007/02/23 17:07:01 fisyak Exp $
 * \file  StTinyMcTrack.h
 * \brief   Persistent MC track class.
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 * 
 * CVS Log is at the bottom of file.
*/

#ifndef StTinyMcTrack_H
#define StTinyMcTrack_H

#include "TObject.h"
#include <cmath>

class StTinyMcTrack : public TObject {
 public:
  StTinyMcTrack();
  virtual ~StTinyMcTrack() {}

  void setPtMc(Float_t val) { mPtMc=val; }
  void setPzMc(Float_t val) { mPzMc=val; }
  void setEtaMc(Float_t val) { mEtaMc=val; }
  void setPhiMc(Float_t val) { mPhiMc=val; }
  void setNHitMc(Short_t val) { mNHitMc=val; }
  void setNSvtHitMc(Short_t val) { mNSvtHitMc=val; }
  void setNSsdHitMc(Short_t val) { mNSsdHitMc=val; }
  void setNFtpcHitMc(Short_t val) { mNFtpcHitMc=val; }
  void setGeantId(Short_t val) { mGeantId=val; }
  void setChargeMc(Short_t val) { mChargeMc=val; }
  void setNAssocGl(Short_t val) { mNAssocGl=val; }
  void setNAssocPr(Short_t val) { mNAssocPr=val; }
  void setStopR(Float_t val) { mStopR=val; }
  void setKey(Short_t val) { mKey=val; }
  void setPrimary(Bool_t val) { mIsPrimary = val;}
  void setValid() {mIsValid = 1;}
  void setParentKey(Short_t val) { mParentKey=val; }
    
  float ptMc() const { return mPtMc; }
  float pxMc() const { return mPtMc*cos(mPhiMc); }
  float pyMc() const { return mPtMc*sin(mPhiMc); }
  float pzMc() const { return mPzMc; }
  float pMc()  const { return ::sqrt((mPtMc*mPtMc)+(mPzMc*mPzMc)); }
  float etaMc() const { return mEtaMc; }
  float phiMc() const { return mPhiMc; }
  short nHitMc() const { return mNHitMc; }
  short nSvtHitMc() const { return mNSvtHitMc; }
  short nSsdHitMc() const { return mNSsdHitMc; }
  short nFtpcHitMc() const { return mNFtpcHitMc; }
  short geantId() const { return mGeantId; }
  short chargeMc() const { return mChargeMc; }
  short nAssocGl() const { return mNAssocGl; }
  short nAssocPr() const { return mNAssocPr; }
  float stopR() const { return mStopR; }
  short key() const { return mKey; }
  Bool_t isPrimary() const {return mIsPrimary;}
  Bool_t isValid() {return  mIsValid;}
  virtual void Print(Option_t *option="") const;
  short parentKey() const { return mParentKey; }
    
private:
  // mc stuff
    
  Char_t     mIsValid;
  Float_t    mPtMc;
  Float_t    mPzMc;
  Float_t    mEtaMc;
  Float_t    mPhiMc;
  Short_t    mNHitMc;
  Short_t    mNSvtHitMc;
  Short_t    mNSsdHitMc;
  Short_t    mNFtpcHitMc;
  Short_t    mGeantId;
  Short_t    mChargeMc;
  Float_t    mStopR;
  Short_t    mKey;
  Short_t    mParentKey;
    
  // assoc stuff
  Short_t      mNAssocGl;
  Short_t      mNAssocPr;
  Bool_t     mIsPrimary;

  ClassDef(StTinyMcTrack,4)
};

#endif
//
// $Log: StTinyMcTrack.h,v $
// Revision 1.7  2007/02/23 17:07:01  fisyak
// Add Ssd and DCA
//
// Revision 1.6  2006/07/24 19:03:16  calderon
// Added parent key data member to StTinyMcTrack.
// Added reco key data member to StTinyRcTrack.
//
// Revision 1.5  2004/03/31 23:42:46  calderon
// Adding info to evaluate idTruth information.
// -Add key to StTinyMcTrack.h
// -Add dominatrack, common hits to dominatrack and average hit quality to StMiniMcPair.h
//
// Revision 1.4  2003/09/02 17:58:43  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.3  2003/05/08 02:09:20  calderon
// Added data members for svt and ftpc fit points for StTinyRcTrack.
// Added data members for svt and ftpc hits for StTinyMcTrack.
// Added methods to calculate px, py, and p from the available pt,  phi and pz, for
// global and primary momenta and also for monte carlo momentum.
// Cleaned up includes in StMiniMcEvent.
//
// Revision 1.2  2002/06/11 21:12:00  calderon
// fix typo, pttMc() -> ptMc()
//
// Revision 1.1  2002/05/30 01:20:58  calderon
// Classes for use in a general framework for extracting efficiencies
// from both embedding and full simulations
// (after GSTAR+TRS+StEvent+StMcEvent+StAssociationMaker)
// so that the information of the track matches gets stored persistently.
//
//

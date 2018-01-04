/**
 * $Id: StTinyMcTrack.h,v 1.14 2018/01/03 18:18:09 genevb Exp $
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
  void setNBemcHitMc(Short_t val) { mNBemcHitMc=val; }
  void setNBprsHitMc(Short_t val) { mNBprsHitMc=val; }
  void setNBsmdeHitMc(Short_t val) { mNBsmdeHitMc=val; }
  void setNBsmdpHitMc(Short_t val) { mNBsmdpHitMc=val; }
  void setNEemcHitMc(Short_t val) { mNEemcHitMc=val; }
  void setNEprsHitMc(Short_t val) { mNEprsHitMc=val; }
  void setNEsmduHitMc(Short_t val) { mNEsmduHitMc=val; }
  void setNEsmdvHitMc(Short_t val) { mNEsmdvHitMc=val; }
  void setGeantId(int val);
  void setPdgId(int val)	{ mPdgId   = val; }
  void setChargeMc(Short_t val) { mChargeMc= val; }
  void setNAssocGl(Short_t val) { mNAssocGl= val; }
  void setNAssocPr(Short_t val) { mNAssocPr= val; }
  void setStopR(Float_t val) { mStopR=val; }
  void setKey(Int_t val) { mKey=val; }
  void setPrimary(Bool_t val) { mIsPrimary = val;}
  void setValid() {mIsValid = 1;}
  void setParentKey(Int_t val) { mParentKey=val; }
  void setParentGeantId(int val);
  void setEmcEnergyMcHit(Float_t val,size_t index) {if (index<3) mEmcEnergyMcHit[index]=val;}
  void setEmcEnergyMcSum(Float_t val) {mEmcEnergyMcSum=val;}  
  void setEmcSoftIdHiTowerMc(Short_t val,size_t index) {if (index<3) mEmcSoftIdHiTowerMc[index]=val;}
  
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
  short nBemcHitMc() const { return mNBemcHitMc; }
  short nBprsHitMc() const { return mNBprsHitMc; }
  short nBsmdeHitMc() const { return mNBsmdeHitMc; }
  short nBsmdpHitMc() const { return mNBsmdpHitMc; }
  short nEemcHitMc() const { return mNEemcHitMc; }
  short nEprsHitMc() const { return mNEprsHitMc; }
  short nEsmduHitMc() const { return mNEsmduHitMc; }
  short nEsmdvHitMc() const { return mNEsmdvHitMc; }
    int geantId() const { return mGeantId; }
  short chargeMc() const { return mChargeMc; }
  short nAssocGl() const { return mNAssocGl; }
  short nAssocPr() const { return mNAssocPr; }
  float stopR() const { return mStopR; }
    int key() const { return mKey; }
  Bool_t isPrimary() const {return mIsPrimary;}
  Bool_t isValid() {return  mIsValid;}
  virtual void Print(Option_t *option="") const;
    int parentKey() const { return mParentKey; }
    int parentGeantId() const { return mParentGeantId; }
  float emcEnergyMcHit(size_t index) const { if (index<3) return mEmcEnergyMcHit[index]; else return -999;}
  float emcEnergyMcSum() const { return mEmcEnergyMcSum; }
  short emcSoftIdHiTowerMc(size_t index) const { if (index<3) return mEmcSoftIdHiTowerMc[index]; else return -999; }
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
  Short_t    mNBemcHitMc;
  Short_t    mNBprsHitMc;
  Short_t    mNBsmdeHitMc;
  Short_t    mNBsmdpHitMc;
  Short_t    mNEemcHitMc;
  Short_t    mNEprsHitMc;
  Short_t    mNEsmduHitMc;
  Short_t    mNEsmdvHitMc;
  UShort_t   mGeantId;		//Geant particle id
  Int_t      mPdgId;		//PDG particle id
  Short_t    mChargeMc;
  Float_t    mStopR;
  Int_t      mKey;		//Geant track id
  Int_t      mParentKey;	//Geant vertex id
  UShort_t   mParentGeantId;	//Geant Parent particle id
  Float_t    mEmcEnergyMcHit[3];
  Float_t    mEmcEnergyMcSum;
  Short_t    mEmcSoftIdHiTowerMc[3];
  
  // assoc stuff
  Short_t    mNAssocGl;		//Number of rc globals   assigned to this mc track
  Short_t    mNAssocPr;		//Number of rc primaries assigned to this mc track
  Bool_t     mIsPrimary;

  ClassDef(StTinyMcTrack,6)
};

#endif
//
// $Log: StTinyMcTrack.h,v $
// Revision 1.14  2018/01/03 18:18:09  genevb
// idTruths and keys moved from short to int
//
// Revision 1.13  2011/04/01 20:00:17  perev
// Comments++
//
// Revision 1.12  2011/02/24 17:58:03  perev
// change IO version
//
// Revision 1.11  2011/02/16 16:46:30  perev
// setPdgId imp added
//
// Revision 1.10  2011/02/16 00:49:48  perev
// mPdgId added
//
// Revision 1.9  2011/02/11 03:32:14  perev
// geantid now is ushort
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

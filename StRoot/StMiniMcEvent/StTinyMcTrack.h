/**
 * $Id: StTinyMcTrack.h,v 1.2 2002/06/11 21:12:00 calderon Exp $
 * \file  StTinyMcTrack.h
 * \brief   Persistent MC track class.
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 *
 * There will be a container of all MC tracks, and these will also be entered into the Matched and Contamination
 * collections as Pairs, with their corresponding RC track.
 * $Log: StTinyMcTrack.h,v $
 * Revision 1.2  2002/06/11 21:12:00  calderon
 * fix typo, pttMc() -> ptMc()
 *
*/

#ifndef StTinyMcTrack_H
#define StTinyMcTrack_H

#include "TObject.h"

class StTinyMcTrack : public TObject {
 public:
  StTinyMcTrack();
  virtual ~StTinyMcTrack();

  void setPtMc(Float_t val) { mPtMc=val; }
  void setPzMc(Float_t val) { mPzMc=val; }
  void setEtaMc(Float_t val) { mEtaMc=val; }
  void setPhiMc(Float_t val) { mPhiMc=val; }
  void setNHitMc(Short_t val) { mNHitMc=val; }
  void setGeantId(Short_t val) { mGeantId=val; }
  void setChargeMc(Short_t val) { mChargeMc=val; }
  void setNAssocGl(Short_t val) { mNAssocGl=val; }
  void setNAssocPr(Short_t val) { mNAssocPr=val; }
  void setStopR(Float_t val) { mStopR=val; }

  float ptMc() const { return mPtMc; }
  float pzMc() const { return mPzMc; }
  float etaMc() const { return mEtaMc; }
  float phiMc() const { return mPhiMc; }
  short nHitMc() const { return mNHitMc; }
  short geantId() const { return mGeantId; }
  short chargeMc() const { return mChargeMc; }
  short nAssocGl() const { return mNAssocGl; }
  short nAssocPr() const { return mNAssocPr; }
  float stopR() const { return mStopR; }

private:
  // mc stuff
    
  Float_t    mPtMc;
  Float_t    mPzMc;
  Float_t    mEtaMc;
  Float_t    mPhiMc;
  Short_t    mNHitMc;
  Short_t    mGeantId;
  Short_t    mChargeMc;
  Float_t    mStopR;

  // assoc stuff
  Short_t      mNAssocGl;
  Short_t      mNAssocPr;

  ClassDef(StTinyMcTrack,1)
};

#endif
//
// $Log: StTinyMcTrack.h,v $
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

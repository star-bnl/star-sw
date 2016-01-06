#ifndef STUEOFFAXISCONESJET
#define STUEOFFAXISCONESJET

#include "TRefArray.h"

#include "StUeJet.h"
#include "StUeOffAxisCones.h"

class StUeOffAxisConesJet : public StUeJet{
 public:
  StUeOffAxisConesJet(){
//    mPt = 0.;
//    mEta = 999;
//    mPhi = 0.;
//    mE = 0.;
      mDensity = 0.;
  }
  ~StUeOffAxisConesJet(){
  }
/*
  float pt() const { return mPt; }
  float eta() const { return mEta; }
  float phi() const { return mPhi; }
  float e() const { return mE; }

  void setPt(float pt) { mPt = pt; }
  void setEta(float eta) { mEta = eta; }
  void setPhi(float phi) { mPhi = phi; }
  void setE(float e) { mE = e; }
*/
  float density() const { return mDensity; }
  void setDensity(float d) { mDensity = d; }
  int numberOfCones() const { return mCones.GetEntriesFast(); }
  StUeOffAxisCones* cone(int i) {return (StUeOffAxisCones *)mCones.At(i); }
  StUeOffAxisCones* addCone(StUeOffAxisCones* cone){mCones.Add((TObject*)cone); return (StUeOffAxisCones *)mCones.Last(); }
 private:
/* 
  float mPt;
  float mEta;
  float mPhi;
  float mE;
*/
  float mDensity;
  TRefArray mCones;
  ClassDef(StUeOffAxisConesJet, 1);
};

#endif

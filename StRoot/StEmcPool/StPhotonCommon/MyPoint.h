#ifndef MYPOINT_H
#define MYPOINT_H

#include <TObject.h>
#include <TVector3.h>

class MyPoint : public TObject{
 protected:
  Float_t fEnergy;
  Int_t fQuality;
  TVector3 fPosition;
  Float_t fDisToTrack;
  //tow clusters
  Int_t fTowClusId[4];
  Float_t fTowClusEn[4];
  //smd clusters
  Int_t fNeta;
  Int_t fNphi;
  Float_t fEnEta;
  Float_t fSigmaEta;
  Float_t fEnPhi;
  Float_t fSigmaPhi;

  
 public:
  MyPoint();
  MyPoint(Float_t,Int_t,const TVector3&,Float_t,Int_t,Int_t);
  MyPoint(MyPoint*);
  ~MyPoint();
  
  Float_t energy() {return fEnergy;}
  Int_t quality() {return fQuality;}
  TVector3 position() {return fPosition;}
  Float_t distanceToTrack() {return fDisToTrack;}
  Int_t towerClusterId(Int_t val) {return fTowClusId[val];}
  Float_t towerClusterEnergy(Int_t val) {return fTowClusEn[val];}
  Int_t nHitsEta() {return fNeta;}
  Int_t nHitsPhi() {return fNphi;}
  Float_t widthEta() {return fSigmaEta;}
  Float_t widthPhi() {return fSigmaPhi;}
  Float_t energyEta() {return fEnEta;}
  Float_t energyPhi() {return fEnPhi;}

  void setEnergy(Float_t val) {fEnergy=val;}
  void setQuality(Int_t val) {fQuality=val;}
  void setPosition(Float_t x,Float_t y,Float_t z){
    fPosition.SetX(x);
    fPosition.SetY(y);
    fPosition.SetZ(z);
  }
  void setDistanceToTrack(Float_t val) {fDisToTrack=val;}
  void setTowerClusterId(Int_t i,Int_t val) {fTowClusId[i]=val;}
  void setTowerClusterEnergy(Int_t i,Float_t val) {fTowClusEn[i]=val;}
  void setHitsEta(Int_t val) {fNeta=val;}
  void setHitsPhi(Int_t val) {fNphi=val;}
  void setWidthEta(Float_t val) {fSigmaEta=val;}
  void setWidthPhi(Float_t val) {fSigmaPhi=val;}
  void setEnergyEta(Float_t val) {fEnEta=val;}
  void setEnergyPhi(Float_t val) {fEnPhi=val;}
 


  ClassDef(MyPoint,4)
};
#endif

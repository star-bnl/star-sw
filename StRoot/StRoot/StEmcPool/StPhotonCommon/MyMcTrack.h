#ifndef MYMCTRACK_H
#define MYMCTRACK_H

#include <TObject.h>
#include <TVector3.h>

class MyMcTrack : public TObject{
 protected:
  Float_t fEnergy;
  TVector3 fMomentum;
  TVector3 fPosition;
  Int_t fPdgId;
  Float_t fStopRadius;

 public:
  MyMcTrack();
  MyMcTrack(Float_t,const TVector3&,const TVector3&,Int_t,Float_t);
  MyMcTrack(MyMcTrack*);
  ~MyMcTrack();

  Float_t energy() {return fEnergy;}
  TVector3 momentum() {return fMomentum;}
  TVector3 position() {return fPosition;}
  Int_t id() {return fPdgId;}
  Float_t stopRadius() {return fStopRadius;}

  void setEnergy(Float_t val) {fEnergy=val;}
  void setId(Int_t val) {fPdgId=val;}
  void setMomentum(Float_t x,Float_t y,Float_t z){
    fMomentum.SetX(x);
    fMomentum.SetY(y);
    fMomentum.SetZ(z);
  }
  void setPosition(Float_t x,Float_t y,Float_t z){
    fPosition.SetX(x);
    fPosition.SetY(y);
    fPosition.SetZ(z);
  }
  void setStopRadius(Float_t val) {fStopRadius=val;}

  ClassDef(MyMcTrack,1)
};


#endif

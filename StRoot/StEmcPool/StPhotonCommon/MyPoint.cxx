#include "MyPoint.h"

ClassImp(MyPoint)

MyPoint::MyPoint()
{
  fEnergy=0;
  fQuality=0;
  fPosition=TVector3();
  fDisToTrack=0;
  fNeta=0;
  fNphi=0;
  fEnEta=0;
  fSigmaEta=0;
  fEnPhi=0;
  fSigmaPhi=0;
  for(Int_t i=0;i<4;i++){
    fTowClusId[i]=-1;
    fTowClusEn[i]=0;
  }
}
MyPoint::MyPoint(Float_t e,Int_t q,const TVector3& p,Float_t d,Int_t Ne,Int_t Np)
{
  fEnergy=e;
  fQuality=q;
  fPosition=TVector3(p);
  fDisToTrack=d;
  fNeta=Ne;
  fNphi=Np;
  fEnEta=0;
  fSigmaEta=0;
  fEnPhi=0;
  fSigmaPhi=0;
  for(Int_t i=0;i<4;i++){
    fTowClusId[i]=-1;
    fTowClusEn[i]=0;
  }
}
MyPoint::MyPoint(MyPoint *point)
{
  fEnergy=point->energy();
  fQuality=point->quality();
  fPosition=TVector3(point->position());
  fDisToTrack=point->distanceToTrack();
  fNeta=point->nHitsEta();
  fNphi=point->nHitsPhi();
  fEnEta=point->energyEta();
  fSigmaEta=point->widthEta();
  fEnPhi=point->energyPhi();
  fSigmaPhi=point->widthPhi();
  for(Int_t i=0;i<4;i++){
    fTowClusId[i]=point->towerClusterId(i);
    fTowClusEn[i]=point->towerClusterEnergy(i);
  }
}
MyPoint::~MyPoint()
{
  //
}



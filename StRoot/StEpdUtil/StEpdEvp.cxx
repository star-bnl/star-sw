#include "StEpdEvp.h"
#include "TClonesArray.h"
#include <iostream>
#include "StRoot/StEpdUtil/StEpdGeom.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuEpdHit.h"

//-----------------------------------------------------
StEpdEvp::StEpdEvp() : kRawData(kFALSE), mMax(3.0), mNmipQtB(115), mNmipQtC(160), mnMipThreshold(0.3)
{
  mEpdGeom = new StEpdGeom();
}

//-----------------------------------------------------
StEpdEvp::~StEpdEvp(){
  //  delete mEpdGeom;   in principle, it's good to delete it, but who cares, and it just causes warnings
}

//-----------------------------------------------------
double StEpdEvp::PsiEast(int order, TClonesArray* col){
  return Psi(order, -1, col);
}

//-----------------------------------------------------
double StEpdEvp::PsiWest(int order, TClonesArray* col){
  return Psi(order, +1, col);
}

//-----------------------------------------------------
double StEpdEvp::Psi(int order, int ew, TClonesArray* col){

  if (order<=0) return -999.0;

  double Qx(0.0),Qy(0.0);

  for (int ihit=0; ihit<col->GetEntries(); ihit++){
    StMuEpdHit* epdHit = (StMuEpdHit*)(col->At(ihit));
    int id = epdHit->id();
    if (id*ew<0) continue;     // wrong EastWest side
    double nMip;
    if (kRawData){nMip=(epdHit->tile()<10)?epdHit->adc()/mNmipQtC:epdHit->adc()/mNmipQtB;} else{nMip = epdHit->nMIP();}
    if (nMip<mnMipThreshold) continue;  // signal too small
    if (nMip>mMax) nMip=mMax;
    double phi = mEpdGeom->TileCenter(epdHit->id()).Phi();
    Qx += nMip*cos(((double)order)*phi);
    Qy += nMip*sin(((double)order)*phi);
  }
  double PsiAngle = atan2(Qy,Qx)/(double)order;
  return PsiAngle;
}

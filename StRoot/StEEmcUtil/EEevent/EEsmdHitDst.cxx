#include <assert.h>
#include "EEsmdHitDst.h"

ClassImp(EEsmdHitDst);

//---------------------------------
//---------------------------------
//---------------------------------
EEsmdHitDst::EEsmdHitDst(){mEnergy=-1; mStrip=-1;};

//---------------------------------
//---------------------------------
//---------------------------------
EEsmdHitDst::~EEsmdHitDst() {};


//---------------------------------
//---------------------------------
//---------------------------------
void EEsmdHitDst::get(int &strip, Float_t &ener) {
  strip=mStrip;
  ener=mEnergy;
}

//---------------------------------
//---------------------------------
//---------------------------------
void EEsmdHitDst::set(int strip,  Float_t ener) {
  //  printf("add sub/int=%d eta=%d ener=%f\n",sub,eta,ener); 
  mStrip=strip; assert(mStrip>=1 && mStrip<=288);
  mEnergy=ener;
}

//---------------------------------
//---------------------------------
//---------------------------------
void EEsmdHitDst::print(){
  printf("strip=%d  Energy=%f \n",mStrip,mEnergy);
}








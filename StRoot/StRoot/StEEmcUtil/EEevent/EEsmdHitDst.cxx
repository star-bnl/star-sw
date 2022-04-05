#include <StMessMgr.h>
#include <cassert>


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
  mStrip=strip; assert(mStrip>=1 && mStrip<=288);
  mEnergy=ener;
}

//---------------------------------
//---------------------------------
//---------------------------------
void EEsmdHitDst::print(){
  // LOG_INFO << Form("EEsmdHitDst::print(): strip=%d  Energy=%f ",mStrip,mEnergy)<<endm;
}








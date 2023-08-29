#include "StMuRHICfRawHit.h"

ClassImp(StMuRHICfRawHit)

StMuRHICfRawHit::StMuRHICfRawHit()
{
  clear();
}

StMuRHICfRawHit::~StMuRHICfRawHit()
{
}

void StMuRHICfRawHit::clear()
{	
  mBunchNumber = 0;
  mRHICfTrigger = 0;
  mRunTime[0] = 0;
  mRunTime[1] = 0;
  mRunTRGM = 0;
  mRunType = 999;

  memset(mPlateADC, 0, sizeof(mPlateADC));
  memset(mPlateADCDelay, 0, sizeof(mPlateADCDelay));
  memset(mGSOSmallADC, 0, sizeof(mGSOSmallADC));
  memset(mGSOLargeADC, 0, sizeof(mGSOLargeADC));
  memset(mTDC, 0, sizeof(mTDC));
  memset(mCAD0, 0, sizeof(mCAD0));
  memset(mGPI0, 0, sizeof(mGPI0));
  memset(mGPI1, 0, sizeof(mGPI1));
}

void StMuRHICfRawHit::setBunchNumber(UInt_t bunch){mBunchNumber = bunch;}
void StMuRHICfRawHit::setRunType(UInt_t type){mRunType = type;}
void StMuRHICfRawHit::setTriggerNumber(UInt_t trigger){mRHICfTrigger = trigger;}
void StMuRHICfRawHit::setRunTime(Int_t idx, UInt_t time){mRunTime[idx] = time;}
void StMuRHICfRawHit::setRunTRGM(UInt_t trgm){mRunTRGM = trgm;}

void StMuRHICfRawHit::setPlateADC(Int_t tower, Int_t plate, Int_t range, Int_t adc) {mPlateADC[tower][plate][range] = adc;}
void StMuRHICfRawHit::setPlateADCDelay(Int_t tower, Int_t plate, Int_t range, Int_t adc) {mPlateADCDelay[tower][plate][range] = adc;}
void StMuRHICfRawHit::setGSOBarADC(Int_t tower, Int_t layer, Int_t xy, Int_t bar, Int_t adc) 
{
  if(tower==0){mGSOSmallADC[layer][xy][bar] = adc;}
  if(tower==1){mGSOLargeADC[layer][xy][bar] = adc;}
}

void StMuRHICfRawHit::setTDC(Int_t idx, UInt_t val){mTDC[idx] = val;}
void StMuRHICfRawHit::setCAD0(Int_t idx, UInt_t val){mCAD0[idx] = val;}
void StMuRHICfRawHit::setGPI0(Int_t idx, UInt_t val){mGPI0[idx] = val;}
void StMuRHICfRawHit::setGPI1(Int_t idx, UInt_t val){mGPI1[idx] = val;}

UInt_t StMuRHICfRawHit::getBunchNumber(){return mBunchNumber;}
UInt_t StMuRHICfRawHit::getRunType(){return mRunType;}
UInt_t StMuRHICfRawHit::getTriggerNumber(){return mRHICfTrigger;}
UInt_t StMuRHICfRawHit::getRunTime(Int_t idx){return mRunTime[idx];}
UInt_t StMuRHICfRawHit::getRunTRGM(){return mRunTRGM;}

UShort_t StMuRHICfRawHit::getPlateADC(Int_t tower, Int_t plate, Int_t range) {return mPlateADC[tower][plate][range];}
UShort_t StMuRHICfRawHit::getPlateADCDelay(Int_t tower, Int_t plate, Int_t range) {return mPlateADCDelay[tower][plate][range];}
UShort_t StMuRHICfRawHit::getGSOBarADC(Int_t tower, Int_t layer, Int_t xy, Int_t bar) 
{
  if(tower==0){return mGSOSmallADC[layer][xy][bar];}
  if(tower==1){return mGSOLargeADC[layer][xy][bar];}
  return 0;
}

UInt_t StMuRHICfRawHit::getTDC(Int_t idx){return mTDC[idx];}
UInt_t StMuRHICfRawHit::getCAD0(Int_t idx){return mCAD0[idx];}
UInt_t StMuRHICfRawHit::getGPI0(Int_t idx){return mGPI0[idx];}
UInt_t StMuRHICfRawHit::getGPI1(Int_t idx){return mGPI1[idx];}

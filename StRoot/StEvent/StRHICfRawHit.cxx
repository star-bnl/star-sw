#include "StRHICfRawHit.h"

ClassImp(StRHICfRawHit)

StRHICfRawHit::StRHICfRawHit()
{
  clear();
}

StRHICfRawHit::~StRHICfRawHit()
{
}

void StRHICfRawHit::clear()
{
  memset(mPlateADC, 0, sizeof(mPlateADC));
  memset(mPlateADCDelay, 0, sizeof(mPlateADCDelay));
  memset(mGSOSmallADC, 0, sizeof(mGSOSmallADC));
  memset(mGSOLargeADC, 0, sizeof(mGSOLargeADC));
  memset(mTDC, 0, sizeof(mTDC));
  memset(mCAD0, 0, sizeof(mCAD0));
  memset(mGPI0, 0, sizeof(mGPI0));
  memset(mGPI1, 0, sizeof(mGPI1));
}

void StRHICfRawHit::setPlateADC(Int_t tower, Int_t plate, Int_t range, Int_t adc) {mPlateADC[tower][plate][range] = adc;}
void StRHICfRawHit::setPlateADCDelay(Int_t tower, Int_t plate, Int_t range, Int_t adc) {mPlateADCDelay[tower][plate][range] = adc;}
void StRHICfRawHit::setGSOBarADC(Int_t tower, Int_t layer, Int_t xy, Int_t bar, Int_t adc) 
{
  if(tower==0){mGSOSmallADC[layer][xy][bar] = adc;}
  if(tower==1){mGSOLargeADC[layer][xy][bar] = adc;}
}

void StRHICfRawHit::setTDC(Int_t idx, UInt_t val){mTDC[idx] = val;}
void StRHICfRawHit::setCAD0(Int_t idx, UInt_t val){mCAD0[idx] = val;}
void StRHICfRawHit::setGPI0(Int_t idx, UInt_t val){mGPI0[idx] = val;}
void StRHICfRawHit::setGPI1(Int_t idx, UInt_t val){mGPI1[idx] = val;}

UShort_t StRHICfRawHit::getPlateADC(Int_t tower, Int_t plate, Int_t range) {return mPlateADC[tower][plate][range];}
UShort_t StRHICfRawHit::getPlateADCDelay(Int_t tower, Int_t plate, Int_t range) {return mPlateADCDelay[tower][plate][range];}
UShort_t StRHICfRawHit::getGSOBarADC(Int_t tower, Int_t layer, Int_t xy, Int_t bar) 
{
  if(tower==0){return mGSOSmallADC[layer][xy][bar];}
  if(tower==1){return mGSOLargeADC[layer][xy][bar];}
  return 0;
}

UInt_t StRHICfRawHit::getTDC(Int_t idx)
{
  if(idx<kRHICfNtdc){return mTDC[idx];}
  return 0;
}

UInt_t StRHICfRawHit::getCAD0(Int_t idx)
{
  if(idx<kRHICfNcad0){return mCAD0[idx];}
  return 0;
}

UInt_t StRHICfRawHit::getGPI0(Int_t idx)
{
  if(idx<kRHICfNgpi0){return mGPI0[idx];}
  return 0;
}

UInt_t StRHICfRawHit::getGPI1(Int_t idx)
{
  if(idx<kRHICfNgpi1){return mGPI1[idx];}
  return 0;
}



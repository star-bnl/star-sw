#ifdef _VANILLA_ROOT_
#include <iostream>
#define LOG_INFO std::cout
#define endm std::endl
#else
#include "St_base/StMessMgr.h"
#endif

/// PicoDst headers
#include "StPicoBTowHit.h"

ClassImp(StPicoBTowHit)

//_________________
StPicoBTowHit::StPicoBTowHit(): mAdc(0), mE(-9000) {
  /* empty */
}

//_________________
/*
StPicoBTowHit::StPicoBTowHit(int id, int adc, float e) {

  if (id  < 0 || adc < 0) return;
  mId   = (id  > std::numeric_limits<unsigned short>::max()) ? std::numeric_limits<unsigned short>::max() : (UShort_t)id;
  mAdc  = (adc > std::numeric_limits<unsigned short>::max()) ? std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
  mE    = (e * 1000. > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(e * 1000.));
}
*/

//_________________
StPicoBTowHit::StPicoBTowHit(Int_t adc, Float_t e) {

  if (adc < 0) return;
  mAdc  = (adc > std::numeric_limits<unsigned short>::max()) ? std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
  mE    = (e * 1000. > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(e * 1000.));
}

//_________________
StPicoBTowHit::StPicoBTowHit(const StPicoBTowHit &hit) {
  //mId = hit.mId;
  mAdc = hit.mAdc;
  mE = hit.mE;
}

//_________________
StPicoBTowHit::~StPicoBTowHit() {
  /* empty */
}

//_________________
void StPicoBTowHit::Print(const Char_t* option) const {
  //LOG_INFO << " Id = " << id() << " Adc = " << adc() << " Energy = " << energy() << endm;
  LOG_INFO << " Adc = " << adc() << " Energy = " << energy() << endm;
}

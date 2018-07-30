/// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoBTowHit.h"

ClassImp(StPicoBTowHit)

//_________________
StPicoBTowHit::StPicoBTowHit(): TObject(), mAdc(0), mE(-9000) {
  /* empty */
}

//_________________
StPicoBTowHit::StPicoBTowHit(Int_t adc, Float_t e) : TObject() {

  if (adc < 0) return;
  mAdc  = (adc > std::numeric_limits<unsigned short>::max()) ? 
    std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
  mE = e;
}

//_________________
StPicoBTowHit::StPicoBTowHit(const StPicoBTowHit &hit) {
  mAdc = hit.mAdc;
  mE = hit.mE;
}

//_________________
StPicoBTowHit::~StPicoBTowHit() {
  /* empty */
}

//_________________
void StPicoBTowHit::Print(const Char_t* option) const {
  LOG_INFO << " Adc = " << adc() << " Energy = " << energy() << endm;
}

//_________________
Bool_t StPicoBTowHit::isBad() const {
  if( energy()==-2. && mAdc==0) {
    return kTRUE;
  }
  else {
    return kFALSE;
  }
}

//_________________
void StPicoBTowHit::setAdc(Int_t adc) {
  if(adc<0) {
    mAdc = 0;
  }
  else {
    mAdc  = (adc > std::numeric_limits<unsigned short>::max()) ?
      std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
  }
}

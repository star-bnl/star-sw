/// C++ headers
#include <limits>

/// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoEmcTrigger.h"

ClassImp(StPicoEmcTrigger)

//_________________
StPicoEmcTrigger::StPicoEmcTrigger(): TObject(), mFlag(0), mId(0), mAdc(0) {
  /* empty */
}

//_________________
StPicoEmcTrigger::StPicoEmcTrigger(Int_t flag, Int_t id, Int_t adc): StPicoEmcTrigger() {

  if (flag < 0 || id < 0 || adc < 0) return;
  mFlag = (flag > std::numeric_limits<unsigned char>::max()) ? std::numeric_limits<unsigned char>::max() : (UChar_t)flag;
  mId = (id > std::numeric_limits<unsigned short>::max()) ? std::numeric_limits<unsigned short>::max() : (UShort_t)id;
  mAdc = (adc > std::numeric_limits<unsigned short>::max()) ? std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
}

//_________________
StPicoEmcTrigger::StPicoEmcTrigger(const StPicoEmcTrigger &trigger) {
  mFlag = trigger.mFlag;
  mId = trigger.mId;
  mAdc = trigger.mAdc;
}

//_________________
StPicoEmcTrigger::~StPicoEmcTrigger() {
  /* empty */
}

//_________________
void StPicoEmcTrigger::Print(const Char_t* option) const {
  LOG_INFO << " Flag = " << mFlag << " Id = " << mId << " Adc = " << mAdc << endm;
}

//_________________
void StPicoEmcTrigger::setFlag(Int_t flag) {
  if (flag < 0) {
    flag = 0;
  }
  else {
    mFlag = (flag > std::numeric_limits<unsigned char>::max()) ?
      std::numeric_limits<unsigned char>::max() : (UChar_t)flag;
  }
}

//_________________
void StPicoEmcTrigger::setId(Int_t id) {
  if (id < 0) {
    mId = 0;
  }
  else {
    mId = (id > std::numeric_limits<unsigned short>::max()) ?
      std::numeric_limits<unsigned short>::max() : (UShort_t)id;
  }
}

//_________________
void StPicoEmcTrigger::setAdc(Int_t adc) {
  if (adc < 0) {
    mAdc = 0;
  }
  else {
    mAdc = (adc > std::numeric_limits<unsigned short>::max()) ?
      std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
  }
}

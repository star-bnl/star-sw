//
// The StPicoEmc trigger class holds EMC trigger information
//

// C++ headers
#include <limits>

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoEmcTrigger.h"

ClassImp(StPicoEmcTrigger)

//_________________
StPicoEmcTrigger::StPicoEmcTrigger(): TObject(), mFlag(0), mId(0), mAdc(0),
                                      mSmdE(0), mSmdP(0) {
  /* empty */
}

//_________________
StPicoEmcTrigger::StPicoEmcTrigger(Int_t flag, Int_t id, Int_t adc): StPicoEmcTrigger() {

  if (flag < 0 || id < 0 || adc < 0) return;
  mFlag = ( (flag > std::numeric_limits<unsigned char>::max()) ?
	    std::numeric_limits<unsigned char>::max() : (UChar_t)flag );
  mId = ( (id > std::numeric_limits<unsigned short>::max()) ?
	  std::numeric_limits<unsigned short>::max() : (UShort_t)id );
  mAdc = ( (adc > std::numeric_limits<unsigned short>::max()) ?
	   std::numeric_limits<unsigned short>::max() : (UShort_t)adc );
  mSmdE = {};
  mSmdP = {};
}

//_________________
StPicoEmcTrigger::StPicoEmcTrigger(Int_t flag, Int_t id, Int_t adc,
                                   std::vector<unsigned short> smdE,
                                   std::vector<unsigned short> smdP): StPicoEmcTrigger() {

  if (flag < 0 || id < 0 || adc < 0) return;
  mFlag = ( (flag > std::numeric_limits<unsigned char>::max()) ?
            std::numeric_limits<unsigned char>::max() : (UChar_t)flag );
  mId = ( (id > std::numeric_limits<unsigned short>::max()) ?
          std::numeric_limits<unsigned short>::max() : (UShort_t)id );
  mAdc = ( (adc > std::numeric_limits<unsigned short>::max()) ?
           std::numeric_limits<unsigned short>::max() : (UShort_t)adc );
  mSmdE = smdE;
  mSmdP = smdP;
}

//_________________
StPicoEmcTrigger::StPicoEmcTrigger(const StPicoEmcTrigger &trigger) : TObject() {
  mFlag = trigger.mFlag;
  mId = trigger.mId;
  mAdc = trigger.mAdc;
  mSmdE = trigger.mSmdE;
  mSmdP = trigger.mSmdP;

}

//_________________
StPicoEmcTrigger::~StPicoEmcTrigger() {
  /* empty */
}

//_________________
void StPicoEmcTrigger::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << "flag: " << mFlag << " id: " << mId << " ADC: " << mAdc
           << " SMDE hits num: " << mSmdE.size()
           << " SMDP hits num: " << mSmdP.size() << endm;
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

//_________________
Int_t StPicoEmcTrigger::smdEIndex(Int_t i) const {
  if ( mSmdE.empty() ) {
    return -1;  // Error: no entries in the STL vector
  }
  else {
    if ( i>=0 && i<=(Int_t)mSmdE.size() ) {
      return (Int_t)mSmdE.at( i );
    }
    else {
      return -2; // Error: out of range
    }
  }
}

//_________________
Int_t StPicoEmcTrigger::smdPIndex(Int_t i) const {
  if ( mSmdP.empty() ) {
    return -1;  // Error: no entries in the STL vector
  }
  else {
    if ( i>=0 && i<=(Int_t)mSmdP.size() ) {
      return (Int_t)mSmdP.at( i );
    }
    else {
      return -2; // Error: out of range
    }
  }
}

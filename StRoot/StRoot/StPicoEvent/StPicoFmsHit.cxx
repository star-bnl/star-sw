//
// StPicoFmsHit stores FMS hit information
//

// C++ headers
#include <iostream>
#include <limits>

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoFmsHit.h"

ClassImp(StPicoFmsHit)

//_________________
StPicoFmsHit::StPicoFmsHit() : TObject(), mChannelDetectorId(0), mAdc(0) {
  /* emtpy */
}

//_________________
StPicoFmsHit::StPicoFmsHit(Int_t detectorId, Int_t channelId, Int_t adc) : TObject() {
  mChannelDetectorId = 32*channelId + detectorId;
  mAdc = adc;
}

//_________________
StPicoFmsHit::StPicoFmsHit(const StPicoFmsHit &hit) : TObject() {
  mChannelDetectorId = hit.mChannelDetectorId;
  mAdc = hit.mAdc;
}

//_________________
StPicoFmsHit::~StPicoFmsHit() {
  /* empty */
}

//_________________
void StPicoFmsHit::Print(const Char_t *option __attribute__((unused)) ) const {
  LOG_INFO << " FMS hit -"
           << " detectorId: " << detectorId()
           << " channel: " << channel()
           << " ADC: " << adc() << endm;
}

//_________________
void StPicoFmsHit::setAdc(Float_t adc) {
  mAdc = ( (adc > std::numeric_limits<unsigned short>::max()) ?
	   std::numeric_limits<unsigned short>::max() : (UShort_t)adc );
}

//_________________
void StPicoFmsHit::setAdc(Int_t adc) {
  mAdc = ( (adc > std::numeric_limits<unsigned short>::max()) ?
	   std::numeric_limits<unsigned short>::max() : (UShort_t)adc );
}

//_________________
void StPicoFmsHit::setChannelDetectorId(Int_t channelId, Int_t detectorId) {
  mChannelDetectorId = ( ( (32*channelId + detectorId) > std::numeric_limits<unsigned short>::max() ) ?
			 std::numeric_limits<unsigned short>::max() : (UShort_t)(32*channelId + detectorId) );
}

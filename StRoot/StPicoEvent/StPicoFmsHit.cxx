#include <iostream>

#include "StPicoEvent/StPicoFmsHit.h"
#include "St_base/StMessMgr.h"

ClassImp(StPicoFmsHit)

/**
 * \author: Peifeng Liu, Stony Brook University, pliuphys@gmail.com
 */
//_________________
StPicoFmsHit::StPicoFmsHit() : TObject(), mChannelDetectorId(0), mAdc(0) {
  /* emtpy */
}

//_________________
StPicoFmsHit::StPicoFmsHit(int detectorId, int channelId, int adc) : TObject() {
  mChannelDetectorId = 32*channelId + detectorId;
  mAdc = adc;
}

//_________________
StPicoFmsHit::StPicoFmsHit(const StPicoFmsHit &hit) {
  mChannelDetectorId = hit.mChannelDetectorId;
  mAdc = hit.mAdc;
}

//_________________
StPicoFmsHit::~StPicoFmsHit() {
  /* empty */
}

//_________________
void StPicoFmsHit::Print(const Char_t *option) const {
  LOG_INFO << " FMS hit -"
           << " detectorId: " << detectorId()
           << " channel: " << channel()
           << " ADC: " << adc()
           << "\n";
}

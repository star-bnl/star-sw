//
// The StPicoBEmc SmdPhi class holds SMDPhi hits associated with an EMC trigger
//

// C++ headers
#include <limits>

// ROOT headers
#include "TMath.h"

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoBEmcSmdPHit.h"

ClassImp(StPicoBEmcSmdPHit)

//_________________
StPicoBEmcSmdPHit::StPicoBEmcSmdPHit() : TObject(),
  mId(-9999),
  mAdc(-9999),
  mEnergy(-9999) {
  /* emtpy */
}

//_________________
StPicoBEmcSmdPHit::StPicoBEmcSmdPHit(Int_t id, Int_t adc, Float_t energy) : TObject() {
  mId = (id > std::numeric_limits<short>::max()) ? -1 : (Short_t)id;
  mAdc = ( adc > std::numeric_limits<unsigned short>::max() ) ?
         std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
  mEnergy = energy;
}

//_________________
StPicoBEmcSmdPHit::StPicoBEmcSmdPHit(const StPicoBEmcSmdPHit &hit) : TObject() {
  mId = hit.mId;
  mAdc = hit.mAdc;
  mEnergy = hit.mEnergy;
}

//_________________
StPicoBEmcSmdPHit::~StPicoBEmcSmdPHit() {
  /* empty */
}

//_________________
void StPicoBEmcSmdPHit::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << "id: " << id() << " ADC: " << adc() << " energy: " << energy() << endm;
}

//
// The StPicoBEmc SmdEta class holds SMDEta hits associated with an EMC trigger
//

// C++ headers
#include <limits>

// ROOT headers
#include "TMath.h"

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoBEmcSmdEHit.h"

ClassImp(StPicoBEmcSmdEHit)

//_________________
StPicoBEmcSmdEHit::StPicoBEmcSmdEHit() : TObject(),
  mId(-9999),
  mAdc(-9999),
  mEnergy(-9999) {
  /* emtpy */
}

//_________________
StPicoBEmcSmdEHit::StPicoBEmcSmdEHit(Int_t id, Int_t adc, Float_t energy) : TObject() {
  mId = (id > std::numeric_limits<short>::max()) ? -1 : (Short_t)id;
  mAdc = ( adc > std::numeric_limits<unsigned short>::max() ) ?
          std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
  mEnergy = energy;
}

//_________________
StPicoBEmcSmdEHit::StPicoBEmcSmdEHit(const StPicoBEmcSmdEHit &hit) {
  mId = hit.mId;
  mAdc = hit.mAdc;
  mEnergy = hit.mEnergy;
}

//_________________
StPicoBEmcSmdEHit::~StPicoBEmcSmdEHit() {
  /* empty */
}

//_________________
void StPicoBEmcSmdEHit::Print(const Char_t* option) const {
  LOG_INFO << "id: " << id() << " ADC: " << adc() << " energy: " << energy() << endm;
}

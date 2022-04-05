//
// StPicoBTowHit stores BEMC tower information
//

// C++ headers
#include <limits>

// ROOT headers
#include "TMath.h"

// PicoDst headers
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

  mE = ( fabs(e * 1000.) > std::numeric_limits<short>::max() ?
	 ( (e > 0.) ? std::numeric_limits<short>::max() :
	   std::numeric_limits<short>::min() ) :
	 (Short_t)( TMath::Nint(e * 1000.) ) );
}

//_________________
StPicoBTowHit::StPicoBTowHit(const StPicoBTowHit &hit) : TObject() {
  mAdc = hit.mAdc;
  mE = hit.mE;
}

//_________________
StPicoBTowHit::~StPicoBTowHit() {
  /* empty */
}

//_________________
void StPicoBTowHit::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << " Adc = " << adc() << " Energy = " << energy() << endm;
}

//_________________
Bool_t StPicoBTowHit::isBad() const {
  if( energy()<=-2. && mAdc==0) {
    return kTRUE;
  }
  else {
    return kFALSE;
  }
}

//_________________
void StPicoBTowHit::setEnergy(Float_t energy) {
  mE = ( fabs(energy * 1000) > std::numeric_limits<short>::max() ?
	 ( (energy > 0) ? std::numeric_limits<short>::max() :
	   std::numeric_limits<short>::min() ) :
	 (Short_t)( TMath::Nint(energy * 1000) ) );
}

//_________________
void StPicoBTowHit::setAdc(Int_t adc) {
  if( adc<0 ) {
    mAdc = 0;
  }
  else {
    mAdc  = (adc > std::numeric_limits<unsigned short>::max()) ?
      std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
  }
}

//
// StPicoBTofHit holds information about BTOF hits
//

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoBTofHit.h"

ClassImp(StPicoBTofHit)

//_________________
StPicoBTofHit::StPicoBTofHit() : TObject(), mId(-1) {
  /* empty */
}

//_________________
StPicoBTofHit::StPicoBTofHit(Int_t id) : TObject() {
  if (id  < 0) return;
  mId = (id > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)id; 
}

//_________________
StPicoBTofHit::StPicoBTofHit(const StPicoBTofHit &hit) : TObject() {
  mId = hit.mId;
}

//_________________
StPicoBTofHit::~StPicoBTofHit() {
  /* empty */
}

//_________________
void StPicoBTofHit::setId(Int_t tray, Int_t module, Int_t cell) {
  Int_t id = (tray - 1) * 192 + (module - 1) * 6 + (cell - 1);
  if (id<0) {
    return;
  }
  else {
    mId = (id > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)id;
  }
}

//_________________
void StPicoBTofHit::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << " Id = " << id() << endm;
}

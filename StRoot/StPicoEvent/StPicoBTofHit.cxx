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
StPicoBTofHit::StPicoBTofHit(int id) : TObject() {
  if (id  < 0) return;
  mId = (id > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)id; 
}

//_________________
StPicoBTofHit::StPicoBTofHit(const StPicoBTofHit &hit) {
  mId = hit.mId;
}

//_________________
StPicoBTofHit::~StPicoBTofHit() {
  /* empty */
}

//_________________
void StPicoBTofHit::Print(const Char_t* option) const {
  LOG_INFO << " Id = " << id() << endm;
}

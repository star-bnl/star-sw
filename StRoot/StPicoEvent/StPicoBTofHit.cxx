#include <limits>

#include "St_base/StMessMgr.h"
#include "StPicoEvent/StPicoBTofHit.h"


StPicoBTofHit::StPicoBTofHit() : mId(0) {}
StPicoBTofHit::StPicoBTofHit(int id): StPicoBTofHit()
{
  if (id  < 0) return;
  mId   = (id > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)id;
}

//----------------------------------------------------------------------------------
StPicoBTofHit::~StPicoBTofHit()
{
  /* noop */
}

//----------------------------------------------------------------------------------
void StPicoBTofHit::Print(const Char_t* option) const
{
  LOG_INFO << " Id = " << id() << endm;
}

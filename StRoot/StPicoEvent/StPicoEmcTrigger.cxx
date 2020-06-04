#include <limits>
#include "St_base/StMessMgr.h"
#include "StPicoEvent/StPicoEmcTrigger.h"

//----------------------------------------------------------------------------------
StPicoEmcTrigger::StPicoEmcTrigger(): mFlag(0), mId(0), mAdc(0)
{
}

//----------------------------------------------------------------------------------
StPicoEmcTrigger::StPicoEmcTrigger(int flag, int id, int adc): StPicoEmcTrigger()
{
  if (flag < 0 || id < 0 || adc < 0) return;

  mFlag = (flag > std::numeric_limits<unsigned char>::max())  ? std::numeric_limits<unsigned char>::max()  : (UChar_t)flag;
  mId   = (id  > std::numeric_limits<unsigned short>::max()) ? std::numeric_limits<unsigned short>::max() : (UShort_t)id;
  mAdc  = (adc > std::numeric_limits<unsigned short>::max()) ? std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
}

//----------------------------------------------------------------------------------
StPicoEmcTrigger::~StPicoEmcTrigger()
{
  /* noop */
}
//----------------------------------------------------------------------------------
void StPicoEmcTrigger::Print(const Char_t* option) const
{
  LOG_INFO << " Flag = " << mFlag << " Id = " << mId << " Adc = " << mAdc << endm;
}

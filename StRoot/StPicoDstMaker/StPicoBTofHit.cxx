#include "StPicoBTofHit.h"
#include "StPicoConstants.h"
#include "StPicoDstMaker.h"

ClassImp(StPicoBTofHit)

//----------------------------------------------------------------------------------
StPicoBTofHit::StPicoBTofHit()
{
  Clear();
}

//----------------------------------------------------------------------------------
StPicoBTofHit::StPicoBTofHit(int id)
{
  Clear();

  if(id  <0) mId   = 0;

  mId   = (id  >Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)id;
}

//----------------------------------------------------------------------------------
StPicoBTofHit::~StPicoBTofHit()
{ /* noop */ }

//----------------------------------------------------------------------------------
void StPicoBTofHit::Clear(const Option_t* opt)
{
  mId = 0;
}
//----------------------------------------------------------------------------------
void StPicoBTofHit::Print(const Char_t *option) const {
  LOG_INFO << " Id = " << id() << endm;
}

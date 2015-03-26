#include "StPicoBTOWHit.h"
#include "StPicoConstants.h"
#include "StPicoDstMaker.h"

ClassImp(StPicoBTOWHit)

//----------------------------------------------------------------------------------
StPicoBTOWHit::StPicoBTOWHit()
{
  Clear();
}

//----------------------------------------------------------------------------------
StPicoBTOWHit::StPicoBTOWHit(int id, int adc, float e)
{
  Clear();

  if(id  <0) mId   = 0;
  if(adc <0) mAdc  = 0;

  mId   = (id  >Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)id;
  mAdc  = (adc >Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)adc;
  mE    = (e*1000. > Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(e*1000.));
}

//----------------------------------------------------------------------------------
StPicoBTOWHit::~StPicoBTOWHit()
{ /* noop */ }

//----------------------------------------------------------------------------------
void StPicoBTOWHit::Clear(const Option_t* opt)
{
  mId = 0;
  mAdc = 0;
  mE = 0;
}
//----------------------------------------------------------------------------------
void StPicoBTOWHit::Print(const Char_t *option) const {
  LOG_INFO << " Id = " << id() << " Adc = " << adc() << " Energy = " << energy() << endm;
}

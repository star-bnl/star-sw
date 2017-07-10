#include "St_base/StMessMgr.h"
#include "StPicoEvent/StPicoBbcTile.h"

StPicoBbcTile::StPicoBbcTile() : mId(0), mQTdata(0)
{
  /* no-op */
}


StPicoBbcTile::StPicoBbcTile(int ID, int ADC, int TAC, int TDC, bool hasTAC) :
  mId(ID),
  mQTdata( (ADC & 0x0FFF) | (TAC & 0x0FFF) << 12 | (TDC & 0x001F) << 24 | hasTAC << 29 )
{
}


void StPicoBbcTile::Print(const Char_t *option) const
{

  /*  --- this below does not compile so I am skipping it.
      Dmitri, could you put it in?  I guess I am missing
      an include.  I must be missing an include file somewhere
      since even 'endm' is unrecognized when I say 'cons'
  LOG_INFO << " Id = " << mId
     << " ADC = " << this->ADC()
     << " TAC = " << this->TAC()
     << " TDC = " << this->TDC()
     << " - This tile "
     << hasTAC()?"has TAC":"does not have TAC"
     << endm;
  */
}

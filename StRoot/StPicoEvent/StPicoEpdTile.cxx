#include "St_base/StMessMgr.h"
#include "StPicoEvent/StPicoEpdTile.h"


StPicoEpdTile::StPicoEpdTile() : mId(0), mQTdata(0)
{
  /* no-op */
}


StPicoEpdTile::StPicoEpdTile(int PP, int TT, DetectorSide EW, int ADC, int TAC, int TDC, bool hasTAC) :
  mId( (100*PP + TT)*EW ),
  mQTdata( (ADC & 0x0FFF) | (TAC & 0x0FFF) << 12 | (TDC & 0x001F) << 24 | hasTAC << 29 )
{
}


void StPicoEpdTile::Print(const Char_t *option) const
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

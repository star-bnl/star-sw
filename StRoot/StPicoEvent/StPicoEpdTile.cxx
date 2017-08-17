#include <iostream>

#include "StPicoEvent/StPicoEpdTile.h"
#include "St_base/StMessMgr.h"


StPicoEpdTile::StPicoEpdTile() : mId(0), mQTdata(0)
{
  /* no-op */
}


StPicoEpdTile::StPicoEpdTile(int positionId, int tileId, DetectorSide EW, int ADC, int TAC, int TDC, bool hasTAC, bool goodStatus) :
  mId( (100*positionId + tileId)*EW ),
  mQTdata( (ADC & 0x0FFF) | (TAC & 0x0FFF) << 12 | (TDC & 0x001F) << 24 | hasTAC << 29 | goodStatus << 30 )
{
  /* no-op */
}


void StPicoEpdTile::Print(const Char_t *option) const
{
  LOG_INFO << " EPD tile -"
	   << " position: " << position()
	   << " tile: " << tile()
	   << " ADC: " << adc()
	   << " TAC: " << tac()
	   << " TDC: " << tdc()
	   << " - This tile " << (hasTac() ? "has TAC" : "does not have TAC")
	   << " - Status is " << (goodStatus() ? "good" : "bad")
	   << endm;
}

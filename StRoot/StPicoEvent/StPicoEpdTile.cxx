#include <iostream>

#include "StPicoEvent/StPicoEpdTile.h"


StPicoEpdTile::StPicoEpdTile() : mId(0), mQTdata(0)
{
  /* no-op */
}


StPicoEpdTile::StPicoEpdTile(int positionId, int tileId, DetectorSide EW, int ADC, int TAC, int TDC, bool hasTAC) :
  mId( (100*positionId + tileId)*EW ),
  mQTdata( (ADC & 0x0FFF) | (TAC & 0x0FFF) << 12 | (TDC & 0x001F) << 24 | hasTAC << 29 )
{
}


void StPicoEpdTile::Print(const Char_t *option) const
{
  std::cout << " EPD tile -"
            << " position: " << position()
            << " tile: " << tile()
            << " ADC: " << adc()
            << " TAC: " << tac()
            << " TDC: " << tdc()
            << " - This tile " << (hasTac() ? "has TAC" : "does not have TAC")
            << std::endl;
}

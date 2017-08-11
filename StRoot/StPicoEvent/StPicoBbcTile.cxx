#include <iostream>

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
  std::cout << " BBC tile - PMT#: " << mId
            << " ADC: " << adc()
            << " TAC: " << tac()
            << " TDC: " << tdc()
            << " - This tile " << (hasTac() ? "has TAC" : "does not have TAC")
            << std::endl;
}

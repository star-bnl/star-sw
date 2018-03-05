#include <iostream>

#include "StPicoEvent/StPicoEpdTile.h"
#include "St_base/StMessMgr.h"

ClassImp(StPicoEpdTile)

//_________________
StPicoEpdTile::StPicoEpdTile() : StPicoEpdTile(0, 0, DetectorSide::Undefined, 0, 0, 0, false) {
  /* empty */
}

//_________________
StPicoEpdTile::StPicoEpdTile(int positionId, int tileId, DetectorSide EW,
  int ADC, int TAC, int TDC, bool hasTAC, bool statusIsGood) : TObject(), mId( (100*positionId + tileId)*EW ),
  mQTdata( (ADC & 0x0FFF) | (TAC & 0x0FFF) << 12 | (TDC & 0x001F) << 24 | hasTAC << 29 | statusIsGood << 30 ) {
  /* empty */
}

//_________________
StPicoEpdTile::StPicoEpdTile(const StPicoEpdTile &tile) {
  mId = tile.mId;
  mQTdata = tile.mQTdata;
}

//_________________
StPicoEpdTile::~StPicoEpdTile() {
  /* empty */
}

//_________________
void StPicoEpdTile::Print(const Char_t *option) const {
  LOG_INFO << " EPD tile -"
           << " position: " << position()
           << " tile: " << tile()
           << " ADC: " << adc()
           << " TAC: " << tac()
           << " TDC: " << tdc()
           << " - This tile " << (hasTac() ? "has TAC" : "does not have TAC")
           << " - Status is " << (isGood() ? "good" : "bad")
           << endm;
}

#include "TClonesArray.h"

#include "StEvent/StTriggerData.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

#include "StPicoDstMaker/StPicoBbcFiller.h"
#include "StPicoDstMaker/StPicoDst.h"
#include "StPicoEvent/StPicoBbcTile.h"
#include "StPicoEvent/StPicoCommon.h"

using namespace StarPicoDst;

//_________________
StBeamDirection eastwestdir(DetectorSide ew) {
  return ew == DetectorSide::East ? StBeamDirection::east : StBeamDirection::west;
}

//_________________
StPicoBbcFiller::StPicoBbcFiller(StPicoDst& picoDst, int year) : mPicoDst(picoDst) { 
  /* no-op */
}

//_________________
void StPicoBbcFiller::fill(const StMuDst& muDst) {
  TClonesArray *mTileCollection = mPicoDst.picoArray(StPicoArrays::BbcTile);

  StMuEvent *event = muDst.event();
  StTriggerData *trg = const_cast<StTriggerData *>(event->triggerData());

  int nTiles = 0;

  // Loop over BBC tiles
  for (DetectorSide ew : detectorSides) {
    for (int pmtId = 1; pmtId <= 24; pmtId++) {
      int ADC = trg->bbcADC(eastwestdir(ew), pmtId, 0);
      int TAC = trg->bbcTDC(eastwestdir(ew), pmtId, 0); // yes I know the method says "TDC" but it's the TAC
      int TDC = trg->bbcTDC5bit(eastwestdir(ew), pmtId);
      int ID  = ew * pmtId;
      bool hasTAC = kTRUE;

      new((*mTileCollection)[nTiles++]) StPicoBbcTile(ID, ADC, TAC, TDC, hasTAC);
    } //for (int pmtId = 1; pmtId <= 24; pmtId++)
  } //for (DetectorSide ew : detectorSides)
}

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


StBeamDirection eastwestdir(DetectorSide ew)
{
  return ew == DetectorSide::East ? StBeamDirection::east : StBeamDirection::west;
}


StPicoBbcFiller::StPicoBbcFiller(StPicoDst& picoDst, int year) :
  mPicoDst(picoDst)
{
}


void StPicoBbcFiller::fill(const StMuDst& muDst)
{
  TClonesArray *mTileCollection = mPicoDst.picoArray(StPicoArrays::BbcTile);

  StMuEvent *event = muDst.event();
  StTriggerData *trg = const_cast<StTriggerData *>(event->triggerData());

  int nTiles = 0;

  // Loop over BBC tiles
  for (DetectorSide ew : detectorSides)
  {
    for (Int_t pmt = 0; pmt < 24; pmt++)
    {
      int ADC = trg->bbcADC(eastwestdir(ew), pmt + 1, 0);
      int TAC = trg->bbcTDC(eastwestdir(ew), pmt + 1, 0); // yes I know the method says "TDC" but it's the TAC
      int TDC = trg->bbcTDC5bit(eastwestdir(ew), pmt + 1);
      int ID  = ew * (pmt + 1);
      bool hasTAC = kTRUE;

      new((*mTileCollection)[nTiles++]) StPicoBbcTile(ID, ADC, TAC, TDC, hasTAC);
    }
  }

}

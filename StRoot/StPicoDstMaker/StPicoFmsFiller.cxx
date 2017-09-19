#include "TClonesArray.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuFmsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFmsHit.h"

#include "StPicoDstMaker/StPicoFmsFiller.h"
#include "StPicoDstMaker/StPicoDst.h"
#include "StPicoEvent/StPicoFmsHit.h"



StPicoFmsFiller::StPicoFmsFiller(StPicoDst& picoDst) :
  mPicoDst(picoDst)
{
}


/**
 * Fills the FmsHit TObjArray in StPicoDst with StPicoFmsHit objects by
 * copying the corresponding data from StMuFmsCollection in StMuDst.
 */
void StPicoFmsFiller::fill(const StMuDst& muDst, const StFmsDbMaker* fmsDbMaker)
{
  StMuFmsCollection*  muFmsCollection = muDst.muFmsCollection();
  TClonesArray* muFmsHits = muFmsCollection ? muFmsCollection->getHitArray() : nullptr;

  if (!muFmsHits)
  {
    std::cout << "StMuFmsHits not found in StMuDst\n";
    return;
  }

  //
  TClonesArray *hitCollection = mPicoDst.picoArray(StPicoArrays::FmsHit);

  for (const TObject* obj : *muFmsHits)
  {
    const StMuFmsHit& muFmsHit = static_cast<const StMuFmsHit&>(*obj);

    int counter = hitCollection->GetEntries();

    new((*hitCollection)[counter]) StPicoFmsHit(muFmsHit.detectorId(),
                                                muFmsHit.channel(),
                                                muFmsHit.adc());
  }
}

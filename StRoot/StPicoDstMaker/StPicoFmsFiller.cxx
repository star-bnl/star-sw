//
// StPicoFmsFiller is a helper class that stores FMS information to the PicoDst
//

// ROOT headers
#include "TClonesArray.h"

// MuDst headers
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuFmsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFmsHit.h"

// PicoDst headers
#include "StPicoDstMaker/StPicoFmsFiller.h"
#include "StPicoEvent/StPicoDst.h"
#include "StPicoEvent/StPicoFmsHit.h"

//_________________
StPicoFmsFiller::StPicoFmsFiller(StPicoDst& picoDst) : mPicoDst(picoDst) {
  /* empty */
}

/**
 * Fills the FmsHit TObjArray in StPicoDst with StPicoFmsHit objects by
 * copying the corresponding data from StMuFmsCollection in StMuDst.
 */
//_________________
void StPicoFmsFiller::fill(const StMuDst& muDst, const StFmsDbMaker* fmsDbMaker) {

  StMuFmsCollection*  muFmsCollection = muDst.muFmsCollection();
  TClonesArray* muFmsHits = muFmsCollection ? muFmsCollection->getHitArray() : nullptr;

  if (!muFmsHits) {
    //    std::cout << "StMuFmsHits not found in StMuDst\n";
    return;
  }

  TClonesArray *hitCollection = mPicoDst.picoArray(StPicoArrays::FmsHit);

  for (const TObject* obj : *muFmsHits) {

    const StMuFmsHit& muFmsHit = static_cast<const StMuFmsHit&>(*obj);
    int counter = hitCollection->GetEntries();
    new((*hitCollection)[counter]) StPicoFmsHit(muFmsHit.detectorId(),
                                                muFmsHit.channel(),
                                                muFmsHit.adc());
  }
}

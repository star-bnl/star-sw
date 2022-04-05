#include "StMuEpdHit.h"


//
// \class StMuEpdHit
// \author Mike Lisa
// \date 14 Jan 2018
// \brief Holds signals for tiles in STAR Event Plane Detector

/*************************************************
 * Hit Class for the STAR Event Plane Detector.
 * There is one "hit" for each (good) tile in the detector
 * 
 * Total size of this object is 10 bytes
 * This object contains only
 *  1) the id of the tile, compacted into a Short_t
 *  2) the QT information (ADC, TDC, TAC, hasTac flag, status flag)
 *     compacted into 32 bits (Int_t
 *  3) the gain-corrected energy loss, in units of the
 *     most probable value (MPV) of a single MIP in the tile,
 *     according to a Landau distribution.  Stored as Float_t
 *
 * The StMuEpdHit in the StMuDST is basically the same as
 *  the StEpdHit object in StEvent
 *
 * - Mike Lisa Jan 2018
 ************************************************/

#include "StEvent/StEpdHit.h"
#include "StMuEpdHit.h"

ClassImp(StMuEpdHit)

StMuEpdHit::StMuEpdHit() : StMuEpdHit(0, 0, 0, 0, 0, 0, false, 0.0, false, 0){
  /* no-op */
}

StMuEpdHit::StMuEpdHit(Int_t position, Int_t tile,
		       Short_t EW, Int_t ADC, Int_t TAC,
		       Int_t TDC, bool hasTAC, Float_t nMIP,
		       bool statusIsGood, Int_t truthId) :
  mId( (100*position + tile)*EW ),
  mQTdata( (ADC & 0x0FFF) | (TAC & 0x0FFF) << 12 | (TDC & 0x001F) << 24 | hasTAC << 29 | statusIsGood << 30 ),
  mnMIP(nMIP),
  mTruthId(truthId)
{
  /* no-op */
}

StMuEpdHit::StMuEpdHit(StEpdHit* epdHit) :
  mId(epdHit->id()),  mQTdata(epdHit->qtData()), mnMIP(epdHit->nMIP()), mTruthId(epdHit->idTruth())
{
  /* no-op */
}


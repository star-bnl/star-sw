#include "StPicoEpdHit.h"
//
// \class StPicoEpdHit
// \author Mike Lisa
// \date 06 March 2018
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
 * The StPicoEpdHit in the StMuDST is basically the same as
 *  the StMuEpdHit object in the muDst and
 *  the StEpdHit object in StEvent
 * Except 
 *   1) it does not inherit from StObject, so can be used in "vanilla root"
 *   2) there is no "Truth ID" which is used for simulations.
 * 
 * - Mike Lisa March 2018
 ************************************************/

#include "StPicoEpdHit.h"

ClassImp(StPicoEpdHit)

//_________________
StPicoEpdHit::StPicoEpdHit() : StPicoEpdHit(0, 0, 0, 0, 0, 0, false, 0.0, false) {
  /* no-op */
}

//_________________
StPicoEpdHit::StPicoEpdHit(int position, int tile,
		       int EW, int ADC, int TAC,
		       int TDC, bool hasTAC, float nMIP,
		       bool statusIsGood) :
  mId( (100*position + tile)*EW ),
  mQTdata( (ADC & 0x0FFF) | (TAC & 0x0FFF) << 12 | (TDC & 0x001F) << 24 | hasTAC << 29 | statusIsGood << 30 ),
  mnMIP(nMIP) {
  /* no-op */
}

//_________________
StPicoEpdHit::StPicoEpdHit(short id, int QTdata, float nMIP) :
  mId(id), mQTdata(QTdata), mnMIP(nMIP) {
  /* no-op */
}

//_________________
StPicoEpdHit::StPicoEpdHit(const StPicoEpdHit &hit) {
  mId = hit.mId;
  mQTdata = hit.mQTdata;
  mnMIP = hit.mnMIP;
}

//_________________
StPicoEpdHit::~StPicoEpdHit() {
  /* no-op */
}

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 4 Jan 2011
//
// For descriptions of FMS hits in GEANT:
//
//   http://www.star.bnl.gov/protected/spin/akio/fpd_geant/
//   http://drupal.star.bnl.gov/STAR/event/2011/01/05/software-and-computing-phone-meeting/fms-simulation-open-request-and-readiness
//
// For descriptions of FMS geometry and mapping in the FMS database:
//
//   http://drupal.star.bnl.gov/STAR/book/export/html/15527
//
//     $STAR/StDb/idl/fmsChannelGeometry.idl
//     $STAR/StDb/idl/fmsDetectorPosition.idl
//     $STAR/StDb/idl/fmsDetectorPosition.idl
//     $STAR/StDb/idl/fmsPatchPannelMap.idl
//     $STAR/StDb/idl/fmsQTMap.idl
//

#include "StMcEvent/StMcEventTypes.hh"
#include "StEventTypes.h"
#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StFmsFastMaker.h"

ClassImp(StFmsFastMaker);

int StFmsFastMaker::getDetectorId(const StMcCalorimeterHit* mcHit) const
{
  /* --- GEANT hits in StMcCalorimeterHit --- */

  /* module : 1=FPD, 2=FMS */
  /* sub    : 1=FMS-North-Large, 2=FMS-South-Large, 3=FMS-North-Small, 4=FMS-South-Small */
  /* eta    : 1-578=Channel */

  /* --- FMS hits in StFmsHit --- */

  /* Detector Name detectorId ew ns type nX nY */
  /* FPD-North 0 0 0 0 7 7 */
  /* FPD-South 1 0 1 0 7 7 */
  /* FPD-North-Pres 2 0 0 1 7 1 */
  /* FPD-South-Pres 3 0 1 1 7 1 */
  /* FPD-North-SMDV 4 0 0 2 48 1 */
  /* FPD-South-SMDV 5 0 1 2 48 1 */
  /* FPD-North-SMDH 6 0 0 3 1 48 */
  /* FPD-South-SMDH 7 0 1 3 1 48 */
  /* FMS-North-Large 8 1 0 4 17 34 */
  /* FMS-South-Large 9 1 1 4 17 34 */
  /* FMS-North-Small 10 1 0 4 12 24 */
  /* FMS-South-Small 11 1 1 4 12 24 */
  /* FHC-North 12 1 0 5 9 12 */
  /* FHC-South 13 1 1 5 9 12 */

  switch (mcHit->module()) {
  case 1: // fpd
    switch (mcHit->sub()) {
    case 1: return 0; // north
    case 2: return 1; // south
    case 5: return 2; // preshower north
    case 6: return 3; // preshower south
    }
    break;
  case 2: // fms
    switch (mcHit->sub()) {
    case 1: return 8; // north large cells
    case 2: return 9; // south large cells
    case 3: return 10; // north small cells
    case 4: return 11; // south small cells
    }
    break;
  }
  return -1;
}

StFmsHit* StFmsFastMaker::makeFmsHit(const StMcCalorimeterHit* mcHit) const
{
  const int MAX_ADC = 4095;

  int detectorId = getDetectorId(mcHit);

  assert(detectorId >= 0);

  int channel = mcHit->eta(); 
  int qtCrate, qtSlot, qtChannel;
  gStFmsDbMaker->getMap(detectorId,channel,&qtCrate,&qtSlot,&qtChannel);
  float gain = gStFmsDbMaker->getGain(detectorId,channel);
  float gainCorrection = gStFmsDbMaker->getGainCorrection(detectorId,channel);
  int adc = int(mcHit->dE()/(gain*gainCorrection)+0.5);
  if (adc > MAX_ADC) adc = MAX_ADC;
  int tdc = 0;
  float energy = adc*gain*gainCorrection;
  return new StFmsHit(detectorId,channel,qtCrate,qtSlot,qtChannel,adc,tdc,energy);
}

int StFmsFastMaker::Make()
{
  // Get StMcEvent
  StMcEvent* mcEvent = (StMcEvent*)GetDataSet("StMcEvent");
  if (!mcEvent) {
    LOG_ERROR << "No StMcEvent" << endm;
    return kStErr;
  }

  // Get StEvent
  StEvent* event = (StEvent*)GetDataSet("StEvent");
  if (!event) {
    event = new StEvent;
    AddData(event);
  }

  // Get FMS collection
  if (!event->fmsCollection()) event->setFmsCollection(new StFmsCollection);

  // Digitize FMS hits
  assert(gStFmsDbMaker);
  fillStEvent(mcEvent,event);

  // Print
  if (Debug()) {
    const StSPtrVecFmsHit& hits = event->fmsCollection()->hits();
    LOG_INFO << "Number of FPD/FMS hits : " << hits.size() << endm;
    for (size_t i = 0; i < hits.size(); ++i) hits[i]->print();
  }

  return kStOk;
}

void StFmsFastMaker::fillStEvent(StMcEvent* mcEvent, StEvent* event)
{
  // module: 1=FPD, 2=FMS
  for (size_t m = 1; m <= mcEvent->fpdHitCollection()->numberOfModules(); ++m) {
    const StSPtrVecMcCalorimeterHit& mchits = mcEvent->fpdHitCollection()->module(m)->detectorHits();
    for (size_t i = 0; i < mchits.size(); ++i)
      event->fmsCollection()->hits().push_back(makeFmsHit(mchits[i]));
  }
}

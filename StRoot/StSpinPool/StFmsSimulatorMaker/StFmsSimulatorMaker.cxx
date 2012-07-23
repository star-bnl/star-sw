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

#include "tables/St_g2t_emc_hit_Table.h"
#include "StEventTypes.h"
#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StFmsSimulatorMaker.h"

ClassImp(StFmsSimulatorMaker);

void StFmsSimulatorMaker::decodeVolumeId(int volumeId, int& ew, int& nstb, int& channel) const
{
  //
  // volumeId = ew*10000+nstb*1000+channel
  //
  // ew      : 1=FPD, 2=FMS
  // nstb    : 1=FMS-North-Large, 2=FMS-South-Large, 3=FMS-North-Small, 4=FMS-South-Small
  // channel : 1-578
  //
  ew      = volumeId / 10000;
  nstb    = volumeId / 1000 % 10;
  channel = volumeId % 1000;
}

int StFmsSimulatorMaker::getDetectorId(int ew, int nstb) const
{
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

  switch (ew) {
  case 1: // fpd
    switch (nstb) {
    case 1: return 0; // north
    case 2: return 1; // south
    case 5: return 2; // preshower north
    case 6: return 3; // preshower south
    }
    break;
  case 2: // fms
    switch (nstb) {
    case 1: return 8; // north large cells
    case 2: return 9; // south large cells
    case 3: return 10; // north small cells
    case 4: return 11; // south small cells
    }
    break;
  }
  return -1;
}

StFmsHit* StFmsSimulatorMaker::makeFmsHit(const g2t_emc_hit_st& hit) const
{
  const int MAX_ADC = 4095;

  int ew, nstb, channel;
  decodeVolumeId(hit.volume_id,ew,nstb,channel);
  int detectorId = getDetectorId(ew,nstb);
  int qtCrate, qtSlot, qtChannel;
  gStFmsDbMaker->getMap(detectorId,channel,&qtCrate,&qtSlot,&qtChannel);
  float gain = gStFmsDbMaker->getGain(detectorId,channel);
  float gainCorrection = gStFmsDbMaker->getGainCorrection(detectorId,channel);
  int adc = int(hit.de/(gain*gainCorrection)+0.5);
  if (adc > MAX_ADC) adc = MAX_ADC;
  int tdc = 0;
  float energy = adc*gain*gainCorrection;
  return new StFmsHit(detectorId,channel,qtCrate,qtSlot,qtChannel,adc,tdc,energy);
}

int StFmsSimulatorMaker::Make()
{
  // Get GEANT FPD/FMS hits
  TDataSet* geant = GetDataSet("geant");
  if (!geant) {
    LOG_ERROR << "No geant" << endm;
    return kStErr;
  }

  TDataSetIter geantIter(geant);
  const St_g2t_emc_hit* g2t_fpd_hit = (const St_g2t_emc_hit*)geantIter("g2t_fpd_hit");
  if (!g2t_fpd_hit) {
    LOG_ERROR << "No g2t_fpd_hit" << endm;
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

  // Digitize GEANT FPD/FMS hits
  if (!gStFmsDbMaker) {
    LOG_ERROR << "No gStFmsDbMaker. StFmsDbMaker library not loaded?" << endm;
    return kStErr;
  }

  fillStEvent(g2t_fpd_hit,event);
  printStEventSummary(event);

  return kStOk;
}

void StFmsSimulatorMaker::fillStEvent(const St_g2t_emc_hit* g2t_fpd_hit, StEvent* event)
{
  const g2t_emc_hit_st* hits = g2t_fpd_hit->GetTable();
  for (int i = 0; i < g2t_fpd_hit->GetNRows(); ++i) {
    StFmsHit* hit = makeFmsHit(hits[i]);
    if (hit) event->fmsCollection()->addHit(hit);
  }
}

void  StFmsSimulatorMaker::printStEventSummary(const StEvent* event)
{
 // Summarize number of hits and energy per detector
  const int NDETECTORS = 14;
  const char* detectorNames[NDETECTORS] = { "FPD-North ", "FPD-South", "FPD-North-Pres", "FPD-South-Pres", "FPD-North-SMDV", "FPD-South-SMDV", "FPD-North-SMDH", "FPD-South-SMDH", "FMS-North-Large", "FMS-South-Large", "FMS-North-Small", "FMS-South-Small", "FHC-North", "FHC-South" };
  int nhits[NDETECTORS];
  float detectorEnergy[NDETECTORS];

  // Zero
  fill(nhits,nhits+NDETECTORS,0);
  fill(detectorEnergy,detectorEnergy+NDETECTORS,0.);

  // Sum number of hits and energies
  const StSPtrVecFmsHit& hits = event->fmsCollection()->hits();
  for (size_t i = 0; i < hits.size(); ++i) {
    const StFmsHit* hit = hits[i];
    if (Debug()) hit->print();
    ++nhits[hit->detectorId()];
    detectorEnergy[hit->detectorId()] += hit->energy();
  }

  // Print detectors summary
  LOG_INFO << "ID\tNAME\t\tNHITS\tENERGY" << endm;
  for (int detectorId = 0; detectorId < NDETECTORS; ++detectorId) {
    LOG_INFO << detectorId << '\t' << detectorNames[detectorId] << '\t' << nhits[detectorId] << '\t' << detectorEnergy[detectorId] << endm;
  }
}

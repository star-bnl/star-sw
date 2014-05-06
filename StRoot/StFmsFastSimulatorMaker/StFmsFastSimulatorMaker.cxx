// $Id: StFmsFastSimulatorMaker.cxx,v 1.2 2014/05/06 16:05:56 jeromel Exp $
//
// $Log: StFmsFastSimulatorMaker.cxx,v $
// Revision 1.2  2014/05/06 16:05:56  jeromel
// Adjust for include - there is no need to specify the path
//
// Revision 1.1  2014/05/06 16:02:04  jeromel
// First version of StFmsFastSimulatorMaker deliverred upon review
//
//
/**
 \file StFmsFastSimulatorMaker.cxx
       Implementation of StFmsFastSimulatorMaker, the FMS fast simulator
 \author Pibero Djawotho <pibero@tamu.edu>
 \date 4 Jan 2011
 */
#include "StFmsFastSimulatorMaker.h"

#include <algorithm>  // For std::fill(), std::max(), std::min()

#include "St_base/StMessMgr.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFmsCollection.h"
#include "StEvent/StFmsHit.h"
#include "StFmsDbMaker/StFmsDbMaker.h"
#include "tables/St_g2t_emc_hit_Table.h"

/* Constructor. */
StFmsFastSimulatorMaker::StFmsFastSimulatorMaker(const Char_t* name) : StMaker(name) { }

/* Process one event. */
Int_t StFmsFastSimulatorMaker::Make() {
  // Check for the FMS database maker, bail out if it can't be located.
  if (!GetMaker("fmsDb")) {
    LOG_ERROR << "No StFmsDbMaker. StFmsDbMaker library not loaded?" << endm;
    return kStErr;
  }  // if
  // Get the existing StEvent, or add one if it doesn't exist.
  StEvent* event = static_cast<StEvent*>(GetDataSet("StEvent"));
  if (!event) {
    event = new StEvent;
    AddData(event);
  }  // if
  // Add an FMS collection to the event if one does not already exist.
  if (!event->fmsCollection()) {
    event->setFmsCollection(new StFmsCollection);
  }  // if
  // Digitize GEANT FPD/FMS hits
  fillStEvent(event);
  printStEventSummary(event);
  return kStOk;
}

/* Fill an event with StFmsHits. */
void StFmsFastSimulatorMaker::fillStEvent(StEvent* event) {
  // Read the g2t table
  St_g2t_emc_hit* hitTable =
      static_cast<St_g2t_emc_hit*>(GetDataSet("g2t_fpd_hit"));
  if (!hitTable) {
    LOG_INFO << "g2t_fpd_hit table is empty" << endm;
    return;  // Nothing to do
  }  // if
  // Loop over FPD hits
  const Int_t nHits = hitTable->GetNRows();
  // Point to the first hit in the table
  const g2t_emc_hit_st* hitPointer = hitTable->GetTable();
  // Loop through hits, incrementing pointer each time
  for (Int_t i(0); i < nHits; ++i, ++hitPointer) {
    if (hitPointer) {
      event->fmsCollection()->addHit(makeFmsHit(*hitPointer));
    }  // if
  }  // for
}

/* Create and set all values in an StFmsHit from a g2t_emc_hit_st. */
StFmsHit* StFmsFastSimulatorMaker::makeFmsHit(const g2t_emc_hit_st& hit) {
  // Existence of StFmsDbMaker was already checked in Make()
  // so we don't confirm the pointer again here.
  StFmsDbMaker* dbMaker = static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));
  // Decode detector information from hit:
  const Int_t channel = hit.volume_id % 1000;
  const Int_t detectorId = getDetectorId(hit);
  // Get gain and correction from the database.
  Float_t gain = dbMaker->getGain(detectorId, channel);
  Float_t gainCorrection = dbMaker->getGainCorrection(detectorId, channel);
  // Check for ADC values outside the allowed range and cap.
  Float_t energy = hit.de;
  Int_t adc = static_cast<Int_t>(energy / (gain * gainCorrection) + 0.5);
  adc = std::max(adc, 0);  // Prevent negative ADC
  adc = std::min(adc, 4095);  // Cap maximum ADC = 4,095
  // Recalculate energy accounting for ADC range
  energy = adc * gain * gainCorrection;
  // Determine the QT crate, slot and channel for this detector and channel.
  Int_t qtCrate, qtSlot, qtChannel;
  dbMaker->getMap(detectorId, channel, &qtCrate, &qtSlot, &qtChannel);
  // Create the hit.
  Int_t tdc = 0;
  return new StFmsHit(detectorId, channel, qtCrate, qtSlot, qtChannel, adc,
                      tdc, energy);
}

/*
 Returns the standard ID for the detector containing a hit.
 
 Detector parameters are defined as follows in the database:
   Name                ID type ew ns nx ny
   FPD-north           0  0    0  0   7  7
   FPD-south           1  0    0  1   7  7
   FPD-north-preshower 2  1    0  0   7  1
   FPD-south-preshower 3  1    0  1   7  1
   FPD-north-smdv      4  2    0  0  48  1
   FPD-south-smdv      5  2    0  1  48  1
   FPD-north-smdh      6  3    0  0   1 48
   FMS-south-smdh      7  3    0  1   1 48
   FMS-north-large     8  4    1  0  17 34
   FMS-south-large     9  4    1  1  17 34
   FMS-north-small     10 0    1  0  12 24
   FMS-south-small     11 0    1  1  12 24
 where
   - ew is the east-or-west location: 0 = east (the FPD), 1 = west (the FMS)
   - ns is the north-or-south location: 0 = north, 1 = south
   - nx, ny is the number of x, y channels
   - type corresponds to:
      - 0 = Small cell
      - 1 = Preshower
      - 2 = SMD-V
      - 3 = SMD-H
      - 4 = Large cell
      - 5 = Hadron calorimetry
 See:
 http://online.star.bnl.gov/dbExplorer/# --> Geometry/fms/ChannelGeometry
 Look into pams/sim/g2t/g2t_volume_id.g
 and search for FLGR (small pbg) or FLXF (large pbg).

Email from Akio describing decoding of the g2t volume ID, 16th Jan 2014:

 G2T volume id is currently
 id = ew*10000+nstb*1000+ch
 where
 ew: 1=fpd, 2=fms
 nstb for ew=1(FPD):
       1=north, 2=south, 3=top, 4=bottom
       5=north pbG preshower, 6=south pbg preshower
 nstb for ew=2(FMS):
       1=north large, 2=south large
       3=north small, 4=south small ch: channel#

 Once we add preshower, it will become
 id = det*100000 + ew*10000+nstb*1000+ch
 where
 det: 0=fpd/fms, 1 for PS
 ch:  will be devided into layer & ch for PS
 so fms/fpd volume id should NOT change at all.
 */
Int_t StFmsFastSimulatorMaker::getDetectorId(const g2t_emc_hit_st& hit) const {
  const Int_t volumeId = hit.volume_id;
  // Decode volume ID into detector type, fpd/fms and north/south locations
  const Int_t fpdOrFms = (volumeId % 100000) / 10000;
  const Int_t module = (volumeId % 10000) / 1000;
  // FPD/FMS and north/south to determine the detector ID
  enum { kFpd = 1, kFms = 2, kNorth = 0, kSouth = 1 };
  switch (fpdOrFms) {
    case kFpd:
      switch (module) {
        case 1: return kFpdNorth;  // north
        case 2: return kFpdSouth;  // south
        case 5: return kFpdNorthPreshower;  // preshower north
        case 6: return kFpdSouthPreshower;  // preshower south
      }  // switch
      break;
    case kFms:
      switch (module) {
        case 1: return kFmsNorthLarge;  // north large cells
        case 2: return kFmsSouthLarge;  // south large cells
        case 3: return kFmsNorthSmall;  // north small cells
        case 4: return kFmsSouthSmall;  // south small cells
      }  // switch
      break;
  }  // switch
  return kFmsInvalidDetectorId;
}

/* Dump hit information to LOG_INFO. */
void StFmsFastSimulatorMaker::printStEventSummary(const StEvent* event) {
  const Int_t NDETECTORS = 14;
  const Char_t* detectorNames[NDETECTORS] = {
      "FPD-North ",
      "FPD-South",
      "FPD-North-Pres",
      "FPD-South-Pres",
      "FPD-North-SMDV",
      "FPD-South-SMDV",
      "FPD-North-SMDH",
      "FPD-South-SMDH",
      "FMS-North-Large",
      "FMS-South-Large",
      "FMS-North-Small",
      "FMS-South-Small",
      "FHC-North",
      "FHC-South"
  };
  // Array of total hits per subdetector, initialised to all zeros
  Int_t nhits[NDETECTORS];
  std::fill(nhits, nhits + NDETECTORS, 0);
  // Array of total energy per sub-detector, initialised to all zeros
  Float_t detectorEnergy[NDETECTORS];
  std::fill(detectorEnergy, detectorEnergy + NDETECTORS, 0.f);
  // Sum number of hits and energies
  const StSPtrVecFmsHit& hits = event->fmsCollection()->hits();
  for (size_t i = 0; i < hits.size(); ++i) {
    const StFmsHit* hit = hits[i];
    if (Debug()) {
      hit->print();
    }  // if
    ++nhits[hit->detectorId()];
    detectorEnergy[hit->detectorId()] += hit->energy();
  }  // for
  // Print detectors summary
  LOG_INFO << "ID\tNAME\t\tNHITS\tENERGY" << endm;
  for (Int_t detectorId = 0; detectorId < NDETECTORS; ++detectorId) {
    LOG_INFO << detectorId << '\t' << detectorNames[detectorId] << '\t'
             << nhits[detectorId] << '\t' << detectorEnergy[detectorId] << endm;
  }  // for
}

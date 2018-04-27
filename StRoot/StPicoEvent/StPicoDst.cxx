#include "St_base/StMessMgr.h"

#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoEmcTrigger.h"
#include "StPicoEvent/StPicoBTowHit.h"
#include "StPicoEvent/StPicoBTofHit.h"
#include "StPicoEvent/StPicoMtdHit.h"
#include "StPicoEvent/StPicoFmsHit.h"
#include "StPicoEvent/StPicoBEmcPidTraits.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"
#include "StPicoEvent/StPicoMtdPidTraits.h"
#include "StPicoEvent/StPicoDst.h"

TClonesArray** StPicoDst::picoArrays = 0;

//_________________
void StPicoDst::unset() {
  picoArrays = 0;
}

//_________________
void StPicoDst::set(TClonesArray** thePicoArrays) {
  picoArrays = thePicoArrays;
}

//_________________
void StPicoDst::print() const {
  LOG_INFO << "\n=========== event header =============\n\n";
  LOG_INFO << " fill/run/event Id = " << event()->fillId() << "/" << event()->runId() << "/" << event()->eventId() << "\n";
  LOG_INFO << " vertex = " << event()->primaryVertex() << "\n";
  LOG_INFO << " refMult = " << event()->refMult() << " refMultFtpc = " << event()->refMultFtpc() << "\n";
  LOG_INFO << " nVpdHits e/w = " << event()->nVpdHitsEast() << "/" << event()->nVpdHitsWest() << " nTofT0 = " << event()->nTofT0() << " vzVpd = " << event()->vzVpd() << "\n";
  LOG_INFO << "=====================================\n\n";
}

//_________________
void StPicoDst::printTracks() {
  if (numberOfTracks() == 0) {
    LOG_INFO << "No tracks found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ track list ( " << numberOfTracks() << " entries )\n\n";
  for (UInt_t i_trk = 0; i_trk < numberOfTracks(); i_trk++) {
    LOG_INFO << "+++ track " << i_trk << "\n";
    track(i_trk)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printTriggers() {

  LOG_INFO << "=== EMC triggers ===" << "\n";

  if (numberOfEmcTriggers() == 0) {
    LOG_INFO << "No triggers found!" << endm;
    return;
  }

  LOG_INFO << "\n\n+++++++++ trigger list ( " << numberOfEmcTriggers() << " entries )\n\n";
  for (UInt_t i_t = 0; i_t < numberOfEmcTriggers(); i_t++) {
    LOG_INFO << "+++ trigger " << i_t << "\n";
    emcTrigger(i_t)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printBTOWHits() {

  if (numberOfBTOWHits() == 0) {
    LOG_INFO << "No BTOWHit found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ BTOWHit list ( " << numberOfBTOWHits() << " entries )\n\n";
  for (UInt_t i_t = 0; i_t < numberOfBTOWHits(); i_t++) {
    LOG_INFO << "+++ btowHit " << i_t << "\n";
    btowHit(i_t)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printBTofHits() {

  if (numberOfBTofHits() == 0) {
    LOG_INFO << "No BTofHit found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ BTof list ( " << numberOfBTofHits() << " entries )\n\n";
  for (UInt_t i_t = 0; i_t < numberOfBTofHits(); i_t++) {
    LOG_INFO << "+++ btofHit " << i_t << "\n";
    btofHit(i_t)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printMtdHits() {

  if (numberOfMtdHits() == 0) {
    LOG_INFO << "No MtdHit found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ Mtd list ( " << numberOfMtdHits() << " entries )\n\n";
  for (UInt_t i_t = 0; i_t < numberOfMtdHits(); i_t++) {
    LOG_INFO << "+++ mtdHit " << i_t << "\n";
    mtdHit(i_t)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printFmsHits() {

  if (numberOfFmsHits() == 0) {
    LOG_INFO << "No FmsHit found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ Fms list ( " << numberOfFmsHits() << " entries )\n\n";
  for (UInt_t i_t = 0; i_t < numberOfFmsHits(); i_t++) {
    LOG_INFO << "+++ fmsHit " << i_t << "\n";
    fmsHit(i_t)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printBEmcPidTraits() {

  if (numberOfBEmcPidTraits() == 0) {
    LOG_INFO << "No BEmc pidTraits found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ BEmc pidTraits list ( " << numberOfBEmcPidTraits() << " entries )\n\n";
  for (UInt_t i_t = 0; i_t < numberOfBEmcPidTraits(); i_t++) {
    LOG_INFO << "+++ BEmcPidTraits " << i_t << "\n";
    bemcPidTraits(i_t)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printBTofPidTraits() {

  if (numberOfBTofPidTraits() == 0) {
    LOG_INFO << "No BTof pidTraits found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ BTof pidTraits list ( " << numberOfBTofPidTraits() << " entries )\n\n";
  for (UInt_t i_t = 0; i_t < numberOfBTofPidTraits(); i_t++) {
    LOG_INFO << "+++ EmcPidTraits " << i_t << "\n";
    btofPidTraits(i_t)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printMtdPidTraits() {

  if (numberOfMtdPidTraits() == 0) {
    LOG_INFO << "No Mtd pidTraits found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ Mtd pidTraits list ( " << numberOfMtdPidTraits() << " entries )\n\n";
  for (UInt_t i_t = 0; i_t < numberOfMtdPidTraits(); i_t++) {
    LOG_INFO << "+++ mtdPidTraits " << i_t << "\n";
    mtdPidTraits(i_t)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}


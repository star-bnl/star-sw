#ifdef _VANILLA_ROOT_
#include <iostream>
#define LOG_INFO std::cout
#define endm std::endl
#else
#include "St_base/StMessMgr.h"
#endif

/// PicoDst headers
#include "StPicoEvent.h"
#include "StPicoTrack.h"
#include "StPicoEmcTrigger.h"
#include "StPicoBTowHit.h"
#include "StPicoBTofHit.h"
#include "StPicoMtdHit.h"
#include "StPicoFmsHit.h"
#include "StPicoBEmcPidTraits.h"
#include "StPicoBTofPidTraits.h"
#include "StPicoMtdPidTraits.h"
#include "StPicoTrackCovMatrix.h"
#include "StPicoDst.h"          //MUST be the last one

TClonesArray** StPicoDst::picoArrays = 0;
StPicoDst *StPicoDst::fgPicoDst = 0;
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
  LOG_INFO << "\n=========== Event header =============\n\n";
  event()->Print();
  LOG_INFO << "=====================================\n\n";
}

//_________________
void StPicoDst::printTracks() {
  if(numberOfTracks() == 0) {
    LOG_INFO << "No tracks found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ track list ( " << numberOfTracks() << " entries )\n\n";
  for(UInt_t iTrk=0; iTrk<numberOfTracks(); iTrk++) {
    LOG_INFO << "+++ track " << iTrk << "\n";
    track(iTrk)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printTriggers() {

  LOG_INFO << "=== EMC triggers ===" << "\n";

  if(numberOfEmcTriggers() == 0) {
    LOG_INFO << "No triggers found!" << endm;
    return;
  }

  LOG_INFO << "\n\n+++++++++ trigger list ( " << numberOfEmcTriggers() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfEmcTriggers(); iEntry++) {
    LOG_INFO << "+++ trigger " << iEntry << "\n";
    emcTrigger(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printBTOWHits() {

  if(numberOfBTOWHits() == 0) {
    LOG_INFO << "No BTOWHit found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ BTOWHit list ( " << numberOfBTOWHits() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfBTOWHits(); iEntry++) {
    LOG_INFO << "+++ btowHit " << iEntry << "\n";
    btowHit(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printBTofHits() {

  if(numberOfBTofHits() == 0) {
    LOG_INFO << "No BTofHit found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ BTof list ( " << numberOfBTofHits() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfBTofHits(); iEntry++) {
    LOG_INFO << "+++ btofHit " << iEntry << "\n";
    btofHit(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printMtdHits() {

  if(numberOfMtdHits() == 0) {
    LOG_INFO << "No MtdHit found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ Mtd list ( " << numberOfMtdHits() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfMtdHits(); iEntry++) {
    LOG_INFO << "+++ mtdHit " << iEntry << "\n";
    mtdHit(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printFmsHits() {

  if(numberOfFmsHits() == 0) {
    LOG_INFO << "No FmsHit found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ Fms list ( " << numberOfFmsHits() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfFmsHits(); iEntry++) {
    LOG_INFO << "+++ fmsHit " << iEntry << "\n";
    fmsHit(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printBEmcPidTraits() {

  if(numberOfBEmcPidTraits() == 0) {
    LOG_INFO << "No BEmc pidTraits found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ BEmc pidTraits list ( " << numberOfBEmcPidTraits() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfBEmcPidTraits(); iEntry++) {
    LOG_INFO << "+++ BEmcPidTraits " << iEntry << "\n";
    bemcPidTraits(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printBTofPidTraits() {

  if(numberOfBTofPidTraits() == 0) {
    LOG_INFO << "No BTof pidTraits found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ BTof pidTraits list ( " << numberOfBTofPidTraits() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfBTofPidTraits(); iEntry++) {
    LOG_INFO << "+++ EmcPidTraits " << iEntry << "\n";
    btofPidTraits(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printMtdPidTraits() {

  if(numberOfMtdPidTraits() == 0) {
    LOG_INFO << "No Mtd pidTraits found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ Mtd pidTraits list ( " << numberOfMtdPidTraits() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfMtdPidTraits(); iEntry++) {
    LOG_INFO << "+++ mtdPidTraits " << iEntry << "\n";
    mtdPidTraits(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printTrackCovMatrices() {
  if (numberOfTrackCovMatrices()==0 ) {
    LOG_INFO << "No track covariant matrices found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ Track covariant matrix list ( " << numberOfTrackCovMatrices() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfTrackCovMatrices(); iEntry++) {
    LOG_INFO << "+++ trackCovMatrix " << iEntry << "\n";
    trackCovMatrix(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//
// StPicoDst holds pointers to TClonesArrays with all data
//

// PicoDst headers
#include "StPicoMessMgr.h"
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
#include "StPicoBEmcSmdEHit.h"
#include "StPicoBEmcSmdPHit.h"
#include "StPicoDst.h"          //MUST be the last one

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
void StPicoDst::printBTowHits() {

  if(numberOfBTowHits() == 0) {
    LOG_INFO << "No BTowHit found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ BTowHit list ( " << numberOfBTowHits() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfBTowHits(); iEntry++) {
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
    LOG_INFO << "No BEMC pidTraits found!" << endm;
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
    LOG_INFO << "No MTD pidTraits found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ MTD pidTraits list ( " << numberOfMtdPidTraits() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfMtdPidTraits(); iEntry++) {
    LOG_INFO << "+++ mtdPidTraits " << iEntry << "\n";
    mtdPidTraits(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printTrackCovMatrices() {

  if(numberOfTrackCovMatrices() == 0) {
    LOG_INFO << "No TrackCovMatrix found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ trackCovMatrix list ( " << numberOfTrackCovMatrices() << " entries )\n\n";
  for(UInt_t iEntry=0; iEntry<numberOfTrackCovMatrices(); iEntry++) {
    LOG_INFO << "+++ trackCovMatrix " << iEntry << "\n";
    trackCovMatrix(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printBEmcSmdEHits() {

  if ( numberOfBEmcSmdEHits() == 0 ) {
    LOG_INFO << "No BEmc SmdE hits found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ BEmc SmdE hits list ( " << numberOfBEmcSmdEHits() << " entries )\n\n";
  for ( UInt_t iEntry=0; iEntry<numberOfBEmcSmdEHits(); iEntry++ ) {
    LOG_INFO << "+++ BEmcSmdEHit " << iEntry << "\n";
    bemcSmdEHit(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

//_________________
void StPicoDst::printBEmcSmdPHits() {

  if ( numberOfBEmcSmdPHits() == 0 ) {
    LOG_INFO << "No BEmc SmdP hits found!" << endm;
    return;
  }

  LOG_INFO << "\n+++++++++ BEmc SmdP hits list ( " << numberOfBEmcSmdPHits() << " entries )\n\n";
  for( UInt_t iEntry=0; iEntry<numberOfBEmcSmdPHits(); iEntry++ ) {
    LOG_INFO << "+++ BEmcSmdPHit " << iEntry << "\n";
    bemcSmdPHit(iEntry)->Print();
    LOG_INFO << "\n";
  }

  LOG_INFO << endm;
}

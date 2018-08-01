/// C++ headers
#include <limits>

/// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoMtdPidTraits.h"

ClassImp(StPicoMtdPidTraits)

//_________________
StPicoMtdPidTraits::StPicoMtdPidTraits() : TObject(),
  mTrackIndex(-1), mMtdHitIndex(-1), mMatchFlag(-1),
  mDeltaY(-999.), mDeltaZ(-999.), mDeltaTimeOfFlight(-999.), 
  mBeta(-999.), mMtdHitChan(-1) {
  /* emtpy */
}

//_________________
StPicoMtdPidTraits::StPicoMtdPidTraits(const StPicoMtdPidTraits &traits) : TObject() {
  mTrackIndex = traits.mTrackIndex;
  mMtdHitIndex = traits.mMtdHitIndex;
  mMatchFlag = traits.mMatchFlag;
  mDeltaY = traits.mDeltaY;
  mDeltaZ = traits.mDeltaZ;
  mDeltaTimeOfFlight = traits.mDeltaTimeOfFlight;
  mBeta = traits.mBeta;
  mMtdHitChan = traits.mMtdHitChan;
}

//_________________
StPicoMtdPidTraits::~StPicoMtdPidTraits() {
  /* emtpy */
}

//_________________
void StPicoMtdPidTraits::Print(const Char_t* option) const {

  LOG_INFO << "Matched hit: backleg =  " << backleg()
           << ", module  = " << module()
           << ", cell    = " << cell()
           << endm;
  LOG_INFO << "Matched track index = " << mTrackIndex << endm;
  LOG_INFO << "(DeltaY, DeltaZ, DeltaTOF, beta) = ("
           << mDeltaY << ", "
           << mDeltaZ << ", "
           << mDeltaTimeOfFlight << ", "
           << mBeta << ")" << endm;
}

//_________________
void StPicoMtdPidTraits::setHitChannel(Int_t backleg, Int_t module, Int_t cell) {
  Int_t gchan = (backleg - 1) * 60 + (module - 1) * 12 + cell;
  mMtdHitChan = (gchan > std::numeric_limits<short>::max()) ? -1 : (Short_t) gchan;  
}

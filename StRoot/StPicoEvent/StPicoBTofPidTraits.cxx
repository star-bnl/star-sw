/// C++ headers
#include <limits>

/// ROOT headers
#include <TMath.h>

/// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoBTofPidTraits.h"

ClassImp(StPicoBTofPidTraits)

//_________________
StPicoBTofPidTraits::StPicoBTofPidTraits() : TObject(),
  mTrackIndex(-1), mBTofCellId(-1), mBTofMatchFlag(-1),
  mBTof(0), mBTofBeta(0), mBTofYLocal(-6), mBTofZLocal(-6),
  mBTofHitPosX(-240), mBTofHitPosY(-240), mBTofHitPosZ(-240) {
  /* empty */
}

//_________________
StPicoBTofPidTraits::StPicoBTofPidTraits(const StPicoBTofPidTraits &traits) : TObject() {
  mTrackIndex = traits.mTrackIndex;
  mBTofCellId = traits.mBTofCellId;
  mBTofMatchFlag = traits.mBTofMatchFlag;
  mBTof = traits.mBTof;
  mBTofBeta = traits.mBTofBeta;
  mBTofYLocal = traits.mBTofYLocal;
  mBTofZLocal = traits.mBTofZLocal;
  mBTofHitPosX = traits.mBTofHitPosX;
  mBTofHitPosY = traits.mBTofHitPosY;
  mBTofHitPosZ = traits.mBTofHitPosZ;
}

//_________________
StPicoBTofPidTraits::~StPicoBTofPidTraits() {
  /* empty */
}

//_________________
void StPicoBTofPidTraits::Print(const Char_t* option) const {
  LOG_INFO << " Matched track index = " << mTrackIndex << endm;
  LOG_INFO << " BTOF cellId = " << btofCellId() << " tof = " << btof() << " beta = " << btofBeta() << endm;
  LOG_INFO << " BTOF match = " << btofMatchFlag() << " yLocal/zLocal " << btofYLocal() << " " << btofZLocal() << endm;
}


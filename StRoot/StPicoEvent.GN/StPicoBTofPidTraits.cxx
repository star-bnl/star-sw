#ifdef _VANILLA_ROOT_
#include <iostream>
#define LOG_INFO std::cout
#define endm std::endl
#else
#include "St_base/StMessMgr.h"
#endif

/// C++ headers
#include <limits>

/// ROOT headers
#include "TMath.h"

/// PicoDst headers
#include "StPicoBTofPidTraits.h"

ClassImp(StPicoBTofPidTraits)

//_________________
StPicoBTofPidTraits::StPicoBTofPidTraits() :
  mTrackIndex(-1),
  mBTofCellId(-1), mBTofMatchFlag(-1),
  mBTof(0), mBTofBeta(0),
  mBTofYLocal(-999), mBTofZLocal(-999),
  mBTofHitPosX(-999), mBTofHitPosY(-999), mBTofHitPosZ(-999) {
  /* empty */
}

//_________________
StPicoBTofPidTraits::StPicoBTofPidTraits(const StPicoBTofPidTraits &traits) {
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

//_________________
void StPicoBTofPidTraits::setTOF(Float_t tof) {
  if(tof<0) {
    mBTof = 0.;
  }
  else {
    mBTof = ( (tof * 1000.) > std::numeric_limits<unsigned short>::max()) ? std::numeric_limits<unsigned short>::max() : (UShort_t)(TMath::Nint(tof * 1000.));
  }
}

//_________________
void StPicoBTofPidTraits::setBeta(Float_t beta) {
  if(beta<0.) {
    mBTofBeta = 0.;
  }
  else {
    mBTofBeta = ( (beta * 20000.) > std::numeric_limits<unsigned short>::max()) ? std::numeric_limits<unsigned short>::max() : (UShort_t)(TMath::Nint(beta * 20000.));
  }
}

//_________________
void StPicoBTofPidTraits::setHitPositionXYZ(Float_t x, Float_t y, Float_t z) {
  mBTofHitPosX = (fabs(x * 100.) > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(x * 100.));
  mBTofHitPosY = (fabs(y * 100.) > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(y * 100.));
  mBTofHitPosZ = (fabs(z * 100.) > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(z * 100.));
}

//_________________
void StPicoBTofPidTraits::setHitPositionX(Float_t x) {
  mBTofHitPosX = (fabs(x * 100.) > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(x * 100.));
}

//_________________
void StPicoBTofPidTraits::setHitPositionY(Float_t y) {
  mBTofHitPosY = (fabs(y * 100.) > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(y * 100.));  
}

//_________________
void StPicoBTofPidTraits::setHitPositionZ(Float_t z) {
  mBTofHitPosZ = (fabs(z * 100.) > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(z * 100.));  
}

//_________________
void StPicoBTofPidTraits::setYLocal(Float_t yLocal) {
  mBTofYLocal  = (fabs(yLocal) * 1000. > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(yLocal * 1000.));
}

//_________________
void StPicoBTofPidTraits::setZLocal(Float_t zLocal) {
  mBTofZLocal  = (fabs(zLocal) * 1000. > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(zLocal * 1000.));
}

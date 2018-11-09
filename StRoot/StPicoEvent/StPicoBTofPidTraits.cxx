//
// StPicoBTofPidTraits keeps information about tracks that matched BTOF
//

// C++ headers
#include <limits>
#include <cmath>

// ROOT headers
#include "TMath.h"

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoBTofPidTraits.h"

ClassImp(StPicoBTofPidTraits)

//_________________
StPicoBTofPidTraits::StPicoBTofPidTraits() : TObject(),
  mTrackIndex(-1), mBTofCellId(-1), mBTofMatchFlag(-1),
  mBTof(0), mBTofBeta(0),
  mBTofYLocal( std::numeric_limits<short>::min() ),
  mBTofZLocal( std::numeric_limits<short>::min() ),
  mBTofHitPosX( std::numeric_limits<short>::min() ),
  mBTofHitPosY( std::numeric_limits<short>::min() ),
  mBTofHitPosZ( std::numeric_limits<short>::min() ) {
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
void StPicoBTofPidTraits::setBeta(Float_t beta) {
  if( beta <= 0) {
    mBTofBeta = 0;
  }
  else {
    mBTofBeta = ( (beta * 20000.) > std::numeric_limits<unsigned short>::max() ?
		  std::numeric_limits<unsigned short>::max() :
		  (UShort_t)( TMath::Nint( beta * 20000. ) ) );
  }
}

//_________________
void StPicoBTofPidTraits::setYLocal(Float_t yLocal) {
  mBTofYLocal = ( fabs(yLocal * 1000.) > std::numeric_limits<short>::max() ?
		  ( (yLocal>0) ? std::numeric_limits<short>::max() :
		    std::numeric_limits<short>::min() ):
		  (Short_t)( TMath::Nint( yLocal * 1000. ) ) );
}

//_________________
void StPicoBTofPidTraits::setZLocal(Float_t zLocal) {
  mBTofZLocal = ( fabs(zLocal * 1000.) > std::numeric_limits<short>::max() ?
		  ( (zLocal>0) ? std::numeric_limits<short>::max() :
		    std::numeric_limits<short>::min() ):
		  (Short_t)( TMath::Nint( zLocal * 1000. ) ) );
}

//_________________
void StPicoBTofPidTraits::setHitPositionX(Float_t x) {
  mBTofHitPosX = ( fabs(x * 100.) > std::numeric_limits<short>::max() ?
		   ( (x>0) ? std::numeric_limits<short>::max() :
		     std::numeric_limits<short>::min() ):
		   (Short_t)( TMath::Nint( x * 100. ) ) );
}

//_________________
void StPicoBTofPidTraits::setHitPositionY(Float_t y) {
  mBTofHitPosY = ( fabs(y * 100.) > std::numeric_limits<short>::max() ?
		   ( (y>0) ? std::numeric_limits<short>::max() :
		     std::numeric_limits<short>::min() ):
		   (Short_t)( TMath::Nint( y * 100. ) ) );
}

//_________________
void StPicoBTofPidTraits::setHitPositionZ(Float_t z) {
  mBTofHitPosZ = ( fabs(z * 100.) > std::numeric_limits<short>::max() ?
		   ( (z>0) ? std::numeric_limits<short>::max() :
		     std::numeric_limits<short>::min() ):
		   (Short_t)( TMath::Nint( z * 100. ) ) );
}

//_________________
void StPicoBTofPidTraits::setHitPositionXYZ(Float_t x, Float_t y, Float_t z) {
  setHitPositionX( x ); setHitPositionY( y ); setHitPositionZ( z );
}

//_________________
void StPicoBTofPidTraits::Print(const Char_t* option) const {
  LOG_INFO << " Matched track index = " << mTrackIndex << endm;
  LOG_INFO << " BTOF cellId = " << btofCellId() << " tof = " << btof()
	   << " beta = " << btofBeta() << endm;
  LOG_INFO << " BTOF match = " << btofMatchFlag() << " yLocal/zLocal "
	   << btofYLocal() << " " << btofZLocal() << endm;
}


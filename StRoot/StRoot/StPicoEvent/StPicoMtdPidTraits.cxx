//
// StPicoMtdPidTraits stores information related to the MTD-matched track
//

// C++ headers
#include <limits>

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoMtdPidTraits.h"

// ROOT headers
#include "TMath.h"

ClassImp(StPicoMtdPidTraits)

//_________________
StPicoMtdPidTraits::StPicoMtdPidTraits() : TObject(),
  mTrackIndex(-1), mMtdHitIndex(-1), mMatchFlag(-1),
  mDeltaY(-999.), mDeltaZ(-999.), mDeltaTimeOfFlight(-999.), 
  mBeta( 0 ), mMtdHitChan(-1) {
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
void StPicoMtdPidTraits::Print(const Char_t* option __attribute__((unused)) ) const {

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

//_________________
void StPicoMtdPidTraits::setBeta(Float_t beta) {
  if( beta <= 0 ) {
    mBeta = 0;
  }
  else {
    mBeta = ( (beta * 20000.) > std::numeric_limits<unsigned short>::max() ?
	      std::numeric_limits<unsigned short>::max() :
	      (UShort_t)( TMath::Nint( beta * 20000. ) ) );
  }
}

//_________________
void StPicoMtdPidTraits::setDeltaY(Float_t dy) {
  mDeltaY = ( TMath::Abs(dy * 200.) > std::numeric_limits<short>::max() ?
	      ( (dy > 0) ? std::numeric_limits<short>::max() :
		std::numeric_limits<short>::min() ) :
	      (Short_t)( TMath::Nint( dy * 200.) ) );
}

//_________________
void StPicoMtdPidTraits::setDeltaZ(Float_t dz) {
  // Provides 1.5 cm precision
  mDeltaZ = ( TMath::Abs(dz * 200.) > std::numeric_limits<short>::max() ?
	      ( (dz > 0) ? std::numeric_limits<short>::max() :
		std::numeric_limits<short>::min() ) :
	      (Short_t)( TMath::Nint( dz * 200. ) ) );
}

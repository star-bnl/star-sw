//
// StPicoETofPidTraits keeps information about tracks that matched eTOF
//

// C++ headers
#include <limits>
#include <cmath>

// ROOT headers
#include "TMath.h"

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoETofPidTraits.h"

ClassImp( StPicoETofPidTraits )

//_________________
StPicoETofPidTraits::StPicoETofPidTraits() : TObject(),
  mTrackIndex( -1 ), mHitIndex( -1 ), mMatchFlag( -1 ),
  mTimeOfFlight( 0 ), mBeta( 0 ),
  mDeltaX( std::numeric_limits<short>::min() ),
  mDeltaY( std::numeric_limits<short>::min() ),
  mCrossingX( 0 ), mCrossingY( 0 ), mCrossingZ( 0 ) {
  /* empty */
}

//_________________
StPicoETofPidTraits::StPicoETofPidTraits( const StPicoETofPidTraits& traits ) : TObject() {
  mTrackIndex   = traits.mTrackIndex;
  mHitIndex     = traits.mHitIndex;
  mMatchFlag    = traits.mMatchFlag;
  mTimeOfFlight = traits.mTimeOfFlight;
  mBeta         = traits.mBeta;
  mDeltaX       = traits.mDeltaX;
  mDeltaY       = traits.mDeltaY;
  mCrossingX    = traits.mCrossingX;
  mCrossingY    = traits.mCrossingY;
  mCrossingZ    = traits.mCrossingZ;
}

//_________________
StPicoETofPidTraits::~StPicoETofPidTraits() {
  /* empty */
}

//_________________
void StPicoETofPidTraits::Print( const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << " matched track index = " << trackIndex()
           << " eTOF hit index = "      << hitIndex()
           << " match flag = "          << matchFlag()
           << " time of flight = "      << tof()
           << " beta = "                << beta()
           << " deltaX = "              << deltaX()
           << " deltaY = "              << deltaY()
           << " crossingX = "           << crossingX()
           << " crossingY = "           << crossingY()
           << " crossingZ = "           << crossingZ()
           << endm;
}

//
// Setters
//

//_________________
void StPicoETofPidTraits::setBeta( const Float_t& beta ) {
  if( beta <= 0) {
    mBeta = 0;
  }
  else {
    mBeta = ( (beta * 20000.) > std::numeric_limits<unsigned short>::max() ?
	      std::numeric_limits<unsigned short>::max() :
	      (UShort_t)( TMath::Nint( beta * 20000. ) ) );
  }
}

//_________________
void StPicoETofPidTraits::setDeltaX( const Float_t& deltaX ) { 
  mDeltaX = ( fabs(deltaX * 800.) > std::numeric_limits<short>::max() ?
	      ( (deltaX>0) ? std::numeric_limits<short>::max() : std::numeric_limits<short>::min() ):
	      (Short_t)( TMath::Nint( deltaX * 800. ) ) );
}

//_________________
void StPicoETofPidTraits::setDeltaY( const Float_t& deltaY ) { 
  mDeltaY = ( fabs(deltaY * 800.) > std::numeric_limits<short>::max() ?
	      ( (deltaY>0) ? std::numeric_limits<short>::max() : std::numeric_limits<short>::min() ):
	      (Short_t)( TMath::Nint( deltaY * 800. ) ) );
}

//_________________
void StPicoETofPidTraits::setCrossingX( const Float_t& x ) { 
  mCrossingX = ( fabs(x * 100.) > std::numeric_limits<short>::max() ?
		 ( (x>0) ? std::numeric_limits<short>::max() : std::numeric_limits<short>::min() ):
		 (Short_t)( TMath::Nint( x * 100. ) ) );
}

//_________________
void StPicoETofPidTraits::setCrossingY( const Float_t& y ) { 
  mCrossingY = ( fabs(y * 100.) > std::numeric_limits<short>::max() ?
		 ( (y>0) ? std::numeric_limits<short>::max() : std::numeric_limits<short>::min() ):
		 (Short_t)( TMath::Nint( y * 100. ) ) );
}

//_________________
void StPicoETofPidTraits::setCrossingZ( const Float_t& z ) { 
  mCrossingZ = ( fabs(z * 100.) > std::numeric_limits<short>::max() ?
  ( (z>0) ? std::numeric_limits<short>::max() : std::numeric_limits<short>::min() ):
  (Short_t)( TMath::Nint( z * 100. ) ) );
}

//
// StPicoETofHit holds information about ETOF hits
//

// C++ headers
#include <limits>

// ROOT headers
#include "TMath.h"

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoETofHit.h"

ClassImp(StPicoETofHit)

//_________________
StPicoETofHit::StPicoETofHit() : TObject(),
 mGeomId( 0 ),
 mLocalX( std::numeric_limits<short>::min() ),
 mLocalY( std::numeric_limits<short>::min() ),
 mClusterSize( 0 ),
 mTime( 0 ),
 mTot( 0 )    //,
 //mGlobalX( 0 ),
 //mGlobalY( 0 ),
 //mGlobalZ( 0 )
{
  /* empty */
}

//_________________
StPicoETofHit::StPicoETofHit( const StPicoETofHit& hit ) {
  mGeomId = hit.mGeomId;
  mLocalX = hit.mLocalX;
  mLocalY = hit.mLocalY;
  mClusterSize = hit.mClusterSize;
  mTime = hit.mTime;
  mTot  = hit.mTot;
  //mGlobalX = hit.mGlobalX;
  //mGlobalY = hit.mGlobalY;
  //mGlobalZ = hit.mGlobalZ;
}

//_________________
StPicoETofHit::~StPicoETofHit() {
  /* empty */
}

//_________________
void StPicoETofHit::Print( const Char_t* option ) const {
  LOG_INFO << " sector  = "     << sector()
           << " zPlane  = "     << zPlane()
           << " counter = "     << counter()
           << " localX = "      << localX()
           << " localY = "      << localY()
           << " clusterSize = " << clusterSize()
           << " time = "        << time()
           << " tot  = "        << tot()
           << endm;
}


//
// Setters
//

//_________________
void StPicoETofHit::setGeomId( const Int_t geomId ) {
  if( geomId<=0 ) {
    mGeomId = 0;
  }
  else {
    mGeomId = ( geomId > std::numeric_limits<unsigned char>::max()) ? std::numeric_limits<unsigned char>::max() : (UChar_t) geomId;
  }
}

//_________________
void StPicoETofHit::setGeomId( const Int_t sector, const Int_t plane, const Int_t counter ) {
  if( sector<13 || sector>24 || plane<1 || plane>3 || counter<1 || counter>3 ) {
    mGeomId = 0;
  }
  else {
    mGeomId  = (UChar_t)( (sector - 13) * 9 + (plane - 1) * 3 + (counter - 1) + 1 );
  }
}

//_________________
void StPicoETofHit::setLocalX( const Float_t& localX ) { 
  mLocalX = ( fabs(localX * 800.) > std::numeric_limits<short>::max() ?
  ( (localX>0) ? std::numeric_limits<short>::max() : std::numeric_limits<short>::min() ):
  (Short_t)( TMath::Nint( localX * 800. ) ) );
}

//_________________
void StPicoETofHit::setLocalY( const Float_t& localY ) { 
  mLocalY = ( fabs(localY * 800.) > std::numeric_limits<short>::max() ?
  ( (localY>0) ? std::numeric_limits<short>::max() : std::numeric_limits<short>::min() ):
  (Short_t)( TMath::Nint( localY * 800. ) ) );
}

//_________________
void StPicoETofHit::setClusterSize( const Int_t clusterSize ) {
  if( clusterSize <= 0 ) {
    mClusterSize = 0;
  }
  else {
    mClusterSize = ( clusterSize > std::numeric_limits<unsigned char>::max()) ? std::numeric_limits<unsigned char>::max() : (UChar_t) clusterSize;
  }
}

//_________________
void StPicoETofHit::setTime( const Float_t& time ) {
  Float_t cyclicTime = time;
  if( cyclicTime < 0. ) {
    while( cyclicTime < 0. ) {
      cyclicTime += 51200;
    }
  }
  if( cyclicTime > 51200. ) {
    while( cyclicTime > 51200. ) {
      cyclicTime -= 51200.;
    }
  }

  mTime = cyclicTime;
}

//_________________
void StPicoETofHit::setTot( const Float_t& tot ) {
  if( tot <= 0 ) {
    mTot = 0;
  }
  else {
    mTot = ( (tot * 250.) > std::numeric_limits<unsigned short>::max() ?
      std::numeric_limits<unsigned short>::max() :
      (UShort_t)( TMath::Nint( tot * 250. ) ) );
  }
}

/*
//_________________
void StPicoETofHit::setGlobalX( const Float_t& x ) { 
  mGlobalX = ( fabs(x * 100.) > std::numeric_limits<short>::max() ?
  ( (x>0) ? std::numeric_limits<short>::max() : std::numeric_limits<short>::min() ):
  (Short_t)( TMath::Nint( x * 100. ) ) );
}

//_________________
void StPicoETofHit::setGlobalY( const Float_t& y ) { 
  mGlobalY = ( fabs(y * 100.) > std::numeric_limits<short>::max() ?
  ( (y>0) ? std::numeric_limits<short>::max() : std::numeric_limits<short>::min() ):
  (Short_t)( TMath::Nint( y * 100. ) ) );
}

//_________________
void StPicoETofHit::setGlobalZ( const Float_t& z ) { 
  mGlobalZ = ( fabs(z * 100.) > std::numeric_limits<short>::max() ?
  ( (z>0) ? std::numeric_limits<short>::max() : std::numeric_limits<short>::min() ):
  (Short_t)( TMath::Nint( z * 100. ) ) );
}

//_________________
void StPicoETofHit::setGlobalPos( const Float_t& x, const Float_t& y, const Float_t& z ) {
  setGlobalX( x );
  setGlobalY( y );
  setGlobalZ( z );
}
*/
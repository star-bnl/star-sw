#include "StEEmcPoint.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include <iostream>

#include "StEvent/StEmcPoint.h"

ClassImp(StEEmcPoint);

// ----------------------------------------------------------------------------
StEEmcPoint::StEEmcPoint()
{
  mEmcPoint=0;
  mRelatives=999;
}

// ----------------------------------------------------------------------------
StEEmcPoint::StEEmcPoint( const StEEmcPoint &p )
{

  mPosition       = p.mPosition;
  mEnergy         = p.mEnergy;
  mFraction       = p.mFraction;
  mTowers         = p.mTowers;
  mWeights        = p.mWeights;
  mSmdClusters[0] = p.mSmdClusters[0];
  mSmdClusters[1] = p.mSmdClusters[1];
  mEmcPoint       = p.mEmcPoint;
  mRelatives      = p.mRelatives;

}

// ----------------------------------------------------------------------------

/*
void StEEmcPoint::print()
{
  std::cout << " X=" << mPosition.X() 
	    << " Y=" << mPosition.Y() 
	    << " energy=" << mEnergy
	    << " frac=" << mFraction
	    << std::endl;
}
*/

// ----------------------------------------------------------------------------

StEmcPoint *StEEmcPoint::stemc()
{

  StThreeVectorF position( mPosition.X(), mPosition.Y(), mPosition.Z() );

  mEmcPoint = new StEmcPoint();
  mEmcPoint->setEnergy( energy() );
  mEmcPoint->setPosition( position );

  /// position error is a kludge, assumption is that we are accurate
  /// to within 1/2 of the molliere radius,  R_{M}=2.0cm in Pb
  StThreeVectorF error( 1.0, 1.0, 1.0 );
  mEmcPoint->setPositionError( error );
  
  /// endcap clusters are not used to determine point energy.
  /// towers + weighting function based on position to get
  /// point energy.
  mEmcPoint->addCluster( kEndcapEmcTowerId, 0 );

  mEmcPoint->addCluster( kEndcapSmdUStripId, mSmdClusters[0].stemc() );
  mEmcPoint->addCluster( kEndcapSmdVStripId, mSmdClusters[1].stemc() );

  return mEmcPoint;

}





// --------------------------------------------------------------------------
Bool_t StEEmcPoint::chiSquare( const StEEmcPoint &other ) const 
{

  /// determine which of two points has the better "chi^2" between
  /// the two smd planes

  Float_t myChi2 = ( mSmdClusters[0].energy() - mSmdClusters[1].energy() );
  myChi2 *= myChi2;

  Float_t otherChi2 = ( other.mSmdClusters[0].energy() - other.mSmdClusters[1].energy() );
  otherChi2 *= otherChi2;

  return myChi2 < otherChi2;

}


// ----------------------------------------------------------------------------

void StEEmcPoint::print()
{

  std::cout << "---------------------------------" << std::endl;
  for ( UInt_t i=0; i<mTowers.size(); i++ ) 
    mTowers[i].print();

  std::cout << "ucluster:" << std::endl;
  mSmdClusters[0].print();
  std::cout << "vcluster:" << std::endl;
  mSmdClusters[1].print();

}



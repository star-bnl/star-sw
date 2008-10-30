/**
 *
 * \class StEEmcIUPoint
 * \brief Base class for representing EEMC points
 *
 * A class for representing EEMC points.
 *
 * \author Weihong He
 *
 */
#include "StEEmcIUPoint.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include <iostream>

#include "StEvent/StEmcPoint.h"

ClassImp(StEEmcIUPoint);

// ----------------------------------------------------------------------------
StEEmcIUPoint::StEEmcIUPoint()
{
  mEmcPoint=0;
  mRelatives=999;
  for ( Int_t i=0;i<4;i++ ) mEnergy[i]=0.; 
  mResidueU=0.;
  mResidueV=0.;
}

// ----------------------------------------------------------------------------
StEEmcIUPoint::StEEmcIUPoint( const StEEmcIUPoint &p )
{

  mPosition       = p.mPosition;
  for ( Int_t i = 0; i < 4; i++ ) 
  mEnergy[i]      = p.mEnergy[i]; 
  mFraction       = p.mFraction;
  mTowers         = p.mTowers;
  mWeights        = p.mWeights;
  mSmdClusters[0] = p.mSmdClusters[0];
  mSmdClusters[1] = p.mSmdClusters[1];
  mEmcPoint       = p.mEmcPoint;
  mRelatives      = p.mRelatives;

  mSector = p.mSector;
  mSigma  = p.mSigma;
  mU      = p.mU;
  mV      = p.mV;

  mResidueU = p.mResidueU;
  mResidueV = p.mResidueV;

}

// ----------------------------------------------------------------------------

/*
void StEEmcIUPoint::print()
{
  std::cout << " X=" << mPosition.X() 
	    << " Y=" << mPosition.Y() 
	    << " energy=" << mEnergy
	    << " frac=" << mFraction
	    << std::endl;
}
*/

// ----------------------------------------------------------------------------

StEmcPoint *StEEmcIUPoint::stemc()
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
Bool_t StEEmcIUPoint::chiSquare( const StEEmcIUPoint &other ) const 
{

  /// determine which of two points has the better "chi^2" between
  /// the two smd planes

  Float_t myChi2 = ( (mSmdClusters[0].energy() - mSmdClusters[1].energy())/(mSmdClusters[0].energy() + mSmdClusters[1].energy()) );
  myChi2 *= myChi2;
  Float_t otherChi2 = ( (other.mSmdClusters[0].energy() - other.mSmdClusters[1].energy())/(other.mSmdClusters[0].energy() + other.mSmdClusters[1].energy()) );
  otherChi2 *= otherChi2;

  return myChi2 < otherChi2;

}


// ----------------------------------------------------------------------------

void StEEmcIUPoint::print()
{

  std::cout << "---------------------------------" << std::endl;
  std::cout << " X=" << mPosition.X() 
            << " Y=" << mPosition.Y() 
            << " energy=" << mEnergy[0] 
            << " frac=" << mFraction
            << std::endl;

  for ( UInt_t i=0; i<mTowers.size(); i++ ) 
    mTowers[i].print();

  std::cout << "ucluster:" << std::endl;
  mSmdClusters[0].print();
  std::cout << "vcluster:" << std::endl;
  mSmdClusters[1].print();

}



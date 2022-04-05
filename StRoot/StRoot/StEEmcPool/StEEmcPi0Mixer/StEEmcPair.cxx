/**
 *
 * \class StEEmcPair
 * \brief A class to represent pairs of points
 *
 * StEEmcPair is a class which represents the pairing of two EEMC
 * points.  The constructor takes in two points and (up to) two
 * vertices.  The two-body decay kinematics are computed at the
 * average z-vertex.
 *
 * When mixing real pi0 pair candidates, we use a common vertex.  
 * When a combinatoric background is being mixed, we have the option
 * to use two seperate vertices.  Whether this is the optimal 
 * solution or not should be studied.
 *
 * \author Jason C. Webb
 *
 */

#include "StEEmcPair.h"
#include <iostream>

#include "StMessMgr.h"

ClassImp(StEEmcPair);

// ----------------------------------------------------------------------------

StEEmcPair::StEEmcPair()
: TObject()
{
  mMass=-1.;
  mEnergy=-1;
  mZgg=-1;
  mPhigg=-1;
  mMomentum=TVector3(0,0,0);
  mVertex1=mMomentum;
  mVertex2=mMomentum;
}


StEEmcPair::StEEmcPair(const StEEmcPoint &p1, const StEEmcPoint &p2)
{
  StEEmcPair(p1,p2,TVector3(0,0,0),TVector3(0,0,0));
}

StEEmcPair::StEEmcPair(const StEEmcPoint &p1, const StEEmcPoint &p2, const TVector3 &v)
{
  StEEmcPair(p1,p2,v, v);
}

StEEmcPair::StEEmcPair(const StEEmcPoint &p1, const StEEmcPoint &p2, const TVector3 &v1, const TVector3 &v2)
: TObject()
{
  mPoint[0]=p1;
  mPoint[1]=p2;
  mVertex1=v1;
  mVertex2=v2;

  /// initialize to zero
  mMass=0.;
  mEnergy=0.;
  mZgg=0.;
  mPhigg=0.;
  mMomentum=TVector3(0,0,0);

  /// compute kinematics
  Kinematics();
}

void StEEmcPair::Kinematics()
{

  /// Determine energy of candidate meson
  mEnergy=mPoint[0].energy() + mPoint[1].energy();

  gMessMgr->Debug()<<"StEEmcPair -I- energy=" << mEnergy << endm;

  if ( mEnergy <= 0. ) {
    mMass=-1.0;
    return;
  }

  /// Energy sharing
  mZgg=TMath::Abs( mPoint[0].energy()-mPoint[1].energy() ) / mEnergy;

  gMessMgr->Debug()<<"StEEmcPair -I- zgg=" << mZgg << endm;

  /// Momenta of each gamma
  TVector3 momentum1=( mPoint[0].position() - mVertex1 ).Unit();
  momentum1 *= mPoint[0].energy();
  TVector3 momentum2=( mPoint[1].position() - mVertex2 ).Unit();
  momentum2 *= mPoint[1].energy();
  /// Opening angle between gammas
  mPhigg=momentum1.Angle(momentum2);
  /// Total momentum
  mMomentum=momentum1+momentum2;
  
  /// Set the "mean" vertex
  mVertex = ( mPoint[0].energy() * mVertex1 + mPoint[1].energy() * mVertex2 );
  mVertex *= 1./mEnergy;

  /// Calculate invariant mass
  mMass = mEnergy * TMath::Sin(mPhigg/2.0) * TMath::Sqrt( 1.0 - mZgg*mZgg );

  gMessMgr->Debug()<<"StEEmcPair -I- mass=" << mMass << endm;
  
}

// ----------------------------------------------------------------------------
StEEmcPair::StEEmcPair( const StEEmcPair &old )
: TObject(old)
{
  mVertex1=old.mVertex1;
  mVertex2=old.mVertex2;
  mVertex=old.mVertex;
  mPoint[0]=old.mPoint[0];
  mPoint[1]=old.mPoint[1];
  /// initialize to zero
  mMass=old.mMass;
  mEnergy=old.mEnergy;
  mZgg=old.mZgg;
  mPhigg=old.mPhigg;
  mMomentum=old.mMomentum;
}

// ----------------------------------------------------------------------------
void StEEmcPair::print() const
{
  std::cout << "pair mass=" << mass() 
	    << " e1=" << mPoint[0].energy() 
	    << " e2=" << mPoint[1].energy()
	    << " zgg=" << zgg() 
	    << " tow1=" << mPoint[0].tower(0).name() 
	    << " zvert=" << mVertex.Z() 
	    << std::endl;
}

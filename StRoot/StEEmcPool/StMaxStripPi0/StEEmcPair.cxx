#include "StEEmcPair.h"
#include "TMath.h"
ClassImp(StEEmcPair);

// ----------------------------------------------------------------------------

StEEmcPair::StEEmcPair()
{
  mMass=-1.;
  mEnergy=-1;
  mZgg=-1;
  mPhigg=-1;
  mMomentum=TVector3(0,0,0);
  mVertex1=mMomentum;
  mVertex2=mMomentum;
}


StEEmcPair::StEEmcPair( StEEmcPoint p1, StEEmcPoint p2 )
{
  StEEmcPair(p1,p2,TVector3(0,0,0),TVector3(0,0,0));
}

StEEmcPair::StEEmcPair( StEEmcPoint p1, StEEmcPoint p2, TVector3 v )
{
  StEEmcPair(p1,p2,v, v);
}

StEEmcPair::StEEmcPair( StEEmcPoint p1, StEEmcPoint p2, TVector3 v1, TVector3 v2 )
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
  if ( mEnergy <= 0. ) {
    mMass=-1.0;
    return;
  }
  /// Energy sharing
  mZgg=TMath::Abs( mPoint[0].energy()-mPoint[1].energy() ) / mEnergy;
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

  
}

// ----------------------------------------------------------------------------
StEEmcPair::StEEmcPair( const StEEmcPair &old )
{
  mVertex1=old.mVertex1;
  mVertex2=old.mVertex2;
  mPoint[0]=old.mPoint[0];
  mPoint[1]=old.mPoint[1];
  /// initialize to zero
  mMass=0.;
  mEnergy=0.;
  mZgg=0.;
  mPhigg=0.;
  mMomentum=TVector3(0,0,0);
  /// kompute kinematics
  Kinematics();
}

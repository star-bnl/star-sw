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
 * \author Weihong He
 *
 */
#include "TMath.h"
#include "StEEmcIUPair.h"
#include <iostream>
ClassImp(StEEmcIUPair);

// ----------------------------------------------------------------------------

StEEmcIUPair::StEEmcIUPair()
{
  mMass=-1.;
  mEnergy=-1;
  mZgg=-1;
  mPhigg=-1;
  mMomentum=TVector3(0,0,0);
  mVertex1=mMomentum;
  mVertex2=mMomentum;
}


StEEmcIUPair::StEEmcIUPair( StEEmcIUPoint p1, StEEmcIUPoint p2 )
{
  StEEmcIUPair(p1,p2,TVector3(0,0,0),TVector3(0,0,0));
}

StEEmcIUPair::StEEmcIUPair( StEEmcIUPoint p1, StEEmcIUPoint p2, TVector3 v )
{
  StEEmcIUPair(p1,p2,v, v);
}

StEEmcIUPair::StEEmcIUPair( StEEmcIUPoint p1, StEEmcIUPoint p2, TVector3 v1, TVector3 v2 )
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

void StEEmcIUPair::Kinematics()
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
  //printf("kinezvert=%f\n",mVertex1.Z());
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
  //printf("mMass=%f\n",mMass);
  
}

// ----------------------------------------------------------------------------
StEEmcIUPair::StEEmcIUPair( const StEEmcIUPair &old )
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

// ----------------------------------------------------------------------------
void StEEmcIUPair::print()
{
  std::cout << "pair mass=" << mass() 
	    << " e1=" << mPoint[0].energy() 
	    << " e2=" << mPoint[1].energy()
	    << " zgg=" << zgg() 
	    << " tow1=" << mPoint[0].tower(0).name() 
	    << " tow2=" << mPoint[1].tower(0).name() 
	    << " zvert=" << mVertex.Z() 
	    << " phigg=" << phigg()
	    << " pz=" << pz()
    << std::endl;

  //StEEmcIUSmdCluster tclust1 = mPoint[0].cluster(0);
  
  //Int_t nums=tclust1.numberOfStrips();
  //for ( Int_t is=0;is<nums;is++ )
  //{
  //  StEEmcStrip sti1=tclust1.strip(is);
  //  float stenergy1=sti1.energy();
  //  printf("clust0stripu.energy=%f index=%d\n",stenergy1,sti1.index());   
  //}
  //StEEmcIUSmdCluster tclust2 = mPoint[0].cluster(1);
  //Int_t n2=tclust2.numberOfStrips();
  //for ( Int_t is=0;is<n2;is++ )
  //{
  //  StEEmcStrip sti2=tclust2.strip(is);
  //  float stenergy2=sti2.energy();
  //  printf("clust0stripv.energy=%f index=%d\n",stenergy2,sti2.index());   
  //}
  //StEEmcIUSmdCluster tclust3 = mPoint[1].cluster(0);
  //Int_t n3=tclust3.numberOfStrips();
  //for ( Int_t is=0;is<n3;is++ )
  //{
  //StEEmcStrip sti3=tclust3.strip(is);
  //  float stenergy3=sti3.energy();
  //printf("clust1stripu.energy=%f index=%d\n",stenergy3,sti3.index());   
  //}
  //StEEmcIUSmdCluster tclust4 = mPoint[1].cluster(1);
  //Int_t n4=tclust4.numberOfStrips();
  //for ( Int_t is=0;is<n4;is++ )
  //{
  //StEEmcStrip sti4=tclust4.strip(is);
  //  float stenergy4=sti4.energy();
  //  printf("clust1stripv.energy=%f index=%d\n",stenergy4,sti4.index());   
  //}
  }

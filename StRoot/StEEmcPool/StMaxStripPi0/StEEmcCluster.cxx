#include "StEEmcCluster.h"
#include "StEvent/StEmcCluster.h"

#include <iostream>

ClassImp(StEEmcCluster);

// ----------------------------------------------------------------------------
StEEmcCluster::StEEmcCluster()
{

  mEmcCluster=0;
  mKey=0;
  mEnergy=0.;

}

// ----------------------------------------------------------------------------
void StEEmcCluster::add( StEEmcTower tower, Float_t weight )
{
  
  if ( weight * tower.energy() <= 0. ) return;
  mWeights.push_back(weight);
  mTowers.push_back( tower );
  
  Float_t energy = weight * tower.energy();
  mEnergy+=energy;

}

StEEmcCluster::~StEEmcCluster(){
  /// If we have created an StEmcCluster, delete it
  //$$$  if ( mEmcCluster != 0 ) delete mEmcCluster;
}


// ----------------------------------------------------------------------------
StEmcCluster *StEEmcCluster::stemc()
{

  if ( mEmcCluster ) return mEmcCluster;
  mEmcCluster=new StEmcCluster();

  mEmcCluster->setEta( momentum().Eta() );
  mEmcCluster->setPhi( momentum().Phi() );
  mEmcCluster->setSigmaEta(-1.);
  mEmcCluster->setSigmaPhi(-1.);
  mEmcCluster->setEnergy( energy() );
  mEmcCluster->SetUniqueID( mKey );
#if 1
  for ( Int_t i=0; i< numberOfTowers(); i++ ) 
    {
      StEmcRawHit *hit=mTowers[i].stemc();
      assert( hit );         
      mEmcCluster->addHit( hit );
    }
#endif
  
  return mEmcCluster;
}

void StEEmcCluster::print()
{
  
  std::cout << "cluster key: " << mKey << std::endl;
  std::cout << "seed tower:  " << mTowers[0].name() << std::endl;
  std::cout << "ntowers:     " << mTowers.size() << std::endl;
  std::cout << "eta:         " << mMomentum.Eta() << std::endl;
  std::cout << "phi:         " << mMomentum.Phi() << std::endl;
  std::cout << "energy:      " << mEnergy << std::endl;
  std::cout << "pt:          " << mMomentum.Perp() << std::endl;
  for ( UInt_t i=0;i<mTowers.size();i++ )
    {
      mTowers[i].printLine(); std::cout << " W=" << mWeights[i] << std::endl;
    }

}

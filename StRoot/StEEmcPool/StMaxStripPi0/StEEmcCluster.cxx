#include "StEEmcCluster.h"
#include "StEvent/StEmcCluster.h"

#include <iostream>

ClassImp(StEEmcCluster);

// ----------------------------------------------------------------------------
StEEmcCluster::StEEmcCluster()
{

  mEmcCluster=0;

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
  //  delete mGeom;
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
  for ( Int_t i=0; i< numberOfTowers(); i++ ) 
    {
      StEmcRawHit *hit=mTowers[i].stemc();
      assert( hit );         
      mEmcCluster->addHit( hit );
    }
  
  return mEmcCluster;
}

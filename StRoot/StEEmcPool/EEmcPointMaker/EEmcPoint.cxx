#include "EEmcPoint.h"
#include <iostream>
#include "StEEmcPool/EEmcAnalysisMaker/EEezCluster.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

ClassImp(EEmcPoint);

// ----------------------------------------------------------------------------
EEmcPoint::EEmcPoint() 
{
  mFlag=0;
}

// ----------------------------------------------------------------------------
EEmcPoint::EEmcPoint( EEezCluster *c )
{

  setSector(c->getSeedTower()->getSector());
  setEnergy(c->getEnergy());
  setFraction(-1.);
  mSmdPoint.setTower ( c->getSeedTower() );
  TVector3 p = c->getMomentum();
  setPosition( kEEmcZSMD * p.Unit() );  

}

// ----------------------------------------------------------------------------
void EEmcPoint::print()
{

  std::cout << "EEmcPoint";
  std::cout << " beneath tower " << mSmdPoint.tower()->getName() << std::endl;
  mPosition.Print("");
  std::cout << "energy = " << mEnergy << std::endl;
  std::cout << "cluster fraction = " << mFraction << std::endl;
  std::cout << "user flag = " << mFlag << std::endl;
  

}

// ----------------------------------------------------------------------------
void EEmcPoint::setSmdPoint( EEmcSmdPoint p ){ 
  mSmdPoint = p; 
  mUmean = p.clusterU()->getMean();
  mVmean = p.clusterV()->getMean();				 
} 


#include "EEmcSmdPoint.h"
#include "StEEmcPool/EEmcSmdClusterMaker/EEezSmdCluster.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcPool/EEmcAnalysisMaker/EEezTower.h"
#include "StEEmcPool/EEmcAnalysisMaker/EEezPatch.h"

ClassImp(EEmcSmdPoint);

// ----------------------------------------------------------------------------
EEmcSmdPoint::EEmcSmdPoint( EEezSmdCluster *u, EEezSmdCluster *v, Float_t fu, Float_t fv )
{

  assert(u); // Pass me some clusters please
  assert(v); // Pass me some clusters please

  mUcluster = u;
  mVcluster = v;
  mTower = 0; 

  Int_t sector = u->getSector();
  if ( sector != v->getSector() ) {
    Warning("EEmcSmdPoint","u sector= %d v sector= %d", sector, v->getSector());
    return;
  }

  EEmcSmdGeom *smd = EEmcSmdGeom::instance();
  mPosition = smd -> getIntersection(sector,u->getMean(),v->getMean());
  mEnergy = fu*u->getEnergy() + fv*v->getEnergy();

}

void EEmcSmdPoint::setFractions( Float_t fu, Float_t fv )
{

  mEnergy = fu*mUcluster->getEnergy() + fv*mVcluster->getEnergy();

}

// ----------------------------------------------------------------------------
Bool_t EEmcSmdPoint::match ( EEezTower *tower )
{

  EEmcGeomSimple geom = EEmcGeomSimple::Instance();
  Int_t mysec = tower -> getSector();
  Int_t mysub = tower -> getSubSector();
  Int_t myeta = tower -> getEtabin();
  
  Int_t sec,sub,eta;
  geom.getTower( mPosition, sec, sub, eta );

  if ( sec != mysec || sub != mysub || eta != myeta ) return false;

  return true;

}

// ----------------------------------------------------------------------------
Bool_t EEmcSmdPoint::match ( EEezCluster *cluster )
{

  EEezTower *seed = cluster -> getSeedTower();
  if ( match(seed) ) 
    return true;
  else
    for ( Int_t i = 0; i < seed -> getNNeighbors(); seed++ ) {
      if ( match( seed->getNeighbor(i) ) ) return true;
    }
  
       
  return false;

}

// ----------------------------------------------------------------------------
Bool_t EEmcSmdPoint::match ( EEezPatch *patch )
{

  return patch->hasTower(tower());

} 

// ----------------------------------------------------------------------------


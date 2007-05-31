//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 30 May 2007
//

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StGammaRawMaker.h"
#include "StBarrelEmcCluster.h"
#include "StBarrelEmcClusterMaker.h"

const float StBarrelEmcClusterMaker::mHighTowerThreshold = 3.0;	// GeV
const float StBarrelEmcClusterMaker::mClusterThreshold = 4.5; // GeV

ClassImp(StBarrelEmcClusterMaker);

int StBarrelEmcClusterMaker::Init()
{
  // Get gamma raw maker
  mGammaRawMaker = (StGammaRawMaker*)GetMaker("grawmaker");
  assert(mGammaRawMaker);
  return StMaker::Init();
}

void StBarrelEmcClusterMaker::Clear(Option_t* option)
{
  for (unsigned int i = 0; i < mClusters.size(); ++i) delete mClusters[i];
  mClusters.clear();
  StMaker::Clear(option);
}

int StBarrelEmcClusterMaker::Make()
{
  // Loop over towers
  for (int id = 1; id <= 4800; ++id) {
    if (StGammaTower* tower = mGammaRawMaker->tower(id, kBEmcTower)) {
      if (tower->energy > mHighTowerThreshold) {
	StBarrelEmcCluster* cluster = makeCluster(tower);
	if (cluster && cluster->energy() > mClusterThreshold)
	  mClusters.push_back(cluster);
      }
    }
  }

  LOG_DEBUG << "Number of BEMC clusters: " << mClusters.size() << endm;

  for (unsigned int i = 0; i < mClusters.size(); ++i) {
    LOG_DEBUG << "---------- BEMC CLUSTER #" << i << " ----------" << endm;
    LOG_DEBUG << *mClusters[i] << endm;
  }

  return kStOk;
}

StBarrelEmcCluster* StBarrelEmcClusterMaker::makeCluster(StGammaTower* tower) const
{
  //
  // A cluster is 3x3 patch of towers around the high tower.
  // The cluster energy is the sum of the energies of the towers.
  // The cluster position is the energy-weighted centroid of all
  // towers in the cluster.
  //
  StEmcGeom* geo = StEmcGeom::instance("bemc");
  StEmcPosition emcPosition;
  StBarrelEmcCluster* cluster = new StBarrelEmcCluster;
  cluster->setSeed(tower);
  int id = tower->id;
  float x, y, z;
  geo->getXYZ(id, x, y, z);
  x *= tower->energy;
  y *= tower->energy;
  z *= tower->energy;
  float energy = tower->energy;

  for (int deta = -1; deta <= 1; ++deta) {
    for (int dphi = -1; dphi <= 1; ++dphi) {
      if (deta || dphi) {
	int id2 = emcPosition.getNextTowerId(id, deta, dphi);
	if (StGammaTower* tower2 = mGammaRawMaker->tower(id2, kBEmcTower)) {
	  if (tower->energy > tower2->energy) {
	    cluster->setTower(deta, dphi, tower2);
	    float x2, y2, z2;
	    geo->getXYZ(id2, x2, y2, z2);
	    x += x2 * tower2->energy;
	    y += y2 * tower2->energy;
	    z += z2 * tower2->energy;
	    energy += tower2->energy;
	  }
	  else {
	    //
	    // The center tower doesn't have the highest energy.
	    // Cancel the cluster.
	    //
	    delete cluster;
	    return 0;
	  }
	}
      }
    }
  }

  x /= energy;
  y /= energy;
  z /= energy;

  cluster->setPosition(x, y, z);
  cluster->setEnergy(energy);

  return cluster;
}

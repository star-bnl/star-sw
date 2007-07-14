// -*- C++ -*-

//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 30 May 2007
//

#ifndef ST_BARREL_EMC_CLUSTER_MAKER_H
#define ST_BARREL_EMC_CLUSTER_MAKER_H

class StBarrelEmcCluster;
class StGammaEventMaker;
class StGammaRawMaker;
class StGammaTower;

#include "StMaker.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

class StBarrelEmcClusterMaker : public StMaker {
public:
  StBarrelEmcClusterMaker(const Char_t* name = "bemc_cluster" ) : StMaker(name) {}
  ~StBarrelEmcClusterMaker() {}

  void Clear(Option_t* option = "");
  int  Init();
  int  Make();

  vector<StBarrelEmcCluster*>& clusters();

private:
  static const float mHighTowerThreshold;
  static const float mClusterThreshold;

  StGammaEventMaker* mGammaEventMaker;
  StGammaRawMaker* mGammaRawMaker;
  TVector3 mVertex;
  vector<StBarrelEmcCluster*> mClusters;

  StBarrelEmcCluster* makeCluster(StGammaTower* tower) const;
  void getTowerPosition(int id, TVector3& position) const;

  ClassDef(StBarrelEmcClusterMaker, 1)
};

inline vector<StBarrelEmcCluster*>& StBarrelEmcClusterMaker::clusters()  { return mClusters; }

inline void StBarrelEmcClusterMaker::getTowerPosition(int id, TVector3& position) const
{
  float x, y, z;
  StEmcGeom::instance("bemc")->getXYZ(id, x, y, z);
  position.SetXYZ(x, y, z);
}

#endif

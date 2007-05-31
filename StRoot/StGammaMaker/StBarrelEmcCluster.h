// -*- C++ -*-

//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 30 May 2007
//

#ifndef ST_BARREL_EMC_CLUSTER_H
#define ST_BARREL_EMC_CLUSTER_H

class StGammaTower;

#include <iostream>
#include <vector>
using namespace std;

#include "TVector3.h"

class StBarrelEmcCluster : public TObject {
public:
  StBarrelEmcCluster();
  ~StBarrelEmcCluster() {}

  TVector3& position();
  const TVector3& position() const;
  float eta() const;
  float phi() const;
  float energy() const;
  float et() const;
  TVector3 momentum(const TVector3& vertex = TVector3(0,0,0)) const;
  StGammaTower* tower(int deta, int dphi) const;
  StGammaTower* seed() const;

  void setSeed(StGammaTower* tower);
  void setTower(int deta, int dphi, StGammaTower* tower);
  void setPosition(const TVector3& position);
  void setPosition(float x, float y, float z);
  void setEnergy(float energy);

private:
  TVector3 mPosition;
  float mEnergy;
  StGammaTower* mTowers[3][3];

  ClassDef(StBarrelEmcCluster, 1);
};

inline StBarrelEmcCluster::StBarrelEmcCluster() { memset(mTowers, 0, sizeof(mTowers)); }
inline TVector3& StBarrelEmcCluster::position() { return mPosition; }
inline const TVector3& StBarrelEmcCluster::position() const { return mPosition; }
inline float StBarrelEmcCluster::eta() const { return mPosition.Eta(); }
inline float StBarrelEmcCluster::phi() const { return mPosition.Phi(); }
inline float StBarrelEmcCluster::energy() const { return mEnergy; }
inline float StBarrelEmcCluster::et() const { return mEnergy * sin(mPosition.Theta()); }
inline StGammaTower* StBarrelEmcCluster::tower(int deta, int dphi) const { return mTowers[deta+1][dphi+1]; }
inline StGammaTower* StBarrelEmcCluster::seed() const { return tower(0,0); }

inline void StBarrelEmcCluster::setSeed(StGammaTower* tower) { setTower(0, 0, tower); }
inline void StBarrelEmcCluster::setTower(int deta, int dphi, StGammaTower* tower) { mTowers[deta+1][dphi+1] = tower; }
inline void StBarrelEmcCluster::setPosition(const TVector3& position) { mPosition = position; }
inline void StBarrelEmcCluster::setPosition(float x, float y, float z) { mPosition.SetXYZ(x, y, z); }
inline void StBarrelEmcCluster::setEnergy(float energy) { mEnergy = energy; }

inline TVector3 StBarrelEmcCluster::momentum(const TVector3& vertex) const
{
  TVector3 mom = mPosition - vertex;
  mom.SetMag(mEnergy);
  return mom;
}

ostream& operator<<(ostream& out, const StBarrelEmcCluster& cluster);

#endif

#ifndef STJTOWERENERGYCUTET0_H
#define STJTOWERENERGYCUTET0_H

#include "StjTowerEnergyCut.h"

#include <TVector3.h>
#include <TMath.h>

class StjTowerEnergyCutEt0 : public StjTowerEnergyCut {

public:
  StjTowerEnergyCutEt0(double min = 0, double max = 50000.0)
    : _min(min), _max(max) { }
  virtual ~StjTowerEnergyCutEt0() { }

  bool operator()(const StjTowerEnergy& deposit)
  {
    TVector3 vec3;
    vec3.SetPtEtaPhi(deposit.towerR, deposit.towerEta, deposit.towerPhi);
    double Et = (deposit.energy)*TMath::Sin(vec3.Theta());

    if(Et <= _min) return true;

    if(Et > _max) return true;

    return false;
  }

private:

  double _min;
  double _max;

  ClassDef(StjTowerEnergyCutEt0, 1)

};

#endif // STJTOWERENERGYCUTET0_H

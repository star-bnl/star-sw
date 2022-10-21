// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutEt.h,v 1.1 2008/11/27 07:35:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUTET_H
#define STJTOWERENERGYCUTET_H

#include "StjTowerEnergyCut.h"

#include <TVector3.h>
#include <TMath.h>

class StjTowerEnergyCutEt : public StjTowerEnergyCut {

public:
  StjTowerEnergyCutEt(double min = 0, double max = 50000.0, bool respect_vertex_position = true)
    : _min(min), _max(max), _flag(respect_vertex_position) { }
  virtual ~StjTowerEnergyCutEt() { }

  bool operator()(const StjTowerEnergy& deposit)
  {
    TVector3 vec3;
    vec3.SetPtEtaPhi(deposit.towerR, deposit.towerEta, deposit.towerPhi);
    //vertex
    if(_flag){
      TVector3 vertex(deposit.vertexX, deposit.vertexY, deposit.vertexZ);
      vec3 = vec3 - vertex;
    }

    double Et = (deposit.energy)*TMath::Sin(vec3.Theta());

    if(Et <= _min) return true;

    if(Et > _max) return true;

    return false;
  }

private:

  double _min;
  double _max;

  bool _flag;
  ClassDef(StjTowerEnergyCutEt, 2)

};

#endif // STJTOWERENERGYCUTET_H

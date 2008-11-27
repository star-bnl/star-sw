// -*- mode: c++;-*-
// $Id: StjTowerEnergyList.h,v 1.1 2008/11/27 07:35:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYLIST_H
#define STJTOWERENERGYLIST_H

#include <TObject.h>

#include <vector>
#include <ostream>
#include <cmath>

class StjTowerEnergy : public TObject {
public:
  int            runNumber;
  int            eventId;
  int            detectorId; // 9: BEMC, 13: EEMC
  int            towerId;
  double         towerR;
  double         towerEta;
  double         towerPhi;
  double         vertexX;
  double         vertexY;
  double         vertexZ;
  double         energy;
  unsigned int   adc;
  float          pedestal;
  float          rms;
  int            status;     // 1 is good for BEMC. 0 is good for EEMC
  ClassDef(StjTowerEnergy, 1)
};

typedef std::vector<StjTowerEnergy> StjTowerEnergyList;

inline bool operator==(const StjTowerEnergy& v1, const StjTowerEnergy& v2)
{
  if(v1.runNumber  != v2.runNumber)  return false;
  if(v1.eventId    != v2.eventId)    return false;	  
  if(v1.detectorId != v2.detectorId) return false;  
  if(v1.towerId    != v2.towerId)    return false;	  
  if(v1.towerR     != v2.towerR)     return false;	  
  if(v1.towerEta   != v2.towerEta)   return false;	  
  if(v1.towerPhi   != v2.towerPhi)   return false;	  
  if(v1.vertexX    != v2.vertexX)    return false;	  
  if(v1.vertexY    != v2.vertexY)    return false;	  
  if(v1.vertexZ    != v2.vertexZ)    return false;	  
  if(fabs(v1.energy - v2.energy) > 0.0001*fabs(v1.energy))     return false;	  
  if(v1.adc        != v2.adc)        return false;	  
  if(v1.pedestal   != v2.pedestal)   return false;	  
  if(v1.rms        != v2.rms)        return false;	  
  if(v1.status     != v2.status)     return false;       
  return true;
  }

inline bool operator!=(const StjTowerEnergy& v1, const StjTowerEnergy& v2)
{
  return(!(v1 == v2));
}

inline bool operator==(const StjTowerEnergyList& v1, const StjTowerEnergyList& v2){
  if(v1.size() != v2.size()) return false;
  for(size_t i = 0; i < v1.size(); ++i) if(v1[i] != v2[i]) return false;
  return true;
}

inline std::ostream& operator<<(std::ostream& out, const StjTowerEnergy& v)
{
  out << "towerId: " << v.towerId << ", energy: " << v.energy << ", .... ";
  return out;
}

inline std::ostream& operator<<(std::ostream& out, const StjTowerEnergyList& v)
{
  out << "TowerEnergyList size: " << v.size();
  return out;
}

#endif // STJTOWERENERGYLIST_H

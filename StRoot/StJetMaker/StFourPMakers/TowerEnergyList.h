// -*- mode: c++;-*-
// $Id: TowerEnergyList.h,v 1.5 2008/07/13 00:05:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYLIST_H
#define TOWERENERGYLIST_H

#include <vector>
#include <ostream>

namespace StSpinJet {

struct TowerEnergy {
  int            runNumber;
  int            eventId;
  int            detectorId; // 9: BEMC, 13: EEMC
  int            towerId;
  double         towerX;
  double         towerY;
  double         towerZ;
  double         vertexX;
  double         vertexY;
  double         vertexZ;
  double         energy;
  unsigned int   adc;
  float          pedestal;
  float          rms;
  int            status;     // 1 is good for BEMC. 0 is good for EEMC
};

typedef std::vector<TowerEnergy> TowerEnergyList;

inline bool operator==(const TowerEnergy& v1, const TowerEnergy& v2)
{
  if(v1.runNumber  != v2.runNumber)  return false;
  if(v1.eventId    != v2.eventId)    return false;	  
  if(v1.detectorId != v2.detectorId) return false;  
  if(v1.towerId    != v2.towerId)    return false;	  
  if(v1.towerX     != v2.towerX)     return false;	  
  if(v1.towerY     != v2.towerY)     return false;	  
  if(v1.towerZ     != v2.towerZ)     return false;	  
  if(v1.vertexX    != v2.vertexX)    return false;	  
  if(v1.vertexY    != v2.vertexY)    return false;	  
  if(v1.vertexZ    != v2.vertexZ)    return false;	  
  if(v1.energy     != v2.energy)     return false;	  
  if(v1.adc        != v2.adc)        return false;	  
  if(v1.pedestal   != v2.pedestal)   return false;	  
  if(v1.rms        != v2.rms)        return false;	  
  if(v1.status     != v2.status)     return false;       
  return true;
  }

inline bool operator!=(const TowerEnergy& v1, const TowerEnergy& v2)
{
  return(!(v1 == v2));
}

inline bool operator==(const TowerEnergyList& v1, const TowerEnergyList& v2){
  if(v1.size() != v2.size()) return false;
  for(size_t i = 0; i < v1.size(); ++i) if(v1[i] != v2[i]) return false;
  return true;
}

inline std::ostream& operator<<(std::ostream& out, const TowerEnergy& v)
{
  out << "towerId: " << v.towerId << ", evergy: " << v.energy << ", .... ";
  return out;
}

inline std::ostream& operator<<(std::ostream& out, const TowerEnergyList& v)
{
  out << "TowerEnergyList size: " << v.size();
  return out;
}

}

#endif // TOWERENERGYLIST_H

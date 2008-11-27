// -*- mode: c++;-*-
// $Id: StjFourVecList.h,v 1.1 2008/11/27 07:29:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECLIST_H
#define STJFOURVECLIST_H

#include <TObject.h>

#include <ostream>
#include <vector>

class StjFourVec : public TObject {
public:
  int            runNumber;
  int            eventId;
  int            fourvecId;
  int            type;       // 0: mc, 1: track, 2: tower energy
  int            detectorId; // 1: TPC, 9: BEMC, 13: EEMC
  short          trackId;
  int            towerId;       
  int            mcparticleId;
  double         pt;
  double         eta;
  double         phi;
  double         m;
  double         vertexZ;
  ClassDef(StjFourVec, 1)
};

typedef std::vector<StjFourVec> StjFourVecList;

inline bool operator==(const StjFourVec& v1, const StjFourVec& v2)
{
  if(v1.runNumber  != v2.runNumber)    return false;
  if(v1.eventId    != v2.eventId)      return false;   
  if(v1.fourvecId  != v2.fourvecId)    return false;   
  if(v1.type       != v2.type)         return false;  
  if(v1.detectorId != v2.detectorId)   return false;   
  if(v1.trackId    != v2.trackId)     return false;   
  if(v1.towerId    != v2.towerId)   return false;   
  if(v1.mcparticleId != v2.mcparticleId)   return false;   
  if(v1.pt         != v2.pt)      return false;   
  if(v1.eta        != v2.eta)   return false;   
  if(v1.phi        != v2.phi) return false;   
  if(v1.m          != v2.m)  return false;   
  return true;
  }

inline bool operator!=(const StjFourVec& v1, const StjFourVec& v2)
{
  return(!(v1 == v2));
}

inline bool operator==(const StjFourVecList& v1, const StjFourVecList& v2){
  if(v1.size() != v2.size()) return false;
  for(size_t i = 0; i < v1.size(); ++i) if(v1[i] != v2[i]) return false;
  return true;
}

inline std::ostream& operator<<(std::ostream& out, const StjFourVec& v)
{
  out << "fourvecId: " << v.fourvecId << ", pt: " << v.pt << ", .... ";
  return out;
}

inline std::ostream& operator<<(std::ostream& out, const StjFourVecList& v)
{
  out << "FourVecList size: " << v.size();
  return out;
}

#endif // STJFOURVECLIST_H

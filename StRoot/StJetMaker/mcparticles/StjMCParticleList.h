// -*- mode: c++;-*-
// $Id: StjMCParticleList.h,v 1.1 2008/11/27 07:40:04 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLELIST_H
#define STJMCPARTICLELIST_H

#include <TObject.h>

#include <ostream>
#include <vector>

struct StjMCParticle : public TObject {
public:
  int            runNumber;
  int            eventId;
  int            mcparticleId;
  int            pdg;
  int            firstMotherId;
  int            lastMotherId;
  int            firstDaughterId;
  int            lastDaughterId;
  double         pt;
  double         eta;
  double         phi;
  double         m;
  double         e;
  int            status; // 1: stable  2: unstable  3: incoming and parton
  double         vertexZ;
  ClassDef(StjMCParticle, 1)
};

typedef std::vector<StjMCParticle> StjMCParticleList;

inline bool operator==(const StjMCParticle& v1, const StjMCParticle& v2)
{
  if(v1.runNumber       != v2.runNumber)  return false;
  if(v1.eventId         != v2.eventId)     return false;   
  if(v1.mcparticleId    != v2.mcparticleId)  return false;   
  if(v1.pdg             != v2.pdg)            return false;   
  if(v1.firstMotherId   != v2.firstMotherId)   return false;   
  if(v1.lastMotherId    != v2.lastMotherId)     return false;   
  if(v1.firstDaughterId != v2.firstDaughterId)   return false;   
  if(v1.lastDaughterId  != v2.lastDaughterId)   return false;   
  if(v1.pt              != v2.pt)               return false;   
  if(v1.eta             != v2.eta)             return false;   
  if(v1.phi             != v2.phi)            return false;   
  if(v1.m               != v2.m)            return false;   
  if(v1.e               != v2.e)            return false;   
  if(v1.vertexZ         != v2.vertexZ)  return false;   
  return true;
  }

inline bool operator!=(const StjMCParticle& v1, const StjMCParticle& v2)
{
  return(!(v1 == v2));
}

inline bool operator==(const StjMCParticleList& v1, const StjMCParticleList& v2){
  if(v1.size() != v2.size()) return false;
  for(size_t i = 0; i < v1.size(); ++i) if(v1[i] != v2[i]) return false;
  return true;
}

inline std::ostream& operator<<(std::ostream& out, const StjMCParticle& v)
{
  out << "mcparticleId: " << v.mcparticleId << ", pt: " << v.pt << ", .... ";
  return out;
}

inline std::ostream& operator<<(std::ostream& out, const StjMCParticleList& v)
{
  out << "MCParticleList size: " << v.size();
  return out;
}

#endif // STJMCPARTICLELIST_H

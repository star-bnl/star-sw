// -*- mode: c++;-*-
// $Id: MCParticleList.h,v 1.1 2008/07/22 03:56:56 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef MCPARTICLELIST_H
#define MCPARTICLELIST_H

#include <ostream>
#include <vector>

namespace StSpinJet {

struct MCParticle {
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
  int            status; // 1: stable  2: unstable  3: incoming and parton
  double         vertexZ;
};

typedef std::vector<MCParticle> MCParticleList;

inline bool operator==(const MCParticle& v1, const MCParticle& v2)
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
  if(v1.vertexZ         != v2.vertexZ)  return false;   
  return true;
  }

inline bool operator!=(const MCParticle& v1, const MCParticle& v2)
{
  return(!(v1 == v2));
}

inline bool operator==(const MCParticleList& v1, const MCParticleList& v2){
  if(v1.size() != v2.size()) return false;
  for(size_t i = 0; i < v1.size(); ++i) if(v1[i] != v2[i]) return false;
  return true;
}

inline std::ostream& operator<<(std::ostream& out, const MCParticle& v)
{
  out << "mcparticleId: " << v.mcparticleId << ", pt: " << v.pt << ", .... ";
  return out;
}

inline std::ostream& operator<<(std::ostream& out, const MCParticleList& v)
{
  out << "MCParticleList size: " << v.size();
  return out;
}

}

#endif // MCPARTICLELIST_H

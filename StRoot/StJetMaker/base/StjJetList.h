// -*- mode: c++;-*-
// $Id: StjJetList.h,v 1.3 2008/08/02 22:43:17 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETLIST_H
#define STJJETLIST_H

#include "StjFourVecList.h"

#include <ostream>
#include <vector>

namespace StSpinJet {

struct StjJet {
  int            runNumber;
  int            eventId;
  int            jetId;
  double         pt;
  double         eta;
  double         phi;
  double         m;
  double         vertexZ;
  double         detectorEta;
  StjFourVecList    fourVecList;
};

typedef std::vector<StjJet> StjJetList;

inline bool operator==(const StjJet& v1, const StjJet& v2)
{
  if(v1.runNumber   != v2.runNumber)    return false;
  if(v1.eventId     != v2.eventId)    return false;   
  if(v1.jetId       != v2.jetId)    return false;   
  if(v1.pt          != v2.pt)      return false;   
  if(v1.eta         != v2.eta)     return false;   
  if(v1.phi         != v2.phi)      return false;   
  if(v1.m           != v2.m)          return false;   
  if(v1.fourVecList != v2.fourVecList)  return false;   
  return true;
  }

inline bool operator!=(const StjJet& v1, const StjJet& v2)
{
  return(!(v1 == v2));
}

inline bool operator==(const StjJetList& v1, const StjJetList& v2){
  if(v1.size() != v2.size()) return false;
  for(size_t i = 0; i < v1.size(); ++i) if(v1[i] != v2[i]) return false;
  return true;
}

inline std::ostream& operator<<(std::ostream& out, const StjJet& v)
{
  out << "jetId: " << v.jetId << ", pt: " << v.pt << ", .... ";
  return out;
}

inline std::ostream& operator<<(std::ostream& out, const StjJetList& v)
{
  out << "JetList size: " << v.size();
  return out;
}

}

#endif // STJJETLIST_H


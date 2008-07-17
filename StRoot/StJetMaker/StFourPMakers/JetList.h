// -*- mode: c++;-*-
// $Id: JetList.h,v 1.1 2008/07/17 17:49:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STSPINJET_JET_H
#define STSPINJET_JET_H

#include "FourVecList.h"

#include <ostream>
#include <vector>

namespace StSpinJet {

struct Jet {
  int            runNumber;
  int            eventId;
  int            jetId;
  double         pt;
  double         eta;
  double         phi;
  double         m;
  FourVecList    fourVecList;
};

typedef std::vector<Jet> JetList;

inline bool operator==(const Jet& v1, const Jet& v2)
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

inline bool operator!=(const Jet& v1, const Jet& v2)
{
  return(!(v1 == v2));
}

inline bool operator==(const JetList& v1, const JetList& v2){
  if(v1.size() != v2.size()) return false;
  for(size_t i = 0; i < v1.size(); ++i) if(v1[i] != v2[i]) return false;
  return true;
}

inline std::ostream& operator<<(std::ostream& out, const Jet& v)
{
  out << "jetId: " << v.jetId << ", pt: " << v.pt << ", .... ";
  return out;
}

inline std::ostream& operator<<(std::ostream& out, const JetList& v)
{
  out << "JetList size: " << v.size();
  return out;
}

}

#endif // STSPINJET_JET_H


// -*- mode: c++;-*-
// $Id: StjTreeIndexList.h,v 1.4 2008/08/28 04:57:08 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEXLIST_H
#define STJTREEINDEXLIST_H

#include "StjTreeIndex.h"

#include <vector>

typedef std::vector<StjTreeIndex> StjTreeIndexList;

inline bool operator==(const StjTreeIndexList& v1, const StjTreeIndexList& v2){
  if(v1.size() != v2.size()) return false;
  for(size_t i = 0; i < v1.size(); ++i) if(v1[i] != v2[i]) return false;
  return true;
}

inline std::ostream& operator<<(std::ostream& out, const StjTreeIndexList& v)
{
  out << "StjTreeIndexList size: " << v.size();
  return out;
}

#endif // STJTREEINDEXLIST_H

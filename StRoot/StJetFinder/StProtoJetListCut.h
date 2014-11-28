// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 28 May 2010
//

#ifndef ST_PROTO_JET_LIST_CUT_H
#define ST_PROTO_JET_LIST_CUT_H

class StProtoJetCut;

#include <vector>
using std::vector;

#include "StJetFinder.h"

class StProtoJetListCut {
public:
  void addCut(StProtoJetCut* c) { mCutList.push_back(c); }
  StJetFinder::JetList operator()(const StJetFinder::JetList& protojets) const;

private:
  bool cut(const StProtoJet& protojet) const;

  vector<StProtoJetCut*> mCutList;
};

#endif	// ST_PROTO_JET_LIST_CUT_H

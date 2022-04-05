// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 28 May 2010
//

#ifndef ST_PROTO_JET_CUT_PT_H
#define ST_PROTO_JET_CUT_PT_H

#include "StProtoJetCut.h"

class StProtoJetCutPt : public StProtoJetCut {
public:
  StProtoJetCutPt(double ptmin, double ptmax) : mPtMin(ptmin), mPtMax(ptmax) {}

  bool operator()(const StProtoJet& protojet) const
  {
    return protojet.pt() <= mPtMin || protojet.pt() >= mPtMax;
  }

private:
  double mPtMin;
  double mPtMax;

  ClassDef(StProtoJetCutPt,0);
};

#endif	// ST_PROTO_JET_CUT_PT_H

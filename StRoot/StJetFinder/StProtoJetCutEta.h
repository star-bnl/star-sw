// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 28 May 2010
//

#ifndef ST_PROTO_JET_CUT_ETA_H
#define ST_PROTO_JET_CUT_ETA_H

#include "StProtoJetCut.h"

class StProtoJetCutEta : public StProtoJetCut {
public:
  StProtoJetCutEta(double etamin, double etamax) : mEtaMin(etamin), mEtaMax(etamax) {}

  bool operator()(const StProtoJet& protojet) const
  {
    return protojet.eta() <= mEtaMin || protojet.eta() >= mEtaMax;
  }

private:
  double mEtaMin;
  double mEtaMax;

  ClassDef(StProtoJetCutEta,0);
};

#endif	// ST_PROTO_JET_CUT_ETA_H

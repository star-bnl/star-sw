// -*- mode: c++;-*-
// $Id: StjJetVariationTrack.h,v 1.1 2008/09/12 22:33:01 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETVARIATIONTRACK_H
#define STJJETVARIATIONTRACK_H

#include "StjJetVariation.h"

#include <TLorentzVector.h>

class StjJetVariationTrack : public StjJetVariation {

public:
  StjJetVariationTrack(double ratio = 0.1)
    : _ratio(ratio) { }
  virtual ~StjJetVariationTrack() { }

  StjJet operator()(const StjJet& jet)
  {
    StjJet ret(jet);

    TLorentzVector p4In;
    p4In.SetPtEtaPhiM(ret.pt, ret.eta, ret.phi, ret.m);

    double newPt = (1.0 + _ratio*(1 - jet.neuRt))*p4In.Pt();
    double newE = (1.0 + _ratio*(1 - jet.neuRt))*p4In.E();

    TLorentzVector p4Out;
    p4Out.SetPtEtaPhiE(newPt, p4In.Eta(), p4In.Phi(), newE);

    ret.pt = p4Out.Pt();
    ret.m = p4Out.M();

    return ret;
  }

private:

  double _ratio;

  ClassDef(StjJetVariationTrack, 1)

};

#endif // STJJETVARIATIONTRACK_H

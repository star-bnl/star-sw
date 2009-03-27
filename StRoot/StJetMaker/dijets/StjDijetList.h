// -*- mode: c++;-*-
// $Id: StjDijetList.h,v 1.4 2009/03/27 19:14:27 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETLIST_H
#define STJDIJETLIST_H

#include <TObject.h>

#include "StjJetList.h"

#include <vector>

class StjDijet : public TObject {
public:
  int            runNumber;
  int            eventId;
  int            dijetId;
  double         m;
  double         eta;
  double         costh;
  double         deta;
  double         dphi;
  double         eth;
  double         etl;
  double         vertexZ;
  double         pt3;
  double         pt4;
  double         eta3;
  double         eta4;
  double         phi3;
  double         phi4;
  double         m3;
  double         m4;
  StjJet         jet3;
  StjJet         jet4;
  double         neuRt3;
  double         neuRt4;
  StjJet         jetSameSide;
  StjJet         jetAwaySide;
  double         neuRtSameSide;
  double         neuRtAwaySide;
  ClassDef(StjDijet, 1)
};

typedef std::vector<StjDijet> StjDijetList;

#endif // STJDIJETLIST_H

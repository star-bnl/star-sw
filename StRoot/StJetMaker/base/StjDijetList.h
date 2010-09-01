// -*- mode: c++;-*-
// $Id: StjDijetList.h,v 1.1 2008/08/12 07:09:44 tai Exp $
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
  double         dphi;
  double         vertexZ;
  StjJet         jet3;
  StjJet         jet4;
  ClassDef(StjDijet, 1)
};

typedef std::vector<StjDijet> StjDijetList;

#endif // STJDIJETLIST_H

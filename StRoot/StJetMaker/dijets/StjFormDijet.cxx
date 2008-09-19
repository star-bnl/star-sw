// $Id: StjFormDijet.cxx,v 1.2 2008/09/19 23:19:17 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjFormDijet.h"

#include <TLorentzVector.h>

#include <algorithm>
#include <iostream>
#include <cmath>

ClassImp(StjFormDijet)

using namespace std;

StjDijetList StjFormDijet::operator()(StjJetList jetList)
{
  StjDijetList ret;
  if(jetList.size() < 2) return ret;
  sort(jetList.begin(), jetList.end(), pt_more());
  StjDijet dijet;
  if(jetList[0].eta >= jetList[1].eta)
    {
      dijet.jet3 = jetList[0];
      dijet.jet4 = jetList[1];
    }
  else
    {
      dijet.jet3 = jetList[1];
      dijet.jet4 = jetList[0];
    }
  dijet.runNumber = dijet.jet3.runNumber;
  dijet.eventId = dijet.jet3.eventId;
  dijet.dijetId = 1;
  dijet.vertexZ = dijet.jet3.vertexZ;
  TLorentzVector jet3;
  TLorentzVector jet4;
  jet3.SetPtEtaPhiM(dijet.jet3.pt, dijet.jet3.eta, dijet.jet3.phi, dijet.jet3.m);
  jet4.SetPtEtaPhiM(dijet.jet4.pt, dijet.jet4.eta, dijet.jet4.phi, dijet.jet4.m);
  dijet.dphi = fabs(jet3.DeltaPhi(jet4));
  TLorentzVector di = jet3 + jet4;
  dijet.m = di.M();
  dijet.eta = 0.5*(dijet.jet3.eta + dijet.jet4.eta);
  dijet.costh = tanh(0.5*(dijet.jet3.eta - dijet.jet4.eta));
  dijet.deta = dijet.jet3.eta - dijet.jet4.eta;
  dijet.neuRt3 = dijet.jet3.neuRt;
  dijet.neuRt4 = dijet.jet4.neuRt;;
  ret.push_back(dijet);
  return ret;
}


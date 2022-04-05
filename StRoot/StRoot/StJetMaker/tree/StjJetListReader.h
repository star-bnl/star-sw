// -*- mode: c++;-*-
// $Id: StjJetListReader.h,v 1.2 2009/12/03 09:57:36 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETLISTREADER_H
#define STJJETLISTREADER_H

#include "StjTreeReaderTwoTrees.h"

#include <StjJetList.h>

#include <Rtypes.h>

class StjJetListReader : public StjTreeReaderTwoTrees {

public:
  StjJetListReader(TTree *jettree, TTree *fourtree)
    : StjTreeReaderTwoTrees(jettree, fourtree) { }
  virtual ~StjJetListReader() { }

  StjJetList getList() { return _list; }

private:

  void SetBranchAddress(TTree* tree) { StjTreeReaderTwoTrees::SetBranchAddress(tree); }
  void SetBranchAddress(TTree *jettree, TTree *fourtree);

  void clearEntry();
  void readEntry();

  StjJetList _list;

  Int_t    _jet_runNumber;
  Int_t    _jet_eventId;
  Int_t    _jet_nJets;
  Int_t    _jet_jetId[128];
  Double_t _jet_pt[128];
  Double_t _jet_eta[128];
  Double_t _jet_phi[128];
  Double_t _jet_m[128];
  Double_t _jet_neuRt[128];
  Double_t _jet_vertexZ;
  Double_t _jet_detectorEta[128];

  Int_t    _four_runNumber;
  Int_t    _four_eventId;
  Int_t    _four_nFourVecs;
  Int_t    _four_fourvecId[4096];
  Int_t    _four_jetId[4096];
  Int_t    _four_type[4096];
  Int_t    _four_detectorId[4096];
  Short_t  _four_trackId[4096];
  Int_t    _four_towerId[4096];
  Int_t    _four_mcparticleId[4096];
  Double_t _four_pt[4096];
  Double_t _four_eta[4096];
  Double_t _four_phi[4096];
  Double_t _four_m[4096];
  Double_t _four_vertexZ;

  ClassDef(StjJetListReader, 1)

};

#endif // STJJETLISTREADER_H

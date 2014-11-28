// -*- mode: c++;-*-
// $Id: StjJetListWriter.h,v 1.8 2008/09/15 05:50:06 tai Exp $
#ifndef STJJETLISTWRITER_H
#define STJJETLISTWRITER_H

#include <TObject.h>

#include "StjFourVecList.h"
#include "StjJetList.h"

#include <Rtypes.h>

class TDirectory;
class TTree;


class StjJetListWriter : public TObject {

public:

  StjJetListWriter(const char* jetTreeName, const char* jetFourVecTreeName, TDirectory* file);
  virtual ~StjJetListWriter() { }

  void Fill(const StjJetList& jetList);
  void Finish();

private:

  void fillFourVecTree(int jetId, const StjFourVecList& fourVecList);

  TDirectory* _file;
  TTree* _jetTree;
  TTree* _jetFourVecTree;

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

  int _ifourvec;

  ClassDef(StjJetListWriter, 1)

};

#endif // STJJETLISTWRITER_H

// -*- mode: c++;-*-
// $Id: StJetJetListWriter.h,v 1.1 2008/07/22 19:24:09 tai Exp $
#ifndef STJETJETLISTWRITER_HH
#define STJETJETLISTWRITER_HH

#include "FourVecList.h"
#include "JetList.h"

#include <Rtypes.h>

class TDirectory;
class TTree;


class StJetJetListWriter {

public:

  StJetJetListWriter(const char* jetTreeName, const char* jetFourVecTreeName, TDirectory* file);
  virtual ~StJetJetListWriter() { }

  void Fill(const StSpinJet::JetList& jetList, const StSpinJet::FourVecList& fourVecList);
  void Finish();

  Int_t Fill();
    
private:

  void fillJetTree(const StSpinJet::JetList& jetList);
  void fillFourVecTree(const StSpinJet::JetList& jetList, const StSpinJet::FourVecList& fourVecList);

  Int_t findJetId(const StSpinJet::FourVec& four, const StSpinJet::JetList& jetList);

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
  Double_t _four_pt[4096];
  Double_t _four_eta[4096];
  Double_t _four_phi[4096];
  Double_t _four_m[4096];
};

#endif // STJETJETLISTWRITER_HH

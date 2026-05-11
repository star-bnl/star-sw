#ifndef _OutputPolicyROOT_h_
#define _OutputPolicyROOT_h_
#include "OutputPolicyNone.h"

#include <iostream>

class TTree;
class TFile;

class CRMCoptions;

class OutputPolicyROOT : public OutputPolicyNone {

 public:
  OutputPolicyROOT();
  
  void InitOutput(const CRMCoptions& cfg) override;
  void FillEvent(const CRMCoptions& cfg,const int nEvent) override;
  void CloseOutput(const CRMCoptions& cfg) override;

 protected:

  double fSigmaPairTot;
  double fSigmaPairInel;
  double fSigmaPairEl;
  double fSigmaTot;
  double fSigmaInel;
  double fSigmaEl;
  int    iSigId;

  TFile* fFile;
  TTree* fHead;
  TTree* fParticle;
};


#endif

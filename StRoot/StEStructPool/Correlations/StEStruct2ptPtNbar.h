/**********************************************************************
 *
 * $Id: StEStruct2ptPtNbar.h,v 1.1 2004/09/16 23:48:05 chunhuih Exp $
 *
 * Author: Chunhui Han adaptation of Aya's 2pt-analysis for pt
 *
 **********************************************************************
 *
 * Description:  Analysis code for 2pt-analysis of pt correlations. 
 *
 *
 ***********************************************************************/
#ifndef __STEBYE2PTPTNBAR__H
#define __STEBYE2PTPTNBAR__H

#include "StEStruct2ptCorrelations.h"

class StEStruct2ptPtNbar : public StEStruct2ptCorrelations {
 private:
  mtBins   **mnMtMt[6];
  etaBins  **mnEtaEta[6];
  phiBins  **mnPhiPhi[6];
  dphiBins **mnJtDEtaDPhi[6];

  mtBins   **maMtMt[6];
  etaBins  **maEtaEta[6];
  phiBins  **maPhiPhi[6];
  dphiBins **maJtDEtaDPhi[6];

  mtBins   **mbMtMt[6];
  etaBins  **mbEtaEta[6];
  phiBins  **mbPhiPhi[6];
  dphiBins **mbJtDEtaDPhi[6];

  mtBins   **mcMtMt[6];
  etaBins  **mcEtaEta[6];
  phiBins  **mcPhiPhi[6];
  dphiBins **mcJtDEtaDPhi[6];

  TH2F **mnHMtMt[6];
  TH2F **mnHEtaEta[6];
  TH2F **mnHPhiPhi[6];
  TH2F **mnHJtDEtaDPhi[6];

  TH2F **maHMtMt[6];
  TH2F **maHEtaEta[6];
  TH2F **maHPhiPhi[6];
  TH2F **maHJtDEtaDPhi[6];

  TH2F **mbHMtMt[6];
  TH2F **mbHEtaEta[6];
  TH2F **mbHPhiPhi[6];
  TH2F **mbHJtDEtaDPhi[6];

  TH2F **mcHMtMt[6];
  TH2F **mcHEtaEta[6];
  TH2F **mcHPhiPhi[6];
  TH2F **mcHJtDEtaDPhi[6];

  TH1F *mHpt;
 public:
  StEStruct2ptPtNbar(int mode) : StEStruct2ptCorrelations(mode) {}
  StEStruct2ptPtNbar(const char* cutFileName, int mode) : StEStruct2ptCorrelations(cutFileName,mode) {}
  ~StEStruct2ptPtNbar();

  // override base class methods
  bool doEvent(StEStructEvent *event);
  void fillHistograms();
  void writeHistograms(TFile* tf);
  void initArraysAndHistograms();
  void deleteArraysAndHistograms();
  void makePairs(StEStructEvent* e1, StEStructEvent* e2, int j);

  ClassDef(StEStruct2ptPtNbar,1)
};

#endif

/*
 * StEmcOfflineCalibrationMipAnalysis.h
 * J. Kevin Adkins, University of Kentucky
 * June 18, 2014
 */

#ifndef STAR_StEmcOfflineCalibrationMipAnalysis
#define STAR_StEmcOfflineCalibrationMipAnalysis

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include <map>
#include <set>

// ROOT classes
class TH1D;
class TH2F;
class TFile;
class TChain;
class TString;

// User defined classes
class StEmcOfflineCalibrationEvent;
class StEmcOfflineCalibrationTrack;
class StEmcOfflineCalibrationVertex;

class StEmcOfflineCalibrationMipAnalysis : public StMaker {
 private:
  StEmcOfflineCalibrationEvent *mEvent;
  StEmcOfflineCalibrationVertex *mVertex;
  StEmcOfflineCalibrationTrack *mTrack;
  
  TFile *mFile;
  TChain *mCalibChain;
  TString mOutfileName;

protected:
  Double_t pedSubAdc;
  const Int_t nTowers;
  set<Int_t> trackTowers;
  set<Int_t> excludedTowers;
  
  TH1D *towerHisto[4800];
  TH2F *mapcheck;

public: 
  StEmcOfflineCalibrationMipAnalysis(const char *name, const char* outfile, TChain *calibChain);
  virtual       ~StEmcOfflineCalibrationMipAnalysis();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEmcOfflineCalibrationMipAnalysis.h,v 1.2 2015/07/28 14:49:07 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
  ClassDef(StEmcOfflineCalibrationMipAnalysis,0);
};

#endif //STAR_StEmcOfflineCalibrationMipAnalysis

/*
 * StEmcOfflineCalibrationElectronAnalysis.h
 * J. Kevin Adkins, University of Kentucky
 * June 24, 2014
 */

#ifndef STAR_ST_EMC_OFFLINE_CALIBRATION_ELECTRON_ANALYSIS
#define STAR_ST_EMC_OFFLINE_CALIBRATION_ELECTRON_ANALYSIS

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include <map>
#include <set>

// ROOT classes
class TH1D;
class TH2F;
class TF1;
class TFile;
class TChain;
class TString;

// User defined classes
class StEmcOfflineCalibrationEvent;
class StEmcOfflineCalibrationTrack;
class StEmcOfflineCalibrationVertex;
class StEmcOfflineCalibrationCluster;
class StEmcOfflineCalibrationTrigger;

// StRoot classes
class StEmcGeom;
class StEmcADCtoEMaker;
class StBemcTables;
class StEmcDecoder;

class StEmcOfflineCalibrationElectronAnalysis : public StMaker {
 private:
  StEmcOfflineCalibrationEvent *mEvent;
  StEmcOfflineCalibrationVertex *mVertex;
  StEmcOfflineCalibrationTrack *mTrack;
  StEmcOfflineCalibrationCluster *mCluster;
  StEmcOfflineCalibrationTrigger *mBHT0, *mBHT1, *mBHT2;

  TFile *mFile, *mGeantFile;
  TChain *mCalibChain;
  TString mOutfileName, mipGainFilename, mGeantFilename;

  StEmcGeom *mEmcGeom;
  StEmcADCtoEMaker *mEmcAdcToE;
  StBemcTables *mBemcTables;
  StEmcDecoder *mEmcDecoder;

 protected:
  Double_t pi;
  Int_t nTowers, nRings, nCrates, nSlices;
  Int_t nGoodElectrons;
  map <Int_t,Int_t> towersAboveTh0, towersAboveTh1, towersAboveTh2;
  set <Int_t> includedTowers;
  set <Int_t> excludedTowers;
  Double_t mipGains[4800];
  Double_t mipError[4800];
  Double_t mipStatus[4800];
  Int_t softId;
  Double_t clusterEnergy;
  Int_t towerCrate, towerSequence, ringIndex, sliceEtaIndex;
  Float_t towerEta, towerPhi, towerTheta;
  Double_t trackEta, trackPhi, towerTrackDr, trackEnergy, trackP;
  Double_t geantScale;
  Float_t maxClusterEt;
  Int_t maxClusterId;

  TF1 *mGeantFits[20];
  TH1D *ringHisto[40], *ringHisto_Unbiased[40], *ringHisto_HT[40];
  TH1D *cratesliceHisto[30][20];

 public: 
  StEmcOfflineCalibrationElectronAnalysis(const char *name, const char* outfile, const char* mipFilename, const char* geantFilename, TChain *calibChain );
  virtual       ~StEmcOfflineCalibrationElectronAnalysis();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  // User defined functions
  Bool_t triggerFire(StEmcOfflineCalibrationTrigger*);
  Bool_t trackPointsToHT(const map<Int_t, Int_t>&, Int_t);
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEmcOfflineCalibrationElectronAnalysis.h,v 1.3 2015/07/28 14:49:07 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StEmcOfflineCalibrationElectronAnalysis,0)   //StAF chain virtual base class for Makers
};

#endif //STAR_ST_EMC_OFFLINE_CALIBRATION_ELECTRON_ANALYSIS

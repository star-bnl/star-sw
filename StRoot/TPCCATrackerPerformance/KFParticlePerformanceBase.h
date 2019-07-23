//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef ALIHLTTPCParticlePERFORMANCEBASE_H
#define ALIHLTTPCParticlePERFORMANCEBASE_H

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCPerformanceBase.h"

#include "AliHLTTPCCADef.h"
#include "AliHLTArray.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include <fstream>
#include <cstdio>
#include <map>

#include "KFPartEfficiencies.h"
#include "KFPVEfficiencies.h"

#include <string>
using std::string;

#include <iostream>
using std::ostream;
using std::istream;

class TObject;
class TParticle;
class AliHLTTPCCAMCPoint;
class AliHLTTPCCAGBTracker;
class TDirectory;
class TH1;
class TH1F;
class TH2F;
class TFile;

class KFParticle;
class TProfile;
/**
 * @class KFParticlePerformanceBase
 */
class KFParticlePerformanceBase: public AliHLTTPCPerformanceBase
{
 public:

  KFParticlePerformanceBase();
  virtual ~KFParticlePerformanceBase(){};
    
    /// Histograms
  virtual void CreateHistos(string histoDir = "", TFile* outFile = 0);

 protected:

  virtual void FillHistos();

  //for KFParticleFinder
  
  vector<KFParticle> *fParticles;
  KFParticle *fPV;
// Arrays of points, tracks, etc

// Names of files
  TString outfileName;
  TDirectory* histodir;

// efficiencies
  KFPartEfficiencies fParteff;
  KFPVEfficiencies fPVeff;
  KFPVEfficiencies fPVeffMCReconstructable;

  int fNEvents;

//histos
  static const int nFitQA = 16;
  TH1F *hFitDaughtersQA[KFPartEfficiencies::nParticles][nFitQA];
  TH1F *hFitQA[KFPartEfficiencies::nParticles][nFitQA];

  static const int nHistoPartParam = 16;
  TH1F *hPartParam[KFPartEfficiencies::nParticles][nHistoPartParam]; // mass, p, pt, Y, decay length, c*tau, chi/ndf, prob, theta, phi, z
  TH1F *hPartParamBG[KFPartEfficiencies::nParticles][nHistoPartParam];
  TH1F *hPartParamGhost[KFPartEfficiencies::nParticles][nHistoPartParam];
//   TH1F *hPartParamCorrBG[KFPartEfficiencies::nParticles][nHistoPartParam];
  TH1F *hPartParamSignal[KFPartEfficiencies::nParticles][nHistoPartParam];
  static const int nHistoPartParamQA = 3;
  TH1F *hPartParamQA[KFPartEfficiencies::nParticles][nHistoPartParamQA*2]; // residuals and pulls of these parameters

  static const int nHistoPartParam2D = 1;
  TH2F *hPartParam2D[KFPartEfficiencies::nParticles][nHistoPartParam2D]; // y-pt,
  TH2F *hPartParam2DBG[KFPartEfficiencies::nParticles][nHistoPartParam2D];
  TH2F *hPartParam2DGhost[KFPartEfficiencies::nParticles][nHistoPartParam2D];
//   TH2F *hPartParam2DCorrBG[KFPartEfficiencies::nParticles][nHistoPartParam2D];
  TH2F *hPartParam2DSignal[KFPartEfficiencies::nParticles][nHistoPartParam2D];

  static const int nPartEfficiency = 8; 
  //1 index - prticle index, 2 - index of efficiency, 3 - hystogram dependency (vs p, pt ..)
  TProfile* hPartEfficiency[KFPartEfficiencies::nParticles][3][nPartEfficiency]; //vs p, pt, y, z, c*tau, decay length, l, r
  
  static const int nHistosPV = 7;
  TH1F *hPVFitQa[2][nHistosPV];

  static const int nHistosPVParam = 14;
  TH1F *hPVParam[nHistosPVParam];
  TH1F *hPVParamGhost[nHistosPVParam];
  TH1F *hPVParamSignal[nHistosPVParam];
  TH1F *hPVParamPileup[nHistosPVParam];
  TH1F *hPVParamBG[nHistosPVParam];

  static const int nFitPVTracksQA = 12;
  TH1F *hFitPVTracksQA[nFitPVTracksQA];
  
  static const int nHistosTP = KFPartEfficiencies::nParticles + 1;
  TH1F *hTrackParameters[nHistosTP];
  
  static const int nPVefficiency = 6;
  TProfile* hPVefficiency[4][nPVefficiency];
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE

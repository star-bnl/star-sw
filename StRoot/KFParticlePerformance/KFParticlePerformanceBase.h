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

#ifndef KFParticlePERFORMANCEBASE_H
#define KFParticlePERFORMANCEBASE_H

#ifdef KFPWITHTRACKER
#include "AliHLTTPCCounters.h"

#include "AliHLTTPCPerformanceBase.h"

#include "AliHLTTPCCADef.h"
#include "AliHLTArray.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#endif

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
class KFParticlePerformanceBase
#ifdef KFPWITHTRACKER
: public AliHLTTPCPerformanceBase
#endif
{
 public:

  KFParticlePerformanceBase();
  virtual ~KFParticlePerformanceBase(){};
    
    /// Histograms
  virtual void CreateHistos(string histoDir = "", TFile* outFile = 0);
#ifndef KFPWITHTRACKER
  TDirectory* GetHistosDirectory() { return fHistoDir; }
#endif

// efficiencies
  KFPartEfficiencies fParteff;
  KFPVEfficiencies fPVeff;
  KFPVEfficiencies fPVeffMCReconstructable;
    
 protected:

  virtual void FillHistos();

  //for KFParticleFinder
  
  std::vector<KFParticle> *fParticles;
  KFParticle *fPV;
// Arrays of points, tracks, etc

// Names of files
  TString outfileName;
  TDirectory* histodir;

  int fNEvents;

//histos
  static const int nFitQA = 16;
  TH1F *hFitDaughtersQA[KFPartEfficiencies::nParticles][nFitQA];
  TH1F *hFitQA[KFPartEfficiencies::nParticles][nFitQA];

  static const int nDSToParticleQA = 7;
  TH1F *hDSToParticleQA[KFPartEfficiencies::nParticles][nDSToParticleQA];
  
  static const int nHistoPartParam = 17;
  TH1F *hPartParam[KFPartEfficiencies::nParticles][nHistoPartParam]; // mass, p, pt, Y, decay length, c*tau, chi/ndf, prob, theta, phi, z, multiplicity
  TH1F *hPartParamBG[KFPartEfficiencies::nParticles][nHistoPartParam];
  TH1F *hPartParamGhost[KFPartEfficiencies::nParticles][nHistoPartParam];
//   TH1F *hPartParamCorrBG[KFPartEfficiencies::nParticles][nHistoPartParam];
  TH1F *hPartParamSignal[KFPartEfficiencies::nParticles][nHistoPartParam];

  static const int nHistoPartParam2D = 2;
  TH2F *hPartParam2D[KFPartEfficiencies::nParticles][nHistoPartParam2D]; // y-pt, z-r
  TH2F *hPartParam2DBG[KFPartEfficiencies::nParticles][nHistoPartParam2D];
  TH2F *hPartParam2DGhost[KFPartEfficiencies::nParticles][nHistoPartParam2D];
//   TH2F *hPartParam2DCorrBG[KFPartEfficiencies::nParticles][nHistoPartParam2D];
  TH2F *hPartParam2DSignal[KFPartEfficiencies::nParticles][nHistoPartParam2D];

  static const int nPartEfficiency = 8; 
  //1 index - prticle index, 2 - index of efficiency, 3 - hystogram dependency (vs p, pt ..)
  TProfile* hPartEfficiency[KFPartEfficiencies::nParticles][3][nPartEfficiency]; //vs p, pt, y, z, c*tau, decay length, l, r
  
  static const int nHistosPV = 7;
  TH1F *hPVFitQa[2][nHistosPV];
  TH2F *hPVFitQa2D[2][2][nHistosPV-1];

  static const int nHistosPVParam = 15;
  TH1F *hPVParam[nHistosPVParam];
  TH1F *hPVParamGhost[nHistosPVParam];
  TH1F *hPVParamSignal[nHistosPVParam];
  TH1F *hPVParamPileup[nHistosPVParam];
  TH1F *hPVParamBG[nHistosPVParam];
  static const int nHistosPVParam2D = 1;
  TH2F *hPVParam2D[nHistosPVParam2D];
  
  static const int nFitPVTracksQA = 12;
  TH1F *hFitPVTracksQA[nFitPVTracksQA];
  
  static const int nHistosTP = KFPartEfficiencies::nParticles + 8;
  TH1F *hTrackParameters[nHistosTP];
  
  static const int nPVefficiency = 6;
  TProfile* hPVefficiency[4][nPVefficiency];
  
#ifndef KFPWITHTRACKER
  TDirectory *fHistoDir; //* ROOT directory with histogramm
  bool fIsHistoCreated;
  void SetHistoCreated(bool v = 1) { fIsHistoCreated = v; }
#endif

 private:
  const KFParticlePerformanceBase& operator = (const KFParticlePerformanceBase&);
  KFParticlePerformanceBase(const KFParticlePerformanceBase&);
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE

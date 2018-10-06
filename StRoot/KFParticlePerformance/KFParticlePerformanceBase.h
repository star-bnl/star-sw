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
class TH3F;
class TDirectory;

class KFParticle;
class TProfile;
class TProfile2D;
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
  virtual void CreateHistos(string histoDir = "", TDirectory* outFile = 0);
#ifndef KFPWITHTRACKER
  TDirectory* GetHistosDirectory() { return fHistoDir; }
#endif

  void DoNotStoreMCHistograms() { fStoreMCHistograms = 0; }
  
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
  bool fStoreMCHistograms;

//histos
  static const int nFitQA = 16;
  TH1F *hFitDaughtersQA[KFPartEfficiencies::nParticles][nFitQA];
  TH1F *hFitQA[KFPartEfficiencies::nParticles][nFitQA];
  TH1F *hFitQANoConstraint[KFPartEfficiencies::nParticles][nFitQA];
  TH1F *hFitQAMassConstraint[KFPartEfficiencies::nParticles][nFitQA];
  TH1F *hFitQATopoConstraint[KFPartEfficiencies::nParticles][nFitQA];
  TH1F *hFitQATopoMassConstraint[KFPartEfficiencies::nParticles][nFitQA];

  static const int nDSToParticleQA = 7;
  TH1F *hDSToParticleQA[KFPartEfficiencies::nParticles][nDSToParticleQA];
  
  static const int nHistoPartParam = 18;
  static const int nParametersSet = 8;
  TH1F *hPartParam[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam]; // mass, p, pt, rapidity, decay length, c*tau, chi/ndf, prob, theta, phi, X, Y, Z, R, L, L/dL, Mt, multiplicity
  TH1F *hPartParamPrimary[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam];
  TH1F *hPartParamPrimaryMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam];
  TH1F *hPartParamPrimaryTopo[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam];
  TH1F *hPartParamPrimaryTopoMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam];
  TH1F *hPartParamSecondary[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam];
  TH1F *hPartParamSecondaryMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam];

  static const int nHistoPartParam2D = 4;
  TH2F *hPartParam2D[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D]; // y-pt, z-r, armenteros, y-mt
  TH2F *hPartParam2DPrimary[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D];
  TH2F *hPartParam2DPrimaryMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D];
  TH2F *hPartParam2DPrimaryTopo[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D];
  TH2F *hPartParam2DPrimaryTopoMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D];
  TH2F *hPartParam2DSecondary[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D];
  TH2F *hPartParam2DSecondaryMass[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D];

  static const int nHistoPartParam3D = 5;
  TH3F *hPartParam3D[1][KFPartEfficiencies::nParticles][nHistoPartParam3D]; // y-pt-M, y-mt-M, b-pt-M, b-y-M, b-mt-M

  static const int nPartEfficiency = 9;
  //1 index - particle index, 2 - index of efficiency, 3 - histogram dependency (vs p, pt ..)
  TProfile* hPartEfficiency[KFPartEfficiencies::nParticles][3][nPartEfficiency]; //vs p, pt, y, z, c*tau, decay length, l, r, Mt
  static const int nPartEfficiency2D = 2; 
  TProfile2D* hPartEfficiency2D[KFPartEfficiencies::nParticles][3][nPartEfficiency2D]; //y-pt, y-mt
  
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

  bool IsCollectZRHistogram(int iParticle) const;
  bool IsCollect3DHistogram(int iParticle) const;
  bool IsCollectArmenteros(int iParticle) const;
  
 private:
  const KFParticlePerformanceBase& operator = (const KFParticlePerformanceBase&);
  KFParticlePerformanceBase(const KFParticlePerformanceBase&);
  
  void CreateFitHistograms(TH1F* histo[nFitQA], int iPart);
  void CreateEfficiencyHistograms(TProfile* histo[3][nPartEfficiency], TProfile2D* histo2[3][nPartEfficiency2D]);
  void CreateParameterHistograms(TH1F* histoParameters[KFPartEfficiencies::nParticles][nHistoPartParam],
                                 TH2F *histoParameters2D[KFPartEfficiencies::nParticles][nHistoPartParam2D],
                                 TH3F *histoParameters3D[KFPartEfficiencies::nParticles][nHistoPartParam3D],
                                 int iPart, bool drawZR = 0);
  void CreateParameterSubfolder(TString folderName, 
                                TH1F* histoParameters[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam],
                                TH2F* histoParameters2D[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D],
                                TH1F* histoFit[KFPartEfficiencies::nParticles][nFitQA], int iPart, bool withWrongPVHypothesis = 0);
  
  TString GetDirectoryPath();
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE

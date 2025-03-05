#ifndef STAR_StKFParticleCandidateAnalysisMaker
#define STAR_StKFParticleCandidateAnalysisMaker

#include "KFParticle.h"

#include "TString.h"
#include "TObject.h"

#include <map>

class TChain;
class TFile;
class TDirectory;
class TH1D;
class TH2D;
class TH3D;

class StKFParticleCandidateAnalysis: public TObject  {
 public:
  StKFParticleCandidateAnalysis(TString inputFile="candidates.root");
  virtual ~StKFParticleCandidateAnalysis() { if(fCandidate) delete fCandidate; }

  void Run();
  void RunMixedEvent();
  void RunRotatedBG();

  void CheckMath();

 private:
  int ParticleIndex(int pdg) {
    std::map<int, int>::iterator it;
    it=fPdgMap.find(pdg);
    if(it != fPdgMap.end()) return it->second;
    else return -1;
  }
  void WriteHistosCurFile( TObject *obj );
  
  TChain* fCandidateFileChain;
  KFParticle* fCandidate;
  
  TFile* fOutputHistoFile;
  TDirectory* fOutputHistoDir;
  static const int NParticles = 26;
  static const int NSignalSets = 3; // total, signal and BG
  static const int NSets = 8;
  static const int NHistos = 32;
  TH1D* fHistos[NParticles][NSignalSets][NSets][NHistos];
  static const int NHistos2D = 4;
  TH2D* fHistos2D[NParticles][NHistos2D];
  static const int NHistos3D = 5;
  TH3D* fHistos3D[NParticles][NHistos3D];
  
  std::map<int,int> fPdgMap;
  
  ClassDef(StKFParticleCandidateAnalysis, 1)
};

#endif // STAR_StKFParticleCandidateAnalysisMaker

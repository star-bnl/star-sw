#ifndef StKFParticlePerformanceInterface_H
#define StKFParticlePerformanceInterface_H

#include "KFMCTrack.h"

#include "TObject.h"
#include "TString.h"

#include <vector>

class KFTopoPerformance;
class KFParticleTopoReconstructor;
class TFile;

class StKFParticlePerformanceInterface: public TObject
{
 public:
  StKFParticlePerformanceInterface(const KFParticleTopoReconstructor* tr, TString outFileName="CbmKFParticleFinderQA.root");
  ~StKFParticlePerformanceInterface();
  
  void PerformanceAnalysis();
  
  void SetMCTracks(std::vector<KFMCTrack>& mcTracks) { fMCTracks = mcTracks; }
  void SetMCIndexes(std::vector<int>& mcIndex) { fMCIndexes = mcIndex;}
  
  void SetEffFileName(const TString& name) { fEfffileName = name; }
  void SetPrintEffFrequency(Int_t n = 100);
  
 private:
#if 0  
  void WriteHistosCurFile( TObject *obj );
#endif  
  //output file with histograms
  TString fOutFileName;
  TFile* fOutFile;
  TString fEfffileName;
  
  KFTopoPerformance* fKFTopoPerformance;
  std::vector<KFMCTrack> fMCTracks;
  std::vector<int> fMCIndexes;
  
  ClassDef(StKFParticlePerformanceInterface,1)
};

#endif //#ifndef StKFParticlePerformanceInterface_H

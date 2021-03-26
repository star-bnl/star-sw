#ifndef __StTPCCAInterface_h__
#define __StTPCCAInterface_h__
#include "Riostream.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"

#include "Sti/StiTrackContainer.h"
#include "TVector3.h"
#include "TH1.h"
#include "TH2.h"
#include "TSpectrum.h"
class StTPCCAInterface {
 public:
 StTPCCAInterface() : fTracker(0) {}
  virtual ~StTPCCAInterface() {
    SafeDelete(fTracker); 
    for (Int_t i = 0; i < 3; i++) {
      SafeDelete(fVertexZPlots[i]);
      SafeDelete(fVertexXYPlots[i]);
    }
    SafeDelete(fSpectrum);
  }
  virtual void SetNewEvent(); // clean and initialize before new event
  virtual void Run(); 
  virtual void RunPerformance();
  virtual AliHLTTPCCAGBTracker* GetTracker() {return fTracker;}
  virtual const TVector3 &VertexCA(Int_t i = 2) const {return *&fVertexCA[i];}
  virtual const TVector3 &VertexCAError(Int_t i = 2) const {return *&fVertexCAError[i];}
  static  TH1F*    VertexZPlot(Int_t  i = 2) {return fVertexZPlots[i];}
  static  TH2F*    VertexXYPlot(Int_t i = 2) {return fVertexXYPlots[i];}
  void FillZHist(TH1F *hist, Double_t Z, Double_t sigmaZ);
 protected:
  virtual void MakeSettings(); // fill fCaParam
  virtual void MakeHits() {cout << "Dummy StTPCCAInterface::;MakeHits is called" << endl;}
  virtual void MakeSeeds() {cout << "Dummy StTPCCAInterface::;MakeSeeds is called" << endl;}
  
  AliHLTTPCCAGBTracker     *fTracker;
  vector<int>               fIdTruth; // id of the Track, which has created CaHit
  vector<AliHLTTPCCAParam>  fCaParam;// settings for all sectors to give CATracker
  vector<AliHLTTPCCAGBHit>  fCaHits; // hits to give CATracker
  double fPreparationTime_real, fPreparationTime_cpu; // time for coping data and performance
  TVector3 fVertexCA[3]; // Estimated from CA primary vertex position, 0 -> east, 1 -> west, 2 -> total 
  TVector3 fVertexCAError[3];
  static TH1F *fVertexZPlots[3];
  static TH2F *fVertexXYPlots[3];
  static TSpectrum *fSpectrum;
};
#endif //  __StTPCCAInterface_h__

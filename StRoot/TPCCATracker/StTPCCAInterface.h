#ifndef __StTPCCAInterface_h__
#define __StTPCCAInterface_h__
#include "Riostream.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"

#include "Sti/StiTrackContainer.h"
#include "TVector3.h"
#include "TH1.h"
#include "TSpectrum.h"
class StTPCCAInterface {
 public:
 StTPCCAInterface() : fTracker(0) {}
  virtual ~StTPCCAInterface() {
    SafeDelete(fTracker); 
#ifdef  __ESTIMATE_Primary_Vertex_Z__
    for (Int_t i = 0; i < 3; i++) {
      SafeDelete(fVertexZPlots[i]);
    }
    SafeDelete(fSpectrum);
#endif /* __ESTIMATE_Primary_Vertex_Z__ */
  }
  virtual void SetNewEvent(); // clean and initialize before new event
  virtual void Run(); 
  virtual void RunPerformance();
  virtual AliHLTTPCCAGBTracker* GetTracker() {return fTracker;}
#ifdef  __ESTIMATE_Primary_Vertex_Z__
  virtual const TVector3 &VertexCA(Int_t i = 2) const {return *&fVertexCA[i];}
  virtual const TVector3 &VertexCAError(Int_t i = 2) const {return *&fVertexCAError[i];}
  static  TH1F*    VertexZPlot(Int_t i = 2) {return fVertexZPlots[i];}
  void FillZHist(TH1F *hist, Double_t Z, Double_t sigmaZ);
#endif /* __ESTIMATE_Primary_Vertex_Z__ */

 protected:
  virtual void MakeSettings(); // fill fCaParam
  virtual void MakeHits() {cout << "Dummy StTPCCAInterface::;MakeHits is called" << endl;}
  virtual void MakeSeeds() {cout << "Dummy StTPCCAInterface::;MakeSeeds is called" << endl;}
  
  AliHLTTPCCAGBTracker     *fTracker;
  vector<int>               fIdTruth; // id of the Track, which has created CaHit
  vector<AliHLTTPCCAParam>  fCaParam;// settings for all sectors to give CATracker
  vector<AliHLTTPCCAGBHit>  fCaHits; // hits to give CATracker
  double fPreparationTime_real, fPreparationTime_cpu; // time for coping data and performance
#ifdef  __ESTIMATE_Primary_Vertex_Z__
  TVector3 fVertexCA[3]; // Estimated from CA primary vertex position, 0 -> east, 1 -> west, 2 -> total 
  TVector3 fVertexCAError[3];
  static TH1F *fVertexZPlots[3];
  static TSpectrum *fSpectrum;
#endif /* __ESTIMATE_Primary_Vertex_Z__ */

  
};
#endif //  __StTPCCAInterface_h__

#ifndef __StEEmcMixQAMaker_h__
#define __StEEmcMixQAMaker_h__

#include "StMaker.h"

class StEEmcMixMaker;
class StEEmcPointMaker;
class TH1F;
class TH2F;

#include <vector>
#include "StEEmcPair.h"

class StEEmcMixQAMaker : public StMaker
{

 public:
  StEEmcMixQAMaker(const Char_t *name);
  ~StEEmcMixQAMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();

  void  Clear(Option_t *opts=""){ /* nada */ };

  void mixer(const Char_t *name, Float_t min=0., Float_t max=999.);
  void points(const Char_t *name);

  /// use combinatoric background points
  void background(){ mBackground=true; }

  Int_t maxPerSector;  // maximum number of pairs matched to sector
  Int_t maxPerEvent;   // maximum number of pairs per event
  Int_t maxPerCluster; // maximum number of points matched to the 6-18 tower cluster

  Float_t zVertexMin;
  Float_t zVertexMax;

 private:
 protected:

  Float_t mMin; // min mass for gated quantities
  Float_t mMax; // max mass for gated quantities

  Bool_t mBackground;

  TH1F *hNcandidates;

  /// Point QA in 12 sectors
  std::vector<TH2F *> hYXpair;  //! Y vs X of pi0 pair
  std::vector<TH2F *> hYXhigh;  //! Y vs X of higher E gamma
  std::vector<TH2F *> hYXlow;   //! Y vs X of lower E gamma
  std::vector<TH2F *> hE1E2;    //! E1 vs E2
  
  /// Mass spectra as a function of sector and pT
  std::vector<Float_t>               mBins;     //! Bin boundaries in pT
  std::vector< std::vector<TH1F *> > hMassR;    //! Inv mass of real pairs
  std::vector< std::vector<TH1F *> > hZggR;     //!
  std::vector< std::vector<TH1F *> > hPhiggR;   //!
  std::vector< std::vector<TH1F *> > hEnergyR;  //!
  std::vector< std::vector<TH1F *> > hZvertexR; //!


  /// integrated
  TH1F *hMassRall;
  TH1F *hZvertexRall;

  Int_t ptbin( StEEmcPair p );

  /// Verify that the pi0 candidate is the only pair of reconstructed
  /// points matching the contiguous group of towers. 
  Bool_t twoBodyCut( StEEmcPair p );

  StEEmcMixMaker   *mEEmixer;  //!
  StEEmcPointMaker *mEEpoints; //!

  ClassDef(StEEmcMixQAMaker,1);  

};

#endif

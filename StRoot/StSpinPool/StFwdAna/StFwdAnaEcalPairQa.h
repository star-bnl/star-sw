/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpose of this class is to make histograms to QA the pairs created by #StMuFcsAnaDataMakePairs

  DESCRIPTION
  The analysis module loops over all the #FcsPairCandidates in #StMuFcsAnaData::mPhPairArr and fills some histograms to understand the quality of all the #FcsPhotonCandidate pairs

  LOG
  @[June 8, 2026] > First instance where relevant functionality was copied from #StMuFcsAnaMakePairs
  @[July 1, 2026] > Changed name from StMuFcsAnaMakePairsQa to StFwdAnaEcalPairQa and modified #DoMake() to grab pairs and photon candidates by calling methods in #StFwdAnaData
*/


#ifndef STFWDANA_STFWDANAECALPAIRQA_HH
#define STFWDANA_STFWDANAECALPAIRQA_HH

#include "StFwdAnaVirtual.h"

class StFwdAnaEcalPairQa : public StFwdAnaVirtual
{
public:
  StFwdAnaEcalPairQa();
  ~StFwdAnaEcalPairQa();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StFwdAnaData* anadata);
  virtual Int_t DoMake(StFwdAnaData* anadata);

  void PaintPairEnergy(TCanvas* canv, const char* savename="test_pairenergy.png") const;
  
protected:
  TH1* mH1F_InvMassClusPairs = 0;            ///< Invariant Mass of all cluster pairs
  TH1* mH1F_InvMassPointPairs = 0;           ///< Invariant Mass of all point pairs
  
  TH1* mH2F_ClusEnergy_ph1Vph2 = 0;               ///< Histogram of two clusters used in energy used in pair reconstruction
  TH1* mH2F_PointEnergy_ph1Vph2 = 0;              ///< Histogram of two points used in energy used in pair reconstruction

  ClassDef(StFwdAnaEcalPairQa,1)
};

#endif


/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpose of this class is to make pairs of #FcsPhotonCandidate in #StMuFcsAnaData::mPhArr and store them into #StMuFcsAnaData::mPhPairArr. These pairs serve as the basline for analysis that requires reconstructing a particle by looking at its two decay particles

  DESCRIPTION
  The analysis module loops over all the #FcsPhotonCandidate in #StMuFcsAnaData::mPhArr and creates #FcsPi0Candidate and stores them into #StMuFcsAnaData::mPhPairArr. You can choose to make pairs with, clusters, points or both.

  LOG
  @[January 14, 2026] > First instance where relevant functionality was copied from #StMuFcsTreeMaker
  @[June 8, 2026] > Changed name of #FcsPi0Candidate to #FcsPairCandidate. Modified #DoMake() to make pairs with clusters, points, or both. Got rid of histograms and paint methods as this class should only be used to generate pairs and any QA related stuff should be done in the new #StMuFcsAnaMakePairsQa.
  @[June 11, 2026] > Fixed a bug where the cluster loop was still only looping over the two highest energy clusters
  @[June 17, 2026] > Changed default of created pair candidates #FcsPairCandidate::mFromPh to be -1 to be consistent with new meaning
  @[July 1, 2026] > Changed name from StMuFcsAnaMakePairs to StFwdAnaMakeEcalPairs and modified #DoMake() to only make pairs of photon candidates that are either from north or south Ecal
*/


#ifndef STFWDANA_STFWDANAMAKEECALPAIRS_HH
#define STFWDANA_STFWDANAMAKEECALPAIRS_HH

#include "StFwdAnaVirtual.h"

class StFwdAnaMakeEcalPairs : public StFwdAnaVirtual
{
public:
  StFwdAnaMakeEcalPairs();
  ~StFwdAnaMakeEcalPairs();

  void setMakeClusPairs(bool val){ mMakeClusPairs=val; }
  void setMakePointPairs(bool val){ mMakePointPairs=val; }

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StFwdAnaData* anadata);
  virtual Int_t DoMake(StFwdAnaData* anadata);

  //void PaintEpdNmipCuts(TCanvas* canv, const char* savename) const;
  
protected:
  bool mMakeClusPairs = true;        ///< Boolean to tell class to make pairs from FCS clusters
  bool mMakePointPairs = true;          ///< Boolean to tell class to make pairs from FCS points

  //@[June 8, 2026] > Not sure yet where to put this so leave this commented out for now so later can be cut and paste into appropriate class
  //static const short NEPDCUTS = 8;
  //TObjArray* mH1F_InvMassEpdCuts[2];    ///< Invariant Mass using different epd nmip cuts and all triggers or only EM triggers
  
  ClassDef(StFwdAnaMakeEcalPairs,1)
};

#endif


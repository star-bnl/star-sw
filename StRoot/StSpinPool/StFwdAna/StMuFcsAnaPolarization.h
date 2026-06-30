/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpose of this class is to process the polarization information in an event

  DESCRIPTION
  Creates histograms to store the blue and yellow polarization information and fills them

  LOG
  @[January 12, 2026] > First instance where relevant functionality was copied from #StMuFcsTreeMaker

*/


#ifndef STMUFCSANAPOLARIZATION_HH
#define STMUFCSANAPOLARIZATION_HH

#include "StMuFcsVirtualAna.h"

class StMuFcsAnaPolarization : public StMuFcsVirtualAna
{
public:
  StMuFcsAnaPolarization();
  ~StMuFcsAnaPolarization();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* data);
  virtual Int_t DoMake(StMuFcsAnaData* mufcsdata);

  void PaintPolarization(TCanvas* canv, const char* savename) const;
  
protected:
  TH1* mH1D_BluePol = 0;                ///< Distribution of Blue beam polarization in %
  TH1* mH1D_YellowPol = 0;              ///< Distribution of Yellow beam Polarization in %
  TH1* mH1D_BluePolErr = 0;             ///< Distribution of Blue beam polarization error in %
  TH1* mH1D_YellowPolErr = 0;           ///< Distribution of Yellow beam Polarization error in %
  
  ClassDef(StMuFcsAnaPolarization,1)
};

#endif


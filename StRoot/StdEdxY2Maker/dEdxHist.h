#ifndef __dEdxHits_h__
#define __dEdxHits_h__
#include "TH2.h"
#include "TH3.h"
#include "TProfile2D.h"
#include "StProbPidTraits.h"
class Hists3D {
 private:
  union {TH1 *hists[9]; TH3F *h3;}; // uncorrected
  TH3F *h3C, *h3N, *h3Ne, *h3Npi, *h3NK, *h3NP, *h3Nd;
  TProfile2D* pdX;
  Int_t fNHist;
 public:
  static Int_t NtotHist;
  Hists3D(const Char_t *Name = "SecRow3", const Char_t *Title = "<log(dEdx/Pion)>",
	  const Char_t *TitleX = "sector", const Char_t *TitleY = "row",
	  Int_t nXBins = 24, 
	  Int_t nYBins = 45,  Double_t ymin = 0, Double_t ymax = -1,
	  Int_t nZBins = 200, Double_t ZdEdxMin = -5., Double_t ZdEdxMax = 5.,
	  Double_t xmin = 0, Double_t xmax = -1, Int_t nh = NtotHist);
  virtual ~Hists3D() {}
  void    Fill(Double_t x, Double_t y, Double_t *z);
};
//________________________________________________________________________________
class Hists2D {
 public:
  TH2F *dev[KPidParticles][3]; // deviation of measurement from prediction ffitZ[hyp]
  TH2F *devT[KPidParticles][3]; // deviation of measurement from prediction ffitZ[hyp], unique
  Hists2D(const Char_t *Name = "fit");
  virtual ~Hists2D() {}
};
#endif

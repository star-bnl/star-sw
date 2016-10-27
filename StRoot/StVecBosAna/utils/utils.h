#ifndef utils_h
#define utils_h

#include <iostream>
#include <fstream>
#include <limits>
#include <map>
#include <set>
#include <stdio.h>
#include <string>
#include <utility> // for std::pair definition

#include "TCanvas.h"
#include "TEllipse.h"
#include "TF1.h"
#include "TH1.h"
#include "TH1F.h"
#include "TH2F.h"
#include "THStack.h"
#include "TGraphErrors.h"
#include "TList.h"
#include "TLorentzVector.h"
#include "TMath.h"
#include "TObjString.h"
#include "TRandom.h"
#include "TString.h"
#include "TStyle.h"
#include "TVector2.h"

#include "ValErrPair.h"


/**
 * \namespace utils
 * \author Dmitri Smirnov
 *
 * A collection of functions and routines to perform common basic operations on ROOT objects.
 */
namespace utils {

typedef std::pair<Double_t, Double_t>          XYPair;

void        Apply(TH1* h, TF1* f);
void        PrintProgress(Int_t i, Int_t iMax);
void        PrintTLorentzVector(const TLorentzVector &lv);
void        ConvertToCumulative(TH1* h, Double_t norm=0);
TH1F*       ConvertToCumulative2(const TH1* h, TH1F* hCumul=0, Bool_t sort=kTRUE);
TH1*        ConvertToProfile(const TH1* h, TH1* p=0, Bool_t weighted=kTRUE);
TH1F*       ConvertToAbs(const TH1F* h);
TH1F*       ConvertToMaxAbs(const TH1F* h1, const TH1F* h2);
TH1*        ConstructTH1C(std::string name, std::string title, TStyle *style=0,
               Int_t nbinsx=1, Double_t xlow=0, Double_t xup=1, Int_t nbinsy=1, Double_t ylow=0, Double_t yup=1,
               std::string opts="DUMMY GRIDX GRIDY");
TH1*        ConstructTH1CWithTGraphErrors(std::string name, std::string title, TStyle *style=0,
               Int_t nbinsx=1, Double_t xlow=0, Double_t xup=1, Int_t nbinsy=1, Double_t ylow=0, Double_t yup=1,
               std::string opts="DUMMY GRIDX GRIDY");
TH1*        ConstructTH1CWithTGraphErrorsMap(std::string name, std::string title,
               std::map<std::string, TStyle*> &sfx2styles,
               Int_t nbinsx=1, Double_t xlow=0, Double_t xup=1, Int_t nbinsy=1, Double_t ylow=0, Double_t yup=1,
               std::string opts="DUMMY GRIDX GRIDY");
TGraph*     ExtractTGraph(TH1 &h, std::string sfx="");
void        CopyBinContentError(const TH1* hFrom, TH1* hTo);
TH1*        CopyReversedBinContentError(const TH1* hFrom, TH1* hTo=0);
Double_t    getIntegralLimits(TH1F* h, Double_t frac, Int_t &bmin, Int_t &bmax);
TH2F*       correctFit(TH2F* h2, TF1* f1);
void        saveCanvas(TCanvas *canvas);
TList*      getFileList(TString fListName);
void        fluctuatePoisson(TH1* h, TRandom* rnd=0);
int         FindFirstBinAbove(TH1F* h, double cl);
Int_t       FindMaximumBinEx(TH1F *h, int blur_radius);
void        BinGraph(TGraphErrors* gr, TH1* h);
void        BinGraphByFirstOnly(TGraphErrors* gr, TH1* h);
void        BinGraphsByMeasId(TList* grList, TH1* h, Bool_t norm=kFALSE);
ValErrPair* FindValErrY(TGraph* gr, Double_t xval, Int_t *pindex=0);
TGraph*     SubGraph(TGraph* gr, Double_t xlo=0, Double_t xhi=0);
void        RemoveOutliers(TGraph* gr, Int_t axis=1, Float_t sigma=3);
void        RemoveOutliers(TH1* h, Float_t sigmax=3);
void        SubtractMinimum(TGraph* gr, Int_t axis=1);
void        UpdateLimits(TH1* h, Int_t axis=0, Bool_t incErr=kTRUE);
void        UpdateLimitsFromGraphs(TH1* h, Int_t axis=0, Bool_t incErr=kTRUE);
void        RebinIntegerAxis(TH1* h, Int_t axis=1);
void        MergeGraphs(TGraph *graph, TCollection *li);
void        MergeGraphs(TGraph *graph, TGraph *graphAdd);
void        AppendToGraph(TGraph *gr, Double_t x, Double_t y, Double_t xe=0, Double_t ye=0);
void        AppendToTGraph(TH1& h, Double_t x, Double_t y, Double_t xe=0, Double_t ye=0, std::string sfx="");
void        PrintNice(TH1* h, FILE *f=stdout);
void        PrintNice(TGraph* gr, FILE *f=stdout);
void        PrintNiceToFile(TH1* h, std::string fileName="");
void        PrintNiceToFile(TGraph* gr, std::string fileName="");
Double_t    GetNonEmptyMaximum(TH1* h, Double_t maxvalue=FLT_MAX);
Double_t    GetNonEmptyMinimum(TH1* h, Double_t minvalue=-FLT_MAX);
Double_t    GetNonEmptyMaximum(THStack* hs, Option_t *option);
Double_t    GetNonEmptyMinimum(THStack* hs, Option_t *option);
Double_t    GetNonEmptyFraction(const TH1* h);
TH1*        AverageIgnoreEmptyBins(const TH1* h1, const TH1* h2, TH1* h);
TH1*        Divide(TH1* h1, TH1* h2, Double_t r12=0, TH1* h=0);
Double_t    WeightedMean(Double_t *A, Double_t *dA, int NDAT);
Double_t    WeightedMeanError(Double_t *dA, int NDAT);
void        CalcWeightedMean(Double_t *A, Double_t *dA, int NDAT, Double_t &Ave, Double_t &dAve);
ValErrPair  CalcWeightedAvrgErr(const ValErrSet &valerrs);
ValErrPair  CalcWeightedAvrgErr(const ValErrPair ve1, const ValErrPair ve2);
Double_t    CalcDivisionError(Double_t x, Double_t y, Double_t dx, Double_t dy);
ValErrPair  CalcDivision(ValErrPair ve1, ValErrPair ve2, Double_t r12=0);
Double_t    QuadErrorDiv(Double_t x, Double_t y, Double_t dx, Double_t dy);
Double_t    QuadErrorSum(Double_t dx, Double_t dy);
Double_t    PackDecimal(ValErrPair ve);
ValErrPair  UnPackDecimal(Double_t d);
TEllipse*   GetEllipse(TVector2 xy1, TVector2 xy2, Double_t x1, Double_t y1, Double_t theta=0);
TEllipse*   GetErrorEllipse(const ValErrPair &p1, const ValErrPair &p2, Double_t corr=0);
void        SetXAxisIntBinsLabels(TH1* h, Int_t xmin, Int_t xmax, Float_t tfx=0.05, Int_t ny=100);
void        SetXYAxisIntBinsLabels(TH1* h, Int_t xmin, Int_t xmax, Int_t ymin, Int_t ymax, Float_t tfx=0.10);


class SystRatioFitFunctor
{
public:

   SystRatioFitFunctor() {};
   SystRatioFitFunctor(TH1D &hR) {
      for (Int_t ib=1; ib<=hR.GetNbinsX(); ++ib) {

          Double_t bc = hR.GetBinContent(ib);
          Double_t be = hR.GetBinError(ib);
          if (!be && !bc) continue; // skip empty bins
          fSet.insert(ValErrPair(bc, be));
      }
   }

   SystRatioFitFunctor(ValErrSet &set) { fSet = set; }
   ~SystRatioFitFunctor() {};

   void SetSet(ValErrSet &set) { fSet = set; }

   Double_t operator()(double *x, double *p)
   {
      Double_t chi2_sum = 0;
      Int_t    nPoints  = 0;

      ValErrSetIter iValErr = fSet.begin();

      for ( ; iValErr != fSet.end(); ++iValErr) {

          Double_t bc = iValErr->first;
          Double_t be = iValErr->second;

          if (!be && !bc) continue; // skip empty bins

          //hRatioCopy->SetBinError(ib, sqrt(be*be + dSyst*dSyst));
          Double_t chi2 = (bc - 1)*(bc - 1)/(be*be + x[0]*x[0]);
          chi2_sum += chi2;
          nPoints++;
      }

      return chi2_sum/nPoints - 1;
   };

private:

   TH1D      fHRatio;
   ValErrSet fSet;
};

}

#endif

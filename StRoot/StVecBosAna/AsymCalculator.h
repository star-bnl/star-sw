#ifndef AsymCalculator_h
#define AsymCalculator_h

#include <string>

#include "TGraphErrors.h"
#include "TH1.h"
#include "TH2.h"
#include "TMultiGraph.h"

#include "utils/ValErrPair.h"

#include "VbAnaOptions.h"
#include "Globals.h"


/**
 * Currently just a set of static methods to calculate various asymmetries from histogrmas provided by the user.
 */
class AsymCalculator
{
public:

   static void   CalcAsimAsym(TH1I &hUp, TH1I &hDown, TH1D &hAsym);
   static void   CalcAsimAsym(TH2I &h2DetCounts_up, TH2I &h2DetCounts_down, TH2D &hAsym);
   static void   CalcAsimAsymPlain(TH1I &hUp, TH1I &hDown, TH1D &hAsym);
   static void   CalcAsimAsymSqrtFormula(TH1I &hUp, TH1I &hDown, TH1D &hAsym);
   static void   FitAsimAsym(TH1D &hAsym);
   static void   FitAsimAsym(TGraph &grAsym);
   static void   FitAsimAsym(TH2D &hAsym, TH1D &hAsymAmplitude, TMultiGraph* grAsymVsPhi=0);
   static void   CombineAsimAsym(const TH1D &hAsymBlu, const TH1D &hAsymYel, TH1D &hAsymComb, bool flipZ=false);

   static ValErrPair CalcAsym(Double_t A, Double_t B, Double_t totalA=1, Double_t totalB=1);
   static ValErrPair CalcAsymSqrtFormula(Double_t A, Double_t B, Double_t C, Double_t D);

   static VbAnaOptions *sVbAnaOptions; ///< Pointer to class object with user options
   static EAsymType  sAsymType; ///< Type of the asymmetry to be calculated
   static const float sPolAverage;
};

#endif

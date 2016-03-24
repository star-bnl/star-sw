#include "AsymCalculator.h"

#include <math.h>

#include "TF1.h"
#include "TF2.h"
#include "TMath.h"
#include "TMarker.h"

#include "utils/utils.h"

using namespace std;


VbAnaOptions *AsymCalculator::sVbAnaOptions = 0;
EAsymType AsymCalculator::sAsymType = kAsymPlain;
const float AsymCalculator::sPolAverage = 0.527;
//const float AsymCalculator::sPolAverage = 0.5256;  //S.F. Dec. 8th 2014

void AsymCalculator::CalcAsimAsym(TH1I &hUp, TH1I &hDown, TH1D &hAsym)
{
   switch (sAsymType) {
   case kAsymPlain:
      CalcAsimAsymPlain(hUp, hDown, hAsym);
      break;
   case kAsymSqrtPhys:
   case kAsymSqrtGeom:
   case kAsymSqrtLumi:
      CalcAsimAsymSqrtFormula(hUp, hDown, hAsym);
      break;
   default:
      Error("CalcAsimAsym(TH1I ...)", "Unknown asymmetry type. No asymmetry will be calculated");
      break;
   }
}


/**
 * Calculates the asymmetry in bins of some observable O. The user provides 2D
 * histograms with yields binned in phi vs. O.
 */
void AsymCalculator::CalcAsimAsym(TH2I &hUp, TH2I &hDown, TH2D &hAsym)
{
   for (int iBinX=1; iBinX<=hUp.GetNbinsX(); iBinX++)
   {
      TH1I *hUpSlice = (TH1I*) hUp.ProjectionY( (string(hUp.GetName())+"_projy").c_str(), iBinX, iBinX);
      TH1I *hDwSlice = (TH1I*) hDown.ProjectionY( (string(hDown.GetName())+"_projy").c_str(), iBinX, iBinX);

      TH1D hAsymSlice("hAsymSlice", "hAsymSlice", hUpSlice->GetNbinsX(), hUpSlice->GetXaxis()->GetXmin(), hUpSlice->GetXaxis()->GetXmax());

      // Check if there are events in the histograms
      if (!hUpSlice->Integral() && !hDwSlice->Integral()) {
         Warning("CalcAsimAsym(TH2I ...)", "Both up and down spin state histograms are empty. No asymmetry will be calculated.");
         delete hUpSlice;
         delete hDwSlice;
         continue;
      }

      CalcAsimAsym(*hUpSlice, *hDwSlice, hAsymSlice);

      delete hUpSlice;
      delete hDwSlice;

      for (int iBinY=1; iBinY<=hUp.GetNbinsY(); iBinY++)
      {
         Double_t asymVal = hAsymSlice.GetBinContent(iBinY);
         Double_t asymErr = hAsymSlice.GetBinError(iBinY);

         hAsym.SetBinContent(iBinX, iBinY, asymVal);
         hAsym.SetBinError(iBinX, iBinY, asymErr);
      }
   }
}


/**
 * Takes two histograms binned in asimuthal angle phi for spin up and down.
 * Returns a hist hChAsym filled with asym values for each valid bin/channel.
 */
void AsymCalculator::CalcAsimAsymPlain(TH1I &hUp, TH1I &hDown, TH1D &hAsym)
{
   for (int iAsimBin=1; iAsimBin<=hUp.GetNbinsX(); iAsimBin++)
   {
      UInt_t totalCountsUp   = 1;
      UInt_t totalCountsDown = 1;

      //Double_t phi1   = hUp.GetBinCenter(iAsimBin);

      // Loop to get total counts (luminosity)
      //for (int iAsimBin2=1; iAsimBin2<=hUp.GetNbinsX(); iAsimBin2++)
      //{
      //   double phi2  = hUp.GetBinCenter(iAsimBin2);
      //   double dPhi  = fabs(phi1 - phi2);
      //   double phase = fmod(dPhi, M_PI);

      //   //printf("iAsimBin, iAsimBin2, phi1, phi2, phase: %d, %d, %f, %f, %f\n", iAsimBin, iAsimBin2, phi1, phi2, phase);

      //   // Calculate luminosity. This strip and ones in cross geometry are excluded.
      //   //if (!AsymCalculator::ExcludeStrip(iAsimBin-1, iAsimBin2-1))
      //   if ( phase != 0 )
      //   {
      //      totalCountsUp   += hUp.GetBinContent(iAsimBin2);
      //      totalCountsDown += hDown.GetBinContent(iAsimBin2);
      //   }
      //}

      UInt_t nCountsUp   = hUp.GetBinContent(iAsimBin);
      UInt_t nCountsDown = hDown.GetBinContent(iAsimBin);

      //printf("nCountsUp, nCountsDown, totalCountsUp, totalCountsDown: %d, %d, %d, %d\n", nCountsUp, nCountsDown, totalCountsUp, totalCountsDown);

      // Calculate raw asymmetries for the i-th bin
      if ( totalCountsUp && totalCountsDown && nCountsUp + nCountsDown )
      {
         // Calculate Asym and dAsym
         ValErrPair chAsym = CalcAsym(nCountsUp, nCountsDown, totalCountsUp, totalCountsDown);

         // Assign large error, i.e. 100% when there is no Up or Down counts
         if (nCountsUp <= 0 || nCountsDown <= 0)
            chAsym.second = 1;

         hAsym.SetBinContent(iAsimBin, chAsym.first);
         hAsym.SetBinError(iAsimBin,   chAsym.second);
      }
   }
}


/**
 * Takes two histograms binned in asimuthal angle phi with spin up and spin
 * down yields. Returns a hist hChAsym filled with asym values for each valid
 * bin/channel.
 */
void AsymCalculator::CalcAsimAsymSqrtFormula(TH1I &hUp, TH1I &hDown, TH1D &hAsym)
{
   // Check if there are events in the histograms
   if (!hUp.Integral() && !hDown.Integral()) {
      Warning("CalcAsimAsymSqrtFormula(TH1I ...)", "Both up and down state histograms are empty. No asymmetry will be calculated");
      return;
   }

   uint32_t nTotBins = hUp.GetNbinsX();
   uint32_t nHalfBins = nTotBins/2;

   for (uint32_t iAsimBin=1; iAsimBin<=nHalfBins; iAsimBin++)
   {
      uint32_t nCountsUL = hUp.GetBinContent(iAsimBin);
      uint32_t nCountsUR = hUp.GetBinContent(iAsimBin + nHalfBins);
      uint32_t nCountsDL = hDown.GetBinContent(iAsimBin);
      uint32_t nCountsDR = hDown.GetBinContent(iAsimBin + nHalfBins);

      ValErrPair asymValErr(0, -1);

      switch (sAsymType) {
      case kAsymSqrtPhys:
         asymValErr = CalcAsymSqrtFormula(nCountsUR, nCountsDL, nCountsUL, nCountsDR);
         break;
      case kAsymSqrtGeom:
         asymValErr = CalcAsymSqrtFormula(nCountsUR, nCountsDR, nCountsUL, nCountsDL);
         break;
      case kAsymSqrtLumi:
         asymValErr = CalcAsymSqrtFormula(nCountsUR, nCountsUL, nCountsDL, nCountsDR);
         break;
      default:
         Error("CalcAsimAsymSqrtFormula(TH1I ...)", "Unknown asymmetry type. No asymmetry will be calculated");
         return;
      }

      if ( asymValErr.second < 0 ) continue;

      hAsym.SetBinContent(iAsimBin, asymValErr.first);
      hAsym.SetBinError(iAsimBin,   asymValErr.second);
   }
}


/**
 * Fits the provided asymmetry histogram with a sine function. The horizontal axis of the histogram
 * is expected to cover the range of -pi to +pi. The fit function is then saved in the histogram for
 * later access.
 */
void AsymCalculator::FitAsimAsym(TH1D &hAsym)
{
   // A way to check the histogram is empty
   double integral = hAsym.Integral();

   if (integral == 0) return;

   TF1 fitFunc("fitFunc", "[0] + [1]*sin(x + [2])", -M_PI, M_PI);
   fitFunc.SetParNames("Offset", "Amplitude", "Phase");
   hAsym.Fit(&fitFunc);
}


/**
 * Fits the provided asymmetry graph with a sine function. The horizontal axis of the histogram is expected to cover the
 * range of -pi to +pi. The fit function is saved in the graph for later access.
 */
void AsymCalculator::FitAsimAsym(TGraph &grAsym)
{
   double fitRangeMax = sAsymType == kAsymPlain ? M_PI : 0;

   TF1 fitFunc("fitFunc", "[0] + [1]*sin(x + [2])", -M_PI, fitRangeMax);
   fitFunc.SetParNames("Offset", "Amplitude", "Phase");
   fitFunc.SetLineColor(grAsym.GetMarkerColor());

   if (sVbAnaOptions) {
      if (sVbAnaOptions->GetFitSinePhase() != DBL_MAX)
         fitFunc.FixParameter(2, sVbAnaOptions->GetFitSinePhase());

      if (sVbAnaOptions->GetFitSineOffset() != DBL_MAX)
         fitFunc.FixParameter(0, sVbAnaOptions->GetFitSineOffset());
   }

   grAsym.Fit(&fitFunc, "R");
}


/** */
void AsymCalculator::FitAsimAsym(TH2D &hAsym, TH1D &hAsymAmplitude, TMultiGraph* grAsymVsPhi)
{
   int iColor = 2;

   for (int iBinX=1; iBinX<=hAsym.GetNbinsX(); iBinX++)
   {
      TH1D *hAsymSlice = (TH1D*) hAsym.ProjectionY("hAsymSlice", iBinX, iBinX);

      if (hAsymSlice->Integral() == 0) continue;

      TGraphErrors* grAsymSlice = new TGraphErrors(hAsymSlice);
      string grName("gbXX");
      sprintf(&grName[0], "gb%02d", iBinX);
      grAsymSlice->SetName(grName.c_str());
      grAsymSlice->SetMarkerStyle(kFullCircle);
      grAsymSlice->SetMarkerSize(2);
      grAsymSlice->SetMarkerColor(iColor);

      FitAsimAsym(*grAsymSlice);

      if (grAsymVsPhi) {
         grAsymVsPhi->Add(grAsymSlice, "p");
      }

      TF1* fitFunc = (TF1*) grAsymSlice->GetListOfFunctions()->FindObject("fitFunc");

      if (fitFunc)
      {
         Double_t val = fitFunc->GetParameter(1);
         Double_t err = fitFunc->GetParError(1);

         hAsymAmplitude.SetBinContent(iBinX, val);
         hAsymAmplitude.SetBinError(iBinX, err);

         // Put a marker on the axis to distinguish fits in the multigraph
         TMarker* binMarker = new TMarker(hAsymAmplitude.GetBinCenter(iBinX), 0, kFullCircle);
         binMarker->SetMarkerSize(2);
         binMarker->SetMarkerColor(iColor);
         hAsymAmplitude.GetListOfFunctions()->Add(binMarker);
      }

      iColor++;
   }
}


/**
 * Takes two one-dimensional histograms with asymmetries for the two beams and properly combines
 * them. All three histograms must have the same structure, i.e. the number of bins and the axis
 * ranges are assumed to be the same.
 */
void AsymCalculator::CombineAsimAsym(const TH1D &hAsymBlu, const TH1D &hAsymYel, TH1D &hAsymComb, bool flipZ)
{
   TH1D hAsymYelModified(hAsymYel);

   if (flipZ)
      utils::CopyReversedBinContentError(&hAsymYel, &hAsymYelModified);

   // Flip the sign of the yellow beam asymmetry. Include underflow and overflow bins but don't
   // change the bin errors
   for (int iBin=0; iBin<=hAsymYelModified.GetNbinsX()+1; iBin++)
   {
      hAsymYelModified.SetBinContent( iBin, -1*hAsymYelModified.GetBinContent(iBin) );
   }

   utils::AverageIgnoreEmptyBins(&hAsymBlu, &hAsymYelModified, &hAsymComb);
}


/**
 * Calculates the asymmetry for two values A and B using the simple formula
 * asym = (A - R * B) / (A * + R * B), where R = totalA/totalB. The correction R can be applied to
 * correct for an a-priori known differences in A and B.
 */
ValErrPair AsymCalculator::CalcAsym(Double_t A, Double_t B, Double_t totalA, Double_t totalB)
{
   Double_t R = 0;

   if (totalB) R = totalA/totalB;

   Double_t denom1  = A+R*B;
   Double_t denom2  = A+B;
   Double_t asym    = 0;
   Double_t asymErr = 0;

   if ( denom1 && denom2 ) {
      asym    = (A-R*B)/denom1;
      asymErr = sqrt(4*B*B*A + 4*A*A*B)/denom2/denom2;
   }

   ValErrPair result(asym, asymErr);

   return result;
}


/**
 * Calculates the asymmetry using the so called square root formula.
 */
ValErrPair AsymCalculator::CalcAsymSqrtFormula(Double_t A, Double_t B, Double_t C, Double_t D)
{
   Double_t asym, asymErr;
   Double_t denom = sqrt(A*B) + sqrt(C*D);

   if ( denom ) {
      //asym    = (sqrt(C*D) - sqrt(A*B))/denom;
      //asymErr = sqrt(A*B*(C+D) + C*D*(A+B))/denom/denom;
      asym    = (1/sPolAverage)* (sqrt(C*D) - sqrt(A*B))/denom;
      asymErr = (1/sPolAverage)* sqrt(A*B*(C+D) + C*D*(A+B))/denom/denom;
   } else {
      asym    =  0;
      asymErr = -1; // set to -1 to indicate invalid result
   }

   ValErrPair result(asym, asymErr);

   return result;
}

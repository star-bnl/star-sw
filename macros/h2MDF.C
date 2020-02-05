// @(#)root/main:$Name:  $:$Id: h2MDF.C,v 1.3 2014/12/22 23:50:53 fisyak Exp $
/*
  rcd("TPointsBUGPRunXII19pp510P13ia_dEdx")
  .x h2MDF.C("mu",5,1,20)
  .x h2MDF.C("sigma",5,1,20)
 */
#ifndef __CINT__
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "Riostream.h"
#include "Riostream.h"
#include "TROOT.h"
#include "TFile.h"
#include "TDirectory.h"
#include "TTree.h"
#include "TLeafI.h"
#include "TH1.h"
#include "TH2.h"
#include "TF2.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TMath.h"
#include "TMultiDimFit.h"
#else
class TMultiDimFit;
#endif
//using namespace std;
TMultiDimFit* fit = 0;
TF2 *f2 = 0;
//   enum EMDFPolyType {
//       kMonomials,
//       kChebyshev,
//       kLegendre
//    };
//________________________________________________________________________________
Double_t MDFfunc(Double_t *x, Double_t *p = 0) {
  if (! fit) return 0;
  Double_t val = fit->Eval(x,p);
  cout << x[0] << "\t" << x[1] << "\t" << val << endl;
  return val;
}
TF2 *MakeTF2() {
  f2 = new TF2("FitResult",MDFfunc, 
	       fit->GetMinVariables()->operator()(0), fit->GetMaxVariables()->operator()(0),
	       fit->GetMinVariables()->operator()(1), fit->GetMaxVariables()->operator()(1));
  return f2;
}  
//________________________________________________________________________________
void h2MDF(const Char_t  *total = "mu", Int_t max=5, TMultiDimFit::EMDFPolyType type = TMultiDimFit::kMonomials, Int_t maxTerm = 10, Double_t ymax = 1){
  TH2D *total2D = (TH2D *) gDirectory->Get(total);
  if (! total2D) {
    cout << "Histogram  has not been found " << endl;
    return;
  }
  // Global data parameters 
  Int_t nVars       = 2;
  
  // make fit object and set parameters on it. 
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  fit = new TMultiDimFit(nVars, type,"vk");

  Int_t mPowers[]   = {max , max};
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(1000);
  fit->SetMaxTerms(maxTerm);
  fit->SetPowerLimit(max);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);

  // variables to hold the temporary input data 
  Double_t *x = new Double_t[nVars];
  
  // Print out the start parameters
  fit->Print("p");

  TAxis *xa = total2D->GetXaxis();
  TAxis *ya = total2D->GetYaxis();
  Int_t nx = xa->GetNbins();
  Int_t ny = ya->GetNbins();
  Int_t iy, ix;
  for (iy = 1; iy <= ny; iy++) {
    for (ix = 1; ix <= nx; ix++) {
      Double_t error = total2D->GetBinError(ix,iy);
      if (error <= 0 || error > 0.01) continue;
      Double_t value = total2D->GetBinContent(ix,iy);
      if (TMath::Abs(value) > ymax) continue;
      x[0]           = xa->GetBinCenter(ix);
      x[1]           = ya->GetBinCenter(iy);
      Double_t yy = value;
      Double_t ee = error*error;
      fit->AddRow(x,yy,ee);
    }
  }
  // Print out the statistics
  fit->Print("s");
  
  // Book histograms 
  fit->MakeHistograms();

  // Find the parameterization 
  fit->FindParameterization();
  //  fit->Fit("m"); // Miuit Fit
  // Print coefficents 
  fit->Print("rc");
  //
  // Now for the data
  //
  Int_t i, j;
  // Assignment to coefficients vector.
  cout << "  row.PolyType = \t"      << fit->GetPolyType() << ";" << endl;
  cout << "  row.NVariables = \t"    << fit->GetNVariables() << ";" << endl;
  cout << "  row.NCoefficients = \t" << fit->GetNCoefficients() << ";" << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMin[%2i] = %10.5g;", i,fit->GetMinVariables()->operator()(i));
  }
  cout << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMax[%2i] = %10.5g;", i,fit->GetMaxVariables()->operator()(i));
  }
  cout << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    for (j = 0; j < fit->GetNVariables(); j++) {
      cout << Form("  row.Power[%2i] = %2i;",i * fit->GetNVariables() + j,
		   fit->GetPowers()[fit->GetPowerIndex()[i] * fit->GetNVariables() + j]);
    }
    cout << endl;
  }
  cout << "  row.DMean = \t"          << fit->GetMeanQuantity() << ";" << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.Coefficients[%2i]    = %15.5g;", i, fit->GetCoefficients()->operator()(i));
    if ((i+1) %2 == 0) cout << endl;
  }
  if (fit->GetNCoefficients()%2) cout << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.CoefficientsRMS[%2i] = %15.5g;", i, fit->GetCoefficientsRMS()->operator()(i));
    if ((i+1) %2 == 0) cout << endl;
  }
  if (fit->GetNCoefficients()%2) cout << endl;
}
//____________________________________________________________________________
/*
  default 
Results of Parameterisation:
----------------------------
 Total reduction of square residuals    0.0004158
 Relative precision obtained:           0.4054
 Error obtained:                        0.0005092
 Multiple correlation coefficient:      0.4495
 Reduced Chi square over sample:        4.369e+09
 Maximum residual value:                0.00249
 Minimum residual value:                -0.002842
 Estimated root mean square:            0.0004123
 Maximum powers used:                   4 4 
 Function codes of candidate functions.
  1: considered,  2: too little contribution,  3: accepted.
 3333333111 1111313111 1111111131 111111
 Loop over candidates stopped because max number of terms reached

Coefficients:
-------------
   #         Value        Error   Powers
 ---------------------------------------
   0    -0.0003281     6.395e-07     0   0
   1     0.0003444     1.421e-05     2   0
   2    -0.0005894     4.141e-06     0   1
   3     0.0006203     6.497e-06     0   3
   4    -0.0003412     3.887e-06     1   0
   5      0.000161     2.843e-06     0   2
   6     0.0006582     2.574e-05     4   0
   7      0.002231     7.724e-05     3   2
   8     -0.002219     9.584e-05     3   4
   9    -9.692e-05     5.424e-06     1   1

  row.PolyType =        0;
  row.NVariables =      2;
  row.NCoefficients =   10;
  row.XMin[ 0] =      -1.45;  row.XMin[ 1] =    -3.1102;
  row.XMax[ 0] =       1.45;  row.XMax[ 1] =     3.1102;
  row.Power[ 0] =  1;  row.Power[ 1] =  1;
  row.Power[ 2] =  3;  row.Power[ 3] =  1;
  row.Power[ 4] =  1;  row.Power[ 5] =  2;
  row.Power[ 6] =  1;  row.Power[ 7] =  4;
  row.Power[ 8] =  2;  row.Power[ 9] =  1;
  row.Power[10] =  1;  row.Power[11] =  3;
  row.Power[12] =  5;  row.Power[13] =  1;
  row.Power[14] =  4;  row.Power[15] =  3;
  row.Power[16] =  4;  row.Power[17] =  5;
  row.Power[18] =  2;  row.Power[19] =  2;
  row.DMean =   0.0008517;
  row.Coefficients[ 0]    =     -0.00032808;  row.Coefficients[ 1]    =      0.00034441;
  row.Coefficients[ 2]    =     -0.00058938;  row.Coefficients[ 3]    =       0.0006203;
  row.Coefficients[ 4]    =     -0.00034119;  row.Coefficients[ 5]    =      0.00016104;
  row.Coefficients[ 6]    =      0.00065815;  row.Coefficients[ 7]    =       0.0022313;
  row.Coefficients[ 8]    =      -0.0022192;  row.Coefficients[ 9]    =     -9.6917e-05;
  row.CoefficientsRMS[ 0] =      6.3947e-07;  row.CoefficientsRMS[ 1] =      1.4207e-05;
  row.CoefficientsRMS[ 2] =      4.1407e-06;  row.CoefficientsRMS[ 3] =      6.4969e-06;
  row.CoefficientsRMS[ 4] =      3.8872e-06;  row.CoefficientsRMS[ 5] =      2.8433e-06;
  row.CoefficientsRMS[ 6] =      2.5737e-05;  row.CoefficientsRMS[ 7] =      7.7242e-05;
  row.CoefficientsRMS[ 8] =      9.5841e-05;  row.CoefficientsRMS[ 9] =      5.4237e-06;

with option = "M"


 */

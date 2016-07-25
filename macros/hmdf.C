// @(#)root/main:$Name:  $:$Id: hmdf.C,v 1.1 2013/06/24 23:40:00 fisyak Exp $
/*
  rcd("TPointsBUGPRunXII19pp510P13ia_dEdx")
  .x hmdf.C("mu",5,1,20)
  .x hmdf.C("sigma",5,1,20)
 */
#ifndef __CINT__
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "Riostream.h"
#include "TROOT.h"
#include "TFile.h"
#include "TDirectory.h"
#include "TTree.h"
#include "TLeafI.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TMath.h"
#include "TMultiDimFit.h"
#else
class TMultiDimFit;
#endif
//using namespace std;
TMultiDimFit* fit = 0;
//   enum EMDFPolyType {
//       kMonomials,
//       kChebyshev,
//       kLegendre
//    };
//________________________________________________________________________________
void hmdf(const Char_t *name="mu", Int_t max=5, Int_t type = TMultiDimFit::kMonomials, Int_t maxTerm = 10, Bool_t logx = kTRUE){
  TH1 *h = (TH1 *) gDirectory->Get(name);
  if (! h) {
    cout << "Histogram  has not been found " << endl;
    return;
  }
  // Global data parameters 
  Int_t nVars       = h->GetDimension();
  
  // make fit object and set parameters on it. 
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  fit = new TMultiDimFit(nVars, (TMultiDimFit::EMDFPolyType) type,"vk");
  Int_t mPowers[3] = {0, 0, 0};
  for (Int_t i = 0; i < nVars; i++) mPowers[i] = max;
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(1000);
  fit->SetMaxTerms(maxTerm);
  fit->SetPowerLimit(max);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);

  // variables to hold the temporary input data 
  Double_t x[3];
  
  // Print out the start parameters
  fit->Print("p");
  TAxis *axis[3] = {0,0,0};
  Int_t nxyz[3] = {1, 1, 1};
  axis[0]                 = h->GetXaxis(); nxyz[0] = axis[0]->GetNbins();
  if (nVars > 1) {axis[1] = h->GetYaxis(); nxyz[1] = axis[1]->GetNbins();}
  else           {axis[1] = 0; nxyz[1] = 1;}
  if (nVars > 2) {axis[2] = h->GetZaxis(); nxyz[2] = axis[2]->GetNbins();}
  else           {axis[2] = 0; nxyz[2] = 1;}
  Int_t iz, iy, ix;
  for (iz = 1; iz <= nxyz[2]; iz++) {
    if (axis[2]) x[2] = axis[2]->GetBinCenter(iz);
    for (iy = 1; iy <= nxyz[1]; iy++) {
      if (axis[1]) x[1] = axis[1]->GetBinCenter(iy);
      for (ix = 1; ix <= nxyz[0]; ix++) {
	Int_t bin = h->GetBin(ix,iy,iz);
	Double_t error = h->GetBinError(bin);
	if (error <= 0) continue;
	Double_t value = h->GetBinContent(bin);
	//	cout << "\t" << ix << "\t" << iy << "\t" << iz << "\t" << value << " +/- " << error << endl;
	x[0] = axis[0]->GetBinCenter(ix);
	//	cout << "xyz\t" << x[0] << "\t" << x[1] << "\t" << x[2] << endl;
	if (logx) {
	  if (x[0] <= 0) continue;
	  x[0] = TMath::Log(x[0]);
	}
	Double_t yy = value;
	Double_t ee = error*error;
	fit->AddRow(x,yy,ee);
      }
    }
  }
  // Print out the statistics
  fit->Print("s");
  
  // Book histograms 
  fit->MakeHistograms();

  // Find the parameterization 
  fit->FindParameterization();

  // Print coefficents 
  fit->Print("rc");
   //
   // Now for the data
   //
  Int_t i, j;
 // Assignment to coefficients vector.
  cout << "  row.PolyType = \t"      << fit->GetPolyType() << ";" << endl;
  cout << "  row.NVariables = \t"    <<fit->GetNVariables() << ";" << endl;
  cout << "  row.NCoefficients = \t" <<fit->GetNCoefficients() << ";" << endl;
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

#ifndef __CINT__
#include <iostream.h>
#include "TROOT.h"
#include "TApplication.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TH2.h"
#include "TSystem.h"
#include "TBrowser.h"
#include "TFile.h"
#include "TRandom.h"
#include "TMultiDimFit.h"
#else
class TH1;
class TH2;
class TMultiDimFit;
#endif
//____________________________________________________________________
Int_t MDFitHist(TH2 *hist, Int_t npx=3, Int_t npy=3) 
{
  if (!hist) return -1;
  // Open output file 
  //  TFile* output = new TFile("mdf.root", "RECREATE");

  // Global data parameters 
  const Int_t nVars       = 2;

  // make fit object and set parameters on it. 
  TMultiDimFit* fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"v");
  //  TMultiDimFit* fit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"v");

  Int_t mPowers[nVars] ;
  mPowers[0] = npx;
  mPowers[1] = npy;
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(1000);
  fit->SetMaxTerms(12);
  fit->SetPowerLimit(1);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);

  // variables to hold the temporary input data 
  Double_t d;
  Double_t e;
  Double_t *z = new Double_t[nVars];
  Double_t *x = new Double_t[nVars];
  
  // Print out the start parameters
  fit->Print("p");

  // Create training sample 
  Int_t i, j;
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  TAxis *fXaxis = hist->GetXaxis();
  TAxis *fYaxis = hist->GetYaxis();

  for (i = 1; i <= nx; i++) {
    for (j = 1; j <= ny; j++) {
      z[0] = fXaxis->GetBinCenter(i);
      z[1] = fYaxis->GetBinCenter(j);
      //      if (hist->GetCellContent(i,j) < 0) continue;
      x[0] = z[0]; //x[0] = 0.5*(z[0]+z[1]);
      x[1] = z[1]; //x[1] = 0.5*(z[0]-z[1]);
      //      d = TMath::Log(hist->GetCellContent(i,j));
      d = hist->GetCellContent(i,j);
      e = hist->GetCellError(i,j);///hist->GetCellContent(i,j);
      if (e < 1.e-7) continue;
      printf("bin: %i,%i x,y: %f %f d=%f e=%f\n",i,j,x[0],x[1],d,e);
      e *= e;
      // Add the row to the fit object
      fit->AddRow(x,d,e);
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

  // Get the min and max of variables from the training sample, used
  // for cuts in test sample. 
  Double_t *xMax = new Double_t[nVars];
  Double_t *xMin = new Double_t[nVars];
  for (i = 0; i < nVars; i++) {
    xMax[i] = (*fit->GetMaxVariables())(i);
    xMin[i] = (*fit->GetMinVariables())(i);
  }
  // Write code to file 
  fit->MakeCode(hist->GetName());

  // Write histograms to disk, and close file 
//   output->Write();
//   output->Close();
//   delete output;

  // We're done 
  delete fit;
  cout << "The END" << endl;

  return 0;
}
//____________________________________________________________________
Int_t MDFitHist(TH1 *hist, Int_t npx = 3) 
{
  if (!hist) return -1;
  // Open output file 
  //  TFile* output = new TFile("mdf.root", "RECREATE");

  // Global data parameters 
  const Int_t nVars       = 1;

  // make fit object and set parameters on it. 
  TMultiDimFit* fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"v");
  //  TMultiDimFit* fit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"v");

  Int_t mPowers[nVars];
  mPowers[0] = npx;
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(1000);
  fit->SetMaxTerms(12);
  fit->SetPowerLimit(1);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);

  // variables to hold the temporary input data 
  Double_t d;
  Double_t e;
  Double_t *z = new Double_t[nVars];
  Double_t *x = new Double_t[nVars];
  
  // Print out the start parameters
  fit->Print("p");

  // Create training sample 
  Int_t i;
  Int_t nx = hist->GetNbinsX();
  TAxis *fXaxis = hist->GetXaxis();

  for (i = 1; i <= nx; i++) {
    z[0] = fXaxis->GetBinCenter(i);
    x[0] = z[0]; //x[0] = 0.5*(z[0]+z[1]);
    d = hist->GetCellContent(i,0);
    e = hist->GetCellError(i,0);///hist->GetCellContent(i,j);
    if (e < 1.e-7) continue;
    printf("bin: %i x: %f d=%f e=%f\n",i,x[0],d,e);
    e *= e;
    // Add the row to the fit object
    fit->AddRow(x,d,e);
  }
  // Print out the statistics
  fit->Print("s");

  // Book histograms 
  fit->MakeHistograms();

  // Find the parameterization 
  fit->FindParameterization();

  // Print coefficents 
  fit->Print("rc");

//   // Get the min and max of variables from the training sample, used
//   // for cuts in test sample. 
//   Double_t *xMax = new Double_t[nVars];
//   Double_t *xMin = new Double_t[nVars];
//   for (i = 0; i < nVars; i++) {
//     xMax[i] = (*fit->GetMaxVariables())(i);
//     xMin[i] = (*fit->GetMinVariables())(i);
//   }
  // Write code to file 
  fit->MakeCode(hist->GetName());

  // Write histograms to disk, and close file 
//   output->Write();
//   output->Close();
//   delete output;

  // We're done 
  delete fit;
  cout << "The END" << endl;

  return 0;
}

#include "Riostream.h"
#include "TPrincipal.h"
#include "TMath.h"   
#include "TH1.h"
#include "TRandom.h"
using namespace std;
void dEdxPrincipal(Int_t n=45, Int_t m=1000000) 
{

  cout << "*************************************************" << endl; 
  cout << "*         Principal Component Analysis          *" << endl;
  cout << "*                                               *" << endl;
  cout << "*  Number of variables:           " << setw(4) << n 
       << "          *" << endl;
  cout << "*  Number of data points:         " << setw(8) << m
       << "      *" << endl;
  cout << "*                                               *" << endl;
  cout << "*************************************************" << endl; 
  
      
  // Initilase the TPrincipal object. Use the empty string for the
  // final argument, if you don't wan't the covariance
  // matrix. Normalising the covariance matrix is a good idea if your
  // variables have different orders of magnitude. 
  TPrincipal* principal = new TPrincipal(n,"ND");
  
  // Use a pseudo-random number generator
  TRandom* random = new TRandom;
  
  // Make the m data-points
  // Make a variable to hold our data
  // Allocate memory for the data point
  //          wire spacing 0.4 cm 
  Double_t IC[7] = {0.989,  0.850,  0.100,  0.004,  0.000,  0.000,  0.000};
  Double_t OC[7] = {0.986,  0.953,  0.726,  0.223,  0.036,  0.005,  0.001};
  Double_t* data = new Double_t[n];
  Double_t xmin = -5, xmax = 5;
  TH1F *X = new TH1F("X","x - distribution (cm)", 100, xmin,xmax);
  TH1F *YI = new TH1F("YI","y - distribution (cm) Inner", 100, xmin,xmax);
  TH1F *YO = new TH1F("YO","y - distribution (cm) Outer", 100, xmin,xmax);
  for (Int_t i = 0; i < m; i++) {
    Double_t x = xmin + (xmax - xmin)*random->Rndm(-1);
    X->Fill(x);
    Int_t iGG = TMath::Nint(x/0.1); // Gating Grid wire
    Int_t iA  = TMath::Nint(x/0.4); // Anode wire
    Int_t jA = TMath::Abs(iA);
    if (jA > 6) continue;
    YI->Fill(x,IC[jA]);
    YO->Fill(x,OC[jA]);
  }
  
#if 0
  for (Int_t i = 0; i < m; i++) {

    // First we create the un-correlated, random variables, according
    // to one of three distributions 
    for (Int_t j = 0; j < n - c; j++) {
       if (j % 3 == 0)
          data[j] = random->Gaus(5,1);
       else if (j % 3 == 1)
          data[j] = random->Poisson(8);
       else
          data[j] = random->Exp(2);
    }

    // Then we create the correlated variables
    for (Int_t j = 0 ; j < c; j++) {
       data[n - c + j] = 0;
       for (Int_t k = 0; k < n - c - j; k++)
          data[n - c + j] += data[k];
    }
    
    // Finally we're ready to add this datapoint to the PCA
    principal->AddRow(data);
  }
    
  // We delete the data after use, since TPrincipal got it by now. 
  delete [] data;
  
  // Do the actual analysis
  principal->MakePrincipals();
  
  // Print out the result on
  principal->Print();

  // Test the PCA 
  principal->Test();

  // Make some histograms of the orginal, principal, residue, etc data 
  principal->MakeHistograms();
  
  // Make two functions to map between feature and pattern space 
  principal->MakeCode();

  // Start a browser, so that we may browse the histograms generated
  // above 
  TBrowser* b = new TBrowser("principalBrowser", principal);
#endif  
}

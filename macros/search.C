#include "TCanvas.h"
#include "TGraph.h"

#include <iostream>
#include <fstream>
using namespace std;

const Int_t MAXNPT = 140; /* number of points */ 
//Float_t x[MAXNPT],y[MAXNPT],z ; /* data arrays */
Float_t y[] = 
{10,10,10,10,10,20,30,50,100,50,30,20,10,10,10,10,10,10,20,30,50,100,50,30,20,10,10,10,10,10,10};
Float_t x[] = 
{0 ,1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30};
Float_t z;
search()
{
  Int_t n,npts;
  
  TCanvas *c1 = new TCanvas("c1","the fit canvas",800,600);

  // fills up the x and y reading file "
//   ifstream infile("./peak.1") ;
//   n = 0 ;
//   while ( (infile >> x[n] >> y[n]) ) { printf("%i - x: %f y: %f\n",n,x[n],y[n]); n++;} 
  npts = sizeof(x)/sizeof(Float_t);

  TH1F *hpx    = new TH1F("hpx","Peaks",npts,0,npts);
  for (Int_t j=1 ; j<=npts ; j++) //hpx->SetBinContent(j,y[j]) ;
    hpx->Fill(x[j]+0.5,y[j]);
  hpx->Draw() ;

  TSpectrum *sp1 = new TSpectrum();
  sp1->Search(hpx,1.0,"") ;
  Int_t npeaks = sp1->GetNPeaks() ;
  cout << "Peaks found " << npeaks << endl ;

  Float_t *peaks ;
  peaks = sp1->GetPositionX() ;
  for (j=0 ; j<npeaks ; j++)
    cout << "Peak at = " << peaks[j] << endl ;

  // creates a TGraph object to display the computed curves
  TGraph *gr  = new TGraph (npts, x, y);
  gr->Draw("C*") ;

  return(0) ;
  
}

/*

File peak.1 to be read by the above code:

0 10
1 10
2 10
3 10
4 10
5 20
6 30
7 50
8 100
9 50
10 30
11 20
12 10
13 10
14 10
15 10
16 10
17 10
18 20
19 30
20 50
21 100
22 50
23 30
24 20
25 10
26 10
27 10
28 10
29 10
30 10


*/

#include "Riostream.h"
#include "Rtypes.h"
#include "TString.h"
#include "TMath.h"
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TStyle.h"
#include "TGraph.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TString.h"
#include "TLegend.h"
#include "TFile.h"
#include "TNamed.h"
#include "THStack.h"
#include "StChain.h"
#include "tables/St_Survey_Table.h"
#include "TGeoMatrix.h"
#endif
TCanvas *c1 = 0;
THStack *hs[6];
TLegend *leg[6];
struct data_t {
  Int_t sector;
  Double_t x, Dx, y, Dy, z, Dz, alpha, Dalpha, beta, Dbeta, gamma, Dgamma;
  const Char_t *Comment;
  void Print() {
    cout << Form("%2i %8.2f %5.2f %8.2f %5.2f %8.2f %5.2f",sector, x, Dx, y, Dy, z, Dz)
	 << Form(" %8.2f %5.2f %8.2f %5.2f %8.2f %5.2f %s", alpha, Dalpha, beta, Dbeta, gamma, Dgamma, Comment) << endl;
  }
};
class SurveyPass_t {
 public:
  Int_t date, time;
  const Char_t *PassName;
  data_t Data[16];
  void Print() {
    cout << Form("%8i %6i %s",date,time,PassName) << endl;
    for (Int_t i = 0; i < 16; i++) {
      Data[i].Print();
    }
  }
};
const Int_t N = 24;
SurveyPass_t Passes[] = {
#if 0
  {20140101,     1460, "AuAu200LomLum 12/14", // AuAu200LomLum
   {
	{ 1,-575.08, 3.59 ,  79.02, 0.07 , 119.69, 2.78 ,  -1.15, 0.05 ,  -0.75, 0.02 ,  -1.84, 0.07 , "Average "},
	{ 2,-848.38, 3.02 ,-756.16, 1.24 , -26.09, 2.82 ,   0.30, 0.02 ,  -0.14, 0.01 ,  -0.27, 0.06 , "Average "},
	{ 3,-146.13, 0.57 ,-239.67, 2.36 , 316.06, 2.64 ,  -1.14, 0.02 ,  -1.11, 0.03 ,  -0.02, 0.06 , "Average "},
	{ 4,  91.94, 0.18 ,-323.91, 2.41 ,-1180.5, 3.61 ,  -0.69, 0.02 ,   0.24, 0.02 ,   0.50, 0.10 , "Average "},
	{ 5,-379.75, 1.30 , 367.56, 0.95 ,1465.23, 3.51 ,  -1.01, 0.05 ,   0.34, 0.02 ,   2.82, 0.13 , "Average "},
	{ 6,1130.06, 3.76 ,-142.99, 0.01 , -73.77, 2.20 ,   1.12, 0.05 ,   0.38, 0.03 ,   3.98, 0.11 , "Average "},
	{ 7, 255.09, 1.26 , 155.50, 0.58 , -49.22, 1.64 ,  -0.18, 0.02 ,   0.81, 0.01 ,   4.55, 0.10 , "Average "},
	{ 8, 463.54, 0.57 , 424.40, 2.06 , -34.31, 2.14 ,  -0.25, 0.02 ,  -0.02, 0.02 ,  -0.76, 0.10 , "Average "},
	{ 9, 134.41, 0.15 ,-189.21, 1.74 ,-471.59, 1.94 ,  -0.78, 0.01 ,  -0.38, 0.02 ,  -0.32, 0.07 , "Average "},
	{10,-740.85, 2.00 , 597.70, 1.24 ,  98.67, 2.38 ,   0.47, 0.03 ,   0.41, 0.02 ,  -1.07, 0.08 , "Average "},
	{11, -61.78, 0.22 , -88.41, 0.58 , 356.01, 1.07 ,  -0.21, 0.01 ,   0.14, 0.02 ,  -0.55, 0.02 , "Average "},
	{12, 196.61, 0.18 , 245.83, 0.56 ,-142.33, 0.88 ,  -0.42, 0.01 ,  -0.29, 0.01 ,   2.11, 0.06 , "Average "},
	{13,  94.63, 0.14 ,  99.80, 0.39 ,  68.14, 0.65 ,  -0.38, 0.01 ,  -0.12, 0.00 ,   1.63, 0.04 , "Average "},
	{14,   5.13, 0.31 ,   3.40, 0.33 ,-2103.9, 7.48 ,   0.46, 0.01 ,   0.61, 0.01 ,   0.15, 0.00 , "Average "},
	{15,   0,    0    ,   0,    0    ,   0,    0    ,   0,    0    ,   0,    0    ,   0,    0    , "Average "},
	{16,  80.55, 0.13 ,  64.76, 0.29 ,  38.02, 0.49 ,   0.32, 0.01 ,   0.23, 0.01 ,   0.20, 0.00 , "Average "},
   }
  }
  ,
  {20140104,     1112, "AuAu200LomLum Mustafa", // AuAu200LomLum
   {
	{ 1,-473.06, 5.32 ,  61.69, 0.12 , 132.61, 4.55 ,  -1.42, 0.07 ,  -0.95, 0.03 ,  -1.04, 0.11, "Average "}, 
	{ 2,-633.91, 4.06 ,-687.72, 1.90 , -21.49, 4.60 ,   0.47, 0.03 ,  -0.29, 0.02 ,  -0.41, 0.09, "Average "}, 
	{ 3,-134.17, 0.92 ,-206.81, 3.68 , 349.88, 4.44 ,  -0.98, 0.02 ,  -0.52, 0.02 ,  -0.06, 0.10, "Average "}, 
	{ 4, 115.96, 0.30 ,-324.76, 3.86 ,-936.33, 5.33 ,  -0.56, 0.03 ,  -0.08, 0.04 ,  -0.88, 0.13, "Average "}, 
	{ 5,-330.60, 2.01 , 310.40, 1.48 ,1439.49, 5.80 ,   0.48, 0.03 ,   0.55, 0.02 ,   0.07, 0.12, "Average "}, 
	{ 6,1014.36, 5.46 , -95.57, 0.11 , -96.64, 3.58 ,  -0.13, 0.07 ,  -0.65, 0.02 ,   0.06, 0.13, "Average "}, 
	{ 7, 237.21, 2.01 , 133.75, 0.93 , -59.70, 2.75 ,  -0.13, 0.02 ,   0.41, 0.02 ,  -2.12, 0.10, "Average "}, 
	{ 8, 435.45, 0.89 , 396.68, 3.29 , -48.50, 3.52 ,   1.11, 0.02 ,  -0.09, 0.03 ,  -2.00, 0.14, "Average "}, 
	{ 9, 138.98, 0.24 ,-215.98, 2.76 ,-488.40, 3.24 ,  -0.38, 0.02 ,  -0.03, 0.02 ,  -1.66, 0.08, "Average "}, 
	{10,-573.38, 2.97 , 543.10, 1.95 ,  70.70, 3.88 ,  -0.25, 0.02 ,  -0.46, 0.02 ,  -1.70, 0.11, "Average "}, 
	{11, -38.15, 0.37 , -89.32, 0.93 , 410.07, 1.84 ,  -0.35, 0.02 ,  -0.16, 0.02 ,  -1.49, 0.02, "Average "}, 
	{12, 191.31, 0.30 , 219.30, 0.90 ,-157.70, 1.46 ,  -0.19, 0.01 ,  -0.23, 0.01 ,  -1.04, 0.02, "Average "}, 
	{13, 102.16, 0.23 ,  78.18, 0.62 ,  79.72, 1.07 ,  -0.49, 0.01 ,  -0.14, 0.01 ,  -0.88, 0.04, "Average "}, 
	{14,   2.07, 0.50 ,   7.42, 0.50 ,-2069.5,12.52 ,   0.05, 0.00 ,   0.15, 0.00 ,  -0.79, 0.00, "Average "}, 
	{15,   0,    0    ,   0,    0    ,   0,    0    ,   0,    0    ,   0,    0    ,   0,    0   , "Average "},
	{16,  87.94, 0.21 ,  50.98, 0.47 ,  50.90, 0.83 ,  -0.16, 0.00 ,  -0.03, 0.01 ,  -0.84, 0.00, "Average "}, 
   }												   
  }
  ,
#endif
  {20140104,     1112, "AuAu200LomLum 0.5 GeV", // AuAu200LomLum
   {
	{ 1, -45.35, 0.33 ,   1.98, 0.03 , -18.32, 2.77 ,  -0.02, 0.00 ,  -0.03, 0.00 ,  -1.06, 0.04, "Average "}, 
	{ 2,  -1.68, 0.75 ,   9.68, 0.38 ,  -1.97, 2.84 ,  -0.04, 0.00 ,   0.04, 0.00 ,  -0.92, 0.02, "Average "}, 
	{ 3,   3.85, 0.06 ,  14.90, 0.22 ,  85.09, 0.60 ,  -0.37, 0.00 ,   0.08, 0.00 ,  -0.10, 0.03, "Average "}, 
	{ 4,  -4.22, 0.05 ,   5.58, 0.18 ,   7.78, 2.96 ,  -0.10, 0.00 ,  -0.03, 0.00 ,   0.04, 0.03, "Average "}, 
	{ 5,  -9.40, 0.49 ,  25.31, 0.51 ,  58.60, 2.94 ,   0.00, 0.00 ,  -0.07, 0.00 ,   0.70, 0.04, "Average "}, 
	{ 6,  28.78, 0.57 ,  -4.45, 0.04 ,  68.41, 2.95 ,  -0.06, 0.00 ,  -0.05, 0.00 ,   1.23, 0.05, "Average "}, 
	{ 7,   4.76, 0.10 ,   2.67, 0.06 ,  72.28, 1.68 ,  -0.09, 0.00 ,   0.04, 0.00 ,   0.41, 0.01, "Average "}, 
	{ 8,   4.92, 0.06 ,  55.78, 0.26 , 110.53, 0.78 ,  -0.05, 0.00 ,   0.07, 0.00 ,   1.63, 0.04, "Average "}, 
	{ 9,  -1.36, 0.03 ,  62.93, 0.35 ,   7.31, 1.67 ,  -0.15, 0.00 ,  -0.07, 0.00 ,   0.06, 0.00, "Average "}, 
	{10, -10.32, 0.20 ,  10.93, 0.16 ,   7.02, 3.26 ,  -0.03, 0.00 ,  -0.07, 0.00 ,   0.41, 0.01, "Average "}, 
	{11,  -0.70, 0.04 ,   5.31, 0.04 ,  26.83, 1.19 ,  -0.07, 0.01 ,  -0.12, 0.01 ,  -0.48, 0.01, "Average "}, 
	{12,  -0.47, 0.03 ,   0.27, 0.03 ,  43.12, 1.01 ,  -0.16, 0.00 ,  -0.06, 0.00 ,   0.44, 0.01, "Average "}, 
	{13,  -0.50, 0.02 ,   2.49, 0.03 ,  35.85, 0.77 ,  -0.14, 0.00 ,  -0.14, 0.00 ,   0.06, 0.00, "Average "}, 
	{14,  -1.64, 0.06 ,   5.34, 0.06 ,-1171.33,50.3 ,  -0.00, 0.00 ,   0.02, 0.00 ,   0.07, 0.00, "Average "}, 
	{15,   0,    0    ,   0,    0    ,   0,    0    ,   0,    0    ,   0,    0    ,   0,    0   , "Average "},
	{16,  -0.57, 0.02 ,   3.13, 0.02 ,  14.77, 0.92 ,  -0.01, 0.00 ,   0.01, 0.00 ,   1.36, 0.03, "Average "}, 
   }												   
  }
  ,
  {20140104,     1113, "AuAu200LomLum 1 GeV", // AuAu200LomLum
   {
	{ 1,  17.47, 0.84,   2.23, 0.07, -17.82, 8.66,   0.02, 0.00,  -0.02, 0.01,   0.10, 0.01, "Average "},
	{ 2,   1.53, 0.71,  -3.40, 0.51,   7.99, 9.17,  -0.10, 0.01,   0.06, 0.01,  -0.61, 0.02, "Average "},
	{ 3,   3.65, 0.13,  18.34, 0.40,  60.37, 1.03,  -0.42, 0.01,   0.10, 0.00,  -0.23, 0.01, "Average "},
	{ 4,  -6.26, 0.13,  11.44, 0.48,  54.14, 1.35,  -0.09, 0.01,  -0.03, 0.00,  -0.10, 0.01, "Average "},
	{ 5,  -6.75, 0.29,   2.73, 0.23,  76.13, 1.24,  -0.04, 0.00,  -0.12, 0.00,  -0.52, 0.01, "Average "},
	{ 6,   3.85, 0.41,  -4.34, 0.09,  56.84, 8.23,  -0.16, 0.00,  -0.06, 0.01,  -0.04, 0.01, "Average "},
	{ 7,   0.94, 0.23,   2.37, 0.15,  87.81, 4.96,  -0.10, 0.00,   0.08, 0.00,   0.62, 0.01, "Average "},
	{ 8,   7.48, 0.15,  21.92, 0.35, 102.05, 1.36,  -0.07, 0.01,   0.04, 0.00,   0.34, 0.01, "Average "},
	{ 9,  -1.26, 0.07,  43.93, 0.66,   7.65, 5.09,  -0.15, 0.01,  -0.06, 0.00,   0.03, 0.01, "Average "},
	{10, -17.90, 0.44,   4.45, 0.30, -32.73, 1.53,  -0.06, 0.01,  -0.08, 0.01,   0.76, 0.02, "Average "},
	{11,  -0.60, 0.10,   6.17, 0.11,  47.84, 0.53,  -0.11, 0.00,  -0.03, 0.00,  -0.41, 0.01, "Average "},
	{12,   1.81, 0.07,   2.08, 0.09,  16.23, 0.44,  -0.10, 0.00,  -0.03, 0.00,   0.18, 0.00, "Average "},
	{13,  -0.33, 0.06,   3.50, 0.07,  58.99, 0.35,  -0.11, 0.00,  -0.03, 0.00,   0.01, 0.00, "Average "},
	{14,  -1.28, 0.11, -42.50, 1.28,-117.34,38.51,  -0.03, 0.00,   0.00, 0.00,  -0.04, 0.00, "Average "},
	{15,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99, "Average "},
	{16,  -1.33, 0.06,   3.99, 0.06,  19.63, 3.32,  -0.03, 0.00,   0.00, 0.00,  -0.04, 0.00, "Average "},
   }												   
  }
  ,
  {20140104,     1114, "Global 1 GeV", // AuAu200LomLum
   {
	{ 1, -96.38, 1.31,   1.43, 0.10,  30.41,39.37,  -0.01, 0.00,   0.03, 0.01,   0.12, 0.01, "Average "},
	{ 2, -85.12, 1.64, -24.29, 1.22, -34.19,35.18,  -0.05, 0.01,   0.05, 0.01,   1.45, 0.04, "Average "},
	{ 3,  -3.13, 0.21, -85.67, 1.23,  44.20,11.98,  -0.02, 0.01,   0.01, 0.01,   0.28, 0.02, "Average "},
	{ 4,   0.52, 0.17, -55.46, 1.45,-110.43,58.76,  -0.05, 0.01,  -0.02, 0.00,  -0.15, 0.02, "Average "},
	{ 5,  57.74, 0.87,  -0.17, 0.28, -39.36,54.57,  -0.04, 0.01,   0.04, 0.01,   0.63, 0.04, "Average "},
	{ 6, -10.06, 0.93,  -5.71, 0.11,  40.62,22.57,  -0.29, 0.00,  -0.02, 0.01,  -0.10, 0.02, "Average "},
	{ 7,   1.26, 0.29,  -0.72, 0.19,  14.56,11.75,  -0.04, 0.00,  -0.01, 0.00,   1.37, 0.02, "Average "},
	{ 8,   4.31, 0.20,  70.30, 1.62,  72.82,15.55,   0.00, 0.01,   0.05, 0.00,   0.03, 0.02, "Average "},
	{ 9,  -0.04, 0.10,   1.95, 0.36, -31.83,19.73,  -0.01, 0.01,  -0.02, 0.00,   0.56, 0.01, "Average "},
	{10, -53.94, 1.44,  17.94, 0.94,   1.39,14.85,   0.01, 0.01,  -0.01, 0.01,   0.52, 0.04, "Average "},
	{11, -18.82, 0.54, -27.04, 0.49, -13.79,10.09,  -0.02, 0.00,  -0.02, 0.00,   0.00, 0.01, "Average "},
	{12,   6.36, 0.45,  -2.22, 0.10,  15.20, 6.91,  -0.01, 0.00,  -0.01, 0.00,   0.25, 0.01, "Average "},
	{13,  -0.63, 0.08,  -0.44, 0.08,   2.97, 5.70,  -0.02, 0.00,  -0.00, 0.00,   0.26, 0.01, "Average "},
	{14, -29.59, 0.34, -29.00, 0.34,-855.16,184.7,  -0.01, 0.00,   0.01, 0.00,   0.56, 0.00, "Average "},
	{15,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99, "Average "},
	{16,  -0.71, 0.08,  29.14, 0.17,  30.47, 2.95,  -0.01, 0.00,   0.01, 0.00,   0.48, 0.00, "Average "},
   }												   
  }
};
const  Int_t NP = sizeof(Passes)/sizeof(SurveyPass_t);
//________________________________________________________________________________
void HftG(const Char_t *opt = "") {
  gStyle->SetMarkerStyle(20);
  gStyle->SetOptStat(0);
  cout << "NP \t" << NP << endl;
  Int_t NH = NP;
  if (NH == 2) NH++; // make average if we have only FF + RF
  TH1D ***dath = new TH1D**[NH]; 
  for (Int_t p = 0; p < NH; p++) {dath[p] = new TH1D*[6]; memset(dath[p],0, 6*sizeof(TH1D*));}
  const Char_t *names[6] = {" #Deltax"," #Deltay"," #Deltaz"," #Delta #alpha"," #Delta #beta"," #Delta #gamma"};
  const Char_t *nameK[6] = {"Dx","Dy","Dz","Da",     "Db",    "Dg"};
  TString Opt(opt);
  for (Int_t i = 0; i < 6; i++) {
    hs[i] = new THStack(nameK[i],names[i]);
    Double_t ymin =  1e10;
    Double_t ymax = -1e10;
    TString Name;
    TString Title;
    if (! i)     leg[i] = new TLegend(0.10,0.65,0.30,0.90);
    else         leg[i] = 0;
    TString same("e");
    Int_t color = 1;
    TH1::SetDefaultSumw2(kTRUE);
    for (Int_t k = 0; k < NP; k++) {
      if (k < NP) {
	if (i == 0 && k < NP) Passes[k].Print();
	Name = Form("%s%s",Passes[k].PassName,nameK[i]);
	if (Opt != "" && ! Name.Contains(Opt,TString::kIgnoreCase)) continue;
	Title = Form("Alignment fit for  %s %s",names[i],Passes[k].PassName);
      } else { // Average
	Name = Form("%s%s%s",Passes[0].PassName,Passes[1].PassName,nameK[i]);
	if (Opt != "" && ! Name.Contains(Opt,TString::kIgnoreCase)) continue;
	Title = Form("Alignment fit for %s sum %s %s",names[i],Passes[0].PassName,Passes[1].PassName);
	
      }
      //      cout << Name.Data() << "\t" << Title.Data() << "\ti\t" << i << "\tk\t" << k << endl;
      dath[k][i] = (TH1D *) gDirectory->Get(Name);
      if (dath[k][i]) delete dath[k][i];
      
      dath[k][i] = new TH1D(Name,Title, 16, 0.5, 16.5);
      //      cout << "Create: " << dath[k][i]->GetName() << "\t" << dath[k][i]->GetTitle() << endl;
      dath[k][i]->SetMarkerColor(color);
      dath[k][i]->SetLineColor(color);
      color++;
      dath[k][i]->SetXTitle("1-10:PxlSec,11-12:pxlhalfs,13:pxl,14:Ist,15:Sst,16:Hft");
      if (i < 3) dath[k][i]->SetYTitle(Form("%s (#mum)",names[i]));
      else       dath[k][i]->SetYTitle(Form("%s (mrad)",names[i]));
      for (Int_t l = 0; l < 16; l++) {
	Int_t secs;
	Double_t val, err;
	if (k < NP) {
	  Double_t *X = &Passes[k].Data[l].x;
	  secs = Passes[k].Data[l].sector;
	  if (X[2*i+1] >= 0 /* && X[2*i+1] < 99 */) {
	    val = X[2*i];
	    err = X[2*i+1];
	  } else {continue;}
	} else {
	  Double_t *X0 = &Passes[0].Data[l].x;
	  Double_t *X1 = &Passes[1].Data[l].x;
	  secs = Passes[0].Data[l].sector;
	  if (X0[2*i+1] >= 0 /* && X0[2*i+1] < 99 */ &&
	      X1[2*i+1] >= 0 /* && X1[2*i+1] < 99 */) {
	    val = 0.5*(X0[2*i] + X1[2*i]);
	    dath[k][i]->SetBinContent(secs,val);
	    err = TMath::Sqrt(X0[2*i+1]*X0[2*i+1]+X1[2*i+1]*X1[2*i+1])/2;
	  } else {continue;}
	} 
	if (err < 0.001) err = 0.001;
	dath[k][i]->SetBinContent(secs,val);
	dath[k][i]->SetBinError(secs,err);
	if (ymin > val - err) ymin = val - err;
	if (ymax < val + err) ymax = val + err;
      }
      hs[i]->Add(dath[k][i]);
      if (leg[i]) {
	if (k < NP) leg[i]->AddEntry(dath[k][i],Passes[k].PassName);
	else        leg[i]->AddEntry(dath[k][i],"sum");
      }
    }
  }
  c1 = new TCanvas("IO","Tpc Outer to Inner alignment parameters",1200,800);
  c1->Divide(3,2);
  for (Int_t i = 0; i < 6; i++) {
    c1->cd(i+1);
    if (! hs[i]) continue;
    TString same("e");
    Double_t ymax = hs[i]->GetMaximum("nostack");
    Double_t ymin = hs[i]->GetMinimum("nostack");
    TList *list = hs[i]->GetHists();
    TIter next(list);
    TH1 *h = 0;
    while ((h = (TH1*) next())) {
      h->GetYaxis()->SetTitleOffset(1.4);
      if (same == "e") {
	if (ymax > 0)     h->SetMaximum(1.1*ymax);
	else              h->SetMaximum(0.9*ymax);
	if (ymin < 0)     h->SetMinimum(1.1*ymin);
	else              h->SetMinimum(0.9*ymin);
      }
      TString hName(h->GetName());
      if (hName.BeginsWith("db",TString::kIgnoreCase)) h->Draw("same");
      else                                             h->Draw(same);
      same = "same";
    }
    if (leg[i]) leg[i]->Draw();
  }
  c1->Update();

  
}

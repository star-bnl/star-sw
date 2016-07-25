#include "TMinuit.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TMath.h"
#include "TLegend.h"
#include "Names.h"
TCanvas *c1 = 0;
struct dEdxPoint_t {
  Char_t  *name;
  Int_t    type;
  Int_t    bin;
  Int_t    NPoint;
  Double_t x;
  Double_t p;
  Double_t z; // correction wrt Manuel
  Double_t y;
  Double_t dy;
  Double_t chisq;
};
Double_t ParsFit[8] = {
   7.25410e-01, //     1  Scale       
   1.56806e-06, //     2  Tmin        
   4.61027e-07, //     3  TminE       
  -2.78686e-07, //     4  A0          
   5.04261e-02, //     5  B1          
  -2.43081e+00, //     6  D0          
  -9.12143e+00, //     7  D1          
  -1.14640e+00  //     8  D2          
};
//#define  NOELECTRONS
//#include "fitPars0.h"
//#include "fitPars2.h"
//#include "FitPars3.h"
//#include "FitParsG.h"
//#include "FitParsGGG.h"
//#include "FitParsG5.h"
//#include "FitPars122.h"
#include "FitParsY.h"
const Int_t N = sizeof(dEdxZ)/sizeof(dEdxPoint_t);
//______________________________________________________________________________
void yFit()
{
  Double_t Z[20],dZ[20];
#if 0
  TMinuit *gMinuit = new TMinuit(5);  //initialize TMinuit with a maximum of 5 params
  gMinuit->SetFCN(fcn);
  
  Double_t arglist[10];
  Int_t ierflg = 0;
  //   f1->SetParNames("constant","coefficient");
  arglist[0] = 1;
  gMinuit->mnexcm("SET ERR", arglist ,1,ierflg);
  gMinuit->DefineParameter( 0, "Scale"  , 7.25410e-01,  0.01,     0, 0.0);        
  gMinuit->DefineParameter( 1, "Tmin "  , 1.56806e-06,  0.00, 1.e-8, 1.e-3); 
  gMinuit->DefineParameter( 2, "TminE"  , 4.61027e-07,  0.00, 1.e-8, 1.e-2); 
  gMinuit->DefineParameter( 3, "A0   "  ,-2.78686e-07,  0.01,     0, 0.0);  
  gMinuit->DefineParameter( 4, "B1   "  , 0.00000e+00,  0.00,     0, 0.0);  
  gMinuit->DefineParameter( 5, "D0   "  , 0.00000e+00,  0.00,     0, 0.0);  
  gMinuit->DefineParameter( 6, "D1   "  , 0.00000e+00,  0.00,     0, 0.0);  
  gMinuit->DefineParameter( 7, "D2   "  , 0.00000e+00,  0.00,     0, 0.0);  
  gMinuit->DefineParameter( 8, "g1   "  , 0.00000e+00,  0.01,     0, 0.0);  
  gMinuit->DefineParameter( 9, "g2   "  , 0.00000e+00,  0.01,     0, 0.0);  
  gMinuit->DefineParameter(10, "g3   "  , 0.00000e+00,  0.00,     0, 0.0);  
  gMinuit->Migrad();
  gMinuit->mnexcm("IMPROVE", arglist ,0,ierflg);
  gMinuit->mnexcm("HESSE", arglist ,0,ierflg);
  gMinuit->mnexcm("end", arglist ,0,ierflg);
  // Print results
  Double_t amin,edm,errdef;
  Int_t nvpar,nparx,icstat;
  gMinuit->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
  gMinuit->mnprin(3,amin);
  for (int k = 0; k < 20; k++)  gMinuit->GetParameter(k, Z[k], dZ[k]);
#endif
  if (!c1) c1 = new TCanvas();//,"gerrors2",2,10,700,500);
  c1->SetFillColor(42);
  c1->SetGrid();
  TGraphErrors *gr[13];
  Double_t xmin = 99, xmax = -99, ymin = 99, ymax = -99;
  for (int k = 0; k < NHYPS; k++) {
    gr[k] = new TGraphErrors();
    Int_t nk = 0;
    //    if (k == 2 || k == 6) continue;
    for (int i = 0; i < N; i++) {
      if (k < NHYPS && dEdxZ[i].type != k) continue;
      if (dEdxZ[i].chisq > 1.e3) continue;
      if (TMath::Abs(dEdxZ[i].z) > 0.09) continue;
      if (dEdxZ[i].dy > 0.025 || dEdxZ[i].dy < 0.0001 || dEdxZ[i].x > 2.5) continue;
      Double_t xx[3];
      xx[0] = dEdxZ[i].x;
      xx[1] = dEdxZ[i].type;
      xx[2] = dEdxZ[i].bin;
      Double_t d, ff;
#if 0
      ff  = func(xx,Z);
      d = dEdxZ[i].y - ff;
      //      xx[0] = ff;
#else
      d = dEdxZ[i].z;
#endif
      if (xx[0] < xmin) xmin = xx[0];
      if (xx[0] > xmax) xmax = xx[0];
      if (d     < ymin) ymin = d;
      if (d     > ymax) ymax = d;
      gr[k]->SetPoint(nk,xx[0],d);
      gr[k]->SetPointError(nk,0.,dEdxZ[i].dy);
      nk++;
    }
    //    printf("k: %i nk: %i\n",k,nk);
  }
  printf("x|y min|max %f %f %f %f\n",xmin,ymin,xmax,ymax);
  TH1F *frame = c1->DrawFrame(xmin-0.01,ymin-0.01,xmax+0.01,ymax+0.01);
  frame->SetYTitle("Z");
  frame->SetXTitle("log10(#beta#gamma)                  ");
  TLegend *leg = new TLegend(0.91,0.11,1.00,0.89,"");//TLegend(0.79,0.91,0.89,0.89,"");
  for (int k = 0; k < NHYPS; k++) {
    if (gr[k]->GetN()>0) {
      Int_t c = k/(NHYPS/2) + 1;
      Int_t s = k%(NHYPS/2) + 20;
      gr[k]->SetMarkerColor(c);
      gr[k]->SetMarkerStyle(s);
      gr[k]->Draw("P");
      leg->AddEntry(gr[k],Names[k],"p");
    }
  }
  leg->Draw();
}

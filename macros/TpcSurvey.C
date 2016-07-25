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
#include <assert.h>
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TGraphErrors.h"
#include "TGraph2DErrors.h"
#include "TH1.h"
#include "TProfile2D.h"
#include "TMultiGraph.h"
#include "TRandom.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TF2.h"
#include "TH2D.h"
#include "TLegend.h"
#include "TLinearFitter.h"
#include "TString.h"
#include "TRegexp.h"
#include "Ask.h"
#include "TArrayD.h"
#include "TArc.h"
#include "TEllipse.h"
#include "TGeoMatrix.h"
#include "TNtuple.h"
#include "TFile.h"
#include "HardWarePosition.C"
#endif
struct FitP_t {
  Int_t set, side, sector, io, row, ndf;
  Float_t z, dz, alpha, dalpha, beta, dbeta, gamma, dgamma, chisq, res, dres;
};
FitP_t BP;
const Char_t *vFitP = "set/I:side/I:sector/I:io/I:row/I:ndf/I:z:dz:alpha:dalpha:beta:dbeta:gamma:dgamma:chisq:res:dres";
struct SurveyData_t {
  const Char_t *system;
  const Char_t *target;
  Double_t XSurvey, YSurvey, ZSurvey; // (X, Z, Y)
  Double_t dXSurvey, dYSurvey, dZSurvey;
  const Char_t *comment;
};
#include "SurveyData_2003_2004_2013.h"
//                 MagCS       : MagCS => SurCS, survey coordinate system => magnet, index l : 0 => 2003, 1 => 2004, 2 => 2013 data
//                 TpcCS       : Tpc as Whole SurCS; TpcCS => SurCS = (MagCS => SurCS) * (survTpc == TpcCS => MagCS) =  MagCS * survTpc
//                 WheelCS[2]  : Wheel in Tpc  (0 => West, 1 => East) : (MagCS => SurCS) * (TpcCS => MagCS) * (survWheelW == WheelCS => TpcCS) = TpcCS * survWheelW
static TGeoHMatrix MagCS[3], TpcCS[3], WheelCS[2][3];
using namespace std;
TLinearFitter *lf = 0;
/*                     y  s  d */
TGraph2DErrors *graphs[2][3][3];
TGraphErrors *graphfit = 0;
TH2          *h2fit = 0;

//TMultiGraph    *mg = 0;
TProfile2D *prof2D = 0;
TProfile2D *prof2DRphi = 0;
TH1D       *zPlot = 0;
TNtuple    *FitP  = 0;
TFile      *fOut  = 0;
std::ostream&  operator<<(std::ostream& os,  const SurveyData_t& v) {
  os << Form("%10s",v.system);
  os << Form("%10s %10.3f +/- %6.3f %10.3f +/- %6.3f %10.3f +/- %6.3f %s",
	     v.target,
	     v.XSurvey,v.dXSurvey,
	     v.ZSurvey,v.dZSurvey,
	     v.YSurvey,v.dYSurvey,
	     v.comment);
  if (TMath::Abs(TMath::Abs(v.YSurvey) - 120) < 20) os << " ++++++++++++++";
  return os;
}

//________________________________________________________________________________
SurveyData_t *GetSurvey(Int_t y, TString &year, Int_t &N) {
  static Int_t yold = -1;
  static SurveyData_t *survey = 0;
  static TString Y;
  static Int_t Nold = -1;
  if (yold == y) {
    year = Y;
    N = Nold;
    assert(survey);
    return survey;
  }
  if (survey) {delete [] survey; survey = 0;}
  yold = y;
  SurveyData_t *surv;
  if (y == 2003) {
    year = "2003";
    cout << "This is the survey data taken September 15 & 16,  2003." << endl;
    N = sizeof(Survey_9_17_03)/sizeof(SurveyData_t);
    surv = &Survey_9_17_03[0];
  } else if (y == 2004) {
    year = "2004";
    cout << "This file contains the combined values for the magnet, TPC AND CONE (SSD & SVT), August 2004" << endl;
    N = sizeof(Survey_8_04)/sizeof(SurveyData_t);
    surv = &Survey_8_04[0];
  } else {
    year = "2013";
    cout << "This file contains the last survey for TPC (02/01/2013)" << endl;
    N = sizeof(Survey_2_01_13)/sizeof(SurveyData_t);
    surv = &Survey_2_01_13[0];
  }
  Y = year;
  Nold = N;
  survey = new SurveyData_t[N];
  for (Int_t i = 0; i < N; i++) {
    survey[i] = surv[i];
    survey[i].XSurvey *=  100; survey[i].dXSurvey *= 100; 
    survey[i].YSurvey *=  100; survey[i].dYSurvey *= 100; 
    survey[i].ZSurvey *=  100; survey[i].dZSurvey *= 100; 
    if (survey[i].dXSurvey < 1e-10) survey[i].dXSurvey = 0.01;
    if (survey[i].dYSurvey < 1e-10) survey[i].dYSurvey = 0.01;
    if (survey[i].dZSurvey < 1e-10) survey[i].dZSurvey = 0.01;
  }
  return survey;
}
//________________________________________________________________________________
void PrintSurvey(Int_t year = 2013) {
  TString Year;
  Int_t N;
  SurveyData_t *survey = GetSurvey(year,Year,N);
  for (Int_t i = 0; i < N; i++) {
    cout << survey[i] << endl;
  }
}
//________________________________________________________________________________
void InitMatrices() {
  Int_t years[3] = {2003,2004,2013};
  for (Int_t l = 0; l < 3; l++) {
    // Surver => Magnet
    MagCS[l] = TGeoHMatrix();
#define __Mag2Surv__
#ifdef  __Mag2Surv__
    Double_t survMagnetZabg[3][2][6] = { // Survey => Magnet
      /*x  y      z(cm)  alpha,    beta, gamma [mrad] */
      {{0., 0., 362.5027, 0, 0, 0}, // 2003,WF 1-8
       {0., 0.,-362.5545, 0, 0, 0}},// 2003,EF 1-8
      {{0., 0., 362.4907, 0, 0, 0}, // 2004,WF 1-8
       {0., 0.,-362.5550, 0, 0, 0}},// 2004,EF 1-8
      {{0., 0., 362.5011, 0, 0, 0},  // y2013w Coordinates are in STAR magent system
       {0., 0.,-362.5550, 0, 0, 0}}}; // y2013e

    Double_t anglesMagnet[3] = {(survMagnetZabg[l][0][3]+survMagnetZabg[l][1][3])/2,
				(survMagnetZabg[l][0][4]+survMagnetZabg[l][1][4])/2,
				(survMagnetZabg[l][0][5]+survMagnetZabg[l][1][5])/2};
    Double_t transMagnet[3] = {(survMagnetZabg[l][0][0]+survMagnetZabg[l][1][0])/2,
			       (survMagnetZabg[l][0][1]+survMagnetZabg[l][1][1])/2,
			       (survMagnetZabg[l][0][2]+survMagnetZabg[l][1][2])/2};
#endif /*  __Mag2Surv__ */
    cout << Form("%4i,Magnet xyz (cm) = %9.4f %9.4f %9.4f abg[mrad] = %5.2f %5.2f %5.2f",
		 years[l],transMagnet[0],transMagnet[1],transMagnet[2],anglesMagnet[0],anglesMagnet[1],anglesMagnet[2]) << endl;
    MagCS[l].RotateX(1e-3*TMath::RadToDeg()*anglesMagnet[0]);
    MagCS[l].RotateY(1e-3*TMath::RadToDeg()*anglesMagnet[1]);
    MagCS[l].RotateZ(1e-3*TMath::RadToDeg()*anglesMagnet[2]);
    MagCS[l].SetTranslation(transMagnet);
    MagCS[l].SetName(Form("MagCS_%i",l));
    MagCS[l].Print();
    // Survey => Magenet => Tpc
    TGeoHMatrix survTpc;
#define __Tpc2Mag__
#ifdef  __Tpc2Mag__  /* East Wheel for TPC  */
    Double_t Tpcxyzabg[3][6] = {
      {-0.2287 -0.0445+0.0046,  -0.1745 +0.0169-0.0014, -231.6913+0.0258,-0.04, -0.46, 0.36}, //  2003,"Tpc","^E..."
      {-0.2287 +0.0094-0.0162,  -0.1745 +0.0370+     0, -231.6945+0.0269, 0.10, -0.55, 0.52}, //  2004,"Tpc","^E..."
      {-0.2287 -0.0095-0.0001,  -0.1745 +0.0013+     0, -231.7106+0.0269, 0.10, -0.48, 0.36}};//  2013,"Tpc","^E..."
    Double_t transTpc[3]  = {Tpcxyzabg[l][0], Tpcxyzabg[l][1], Tpcxyzabg[l][2]+229.71+1.7780}; // 3-rd iteration
    survTpc.RotateX(1e-3*TMath::RadToDeg()*Tpcxyzabg[l][3]);
    survTpc.RotateY(1e-3*TMath::RadToDeg()*Tpcxyzabg[l][4]);
    survTpc.RotateZ(1e-3*TMath::RadToDeg()*Tpcxyzabg[l][5]);
    survTpc.SetTranslation(transTpc);
    cout << Form("%4i,Tpc xyz (cm) = %9.4f %9.4f %9.4f abg[mrad] = %5.2f %5.2f %5.2f",
		 years[l],transTpc[0],transTpc[1],transTpc[2],Tpcxyzabg[l][3],Tpcxyzabg[l][4],Tpcxyzabg[l][5]) << endl;
    Double_t *rotTpc = survTpc.GetRotationMatrix();
    Double_t *trTpc  = survTpc.GetTranslation();
    cout << "{0,";
    for (Int_t m = 0; m < 9; m++) {
      cout << Form("%8.5f,",rotTpc[m]);
    }
    for (Int_t m = 0; m < 3; m++) {
      cout << Form("%9.4f,",trTpc[m]);
      }
    cout << Form("0,0,0,0,0,0,\"%4i Tpc\"},",years[l]) << endl;
    survTpc.Print();
    TpcCS[l] = MagCS[l] * survTpc;
    TpcCS[l].SetName(Form("TpcCS_%i",l));
    TpcCS[l].Print();
#endif
    for (Int_t side = 0; side < 2; side++) { // West and East
      TGeoHMatrix survWheelW;
#define __Wheel2Tpc__ 
#ifdef  __Wheel2Tpc__
      const Char_t *sideName[2] = {"west","east"};
      Double_t survWheelZabg[3][2][6] = { // Survey => Tpc == average Wheel => Wheel
#if defined(__1ST_ITER__)
	{{0., 0., 231.5017,  0.13,  0.12, 0},  //  MakeGraph(2003,"Tpc","^W...")
	 {0., 0.,-231.4622, -0.00, -0.00, 0}}, //  MakeGraph(2003,"Tpc","^E...")
	{{0., 0., 231.5074,  0.09,  0.10, 0},  //  MakeGraph(2004,"Tpc","^W...")            	  
         {0., 0.,-231.4560,  0.00,  0.00, 0}}, //  MakeGraph(2004,"Tpc","^E...")
	{{0., 0., 231.4993,  0.16,  0.11, 0},  //  MakeGraph(2013,"Tpc","^W...")       
	 {0., 0.,-231.4611, -0.00, -0.00, 0}} //  MakeGraph(2013,"Tpc","^E...")
#elif defined(__2ND_ITER__)
	{{0., 0., 231.4759,  0.13,  0.12, 0},  //  MakeGraph(2003,"Tpc","^W...")
	 {0., 0.,-231.4880, -0.00, -0.00, 0}}, //  MakeGraph(2003,"Tpc","^E...")
	{{0., 0., 231.4805,  0.08,  0.10, 0},  //  MakeGraph(2004,"Tpc","^W...")            	  
         {0., 0.,-231.4829,  0.00,  0.00, 0}}, //  MakeGraph(2004,"Tpc","^E...")
	{{0., 0., 231.4724,  0.16,  0.11, 0},  //  MakeGraph(2013,"Tpc","^W...")       
	 {0., 0.,-231.4880, -0.00, -0.00, 0}} //  MakeGraph(2013,"Tpc","^E...")
#elif defined(__3RD_ITER__)
	{{ 0.0393, -0.0305, 231.4759,  0.13,  0.12, -0.35},  //  MakeGraph(2003,"Tpc","^W...")
	 { 0.    ,  0.    ,-231.4880, -0.00, -0.00,  0.04}}, //  MakeGraph(2003,"Tpc","^E...")
	{{ 0.0330, -0.0092, 231.4805,  0.08,  0.10, -0.34},  //  MakeGraph(2004,"Tpc","^W...")            	  
         { 0.    , -0.0001,-231.4829,  0.00,  0.00,  0.05}}, //  MakeGraph(2004,"Tpc","^E...")
	{{ 0.0193, -0.0133, 231.4724,  0.16,  0.11, -0.36},  //  MakeGraph(2013,"Tpc","^W...")       
	 { 0.    , 0.     ,-231.4880, -0.00, -0.00,  0.03}} //  MakeGraph(2013,"Tpc","^E...")
#elif defined(__4TH_ITER__)
	{{ 0.0393, -0.0305, 231.4759,  0.13,  0.12, -0.35-0.04},  //  MakeGraph(2003,"Tpc","^W...")
	 { 0.    ,  0.    ,-231.4880, -0.00, -0.00,  0.04     }}, //  MakeGraph(2003,"Tpc","^E...")
	{{ 0.0330, -0.0092, 231.4805,  0.08,  0.10, -0.34-0.03},  //  MakeGraph(2004,"Tpc","^W...")            	  
         { 0.    , -0.0001,-231.4829,  0.00,  0.00,  0.05-0.02}}, //  MakeGraph(2004,"Tpc","^E...")
	{{ 0.0193, -0.0133, 231.4724,  0.16,  0.11, -0.36-0.03},  //  MakeGraph(2013,"Tpc","^W...")       
	 { 0.    , 0.     ,-231.4880, -0.00, -0.00,  0.03-0.02}} //  MakeGraph(2013,"Tpc","^E...")
#elif defined(__5TH_ITER__)
	{{ 0.0393, -0.0305, 231.4759,  0.13,  0.12, -0.35-0.04},  //  MakeGraph(2003,"Tpc","^W...")
	 { 0.    ,  0.    ,-231.4880, -0.00, -0.00,  0.04     }}, //  MakeGraph(2003,"Tpc","^E...")
	{{ 0.0330, -0.0092, 231.4805,  0.08,  0.10, -0.34-0.03},  //  MakeGraph(2004,"Tpc","^W...")            	  
         { 0.    , -0.0001,-231.4829,  0.00,  0.00,  0.05-0.02+0.02}}, //  MakeGraph(2004,"Tpc","^E...")
	{{ 0.0193, -0.0133, 231.4724,  0.16,  0.11, -0.36-0.03},  //  MakeGraph(2013,"Tpc","^W...")       
	 { 0.    , 0.     ,-231.4880, -0.00, -0.00,  0.03-0.02+0.06}} //  MakeGraph(2013,"Tpc","^E...")
#elif defined(__6TH_ITER__)
	{{ 0.0393, -0.0305, 231.4759,  0.13,  0.12, -0.35-0.04},  //  MakeGraph(2003,"Tpc","^W...")
	 { 0.    ,  0.    ,-231.4880, -0.00, -0.00,  0.04     }}, //  MakeGraph(2003,"Tpc","^E...")
	{{ 0.0330, -0.0092, 231.4805,  0.08,  0.10, -0.34-0.03},  //  MakeGraph(2004,"Tpc","^W...")            	  
         { 0.    , -0.0001,-231.4829,  0.00,  0.00,  0.05-0.02+0.02}}, //  MakeGraph(2004,"Tpc","^E...")
	{{ 0.0193, -0.0133, 231.4724,  0.16,  0.11, -0.36-0.03},  //  MakeGraph(2013,"Tpc","^W...")       
	 { 0.    , 0.     ,-231.4880, -0.00, -0.00,  0.03-0.02+0.06-0.03}} //  MakeGraph(2013,"Tpc","^E...")
#else
	{{ 0.0393, -0.0305, 231.4759,  0.13,  0.12, -0.35-0.04},  //  MakeGraph(2003,"Tpc","^W...")
	 { 0.    ,  0.    ,-231.4880, -0.00, -0.00,  0.04     }}, //  MakeGraph(2003,"Tpc","^E...")
	{{ 0.0330, -0.0092, 231.4805,  0.08,  0.10, -0.34-0.03},  //  MakeGraph(2004,"Tpc","^W...")            	  
         { 0.    , -0.0001,-231.4829,  0.00,  0.00,  0.05-0.02+0.02}}, //  MakeGraph(2004,"Tpc","^E...")
	{{ 0.0193, -0.0133, 231.4724,  0.16,  0.11, -0.36-0.03},  //  MakeGraph(2013,"Tpc","^W...")       
	 { 0.    , 0.     ,-231.4880, -0.00, -0.00,  0.03-0.02+0.06-0.03-0.01}} //  MakeGraph(2013,"Tpc","^E...")
#endif
      };
      cout << Form("%4i,Wheel xyz (cm) = %8.4f %8.4f %8.4f abg[mrad] = %6.2f  %6.2f  %6.2f",
		   years[l],survWheelZabg[l][side][0],survWheelZabg[l][side][1],survWheelZabg[l][side][2],
		   survWheelZabg[l][side][3],survWheelZabg[l][side][4],survWheelZabg[l][side][5]) << endl;
      Double_t z = survWheelZabg[l][side][2];
      if (z > 0) z -= (229.71+1.7780);
      else       z += (229.71+1.7780);
      cout << Form("%4i,TpcHalf xyz (cm) = %8.4f %8.4f %8.4f abg[mrad] = %6.2f  %6.2f  %6.2f",
		   years[l],survWheelZabg[l][side][0],survWheelZabg[l][side][1],z,
		   survWheelZabg[l][side][3],survWheelZabg[l][side][4],survWheelZabg[l][side][5]) << endl;
      survWheelW.RotateX(1e-3*TMath::RadToDeg()*survWheelZabg[l][side][3]);
      survWheelW.RotateY(1e-3*TMath::RadToDeg()*survWheelZabg[l][side][4]);
      survWheelW.RotateZ(1e-3*TMath::RadToDeg()*survWheelZabg[l][side][5]);
      survWheelW.SetTranslation(survWheelZabg[l][side]); survWheelW.Print();
      Double_t *rot = survWheelW.GetRotationMatrix();
      Double_t *tr  = survWheelW.GetTranslation();
      cout << "{" << (side+1)%2 << ",";
      for (Int_t m = 0; m < 9; m++) {
	cout << Form("%8.5f,",rot[m]);
      }
      for (Int_t m = 0; m < 3; m++) {
	cout << Form("%9.4f,",tr[m]);
      }
      cout << Form("0,0,0,0,0,0,\"%4i %s\"},",years[l],sideName[side]) << endl;
#endif /* __Wheel2Tpc__ */
      WheelCS[side][l] = TpcCS[l] * survWheelW;;
      WheelCS[side][l].SetName(Form("WheelCS%i_%i",side,l));
      WheelCS[side][l].Print();
    }
  } // year
}
//________________________________________________________________________________
void FitGraph(TGraph2DErrors *graph = 0) {
  if (! graph) return;
  graph->Print("");
  if (lf ) {delete lf; lf = 0;}
  Int_t n = graph->GetN();
  cout << "graph " << graph->GetName() << " with " << n << " points" << endl;
  if (! lf) {
    lf = new TLinearFitter(3);
    /* zG = z0   - b  *(xG - x0) + a *(yG - y0) => z0 -b*xG +a*yG
       zG = p[0] + p[1]*x        + p[2]*y                        */
    TF2 *f2 = new TF2("f2","1++x++y",-500,500,-500,500);
    lf->SetFormula(f2);
    lf->SetUserFunc(f2);
  }
  lf->ClearPoints();
  if (n < 1) {
    cout << n << " points is not enought to fit" << endl;
    return;
  }
  TArrayD X(2*n); Double_t *x = X.GetArray();
  TArrayD Y(  n); Double_t *y = Y.GetArray();
  TArrayD E(  n); Double_t *e = E.GetArray();
  Double_t xmin = 1e9, xmax = -1e9;
  Double_t ymin = 1e9, ymax = -1e9;
  for (Int_t i = 0; i < n; i++) {
    x[2*i  ] = graph->GetX()[i];
    x[2*i+1] = graph->GetY()[i];
    if (xmin > x[2*i  ]) xmin = x[2*i  ];
    if (xmax < x[2*i  ]) xmax = x[2*i  ];
    if (ymin > x[2*i+1]) ymin = x[2*i+1];
    if (ymax < x[2*i+1]) ymax = x[2*i+1];
    y[i]     = graph->GetZ()[i];
    e[i]     = graph->GetEZ()[i];
    if (n < 4) {
      cout << Form("%2i xy %8.3f %8.3f z %8.3f +/- %8.2f",i,x[2*i  ],x[2*i+1],y[i],e[i]) << endl;
    }
  }
  if (n < 3) return;
  lf->AssignData(n, 2, x, y, e);
  //Perform the fitting and look at the results
  Double_t h = 0.75;
  TBits bits(n);
  if (n < 4) lf->Eval();
  else      {lf->EvalRobust(h);   lf->GetFitSample(bits);}
  TVectorD params;
  TVectorD errors;
  lf->GetParameters(params);
  //  lf->GetErrors(errors);
  //  for (Int_t i=0; i<3; i++)    cout << Form("par[%d]=%f ", i, params(i));
  Double_t chisquare=lf->GetChisquare();
  cout << Form("chisquare=%f\n", chisquare) << endl;
  
  //  bits.Print();
  Int_t j = 0;
  TF2 *func = (TF2*) lf->GetUserFunc();
  Double_t xav = 0;
  Double_t yav = 0;
  Double_t zav = 0; 
  TArrayD Xold(X);
  TArrayD Yold(Y);
  TArrayD Eold(E);
  for (Int_t l = 0; l < n; l++) {
    Double_t res = y[l] - func->Eval(x[2*l],x[2*l+1]);
    Double_t dev = res/e[l];
    xav     += x[2*l  ];
    yav     += x[2*l+1];
    zav     += y[l];
#if 0
    cout << Form("%2i xy %8.3f %8.3f z %8.3f +/- %8.2f res = %8.0f dev = %7.2f Bit: %1i",
		 l,x[2*l  ],x[2*l+1],y[l],e[l],1e4*res,dev,bits.TestBitNumber(l)) << endl;
#endif
    if (n >= 4 && ! bits.TestBitNumber(l) && TMath::Abs(dev) > 5)  continue;
    x[2*j  ] = x[2*l  ];
    x[2*j+1] = x[2*l+1];
    y[j]     = y[l];
    e[j]     = e[l];
    if (e[j] < 1e-10) e[j] = 0.01;
    j++;
  }
  if (j < 1) return;
  xav /= n;
  yav /= n;
  zav /= n;
  lf->ClearPoints();
  lf->AssignData(j, 2, x, y, e);
  lf->Eval();
  lf->GetParameters(params);
  lf->GetErrors(errors);
  //    for (Int_t i=0; i<3; i++)   cout << Form("par[%d]=%f+-%f\t", i, params(i), errors(i));
  chisquare=lf->GetChisquare();
  //    cout << Form("chisquare=%f\n", chisquare) << endl;
  TString line(Form("%-12s",graph->GetTitle()));
  //    line += Form("\txyz = %8.3f %8.3f %8.3f z =%9.4f +/- %6.4f (cm) alpha =%6.2f +/- %4.2f [mrad] beta =%6.2f +/- %4.2f [mrad] chi2/ndf%7.2f/%2i",
  //		 xav,yav,zav,params(0),errors(0),1e3*params(2),1e3*errors(2),-1e3*params(1),1e3*errors(1),chisquare,j-3);
  line += Form("z =%9.4f +/- %6.4f (cm) alpha =%6.2f +/- %4.2f [mrad] beta =%6.2f +/- %4.2f [mrad] chi2/ndf%12.2f/%4i",
	       params(0),errors(0),1e3*params(2),1e3*errors(2),-1e3*params(1),1e3*errors(1),chisquare,j-3);
  if (FitP) {
    BP.z = params(0); BP.dz = errors(0);
    BP.alpha =  1e3*params(2); BP.dalpha = 1e3*errors(2);
    BP.beta  = -1e3*params(1); BP.dbeta  = 1e3*errors(1);
    BP.chisq = chisquare;
    BP.ndf   = j-3;
  }
  if (! gROOT->IsBatch()) {
#if 1
    TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(graph->GetName());
    if (! c1 ) c1 = new TCanvas(graph->GetName(),graph->GetTitle());//,900,1000);
    c1->cd();
#endif
    func = (TF2*) lf->GetUserFunc();
#if 1      
    TH2D *plot = (TH2D *) gDirectory->Get(Form("h%s",graph->GetName()));
    if (plot) plot->Reset();
    else {
      plot = new TH2D(Form("h%s",graph->GetName()),
		      Form("%s: z =%6.2f +/- %4.2f (cm) #alpha =%6.2f +/- %4.2f [mrad] "
			   "#beta =%6.2f +/- %4.2f [mrad] #chi^{2}/ndf %8.2f/%i",
			   graph->GetName(),
			   params(0),errors(0),
			   1e3*params(2),1e3*errors(2),
			   -1e3*params(1),1e3*errors(1),chisquare,j-3),
		      100, xmin-10, xmax+10,
		      100, ymin-10, ymax+10);
      plot->SetXTitle("X(cm)");
      plot->SetYTitle("Y(cm)");
      plot->SetStats(0);
    }
    //       TLegend *l = new TLegend(0.1,0.8,0.9,0.88);
    //       l->AddEntry(plot,
    // 		  Form("z =%6.2f +/- %4.2f (cm) #beta =%6.2f +/- %4.2f [mrad] #alpha =%6.2f +/- %4.2f [mrad] #chi^{2}/ndf %8.2f/%3i",
    // 		       params(0),errors(0),-1e3*params(1),1e3*errors(1),1e3*params(2),1e3*errors(2),chisquare,j-3));
    //       plot->GetListOfFunctions()->Add(l);
#endif
    Double_t resAv = 0;
    Double_t res2  = 0;
    for (Int_t l = 0; l < n; l++) {
      Double_t res = Yold[l] - func->Eval(Xold[2*l],Xold[2*l+1]);
#if 0
      cout << Form("Res %7.1f (mkm) at %4i x = %8.4f and y = %8.4f",1e4*res,l,Xold[2*l],Xold[2*l+1]) << endl;
#endif
      resAv += res;
      res2  += res*res;
#if 1
      plot->Fill(Xold[2*l],Xold[2*l+1],1e4*res);
#endif
    }
    resAv /= n;
    res2  /= n;
    Double_t resRMS = TMath::Sqrt(res2 - resAv*resAv);
    if (FitP) {
      BP.res   = resAv;
      BP.dres  = resRMS;
    }
    TString Out("Results.data");
    ofstream out;
    if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
    else                              out.open(Out, ios::app);
    line +=  Form(" res. =%6.1f +/-%6.1f (mkm)",1e4*resAv,1e4*resRMS);
    cout << line << endl;
    out << line << endl;
    out.close();
#if 1
    plot->Draw("colz");
    c1->Update();
    //      while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 
#endif
  }
  if (FitP) {
    FitP->Fill((Float_t *)&BP.set);
  }
  //  if (! gROOT->IsBatch() && Ask()) return;
}
//________________________________________________________________________________
TGraph2DErrors *MakeGraph(Int_t iY=2013, const Char_t *system = "Magnet", const Char_t *pattern = "^WF", Double_t zmin = 0, Double_t zmax = 0/*, Int_t section = 0 */) {
  TString patt(pattern);
  TRegexp reg(pattern);
  Int_t side = -1;
  if (patt.BeginsWith("^W")) {side = 0;}
  if (patt.BeginsWith("^E")) {side = 1;}
  if (side < 0) {
    cout << "Illegal side for pattern " << pattern << endl;
    return 0;
  }
  if (zmin == 0 && zmax == 0) {
    if (side == 0) {zmin =   10; zmax = 500;}
    if (side == 1) {zmin = -500; zmax = -10;}
  }
  TString System(system);
  Int_t N = 0;
  TString year;
  SurveyData_t *survey = GetSurvey(iY,year,N);
  Int_t l = -1;
  if (iY == 2014) iY = 2013;
  if (iY == 2003) l = 0;
  if (iY == 2004) l = 1;
  if (iY == 2013) l = 2;
  if (l < 0) {
    cout << "Illegal year" << endl;
    return 0;
  }
  TString Name = system; Name += pattern;
  Name.ReplaceAll(".","");
  Name.ReplaceAll("^","");
  Name.ReplaceAll("*","");
  Name.ReplaceAll("$","");
  if (year != "2013") Name += year;
  TString name;
  if (! fOut) {
    name = Name; name += ".root";
    fOut = new TFile(name,"recreate");
  }
  if (! FitP) {
    FitP = new TNtuple("FitP","Results of fit", vFitP);
  }
  Int_t set = -1;
  if       (Name.Contains("ZF")) set = 0;
  else if  (Name.Contains("FF")) set = 1; 
  else if  (Name.Contains("RF")) set = 2; 
  else if  (Name.Contains("RC")) set = 3; 
  else if  (Name.Contains("ZC")) set = 4; 
  BP.set = set;
  BP.side = side;
  name ="G";
  name += Name;
  TGraph2DErrors *graph = (TGraph2DErrors *) gDirectory->Get(name);
  if (graph) {delete graph; graph = 0;}
  graph = new TGraph2DErrors();
  graph->SetName(name);
  graph->SetTitle(name);
  name ="P";
  name += Name;
  TProfile2D *prof2D = (TProfile2D *) gDirectory->Get(name);
  if (prof2D) prof2D->Reset();
  else        prof2D = new TProfile2D(name,name,200,-200,200,200,-200,200);
  name ="Z";
  name += Name;
  zPlot = (TH1D *) gDirectory->Get(name);
  if (zPlot) zPlot->Reset();
  else       zPlot = (TH1D *) gDirectory->Get(name);
  if (zPlot) zPlot->Reset();
  else        zPlot = new TH1D(name,name,4200,-210,210);
  name = "Rphi";
  name += Name;
  prof2DRphi = (TProfile2D *) gDirectory->Get(name);
  if (prof2DRphi) prof2DRphi->Reset();
  else            prof2DRphi = new TProfile2D(name,name,36,-180,180,50,50,200);
  //    graph->SetPoint(n,survey->XSurvey,survey->ZSurvey,survey->YSurvey);
  Int_t n = 0;
  for (Int_t i = 0; i < N; i++, survey++) {
    if (! System.Contains(survey->system,TString::kIgnoreCase)) {
      //      cout << "system " << *survey << " skipped" << endl;
      continue;
    }
    TString Target(survey->target);
    TString Comment(survey->comment);
    if (survey->YSurvey < zmin || survey->YSurvey > zmax) {
      //      cout << "Z " << *survey << " skipped" << endl;
      continue;
    }
    if (! Target.Contains(reg)) {
      //      cout << "Target " << *survey << " skipped" << endl;
      continue;
    }
#if 0
    cout << *survey << endl;
#endif
    Double_t xyzG[3] = {survey->XSurvey,survey->ZSurvey,survey->YSurvey};
    Double_t xyzL[3];
    if (Comment.Contains("Membrane") || Comment.Contains("Strip")) {
      xyzL[0] = xyzG[0];
      xyzL[1] = xyzG[1];
      xyzL[2] = xyzG[2];
    } else {
      if (System == "Magnet")     MagCS[l].MasterToLocal(xyzG,xyzL);
      else                        WheelCS[side][l].MasterToLocal(xyzG,xyzL);
    }
#if 0
    //    Double_t x = survey->XSurvey;
    //    Double_t y = survey->ZSurvey;
    Double_t x = xyzL[0];
    Double_t y = xyzL[1];
    cout << Form(" R =%6.2f phi = %8.3f",TMath::Sqrt(x*x + y*y),TMath::RadToDeg()*TMath::ATan2(y,x));
    cout << endl;
#endif
    graph->SetPoint(n,xyzL[0],xyzL[1],xyzL[2]);
    graph->SetPointError(n,survey->dXSurvey,survey->dZSurvey,survey->dYSurvey);
    prof2D->Fill(xyzL[0],xyzL[1],xyzL[2]);
    prof2DRphi->Fill(TMath::RadToDeg()*TMath::ATan2(xyzL[1],xyzL[0]),TMath::Sqrt(xyzL[0]*xyzL[0]+xyzL[1]*xyzL[1]),xyzL[2]);
    zPlot->Fill(xyzL[2]);
#if 0
    cout << Form("%10s",graph->GetName()) << "                ";
    cout << Form(" %10.3f +/- %8.3f %10.3f +/- %8.3f %10.3f +/- %8.3f",
		 graph->GetX()[n],graph->GetEX()[n],
		 graph->GetY()[n],graph->GetEY()[n],
		 graph->GetZ()[n],graph->GetEZ()[n]) << endl;
#endif
    n++;
  }
  if (n > 0) FitGraph(graph);
  return graph;
}
//____________________________________________________________________
void MakeSectors(const Char_t *pattern = "ZF", Int_t iY=2013, const Char_t *system = "Tpc") {
  TString select;
  for (Int_t io = 0; io < 2; io++) {
    for (Int_t sec = 1; sec <= 24; sec++) {
      memset(&BP.set, 0, sizeof( FitP_t));
      BP.sector = sec;
      BP.io     = io;
      if (sec <= 12) select = "^W";
      else           select = "^E";
      if (io == 0)   select += "I";
      else           select += "O";
      select += Form("%02i.*",sec);
      select += pattern;
      MakeGraph(iY,system,select,-10,10);
    }
  }
}
//____________________________________________________________________
void myfcn(Int_t &, Double_t *, Double_t &f, Double_t *par, Int_t) {
  //minimisation function computing the sum of squares of residuals
  Int_t np = graphfit->GetN();
  f = 0;
  Double_t *x = graphfit->GetX();
  Double_t *y = graphfit->GetY();
  Double_t *ex = graphfit->GetEX();
  Double_t *ey = graphfit->GetEY();
  for (Int_t i=0;i<np;i++) {
    Double_t u = x[i] - par[0];
    Double_t v = y[i] - par[1];
    Double_t R = TMath::Sqrt(u*u+v*v);
    Double_t dr = par[2] - R;
    if (ex && ey) {
      Double_t dx = u*ex[i];
      Double_t dy = v*ey[i];
      Double_t er = TMath::Sqrt(dx*dx + dy*dy)/R;
      dr /= er;
    }
    f += dr*dr;
  }
}

//____________________________________________________________________
void fitCircle(TGraphErrors *graph) {
  if (! graph) return;
  graphfit = graph;
  //generates n points around a circle and fit them
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1",600,600);
  c1->SetGrid();
  graphfit->SetMarkerStyle(20);
  graphfit->Draw("axp");
  
  //Fit a circle to the graph points
  TVirtualFitter::SetDefaultFitter("Minuit");  //default is Minuit
  TVirtualFitter *fitter = TVirtualFitter::Fitter(graph, 3);
  fitter->SetFCN(myfcn);
   
  fitter->SetParameter(0, "x0",   0, 0.1, 0,0);
  fitter->SetParameter(1, "y0",   0, 0.1, 0,0);
  fitter->SetParameter(2, "R",    1, 0.1, 0,0);
  
  Double_t arglist[1] = {0};
  fitter->ExecuteCommand("MIGRAD", arglist, 0);
  
  //Draw the circle on top of the points
  TArc *arc = new TArc(fitter->GetParameter(0),
		       fitter->GetParameter(1),fitter->GetParameter(2));
  arc->SetLineColor(kRed);
  arc->SetLineWidth(4);
  arc->Draw();
}
//____________________________________________________________________
void myfcn2(Int_t &, Double_t *, Double_t &f, Double_t *par, Int_t) {
  //minimisation function computing the sum of squares of residuals
  Int_t nx = h2fit->GetNbinsX();
  Int_t ny = h2fit->GetNbinsY();
  f = 0;
  for (Int_t i = 1; i <= nx; i++) {
    Double_t x = h2fit->GetXaxis()->GetBinCenter(i);
    for (Int_t j = 1; j <= ny; j++) {
      Double_t z = h2fit->GetBinContent(i,j);
      Double_t ez = h2fit->GetBinError(i,j);
      if (ez < 1) continue;
      if (z/ez < 3) continue;
      Double_t y = h2fit->GetYaxis()->GetBinCenter(j);
      Double_t u = x - par[0];
      Double_t v = y - par[1];
      Double_t R = TMath::Sqrt(u*u+v*v);
      Double_t dr = par[2] - R;
      //      dr /= ez;
      f += dr*dr*z;
    }
  }
}

//____________________________________________________________________
void fitCircle(TH2 *h2) {
  if (! h2) return;
  h2fit = h2;
  //generates n points around a circle and fit them
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1",600,600);
  c1->SetGrid();
  //  h2fit->SetMarkerStyle(20);
  //  h2fit->Draw("axp");
  h2fit->Draw("colz");
  //Fit a circle to the graph points
  TVirtualFitter::SetDefaultFitter("Minuit");  //default is Minuit
  TVirtualFitter *fitter = TVirtualFitter::Fitter(h2, 3);
  fitter->SetFCN(myfcn2);
   
  fitter->SetParameter(0, "x0",   0, 0.1, 0,0);
  fitter->SetParameter(1, "y0",   0, 0.1, 0,0);
  fitter->SetParameter(2, "R",    4, 0.1, 0,0);
  //  fitter->FixParameter(2);
  fitter->ReleaseParameter(2);
  Double_t arglist[1] = {0};
  fitter->ExecuteCommand("MIGRAD", arglist, 0);
  
  //Draw the circle on top of the points
  TArc *arc = new TArc(fitter->GetParameter(0),
		       fitter->GetParameter(1),fitter->GetParameter(2));
  arc->SetLineColor(kRed);
  arc->SetLineWidth(4);
  arc->Draw();
}
//____________________________________________________________________
void myfcnEllipse(Int_t &, Double_t *, Double_t &f, Double_t *par, Int_t) {
  //minimisation function computing the sum of squares of residuals
  Int_t np = graphfit->GetN();
  f = 0;
  Double_t *x = graphfit->GetX();
  Double_t *y = graphfit->GetY();
  Double_t *ex = graphfit->GetEX();
  Double_t *ey = graphfit->GetEY();
  for (Int_t i=0;i<np;i++) {
    Double_t u = x[i] - par[0];
    Double_t v = y[i] - par[1];
    Double_t R = TMath::Sqrt(u*u+v*v);
    Double_t phi = TMath::ATan2(v,u);
    Double_t ecl = par[3]*TMath::Cos(phi-par[2]);
    Double_t R_exp = par[4]/TMath::Sqrt(1 - ecl*ecl);
    Double_t dr = R_exp - R;
    if (ex && ey) {
      Double_t dx = u*ex[i];
      Double_t dy = v*ey[i];
      Double_t er = TMath::Sqrt(dx*dx + dy*dy)/R;
      dr /= er;
    }
    f += dr*dr;
  }
}
//____________________________________________________________________
void fitEllipse(TGraphErrors *graph) {
  if (! graph) return;
  graphfit = graph;
  //generates n points around a circle and fit them
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1",600,600);
  c1->SetGrid();
  graphfit->SetMarkerStyle(20);
  graphfit->Draw("axp");
  
  //Fit a circle to the graph points
  TVirtualFitter::SetDefaultFitter("Minuit");  //default is Minuit
  TVirtualFitter *fitter = TVirtualFitter::Fitter(graph, 5);
  fitter->SetFCN(myfcnEllipse);
  // (x-x0)**2 + (y-y0)**2 = B**2/(1 - epsilon**2*cos(phi-phi0)**2)
  fitter->SetParameter(0, "x0",   0, 0.1, 0,0);
  fitter->SetParameter(1, "y0",   0, 0.1, 0,0);
  fitter->SetParameter(2, "phi0",   0.1, 0.1, 0,0);
  fitter->SetParameter(3, "epsilon",   0.1, 0.1, 0,1);
  fitter->SetParameter(4, "B",    1, 0.1, 0,0);
  
  Double_t arglist[1] = {0};
  //  fitter->ExecuteCommand("MIGRAD", arglist, 0);
  fitter->ExecuteCommand("MINIMIZE", arglist, 0);
  Double_t x1 = fitter->GetParameter(0);
  Double_t y1 = fitter->GetParameter(1);
  Double_t theta = TMath::RadToDeg()*fitter->GetParameter(2);
  Double_t r1 = fitter->GetParameter(4)/TMath::Sqrt(1 - fitter->GetParameter(3)*fitter->GetParameter(3));
  Double_t r2 = fitter->GetParameter(4);
  //Draw the ellipse on top of the points
  TEllipse *ellipse = new TEllipse(x1,y1,r1,r2,0,360,theta);
  ellipse->SetLineColor(kRed);
  ellipse->SetLineWidth(4);
  ellipse->Draw();
}
//________________________________________________________________________________
TGraphErrors *MakeRGraph(Int_t iY=2004, const Char_t *pattern = "^EAO", 
			 Double_t zmin = -500, Double_t zmax = 500, 
			 Double_t rmin = 0, Double_t rmax = 200, Bool_t ellipse = kFALSE) {
  Int_t l = -1;
  if (iY == 2003) l = 0;
  if (iY == 2004) l = 1;
  if (iY == 2013) l = 2;
  if (l < 0) {
    cout << "Illegal year" << endl;
    return 0;
  }
  const Char_t *system = "Tpc";
  TString patt(pattern);
  TRegexp reg(pattern);
  TString System(system);
  Int_t N = 0;
  TString year;
  SurveyData_t *survey0 = GetSurvey(iY,year,N);
  SurveyData_t *survey = 0;
  TGraphErrors *graph = 0;
  TString plotName("gamPlot");
  plotName += pattern;
  plotName.ReplaceAll("^","");
  TH1D *gamPlot = (TH1D *) gDirectory->Get(plotName);
  if (! gamPlot) gamPlot = new TH1D(plotName,"gamma[mrad] versus sector",24,0.5,24.5);
  else           gamPlot->Reset();
  Int_t side = -1;
  if      (patt.BeginsWith("^W")) side = 0;
  else if (patt.BeginsWith("^E")) side = 1;
  if (side < 0) {
    cout << "Illegal side for pattern " << patt.Data() << endl;
    return 0;
  }
  Int_t n = 0;
  survey = survey0;
  for (Int_t i = 0; i < N; i++, survey++) {
    if (! System.Contains(survey->system,TString::kIgnoreCase)) continue;
    TString Target(survey->target);
    Int_t sec;
    Int_t nread = sscanf(Target.Data(),"%*3c%2i",&sec);
    if (nread != 1) continue;
    if (survey->YSurvey < zmin || survey->YSurvey > zmax) continue;
    if (! Target.Contains(reg)) continue;
    cout << *survey;
    Double_t xyzG[3] = {survey->XSurvey,survey->ZSurvey,survey->YSurvey};
    Double_t xyzL[3];
    WheelCS[side][l].MasterToLocal(xyzG,xyzL);
    //    Double_t x = survey->XSurvey;
    //    Double_t y = survey->ZSurvey;
    Double_t x = xyzL[0];
    Double_t y = xyzL[1];
    Double_t r = TMath::Sqrt(x*x + y*y);
    if (r < rmin || r > rmax) continue;
    cout << endl;
    if (! graph) {
      graph = new TGraphErrors();
      TString Name = survey->system; Name += survey->target; Name += year;
      graph->SetName(Name);
      graph->SetTitle(Name);
    }
    //    graph->SetPoint(n,survey->XSurvey,survey->ZSurvey);
    graph->SetPoint(n,xyzL[0],xyzL[1]);
    graph->SetPointError(n,survey->dXSurvey,survey->dZSurvey);
#if 0
    cout << Form("%10s",graph->GetName()) << "                ";
    cout << Form(" %10.3f +/- %8.3f %10.3f +/- %8.3f %10.3f +/- %8.3f",
		 graph->GetX()[n],graph->GetEX()[n],
		 graph->GetY()[n],graph->GetEY()[n]) << endl;
#endif
    n++;
  }
  if (n <= 1) return graph;
  if (! ellipse)    fitCircle(graph);
  else              fitEllipse(graph);
  TVirtualFitter *fitter =TVirtualFitter::GetFitter();
  if (! fitter) return graph;
  Double_t x0 = fitter->GetParameter(0);
  Double_t y0 = fitter->GetParameter(1);
  Double_t gamma = 0;
  Double_t gamma2 = 0;
  n = 0;
  survey = survey0;
  for (Int_t i = 0; i < N; i++, survey++) {
    if (! System.Contains(survey->system,TString::kIgnoreCase)) continue;
    TString Target(survey->target);
    Int_t sec;
    Int_t nread = sscanf(Target.Data(),"%*3c%2d",&sec);
    if (nread != 1) continue;
    if (survey->YSurvey < zmin || survey->YSurvey > zmax) continue;
    if (! Target.Contains(reg)) continue;
    cout << *survey;
    Double_t xyzG[3] = {survey->XSurvey,survey->ZSurvey,survey->YSurvey};
    Double_t xyzL[3];
    if (patt.BeginsWith("^W")) WheelCS[0][l].MasterToLocal(xyzG,xyzL);
    else                       WheelCS[1][l].MasterToLocal(xyzG,xyzL);
    //    Double_t x = survey->XSurvey - x0;
    //    Double_t y = survey->ZSurvey - y0;
    Double_t x = xyzL[0] - x0;
    Double_t y = xyzL[1] - y0;
    //    Double_t z = survey->YSurvey;
    Double_t r = TMath::Sqrt(x*x + y*y);
    Double_t phiSec;
    if (patt.BeginsWith("^W")) phiSec = 30*(3 - sec);
    else                       phiSec = 30*(sec - 21);
    if (phiSec < -180) phiSec += 360;
    if (phiSec >  180) phiSec -= 360;
    phiSec *= TMath::DegToRad();
    Double_t angle = TMath::ATan2(y,x);
    if (angle < 0) angle += TMath::TwoPi();
    if (r < rmin || r > rmax) continue;
    Double_t diff = angle-phiSec;
    if (diff < -TMath::Pi()) diff += TMath::TwoPi();
    if (diff >  TMath::Pi()) diff -= TMath::TwoPi();
    cout << Form(" R =%6.2f phi = %8.3f sec %8.3f diff %8.3f",r,angle,phiSec,1e3*diff);
    cout << endl;
    gamPlot->Fill(sec,1e3*diff);
    gamma += diff;
    gamma2 += diff*diff;
    n++;
  }
  n++;
  gamma /= n;
  gamma2 /= n;
  Double_t dgam = TMath::Sqrt(gamma2 - gamma*gamma)/TMath::Sqrt(n-1);
  cout << Form("%10s",graph->GetName());
  cout << Form(" x0 =%8.4f +/- %7.4f y0 =%8.4f +/- %7.4f R =%8.4f +/- %7.4f gamma =%6.2f +/- %5.2f", 
	       fitter->GetParameter(0),fitter->GetParError(0),
	       fitter->GetParameter(1),fitter->GetParError(1),
	       fitter->GetParameter(2),fitter->GetParError(2),
	       1e3*gamma,1e3*dgam) << endl;
  return graph;
}
//________________________________________________________________________________
void TpcSurvey() {
  InitMatrices();
}
//________________________________________________________________________________
void TpcSurveyAll(Int_t d1 = 0) {
  //  PrintSurvey();
  //                     l  d  wea; l = 0 -> 2003, d = Magnet, Tpc, Svt, wea = West, East, Any  
  //                                    1 -> 2004
  //                                    2 -> 2013
  memset(graphs, 0, sizeof(graphs));
  const Char_t *systems[5] = {"Magnet","Tpc" ,"AO", "BO","DO"};
  const Char_t *site[5][2] = 
    {{"^WF"    ,"^EF"    }, // magnet
     {"^W....$","^E....$"}, // wheels
     {"^WAO"    ,"^EAO"     },
     {"^WBO"    ,"^EBO"     },
     {"^WDO"    ,"^EDO"     }};
  const Int_t ys[3] = {2003, 2004, 2013};
  InitMatrices();
  for (Int_t d = d1; d < 5; d++) {// detectors: Magnet, Tpc, TpcR
    for (Int_t l = 2; l < 3; l++) {// year
      for (Int_t s = 0; s < 2; s++) { // side
	if (d < 2) {// magnet and wheel
	  MakeGraph(ys[l],systems[d],site[d][s]);
	} else  {// TpcR
	  MakeRGraph(ys[l],site[d][s]);
	}
	if (! gROOT->IsBatch() && Ask()) return;
      }
    }
  }
}
//________________________________________________________________________________
void DrawFitP(const Char_t *var = "alpha", Double_t ymax = 5, Int_t set = -1, Int_t io = -1) {
  if (! FitP) FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP) return;
  Int_t s1 = 0;
  Int_t s2 = 4;
  if (set >= 0) {s1 = s2 = set;}
  Int_t io1 = 0;
  Int_t io2 = 1;
  if (io >= 0) {io1 = io2 = io;}
  for (Int_t i = io1; i <= io2; i++) {
    TString ION("Inner");
    if (i == 1) ION = "Outer";
    TCanvas *c = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(ION);
    if (! c) c = new TCanvas(ION,ION);
    c->Clear();
    if (s2 > s1) c->Divide(1,s2 - s1 + 1);
    for (Int_t s = s1; s <= s2; s++) {
      if (s2 > s1) c->cd(s+1)->Clear();
      TString same("");
      for (Int_t we = 0; we < 2; we++) {
	FitP->SetMarkerColor(we+1);
	TString Draw(var);
	Draw += ":secE2W(sector) >>";
	Draw += var; 
	if (io == 0) Draw += "I";
	else         Draw += "O";
	if (we == 0) Draw += "W";
	else         Draw += "E";
	Draw += Form("(12,0.5,12.5,100,%f,%f)",-ymax,ymax);
	TString Cut("ndf>0");
	Cut += Form(" && io == %i",i);
	if (we == 0) Cut += " && sector <= 12";
	else         Cut += " && sector >  12";
	Cut += Form(" && set == %i",s);
	FitP->Draw(Draw,Cut,same);
	same = "same";
      }
    }
  }
}
//________________________________________________________________________________
void y2014M() {
#if 1
  MakeGraph(2014,"Tpc","^WI.*FF",-10, 10);
  MakeGraph(2014,"Tpc","^WO.*FF",-10, 10);
  MakeGraph(2014,"Tpc","^W.*FF", -10, 10);
  MakeGraph(2014,"Tpc","^EI.*FF",-10, 10);
  MakeGraph(2014,"Tpc","^EO.*FF",-10, 10);
  MakeGraph(2014,"Tpc","^E.*FF", -10, 10);
  MakeGraph(2014,"Tpc","^WI.*ZF",-10, 10);
  MakeGraph(2014,"Tpc","^WO.*ZF",-10, 10);
  MakeGraph(2014,"Tpc","^W.*ZF", -10, 10);
  MakeGraph(2014,"Tpc","^EI.*ZF",-10, 10);
  MakeGraph(2014,"Tpc","^EO.*ZF",-10, 10);
  MakeGraph(2014,"Tpc","^E.*ZF", -10, 10);
  MakeGraph(2014,"Tpc","^WI.*ZR",-10, 10);
  MakeGraph(2014,"Tpc","^WO.*ZR",-10, 10);
  MakeGraph(2014,"Tpc","^W.*ZR", -10, 10);
  MakeGraph(2014,"Tpc","^EI.*ZR",-10, 10);
  MakeGraph(2014,"Tpc","^EO.*ZR",-10, 10);
  MakeGraph(2014,"Tpc","^E.*ZR", -10, 10);
  MakeGraph(2014,"Tpc","^WI.*RF",-10, 10);
  MakeGraph(2014,"Tpc","^WO.*RF",-10, 10);
  MakeGraph(2014,"Tpc","^W.*RF", -10, 10);
  MakeGraph(2014,"Tpc","^EI.*RF",-10, 10);
  MakeGraph(2014,"Tpc","^EO.*RF",-10, 10);
  MakeGraph(2014,"Tpc","^E.*RF", -10, 10);
#endif
  MakeGraph(2014,"Tpc","^WI.*ZQ",-10, 10);
  MakeGraph(2014,"Tpc","^WO.*ZQ",-10, 10);
  MakeGraph(2014,"Tpc","^W.*ZQ", -10, 10);
  MakeGraph(2014,"Tpc","^EI.*ZQ",-10, 10);
  MakeGraph(2014,"Tpc","^EO.*ZQ",-10, 10);
  MakeGraph(2014,"Tpc","^E.*ZQ", -10, 10);
  MakeGraph(2014,"Tpc","^WI.*RQ",-10, 10);
  MakeGraph(2014,"Tpc","^WO.*RQ",-10, 10);
  MakeGraph(2014,"Tpc","^W.*RQ", -10, 10);
  MakeGraph(2014,"Tpc","^EI.*RQ",-10, 10);
  MakeGraph(2014,"Tpc","^EO.*RQ",-10, 10);
  MakeGraph(2014,"Tpc","^E.*RQ", -10, 10);
}

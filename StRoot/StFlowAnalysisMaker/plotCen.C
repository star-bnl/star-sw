///////////////////////////////////////////////////////////////////////////////
//
// $Id: plotCen.C,v 1.16 2003/05/06 21:33:08 posk Exp $
//
// Author:       Art Poskanzer, LBNL, July 2000
//               FTPC added by Markus Oldenburg, MPI, Dec 2000
// Description:  Macro to plot histograms made by StFlowAnalysisMaker.
//               Plots a set of histograms with different centrality
//               starting with anaXX.root given by first run number XX.
//               Run Number appended to "ana" is entered in the bottom, left box.
//               Default selN = 1 and harN = 2.
//               First time type .x plotCen.C() to see the menu.
//               After the first execution, just type plotCen(N) .
//               A negative N plots all pages starting with page N.
//               Place a symbolic link to this file in StRoot/macros/analysis .
//
//
///////////////////////////////////////////////////////////////////////////////

#include <math.h>
#include "TMath.h" 
 
const  Int_t nCens = 10;
int    runNumber   = 0;
char   runName[6];
char   fileName[30];
char   histTitle[30];
TFile* histFile[nCens];
char   tmp[10];
TCanvas* c;

TCanvas* plotCen(Int_t pageNumber=0, Int_t selN=2, Int_t harN=2){

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
    
  gROOT->SetStyle("Bold");                              // set style
  gROOT->ForceStyle();

  int canvasWidth = 600, canvasHeight = 780;             // portrait
  int columns = 2;
  int rows    = nCens/columns;
  int pads    = rows*columns;

  // names of histograms made by StFlowAnalysisMaker
  // also projections of some of these histograms
  const char* baseName[] = {
    "Flow_Cos_Sel",
    "Flow_Res_Sel",
    "Flow_v_Sel",
    "Flow_v_ScalarProd_Sel",
    "Flow_Cumul_v_Order2_Sel",
    "Flow_Cumul_v_Order4_Sel",
    "Flow_VertexZ",
    "Flow_VertexXY2D",
    "Flow_EtaSymVerZ2D_Tpc",
    "Flow_EtaSym_Tpc",
    "Flow_EtaSymVerZ2D_Ftpc",
    "Flow_EtaSym_Ftpc",
    "Flow_CTBvsZDC2D",
    "Flow_Cent",
    "Flow_OrigMult",
    "Flow_Mult",
    "Flow_MultOverOrig",
    "Flow_MultEta",
    "Flow_MultPart",
    "Flow_Charge_Ftpc",
    "Flow_DcaGlobal_Tpc",
    "Flow_Dca_Ftpc",
    "Flow_DcaGlobal_Ftpc",
    "Flow_Chi2_Tpc",
    "Flow_Chi2_Ftpc",
    "Flow_FitPts_Tpc",
    "Flow_MaxPts_Tpc",
    "Flow_FitOverMax_Tpc",
    "Flow_FitPts_Ftpc",
    "Flow_MaxPts_Ftpc",
    "Flow_FitOverMax_Ftpc",
    //"Flow_EtaPtPhi3D",
    //"Flow_EtaPtPhi2D.PhiEta",
    //"Flow_EtaPtPhi2D.PhiPt",
    "Flow_YieldAll2D",
    "Flow_YieldAll.Eta",
    "Flow_YieldAll.Pt",
    "Flow_YieldPart2D",
    "Flow_YieldPart.Eta",
    "Flow_YieldPart.Pt",
    "Flow_MeanDedxPos2D",
    "Flow_MeanDedxNeg2D",
    "Flow_PidPiPlusPart",
    "Flow_PidPiMinusPart",
    "Flow_PidProtonPart",
    "Flow_PidAntiProtonPart",
    "Flow_PidKplusPart",
    "Flow_PidKminusPart",
    "Flow_PidDeuteronPart",
    "Flow_PidAntiDeuteronPart",
    "Flow_PidElectronPart",
    "Flow_PidPositronPart",
    "Flow_PidMult",
    //"Flow_Bin_Eta",
    //"Flow_Bin_Pt",
    "Flow_CosPhiLab",
    "Flow_Yield2D_Sel",
    "Flow_Yield.Eta_Sel",
    "Flow_Yield.Pt_Sel",
    "Flow_Mul_Sel",
    "Flow_Phi_East_Sel",
    "Flow_Phi_Flat_East_Sel",
    "Flow_Phi_Weight_East_Sel",
    "Flow_Phi_West_Sel",
    "Flow_Phi_Flat_West_Sel",
    "Flow_Phi_Weight_West_Sel",
    "Flow_Phi_FtpcEast_Sel",
    "Flow_Phi_Flat_FtpcEast_Sel",
    "Flow_Phi_Weight_FtpcEast_Sel",
    "Flow_Phi_FtpcWest_Sel",
    "Flow_Phi_Flat_FtpcWest_Sel",
    "Flow_Phi_Weight_FtpcWest_Sel",
    "Flow_Psi_Subs",
    "Flow_Psi_Sel",
    "Flow_Psi_Sub_Corr_Sel",
    "Flow_Psi_Sub_Corr_Diff_Sel",
    "Flow_Phi_Corr_Sel",
    "Flow_vObs2D_Sel",
    "Flow_vObsEta_Sel",
    "Flow_vObsPt_Sel",
    "Flow_v2D_Sel",
    "Flow_vEta_Sel",
    "Flow_vPt_Sel",
    "Flow_q_Sel",
    "Flow_vObs2D_ScalarProd_Sel",
    "Flow_v2D_ScalarProd_Sel",
    "Flow_vEta_ScalarProd_Sel",
    "Flow_vPt_ScalarProd_Sel", 
    "Flow_Cumul_vEta_Order2_Sel",
    "Flow_Cumul_vPt_Order2_Sel" 
};
  const int nNames = sizeof(baseName) / sizeof(char*);
  const int nDoubles = 6;
  const int nSingles = 45 + nDoubles;

  // construct array of short names
  char* shortName[] = new char*[nNames];
  for (int n = 0; n < nNames; n++) {
    shortName[n] = new char[35];
    strcpy(shortName[n], baseName[n]);
    char* cp = strstr(shortName[n],"_Sel");
    if (cp) *cp = '\0';                                  // truncate
  }

  // input the first run number
  if (runNumber == 0) {
    cout << "     first run number? ";
    fgets(tmp, sizeof(tmp), stdin);
    runNumber = atoi(tmp);
    sprintf(runName, "ana%2d", runNumber);               // add ana prefix
    cout << " first run name = " << runName << endl;

  // open the files
    for (int n = 0; n < nCens; n++) {
      sprintf(fileName, "ana%2d.root", runNumber + n);
      cout << " file name = " << fileName << endl;
      histFile[n] = new TFile(fileName);
    }
  }

  // input the page number
  while (pageNumber <= 0 || pageNumber > nNames) {
    if (pageNumber < 0) {                                // plot all
      plotCenAll(nNames, selN, harN, -pageNumber);
      return c;
    }
    cout << "-1: \t All" << endl;                        // print menu
    for (int i = 0; i < nNames; i++) {
      cout << i+1 << ":\t " << shortName[i] << endl;
    }
    cout << "     page number? ";
    fgets(tmp, sizeof(tmp), stdin);
    pageNumber = atoi(tmp);
  }

  // set flags
  bool multiGraph  = kFALSE;
  bool doubleGraph = kFALSE;
  bool singleGraph = kFALSE;
  if (pageNumber > 0 && pageNumber <= nDoubles) {
    doubleGraph = kTRUE;
  } else if (pageNumber > nDoubles && pageNumber <= nSingles) {
    singleGraph = kTRUE;
  } else {
    multiGraph  = kTRUE;
  }
  pageNumber--;

  // set constants
  float twopi   = 2. * TMath::Pi();
  float etaMax  =   1.5;
  float yMin    =  -4.5;
  float yMax    =   4.5;
  float qMax    =   3.5;
  float phiMax  = twopi; 
  int   n_qBins =    50;
  float Ycm     =   0.0;
  TString* histProjName = NULL;

  // construct histName and histProjName
  char sel[2];
  sprintf(sel,"%d",selN);
  char har[2];
  sprintf(har,"%d",harN);
  float order = (float)harN;
  char* temp = new char[35];                       // construct histName
  strcpy(temp,shortName[pageNumber]);
  char* cproj = strstr(temp,".");
  if (cproj) {                                     // a projection
    *cproj = '\0';                                 // remove from "." on
    if (singleGraph) {
      cproj = strstr(temp,"2");
      if (cproj) {                                 // a 2D projection 
	*cproj = '\0';                             // remove from "2D" on
	strcat(temp,"3D");
      } else {
	strcat(temp,"2D");
      }
    } else {
      strcat(temp,"2D_Sel");
    }
    TString* histName = new TString(temp);
    histProjName = new TString(baseName[pageNumber]);
    if (multiGraph) {
      histProjName->Append(*sel);
      histProjName->Append("_Har");
      histProjName->Append(*har);
    }
  } else {                                         // not projection
    TString* histName = new TString(baseName[pageNumber]);
  }
  if (!singleGraph) histName->Append(*sel);
  if (multiGraph) {
    histName->Append("_Har");
    histName->Append(*har);
  }
  cout << " graph name= " << histName->Data() << endl;

  // make the graph page
  c = new TCanvas(shortName[pageNumber], shortName[pageNumber],
			   canvasWidth, canvasHeight);
  c->ToggleEventStatus();
  TPaveLabel* title = new TPaveLabel(0.1,0.96,0.9,0.99,histName->Data());
  title->Draw();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.95);
  graphPad->Draw();
  graphPad->cd();
  graphPad->Divide(columns, rows);
  TLine* lineZeroY = new TLine(yMin, 0., yMax, 0.);
  TLine* lineYcm   = new TLine(Ycm, -10., Ycm, 10.);
  float v;
  float err;
  for (int i = 0; i < pads; i++) {
    int fileN = i;                           // file number
    int padN = fileN + 1;                    // pad number
    sprintf(histTitle,"Centrality %d",fileN);
    cout << "centrality= " << fileN << endl;

    // get the histogram
    bool twoD;
    bool threeD;
    if (histProjName) {
      if (strstr(temp,"3D")) {                      // 2D projection
	twoD = kTRUE;
	TH3* hist3D = dynamic_cast<TH3*>(histFile[fileN]->Get(histName->Data()));
	if (!hist3D) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return c;
	}
	hist3D->SetTitle(histTitle);
      } else {                                      // 1D projection
	TH2* hist2D = dynamic_cast<TH2*>(histFile[fileN]->Get(histName->Data()));
	if (!hist2D) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return c;
	}
	hist2D->SetTitle(histTitle);
      }
    } else {
      if (strstr(shortName[pageNumber],"3D")!=0) {  // 3D
	threeD = kTRUE;
	TH3* hist3D = dynamic_cast<TH3*>(histFile[fileN]->Get(histName->Data()));
	if (!hist3D) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return c;
	}
	hist3D->SetTitle(histTitle);
      } else if (strstr(shortName[pageNumber],"2D")!=0) { // 2D
	twoD = kTRUE;
	TH2* hist2D = dynamic_cast<TH2*>(histFile[fileN]->Get(histName->Data()));
	if (!hist2D) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return c;
	}
	hist2D->SetTitle(histTitle);
      } else {                                            // 1D
	TH1* hist = dynamic_cast<TH1*>(histFile[fileN]->Get(histName->Data()));
	if (!hist) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return c;
	}
	float ptMax = hist->GetXaxis()->GetXmax();
	hist->SetTitle(histTitle);
      }
    }
    
    // make the plots
    graphPad->cd(padN);
    
    if (threeD) {                                            // 3D
      gStyle->SetOptStat(10);
      hist3D->Draw("BOX");
    } else if (twoD) {                                       // 2D
      if (strstr(shortName[pageNumber],".PhiEta")!=0) {      // 3D Phi Eta proj.
	TH2D* projZX = (TH2*)hist3D->Project3D("zx");
	projZX->SetName(histProjName->Data());
	projZX->SetYTitle("azimuthal angle (rad)");
	projZX->SetXTitle("rapidity");
	gStyle->SetOptStat(0);
	if (projZX) projZX->Draw("COLZ");
      } else if (strstr(shortName[pageNumber],".PhiPt")!=0) { // 3D Phi Pt proj.
	TH2D* projZY = (TH2*)hist3D->Project3D("zy");
	projZY->SetName(histProjName->Data());
	projZY->SetYTitle("azimuthal angle (rad");
	projZY->SetXTitle("Pt (GeV)");
	gStyle->SetOptStat(0);
	if (projZY) projZY->Draw("COLZ");
      } else	if (strstr(shortName[pageNumber],"XY")!=0) {    // Vertex XY
	TLine* lineZeroX = new TLine(-1., 0., 1., 0.);
	TLine* lineZero  = new TLine(0., -1., 0., 1.);
	gStyle->SetOptStat(10);
	hist2D->Draw("COLZ");
	lineZeroX->Draw();
	lineZero->Draw();
      } else if (strstr(shortName[pageNumber],"Dedx")!=0) {   // dE/dx
	gStyle->SetOptStat(10);
	gPad  ->SetLogz();
	hist2D->Draw("COLZ");
      } else if (strstr(shortName[pageNumber],"_v")!=0) {    // v
	hist2D->SetMaximum(20.);
	hist2D->SetMinimum(-20.);
	gStyle->SetOptStat(0);
	hist2D->Draw("COLZ");
      } else {                                               // other 2D
	gStyle->SetOptStat(10);
	hist2D->Draw("COLZ");
      }
    } else if (strstr(shortName[pageNumber],".Eta")!=0) { // 2D Eta projection
      if (singleGraph) {
	TH1D* projX = hist2D->ProjectionX(histName->Data(), 0, 9999);
      } else {
	//TH1D* projX = hist2D->ProjectionX(histName->Data(), 0, 9999, "E");
	TH1D* projX = hist2D->ProjectionX(histName->Data(), 0, 9999);
      }
      projX->SetName(histProjName->Data());
      char* xTitle = hist2D->GetXaxis()->GetTitle();
      projX->SetXTitle(xTitle);
      projX->SetYTitle("Counts");
      gStyle->SetOptStat(0);
      if (projX) projX->Draw("H");
      if (!singleGraph) lineZeroY->Draw();
    } else if (strstr(shortName[pageNumber],".Pt")!=0) { // 2D Pt projection
      if (singleGraph) {
	TH1D* projY = hist2D->ProjectionY(histName->Data(), 0, 9999); 
      } else {
	TH1D* projY = hist2D->ProjectionY(histName->Data(), 0, 9999, "E");
	}
      projY->SetName(histProjName->Data());
      projY->SetXTitle("Pt (GeV)");
	projY->SetYTitle("Counts");
	gPad->SetLogy();
	gStyle->SetOptStat(0);
	if (projY) projY->Draw("H");
    } else if (strstr(shortName[pageNumber],"Corr")!=0) { // azimuthal corr.
      float norm = (float)(hist->GetNbinsX()) / hist->Integral(); 
      cout << "  Normalized by: " << norm << endl;
      hist->Scale(norm);                           // normalize height to one
      if (strstr(shortName[pageNumber],"Sub")!=0) { 
	TF1* funcSubCorr = new TF1("SubCorr", SubCorr, 0., twopi/order, 2);
	funcSubCorr->SetParNames("chi", "har");
	funcSubCorr->SetParameters(1., order);             // initial value
	funcSubCorr->SetParLimits(1, 1, 1);                // har is fixed
	hist->Fit("SubCorr");
	delete funcSubCorr;
      } else {
	TF1* funcCos2 = new TF1("funcCos2",
           "1+[0]*2/100*cos([2]*x)+[1]*2/100*cos(([2]+1)*x)", 0., twopi/order);
	funcCos2->SetParNames("k=1", "k=2", "har");
	funcCos2->SetParameters(0, 0, order);              // initial values
	funcCos2->SetParLimits(2, 1, 1);                   // har is fixed
	hist->Fit("funcCos2");
	delete funcCos2;
      }
      if (strstr(shortName[pageNumber],"Phi")!=0)
	hist->SetMinimum(0.9*(hist->GetMinimum()));
      gStyle->SetOptStat(10);
      gStyle->SetOptFit(111);
      hist->Draw("E1");
    } else if (strstr(shortName[pageNumber],"_q")!=0) {    // q distibution
      gStyle->SetOptStat(10);
      gStyle->SetOptFit(111);
      double area = hist->Integral() * qMax / (float)n_qBins; 
      TString* histMulName = new TString("Flow_Mul_Sel");
      histMulName->Append(*sel);
      histMulName->Append("_Har");
      histMulName->Append(*har);
      TH1* histMult = dynamic_cast<TH1*>(histFile[fileN]->Get(histMulName->Data()));
      if (!histMult) {
	cout << "### Can't find histogram " << histMulName->Data() << endl;
	return can;
      }
      delete histMulName;
      float mult = histMult->GetMean();
      TF1* fit_q = new TF1("qDist", qDist, 0., qMax, 4);
      fit_q->SetParNames("v", "mult", "area", "g");
      float qMean = hist->GetMean();
      float v2N = (qMean > 1.) ? qMean - 1. : 0.;
      float vGuess = 100. * sqrt(v2N / mult);
      fit_q->SetParameters(vGuess, mult, area, 0.3); // initial values
      fit_q->SetParLimits(1, 1, 1);               // mult is fixed
      fit_q->SetParLimits(2, 1, 1);               // area is fixed
      //fit_q->FixParameter(3, 0.6);              // g is fixed
      hist->Fit("qDist");
      fit_q->Draw("same");
    } else if (strstr(shortName[pageNumber],"Phi")!=0) {  // Phi distibutions
      hist->SetMinimum(0.9*(hist->GetMinimum()));
      if (strstr(shortName[pageNumber],"Weight")!=0) {
	TLine* lineOnePhi  = new TLine(0., 1., phiMax, 1.);
	gStyle->SetOptStat(0);
	hist->Draw(); 
	lineOnePhi->Draw();
      } else {
	gStyle->SetOptStat(10);
	hist->Draw(); 
      }
    } else if (strstr(shortName[pageNumber],"Psi")!=0) {    // Psi distibutions
      gStyle->SetOptStat(10);
      hist->Draw("E1"); 
    } else if (strstr(shortName[pageNumber],"Eta")!=0) {    // Eta distibutions
      if (strstr(shortName[pageNumber],"_v")!=0 ) {
	hist->SetMaximum(10.);
	hist->SetMinimum(-10.);
      }
      gStyle->SetOptStat(100110);
      hist->Draw();
      lineZeroY->Draw();
      lineYcm->Draw();
    } else if (strstr(shortName[pageNumber],"Pt")!=0) {     // Pt distibutions
      if (strstr(shortName[pageNumber],"_v")!=0 ) {
	hist->SetMaximum(30.);
	hist->SetMinimum(-5.);
      }
      gStyle->SetOptStat(100110);
      hist->Draw();
      if (strstr(shortName[pageNumber],"v")!=0) {
	TLine* lineZeroPt  = new TLine(0., 0., ptMax, 0.);
	lineZeroPt->Draw();
      }
    } else if (strstr(shortName[pageNumber],"Bin")!=0) {    // Bin hists
      if (strstr(shortName[pageNumber],"Pt")!=0) {
	TLine* lineDiagonal = new TLine(0., 0., ptMax, ptMax);
      } else {
	TLine* lineDiagonal = new TLine(-etaMax, -etaMax, etaMax, etaMax);
      }
      gStyle->SetOptStat(0);
      hist->SetMarkerStyle(21);
      hist->SetMarkerColor(2);
      hist->Draw();
      lineDiagonal->Draw();
    } else if (strstr(shortName[pageNumber],"CosPhi")!=0) {  // CosPhiLab
      TLine* lineZeroHar = new TLine(0.5, 0., 3.5, 0.);
      gStyle->SetOptStat(0);
      hist->Draw();
      lineZeroHar->Draw();
    } else if (strstr(shortName[pageNumber],"PidMult")!=0) {  // PID Mult
      gPad->SetLogy();
      gStyle->SetOptStat(0);
      hist->Draw();
    } else if (strstr(shortName[pageNumber],"_v")!=0 ) {      // v 1D
      TLine* lineZeroHar = new TLine(0.5, 0., 3.5, 0.);
      hist->SetMaximum(10.);
      gStyle->SetOptStat(0);
      hist->Draw();
      lineZeroHar->Draw();
      for (int n=1; n < 4; n++) {
	v   = hist->GetBinContent(n);                       // output v values
	err = hist->GetBinError(n);
	if (n==2) cout << " v2 = " << v << " +/- " << err << endl;
	if (TMath::IsNaN(v)) {
	  hist->SetBinContent(n, 0.);
	  hist->SetBinError(n, 0.);
	}
      }
    } else {                                              // all other 1D
      gStyle->SetOptStat(100110);
      hist->Draw(); 
    }
  }
  
  delete [] temp;
  delete histName;
  if (histProjName) delete histProjName;
  for (int m = 0; m < nNames; m++) {  
    delete [] shortName[m];
  }
  delete shortName[];
  
  return c;
}

void plotCenAll(Int_t nNames, Int_t selN, Int_t harN, Int_t first = 1) {
  for (int i =  first; i < nNames + 1; i++) {
    c = plotCen(i, selN, harN);
    c->Update();
    cout << "save? y/[n], quit? q" << endl;
    fgets(tmp, sizeof(tmp), stdin);
    if (strstr(tmp,"y")!=0) c->Print(".ps");
    else if (strstr(tmp,"q")!=0) return;
  }
  cout << "  Done" << endl;
}

//-----------------------------------------------------------------------

static Double_t qDist(double* q, double* par) {
  // Calculates the q distribution given the parameters v, mult, area, g

  double sig2 = 0.5 * (1. + par[3]);
  double expo = (par[1]*par[0]*par[0]/10000. + q[0]*q[0]) / (2*sig2);
  Double_t dNdq = par[2] * (q[0]*exp(-expo)/sig2) * 
    TMath::BesselI0(q[0]*par[0]/100.*sqrt(par[1])/sig2);

  return dNdq;
}

//-----------------------------------------------------------------------

static Double_t SubCorr(double* x, double* par) {
  // Calculates the n(Psi_a - Psi_b) distribution by fitting chi
  // From J.-Y. Ollitrault, Nucl. Phys. A590, 561c (1995), Eq. 6. with correc.
  // The Struve functions are included.

  double chi2 = par[0] * par[0] / 2;     // divide by two for SV chi
  double z = chi2 * cos(par[1]*x[0]);
  double TwoOverPi = 2./TMath::Pi();

  Double_t dNdPsi = exp(-chi2)/TwoOverPi * (TwoOverPi*(1.+chi2) 
                    + z*(TMath::BesselI0(z) + StruveL0(z))
                    + chi2*(TMath::BesselI1(z) + StruveL1(z)));

  return dNdPsi;
}

//-----------------------------------------------------------------------

static Double_t StruveL1(Double_t x)
{
  // Modified Struve Function of Order One
  //

  const Double_t pi=TMath::Pi();
  Double_t a1,sl1,bi1,s;
  Double_t r=1.0;
  Int_t km;
  
  if (x<=20.) {
    s=0.0;
    for (int i=1; i<=60;i++){
      r*=x*x/(4.0*i*i-1.0);
      s+=r;
      if(TMath::Abs(r)<TMath::Abs(s)*1.e-12)break;
    }
    sl1=2.0/pi*s;
  }else{
    s=1.0;
    km=int(0.5*x);
    if(x>50.0)km=25;
    for (int i=1; i<=km; i++){
      r*=(2*i+3)*(2*i+1)/x/x;
      s+=r;
      if(TMath::Abs(r/s)<1.0e-12)break;
    }
    sl1=2.0/pi*(-1.0+1.0/(x*x)+3.0*s/(x*x*x*x));
    a1=TMath::Exp(x)/TMath::Sqrt(2*pi*x);
    r=1.0;
    bi1=1.0;
    for (int i=1; i<=16; i++){
      r=-0.125*r*(4.0-(2.0*i-1.0)*(2.0*i-1.0))/(i*x);
      bi1+=r;
      if(TMath::Abs(r/bi1)<1.0e-12)break;
    }
    sl1+=a1*bi1;
  }
  
  return sl1;
  
}

static Double_t StruveL0(Double_t x)
{
  // Modified Struve Function of Order Zero
  //
  
  const Double_t pi=TMath::Pi();
  
  Double_t s=1.0;
  Double_t r=1.0;
  
  Double_t a0,sl0,a1,bi0;
  
  Int_t km;
  
  if (x<=20.) {
    a0=2.0*x/pi;
    for (int i=1; i<=60;i++){
      r*=(x/(2*i+1))*(x/(2*i+1));
      s+=r;
      if(TMath::Abs(r/s)<1.e-12)break;
    }
    sl0=a0*s;
  }else{
    km=int(5*(x+1.0));
    if(x>=50.0)km=25;
    for (int i=1; i<=km; i++){
      r*=(2*i-1)*(2*i-1)/x/x;
      s+=r;
      if(TMath::Abs(r/s)<1.0e-12)break;
    }
    a1=TMath::Exp(x)/TMath::Sqrt(2*pi*x);
    r=1.0;
    bi0=1.0;
    for (int i=1; i<=16; i++){
      r=0.125*r*(2.0*i-1.0)*(2.0*i-1.0)/(i*x);
      bi0+=r;
      if(TMath::Abs(r/bi0)<1.0e-12)break;
    }
    
    bi0=a1*bi0;
    sl0=-2.0/(pi*x)*s+bi0;
  }
  
  return sl0;
  
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: plotCen.C,v $
// Revision 1.16  2003/05/06 21:33:08  posk
// Removed some histograms.
//
// Revision 1.15  2003/03/18 17:58:38  posk
// Kirill Fillimonov's improved fit to the angle between subevent planes.
//
// Revision 1.14  2003/03/17 20:46:57  posk
// Improved fit to q dist.
//
// Revision 1.13  2003/03/11 23:04:31  posk
// Includes scalar product hists.
//
// Revision 1.12  2003/02/25 19:25:33  posk
// Improved plotting.
//
// Revision 1.11  2003/01/10 16:40:55  oldi
// Several changes to comply with FTPC tracks:
// - Switch to include/exclude FTPC tracks introduced.
//   The same switch changes the range of the eta histograms.
// - Eta symmetry plots for FTPC tracks added and separated from TPC plots.
// - PhiWgts and related histograms for FTPC tracks split in FarEast, East,
//   West, FarWest (depending on vertex.z()).
// - Psi_Diff plots for 2 different selections and the first 2 harmonics added.
// - Cut to exclude mu-events with no primary vertex introduced.
//   (This is possible for UPC events and FTPC tracks.)
// - Global DCA cut for FTPC tracks added.
// - Global DCA cuts for event plane selection separated for TPC and FTPC tracks.
// - Charge cut for FTPC tracks added.
//
// Revision 1.10  2001/12/11 22:04:17  posk
// Four sets of phiWgt histograms.
// StFlowMaker StFlowEvent::PhiWeight() changes.
// Cumulant histogram names changed.
//
// Revision 1.9  2001/11/09 21:15:11  posk
// Switched from CERNLIB to TMath. Using global dca instead of dca.
//
// Revision 1.8  2001/05/22 20:11:24  posk
// Changed dEdx graphs.
//
// Revision 1.7  2000/12/12 15:01:11  posk
// Put log comments at end of file.
//
// Revision 1.6  2000/12/08 17:04:09  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.3  2000/09/15 22:52:56  posk
// Added Pt weighting for event plane calculation.
//
// Revision 1.1  2000/08/31 18:50:33  posk
// Added plotCen.C to plot from a series of files with different centralities.
//
///////////////////////////////////////////////////////////////////////////////

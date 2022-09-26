/*  Compare dE/dx distributions from Bichsel with dN/dx parameerizations
    root.exe lBichsel.C dNdxFunc.C
*/
#include "TMath.h"
#include "TF1.h"
#include "StBichsel/StdEdxModel.h"
#include "StBichsel/Bichsel.h"
#include "TLegend.h"
#include "TCanvas.h"
#include "TArrayD.h"
#include "TGraph.h"
#include "TAxis.h"
#include "TH2.h"
#include "TObjArray.h"
#include "TSystem.h"
#include "TLegend.h"
#include "Ask.h"
#include "Names.h"
#include "TDatime.h"
Bichsel *m_Bichsel = 0;
//________________________________________________________________________________
Double_t bichselZ(Double_t *x,Double_t *par) {
  return m_Bichsel->GetMostProbableZ(x[0],par[0]);
}
//________________________________________________________________________________
TF1 *ZMPB(Double_t log2dx = 1) {
  TF1 *f = 0;
  if (! f) {
    f = new TF1(Form("B%i",(int)log2dx+2),bichselZ,-1,4,1);
    f->SetParName(0,"log2dx");
    f->SetParameter(0,log2dx);
  }
  return f;
}
//________________________________________________________________________________
void dNdxFunctions(Double_t log2dx = 1) {
  if (!m_Bichsel) m_Bichsel = Bichsel::Instance();
  TCanvas *c1 = new TCanvas("c1","c1");
  TLegend *l = new TLegend(0.4,0.6,0.8,0.9);
  TH1F *frame = c1->DrawFrame(-2.5,0.5,5,9);
  frame->SetTitle("The most probable log(dE/dx[keV/cm]) versu log_{10}(#beta #gamma)");
  frame->SetXTitle("log_{10}(#beta #gamma)");

  //  for (Int_t color = 1; color < 8; color++) {
  for (Int_t color = 2; color <= 4; color++) {
    Double_t log2dx = color - 2;
    Double_t dx = TMath::Power(2.,log2dx);
#if 0
    TF1 *fnOld = StdEdxModel::ZMPold(log2dx);
    fnOld->SetLineColor(color);
    fnOld->SetMarkerColor(color);
    fnOld->Draw("same");
    l->AddEntry(fnOld,Form("%4.1fcm Old",dx));
#endif
    TF1 *fn = StdEdxModel::ZMP(log2dx);
    fn->SetLineColor(color);
    fn->SetMarkerColor(color);
    fn->Draw("same");
    l->AddEntry(fn,Form("%4.1fcm",dx));
#if 0
    TF1 *fr = StdEdxModel::ZMPR(log2dx);
    fr->SetLineColor(color);
    fr->SetMarkerColor(color);
    fr->Draw("same");
    l->AddEntry(fr,Form("R%4.1fcm",dx));
#endif
#if 1
    TF1 *bn = ZMPB(log2dx);
    bn->SetLineColor(color);
    bn->SetMarkerColor(color);
    bn->Draw("same");
    l->AddEntry(bn,Form("Bichel %4.1fcm",dx));
#endif
  }
  l->Draw();
}
//________________________________________________________________________________
Double_t checkParameters(Double_t *x, Double_t *p) {
  Double_t X = TMath::Exp(x[0]);
  Int_t k = p[0];
  return StdEdxModel::instance()->Parameter(X, k);
    }
//________________________________________________________________________________
TF1 *CheckParameters(Int_t k = 0) {
  static TF1 *f = 0;
  if (!f) {
    f = new TF1("CheckParamer",checkParameters,3,9,1);
  }
  f->SetParameter(0,k);
  return f;
}
//________________________________________________________________________________
Double_t scanleFCn(Double_t *x, Double_t *p) {
  Double_t pMoMIP = 0.526;
  Double_t bg10pi = TMath::Log10(pMoMIP/0.13956995);// =  5.76193821239086468e-01
  Double_t bg10P = TMath::Log10(pMoMIP/0.93827231); //  = -2.51343155597725409e-01;
  Double_t difMIP = 1.18931 - (-0.03569); // Positive Outer
  StdEdxModel::SetScale(x[0]);
  TF1 *F = StdEdxModel::ZMP(1.0);
  Double_t  diff1 =  F->Eval(bg10P) - F->Eval(bg10pi) - difMIP;
  return diff1;
}
//________________________________________________________________________________
TF1 *Fdif() {
  TF1 *f = new TF1("FDIFF",scanleFCn,0.5,2.,0);
  return f;
}
//________________________________________________________________________________
TGraph *dNdxBeta() {
  TH1D *dNdx = StdEdxModel::instance()->GetdNdx();
  Int_t nx = dNdx->GetNbinsX();
  TArrayD X(nx);
  TArrayD Y(nx);
  for (Int_t i = 1; i <= nx; i++) {
    Double_t bg = dNdx->GetXaxis()->GetBinCenter(i);
    Double_t beta = bg/TMath::Sqrt(1 + bg*bg);
    X[i-1] = beta;
    Y[i-1] = dNdx->GetBinContent(i);
  }
  TGraph *gr = new TGraph(nx,X.GetArray(), Y.GetArray());
  return gr;
}
//________________________________________________________________________________
TGraph *dNdxBetaLogLog() {
  TH1D *dNdx = StdEdxModel::instance()->GetdNdx();
  Int_t nx = dNdx->GetNbinsX();
  TArrayD X(nx);
  TArrayD Y(nx);
  for (Int_t i = 1; i <= nx; i++) {
    Double_t bg = dNdx->GetXaxis()->GetBinCenter(i);
    Double_t beta = bg/TMath::Sqrt(1 + bg*bg);
    X[i-1] = TMath::Log(beta);
    Y[i-1] = TMath::Log(dNdx->GetBinContent(i));
  }
  TGraph *gr = new TGraph(nx,X.GetArray(), Y.GetArray());
  return gr;
}
//________________________________________________________________________________
Double_t LdNdxlgamFunc(Double_t *x, Double_t *p) {
  Double_t lgam = x[0];
  Double_t gamma = TMath::Power(10.,lgam) + 1;
  Double_t betaGamma = TMath::Sqrt(gamma*gamma - 1.);
  return TMath::Log(StdEdxModel::instance()->dNdx(betaGamma, p[0]));
}
//________________________________________________________________________________
TF1 *LdNdxlgam(Int_t charge = 1) {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("LdNdxlgam", LdNdxlgamFunc, -3., 5., 1);
    f->SetParameter(0, charge);
  }
  return f;
}
//________________________________________________________________________________
Double_t LdNdxlgamFuncHeed(Double_t *x, Double_t *p) {
  static TH1D *dNdxL10 = 0;
  if (! dNdxL10) {
    TFile *f = new TFile("$STAR/StarDb/dEdxModel/dNdx_Heed.root");
    if (! f ) return 0;
    dNdxL10 = (TH1D*) f->Get("dNdxL10");
    if (! dNdxL10) return 0;
  }
  Double_t lgam = x[0];
  Double_t gamma = TMath::Power(10.,lgam) + 1;
  Double_t betaGamma = TMath::Sqrt(gamma*gamma - 1.);
  return TMath::Log(p[0]*p[0]*dNdxL10->Interpolate(TMath::Log10(betaGamma)));
}
//________________________________________________________________________________
TF1 *LdNdxlgamHeed(Int_t charge = 1) {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("LdNdxlgamHeed", LdNdxlgamFuncHeed, -3., 5., 1);
    f->SetParameter(0, charge);
  }
  return f;
}
//________________________________________________________________________________
TGraph *dNdxBetaLogLogHeed() {
  TFile *f = new TFile("$STAR/StarDb/dEdxModel/dNdx_Heed.root");
  if (! f ) return 0;
  TH1D *dNdxL10 = (TH1D*) f->Get("dNdxL10");
  Int_t nx = dNdxL10->GetNbinsX();
  TArrayD X(nx);
  TArrayD Y(nx);
  for (Int_t i = 1; i <= nx; i++) {
    Double_t bgL10 = dNdxL10->GetXaxis()->GetBinCenter(i);
    Double_t bg  = TMath::Power(10., bgL10);
    Double_t beta = bg/TMath::Sqrt(1 + bg*bg);
    X[i-1] = TMath::Log(beta);
    Y[i-1] = TMath::Log(1.2*dNdxL10->GetBinContent(i)); // +20% to match Bichsel
  }
  TGraph *gr = new TGraph(nx,X.GetArray(), Y.GetArray());
  gr->SetMarkerColor(2);
  return gr;
}
//________________________________________________________________________________
Double_t LdNdxL10bgFunc(Double_t *x, Double_t *p) {
  Double_t L10bg = x[0];
  //  Double_t betaGamma = TMath::Abs(p[0])*TMath::Power(10., L10bg);
  Double_t betaGamma = TMath::Power(10., L10bg);
  return TMath::Log(StdEdxModel::instance()->dNdx(betaGamma, p[0])) + p[1];
}
//________________________________________________________________________________
TF1 *LdNdxL10bg(Int_t charge = 1) {
  TString Name(Form("dNdxL10bg_%i",charge));
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(Name);
  if (! f) {
    f = new TF1(Form(Name,charge), LdNdxL10bgFunc, -3., 5., 2);
    f->SetParameter(0, charge);
    f->SetParameter(1,      0);
    f->SetLineColor(TMath::Abs(charge));
  }
  return f;
}
//________________________________________________________________________________
void DrawdNdxL10bg(Int_t charge = 1, Double_t factor = 0.5) {
  TF1 *f0 = LdNdxL10bg(charge);
  f0->Draw("same");
  TF1 *f1 = new TF1(*f0); f1->SetName(Form("%s+10",f0->GetName()));
  f1->SetParameter(1,factor);
  f1->SetLineColor(2);
  f1->Draw("same");
  TF1 *f2 = new TF1(*f0); f1->SetName(Form("%s+10",f0->GetName()));
  f2->SetParameter(1,- factor);
  f2->SetLineColor(2);
  f2->Draw("same");
}
/*
root.exe [2] TGraph *gb = dNdxBetaLogLog()
root.exe [3] gb->Draw("axp")
root.exe [4] TGraph *gh = dNdxBetaLogLogHeed()
root.exe [5] gh->Draw("p")
root.exe [6] gb->Fit("pol1","er","",-5,-2)

****************************************
Minimizer is Minuit / Migrad
Chi2                      =    0.0588655
NDf                       =           10
Edm                       =   7.9069e-21
NCalls                    =           51
p0                        =      3.82557   +/-   0.0953801   
p1                        =     -1.53436   +/-   0.027871    
(class TFitResultPtr)77739248
root.exe [7] gh->Fit("pol1","er","",-5,-2)

****************************************
Minimizer is Minuit / Migrad
Chi2                      =    0.0456108
NDf                       =           10
Edm                       =  2.02813e-21
NCalls                    =           47
p0                        =      3.80815   +/-   0.0848441   
p1                        =     -1.54108   +/-   0.0252986   
*/
//________________________________________________________________________________
Double_t dXCorrectionFunc(Double_t *x, Double_t *p) { // dX correction due to multiple scattering
  Double_t bgL10 = x[0];
  Double_t charge = p[0];
  Double_t mass   = p[1];
  Double_t cor0   = p[2];
  Double_t cor1   = p[3];
  Double_t bg     = TMath::Power(10., bgL10);// bg**2 =  b**2/(1 - b**2); bg**2 - bg**2*b**2 = b**2; b**2 = bg**2/(1 + bg**2)
  Double_t bg2    = bg*bg;
  Double_t beta2  = bg2/(1 + bg2);
  Double_t P      = mass*bg;
  Double_t Cor    = cor0 + cor1*charge*charge/(beta2*P*P);
  return - TMath::Log(1. + Cor);
}
//________________________________________________________________________________
TF1 *dXCorrection(Int_t charge=2, Double_t mass=3.727417) {
  TString Name(Form("dXCor_%i_%i", TMath::Abs(charge), (Int_t) 10*mass));
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(Name);
  if (! f) {
    f = new TF1(Name,dXCorrectionFunc , -3., 5., 4);
    f->SetParNames("charge","mass","cor0","cor1");
    f->FixParameter(0, charge);
    f->FixParameter(1, mass);
    f->SetParameter(2, 0.0);
    f->SetParameter(3, 0.0);
    f->SetLineColor(TMath::Abs(charge));
  }
  return f;
}
//________________________________________________________________________________
TString TF1Print(TF1 *sat=0, const Char_t *partName = "") {
  TString Line;
  if (sat) {
    Double_t *pars = sat->GetParameters();
    Int_t N = sat->GetNpar();
    Double_t chisq = sat->GetChisquare();
    Int_t NDF = sat->GetNDF();
    sat->GetParameters(pars);
    Line = Form("  Double_t pars[%i] = {",N);
    for (Int_t i = 0; i < N; i++) {
      Line += Form("%10.5g", pars[i]);
      if (i == N - 1) {Line += "}; // "; Line += partName; Line += Form("\tchisq = %f / NDF = %i",chisq,NDF);}
      else            {Line += ", ";}
    }
  }
  return Line;
}
//________________________________________________________________________________
void dNdxCorrections() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TF1 *pol7 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol7");
  if (! pol7) {
    TF1::InitStandardFunctions();
    pol7 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol7");
  }
  Double_t pars[8] = {  -0.07368,  0.0070045,  0.0056807, -0.0045777, -0.0030487,  0.0027636, -0.00067063,  5.342e-05}; //
  pol7->SetParameters(pars);
  TString Out = "dNdxCorrections.data";
  ofstream out;
  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              out.open(Out, ios::app);
  TDatime t;
  out  << "// " << gSystem->WorkingDirectory() << "\t" << t.AsString()  << endl;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TH1D **part = new TH1D*[nn];
  TIter next(files);
  TFile *f = 0;
  Int_t i = 0;
  TFile *fOut = new TFile("dNdxCorrections.root","recreate");
  TObjArray *arr = new TObjArray(4);
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    const Char_t *IO[3] = {"","I","O"};
    for (Int_t io = 0; io < 3; io++) {
      TH2F *dNdxVsBgC = (TH2F *) f->Get(Form("dNdxVsBgC%s",IO[io]));
      part[i] = 0;
      if (! dNdxVsBgC) continue;
      TString partName(gSystem->DirName(f->GetName()));
      partName += IO[io];
      if (dNdxVsBgC->GetEntries() <10) {
	cout << dNdxVsBgC->GetName() << " for " << partName.Data() << " is empty" << endl;
	continue;
      }
      TH1D *projX = dNdxVsBgC->ProjectionX();
      Int_t kx1 = projX->FindFirstBinAbove(100);
      Int_t kx2 = projX->FindLastBinAbove(100);
      if (kx1 >= kx2) continue;
      //    dNdxVsBgC->FitSlicesY(0, kx1, kx2, 0, "QNRg3s", arr);
      dNdxVsBgC->FitSlicesY(0, kx1, kx2, 0, "QNRg5s", arr);
      TH1D *hist = (TH1D *) (*arr)[1];
      TCanvas *c2D = new TCanvas(partName+"2D",partName+"2D");
      c2D->SetLogz(1);
      dNdxVsBgC->Draw("colz");
      Int_t color = i%6 + 1;
      Int_t Marker = 20 + i/6;
      hist->SetMarkerColor(color);
      hist->SetMarkerStyle(Marker);
      c2D->Update();
      fOut->cd();
      part[i] = new TH1D(*hist); part[i]->SetName(partName);
      //    part[i]->Draw();
      c2D->Update();
      Int_t nx = part[i]->GetXaxis()->GetNbins();
      Int_t k1 = nx;
      Int_t k2 = 1;
      Int_t nn = 0;
      for (Int_t k = 1; k <= nx; k++) {
	//	Double_t v = part[i]->GetBinContent(k);
	Double_t x = part[i]->GetBinCenter(k);
	Double_t v = part[i]->GetBinContent(k) - pol7->Eval(x);
	Double_t e = part[i]->GetBinError(k);
	if (e < 1e-6 || TMath::Abs(v) < 3*e) {//  || v > -0.055 | TMath::Abs(v+0.07) > 0.02) {
	  part[i]->SetBinContent(k, 0);
	  part[i]->SetBinError(k, 0);
	  continue;
	}
	part[i]->SetBinContent(k,v);
	k1 = TMath::Min(k, k1);
	k2 = TMath::Max(k, k1);
	nn++;
      }
      if (k1 < k2) {
	dNdxVsBgC->GetXaxis()->SetRange(k1,k2);
	part[i]->GetXaxis()->SetRange(k1,k2);
      }
      Double_t fMass = -1;
      Int_t   fCharge = 0;
      for (Int_t h = 0; h < NTpcRSParts; h++) {
	if (TString(TpcRSPart[h].name).Contains(partName, TString::kIgnoreCase)) {
	  fMass = TpcRSPart[h].mass;
	  fCharge = TpcRSPart[h].charge;
	  break;
	}
      }
      Int_t charge = fCharge;
#if 0
      if      (partName.Contains("alpha",TString::kIgnoreCase) || 
	       partName.Contains("He",TString::kIgnoreCase)) charge = 2;
      else if (partName.Contains("Li",TString::kIgnoreCase)) charge = 3;
      else if (partName.Contains("Be",TString::kIgnoreCase)) charge = 4;
      else if (partName.Contains("B",TString::kIgnoreCase)) charge = 5;
      /*
	alpha bgL10Min -1.4
	deuteron       -1.5
	electron        1.0
	He3            -1.4
	kaon           -1.2
	muon           -0.8
	proton         -1.4
	triton        -1.6
      */
#endif
#if 0
      TF1 *lg = LdNdxL10bg(charge);
      TAxis *xax = part[i]->GetXaxis();
      for (Int_t k = kx2; k >= kx1; k--) {
	Double_t xx = xax->GetBinCenter(k);
	Double_t v = part[i]->GetBinContent(k);
	Double_t f = lg->Eval(xx);
	part[i]->SetBinContent(k, v - f);
      }
#endif
      part[i]->SetMarkerColor(color);
      part[i]->SetMarkerStyle(Marker);
      hist->Draw("same");
      c2D->Update();
      if (Ask()) break;
      TCanvas *c = new TCanvas("c"+partName,"c"+partName);
      part[i]->Draw();
      TF1 *sat = StdEdxModel::Saturation();
      sat->SetParameters(-0.07,     1.,    1.,     part[i]->GetMean(),  0.0, 0.0, 0.0);
      sat->SetParLimits(2, -10, 10);
      Double_t xmin = part[i]->GetXaxis()->GetBinLowEdge(k1);
      Double_t xmax = part[i]->GetXaxis()->GetBinUpEdge(k2);
      sat->SetParLimits(3, xmin, xmax);
      sat->FixParameter(4, 0);
      sat->FixParameter(5, 0);
      sat->FixParameter(6, 0);
      part[i]->Fit(sat);
      TString Line;
#if 0
      sat->ReleaseParameter(4);
      part[i]->Fit(sat);
      Line = TF1Print(sat, partName);
      cout << Line << endl;
      out  << Line << endl;
      sat->ReleaseParameter(5);
      part[i]->Fit(sat);
      Line = TF1Print(sat, partName);
      cout << Line << endl;
      out  << Line << endl;
      sat->ReleaseParameter(6);
#endif
      part[i]->Fit(sat);
      Line = TF1Print(sat, partName);
      cout << Line << endl;
      out  << Line << endl;
      i++;
      c->Update();
      if (Ask()) break;
    }
  }
  TCanvas *c1 = new TCanvas("c1","Summary");
  TH1F *frame = c1->DrawFrame(-1,-0.2,5,0.0);
  frame->SetTitle("log((nP/dX)/(dN/dx)) versus log_{10}(#beta #gamma) at the first hit");
  frame->SetXTitle(" log_{10}(#beta #gamma)");
  TLegend *l = new TLegend(0.2,0.4,0.5,0.8);
  for (Int_t j = 0; j < i; j++) {part[j]->SetStats(0); part[j]->Draw("same"); l->AddEntry(part[j], part[j]->GetName());}
  l->Draw();
  fOut->Write();
  out.close();
}
//________________________________________________________________________________
void dXdS() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TH1D **part = new TH1D*[nn];
  TIter next(files);
  TFile *f = 0;
  Int_t i = 0;
  const Char_t *IO[2] = {"I","O"};
  //  TFile *fOut = new TFile("dXdSCorrections.root","recreate");
  TObjArray *arr = new TObjArray(4);
  TCanvas *c1 = new TCanvas("c1","dX/dS");
  TH1F *frame = c1->DrawFrame(-1.5,-0.1,5,0.1);
  TLegend *l = new TLegend(0.6,0.6,0.9,0.9);
  l->Draw();
  Int_t color = 0;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    color++;
    for (Int_t io = 0; io < 2; io++) {
      TString name(Form("dXdSR%s",IO[io]));
      TH2F *dXdSR  = (TH2F *) f->Get(name);
      if (! dXdSR) continue;
      dXdSR->FitSlicesY(0, 0, -1, 0, "QNRg3s", arr);
      TH1D *h = (TH1D *) (*arr)[1];
      if (! h) continue;
      TString partName(gSystem->DirName(f->GetName()));
      h->SetMarkerColor(color); 
      h->SetMarkerStyle(20+io);
      h->Draw("same");
      l->AddEntry(h,Form("%s %s",partName.Data(),IO[io]));
      c1->Update();
    }
  }
}
//________________________________________________________________________________
void dPCorrections() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TString Out = "dPLCorrections.data";
  ofstream out;
  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              out.open(Out, ios::app);
  TDatime t;
  out  << "// " << gSystem->WorkingDirectory() << "\t" << t.AsString()  << endl;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TH1D **part = new TH1D*[nn];
  TIter next(files);
  TFile *f = 0;
  Int_t i = 0;
  TFile *fOut = new TFile("dPLCorrections.root","recreate");
  TObjArray *arr = new TObjArray(4);
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    TH2F *dNdxVsBgC = (TH2F *) f->Get("dNdxVsBgC");
    part[i] = 0;
    if (! dNdxVsBgC) continue;
    TString partName(gSystem->DirName(f->GetName()));
    if (dNdxVsBgC->GetEntries() <10) {
      cout << dNdxVsBgC->GetName() << " for " << partName.Data() << " is empty" << endl;
      continue;
    }
    TH1D *projX = dNdxVsBgC->ProjectionX();
    Int_t kx1 = projX->FindFirstBinAbove(100);
    Int_t kx2 = projX->FindLastBinAbove(100);
    if (kx1 >= kx2) continue;
    //    dNdxVsBgC->FitSlicesY(0, kx1, kx2, 0, "QNRg3s", arr);
    dNdxVsBgC->FitSlicesY(0, kx1, kx2, 0, "QNRg5s", arr);
    TH1D *hist = (TH1D *) (*arr)[1];
    TCanvas *c2D = new TCanvas(partName+"2D",partName+"2D");
    c2D->SetLogz(1);
    dNdxVsBgC->Draw("colz");
    Int_t color = i%6 + 1;
    Int_t Marker = 20 + i/6;
    hist->SetMarkerColor(color);
    hist->SetMarkerStyle(Marker);
    c2D->Update();
    fOut->cd();
    part[i] = new TH1D(*hist); part[i]->SetName(partName);
    //    part[i]->Draw();
    c2D->Update();
    Int_t nx = part[i]->GetXaxis()->GetNbins();
    Int_t k1 = nx;
    Int_t k2 = 1;
    Int_t nn = 0;
    for (Int_t k = 1; k <= nx; k++) {
      Double_t x = part[i]->GetBinCenter(k);
      Double_t v = part[i]->GetBinContent(k); // - pol7->Eval(x);
      Double_t e = part[i]->GetBinError(k);
      if (e < 1e-6 || TMath::Abs(v) < 3*e) {//  || v > -0.055 | TMath::Abs(v+0.07) > 0.02) {
	part[i]->SetBinContent(k, 0);
	part[i]->SetBinError(k, 0);
	continue;
      }
      k1 = TMath::Min(k, k1);
      k2 = TMath::Max(k, k1);
      nn++;
    }
    if (k1 < k2) {
      dNdxVsBgC->GetXaxis()->SetRange(k1,k2);
      part[i]->GetXaxis()->SetRange(k1,k2);
    }
    Double_t fMass = -1;
    Int_t   fCharge = 0;
    for (Int_t h = 0; h < NTpcRSParts; h++) {
      if (TString(TpcRSPart[h].name).Contains(partName, TString::kIgnoreCase)) {
	fMass = TpcRSPart[h].mass;
	fCharge = TpcRSPart[h].charge;
	break;
      }
    }
    Int_t charge = fCharge;
#if 0
    if      (partName.Contains("alpha",TString::kIgnoreCase) || 
	     partName.Contains("He",TString::kIgnoreCase)) charge = 2;
    else if (partName.Contains("Li",TString::kIgnoreCase)) charge = 3;
    else if (partName.Contains("Be",TString::kIgnoreCase)) charge = 4;
    else if (partName.Contains("B",TString::kIgnoreCase)) charge = 5;
    /*
      alpha bgL10Min -1.4
      deuteron       -1.5
      electron        1.0
      He3            -1.4
      kaon           -1.2
      muon           -0.8
      proton         -1.4
       triton        -1.6
     */
#endif
#if 0
    TF1 *lg = LdNdxL10bg(charge);
    TAxis *xax = part[i]->GetXaxis();
    for (Int_t k = kx2; k >= kx1; k--) {
      Double_t xx = xax->GetBinCenter(k);
      Double_t v = part[i]->GetBinContent(k);
      Double_t f = lg->Eval(xx);
      part[i]->SetBinContent(k, v - f);

    }
#endif
    part[i]->SetMarkerColor(color);
    part[i]->SetMarkerStyle(Marker);
    hist->Draw("same");
    c2D->Update();
    if (Ask()) goto ENDL;
    TCanvas *c = new TCanvas("c"+partName,"c"+partName);
    part[i]->Draw();
    TF1 *sat = StdEdxModel::Saturation();
    sat->SetParameters(-0.07,     1.,    1.,     part[i]->GetMean(),  0.0, 0.0, 0.0);
    sat->SetParLimits(2, -10, 10);
    Double_t xmin = part[i]->GetXaxis()->GetBinLowEdge(k1);
    Double_t xmax = part[i]->GetXaxis()->GetBinUpEdge(k2);
    sat->SetParLimits(3, xmin, xmax);
    sat->FixParameter(4, 0);
    sat->FixParameter(5, 0);
    sat->FixParameter(6, 0);
    part[i]->Fit(sat);
    TString Line;
#if 1
    sat->ReleaseParameter(4);
    part[i]->Fit(sat);
    Line = TF1Print(sat, partName);
    cout << Line << endl;
    out  << Line << endl;
    sat->ReleaseParameter(5);
    part[i]->Fit(sat);
    Line = TF1Print(sat, partName);
    cout << Line << endl;
    out  << Line << endl;
    sat->ReleaseParameter(6);
#endif
    part[i]->Fit(sat);
    Line = TF1Print(sat, partName);
    cout << Line << endl;
    out  << Line << endl;
    i++;
    c->Update();
    if (Ask()) goto ENDL;
  }
 ENDL:
  TCanvas *c1 = new TCanvas("c1","Summary");
  TH1F *frame = c1->DrawFrame(-1,-0.2,5,0.0);
  TLegend *l = new TLegend(0.2,0.4,0.5,0.8);
  for (Int_t j = 0; j < i; j++) {part[j]->SetStats(0); part[j]->Draw("same"); l->AddEntry(part[j], part[j]->GetName());}
  l->Draw();
  fOut->Write();
  out.close();
}
//________________________________________________________________________________
void dNdxFunc() {
  dNdxCorrections();
  //dXdS();
}
/*--------------------------------------------------------------------------------
c1->Divide(2,2)
c1->Clear()
c1->Divide(2,2)
c1->cd(1)->SetLogz(1)
pi->Draw("fRcHit.mdX-fMcHit.mdS:fMcHit.mLgamma>>dSdXOM2D(100,-0.5,1.5,100,-0.2,0.0)","fNoMcHit==1&&fNoRcHit==1&&fMcHit.mAdc>2&&fMcHit.mVolumeId%100>40","colz",100000)
piM->Draw("fRcHit.mdX-fMcHit.mdS:fMcHit.mLgamma>>dSdXOM2D(100,-0.5,1.5,100,-0.2,0.0)","fNoMcHit==1&&fNoRcHit==1&&fMcHit.mAdc>2&&fMcHit.mVolumeId%100>40","colz",100000)
c1->cd(2)
c1->cd(2)->SetLogz(1)
piP->Draw("fRcHit.mdX-fMcHit.mdS:fMcHit.mLgamma>>dSdXOM2D(100,-0.5,1.5,100,-0.2,0.0)","fNoMcHit==1&&fNoRcHit==1&&fMcHit.mAdc>2&&fMcHit.mVolumeId%100>40","colz",100000)
piP->Draw("fRcHit.mdX-fMcHit.mdS:fMcHit.mLgamma>>dSdXOP2D(100,-0.5,1.5,100,-0.2,0.0)","fNoMcHit==1&&fNoRcHit==1&&fMcHit.mAdc>2&&fMcHit.mVolumeId%100>40","colz",100000)
c1->cd(1)
piM->Draw("fRcHit.mdX-fMcHit.mdS:fMcHit.mLgamma>>dSdXOM2D(100,-0.5,1.5,100,-0.2,0.0)","fNoMcHit==1&&fNoRcHit==1&&fMcHit.mAdc>2&&fMcHit.mVolumeId%100>40","colz",100000)
c1->cd(3)
c1->cd(3)->SetLogz(1)
piM->Draw("fRcHit.mdX-fMcHit.mdS:fMcHit.mLgamma>>dSdXIM2D(100,-0.5,1.5,100,-0.2,0.0)","fNoMcHit==1&&fNoRcHit==1&&fMcHit.mAdc>2&&fMcHit.mVolumeId%100<=40","colz",100000)
c1->cd(4)->SetLogz(1)
piP->Draw("fRcHit.mdX-fMcHit.mdS:fMcHit.mLgamma>>dSdXIP2D(100,-0.5,1.5,100,-0.2,0.0)","fNoMcHit==1&&fNoRcHit==1&&fMcHit.mAdc>2&&fMcHit.mVolumeId%100<=40","colz",100000)
  
  --------------------------------------------------------------------------------*/

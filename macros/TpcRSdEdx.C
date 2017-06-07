/* FPE_OFF
   root.exe Heed*_G.root lBichsel.C TpcRSdEdx.C+
*/
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
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TF1.h"
#include "TF2.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
#include "TString.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#include "TGraphErrors.h"
#include "TGraph2DErrors.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#endif
const Int_t NHYP = 15; // 10
const Char_t *names[15] = {"electron","positron","muon+","muon-","pion+","pion-","kaon+","kaon-","proton","pbar"  ,"deuteron","triton","He3","alpha","pionMIP"};
//const Char_t *names[15] = {"electron","positron","muon","muon","pion+","pion-","kaon","kaon","proton","pbar"  ,"deuteron","triton","He3","alpha","all"};
const Char_t *namesh[15]= {"eN"      ,"eP"      ,"muP"  ,"muN",  "piP"  ,"piN",  "kaonP","kaonN","protonP","protonN","deuteronP","tritonP","He3P","alphaP","piP"};
const Double_t masses[15] = {0.51099907e-3,0.51099907e-3,
			     0.1056584,0.1056584,
			     0.13956995,0.13956995,
			     0.493677,0.493677,
			     0.93827231,0.93827231,
			     1.875613   ,2.80925  , 2.80923, 3.727417,
			     0.13956995
};
const Double_t chargeSQ[14] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4};
TGraphErrors *graphs[10];
TGraphErrors *gAll = 0;
//const Char_t *FMT = "y2011_TpcRS_phys_off_Bichsel_%s.root";
const Char_t *FMT = "Heed_%s_g.root";
//const Double_t params[7] = {0.762e-1, 10.632, 0.134e-4 , 1.863,  1.948, masses[2], masses[2]}; // for Aleph formale
const Double_t params[7] = {0.762e-1, 10.632, 0.134e-4 , 1.863,  1.948/2, masses[2], masses[2]};
//________________________________________________________________________________
Double_t tmax(Double_t m, Double_t M, Double_t bg) {
  Double_t mOverM = m/M;
  Double_t bg2 = bg*bg;
  Double_t gamma = TMath::Sqrt(bg2 + 1);
  return 2*m*bg2/(1. + mOverM*(2*gamma + mOverM)); 
}
//________________________________________________________________________________
Double_t tmaxL10(Double_t *x, Double_t *p) {
  Double_t bg = TMath::Power(10.,x[0]);
  Double_t m = masses[0];
  Double_t M = p[0];
  return tmax(m,M,bg);
}
//________________________________________________________________________________
void Tmaxs() {
  for (Int_t i = 1; i < NHYP; i++) {
    TF1 *f = new TF1(Form("T%s",namesh[i]),tmaxL10,-1.,6.,1);
    f->SetParameter(0,masses[i]);
    f->SetLineColor(i);
    f->Draw();
  }
}
//________________________________________________________________________________
Double_t AlephBetheBlochFunc(Double_t *x, Double_t *par) {
#if 1
  // W.Blum, L. Rolandi "Particle Detection with Drift Chambers", page 246, eq. (9.5)
  // F_g(v) = p[0]/beta**p[3]*(p[1] - beta**p[3] - log(p[2] + (beta*gamma)**-p[4]);
  //        = p[0]*(1/beta**p[3]*(p[1] - log(p[2] + 1/(beta*gamma)**p[4])) - 1) 
  Double_t dEdxMIP = 2.39761562607903311;
  Double_t bg = TMath::Power(10.,x[0]);
  Double_t b2inv = 1. + 1./(bg*bg);
  Double_t beta  = 1./TMath::Sqrt(b2inv);
  //  return par[0]/TMath::Power(beta,par[3])*(par[1] - TMath::Power(beta, par[3]) - TMath::Log(par[2] + 1./TMath::Power(bg,par[4])));
  Double_t val = dEdxMIP*par[0]*(-1. + TMath::Power(beta,-par[3])*(par[1] - TMath::Log(TMath::Max(1e-10,par[2] + TMath::Power(bg,-par[4])))));
  if (val <= 0) return 0;
  return TMath::Log(val);
#else
  Double_t xx[2] = {x[0], par[6]};
  return AlephBetheBloch2DFunc(xx,par);
#endif
}
//________________________________________________________________________________
TF1 *AlephBetheBloch() {
  // ALICE TPC TDR 7.2.1.1 (p. 148) 
  // dependebce of dE/dx versus beta*gamma
  // 1/beta**2 = 1. + 1/(beta*gamma)**2
  // f(beta*gamma) = P1/beta**P4*(P2 - beta**P4 - ln(p3 + 1/(beta*Gamma)**P5))
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject("AlephBB");
  delete f;
  f = new TF1("AlephBB",AlephBetheBlochFunc,-1,6,7);
  const Double_t pars[7] = {0.762e-1, 10.632, 0.134e-4 , 1.863,  1.948, masses[2], masses[2]}; // for Aleph formula
  f->SetParNames("scale","densC","extra","betaP","logBGp","Tcut","mass");
  f->SetParameters(pars);
  f->FixParameter(5,-1);
  f->FixParameter(6,masses[2]);
  return f;
}
//________________________________________________________________________________
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t poverm   = TMath::Power(10.,x[0]);
  Double_t Norm = par[1];
    //    Bichsel::Instance()->GetI70M(TMath::Log10(4.),1.)/
    //    Bichsel::Instance()->GetI70M(TMath::Log10(4.),par[0]);
  return  TMath::Log(Norm*Bichsel::Instance()->GetI70M(TMath::Log10(poverm),par[0]));
}
//________________________________________________________________________________
TF1 *BichselF() {
  TF1 *f = new TF1("Bichsel",bichsel70,-1,4,2);
  f->SetParameter(0,1.);
  f->SetParameter(1,0);
  f->SetLineColor(2);
  return f;
}
//________________________________________________________________________________
Double_t jade(Double_t *x, Double_t *p) {
  // H.Breuker at al., "Particle Identification with the OPAL jet chambers in the region of the relativistic rise",
  // NIM A260 (1987) 329-342. Bethe-Bloch
  Double_t xi = p[0];
  Double_t K  = p[1];
  Double_t xA = p[2];
  Double_t a  = p[3];
  Double_t z  = p[4];
  Double_t m  = p[5];
  Double_t bg10 = x[0];
  Double_t bg   = TMath::Power(10., bg10);
  Double_t beta2 = bg*bg/(1 + bg*bg);
  Double_t gamma2 = bg*bg/beta2;
  Double_t X = bg10; // log10(beta*gamma)
  Double_t X1_0 = TMath::Power(2*TMath::Log(10.)/(m*a),1./(m-1)); // X_1 - X_0
  Double_t X_0 = xA - a *TMath::Power(X1_0,m)/(2*TMath::Log(10.)); 
  Double_t X_1 = X1_0 + X_0;
  Double_t delta = 0;
  if (X > X_0 && X < X_1) {delta = 2*TMath::Log(10.)*(X-xA) + a*TMath::Power(X_1 - X, m);}
  else if      ( X > X_1) {delta = 2*TMath::Log(10.)*(X-xA);}
  Double_t value = xi*(z*z/beta2)*(K + 2*TMath::Log(z) + TMath::Log(gamma2) - beta2 - delta);
  return TMath::Log(value);
}
//________________________________________________________________________________
TF1 *Jade(Int_t z = 1) {
  TF1 *f = new TF1(Form("Jade%i",z),jade,-1,5,6);
  f->SetParNames("xi","K","xA","a","z","m");
  f->SetParameters(0.5,11.3,2.1,0.19,1,3);
  f->FixParameter(4,z);
  f->FixParameter(5,3);
  return f;
}
//________________________________________________________________________________
void CompareAleph2Bichsel() {
  TCanvas *c1 = new TCanvas("c1");
  TH1F *hr = c1->DrawFrame(-1,0.35,4,2);
  hr->SetTitle("dE/dx predictions from Aleph and Bichsel parameterizations");
  hr->SetXTitle("log_{10} ( #beta#gamma  ) ");
  hr->SetYTitle("log_{10} (dE/dx (keV/cm))");  
  TLegend *leg = new TLegend(0.6,0.5,0.9,0.8);
  TF1 *aleph = AlephBetheBloch();
  aleph->Draw("same"); leg->AddEntry(aleph,"ALEPH & Alice");
  Double_t dxlog2[3] = {0., 1., 2.};
  for (Int_t i = 0; i < 3; i++) {
    TF1 *bich = BichselF(); 
    bich->SetParameter(0,dxlog2[i]);
    bich->SetLineColor(2+i);
    bich->Draw("same"); leg->AddEntry(bich,Form("Bichsel with dX = %2.0f",TMath::Power(2.,dxlog2[i])));
  }
  leg->Draw();
}
//________________________________________________________________________________
void DrawPar() {
  struct par_t {
    const Char_t *name;
    Double_t     p[7];
  };
  Int_t nh = 8;
  par_t pars[8] = {
    {"muon"    , {0.137587 , 5.38061, 0.0023921  , 3.76412, 1.5346 , -1,  0.105658}}, 
    {"pion"    , {0.111617 , 7.02397, 0.0011058  , 2.54329, 1.65499, -1, 0.13957 }},
    {"kaon"    , {0.0456581, 16.7793, 2.57426e-07, 2.39358, 3.55545, -1, 0.493677}},
    {"proton"  , {0.0271003, 27.8546, 2.44759e-11, 2.32074, 5.99509, -1, 0.938272}},
    {"deuteron", {0.0287168, 25.8611, 4.12635e-10, 2.39857, 5.96523, -1, 1.87561 }},
    {"triton"  , {0.0320681, 23.2456, 2.30097e-08, 2.39791, 5.45542, -1, 2.80925 }},
    {"alpha"   , {0.0762   , 10.632 , -0.00172418, 1.863  , 1.948  , -1, 3.72742 }},
    {"all"     , {2.09882e-01,4.42206e+00,3.25646e-02,2.24340e+00,1.07360e+00,0}}
  };
    /*
    {"electron", { 0.106429,   9.3921,  0.00415669,  -5890.7,  1.17311, 0.000510999}},
    {"muon"    , { 0.130016,   6.00543, 0.00178123,  2.52634,  1.56639, 0.105658}},
    {"pion"    , { 0.0735016, 11.2343,  5.80563e-05, 2.13493,  2.20037, 0.13957}},
    {"kaon"    , { 0.0437541, 17.9757,  9.10954e-08, 2.36887,  3.77365, 0.493677}},
    {"proton"  , { 0.030051,  25.3659,  9.67102e-11, 2.43105,  5.69115, 0.938272}},
    {"deuteron", { 0.0286443, 26.5876,  2.46795e-10, 2.38376,  6.12088, 1.87561}},
    {"triton"  , { 0.0319365, 24.1234,  1.71975e-08, 2.35045,  5.51526, 2.80925}},
    {"He3"     , { 0.118832,  24.2682,  -31247.1,    0.492737, 2.09498, 2.80923}},
    {"alpha"   , { 0.14097,   15.6047,  -1.59386,    0.587276,-0.141881,3.72742}},
    {"pionMIP" , { 0.0511063, 15.3941,  0.000625757, 2.25629,  3.41833, 0.13957}}
    */
  /*
    "electron", { 0.080255,  11.1544,  0.000263695, -4281.43, 1.51653, -1, 0.00051099},
    "muon    ", { 0.130018,   6.00528, 0.00178129,   2.52644, 1.56639, -1, 0.10565},
    "pion    ", { 0.0832391,  9.98481, 0.000169188,  2.13745, 1.97676, -1, 0.13957, 
    "kaon    ", { 0.0437542, 17.9756,  9.10996e-08,  2.36888, 3.77363, -1, 0.49367},
    "proton  ", { 0.0309674, 24.6207,  1.83988e-10,  2.43202, 5.5304,  -1, 0.938272},
    "deuteron", { 0.0598169, 13.0592,  1.24324e-05,  2.3652,  3.0262,  -1, 1.87561},
    "triton  ", { 0.181795,   4.7822,  0.0158898,    2.2865,  1.18566, -1, 2.8092},
    "gAll fit", { 1.08778e-01, 7.56784e+00, 9.39237e-04, 2.27942, 1.70087,-1, 1.39570e-01},
    "g fit", { 0.10417,    7.41566, 0.00073,      2.35903, 1.74781,  },
    "
    */
  TLegend *l = new TLegend(0.5,0.6,0.8,0.9);
  TString same;
  for (Int_t i = 0; i < nh; i++) {
    TF1 *f = AlephBetheBloch();
    f->SetParameters(pars[i].p);
    f->SetName(pars[i].name);
    Int_t color = i+2;
    if (i == 7) color = 1;
    f->SetLineColor(color);
    f->Draw(same);
    same = "same";
    l->AddEntry(f,pars[i].name);
  }
  l->Draw();
}
//________________________________________________________________________________
Int_t GetFileList(TFile *files[14]) {
  TSeqCollection *filesC = gROOT->GetListOfFiles();
  TIter next(filesC);
  TFile *f = 0;
  memset(files, 0, NHYP*sizeof(TFile*));
  Int_t NF = 0;
  while ((f = (TFile *) next())) {
    TString name(f->GetName());
    for (Int_t i = NHYP - 1; i >= 0; i--) {
      //      if (name.Contains(Form("Heed_%s",names[i]))) {
      if (name.Contains(Form("%s",names[i]))) {
	files[i] = f;
	NF++;
      }
    }
  }
  return NF;
}
//________________________________________________________________________________
void TpcRSdEdx(const Char_t *fopt = "I70") {
  Bool_t fI70 = kFALSE;
  TString fOpt(fopt);
  if (fOpt.Contains("I70")) fI70 = kTRUE;
  Double_t scales[3] = {0};
  Double_t sigmas[3] = {0.076};
  //  Double_t scales[2] = {-6.75100587779081402e-03,-8.64200502877701150e-03}; // pi70->Interpolate(TMath::Log10(4.)); piz->Interpolate(TMath::Log10(4.));
  //  Double_t sigmas[2] = { 7.56777648706270512e-02, 7.46023243878390085e-02}; // pi70S->Interpolate(TMath::Log10(4.)); pizS->Interpolate(TMath::Log10(4.));
  Double_t scale = fI70 ? scales[0] : scales[1];
  Double_t sigma = fI70 ? sigmas[0] : sigmas[1];
  TString c1N(fopt); 
  TString c2N("DeV"); c2N += fopt;
  TString c3N("sigma"); c3N += fopt;

  TFile *files[NHYP];
  Int_t NF = GetFileList(files);
  if (! NF) return;
  TLegend *l = new TLegend(0.5,0.6,0.8,0.9);
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(c1N);
  if (! c1 ) c1 = new TCanvas(c1N,c1N);
  else       c1->Clear();
  TH1F *hr = c1->DrawFrame(-1,0,6,6);
  hr->SetTitle("TpcRS log(dE/dx) versus log_{ 10} (   #beta #gamma)");
  hr->SetXTitle("log_{10} ( #beta#gamma  ) ");
  hr->SetYTitle("log (dE/dx [keV/cm])");  

  TLegend *l2 = new TLegend(0.5,0.6,0.8,0.9);
  TCanvas *c2 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(c2N);
  if (! c2 ) c2 = new TCanvas(c2N,c2N);
  else       c2->Clear();
  TH1F *hr2 = c2->DrawFrame(-1,-0.2,6,0.2);
  hr2->SetTitle(Form("Deviation TpcRS log(dE/dx) from %s Prediction versus log_{ 10}(   #beta #gamma)",fopt));
  hr2->SetXTitle("log_{10} ( #beta#gamma  ) ");
  hr2->SetYTitle("log (dE/dx / Prediction)");  
  TLegend *l3 = new TLegend(0.5,0.6,0.8,0.9);
  TCanvas *c3 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(c3N);
  if (! c3 ) c3 = new TCanvas(c3N,c3N);
  else       c3->Clear();
  TH1F *hr3 = c3->DrawFrame(-1,0.0,6,1.8);
  hr3->SetTitle(Form("Relative error of dE/dx log_{ 10}(   #beta #gamma) normalized at %5.3f",sigma));
  hr3->SetXTitle("log_{10} ( #beta#gamma  ) ");
  hr3->SetYTitle("#sigma (log (dE/dx))");  
  Bichsel *b = Bichsel::Instance();
  c1->cd(); l->Draw();
  c2->cd(); l2->Draw();
  c3->cd(); l3->Draw();
  Int_t color = 0;
  TString Out("McPiD");
  Out += fOpt; Out += ".root";
  TFile *fOut = new TFile(Out,"recreate");
  TH2F *all = 0;
  TH2F *h2 = 0;
  //  for (Int_t i = 0; i < NHYP+1; i++) {
  for (Int_t i = 0; i < NHYP; i++) {
    if (i < NHYP) {
      if (! files[i]) continue;
      files[i]->cd();
      h2 = (TH2F *) files[i]->Get(Form("%s%s",fopt,namesh[i]));
      if (! h2) continue;
      if (h2->GetEntries() <100) continue;
      cout << "in file " << files[i]->GetName() << " Found histogram " << h2->GetName() << endl;
      if (i < 10) {
	TH2F *h2i = (TH2F *) files[i+1]->Get(Form("%s%s",fopt,namesh[i+1]));
	if (h2i && h2i->GetEntries() > 100) {
	  cout << "in file " << files[i+1]->GetName() << "Found histogram " << h2i->GetName() << endl;
	  h2->Add(h2i); i++; 
	  files[i]->cd();
	}
	if (! all) {fOut->cd(); all = new TH2F(*h2); all->SetName(Form("allP%s",fopt));}
	else       {all->Add(h2);}
      }
    } else {
      fOut->cd();
      h2 = all;
    }
    if (! h2) continue;
    //    h2->FitSlicesY(0,0,-1,10,"qen3s");
    h2->FitSlicesY(0,0,-1,10,"qen5s");
    TH1D *h1p = (TH1D *) gDirectory->Get(Form("%s_1",h2->GetName()));
    if (! h1p) continue;
    l2->AddEntry(h1p,names[i]);
    TH1D *s1 = (TH1D *) gDirectory->Get(Form("%s_2",h2->GetName()));
    l3->AddEntry(s1,names[i]);
    TString name(h1p->GetName());
    TString title(h2->GetTitle());
    title.ReplaceAll(" Bichsel","");
    name.ReplaceAll("N70B_1","70");
    name.ReplaceAll("P70B_1","70");
    name.ReplaceAll("NzB_1","z");
    name.ReplaceAll("PzB_1","z");
    h1p->SetName(name);
    h1p->SetTitle(title + Form(" #mu - %s",fopt));
    name += "S";
    s1->SetName(name);
    s1->SetTitle(title + " #sigma");
    TH1D *h1 = 0;
    if (i < NHYP) {
      h1 = new TH1D(*h1p); h1->SetName(Form("%sp",h1->GetName()));
      h1->SetTitle(title + " #mu");
    }
    color++;
    if (color < 8) {
      if (h1) {
	h1->SetMarkerStyle(20);
	h1->SetMarkerColor(color);
	h1->SetLineColor(color);
      }
      h1p->SetMarkerStyle(20);
      h1p->SetMarkerColor(color);
      h1p->SetLineColor(color);
      s1->SetMarkerStyle(20);
      s1->SetMarkerColor(color);
      s1->SetLineColor(color);
    } else {
      if (h1) {
	h1->SetMarkerStyle(21);
	h1->SetMarkerColor(color-7);
	h1->SetLineColor(color-7);
      }
      h1p->SetMarkerStyle(21);
      h1p->SetMarkerColor(color-7);
      h1p->SetLineColor(color-7);
      s1->SetMarkerStyle(21);
      s1->SetMarkerColor(color-7);
      s1->SetLineColor(color-7);
    }
    if (h1p) {
      Int_t nx = h1p->GetNbinsX();
      Int_t xmin = 9999;
      Int_t xmax =-9999;
      for (Int_t ix = 1; ix <= nx; ix++) {
	Double_t err = h1p->GetBinError(ix);
	if (err <= 0. || err > 0.01) continue;
	Double_t val  = h1p->GetBinContent(ix);
	if (s1) {
	  if (s1->GetBinError(ix) > 0.01) continue;
	}
	val -= scale;
	h1p->SetBinContent(ix,val);
	if (h1) {
	  Double_t bg10 = h1->GetXaxis()->GetBinCenter(ix);
	  //       if (val < 1*err) {
	  // 	h1->SetBinContent(ix, 0.);
	  // 	h1->SetBinError(ix, 0.);
	  //       }
	  if      (fOpt.Contains("I70",TString::kIgnoreCase))   val += TMath::Log(chargeSQ[i]*b->GetI70M(bg10));
	  else if (fOpt.Contains("fitz",TString::kIgnoreCase))  val += TMath::Log(chargeSQ[i]*TMath::Exp(b->GetMostProbableZ(bg10)));
	  else                                                  val += TMath::Log(chargeSQ[i]*StdEdxModel::instance()->dNdx(TMath::Power(10.,bg10)));
	  h1->SetBinContent(ix, val);
	}
	if (ix < xmin) xmin = ix;
	if (ix > xmax) xmax = ix;
      }
      h1p->GetXaxis()->SetRange(xmin,xmax);
      s1->GetXaxis()->SetRange(xmin,xmax);
      s1->Scale(1./sigma);
      if (h1) {
	h1->GetXaxis()->SetRange(xmin,xmax);
	l->AddEntry(h1,names[i]);
	h1->SetStats(0);
#if 0
	TF1 *A = AlephBetheBloch();
	if (color < 8) 
	  A->SetLineColor(color);
	else 
	  A->SetLineColor(color-7);
#endif
      }
    }
    fOut->cd();
#if 0
    if (h1) {
      h1->Fit(A,"er","same",xmin,xmax);
      Double_t ppar[7];
      A->GetParameters(ppar);
      cout << Form("/* %10s */ %12.5f, %12.5f,",namesh[i], xmin, xmax);
      for (Int_t j = 0; j < 6; j++) {cout << Form("%12.5f,",ppar[j]);}
      cout << endl;
      //    h1->Draw("same");
      TH1D *d1 = new TH1D(*h1);
      d1->SetName(Form("diff_%s",h1->GetName()));
      d1->Add(A,-1.);
      d1->Write();
    }
#else
    c1->cd();
    if (h1) h1->Draw("same");
    c1->Update();
    c2->cd();
    h1p->Draw("same");
    c2->Update();
    c3->cd();
    s1->Draw("same");
    c3->Update();
#endif
    //    h2->Write();
    if (h1) h1->Write();
    h1p->Write();
    s1->Write();
    //    h1->Fit(F,"er","same",-1,5);
  }
}


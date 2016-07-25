#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TClassTable.h"
//#include "DeDxTree.C"
#include "TMinuit.h"
#include "TCanvas.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TLegend.h"
#include "TPaveStats.h"
#include "TFitter.h"
#else
class TMinuit;
class TF1;
class TH1F;
class TH2F;
class TH3F;
class TProfile;
class TH2D;
class TCanvas;
class TSpectrum;
class TSystem;
class Bichsel;
#endif
#define Legend
Bichsel *m_Bichsel = 0;
#include "Names.h"
const Int_t jhead = 7;
TFile * fOut = 0;
TNtuple *fitP = 0;
Float_t *tuple = 0;
TCanvas *c1 = 0;
static Double_t frac[NHypTypes];
static Int_t    hyp    = 0;
static Double_t LogTot = 0;
static Double_t Sigma  = 0;
static Double_t Norm   = 0;
static Int_t    iHyp   = 0;
static Int_t    ndf    = 0;               
static Double_t Overlap= 0;
static Double_t zCurrent      = 0;
static Double_t zval[NHypTypes];
static Double_t *Par   = 0;
static Double_t dEdxRef   = 0;
static const Double_t zmin = -4;
static const Double_t zmax =  6;
#define OVERLAP
//________________________________________________________________________________
Double_t SigmaBG(Double_t bglog10) {
  static TF1 *fSigmaBG = 0;
  static Double_t params[6] = {-5.93439e-01, 
			        4.43305e-01, 
			       -8.35654e-01, 
			        5.15944e-01, 
			       -1.35251e-01, 
			        1.28334e-02};
  if (! fSigmaBG ) {
    fSigmaBG = new TF1("fSigmaBG","pol5",-1.,4.);
    fSigmaBG->SetParameters(params); 
  }
  if (bglog10 <-0.5) bglog10 = -0.5;
  if (bglog10 > 3.5) bglog10 =  3.5;
  return TMath::Exp(fSigmaBG->Eval(bglog10)+0.52);
}
//________________________________________________________________________________
Double_t func(Double_t *x,Double_t *par) {
  zCurrent   = x[0];
  Double_t Value = 0;
  Int_t h1, h2;
  h1 = 0; h2 = NHypTypes - 1;
  if (iHyp >= h1 && iHyp <= h2) {h1 = h2 = iHyp;}
  for (Int_t i = h1; i <= h2; i++) { 
    if (frac[i] > 0) {
      Double_t sigma = par[2*NHypTypes+jhead+i]*Sigma;
      assert(sigma > 0);
      if (par[NHypTypes+jhead+i] > 0 && sigma > 0) {
	Double_t dev = (zCurrent - zval[i])/sigma;
	//      Value += frac[i]*TMath::Gaus(z,par[jhead+i]+par[3*NHypTypes+jhead+i],sigma,1);
	//	if (ndf < 2) 
	Value += frac[i]*TMath::Gaus(dev,0,1.,1)/sigma;
#if 0
	  //	else 
	Value += frac[i]*TMath::Student(dev,ndf);
	cout << "i\t" << i << "\tx = " << x[0] << " frac " << frac[i] << "\tsigma\t" << sigma 
	     << "\tValue\t" << Value << endl;
	//      }
#endif
      }
    }
  }
  return Value;
}
//________________________________________________________________________________
Double_t funCv(Double_t *x, Double_t *par) {
  Double_t w = x[0];
  if (w < zmin) return 0;
  Double_t E = dEdxRef*TMath::Exp(zCurrent);
  Double_t E1 = dEdxRef*TMath::Exp(w);
  Double_t E2 = E - E1;
  if (E2 < 0.) return 0;
  Double_t v = TMath::Log(E2/dEdxRef);
  if (v < zmin) return 0;
  return func(&w,Par)*func(&v,Par);
}
//________________________________________________________________________________
Double_t funcI(Double_t *x,Double_t *par) {
  static TF1 *fInt = 0;
  if (! fInt) {
    fInt = new TF1("fInt",funCv,zmin,zmax,0);
  }
  return fInt->Integral(zmin,zCurrent);
}
//________________________________________________________________________________
Double_t fithfcn(Double_t *x,Double_t *par) {
  // par[0] - hyp 
  // par[1] - log (Total)
  // par[2] - Sigma
  // par[3] - Norm
  // par[4] - current hyp 
  // par[5] - Student ndf (< 2 use Gaus) jhead = 7
  // par[6] - ovelap
  // par[            jhead:  NHypTypes+jhead-1] = Delta_z of "e","p","K","pi","mu","d" wrt ref
  // par[  NHypTypes+jhead:2*NHypTypes+jhead-1] = arcsin(sqrt(fraction)) of "e","p","K","pi","mu","d"
  // par[2*NHypTypes+jhead:3*NHypTypes+jhead-1] = sigma of "e","p","K","pi","mu","d" wrt Sigma
  // par[3*NHypTypes+jhead:4*NHypTypes+jhead-1] = z of ref
  zCurrent   = x[0];
  hyp    = ((Int_t) par[0])%NHypTypes;
  LogTot =         par[1];
  Sigma  =         par[2];
  Norm   =         par[3];
  iHyp   = (Int_t) par[4];
  ndf    = (Int_t) par[5];
  Overlap=         par[6];
  Par    =         par;
#if 0
  cout << "hyp\t" << hyp << "\tLogTot\t" << LogTot << "\tSigma\t" << Sigma << endl;
#endif
  Int_t i;
  memset (frac, 0, NHypTypes*sizeof(Double_t));
  frac[hyp] = 1;
  for (i = 0; i < NHypTypes; i++) {
    if (par[NHypTypes+jhead+i] > 0) {
      if (i != hyp) {
	frac[i] = TMath::Sin(par[NHypTypes+jhead+i]);
	frac[i] *= frac[i];
	frac[hyp] -= frac[i];
      }
    } else {frac[i] = 0;}
  }
  Double_t Value = -1e10;
  if (frac[hyp] < 0) return Value;
  Int_t indx[NHypTypes], indxF[NHypTypes];
  for (Int_t i = 0; i < NHypTypes; i++) zval[i] = par[jhead+i] + par[3*NHypTypes+jhead+i];
  TMath::Sort(NHypTypes, zval, indxF, kFALSE);
  TMath::Sort(NHypTypes, &par[3*NHypTypes+jhead], indx, kFALSE);
  for (Int_t i = 0; i < NHypTypes; i++) if (indx[i] != indxF[i]) return Value;
  // -----
  Value = func(x,par);
  if (Overlap > 0) {
    Double_t Ov = TMath::Sin(Overlap);
    Ov *= Ov;
    Value *= (1. - Ov);
    Value += Ov*funcI(x,par);
  }
  return Norm*TMath::Exp(LogTot)*Value;
}
//________________________________________________________________________________
void FitH(const Char_t *set="z", Int_t h=0, Int_t bin = -1) {
  if (!m_Bichsel) {
    gSystem->Load("StBichsel");
    m_Bichsel = Bichsel::Instance();
  }
  if (! gROOT->IsBatch() && ! c1) {c1 = new TCanvas(); c1->SetLogy(1);}
  Int_t jb10  = 0;
  Int_t jbase = 0;
  Int_t npar  = 0;
  TH2 *hists[NHYPS];
  TProfile *histp[NHYPS];
  TFile *fRootFile = (TFile *) gDirectory->GetFile();
  if (! fRootFile ) {printf("Cannot find/open %s",fRootFile->GetName()); return;}
  else               printf("%s found\n",fRootFile->GetName());
  if (!fOut) {
    TString newfile("FitHnew");
    newfile += set;
    if (h >= 0) newfile += Names[h];
    else  newfile += "All";
    newfile += gSystem->BaseName(fRootFile->GetName());
    newfile.ReplaceAll("+","P");
    newfile.ReplaceAll("-","N");
    fOut = new TFile(newfile,"update");
  }
  if (fOut) fOut->cd();
  const Int_t NHypTypes = NHYPS/2;
  TH1 *proj = 0;
  //  Double_t params[12];
  TF1 *g = new TF1("F6B",fithfcn,zmin,zmax,jhead+4*NHypTypes);
  g->SetParName(0,"HypType");
  g->SetParName(1,"TotalL");
  g->SetParName(2,"Sigma");
  g->SetParName(3,"Norm");
  g->SetParName(4,"CHyp");
  g->SetParName(5,"ndf");
  g->SetParName(6,"Overlap");
  //  Int_t NFit = 0;
  Int_t ih1 = 0;
  Int_t ih2 = NHYPS;
  for (Int_t ih = ih1; ih <= ih2; ih++) {
    if (h >= 0 && ih != h) continue;
    Char_t *HistN =  HistNames[ih];
    if (TString(set) == "70") HistN = HistNames70[ih];
    hists[ih] = (TH2 *) fRootFile->Get(HistN);
    if (!hists[ih]) {printf("Cannot find histogram %s\n",HistNames[ih]); continue;}
    histp[ih] = (TProfile *) fRootFile->Get(HistNameP[ih]);
    if (!histp[ih]) {printf("Cannot find histogram %s\n",HistNameP[ih]); continue;;}
    Int_t k = 0;
    Int_t l = 0;
    if (ih > NHypTypes) k = NHypTypes;
    for (l = k; l < k + NHypTypes; l++) {
      Int_t jh = l%NHypTypes;
      g->SetParName(jhead            +jh,Form("Delta %s",Names[l]));
      g->SetParName(jhead+  NHypTypes+jh,Form("Phi   %s",Names[l]));
      g->SetParName(jhead+2*NHypTypes+jh,Form("sigma %s",Names[l]));
      g->SetParName(jhead+3*NHypTypes+jh,Form("RefZ  %s",Names[l]));
    }
    if (! fitP) {//     0    1    2          3     4    5     6    7
      TString TupleDef("j:pmom:dEdxRef:NFitPoints:NDFit:prob:chisq:bg10");
      jb10 = 8;
      jbase = jb10 + NHypTypes;
      for (Int_t l = 0; l < NHypTypes; l++) {
	TupleDef += ":bg10";   TupleDef += Names[l];
      }
      npar = g->GetNpar();
      for (Int_t l = 0; l < npar; l++) {
	TupleDef += ":"; TupleDef += g->GetParName(l);  TupleDef += ":err"; TupleDef += g->GetParName(l);
      }
      TupleDef.ReplaceAll(" ","");
      TupleDef.ReplaceAll("+","");
      TupleDef.ReplaceAll("-","");
      TupleDef.ReplaceAll("bar",""); cout << "Create tuple with " << TupleDef << endl;
      fitP = new TNtuple("FitP","Fit results",TupleDef);
      fitP->SetMarkerStyle(20);
      tuple = new Float_t[jbase+2*npar];
    }
    Int_t nx = hists[ih]->GetNbinsX();
    Double_t ymin = hists[ih]->GetYaxis()->GetXmin();
    Double_t ymax = hists[ih]->GetYaxis()->GetXmax();
    for (int j=1; j<=nx; j++) {
      if (tuple) {
	memset (tuple, 0, (jbase+2*npar)*sizeof(Float_t));
	tuple[0] = j;
      }
      if (bin >= 0 && j != bin) continue;
      TString name(Form("%s_%i_%i",hists[ih]->GetName(),ih,j));
      proj = hists[ih]->ProjectionY(name.Data(),j,j);
      Double_t Total = proj->Integral();
      printf("hist:%s bin %3i for hyp %i has only %10.0f entries",hists[ih]->GetName(),j,ih,Total);
      if (Total < 100.) {
	printf("====  skip\n");
	delete proj;
	continue;
      }
#if 0
      TVirtualFitter *hFitter = TVirtualFitter::Fitter(proj,g->GetNpar());
      assert(hFitter);
      hFitter->SetFCN(MyLikelihood);
#endif
      printf("====  get \n");
      Double_t dX = proj->GetXaxis()->GetBinWidth(1);
      Double_t Sigma = 0.1;
      
      Double_t bg10 = hists[ih]->GetBinCenter(j);
      Double_t bg = TMath::Power(10., bg10);
      Double_t pmom = Masses[ih]*bg;
      if (tuple) {
	tuple[1] = pmom;
	tuple[7] = bg10;
      }
      Double_t devZ[NHypTypes];
      Double_t sigma[NHypTypes];
      dEdxRef = 0;
      if (TString(set) == "70")  dEdxRef = 1.e-6*m_Bichsel->GetI70(bg10,1.0); 
      else                       dEdxRef = 1.e-6*TMath::Exp(m_Bichsel->GetMostProbableZ(bg10,1.0));
      if (tuple) tuple[2] = dEdxRef;
      if (ih > NHypTypes) k = NHypTypes;
      //      Int_t iok = 1;
      g->FixParameter(0,ih); 
      g->FixParameter(1,TMath::Log(Total*dX));
      g->SetParameter(2,Sigma); g->SetParLimits(2,0.25*Sigma,2*Sigma); // Sigma
      g->SetParameter(3,1); g->SetParLimits(3, 0.95, 1.05);
      g->FixParameter(4,-1.);
      g->FixParameter(5, 0); // g->FixParameter(5, 3); // 
      g->FixParameter(6, 0);
      Int_t ifix[NHypTypes];
      Double_t norm = 0;
      for (l = k; l < k + NHypTypes; l++) {
	Int_t jh = l%NHypTypes;
	bg = pmom/Masses[l];
	bg10 = TMath::Log10(bg);
	if (tuple) {
	  tuple[jb10+jh] = bg10;
	}
	Double_t dEdx = 0;
	if (TString(set) == "70")  dEdx = 1.e-6*m_Bichsel->GetI70(bg10,1.0); 
	else                       dEdx = 1.e-6*TMath::Exp(m_Bichsel->GetMostProbableZ(bg10,1.0));
	devZ[jh] = TMath::Log(dEdx/dEdxRef);
	sigma[jh] = 1;// SigmaBG(bg10);
	printf("pmom = %10.3f %s devZ[%i] = %f sigma = %f\n", pmom, Names[l], jh, devZ[jh],sigma[jh]);
	g->FixParameter(jhead+            jh,0);
	g->FixParameter(jhead+2*NHypTypes+jh,sigma[jh]);// sigma
	if (devZ[jh] < ymin - 3*Sigma ||
	    devZ[jh] > ymax + 3*Sigma ) {
	  g->FixParameter(jhead+  NHypTypes+jh,-99); // frac
	  ifix[jh] = 1;
	} else {
	  ifix[jh] = 0;
	  norm += 1;
	}
	g->FixParameter(jhead+3*NHypTypes+jh,devZ[jh]);
      }
      assert(norm);
      norm = TMath::ASin(1./TMath::Sqrt(norm));
      for (l = 0; l < NHypTypes; l++) {
	if (! ifix[l]) {
	  g->ReleaseParameter(jhead+  NHypTypes+l);
	  g->SetParameter(jhead+  NHypTypes+l,norm); // frac
	  g->SetParLimits(jhead+  NHypTypes+l,0., TMath::Pi()/2.);
	}
      }
      g->FixParameter(jhead+  NHypTypes+ih%NHypTypes, 99.);
      //      g->Print();
      proj->Fit(g->GetName(),"LM");//,"RIM");
      Sigma = g->GetParameter(2);
      Double_t zmin[NHypTypes], zmax[NHypTypes];
      Int_t    imin[NHypTypes], imax[NHypTypes];
      for (Int_t jh = 0; jh < NHypTypes; jh++) {
	zmin[jh] = - 3*Sigma;
	zmax[jh] =   3*Sigma;
	imin[jh] = imax[jh] = -1;
	for (Int_t lh = 0; lh < NHypTypes; lh++) {
	  if (jh != lh) {
	    Double_t dz = devZ[lh] - devZ[jh];
	    if (dz < 0 && dz > zmin[jh]) {zmin[jh] = dz; imin[jh] = lh;}
	    if (dz > 0 && dz < zmax[jh]) {zmax[jh] = dz; imax[jh] = lh;}
	  }
	}
	Double_t Frac  = g->GetParameter(jhead+  NHypTypes+jh);
	Double_t dFrac =  g->GetParError(jhead+  NHypTypes+jh);
	if (TMath::Abs(Frac) < 3*dFrac || Frac <= -99) {
	  g->FixParameter(jhead+  NHypTypes+jh, -99);
	  g->FixParameter(jhead+            jh,0);
	}
	else {
	  g->ReleaseParameter(jhead+ jh);
	  g->SetParameter(jhead+ jh, 0); 
#if 0
	  g->SetParameter(jhead+ jh, 0); g->SetParLimits(jhead+ jh,zmin[jh],zmax[jh]); 
#endif
	}
      }
      // mu and pi
      if (g->GetParameter(jhead+  NHypTypes+kPidMuon) >  -99.0 &&
	  g->GetParameter(jhead+  NHypTypes+kPidPion) <= -99.0) {
	g->ReleaseParameter(jhead+  NHypTypes+kPidPion);
	g->ReleaseParameter(jhead+            kPidPion);
	g->SetParameter(jhead+  NHypTypes+kPidPion, g->GetParameter(jhead+  NHypTypes+kPidMuon));
	g->SetParameter(jhead+            kPidPion, g->GetParameter(jhead+            kPidMuon));
	g->FixParameter(jhead+  NHypTypes+kPidMuon, -99);
	g->FixParameter(jhead+            kPidMuon,   0);
      }
      g->FixParameter(jhead+  NHypTypes+ih%NHypTypes, 99.);
      //      g->FixParameter(3,1);
      proj->Fit(g->GetName(),"LM");//,"RIM");
#ifdef OVERLAP
      g->ReleaseParameter(6);
      g->SetParameter(6,0.01);
      proj->Fit(g->GetName(),"LM");//,"RIM");
#endif
#ifdef Legend
      TLegend *leg = new TLegend(0.1,0.7,0.25,0.9,"");
      proj->GetListOfFunctions()->Add(leg);
#endif
      for (Int_t i = 0, icol = 2 ; i < NHypTypes; i++, icol++) {
	if (g->GetParameter(jhead+  NHypTypes+i) <= -99.0) continue;
	Int_t j = i;
	if (k > 0) j += NHypTypes;
	TF1 *f = new TF1(*g);
	if (i >= 0) {
	  f->SetName(Form("F%s_%s",Names[j],proj->GetName()));
#ifdef Legend
	  leg->AddEntry(f,Names[j]);
#endif
	}
	else    f->SetName(Form("F%s",proj->GetName()));
	Double_t ch = i;
	f->FixParameter(4,ch);
	if (icol == 5) icol++;
	f->SetLineColor(icol);
	proj->GetListOfFunctions()->Add(f);
      }
      if (c1) {
	proj->Draw();
	TPaveStats *st = (TPaveStats*) proj->FindObject("stats");
	if (st) st->SetY1NDC(0.6);
	c1->Update();
      }
      if (fitP && tuple) {
	tuple[3] = g->GetNumberFitPoints();
	tuple[4] = g->GetNDF();
	tuple[5] = g->GetProb();
	tuple[6] = g->GetChisquare();
	Int_t j = jbase;
	for (Int_t l = 0; l < npar; l++) {
	  tuple[j] = g->GetParameter(l); j++;
	  tuple[j] = g->GetParError(l); j++;
	}
	fitP->Fill(&tuple[0]);
      }
      if (fOut) proj->Write();
      //      break;
    }
    //    break;
  }
  if ( gROOT->IsBatch()) {
    if (fOut) {fOut->Write(); delete fOut; fOut = 0;}
    if (tuple) {delete [] tuple; tuple = 0;}
  }
  if (fRootFile) fRootFile->cd();
}



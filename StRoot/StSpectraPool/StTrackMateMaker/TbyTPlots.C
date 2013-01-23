/*
  .L TbyTPlots.C+
  Init();
 */
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "TString.h"
#include "TLegend.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#else
class TSystem;
class TMath;
class TH1;
class TH2;
class TH3;
class TProfile;
class TStyle;
class TF1;
class TTree;
class TChain;
class TFile;
class TNtuple;
class TCanvas;
class TMinuit;
class TSpectrum;
class TString;
class TLegend;
class TLine;
class TText;
class TROOT;
class TList;
class TPolyMarker;
#endif
TFile *fOut = 0;
TCanvas *c1 = 0;
TChain *fChain = 0;
const Int_t NfitPtsHist = 7;
struct mc_data_t {
  float oldPtGl;
  float newPtGl;
  float oldEtaGl;
  float newEtaGl;
  float oldPhiGl;
  float newPhiGl;
  float oldPGl;
  float newPGl;
  float oldFitPtsGl;
  float newFitPtsGl;
  float oldPtPr;
  float newPtPr;
  float oldEtaPr;
  float newEtaPr;
  float oldPhiPr;
  float newPhiPr;
  float oldPPr;
  float newPPr;
  float oldFitPtsPr;
  float newFitPtsPr;
  float oldDedx;
  float newDedx;
  float oldCharge;
  float newCharge;
  float maxPing;
  float Prim;
  float oldChi2Gl0;
  float newChi2Gl0;
  float oldChi2Gl1;
  float newChi2Gl1;
  float oldChi2Pr0;
  float newChi2Pr0;
  float oldChi2Pr1;
  float newChi2Pr1;
  float firstHitsDist;
  float lastHitsDist;
  float oldPrimX;
  float oldPrimY;
  float oldPrimZ;
  float newPrimX;
  float newPrimY;
  float newPrimZ;
};
static mc_data_t data;
static Float_t  refMult;
const Char_t *GP[2] = {"Gl","Pr"};
const Char_t *GPTitle[2] = {"Global","Primary"};
static TString NameEffpT[4] = {"pTnew","pTold","pTOnew","pTOold"};
static TString NameEffPhi[4] = {"Phinew","Phiold","PhiOnew","PhiOold"};
const Int_t minNFP = 10; // 25; // 10;
const Int_t effNFP = 15; // 25; // 15;
static TString Old("Old");
static TString New("New");
const Char_t *charge[2] = {"pos","neg"};
TFile *fIn = (TFile *) gDirectory;
TString gTitle;
TString cTitle;
//________________________________________________________________________________
void DrawPng(TCanvas *c) {
  static Int_t nPng = 0;
  if (! c) return;
  TString pngName("");
  c->Update(); pngName = c->GetName();
  pngName.ReplaceAll(" ","_");
  pngName.ReplaceAll("(","_");
  pngName.ReplaceAll(")","_");
  pngName.ReplaceAll("{","_");
  pngName.ReplaceAll("}","_");
  pngName.ReplaceAll("<","lt");
  pngName.ReplaceAll(">","gt");
  pngName.ReplaceAll("old_new_","");
  pngName.ReplaceAll("old_new","");
  pngName.ReplaceAll("Old_New_","");
  pngName.ReplaceAll("Old_New","");
  pngName.ReplaceAll("GeV/c","");
  pngName.ReplaceAll(".","_");
  pngName.ReplaceAll("/","_");
  pngName.ReplaceAll("^","_");
  pngName.ReplaceAll("__","_");
  pngName.ReplaceAll("__","_");
  pngName.ReplaceAll("#","");
  pngName += ".png"; 
  c->SaveAs(pngName);
  nPng++;
  cout << "Draw #\t" << nPng << "\t" << pngName << endl;
}
//________________________________________________________________________________
void DrawFitPnts(Int_t ping=0) {
  //    gStyle->SetPalette(1,0);
  //________________________________________________________________________________
  // Fit pts correlation
  //________________________________________________________________________________
  TString Name("fitPtsHist");
  if (ping) Name += "Ping";
  TH2F* fitPtsHist = (TH2F*) fIn->Get(Name);
  if (! fitPtsHist) return;
  cTitle += gTitle;
  cTitle += "Fit Points Corr";
  if (ping) cTitle += " Matched";
  c1 = new TCanvas(cTitle,cTitle,400,400); c1->SetLeftMargin(0.14);
  c1->SetLogz();
  fitPtsHist->SetStats(0);
  fitPtsHist->GetXaxis()->SetRange(0,46);
  //  fitPtsHist->GetYaxis()->SetRange(0,45);
  fitPtsHist->Draw("colz");
  Int_t nx = fitPtsHist->GetNbinsX();
  Int_t ny = fitPtsHist->GetNbinsY();
  Double_t meanX = 0, meanY = 0;
  Double_t X = 0, Y = 0;
  for (Int_t ix = 1; ix <= nx; ix++) {
    Double_t x = fitPtsHist->GetXaxis()->GetBinCenter(ix);
    for (Int_t iy = 1; iy <= ny; iy++) {
      Double_t y = fitPtsHist->GetYaxis()->GetBinCenter(iy);
      Double_t cont = fitPtsHist->GetBinContent(ix,iy);
      if (x >= effNFP) {X += cont; meanX += x*cont;}
      if (y >= effNFP) {Y += cont; meanY += y*cont;}
    }
  }
  if (X > 0) meanX /= X;
  if (Y > 0) meanY /= Y;
  TLegend* leg2 = new TLegend(0.12,.85,.95,.95);
  leg2->SetTextSize(0.033);
  fitPtsHist->SetMarkerStyle(20);
  TProfile *pfx = fitPtsHist->ProfileX("_pfx",2,-1,"e"); pfx->Draw("same");
  fitPtsHist->SetMarkerColor(0);
  TProfile *pfy = fitPtsHist->ProfileY("_pfy",2,-1,"e"); pfy->Draw("same");
  leg2->AddEntry(pfy,Form("profY, <fit points. %s> = %7.2f for NFP >= %i",New.Data(),meanX,effNFP));
  leg2->AddEntry(pfx,Form("profX, <fit points. %s> = %7.2f for NFP >= %i",Old.Data(),meanY,effNFP));
  leg2->Draw();
  fitPtsHist->SetMarkerColor(1);
  DrawPng(c1);
}
//________________________________________________________________________________
void DrawRelMomDifNft(Int_t k=0, Int_t kase=0) {
  //________________________________________________________________________________
  // Momentum difference, vs fit pts
  //________________________________________________________________________________
  TH2F* pTDifNFP[2];
  if (kase == 0) for (Int_t c = 0; c < 2; c++) pTDifNFP[c] = (TH2F *) fIn->Get(Form("pTDifNFP%s%s",GP[k],charge[c]));
  else           for (Int_t c = 0; c < 2; c++) pTDifNFP[c] = (TH2F *) fIn->Get(Form("pTDifNFP5%s%s",GP[k],charge[c]));
  if (! pTDifNFP[0] || ! pTDifNFP[1]) return;
  if (pTDifNFP[0]->GetEntries() <100 || pTDifNFP[1]->GetEntries() <100) return;
  cTitle  = gTitle;
  cTitle += "P diff vs NoFitPnts for "; cTitle += GPTitle[k];
  if (kase) cTitle += " with pT > 0.5 GeV/c";
  c1 = new TCanvas(cTitle,cTitle,400,400); c1->SetLeftMargin(0.14);
  TString cTitle2(cTitle);
  cTitle2 += " Shift";
  TCanvas* c2 = new TCanvas(cTitle2,cTitle2,400,400); c2->SetLeftMargin(0.14);
  TString cTitle3(cTitle);
  cTitle3 += " Sigma";
  TCanvas* c3 = new TCanvas(cTitle3,cTitle3,400,400); c3->SetLeftMargin(0.14);
  Double_t ymax = pTDifNFP[0]->GetMaximum();
  if (pTDifNFP[1]->GetMaximum() > ymax) ymax = pTDifNFP[1]->GetMaximum();
  c1->cd();
  c1->SetLogy();
  c1->SetTicks(1,1);
  TString opt("e");
  TLegend* leg2 = new TLegend(0.72,.4,1,1);
  leg2->SetBorderSize(0);
  leg2->SetFillColor(0);
  leg2->SetTextSize(0.033);
  Int_t nx = pTDifNFP[0]->GetNbinsX();
  //    Int_t ny = pTDifNFP->GetNbinsY();
  TH1 *proj = 0;
  for (Int_t i = 1; i <= nx; i++) {
    for (Int_t c = 0; c < 2; c++) {
      proj = pTDifNFP[c]->ProjectionY(Form("%s_prj_%i",pTDifNFP[c]->GetName(),i),i,i,"e");
      proj->SetStats(0);
      proj->SetMaximum(1.1*ymax);
      proj->SetMinimum(10);
      proj->SetMarkerColor(i+1);
      proj->SetMarkerStyle(20+c);
      proj->SetTitle(pTDifNFP[c]->GetYaxis()->GetTitle());
      proj->Draw(opt); opt = "esame";
      Int_t xmin = (Int_t) pTDifNFP[c]->GetXaxis()->GetBinLowEdge(i);
      Int_t xmax = (Int_t) pTDifNFP[c]->GetXaxis()->GetBinUpEdge(i);
      leg2->AddEntry(proj,Form("%s %i<Nfp <%i",charge[c],xmin,xmax));
      cout << i << ")" << charge[c] << " = [" << xmin << "," << xmax << "]  Mean, RMS " 
	   << proj->GetMean() << ", " << proj->GetRMS() << endl;
    }
  }
  leg2->Draw();
  c2->cd();
  TString name;
  TLegend* leg7 = new TLegend(0.62,.65,.85,.85);
  leg7->SetBorderSize(0);
  leg7->SetFillColor(0);
  leg7->SetTextSize(0.033);
  TLegend* leg8 = new TLegend(0.62,.65,.85,.85);
  leg8->SetBorderSize(0);
  leg8->SetFillColor(0);
  leg8->SetTextSize(0.033);
  ymax = 0;
  Double_t ymin = 0;
  Double_t smax = 0;
  for (Int_t c = 0; c < 2; c++) {
    pTDifNFP[c]->FitSlicesY();
    name = pTDifNFP[c]->GetName(); name += "_1";
    TH1 *mu = (TH1 *) fIn->Get(name); 
    TH1 *sigma = (TH1 *) fIn->Get(name);
    if (mu->GetMaximum() > ymax) ymax = mu->GetMaximum();
    if (mu->GetMinimum() < ymin) ymin = mu->GetMinimum();
    if (sigma->GetMaximum() > smax) smax = sigma->GetMaximum();
  }
  for (Int_t c = 0; c < 2; c++) {
    c2->cd();
    pTDifNFP[c]->FitSlicesY();
    name = pTDifNFP[c]->GetName(); name += "_1";
    TH1 *mu = (TH1 *) fIn->Get(name); mu->SetMaximum(1.1*ymax); if (ymin < 0) mu->SetMinimum(1.1*ymin);
    mu->SetMarkerStyle(20);
    mu->SetMarkerColor(1+c);
    //    mu->SetTitle("Relative Shift versus no. of New fit points");
    mu->SetStats(0);
    leg7->AddEntry(mu,charge[c],"p");
    TString X(pTDifNFP[c]->GetYaxis()->GetTitle());
    TString T("Shift:");
    T += X; T += " "; T += GP[k]; if (kase) T += " with pT > 0.5 GeV/c";
    mu->SetTitle(T.Data());
    if (! c) mu->Draw();
    else     mu->Draw("same");
    c3->cd();
    name = pTDifNFP[c]->GetName(); name += "_2";
    TH1 *sigma = (TH1 *) fIn->Get(name); //sigma->SetMaximum(0.1);
    sigma->SetMarkerStyle(20);
    sigma->SetMarkerColor(1+c);
    //    sigma->SetTitle("Relative Resolution versus no. of New fit points");
    sigma->SetStats(0);
    leg8->AddEntry(sigma,charge[c],"p");
    T = "Sigma: ";
    T += X; T += " "; T += GP[k]; if (kase) T += " with pT > 0.5 GeV/c";
    sigma->SetTitle(T.Data());
    if (! c) sigma->Draw();
    else     sigma->Draw("same");
  }
  c2->cd(); leg7->Draw();
  c3->cd(); leg8->Draw();
  DrawPng(c1);
  DrawPng(c2); 
  DrawPng(c3);
}
//________________________________________________________________________________
void EffRefMult(Int_t k = 0) {
  //________________________________________________________________________________
  // Efficiency versus refMult and pT
  //________________________________________________________________________________
  TH2F *pTEf[4];
  Double_t ymax = 0;
  for (Int_t i = 0; i < 4; i++) {
    pTEf[i] = (TH2F *) fIn->Get(Form("%s%s",NameEffpT[i].Data(),GP[k]));
    if (! pTEf[i]) continue;
    Double_t y = pTEf[i]->GetMaximum();
    if (y > ymax) ymax = y;
  }
  //  cout << "ymax\t" << ymax << endl;
  if (ymax < 100) return;
  cTitle  = gTitle; cTitle += GP[k];
  TString hTitle("Momentum Dist"); hTitle += " for "; hTitle += GPTitle[k];
  cTitle += " "; cTitle += hTitle;
  c1 = new TCanvas(cTitle,cTitle,400,400); c1->SetLeftMargin(0.14);
  c1->SetLogy();
  c1->SetTicks(1,1);
  //  gStyle->SetOptTitle(0);
  //  gStyle->SetOptStat(0);
  TH1F *dummy = c1->DrawFrame(0.0,1,5.0, 10*ymax);
  dummy->SetTitle(hTitle);
  dummy->SetXTitle("pT (GeV/c)");
  c1->Update();
  TLegend* leg3 = new TLegend(.35,.6,.9,.9);
  leg3->SetBorderSize(0);
  leg3->SetFillColor(0);
  leg3->SetTextSize(0.023);
  TH1D *proj;
  for (Int_t i = 0; i < 4; i++) {
    if (pTEf[i]) {
      TAxis *yax = pTEf[i]->GetYaxis();
      Int_t ny = yax->GetNbins();
      for (Int_t j = 1; j < ny; j++) {
	proj = pTEf[i]->ProjectionX(Form("%sHi%i",pTEf[i]->GetName(),j),j,j,"e");
	//	cout << proj->GetName() << "\t" << proj->GetEntries() << endl;
	if (proj->GetEntries() <= 0) continue;
	proj->SetMarkerStyle(20+i);
	proj->SetMarkerColor(j);
	proj->Draw("same");
	Int_t xmin = (Int_t) yax->GetBinLowEdge(j);
	Int_t xmax = (Int_t) yax->GetBinUpEdge(j);
	TString title(proj->GetTitle());
	title.ReplaceAll(" versus pTnew","");
	title.ReplaceAll(" versus pTold","");
	title.ReplaceAll(" for Global","");
	title.ReplaceAll(" for Primaries","");
	leg3->AddEntry(proj,Form("%s for %i < Mult < %i",title.Data(),xmin,xmax),"P");
      }
    }
  }
  leg3->Draw();
  DrawPng(c1);
}
//________________________________________________________________________________
void DrawEfficiency(Int_t k = 0, Double_t pmax = 2.5) {
  //________________________________________________________________________________
  // Efficiency itself
  //________________________________________________________________________________
  TH2F *pTEf[4];
  TH1D *pTEfP[4];
  for (Int_t i = 0; i < 4; i++) {
    pTEf[i] = (TH2F *) fIn->Get(Form("%s%s",NameEffpT[i].Data(),GP[k]));
    if (pTEf[i]) pTEfP[i] = pTEf[i]->ProjectionX(Form("%s_all",pTEf[i]->GetName()),-1,-1,"e");
    else         pTEfP[i] = 0;
  }
  Double_t emin = 0.6;
  if (k == 1) emin = 0.9;
  cTitle  = gTitle;
  TString hTitle("");
  hTitle += GPTitle[k]; hTitle +=" track efficiencies vs pT"; 
  cTitle += hTitle;
  TCanvas* c1 = new TCanvas(cTitle,cTitle,400,400);// c1->SetLeftMargin(0.14);
  //  c1->SetTicks(1,1);
  TH1F* dummyeff= c1->DrawFrame(0.0,emin,pmax, 1.01);
  dummyeff->SetTitle(hTitle);
  dummyeff->SetXTitle("pT (GeV/c)");
  dummyeff->SetYTitle(Form("fit points>=%i rel. efficiency",effNFP)); 
  TLegend* leg4 = new TLegend(.2,.15,.5,.4);
#if 0
  leg4->SetBorderSize(0);
  leg4->SetFillColor(0);
  leg4->SetTextSize(0.033);
#endif
  TAxis *y = pTEf[0]->GetYaxis();
  Int_t ny = y->GetNbins();
  Double_t yMin = y->GetXmin();
  Double_t yMax = y->GetXmax();
  Int_t mstep = 2;
  Int_t m = ny + 1;
  for (; m >= 0; m -= mstep) {
    if (m < 0) m = 0;
    Int_t m1 = -1, m2 = -1;
    if (m == 0) {}
    else {
      m1 = m-mstep+1;
      m2 = m;
    }
    for (Int_t i = 0; i < 4; i++) {
      pTEfP[i] = pTEf[i]->ProjectionX(Form("%s_all_%i",pTEf[i]->GetName(),m),m1,m2,"e");
    }
    if (! pTEfP[0] || pTEfP[0]->GetEntries() < 1e3) continue;
    if (! pTEfP[1] || pTEfP[1]->GetEntries() < 1e3) continue;
    TH1D *effNewP = new TH1D(*pTEfP[3]); effNewP->SetName(Form("eff%s%i",New.Data(),m)); effNewP->SetTitle(hTitle);
    effNewP->Divide(pTEfP[3],pTEfP[1],1.,1.,"b");
    TH1D *effOldP = new TH1D(*pTEfP[2]); effOldP->SetName(Form("eff%s%i",Old.Data(),m)); effOldP->SetTitle(hTitle);
    effOldP->Divide(pTEfP[2],pTEfP[0],1.,1.,"b");
    effOldP->SetMarkerStyle(21); 
    effOldP->SetMarkerColor(1);
    effNewP->SetMarkerStyle(20); 
    effNewP->SetMarkerColor(1);
    if (m) {
      effOldP->SetMarkerColor(m/mstep+2);
      effNewP->SetMarkerColor(m/mstep+2);
    }
    Int_t ymin = (Int_t) pTEf[0]->GetYaxis()->GetBinLowEdge(m-mstep+1);
    Int_t ymax = (Int_t) pTEf[0]->GetYaxis()->GetBinUpEdge(m);
    c1->cd();
    TString NewName;
    if (! m) NewName = " for All";
    else {
      if      (ymin < yMin) NewName = Form(" for Mult < %i", ymax);
      else if (ymax > yMax) NewName = Form(" for Mult > %i", ymin);
      else                  NewName = Form(" for %i < Mult < %i", ymin, ymax);
    }
    TString NewcName = c1->GetName(); NewcName += NewName;
    cout << NewcName.Data() << endl;
    effOldP->Draw("same");
    effNewP->Draw("same");
    leg4->AddEntry(effOldP,Form("%s efficiency %s",Old.Data(),NewName.Data()));
    leg4->AddEntry(effNewP,Form("%s efficiency %s",New.Data(),NewName.Data()));
    TCanvas* c3 = new TCanvas(NewcName,NewcName,400,400);// c3->SetLeftMargin(0.14);
  //  c3->SetTicks(1,1);
    TH1F* dummyeff3= c3->DrawFrame(0.0,emin,pmax, 1.01);
    TString ttitle(hTitle);
    ttitle += NewName;
    dummyeff3->SetTitle(ttitle);
    dummyeff3->SetXTitle("pT (GeV/c)");
    dummyeff3->SetYTitle(Form("fit points>=%i rel. efficiency",effNFP)); 
    effOldP->Draw("same");
    effNewP->Draw("same");
    TLegend* leg5 = new TLegend(.2,.15,.8,.35);
    leg5->AddEntry(effOldP,Form("%s efficiency %s",Old.Data(),NewName.Data()));
    leg5->AddEntry(effNewP,Form("%s efficiency %s",New.Data(),NewName.Data()));
    leg5->Draw();
    DrawPng(c3);
  }
  c1->cd();
  leg4->Draw();
  DrawPng(c1);
}
//________________________________________________________________________________
void DrawPhiEfficiency(Int_t k = 0) {
  //________________________________________________________________________________
  // Efficiency itself
  //________________________________________________________________________________
  TH2F *PhiEf[4];
  TH1D *PhiEfP[4];
  for (Int_t i = 0; i < 4; i++) {
    PhiEf[i] = (TH2F *) fIn->Get(Form("%s%s",NameEffPhi[i].Data(),GP[k]));
    if (PhiEf[i]) PhiEfP[i] = PhiEf[i]->ProjectionX(Form("%s_all",PhiEf[i]->GetName()),-1,-1,"e");
    else         PhiEfP[i] = 0;
  }
  Double_t emin = 0.7;
  if (k == 1) emin = 0.9;
  cTitle  = gTitle;
  TString hTitle("");
  hTitle += GPTitle[k]; hTitle +=" track efficiencies vs #phi"; 
  cTitle += hTitle;
  TCanvas* c1 = new TCanvas(cTitle,cTitle,400,400);// c1->SetLeftMargin(0.14);
  //  c1->SetTicks(1,1);
  TH1F* dummyeff= c1->DrawFrame(-180,emin,180, 1.01);
  dummyeff->SetTitle(hTitle);
  dummyeff->SetXTitle("#phi (degree)");
  dummyeff->SetYTitle(Form("fit points>=%i rel. efficiency",effNFP)); 
  TLegend* leg4 = new TLegend(.2,.15,.5,.4);
#if 0
  leg4->SetBorderSize(0);
  leg4->SetFillColor(0);
  leg4->SetTextSize(0.033);
#endif
  TAxis *y = PhiEf[0]->GetYaxis();
  Int_t ny = y->GetNbins();
  Double_t yMin = y->GetXmin();
  Double_t yMax = y->GetXmax();
  Int_t mstep = 2;
  Int_t m = ny + 1;
  for (; m >= 0; m -= mstep) {
    if (m < 0) m = 0;
    Int_t m1 = -1, m2 = -1;
    if (m == 0) {}
    else {
      m1 = m-mstep+1;
      m2 = m;
    }
    for (Int_t i = 0; i < 4; i++) {
      PhiEfP[i] = PhiEf[i]->ProjectionX(Form("%s_all_%i",PhiEf[i]->GetName(),m),m1,m2,"e");
    }
    if (! PhiEfP[0] || PhiEfP[0]->GetEntries() < 1e3) continue;
    if (! PhiEfP[1] || PhiEfP[1]->GetEntries() < 1e3) continue;
    TH1D *effNewP = new TH1D(*PhiEfP[3]); effNewP->SetName(Form("eff%s%i",New.Data(),m)); effNewP->SetTitle(hTitle);
    effNewP->Divide(PhiEfP[3],PhiEfP[1],1.,1.,"b");
    TH1D *effOldP = new TH1D(*PhiEfP[2]); effOldP->SetName(Form("eff%s%i",Old.Data(),m)); effOldP->SetTitle(hTitle);
    effOldP->Divide(PhiEfP[2],PhiEfP[0],1.,1.,"b");
    effOldP->SetMarkerStyle(21); 
    effOldP->SetMarkerColor(1);
    effNewP->SetMarkerStyle(20); 
    effNewP->SetMarkerColor(1);
    if (m) {
      effOldP->SetMarkerColor(m/mstep+2);
      effNewP->SetMarkerColor(m/mstep+2);
    }
    Int_t ymin = (Int_t) PhiEf[0]->GetYaxis()->GetBinLowEdge(m-mstep+1);
    Int_t ymax = (Int_t) PhiEf[0]->GetYaxis()->GetBinUpEdge(m);
    c1->cd();
    TString NewName;
    if (! m) NewName = " for All";
    else {
      if      (ymin < yMin) NewName = Form(" for Mult < %i", ymax);
      else if (ymax > yMax) NewName = Form(" for Mult > %i", ymin);
      else                  NewName = Form(" for %i < Mult < %i", ymin, ymax);
    }
    TString NewcName = c1->GetName(); NewcName += NewName;
    cout << NewcName.Data() << endl;
    effOldP->Draw("same");
    effNewP->Draw("same");
    leg4->AddEntry(effOldP,Form("%s efficiency %s",Old.Data(),NewName.Data()));
    leg4->AddEntry(effNewP,Form("%s efficiency %s",New.Data(),NewName.Data()));
    TCanvas* c3 = new TCanvas(NewcName,NewcName,400,400);// c3->SetLeftMargin(0.14);
  //  c3->SetTicks(1,1);
    TH1F* dummyeff3= c3->DrawFrame(-180,emin,180, 1.01);
    TString ttitle(hTitle);
    ttitle += NewName;
    dummyeff3->SetTitle(ttitle);
    dummyeff3->SetXTitle("#phi (degrees)");
    dummyeff3->SetYTitle(Form("fit points>=%i rel. efficiency",effNFP)); 
    effOldP->Draw("same");
    effNewP->Draw("same");
    TLegend* leg5 = new TLegend(.2,.15,.8,.35);
    leg5->AddEntry(effOldP,Form("%s efficiency %s",Old.Data(),NewName.Data()));
    leg5->AddEntry(effNewP,Form("%s efficiency %s",New.Data(),NewName.Data()));
    leg5->Draw();
    DrawPng(c3);
  }
  c1->cd();
  leg4->Draw();
  DrawPng(c1);
}
//________________________________________________________________________________
void DrawEffVsMult(Int_t k = 0) {
  //________________________________________________________________________________
  // Efficiency itself
  //________________________________________________________________________________
  cTitle  = gTitle;
  TString hTitle("");
  hTitle += " Efficiencies"; hTitle += " versus Multiplicity for "; hTitle += GPTitle[k];
  cTitle += hTitle;
  TCanvas* cnv4 = new TCanvas(cTitle,cTitle,400,400); c1 = cnv4; c1->SetLeftMargin(0.14);
  c1->SetTicks(1,1);
  TH2F *pTEf[4];
  TH1D *pTEfP[4];
  for (Int_t i = 0; i < 4; i++) {
    pTEf[i] = (TH2F *) fIn->Get(Form("%s%s",NameEffpT[i].Data(),GP[k]));
    if (pTEf[i]) pTEfP[i] = pTEf[i]->ProjectionY(Form("%s_all",pTEf[i]->GetName()),-1,-1,"e");
    else         pTEfP[i] = 0;
  }
  TH1F* dummyeff= cnv4->DrawFrame(0.0,0.6,700, 1.05);
  dummyeff->SetTitle(hTitle);
  dummyeff->SetXTitle("(uncorrected) Multiplicity");
  dummyeff->SetYTitle(Form("fit points>=%i rel. efficiency",effNFP)); 
  TLegend* leg4 = new TLegend(.2,.15,.5,.4);
#if 0
  leg4->SetBorderSize(0);
  leg4->SetFillColor(0);
  leg4->SetTextSize(0.033);
#endif
  TAxis *x = pTEf[0]->GetXaxis();
  Double_t xMin = x->GetXmin();
  Double_t xMax = x->GetXmax();
  Double_t pTs[4] = {0.1, 0.2, 0.5, 2.0};
  for (Int_t m = 0; m < 5; m++) {
    if (m < 0) m = 0;
    Int_t m1 = -1, m2 = -1;
    if (m == 0) {}
    else {
      m1            = x->FindBin(pTs[m-1]);
      if (m < 4) m2 = x->FindBin(pTs[m])-1;
    }
    for (Int_t i = 0; i < 4; i++) {
      pTEfP[i] = pTEf[i]->ProjectionY(Form("%s_all_%i",pTEf[i]->GetName(),m),m1,m2,"e");
    }
    if (! pTEfP[0] || pTEfP[0]->GetEntries() < 1e3) continue;
    if (! pTEfP[1] || pTEfP[1]->GetEntries() < 1e3) continue;
    TH1D *effNewP = new TH1D(*pTEfP[3]); effNewP->SetName(Form("eff%s%i",New.Data(),m)); effNewP->SetTitle(hTitle);
    effNewP->Divide(pTEfP[3],pTEfP[1],1.,1.,"b");
    TH1D *effOldP = new TH1D(*pTEfP[2]); effOldP->SetName(Form("eff%s%i",Old.Data(),m)); effOldP->SetTitle(hTitle);
    effOldP->Divide(pTEfP[2],pTEfP[0],1.,1.,"b");
    effOldP->SetMarkerStyle(21); 
    effOldP->SetMarkerColor(1);
    effNewP->SetMarkerStyle(20); 
    effNewP->SetMarkerColor(1);
    if (m) {
      effOldP->SetMarkerColor(m+1);
      effNewP->SetMarkerColor(m+1);
    }
    Double_t xmin = xMin;
    Double_t xmax = xMax;
    if (m1 >= 0)  xmin = x->GetBinLowEdge(m1);
    if (m2 >= 0)  xmax = x->GetBinUpEdge(m2);
    effOldP->Draw("same");
    effNewP->Draw("same");
    if (! m) {
      leg4->AddEntry(effOldP,Form("%s efficiency for ALL",Old.Data()));
      leg4->AddEntry(effNewP,Form("%s efficiency for ALL",New.Data()));
    } else {
      if        (xmin < xMin) {
	leg4->AddEntry(effOldP,Form("%s efficiency for pT < %4.1f",Old.Data(), xmax));
	leg4->AddEntry(effNewP,Form("%s efficiency for pT < %4.1f",New.Data(), xmax));
      } else if (xmax > xMax) {	        			       
	leg4->AddEntry(effOldP,Form("%s efficiency for pT > %4.1f",Old.Data(), xmin));
	leg4->AddEntry(effNewP,Form("%s efficiency for pT > %4.1f",New.Data(), xmin));
      } else {
	leg4->AddEntry(effOldP,Form("%s efficiency for %4.1f < pT < %4.1f", Old.Data(), xmin, xmax));
	leg4->AddEntry(effNewP,Form("%s efficiency for %4.1f < pT < %4.1f", New.Data(), xmin, xmax));
      }
    }
  }
  leg4->Draw();
  DrawPng(c1);
}
//________________________________________________________________________________
void DrawpTDiff(Int_t k=0, Double_t pmax = 2.5) {
  //________________________________________________________________________________
  // Pt difference, vs pt
  //________________________________________________________________________________
  cTitle  = gTitle;
  TString hTitle("");
  hTitle += Form("<p_{T}^{%s} - p_{T}^{%s}> (GeV/c) versus p_{T}",Old.Data(),New.Data()); hTitle += " for "; hTitle += GPTitle[k];
  cTitle += "<p_{T}^{Old} - p_{T}^{New}> (GeV/c) versus p_{T}"; cTitle += " for "; cTitle += GPTitle[k];
  TCanvas* ptdiffgrcnv = new TCanvas(cTitle,cTitle,400,400); c1 = ptdiffgrcnv; c1->SetLeftMargin(0.14);
  TH1F* dummy2 = c1->DrawFrame(0,-.05,pmax, 0.05);
  dummy2->SetTitle(hTitle);
  dummy2->SetXTitle("p_{T} (GeV/c)");
  dummy2->SetYTitle(Form("p_{T}^{%s} - p_{T}^{%s} GeV/c", Old.Data(), New.Data()));
  dummy2->GetYaxis()->SetTitleOffset(1.75);
  //  c1->SetLeftMargin(0.15);
  //  c1->SetTicks(1,1);
  TLegend* leg7 = new TLegend(0.22,.65,.45,.85);
  leg7->SetBorderSize(0);
  leg7->SetFillColor(0);
  leg7->SetTextSize(0.033);
  for (Int_t i = 0; i < 2; i++) {
    TH2F *pTdiff = (TH2F *) fIn->Get(Form("pTdiff%s%s",charge[i],GP[k]));
    if (! pTdiff) continue;
    TProfile *profx = pTdiff->ProfileX();
    profx->SetMarkerStyle(20+i);
    profx->SetMarkerColor(i+1);
    profx->Draw("same");
    if (i == 0) leg7->AddEntry(profx,"Positives","P");
    else        leg7->AddEntry(profx,"Negatives","P");
  }
  leg7->Draw();
  DrawPng(c1);
}
//________________________________________________________________________________
void DrawPhiDiff(Int_t k=0, const Char_t *opt = "Phi") {
  //________________________________________________________________________________
  // Pt difference, vs pt
  //________________________________________________________________________________
  cTitle  = gTitle;
  TString hTitle;
  hTitle += Form("(p_{T}^{%s} - p_{T}^{%s})/p_{T}^{%s} versus #phi_{%s} ", Old.Data(), New.Data(),New.Data(),New.Data()); hTitle += " for "; hTitle += GPTitle[k];
  if (TString(opt).Contains("Phi5")) hTitle += " with pT > 0.5 GeV/c";
  if (TString(opt).Contains("EP"))  hTitle += " and #eta > 0";
  if (TString(opt).Contains("EN"))  hTitle += " and #eta < 0";
  cTitle += hTitle;
  TCanvas* ptdiffgrcnv = new TCanvas(cTitle,cTitle,400,400); c1 = ptdiffgrcnv; c1->SetLeftMargin(0.14);
  TH1F* dummy2 = c1->DrawFrame(-TMath::Pi(),-.01,TMath::Pi(),.02);
  dummy2->SetTitle(hTitle);
  dummy2->SetXTitle("#phi");
  dummy2->SetYTitle(Form("(p_{T}^{%s} - p_{T}^{%s})/p_{T}^{%s}", Old.Data(), New.Data(), New.Data()));
  dummy2->GetYaxis()->SetTitleOffset(1.75);
  //  c1->SetLeftMargin(0.15);
  //  c1->SetTicks(1,1);
  TLegend* leg7 = new TLegend(0.22,.65,.45,.85);
  leg7->SetBorderSize(0);
  leg7->SetFillColor(0);
  leg7->SetTextSize(0.033);
  TF1 *gPhi = (TF1 *) gROOT->GetFunction("gPhi");
  if (! gPhi) {
    gPhi = new TF1("gPhi","([0]+[1]*x)*sin(x+[2])+[3]",-4,4);
    gPhi->SetParameters(-1.34684e-03,-2.05069e-04,4.76465e-01,4.30224e-03);
  }
  for (Int_t i = 0; i < 2; i++) {
    TH2F *pTdiff = (TH2F *) fIn->Get(Form("%sdiffR%s%s",opt,charge[i],GP[k]));
    if (! pTdiff) continue;
    pTdiff->FitSlicesY(0,0,0,100,"qnr");
    TH1 *profx = (TH1 *) fIn->Get(Form("%sdiffR%s%s_1",opt,charge[i],GP[k]));
    if (! profx) continue;
    profx->Fit(gPhi,"0");
    profx->SetMarkerStyle(20);
    profx->SetMarkerColor(i+1);
    TF1 *fu = profx->GetFunction("gPhi");
    if (fu) {
      fu->SetLineColor(i+1);
      fu->Draw("same");
    }
    profx->Draw("same");
    if (i == 0) leg7->AddEntry(profx,"Positives","P");
    else        leg7->AddEntry(profx,"Negatives","P");
  }
  leg7->Draw();
  DrawPng(c1);
}
//________________________________________________________________________________
void DrawRpTDiff(Int_t k = 0, const Char_t *opt="pTdiffR", Double_t pmax = 2.5) {
  //________________________________________________________________________________
  // relative Pt difference, vs pt
  //________________________________________________________________________________
  
  cTitle  = gTitle;
  TString yTitle, sTitle;
  if (TString(opt) == "pTInvdiffR") {
    cTitle += "Inv pT diff (perc.) vs pT"; 
    yTitle = Form("(1/p_{T}^{%s} - 1/p_{T}^{%s})/1/p_{T}^{%s}", Old.Data(), New.Data(), New.Data());
    sTitle = Form("Sigma of Relative difference 1/pT (%s - %s)/%s vs pT", Old.Data(), New.Data(), New.Data());
  } else {
    cTitle += "pT diff (perc.) vs pT"; 
    yTitle = Form("(p_{T}^{%s} - p_{T}^{%s})/p_{T}^{%s}",  Old.Data(), New.Data(), New.Data());
    sTitle = Form("Sigma of Relative difference pT (%s - %s)/%s vs pT", Old.Data(), New.Data(), New.Data());
  }
  cTitle += " for "; cTitle += GPTitle[k];
  c1 = new TCanvas(cTitle,cTitle,400,400); c1->SetLeftMargin(0.14);
  c1->SetTicks(1,1);
  c1->SetLeftMargin(0.15);
  TString cTitle2(cTitle);
  cTitle2 += "Sigma";
  TCanvas *c2 = new TCanvas(cTitle2,cTitle2,400,400); c2->SetLeftMargin(0.14);
  c2->SetTicks(1,1);
  c2->SetLeftMargin(0.15);
  TString cTitle3(cTitle);
  c1->cd();
  TLegend* leg8 = new TLegend(0.22,.65,.45,.85);
  leg8->SetBorderSize(0);
  leg8->SetFillColor(0);
  leg8->SetTextSize(0.033);
  TLegend* leg9 = new TLegend(0.22,.65,.45,.85);
  leg9->SetBorderSize(0);
  leg9->SetFillColor(0);
  leg9->SetTextSize(0.033);
  for (Int_t i = 0; i < 2; i++) {
    TH2F *pTdiffR = (TH2F *) fIn->Get(Form("%s%s%s",opt,charge[i],GP[k]));
    if (! pTdiffR) {cout << "Histogram " << Form("%s%s%s",opt,charge[i],GP[k]) << " is not found" << endl; continue;}
    c1->cd(1);
    pTdiffR->FitSlicesY();
    TH1 *mu = (TH1 *) fIn->Get(Form("%s%s%s_1",opt,charge[i],GP[k])); 
    if (mu) {
      c1->cd();
      mu->SetAxisRange(0,pmax);
      mu->SetTitle(cTitle);
      mu->SetXTitle("pT (GeV/c)");
      mu->SetYTitle(yTitle);
      mu->GetYaxis()->SetTitleOffset(1.75);
      mu->SetMinimum(-0.02);
      mu->SetMaximum(0.03);
      mu->SetStats(0);
      mu->SetMarkerStyle(20); mu->SetMarkerColor(i+1);
      if (i == 0) mu->Draw();
      else mu->Draw("same");
      if (i == 0) leg8->AddEntry(mu,"Positives (fit)","P");
      else        leg8->AddEntry(mu,"Negatives (fit)","P");
    } else {cout << "Histogram " << Form("%s%s%s_1",opt,charge[i],GP[k]) << " is not found" << endl;}
    TH1 *sigma = (TH1 *) fIn->Get(Form("%s%s%s_2",opt,charge[i],GP[k])); 
    if (sigma) {
      c2->cd();
      sigma->SetMarkerStyle(20); sigma->SetMarkerColor(i+1);
      sigma->SetStats(0);
      if (i == 0) {
	sigma->SetAxisRange(0,pmax);
	sigma->SetTitle(sTitle);
	sigma->SetXTitle("pT (GeV/c)");
	TString s("#sigma(");
	s += yTitle; s+= ")";
	sigma->SetYTitle(s);
	sigma->GetYaxis()->SetTitleOffset(1.75);
	sigma->SetMinimum(0);
	sigma->SetMaximum(0.10);
	sigma->Draw();
      } else
	sigma->Draw("same");
      if (i == 0) leg9->AddEntry(mu,"Positives (fit)","P");
      else        leg9->AddEntry(mu,"Negatives (fit)","P");
      
    } else {cout << "Histogram " << Form("%s%s%s_2",opt,charge[i],GP[k]) << " is not found" << endl;}
  } 
  c1->cd();
  leg8->Draw();
  c2->cd();
  leg9->Draw();
  DrawPng(c1);
  DrawPng(c2);
}
//________________________________________________________________________________
void Init(const Char_t *file="Plots.root") {
  TString pwd(gSystem->BaseName( gSystem->WorkingDirectory()));
  TObjArray *obj = pwd.Tokenize("_");
  Int_t nParsed = obj->GetEntries();
  if (nParsed == 2) {
    Old = ((TObjString *) obj->At(0))->GetName();
    New = ((TObjString *) obj->At(1))->GetName();
    NameEffpT[0] = Form("pT%s",New.Data());
    NameEffpT[1] = Form("pT%s",Old.Data());
    NameEffpT[2] = Form("pTO%s",New.Data());
    NameEffpT[3] = Form("pTO%s",Old.Data());
    NameEffPhi[0] = Form("Phi%s",New.Data());
    NameEffPhi[1] = Form("Phi%s",Old.Data());
    NameEffPhi[2] = Form("PhiO%s",New.Data());
    NameEffPhi[3] = Form("PhiO%s",Old.Data());
  }
  gTitle = file;
  if (gTitle != "") {
    fIn = new TFile(gTitle);
    //  gTitle.ReplaceAll(".Plots.root","");
    gTitle = "";
  }
}
//________________________________________________________________________________
void DrawPrimVx() {
  const Char_t *XYZ[3] = {"X","Y","Z"};
  for (Int_t i = 0; i < 3; i++) {
    TH1* h = (TH1 *) gDirectory->Get(Form("DifPv%s",XYZ[i]));
    if (! h) continue;
    TCanvas *c = new TCanvas(Form("PrimaryVertex%sdiff",XYZ[i]),Form("PrimaryVertex%sdiff",XYZ[i]));
    h->Draw();
    DrawPng(c);
  }
}
//________________________________________________________________________________
void DrawCharge() {
  const Char_t *hNames[2] = {"Charge","Charge15"};
  //  const Char_t *fNames[2] = {Form("%sVs%sChargeAllPr",New.Data(),Old.Data()),Form("%sVs%sChargePrNFPGE15",New.Data(),Old.Data())};
  const Char_t *fNames[2] = {"NewVsOldChargeAllPr","NewVsOldChargePrNFPGE15"};
  for (Int_t i = 0; i < 2; i++) {
    TH2 *h = (TH2 *) gDirectory->Get(hNames[i]);
    if (! h ) continue;
    TCanvas *c = new TCanvas(fNames[i],fNames[i]);
    h->Draw("text");
    DrawPng(c);
  }
}
//________________________________________________________________________________
void Draw(const Char_t *file="Plots.root") {
  Init(file);
  DrawFitPnts();  DrawFitPnts(1);
  DrawRelMomDifNft(0,0);  DrawRelMomDifNft(1,0);
  DrawRelMomDifNft(0,5);  DrawRelMomDifNft(1,5);
  EffRefMult(0);  EffRefMult(1);
  DrawEfficiency(0);  DrawEfficiency(1);
  DrawPhiEfficiency(0);  DrawPhiEfficiency(1);
  DrawpTDiff(0); DrawpTDiff(1);
  DrawRpTDiff(0);DrawRpTDiff(1);
  DrawRpTDiff(0,"pTInvdiffR");  DrawRpTDiff(1,"pTInvdiffR");
  DrawPrimVx();
  DrawCharge();
#if 0
  DrawPhiDiff(0);DrawPhiDiff(1);
  DrawPhiDiff(0,"Phi5");DrawPhiDiff(1,"Phi5");
  DrawPhiDiff(0,"PhiEP");DrawPhiDiff(1,"PhiEP");
  DrawPhiDiff(0,"Phi5EP");DrawPhiDiff(1,"Phi5EP");
  DrawPhiDiff(0,"PhiEN");DrawPhiDiff(1,"PhiEN");
  DrawPhiDiff(0,"Phi5EN");DrawPhiDiff(1,"Phi5EN");
#endif
}
//________________________________________________________________________________
void TbyTPlots(const Char_t *file = 0, Int_t Nentries=0) {
  TString Out(file);
  Init(Out.Data());
  if (Out == "") {
    TString TreeName("trackMateComp");
    fChain = new TChain(TreeName);
    TFileSet dir(".");
    TDataSetIter next(&dir,0);
    TDataSet *set = 0;
    TFile *f = 0;
    Int_t FileNo = 0;
    Int_t Ntotal = 0;
    TString tFile("");
    while ((set = next())) {
      TString Title(set->GetTitle());
      if (Title != "file") continue;
      TString Name(set->GetName());
      if (! Name.EndsWith(".root")) continue;
      if (! (Name.BeginsWith("trackMateFile") || Name.BeginsWith("TbyT"))) continue;
      f = new TFile(Name);
      TChain *tree = (TChain *) f->Get(TreeName);
      if (tree) {
	const Char_t *pFile = f->GetName();
	ULong64_t nEvents = tree->GetEntries();
	if (nEvents > 0) {
	  FileNo++; 
	  Ntotal += nEvents;
	  cout << "\tAdd "<< FileNo << "\t" << nEvents << "\tEvents, Total = " << Ntotal << endl;
	  fChain->Add(pFile);
	}
	delete f;
      }
    }
    fChain->SetBranchAddress("data_array",&data.oldPtGl);
    fChain->SetBranchAddress("ev_array",&refMult);
    tFile.ReplaceAll(".root","");
    tFile.ReplaceAll("trackMate_physics_","");
    tFile.ReplaceAll("trackMate_","");
    tFile.ReplaceAll("5043073_raw_1010010_","");
    tFile.ReplaceAll("5046043_raw_1010010_","");
    Out = tFile;
    Out += "Plots.root";
    TFile *fOut = new TFile(Out.Data(),"recreate");
    cout << "Opened " << Out << endl;
    // book
    TString Title;
    TString Name;
    TH1D *DifPvX = new TH1D("DifPvX",Form("Difference in X for %s - %s positions",New.Data(),Old.Data()),100,-0.25,0.25);
    TH1D *DifPvY = new TH1D("DifPvY",Form("Difference in Y for %s - %s positions",New.Data(),Old.Data()),100,-0.25,0.25);
    TH1D *DifPvZ = new TH1D("DifPvZ",Form("Difference in Z for %s - %s positions",New.Data(),Old.Data()),100,-0.25,0.25);
    TH2D *Charge = new TH2D("Charge",Form("Charge flip between %s and %s for all primaries",New.Data(),Old.Data()),3,-1.5,1.5,3,-1.5,1.5);
    Charge->SetXTitle(Form("%s charge",Old.Data()));
    Charge->SetYTitle(Form("%s charge",New.Data()));
    TH2D *Charge15 = new TH2D("Charge15",Form("Charge flip between %s and %s for all primaries with NFP >= 15",New.Data(),Old.Data()),3,-1.5,1.5,3,-1.5,1.5);
    Charge15->SetXTitle(Form("%s charge",Old.Data()));
    Charge15->SetYTitle(Form("%s charge",New.Data()));
    //________________________________________________________________________________
    // Fit pts correlation
    //________________________________________________________________________________
    Name = "fitPtsHist", Title = Form("Fit Pts %s vs %s",Old.Data(), New.Data());
    TH2F* fitPtsHist = new TH2F(Name,Title,50,0,50,50,0,50);
    fitPtsHist->SetXTitle(Form("fit points, %s",New.Data()));
    fitPtsHist->SetYTitle(Form("fit points, %s",Old.Data()));
    Name = "fitPtsHistPing", Title = Form("Fit Pts %s vs %s matched", Old.Data(), New.Data());
    TH2F* fitPtsHistPing = new TH2F(Name,Title,50,0,50,50,0,50);
    fitPtsHist->SetXTitle(Form("fit points, %s",New.Data()));
    fitPtsHist->SetYTitle(Form("fit points, %s",Old.Data()));
    //________________________________________________________________________________
    // Relative Momentum difference, vs fit pts
    //________________________________________________________________________________
    TH2F* pTDifNFP[2][2];
    TH2F* pTDifNFP5[2][2];
    for (Int_t k = 0; k < 2; k++) {
      for (Int_t c = 0; c < 2; c++) {
	Name = Form("pTDifNFP%s%s",GP[k],charge[c]);
	Title = Form("Relative Momentum difference vs no. of %s fit pts for %s tracks",New.Data(),GPTitle[k]);
	pTDifNFP[k][c] = new TH2F(Name,Title, 7,10.,45.,200,-.2,.2);
	pTDifNFP[k][c]->SetYTitle(Form("Momentum Difference (pT_%s - pT_%s)/pT_%s",Old.Data(),New.Data(),New.Data()));
	pTDifNFP[k][c]->SetXTitle(Form("No. %s fit points",New.Data()));
	pTDifNFP[k][c]->SetMarkerStyle(20);
	Name = Form("pTDifNFP5%s%s",GP[k],charge[c]);
	Title = Form("Relative Momentum difference vs no. of %s fit pts for %s tracks with pT > 0.5 GeV/c",New.Data(),GPTitle[k]);
	pTDifNFP5[k][c] = new TH2F(Name,Title, 7,10.,45.,200,-.2,.2);
	pTDifNFP5[k][c]->SetYTitle(Form("Momentum Difference (pT_%s - pT_%s)/pT_%s", Old.Data(), New.Data(), New.Data()));
	pTDifNFP5[k][c]->SetXTitle(Form("No. %s fit points",New.Data()));
	pTDifNFP5[k][c]->SetMarkerStyle(20);
      }
    }
    //________________________________________________________________________________
    // Efficiency versus Mult and pT
    //________________________________________________________________________________
    //
    TH2F *pTEf[2][4]; 
    const Char_t *TitleEffPt[4] = {Form("%s Fit Pts>%i %s versus pT_%s |#eta| < 0.5",New.Data(),effNFP,New.Data(),New.Data()),
				   Form("%s Fit Pts>%i %s versus pT_%s |#eta| < 0.5",Old.Data(),effNFP,Old.Data(),Old.Data()),
				   Form("%s & %s Fit Pts>%i %s and %s versus pT_%s |#eta| < 0.5",New.Data(),Old.Data(),effNFP,Old.Data(),New.Data(),New.Data()),
				   Form("%s & %s Fit Pts>%i %s and %s versus pT_%s |#eta| < 0.5",New.Data(),Old.Data(),effNFP,Old.Data(),New.Data(),Old.Data())};
    for (Int_t k = 0; k < 2; k++) {
      for (Int_t i = 0; i < 4; i++) {
	Name = Form("%s%s",NameEffpT[i].Data(),GP[k]);
	Title = Form("%s for %s",TitleEffPt[i],GPTitle[k]);
	pTEf[k][i] = new TH2F(Name,Title,70,0,7,7,0,700);
	pTEf[k][i]->Sumw2();
	pTEf[k][i]->SetMarkerStyle(20+i);
	pTEf[k][i]->SetMarkerColor(1+i);
      }
    }
    // Efficiency versus Mult and Phi
    //________________________________________________________________________________
    //
    const Char_t *TitleEffPhi[4] = {Form("%s Fit Pts>%i %s versus #phi_%s |#eta| < 0.5",New.Data(),effNFP,New.Data(),New.Data()),
				    Form("%s Fit Pts>%i %s versus #phi_%s |#eta| < 0.5",Old.Data(),effNFP,Old.Data(),Old.Data()),
				    Form("%s & %s Fit Pts>%i %s and %s versus #phi_%s |#eta| < 0.5",New.Data(),Old.Data(),effNFP,Old.Data(),New.Data(),New.Data()),
				    Form("%s & %s Fit Pts>%i %s and %s versus #phi_%s |#eta| < 0.5",New.Data(),Old.Data(),effNFP,Old.Data(),New.Data(),Old.Data())};
    TH2F *PhiEf[2][4]; 
    for (Int_t k = 0; k < 2; k++) {
      for (Int_t i = 0; i < 4; i++) {
	Name = Form("%s%s",NameEffPhi[i].Data(),GP[k]);
	Title = Form("%s for %s",TitleEffPhi[i],GPTitle[k]);
	PhiEf[k][i] = new TH2F(Name,Title,180,-180,180,7,0,700);
	PhiEf[k][i]->Sumw2();
	PhiEf[k][i]->SetMarkerStyle(20+i);
	PhiEf[k][i]->SetMarkerColor(1+i);
      }
    }
    //________________________________________________________________________________
    // Pt difference, vs pt
    //________________________________________________________________________________
    TH2F* pTdiff[2][2];
    TH2F* pTdiffR[2][2];
    TH2F* pTInvdiffR[2][2];
    TH2F* PhidiffR[3][2][2];
    TH2F* Phi5diffR[3][2][2];
    for (Int_t k = 0; k < 2; k++) {
      for (Int_t i = 0; i < 2; i++) {
	Name = Form("pTdiff%s%s",charge[i],GP[k]);
	Title = Form("pT_%s - pT_%s for %s",Old.Data(),New.Data(),GPTitle[k]);
	pTdiff[k][i] = new TH2F(Name,Title,40,0,4,200,-.8,.8);
	pTdiff[k][i]->SetXTitle(Form("p_{T} %s (GeV/c)",New.Data()));
	TString YTitle = Form("p_{T} Difference (pT_%s - pT_%s),",Old.Data(),New.Data());
	TString title("");
	if (i == 0) 
	  title += " +";
	else 
	  title += " -";
	title += "charge";
	if (k == 0) title += " for Globals";
	else        title += " for Primaries";
	TString yTitle = YTitle; yTitle += title;
	pTdiff[k][i]->SetYTitle(yTitle);
	pTdiff[k][i]->Sumw2();
	pTdiff[k][i]->SetMarkerStyle(20);
	pTdiff[k][i]->SetMarkerColor(i+2);
	Name = Form("pTdiffR%s%s",charge[i],GP[k]);
	pTdiffR[k][i] = new TH2F(Name,Title,40,0,4,600,-0.15,0.15);
	pTdiffR[k][i]->SetXTitle(Form("p_{T} %s (GeV/c)",New.Data()));
	YTitle = Form("(pT_%s - pT_%s)/pT_%s,",Old.Data(),New.Data(),New.Data());
	yTitle = YTitle; yTitle += title;
	pTdiffR[k][i]->SetYTitle(yTitle);
	pTdiffR[k][i]->Sumw2();
	pTdiffR[k][i]->SetMarkerStyle(20);
	pTdiffR[k][i]->SetMarkerColor(i+2);
	
	Name = Form("pTInvdiffR%s%s",charge[i],GP[k]);
	pTInvdiffR[k][i] = new TH2F(Name,Title,40,0.,4.,600,-0.15,0.15);
	pTInvdiffR[k][i]->SetXTitle(Form("pT_%s",New.Data()));
	YTitle = Form("(1/pT_%s - 1/pT_%s)/(1/pT_%s), versus pT_%s",Old.Data(),New.Data(),New.Data(),New.Data());
	yTitle = YTitle; yTitle += title;
	pTInvdiffR[k][i]->SetYTitle(yTitle);
	pTInvdiffR[k][i]->Sumw2();
	pTInvdiffR[k][i]->SetMarkerStyle(20);
	pTInvdiffR[k][i]->SetMarkerColor(i+2);
	const Char_t *NameEta[3] = {"","EP","EN"};
	const Char_t *TitleEta[3] = {"all #eta", "|#eta| > 0","|#eta| < 0"};
	for (Int_t l = 0; l < 3; l++) {
	  Name = Form("Phi%sdiffR%s%s",NameEta[l],charge[i],GP[k]);
	  Title = Form("pT_%s - pT_%s for %s and %s",Old.Data(),New.Data(),GPTitle[k],TitleEta[l]);
	  PhidiffR[l][k][i] = new TH2F(Name,Title,72,-TMath::Pi(),TMath::Pi(),600,-0.15,0.15);
	  PhidiffR[l][k][i]->SetXTitle(Form("#phi_%s",New.Data()));
	  YTitle = Form("(pT_%s - pT_%s)/(pT_%s), versus #phi_%s",Old.Data(),New.Data(),New.Data(),New.Data());
	  yTitle = YTitle; yTitle += title; 
	  PhidiffR[l][k][i]->SetYTitle(yTitle);
	  PhidiffR[l][k][i]->Sumw2();
	  PhidiffR[l][k][i]->SetMarkerStyle(20);
	  PhidiffR[l][k][i]->SetMarkerColor(i+2);
	  
	  Name = Form("Phi5%sdiffR%s%s",NameEta[l],charge[i],GP[k]);
	  Phi5diffR[l][k][i] = new TH2F(Name,Title,72,-TMath::Pi(),TMath::Pi(),600,-0.15,0.15);
	  Phi5diffR[l][k][i]->SetXTitle(Form("#phi_%s",New.Data()));
	  YTitle = Form("(pT_%s - pT_%s)/(pT_%s), versus #phi_{%s} for pT > 0.5 GeV/c",Old.Data(),New.Data(),New.Data(),New.Data());
	  yTitle = YTitle; yTitle += title;
	  Phi5diffR[l][k][i]->SetYTitle(yTitle);
	  Phi5diffR[l][k][i]->Sumw2();
	  Phi5diffR[l][k][i]->SetMarkerStyle(20);
	  Phi5diffR[l][k][i]->SetMarkerColor(i+2);
	}
      }
    }
    // sector 20 and sector 04 pT distributins
    TH1D *secpT[2][6];
    const Char_t *secNames[4] = {"20","04","17","07"};
    const Char_t *pTNames[6] = {Old.Data(), New.Data(),
				Form("%sNot%s",Old.Data(),New.Data()), Form("%sNot%s", New.Data(),Old.Data()),
				Form("%sAnd%s",Old.Data(),New.Data()), Form("%sAnd%s", New.Data(),Old.Data())};
    const Char_t *pTitles[6] = {Old.Data(), New.Data(),
				Form("Reconstructed by %s and Not by %s",Old.Data(),New.Data()), Form("reconstructed by %s and Not by %s", New.Data(),Old.Data()),
				Form("Reconstructed by both %s And %s",Old.Data(),New.Data()), Form("reconstructed by both %s And %s", New.Data(),Old.Data())};
    for (Int_t i = 0; i < 4; i++) { // s = 0 => Sector 20; s = 1 => Sector 04, s = 2 => Sector 17; s = 3 => Sector 07
      for (Int_t t = 0; t < 6; t++) {
	Name = pTNames[t]; Name += secNames[i];
	Title = "Sector "; Title +=  secNames[i]; Title += " "; Title += pTitles[i];
	secpT[i][t] = new TH1D(Name,Title,100,0,10);
      }
    }
    TH2F *etaphi[2];
    etaphi[0] = new TH2F(Form("EtaPhi%s",Old.Data()),Form("#phi versus #eta for %s",Old.Data()),
			 200,-2,2,90,-180,180);
    etaphi[1] = new TH2F(Form("EtaPhi%s",New.Data()),Form("#phi versus #eta for %s",New.Data()),
			 200,-2,2,90,-180,180);
    // Loop
    Long64_t nentries = fChain->GetEntriesFast();
    if (Nentries > 0 && nentries > Nentries) nentries = Nentries;
    Int_t nbytes = 0, nb = 0;
    Float_t RefMultOld = -1;
    for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = fChain->LoadTree(jentry);
      if (ientry < 0) break;
      if (jentry%10000 == 0) cout << "Read entry " << jentry << endl;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      if (data.newFitPtsGl <= 0 && data.oldFitPtsGl <= 0) continue;
      if ((data.newFitPtsGl > 0 && TMath::Abs(data.newEtaGl) > 0.5) ||
	  (data.oldFitPtsGl > 0 && TMath::Abs(data.oldEtaGl) > 0.5)) continue;
      fitPtsHist->Fill(data.newFitPtsGl,data.oldFitPtsGl);
      Double_t pTdiffGl  = -9999;
      Double_t pTdiffGlR = -9999;
      Double_t pTInvdiffGlR = -9999;
      Double_t pTdiffPr  = -9999;
      Double_t pTdiffPrR = -9999;
      Double_t pTInvdiffPrR = -9999;
      Int_t oldSec = -1;
      if (data.oldFitPtsGl >= effNFP) {
	etaphi[0]->Fill(data.oldEtaGl,TMath::RadToDeg()*data.oldPhiGl);
	if (data.oldEtaGl < 0) {
	  if ( -45 < TMath::RadToDeg()*data.oldPhiGl && TMath::RadToDeg()*data.oldPhiGl <  -15) oldSec = 0;
	  if (-135 < TMath::RadToDeg()*data.oldPhiGl && TMath::RadToDeg()*data.oldPhiGl < -105) oldSec = 2;
	} else {
	  if ( -45 < TMath::RadToDeg()*data.oldPhiGl && TMath::RadToDeg()*data.oldPhiGl <  -15) oldSec = 1;
	  if (-135 < TMath::RadToDeg()*data.oldPhiGl && TMath::RadToDeg()*data.oldPhiGl < -105) oldSec = 3;
	}
      }
      Int_t newSec = -1;
      if (data.newFitPtsGl >= effNFP) {
	etaphi[1]->Fill(data.newEtaGl,TMath::RadToDeg()*data.newPhiGl);
	if (data.newEtaGl < 0) {
	  if ( -45 < TMath::RadToDeg()*data.newPhiGl && TMath::RadToDeg()*data.newPhiGl <  -15) newSec = 0;
	  if (-135 < TMath::RadToDeg()*data.newPhiGl && TMath::RadToDeg()*data.newPhiGl < -105) newSec = 2;
	} else {
	  if ( -45 < TMath::RadToDeg()*data.newPhiGl && TMath::RadToDeg()*data.newPhiGl <  -15) newSec = 1;
	  if (-135 < TMath::RadToDeg()*data.newPhiGl && TMath::RadToDeg()*data.newPhiGl < -105) newSec = 3;
	}
      }
      if (data.newFitPtsGl >= effNFP) {
	pTEf[0][0]->Fill(data.newPtGl,refMult); 
	PhiEf[0][0]->Fill(TMath::RadToDeg()*data.newPhiGl,refMult);
	if (newSec >= 0) {
	  secpT[newSec][1]->Fill(data.newPtGl); // StiCA
	  if (oldSec < 0) 
	    secpT[newSec][3]->Fill(data.newPtGl); // StiCA but not Sti
	  else 
	    secpT[newSec][5]->Fill(data.newPtGl); // StiCA and Sti
	}
      }
      if (data.oldFitPtsGl >= effNFP) {
	pTEf[0][1]->Fill(data.oldPtGl,refMult); 
	PhiEf[0][1]->Fill(TMath::RadToDeg()*data.oldPhiGl,refMult);
	if (oldSec >= 0) {
	  secpT[oldSec][0]->Fill(data.oldPtGl); // Sti
	  if (newSec < 0) 
	    secpT[oldSec][2]->Fill(data.oldPtGl); // Sti but not StiCA
	  else 
	    secpT[oldSec][4]->Fill(data.oldPtGl); // Sti and  StiCA
	}
      }
      Int_t Prim = (Int_t) data.Prim;
      if (Prim%10 && data.newFitPtsPr >= effNFP) {pTEf[1][0]->Fill(data.newPtPr,refMult); PhiEf[1][0]->Fill(TMath::RadToDeg()*data.newPhiPr,refMult);}
      if (Prim/10 && data.oldFitPtsPr >= effNFP) {pTEf[1][1]->Fill(data.oldPtPr,refMult); PhiEf[1][1]->Fill(TMath::RadToDeg()*data.oldPhiPr,refMult);}
      // Matched 
#if 0
      if (data.newFitPtsGl - data.maxPing >= 3) continue;
#else
      if (data.newFitPtsGl > 2*data.maxPing ||
	  data.oldFitPtsGl > 2*data.maxPing ) continue;
#endif
      if (data.newFitPtsGl < minNFP || data.oldFitPtsGl < minNFP) continue;
      //      if (data.firstHitsDist < 0 || data.firstHitsDist> 1) continue;
      fitPtsHistPing->Fill(data.newFitPtsGl,data.oldFitPtsGl);
      pTdiffGl  = data.oldPtGl - data.newPtGl;
      pTdiffGlR = data.oldPtGl/data.newPtGl - 1;
      pTInvdiffGlR = data.newPtGl/data.oldPtGl - 1;
      if (Prim == 11) {
	if (RefMultOld != refMult) {
	  RefMultOld = refMult;
	  if (! (data.newPrimX == 0 && data.newPrimY == 0 && data.newPrimZ == 0) &&
	      ! (data.oldPrimX == 0 && data.oldPrimY == 0 && data.oldPrimZ == 0)) {
	    DifPvX->Fill(data.newPrimX - data.oldPrimX);
	    DifPvY->Fill(data.newPrimY - data.oldPrimY);
	    DifPvZ->Fill(data.newPrimZ - data.oldPrimZ);
	  }
	}
	Charge->Fill(data.oldCharge,data.newCharge);
	if (data.newFitPtsGl >= 15 && data.oldFitPtsGl >= 15) Charge15->Fill(data.oldCharge,data.newCharge);
	Charge15->Fill(data.oldCharge,data.newCharge);
	pTdiffPr = data.oldPtPr - data.newPtPr;
	pTdiffPrR = data.oldPtPr/data.newPtPr - 1;
	pTInvdiffPrR = data.newPtPr/data.oldPtPr - 1;
	
	
      }
      Int_t charge = 0;
      if ((data.newCharge && data.newCharge < 0) ||
	  (data.oldCharge && data.oldCharge < 0)) charge = 1;
      pTDifNFP[0][charge]->Fill(data.newFitPtsGl,pTdiffGlR); 
      if (data.newPtGl > 0.5)  pTDifNFP5[0][charge]->Fill(data.newFitPtsGl,pTdiffGlR); 
      if (Prim == 11) {
	pTDifNFP[1][charge]->Fill(data.newFitPtsPr,pTdiffPrR); 
	if (data.newPtPr > 0.5)  pTDifNFP5[1][charge]->Fill(data.newFitPtsPr,pTdiffPrR); 
      }
      // Efficiency for more than effNFP fit points
      if (data.newFitPtsGl < effNFP || data.oldFitPtsGl < effNFP) continue;
      pTdiff[0][charge]->Fill(data.newPtGl,pTdiffGl);
      pTdiffR[0][charge]->Fill(data.newPtGl,pTdiffGlR);
      pTInvdiffR[0][charge]->Fill(data.newPtGl,pTInvdiffGlR);
      PhidiffR[0][0][charge]->Fill(data.newPhiGl,pTdiffGlR);
      if (data.newEtaGl > 0) PhidiffR[1][0][charge]->Fill(data.newPhiGl,pTdiffGlR);
      else                   PhidiffR[2][0][charge]->Fill(data.newPhiGl,pTdiffGlR);
      if (data.newPtGl > 0.5) {
	Phi5diffR[0][0][charge]->Fill(data.newPhiGl,pTdiffGlR);
	if (data.newEtaGl > 0) Phi5diffR[1][0][charge]->Fill(data.newPhiGl,pTdiffGlR);
	else                   Phi5diffR[2][0][charge]->Fill(data.newPhiGl,pTdiffGlR);
      }
      pTEf[0][2]->Fill(data.newPtGl,refMult); PhiEf[0][2]->Fill(TMath::RadToDeg()*data.newPhiGl,refMult);
      pTEf[0][3]->Fill(data.oldPtGl,refMult); PhiEf[0][3]->Fill(TMath::RadToDeg()*data.oldPhiGl,refMult);
      if (Prim == 11) {
	pTdiff[1][charge]->Fill(data.newPtPr,pTdiffPr);
	pTdiffR[1][charge]->Fill(data.newPtPr,pTdiffPrR);
	pTInvdiffR[1][charge]->Fill(data.newPtPr,pTInvdiffPrR);
	PhidiffR[0][1][charge]->Fill(data.newPhiPr,pTdiffPrR);
	if (data.newEtaPr > 0) PhidiffR[1][1][charge]->Fill(data.newPhiPr,pTdiffPrR);
	else                   PhidiffR[2][1][charge]->Fill(data.newPhiPr,pTdiffPrR);
	if (data.newPtPr > 0.5) {
	  Phi5diffR[0][1][charge]->Fill(data.newPhiPr,pTdiffPrR);
	  if (data.newEtaPr > 0) Phi5diffR[1][1][charge]->Fill(data.newPhiPr,pTdiffPrR);
	  else                   Phi5diffR[2][1][charge]->Fill(data.newPhiPr,pTdiffPrR);
	}
	pTEf[1][2]->Fill(data.newPtPr,refMult); PhiEf[1][2]->Fill(TMath::RadToDeg()*data.newPhiPr,refMult);
	pTEf[1][3]->Fill(data.oldPtPr,refMult); PhiEf[1][3]->Fill(TMath::RadToDeg()*data.oldPhiPr,refMult);
      }
    }
    if (fOut) fOut->Write();
    delete fOut;
  }
  Draw(Out);
}
  

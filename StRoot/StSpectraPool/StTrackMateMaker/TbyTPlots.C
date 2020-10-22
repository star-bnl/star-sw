/*
  root.exe TbyTPlots.C+
  root.exe Plots.root
  .L TbyTPlots.C+
  Init();
  Draw();
 */
//#define __SAVE_ROOT_PICTURES_
#
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TProfile2D.h"
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
#include "TDirectory.h"
#include "TPolyMarker.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TEfficiency.h"
#include "Ask.h"
//#include "StSpectraPool/StTrackMateMaker/TrackMatch.h#ifndef __TrackMatch__
#ifndef __TrackMatch__
#define __TrackMatch__
#include "TObject.h"
class TrackParameters : public TObject {
 public:
  TrackParameters() {}
  virtual ~TrackParameters() {}
  void set() {memset(&begin, 0, &end-&begin);}
  Bool_t IsEmpty()    {return MatchStatus == 0;}
  Bool_t IsMatch()    {return MatchStatus & 1;}
  Bool_t IsClone()    {return MatchStatus & 2;}
  Bool_t IsSplitted() {return MatchStatus & 4;}
  Bool_t IsLost()     {return MatchStatus & 8;}
  Int_t  charge()     {return (Charge > 0) ? 0 : 1;}
  Char_t begin;
  Int_t   Id;
  Float_t PtGl;
  Float_t EtaGl;
  Float_t PhiGl;
  Float_t PGl;
  Float_t FitPtsGl;
  Float_t PtPr;
  Float_t EtaPr;
  Float_t PhiPr;
  Float_t PPr;
  Float_t FitPtsPr;
  Float_t Dedx;
  Float_t Charge;
  Float_t Prim;
  Float_t Chi2Gl0;
  Float_t Chi2Gl1;
  Float_t Chi2Pr0;
  Float_t Chi2Pr1;
  Float_t FirstPointX;
  Float_t FirstPointY;
  Float_t FirstPointZ;
  Float_t LastPointX;
  Float_t LastPointY;
  Float_t LastPointZ;
  Float_t PrimX;
  Float_t PrimY;
  Float_t PrimZ;
/*   Float_t SecF;  // first and last hit sector */
/*   Float_t SecL; */
  Int_t   hitMap; // HFT hits pxl + 10 * ist + 100 *Ssd
  Int_t   MatchStatus; // 1 - ReCo (1 to 1 match), 2 - CLone, 4 - Splitted, 8 - Lost 
  Char_t end;
  ClassDef(TrackParameters,3)
};
class TrackMatch : public TObject {
 public:
 TrackMatch() {}
 virtual ~TrackMatch() {}
  TrackParameters newP;
  TrackParameters oldP;
  ClassDef(TrackMatch,3)
};
#endif
#include "TObject.h"
TFile *fOut = 0;
TCanvas *c1 = 0;
TChain *fChain = 0;
const Int_t NfitPtsHist = 7;
enum {kGP = 2, kGl = 0, kPr = 1,// global primary
      keFF = 3, 
      keff = 0,
      kclone = 1,
      klost = 2,
      kVAR = 3,
      kpT  = 0,
      kEta = 1,
      kPhi = 2,
      knew = 0,
      kold = 1,
      kCharge = 2,
      kpos = 0,
      kneg = 1
};
const Char_t *GP[kGP] = {"Gl","Pr"};
const Char_t *GPTitle[kGP] = {"Global","Primary"};
static TString EffClone[keFF] = {"Eff","Clone","Lost"};
static TString pTEtaPhi[kVAR] = {"pT", "Eta", "Phi"};
static TString pTEtaPhiT[kVAR] = {"p_{T}", "#eta", "#phi(^{o})"};
const Int_t effNFP = 15; // 25; // 15;
static TString Old("Old");
static TString New("New");
const Char_t *charge[kCharge] = {"pos","neg"};
const Char_t *chargeT[kCharge] = {"(+)","(-)"};
const Char_t *NewOldV[2] = {New.Data(), Old.Data()};
const Char_t *NewOld[2] = {"New", "Old"};
enum {npT    = 99};
//  Double_t pTMax =   10;
const Double_t ptBins[npT+1] = {
  0.01, 0.04, 0.06,
  0.07, 0.08, 0.11, 0.14, 0.16, 0.17, 0.19, 0.21, 0.22, 0.23,
  0.24, 0.26, 0.27, 0.28, 0.29, 0.30, 0.31, 0.32, 0.33, 0.34,
  0.35, 0.36, 0.37, 0.38, 0.39, 0.40, 0.41, 0.42, 0.43, 0.44,
  0.45, 0.46, 0.47, 0.48, 0.49, 0.50, 0.51, 0.53, 0.54, 0.55,
  0.56, 0.57, 0.58, 0.60, 0.61, 0.62, 0.63, 0.65, 0.66, 0.67,
  0.69, 0.70, 0.72, 0.73, 0.75, 0.76, 0.78, 0.80, 0.81, 0.83,
  0.85, 0.87, 0.89, 0.91, 0.93, 0.96, 0.98, 1.01, 1.03, 1.06,
  1.09, 1.12, 1.16, 1.19, 1.23, 1.27, 1.31, 1.35, 1.40, 1.45,
  1.51, 1.57, 1.64, 1.71, 1.80, 1.89, 2.00, 2.11, 2.24, 2.39,
  2.57, 2.78, 3.05, 3.38, 3.80, 4.28, 4.96//, 5.88, 7.25,10.00
  //  20.0, 30.0, 50.0, 100.
};
TProfile  *Eff[2][2][3][2][3] = {0}; 
TString gTitle;
TString cTitle;
Int_t _debug = 0;
//________________________________________________________________________________
//TrackParmeters::TrackParmeters(StGlobalTrack *gTrack) {}
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
  //  pngName += ".png"; 
  pngName += ".svg"; 
  c->SaveAs(pngName);
  nPng++;
  cout << "Draw #\t" << nPng << "\t" << pngName << endl;
#ifdef __SAVE_ROOT_PICTURES_
  //  pngName.ReplaceAll(".png",".root");
  pngName.ReplaceAll(".svg",".root");
  c->SaveAs(pngName);
#endif
}
//________________________________________________________________________________
void DrawFitPnts(Int_t ping=0) {
  //    gStyle->SetPalette(1,0);
  //________________________________________________________________________________
  // Fit pts correlation
  //________________________________________________________________________________
  TString Name("fitPtsHist");
  TH2D* fitPtsHist = (TH2D*) TDirectory::CurrentDirectory()->Get(Name);
  if (! fitPtsHist) return;
  cTitle += gTitle;
  cTitle += "Fit Points Corr";
  if (ping) cTitle += " Matched";
  c1 = new TCanvas(cTitle,cTitle,800,800); c1->SetLeftMargin(0.14);
  c1->SetLogz();
  fitPtsHist->SetStats(0);
  //  fitPtsHist->GetXaxis()->SetRange(0,73);
  //  fitPtsHist->GetYaxis()->SetRange(0,72);
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
//________________________________________________________________________________
void DrawRelMomDifNft(Int_t k=0, Int_t kase=0) {
  //________________________________________________________________________________
  // Momentum difference, vs fit pts
  //________________________________________________________________________________
  TH2D* pTDifNFP[2];
  if (kase == 0) for (Int_t c = 0; c < 2; c++) pTDifNFP[c] = (TH2D *) TDirectory::CurrentDirectory()->Get(Form("pTDifNFP%s%s",GP[k],charge[c]));
  else           for (Int_t c = 0; c < 2; c++) pTDifNFP[c] = (TH2D *) TDirectory::CurrentDirectory()->Get(Form("pTDifNFP5%s%s",GP[k],charge[c]));
  if (! pTDifNFP[0] || ! pTDifNFP[1]) return;
  if (pTDifNFP[0]->GetEntries() <100 || pTDifNFP[1]->GetEntries() <100) return;
  cTitle  = gTitle;
  cTitle += "P diff vs NoFitPnts for "; cTitle += GPTitle[k];
  if (kase) cTitle += " with pT > 0.5 GeV/c";
  c1 = new TCanvas(cTitle,cTitle,800,800); c1->SetLeftMargin(0.14);
  TString cTitle2(cTitle);
  cTitle2 += " Shift";
  TCanvas* c2 = new TCanvas(cTitle2,cTitle2,800,800); c2->SetLeftMargin(0.14);
  TString cTitle3(cTitle);
  cTitle3 += " Sigma";
  TCanvas* c3 = new TCanvas(cTitle3,cTitle3,800,800); c3->SetLeftMargin(0.14);
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
    TH1 *mu = (TH1 *) TDirectory::CurrentDirectory()->Get(name); 
    TH1 *sigma = (TH1 *) TDirectory::CurrentDirectory()->Get(name);
    if (mu->GetMaximum() > ymax) ymax = mu->GetMaximum();
    if (mu->GetMinimum() < ymin) ymin = mu->GetMinimum();
    if (sigma->GetMaximum() > smax) smax = sigma->GetMaximum();
  }
  for (Int_t c = 0; c < 2; c++) {
    c2->cd();
    pTDifNFP[c]->FitSlicesY();
    name = pTDifNFP[c]->GetName(); name += "_1";
    TH1 *mu = (TH1 *) TDirectory::CurrentDirectory()->Get(name); mu->SetMaximum(1.1*ymax); if (ymin < 0) mu->SetMinimum(1.1*ymin);
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
    TH1 *sigma = (TH1 *) TDirectory::CurrentDirectory()->Get(name); //sigma->SetMaximum(0.1);
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
void DrawEfficiency() {
  for (Int_t gp = 0; gp < 2; gp++) {// Global / Primaries
    for (Int_t ec = 0; ec < 3; ec++) {// Efficiency Clone Lost
      for (Int_t var = 0; var < 3; var++) {// pT, Eta, Phi
	TString cName(EffClone[ec]); cName += pTEtaPhi[var]; cName += GP[gp];
	TCanvas *c1 = new TCanvas(cName,cName);
	if (var == 0) c1->SetLogx(1);
	Double_t yMax = 0;
	Double_t yMin = 99;
	for (Int_t no = 0; no < 2; no++) {// New Old
	  for (Int_t c = 0; c < 2; c++) {// charge +/-
	    if (!  Eff[gp][c][ec][no][var] ) continue;
	    Eff[gp][c][ec][no][var]->SetStats(0); 
	    Double_t y = Eff[gp][c][ec][no][var]->GetMaximum();
	    if (y > yMax) yMax = y;
	    y = Eff[gp][c][ec][no][var]->GetMinimum();
	    if (y < yMin) yMin = y;
	  }
	}
	TString same;
	TLegend *l = new TLegend(.8,.8,1.0,1.0);
	for (Int_t no = 0; no < 2; no++) {// New Old
	  for (Int_t c = 0; c < 2; c++) {// charge +/-
	    if (!  Eff[gp][c][ec][no][var] ) continue;
	    Eff[gp][c][ec][no][var]->SetMaximum(1.02*yMax);
	    Eff[gp][c][ec][no][var]->SetMinimum(0.92*yMin);
	    Eff[gp][c][ec][no][var]->Draw(same);
	    same = "same";
	    l->AddEntry(Eff[gp][c][ec][no][var], Form("%s %s",  NewOldV[no],  chargeT[c]));
	  }
	}
	l->Draw();
	c1->Update();
	DrawPng(c1);
	//	if (Ask()) return;
      }
    }
  }
}
//________________________________________________________________________________
void DrawpTDiff(Int_t k=0, Double_t pmax = 5.0) {
  //________________________________________________________________________________
  // Pt difference, vs pt
  //________________________________________________________________________________
  cTitle  = gTitle;
  TString hTitle("");
  hTitle += Form("<p_{T}^{%s} - p_{T}^{%s}> (GeV/c) versus p_{T}",Old.Data(),New.Data()); hTitle += " for "; hTitle += GPTitle[k];
  cTitle += "<p_{T}^{Old} - p_{T}^{New}> (GeV/c) versus p_{T}"; cTitle += " for "; cTitle += GPTitle[k];
  TCanvas* ptdiffgrcnv = new TCanvas(cTitle,cTitle,800,800); c1 = ptdiffgrcnv; c1->SetLeftMargin(0.14);
  TH1F* dummy2 = c1->DrawFrame(0,-.10,pmax, 0.10);
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
    TH2D *pTDiff = (TH2D *) TDirectory::CurrentDirectory()->Get(Form("pTDiff%s%s",charge[i],GP[k]));
    if (! pTDiff) continue;
    TProfile *profx = pTDiff->ProfileX();
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
  TCanvas* ptdiffgrcnv = new TCanvas(cTitle,cTitle,800,800); c1 = ptdiffgrcnv; c1->SetLeftMargin(0.14);
  TH1F* dummy2 = c1->DrawFrame(-180,-.05,180,.05);
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
    TH2D *pTDiff = (TH2D *) TDirectory::CurrentDirectory()->Get(Form("%sDiffR%s%s",opt,charge[i],GP[k]));
    if (! pTDiff) continue;
    pTDiff->FitSlicesY(0,0,0,100,"qnr");
    TH1 *profx = (TH1 *) TDirectory::CurrentDirectory()->Get(Form("%sDiffR%s%s_1",opt,charge[i],GP[k]));
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
void DrawRpTDiff(Int_t k = 0, const Char_t *opt="pTDiffR", Double_t pmax = 5.0) {
  //________________________________________________________________________________
  // relative Pt Difference, vs pt
  //________________________________________________________________________________
  
  cTitle  = gTitle;
  TString yTitle, sTitle;
  if (TString(opt) == "pTInvDiffR") {
    cTitle += "Inv pT diff (perc.) vs pT"; 
    yTitle = Form("(1/p_{T}^{%s} - 1/p_{T}^{%s})/1/p_{T}^{%s}", Old.Data(), New.Data(), New.Data());
    sTitle = Form("Sigma of Relative difference 1/pT (%s - %s)/%s vs pT", Old.Data(), New.Data(), New.Data());
  } else {
    cTitle += "pT diff (perc.) vs pT"; 
    yTitle = Form("(p_{T}^{%s} - p_{T}^{%s})/p_{T}^{%s}",  Old.Data(), New.Data(), New.Data());
    sTitle = Form("Sigma of Relative difference pT (%s - %s)/%s vs pT", Old.Data(), New.Data(), New.Data());
  }
  cTitle += " for "; cTitle += GPTitle[k];
  c1 = new TCanvas(cTitle,cTitle,800,800); c1->SetLeftMargin(0.14);
  c1->SetTicks(1,1);
  c1->SetLeftMargin(0.15);
  TString cTitle2(cTitle);
  cTitle2 += "Sigma";
  TCanvas *c2 = new TCanvas(cTitle2,cTitle2,800,800); c2->SetLeftMargin(0.14);
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
    TH2D *pTDiffR = (TH2D *) TDirectory::CurrentDirectory()->Get(Form("%s%s%s",opt,charge[i],GP[k]));
    if (! pTDiffR) {cout << "Histogram " << Form("%s%s%s",opt,charge[i],GP[k]) << " is not found" << endl; continue;}
    c1->cd(1);
    pTDiffR->FitSlicesY();
    TH1 *mu = (TH1 *) TDirectory::CurrentDirectory()->Get(Form("%s%s%s_1",opt,charge[i],GP[k])); 
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
    TH1 *sigma = (TH1 *) TDirectory::CurrentDirectory()->Get(Form("%s%s%s_2",opt,charge[i],GP[k])); 
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
void SetNewOld() {
  TString pwd(gSystem->BaseName( gSystem->WorkingDirectory()));
  TObjArray *obj = pwd.Tokenize("_");
  Int_t nParsed = obj->GetEntries();
  if (nParsed >= 2) {
    Old = ((TObjString *) obj->At(nParsed-2))->GetName();
    New = ((TObjString *) obj->At(nParsed-1))->GetName();
  }
  delete obj;
}
//________________________________________________________________________________
void Init(const Char_t *file="Plots.root") {
  SetNewOld();
  if (! TDirectory::CurrentDirectory()) {
    if (file) TFile::Open(file);
    if (! TDirectory::CurrentDirectory()) return;
  }
  for (Int_t gp = 0; gp < 2; gp++) {// Global / Primaries
    for (Int_t c = 0; c < 2; c++) {// charge +/-
      for (Int_t ec = 0; ec < 3; ec++) {// Efficiency Clone Lost
	for (Int_t no = 0; no < 2; no++) {// New Old
	  for (Int_t var = 0; var < 3; var++) {// pT, Eta, Phi
	    TString Name(EffClone[ec]); Name += pTEtaPhi[var]; Name += GP[gp]; Name += charge[c]; Name += NewOld[no];
	    Eff[gp][c][ec][no][var] = (TProfile *) TDirectory::CurrentDirectory()->Get(Name);
	  }
	}
      }
    }
  }
}
//________________________________________________________________________________
void DrawPrimVx() {
  const Char_t *XYZ[3] = {"X","Y","Z"};
  for (Int_t i = 0; i < 3; i++) {
    TH1* h = (TH1 *) TDirectory::CurrentDirectory()->Get(Form("DifPv%s",XYZ[i]));
    if (! h) continue;
    TCanvas *c = new TCanvas(Form("PrimaryVertex%sDiff",XYZ[i]),Form("PrimaryVertex%sDiff",XYZ[i]));
    h->Draw();
    DrawPng(c);
  }
}
//________________________________________________________________________________
void DrawCharge() {
  const Char_t *hNames[1] = {"Charge"};
  //  const Char_t *fNames[2] = {Form("%sVs%sChargeAllPr",New.Data(),Old.Data()),Form("%sVs%sChargePrNFPGE15",New.Data(),Old.Data())};
  const Char_t *fNames[2] = {"NewVsOldChargeAllPr","NewVsOldChargePrNFPGE15"};
  for (Int_t i = 0; i < 1; i++) {
    TH2 *h = (TH2 *) TDirectory::CurrentDirectory()->Get(hNames[i]);
    if (! h ) continue;
    TCanvas *c = new TCanvas(fNames[i],fNames[i]);
    h->Draw("text");
    DrawPng(c);
  }
}
//________________________________________________________________________________
void Draw(const Char_t *file="Plots.root") {
  Init(file);
  DrawEfficiency();
#if 1
  DrawFitPnts();
  DrawRelMomDifNft(0,0);  DrawRelMomDifNft(1,0);
  DrawRpTDiff(0);DrawRpTDiff(1);
  DrawRpTDiff(0,"pTInvDiffR");  DrawRpTDiff(1,"pTInvDiffR");
  DrawPrimVx();
  DrawCharge();
#endif
}
//________________________________________________________________________________
void TbyTPlots(const Char_t *file = 0, Int_t Nentries=0) {
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
	tFile = pFile;
      }
      delete f;
    }
  }
  TrackMatch *T = new TrackMatch;
  fChain->SetBranchAddress("TrackMatch", &T);
#if 0
  tFile.ReplaceAll(".root","");
  tFile.ReplaceAll("trackMate_physics_","");
  tFile.ReplaceAll("trackMate_","");
  tFile.ReplaceAll("5043073_raw_1010010_","");
  tFile.ReplaceAll("5046043_raw_1010010_","");
  Out = tFile;
#endif
  TString Out(file);
  if (Out == "") {
    Out += "Plots.root";
  }
  TFile *fOut = new TFile(Out.Data(),"recreate");
  cout << "Opened " << Out << endl;
  // book
  gTitle = file;
  if (gTitle != "") {
    TDirectory::CurrentDirectory() = new TFile(gTitle);
    //  gTitle.ReplaceAll(".Plots.root","");
    gTitle = "";
  }
  SetNewOld();
  TString Title;
  TString Name;
  // Efficiencies
  //________________________________________________________________________________
  //            GP  C  Eff/Clone/Lost new/old pT/Eta/Phi
  const Char_t *Titles[6] = {Form("Efficiency no. of Fit Pts>%i %s versus ",effNFP,New.Data(),New.Data()),
			     Form("Efficiency no. of Fit Pts>%i %s versus ",effNFP,Old.Data(),Old.Data()),
			     Form("Clone rate no. of Fit Pts>%i %s versus ",effNFP,New.Data(),New.Data()),
			     Form("Clone rate no. of Fit Pts>%i %s versus ",effNFP,Old.Data(),Old.Data()),
			     Form("Lost rate no. of Fit Pts>%i %s versus " ,effNFP,New.Data(),New.Data()),
			     Form("Lost rate no. of Fit Pts>%i %s versus " ,effNFP,Old.Data(),Old.Data())};
  for (Int_t gp = 0; gp < kGP; gp++) {// Global / Primaries
    for (Int_t c = 0; c < kCharge; c++) {// charge +/-
      for (Int_t ec = 0; ec < keFF; ec++) {// Efficiency Clone Lost
	for (Int_t no = 0; no < 2; no++) {// New Old
	  for (Int_t var = 0; var < 3; var++) {// pT, Eta, Phi
	    Name = EffClone[ec]; Name += pTEtaPhi[var]; Name += GP[gp]; Name += charge[c]; Name += NewOld[no];
	    Title = Titles[no+2*ec]; Title += pTEtaPhiT[var]; Title += " for "; Title +=  GPTitle[gp]; Title += chargeT[c];
	    if      (var == 0) Eff[gp][c][ec][no][var] = new TProfile(Name,Title, npT, ptBins);
	    else if (var == 1) Eff[gp][c][ec][no][var] = new TProfile(Name,Title, 60, -3.0, 3.0);
	    else               Eff[gp][c][ec][no][var] = new TProfile(Name,Title, 90, -180., 180.);
	    Eff[gp][c][ec][no][var]->SetXTitle(pTEtaPhiT[var]);
	    if (c  == 1) Eff[gp][c][ec][no][var]->SetMarkerColor(2);  // negative is red
	    if (no == 1) Eff[gp][c][ec][no][var]->SetMarkerStyle(25); // Old is empty boxes
	  }
	}
      }
    }
  }
  TH1D *DifPvX = new TH1D("DifPvX",Form("Difference in X for %s - %s positions",New.Data(),Old.Data()),100,-1.00,1.00);
  TH1D *DifPvY = new TH1D("DifPvY",Form("Difference in Y for %s - %s positions",New.Data(),Old.Data()),100,-1.00,1.00);
  TH1D *DifPvZ = new TH1D("DifPvZ",Form("Difference in Z for %s - %s positions",New.Data(),Old.Data()),100,-1.00,1.00);
  TH2D *Charge = new TH2D("Charge",Form("Charge flip between %s and %s for all primaries",New.Data(),Old.Data()),3,-1.5,1.5,3,-1.5,1.5);
  Charge->SetXTitle(Form("%s charge",Old.Data()));
  Charge->SetYTitle(Form("%s charge",New.Data()));
  //________________________________________________________________________________
  // Fit pts correlation
  //________________________________________________________________________________
  Name = "fitPtsHist", Title = Form("Fit Pts %s vs %s",Old.Data(), New.Data());
  TH2D* fitPtsHist = new TH2D(Name,Title,101,effNFP-0.5,100+effNFP+0.5,101,effNFP-0.5,100+effNFP+0.5);
  fitPtsHist->SetXTitle(Form("fit points, %s",New.Data()));
  fitPtsHist->SetYTitle(Form("fit points, %s",Old.Data()));
  //________________________________________________________________________________
  // Relative Momentum Difference, vs fit pts
  //________________________________________________________________________________
  TH2D* pTDifNFP[2][2];
  TH2D* pTDifNFP5[2][2];
  for (Int_t gp = 0; gp < kGP; gp++) {
    for (Int_t c = 0; c < kCharge; c++) {
      Name = Form("pTDifNFP%s%s",GP[gp],charge[c]);
      Title = Form("Relative Momentum difference vs no. of %s fit pts for %s tracks",New.Data(),GPTitle[gp]);
      pTDifNFP[gp][c] = new TH2D(Name,Title,76-effNFP,effNFP-0.5,75.5 ,200,-.2,.2);
      pTDifNFP[gp][c]->SetYTitle(Form("(pT_%s - pT_%s)/pT_%s",Old.Data(),New.Data(),New.Data()));
      pTDifNFP[gp][c]->SetXTitle(Form("No. %s fit points",New.Data()));
      pTDifNFP[gp][c]->SetMarkerStyle(20);
      Name = Form("pTDifNFP5%s%s",GP[gp],charge[c]);
      Title = Form("Relative Momentum difference vs no. of %s fit pts for %s tracks with pT > 0.5 GeV/c",New.Data(),GPTitle[gp]);
      pTDifNFP5[gp][c] = new TH2D(Name,Title,101,effNFP-0.5,100+effNFP+0.5, 200,-.2,.2);
      pTDifNFP5[gp][c]->SetYTitle(Form("(pT_%s - pT_%s)/pT_%s", Old.Data(), New.Data(), New.Data()));
      pTDifNFP5[gp][c]->SetXTitle(Form("No. %s fit points",New.Data()));
      pTDifNFP5[gp][c]->SetMarkerStyle(20);
    }
  }
  //________________________________________________________________________________
  //________________________________________________________________________________
  // Pt difference, vs pt
  //________________________________________________________________________________
  TH2D* pTDiff[kGP][kCharge];
  TH2D* pTDiffR[kGP][kCharge];
  TH2D* pTInvDiffR[kGP][kCharge];
  TH2D* PhiDiffR[3][kGP][kCharge];
  for (Int_t gp = 0; gp < kGP; gp++) {
    for (Int_t c = 0; c < kCharge; c++) {
      Name = Form("pTDiff%s%s",charge[c],GP[gp]);
      Title = Form("pT_%s - pT_%s for %s",Old.Data(),New.Data(),GPTitle[gp]);
      pTDiff[gp][c] = new TH2D(Name,Title,40,0,4,601,-.15025,0.15025);
      pTDiff[gp][c]->SetXTitle(Form("p_{T} %s (GeV/c)",New.Data()));
      TString YTitle = Form("p_{T} Difference (pT_%s - pT_%s),",Old.Data(),New.Data());
      TString title("");
      if (c == 0) 
	title += " +";
      else 
	title += " -";
      title += "charge";
      if (gp == 0) title += " for Globals";
      else        title += " for Primaries";
      TString yTitle = YTitle; yTitle += title;
      pTDiff[gp][c]->SetYTitle(yTitle);
      pTDiff[gp][c]->Sumw2();
      pTDiff[gp][c]->SetMarkerStyle(20);
      pTDiff[gp][c]->SetMarkerColor(c+2);
      Name = Form("pTDiffR%s%s",charge[c],GP[gp]);
      pTDiffR[gp][c] = new TH2D(Name,Title,40,0,4,601,-.15025,0.15025);
      pTDiffR[gp][c]->SetXTitle(Form("p_{T} %s (GeV/c)",New.Data()));
      YTitle = Form("(pT_%s - pT_%s)/pT_%s,",Old.Data(),New.Data(),New.Data());
      yTitle = YTitle; yTitle += title;
      pTDiffR[gp][c]->SetYTitle(yTitle);
      pTDiffR[gp][c]->Sumw2();
      pTDiffR[gp][c]->SetMarkerStyle(20);
      pTDiffR[gp][c]->SetMarkerColor(c+1);
      
      Name = Form("pTInvDiffR%s%s",charge[c],GP[gp]);
      pTInvDiffR[gp][c] = new TH2D(Name,Title,40,0.,4.,601,-.15025,0.15025);
      pTInvDiffR[gp][c]->SetXTitle(Form("pT_%s",New.Data()));
      YTitle = Form("(1/pT_%s - 1/pT_%s)/(1/pT_%s), versus pT_%s",Old.Data(),New.Data(),New.Data(),New.Data());
      yTitle = YTitle; yTitle += title;
      pTInvDiffR[gp][c]->SetYTitle(yTitle);
      pTInvDiffR[gp][c]->Sumw2();
      pTInvDiffR[gp][c]->SetMarkerStyle(20);
      pTInvDiffR[gp][c]->SetMarkerColor(c+1);
      const Char_t *NameEta[3] = {"","EP","EN"};
      const Char_t *TitleEta[3] = {"all #eta", "|#eta| > 0","|#eta| < 0"};
      for (Int_t l = 0; l < 3; l++) {
	Name = Form("Phi%sDiffR%s%s",NameEta[l],charge[c],GP[gp]);
	Title = Form("pT_%s - pT_%s for %s and %s",Old.Data(),New.Data(),GPTitle[gp],TitleEta[l]);
	PhiDiffR[l][gp][c] = new TH2D(Name,Title,180,-180,180,601,-.15025,0.15025);
	PhiDiffR[l][gp][c]->SetXTitle(Form("#phi_%s",New.Data()));
	YTitle = Form("(pT_%s - pT_%s)/(pT_%s), versus #phi_%s",Old.Data(),New.Data(),New.Data(),New.Data());
	yTitle = YTitle; yTitle += title; 
	PhiDiffR[l][gp][c]->SetYTitle(yTitle);
	PhiDiffR[l][gp][c]->Sumw2();
	PhiDiffR[l][gp][c]->SetMarkerStyle(20);
	PhiDiffR[l][gp][c]->SetMarkerColor(c+1);
      }
    }
  }
  TH2D *etaphi[2];
  etaphi[0] = new TH2D(Form("EtaPhi%s",Old.Data()),Form("#phi versus #eta for %s",Old.Data()),
		       200,-2,2,90,-180,180);
  etaphi[1] = new TH2D(Form("EtaPhi%s",New.Data()),Form("#phi versus #eta for %s",New.Data()),
		       200,-2,2,90,-180,180);
  // Loop
  Long64_t nentries = fChain->GetEntriesFast();
  if (Nentries > 0 && nentries > Nentries) nentries = Nentries;
  Int_t nbytes = 0, nb = 0;
  Float_t RefMultOld = -1;
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = fChain->LoadTree(jentry);
    if (ientry < 0) break;
    if (jentry%100000 == 0) cout << "Read entry " << jentry << endl;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    for (Int_t gp = 0; gp < 2; gp++) {// Global Primary
      Double_t Vars[2][kVAR] = {0}; // 2 -> New/Old; 3 -> pT/Eta/Phi
      Int_t c[kCharge] = {0};
      Int_t iOld = T->newP.MatchStatus; // 0 - lost, 1 - match, > 1 - clone, 
      Int_t iNew = T->oldP.MatchStatus;
      if (_debug && iOld != iNew) {
	static Int_t iBreak = 0;
	iBreak++;
      }
      if (gp == 0) {
	if (iOld && T->newP.FitPtsGl < effNFP) iOld = 0;
	if (iNew && T->oldP.FitPtsGl < effNFP) iNew = 0;
	if (iOld) {Vars[kold][kpT] = T->newP.PtGl; Vars[kold][kEta] = T->newP.EtaGl; Vars[kold][kPhi] = TMath::RadToDeg()*T->newP.PhiGl; c[kold] = T->newP.charge();}
	if (iNew) {Vars[knew][kpT] = T->oldP.PtGl; Vars[knew][kEta] = T->oldP.EtaGl; Vars[knew][kPhi] = TMath::RadToDeg()*T->oldP.PhiGl; c[knew] = T->oldP.charge();}
      } else {
	if (iOld && T->newP.FitPtsPr < effNFP) iOld = 0;
	if (iNew && T->oldP.FitPtsPr < effNFP) iNew = 0;
	if (iOld) {Vars[kold][kpT] = T->newP.PtPr; Vars[kold][kEta] = T->newP.EtaPr; Vars[kold][kPhi] = TMath::RadToDeg()*T->newP.PhiPr; c[kold] = T->newP.charge();}
	if (iNew) {Vars[knew][kpT] = T->oldP.PtPr; Vars[knew][kEta] = T->oldP.EtaPr; Vars[knew][kPhi] = TMath::RadToDeg()*T->oldP.PhiPr; c[knew] = T->oldP.charge();}
      }
      if (iOld == 0 && iNew == 0) continue;
      Double_t LostOld = iOld == 0 ? 1 : 0, MatchedOld = iOld >= 1 ? 1 : 0, CloneOld = iNew > 1 ? 1 : 0;
      Double_t LostNew = iNew == 0 ? 1 : 0, MatchedNew = iNew >= 1 ? 1 : 0, CloneNew = iOld > 1 ? 1 : 0;
      if (_debug) {
	cout << ientry << "\tgp = " << gp << "\tOld " << iOld << " Id = " << T->newP.Id << "\tL/M/C = " << LostOld << "/" << MatchedOld << "/" << CloneOld 
	     << "\tc/pT/eta/phi" << c[kold] << "/" << Vars[kold][kpT] << "/" << Vars[kold][kEta] << "/" << Vars[kold][kPhi] << endl; 
	cout << ientry << "\tgp = " << gp << "\tNew " << iNew << " Id = " << T->oldP.Id << "\tL/M/C = " << LostNew << "/" << MatchedNew << "/" << CloneNew 
	     << "\tc/pT/eta/phi" << c[knew] << "/" << Vars[knew][kpT] << "/" << Vars[knew][kpT] << "/" << Vars[knew][kPhi] << endl; 
      }
      for (Int_t var = 0; var < 3 ; var++) {
	if (iNew) Eff[gp][c[knew]][kclone][knew][var]->Fill(Vars[knew][var],CloneNew);
	if (iOld) Eff[gp][c[kold]][kclone][kold][var]->Fill(Vars[kold][var],CloneOld);
	if (CloneOld < 0.5 && CloneNew < 0.5) {
	  if (iNew) {
	    Eff[gp][c[knew]][keff][knew][var]->Fill(Vars[knew][var],MatchedOld);
	    Eff[gp][c[knew]][klost][knew][var]->Fill(Vars[knew][var],LostOld);
	  }
	  if (iOld) {
	    Eff[gp][c[kold]][keff][kold][var]->Fill(Vars[kold][var],MatchedNew);
	    Eff[gp][c[kold]][klost][kold][var]->Fill(Vars[kold][var],LostNew);
	  }
	}
      } // var loop
      // Matched only
      if (iOld != 1 || iNew != 1) continue;
      // |eta| < 1.0
      if (TMath::Abs(Vars[knew][kEta]) > 1 || TMath::Abs(Vars[knew][kEta]) > 1) continue;
      if (gp == 0) fitPtsHist->Fill(T->oldP.FitPtsGl,T->newP.FitPtsGl);
      Double_t pTdiff     = Vars[knew][kpT] - Vars[kold][kpT]; // pT_new - pT_old
      Double_t pTdiffR    = 1 - Vars[kold][kpT]/Vars[knew][kpT]; // (pT_new - pT_old)/pT_new = 1 - pT_old/pT_new;
      Double_t pTInvdiffR = 1 - Vars[knew][kpT]/Vars[kold][kpT]; // (1/pt_new - 1/pT_old)/(1/pT_new) = 1 - pT_new/pT_old;
      Double_t phiNew = Vars[knew][kPhi];
      Double_t phiOld = Vars[kold][kPhi];
      if (gp == 1) {
	DifPvX->Fill(T->oldP.PrimX - T->newP.PrimX);
	DifPvY->Fill(T->oldP.PrimY - T->newP.PrimY);
	DifPvZ->Fill(T->oldP.PrimZ - T->newP.PrimZ);
      }
      Int_t chsign = 0;
      if ((T->oldP.Charge && T->oldP.Charge < 0) ||
	  (T->newP.Charge && T->newP.Charge < 0)) chsign = 1;
      if (gp == 0) {
	Charge->Fill(T->newP.Charge,T->oldP.Charge);
	pTDifNFP[0][chsign]->Fill(T->oldP.FitPtsGl,pTdiffR); 
	if (Vars[knew][kEta] > 0.5)  pTDifNFP5[0][chsign]->Fill(T->oldP.FitPtsGl,pTdiffR); 
      }
      pTDiff[gp][chsign]->Fill(Vars[knew][kpT],pTdiff);
      pTDiffR[gp][chsign]->Fill(Vars[knew][kpT],pTdiffR);
      pTInvDiffR[gp][chsign]->Fill(Vars[knew][kpT],pTInvdiffR);
      PhiDiffR[0][gp][chsign]->Fill(Vars[knew][kPhi],pTdiffR);
      if (T->oldP.EtaGl > 0) PhiDiffR[1][gp][chsign]->Fill(Vars[knew][kPhi],pTdiffR);
      else                   PhiDiffR[2][gp][chsign]->Fill(Vars[knew][kPhi],pTdiffR);
    } // Global Primary loop 
  } // entires loop
  if (fOut) {
    fOut->Write();
    fOut->cd();
  }
  Draw(Out);
}

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
const Char_t *GP[2] = {"Gl","Pr"};
const Char_t *GPTitle[2] = {"Global","Primary"};
static TString NameEffpT[4] = {"pTnew","pTold","pTOnew","pTOold"};
static TString NameEffPhi[4] = {"Phinew","Phiold","PhiOnew","PhiOold"};
static TString NameEffEta[4] = {"Etanew","Etaold","EtaOnew","EtaOold"};
static TString EffClone[3] = {"Eff","Clone","Lost"};
static TString pTEtaPhi[3] = {"pT", "Eta", "Phi"};
static TString pTEtaPhiT[3] = {"p_{T}", "#eta", "#phi(^{o})"};
const Int_t minNFP = 10; // 25; // 10;
const Int_t effNFP = 10; // 15; // 25; // 15;
static TString Old("Old");
static TString New("New");
const Char_t *charge[2] = {"pos","neg"};
const Char_t *chargeT[2] = {"(+)","(-)"};
const Char_t *NewOld[2] = {"New", "Old"};
enum {npT    = 106};
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
  2.57, 2.78, 3.05, 3.38, 3.80, 4.28, 4.96, 5.88, 7.25,10.00, 
  20.0, 30.0, 50.0, 100.
};
TProfile  *Eff[2][2][3][2][3] = {0}; 
TFile *fIn = (TFile *) gDirectory;
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
  pngName += ".png"; 
  c->SaveAs(pngName);
  nPng++;
  cout << "Draw #\t" << nPng << "\t" << pngName << endl;
#ifdef __SAVE_ROOT_PICTURES_
  pngName.ReplaceAll(".png",".root");
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
  if (ping) Name += "Ping";
  TH2D* fitPtsHist = (TH2D*) fIn->Get(Name);
  if (! fitPtsHist) return;
  cTitle += gTitle;
  cTitle += "Fit Points Corr";
  if (ping) cTitle += " Matched";
  c1 = new TCanvas(cTitle,cTitle,800,800); c1->SetLeftMargin(0.14);
  c1->SetLogz();
  fitPtsHist->SetStats(0);
  fitPtsHist->GetXaxis()->SetRange(0,73);
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
void DrawHitPattern() {
  //    gStyle->SetPalette(1,0);
  //________________________________________________________________________________
  // Efficiency of Primary tracks with respect to Globals versus Hft hit pattern
  //________________________________________________________________________________
  TString Name("HftPattern");
  TH2D* HftPattern = (TH2D*) fIn->Get(Name);
  if (! HftPattern) return;
  //  cTitle += gTitle;
  cTitle = "HFT Hit Pattern";
  c1 = new TCanvas(cTitle,cTitle,800,800); c1->SetLeftMargin(0.14);
  //  HftPattern->GetYaxis()->SetRange(0,72);
  const Char_t *charge[2] = {"P","N"};
  const Char_t *ON[2]     = {"Old","New"};
  TString same("");
  for (Int_t oldnew = 1; oldnew >= 0; oldnew--) { //loop over (old,new)
    for (Int_t c = 1; c <= 2; c++) {// loop over charge (+,-)
      TH1D *G = HftPattern->ProjectionX(Form("G%s%s",ON[oldnew],charge[c-1]),  c+4*oldnew,   c+4*oldnew);
      G->SetStats(0);
      G->SetYTitle("Ratio Pr/Gl");
      G->SetStats(0);
      TH1D *P = HftPattern->ProjectionX(Form("P%s%s",ON[oldnew],charge[c-1]),2+c+4*oldnew, 2+c+4*oldnew);
      TEfficiency *ef = new TEfficiency(*P,*G);
      ef->SetMarkerColor(c);
      ef->SetMarkerStyle(20+oldnew);
      ef->Draw(same); same = "same";
      c1->Update();
    }
  }
  DrawPng(c1);
}
//________________________________________________________________________________
void DrawRelMomDifNft(Int_t k=0, Int_t kase=0) {
  //________________________________________________________________________________
  // Momentum difference, vs fit pts
  //________________________________________________________________________________
  TH2D* pTDifNFP[2];
  if (kase == 0) for (Int_t c = 0; c < 2; c++) pTDifNFP[c] = (TH2D *) fIn->Get(Form("pTDifNFP%s%s",GP[k],charge[c]));
  else           for (Int_t c = 0; c < 2; c++) pTDifNFP[c] = (TH2D *) fIn->Get(Form("pTDifNFP5%s%s",GP[k],charge[c]));
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
  TH2D *pTEf[4];
  Double_t ymax = 0;
  for (Int_t i = 0; i < 4; i++) {
    pTEf[i] = (TH2D *) fIn->Get(Form("%s%s",NameEffpT[i].Data(),GP[k]));
    if (! pTEf[i]) continue;
    Double_t y = pTEf[i]->GetMaximum();
    if (y > ymax) ymax = y;
  }
  //  cout << "ymax\t" << ymax << endl;
  if (ymax < 100) return;
  cTitle  = gTitle; cTitle += GP[k];
  TString hTitle("Momentum Dist"); hTitle += " for "; hTitle += GPTitle[k];
  cTitle += " "; cTitle += hTitle;
  c1 = new TCanvas(cTitle,cTitle,800,800); c1->SetLeftMargin(0.14);
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
void DrawEfficiency() {
  for (Int_t gp = 0; gp < 2; gp++) {// Global / Primaries
    for (Int_t ec = 0; ec < 3; ec++) {// Efficiency Clone Lost
      for (Int_t var = 0; var < 3; var++) {// pT, Eta, Phi
	TString cName(EffClone[ec]); cName += pTEtaPhi[var]; cName += GP[gp];
	TCanvas *c1 = new TCanvas(cName,cName);
	if (var == 0) c1->SetLogx(1);
	TString same;
	TLegend *l = new TLegend(.2,.15,.5,.4);
	for (Int_t no = 0; no < 2; no++) {// New Old
	  for (Int_t c = 0; c < 2; c++) {// charge +/-
	    if (!  Eff[gp][c][ec][no][var] ) continue;
	    Eff[gp][c][ec][no][var]->Draw(same);
	    same = "same";
	    l->AddEntry(Eff[gp][c][ec][no][var], Form("%s %s",  NewOld[no],  chargeT[c]));
	  }
	}
	l->Draw();
	c1->Update();
	//	DrawPng(c);
	if (Ask()) return;
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
    TH2D *pTdiff = (TH2D *) fIn->Get(Form("pTdiff%s%s",charge[i],GP[k]));
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
  TCanvas* ptdiffgrcnv = new TCanvas(cTitle,cTitle,800,800); c1 = ptdiffgrcnv; c1->SetLeftMargin(0.14);
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
    TH2D *pTdiff = (TH2D *) fIn->Get(Form("%sdiffR%s%s",opt,charge[i],GP[k]));
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
void DrawRpTDiff(Int_t k = 0, const Char_t *opt="pTdiffR", Double_t pmax = 5.0) {
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
    TH2D *pTdiffR = (TH2D *) fIn->Get(Form("%s%s%s",opt,charge[i],GP[k]));
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
  for (Int_t gp = 0; gp < 2; gp++) {// Global / Primaries
    for (Int_t c = 0; c < 2; c++) {// charge +/-
      for (Int_t ec = 0; ec < 3; ec++) {// Efficiency Clone Lost
	for (Int_t no = 0; no < 2; no++) {// New Old
	  for (Int_t var = 0; var < 3; var++) {// pT, Eta, Phi
	    TString Name(EffClone[ec]); Name += pTEtaPhi[var]; Name += GP[gp]; Name += charge[c]; Name += NewOld[no];
	    Eff[gp][c][ec][no][var] = (TProfile *) gDirectory->Get(Name);
	  }
	}
      }
    }
  }
#if 0
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
#endif
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
  DrawEfficiency();
#if 0
  DrawFitPnts();  DrawFitPnts(1);
  DrawRelMomDifNft(0,0);  DrawRelMomDifNft(1,0);
  DrawRpTDiff(0);DrawRpTDiff(1);
  DrawRpTDiff(0,"pTInvdiffR");  DrawRpTDiff(1,"pTInvdiffR");
  DrawPrimVx();
  DrawCharge();
  DrawHitPattern();
#endif
}
//________________________________________________________________________________
void TbyTPlots(const Char_t *file = 0, Int_t Nentries=0) {
  TString Out(file);
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
    TrackMatch *T = new TrackMatch;
    fChain->SetBranchAddress("TrackMatch", &T);
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
#if 0
    TH1D *DifPvX = new TH1D("DifPvX",Form("Difference in X for %s - %s positions",New.Data(),Old.Data()),100,-1.00,1.00);
    TH1D *DifPvY = new TH1D("DifPvY",Form("Difference in Y for %s - %s positions",New.Data(),Old.Data()),100,-1.00,1.00);
    TH1D *DifPvZ = new TH1D("DifPvZ",Form("Difference in Z for %s - %s positions",New.Data(),Old.Data()),100,-1.00,1.00);
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
    TH2D* fitPtsHist = new TH2D(Name,Title,73,-0.5,72.5,73,-0.5,72.5);
    fitPtsHist->SetXTitle(Form("fit points, %s",New.Data()));
    fitPtsHist->SetYTitle(Form("fit points, %s",Old.Data()));
    Name = "fitPtsHistPing", Title = Form("Fit Pts %s vs %s matched", Old.Data(), New.Data());
    TH2D* fitPtsHistPing = new TH2D(Name,Title,73,-0.5,72.5,73,-0.5,72.5);
    fitPtsHist->SetXTitle(Form("fit points, %s",New.Data()));
    fitPtsHist->SetYTitle(Form("fit points, %s",Old.Data()));
    // Primary to Global ratio for HFT hit pattern (pxl + 10*ist + 100*ssd)
    TH2D *HftPattern = new TH2D("HftPattern","charge (+1:-2) + 2* Global/Primary Old/New) versus patter charge + (pxl + 10*ist + 100*ssd)",100,-0.5,99.5,8,0.5,8.5);
    HftPattern->SetXTitle("hit pattern: (pxl + 10*ist + 100*ssd)");
    HftPattern->SetYTitle("charge + 2*(Old:Global=1,Primary=2,New:Global=3,Primary=4)");
    //________________________________________________________________________________
    // Relative Momentum difference, vs fit pts
    //________________________________________________________________________________
    TH2D* pTDifNFP[2]1[2];
    TH2D* pTDifNFP5[2][2];
    for (Int_t k = 0; k < 2; k++) {
      for (Int_t c = 0; c < 2; c++) {
	Name = Form("pTDifNFP%s%s",GP[k],charge[c]);
	Title = Form("Relative Momentum difference vs no. of %s fit pts for %s tracks",New.Data(),GPTitle[k]);
	pTDifNFP[k][c] = new TH2D(Name,Title, 7,10.,72.,200,-.2,.2);
	pTDifNFP[k][c]->SetYTitle(Form("Momentum Difference (pT_%s - pT_%s)/pT_%s",Old.Data(),New.Data(),New.Data()));
	pTDifNFP[k][c]->SetXTitle(Form("No. %s fit points",New.Data()));
	pTDifNFP[k][c]->SetMarkerStyle(20);
	Name = Form("pTDifNFP5%s%s",GP[k],charge[c]);
	Title = Form("Relative Momentum difference vs no. of %s fit pts for %s tracks with pT > 0.5 GeV/c",New.Data(),GPTitle[k]);
	pTDifNFP5[k][c] = new TH2D(Name,Title, 63, 9.5, 72.5, 200,-.2,.2);
	pTDifNFP5[k][c]->SetYTitle(Form("Momentum Difference (pT_%s - pT_%s)/pT_%s", Old.Data(), New.Data(), New.Data()));
	pTDifNFP5[k][c]->SetXTitle(Form("No. %s fit points",New.Data()));
	pTDifNFP5[k][c]->SetMarkerStyle(20);
      }
    }
    //________________________________________________________________________________
#endif
    // Efficiencies
    //________________________________________________________________________________
    //            GP  C  Eff/Clone/Lost new/old pT/Eta/Phi
    const Char_t *Titles[6] = {Form("Efficiency no. of Fit Pts>%i %s versus ",effNFP,New.Data(),New.Data()),
			       Form("Efficiency no. of Fit Pts>%i %s versus ",effNFP,Old.Data(),Old.Data()),
			       Form("Clone rate no. of Fit Pts>%i %s versus ",effNFP,New.Data(),New.Data()),
			       Form("Clone rate no. of Fit Pts>%i %s versus ",effNFP,Old.Data(),Old.Data()),
			       Form("Lost rate no. of Fit Pts>%i %s versus " ,effNFP,New.Data(),New.Data()),
			       Form("Lost rate no. of Fit Pts>%i %s versus " ,effNFP,Old.Data(),Old.Data())};
    for (Int_t gp = 0; gp < 2; gp++) {// Global / Primaries
      for (Int_t c = 0; c < 2; c++) {// charge +/-
	for (Int_t ec = 0; ec < 3; ec++) {// Efficiency Clone Lost
	  for (Int_t no = 0; no < 2; no++) {// New Old
	    for (Int_t var = 0; var < 3; var++) {// pT, Eta, Phi
	      Name = EffClone[ec]; Name += pTEtaPhi[var]; Name += GP[gp]; Name += charge[c]; Name += NewOld[no];
	      Title = Titles[no+2*ec]; Title += pTEtaPhiT[var]; Title += " for "; Title +=  GPTitle[gp]; Title += chargeT[c];
	      if      (var == 0) Eff[gp][c][ec][no][var] = new TProfile(Name,Title, npT, ptBins);
	      else if (var == 1) Eff[gp][c][ec][no][var] = new TProfile(Name,Title, 120, -3.0, 3.0);
	      else               Eff[gp][c][ec][no][var] = new TProfile(Name,Title, 180, -180., 180.);
	      Eff[gp][c][ec][no][var]->SetXTitle(pTEtaPhiT[var]);
	      if (c  == 1) Eff[gp][c][ec][no][var]->SetMarkerColor(2);  // negative is red
	      if (no == 1) Eff[gp][c][ec][no][var]->SetMarkerStyle(25); // Old is empty boxes
	    }
	  }
	}
      }
    }
#if 0
    //________________________________________________________________________________
    // Pt difference, vs pt
    //________________________________________________________________________________
    TH2D* pTdiff[2][2];
    TH2D* pTdiffR[2][2];
    TH2D* pTInvdiffR[2][2];
    TH2D* PhidiffR[3][2][2];
    TH2D* Phi5diffR[3][2][2];
    for (Int_t k = 0; k < 2; k++) {
      for (Int_t i = 0; i < 2; i++) {
	Name = Form("pTdiff%s%s",charge[i],GP[k]);
	Title = Form("pT_%s - pT_%s for %s",Old.Data(),New.Data(),GPTitle[k]);
	pTdiff[k][i] = new TH2D(Name,Title,40,0,4,601,-.15025,0.15025);
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
	pTdiffR[k][i] = new TH2D(Name,Title,40,0,4,601,-.15025,0.15025);
	pTdiffR[k][i]->SetXTitle(Form("p_{T} %s (GeV/c)",New.Data()));
	YTitle = Form("(pT_%s - pT_%s)/pT_%s,",Old.Data(),New.Data(),New.Data());
	yTitle = YTitle; yTitle += title;
	pTdiffR[k][i]->SetYTitle(yTitle);
	pTdiffR[k][i]->Sumw2();
	pTdiffR[k][i]->SetMarkerStyle(20);
	pTdiffR[k][i]->SetMarkerColor(i+2);
	
	Name = Form("pTInvdiffR%s%s",charge[i],GP[k]);
	pTInvdiffR[k][i] = new TH2D(Name,Title,40,0.,4.,601,-.15025,0.15025);
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
	  PhidiffR[l][k][i] = new TH2D(Name,Title,72,-TMath::Pi(),TMath::Pi(),601,-.15025,0.15025);
	  PhidiffR[l][k][i]->SetXTitle(Form("#phi_%s",New.Data()));
	  YTitle = Form("(pT_%s - pT_%s)/(pT_%s), versus #phi_%s",Old.Data(),New.Data(),New.Data(),New.Data());
	  yTitle = YTitle; yTitle += title; 
	  PhidiffR[l][k][i]->SetYTitle(yTitle);
	  PhidiffR[l][k][i]->Sumw2();
	  PhidiffR[l][k][i]->SetMarkerStyle(20);
	  PhidiffR[l][k][i]->SetMarkerColor(i+2);
	  
	  Name = Form("Phi5%sdiffR%s%s",NameEta[l],charge[i],GP[k]);
	  Phi5diffR[l][k][i] = new TH2D(Name,Title,72,-TMath::Pi(),TMath::Pi(),601,-.15025,0.15025);
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
    TH2D *etaphi[2];
    etaphi[0] = new TH2D(Form("EtaPhi%s",Old.Data()),Form("#phi versus #eta for %s",Old.Data()),
			 200,-2,2,90,-180,180);
    etaphi[1] = new TH2D(Form("EtaPhi%s",New.Data()),Form("#phi versus #eta for %s",New.Data()),
			 200,-2,2,90,-180,180);
#endif
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
	Double_t Vars[2][3] = {0}; // 2 -> New/Old; 3 -> pT/Eta/Phi
	Int_t c[2] = {0};
	Int_t iOld = T->oldP.MatchStatus; // 0 - lost, 1 - match, > 1 - clone, 
	Int_t iNew = T->newP.MatchStatus;
	if (_debug && iOld != iNew) {
	  static Int_t iBreak = 0;
	  iBreak++;
	}
	if (gp == 0) {
	  if (iOld && T->oldP.FitPtsGl < effNFP) iOld = 0;
	  if (iNew && T->newP.FitPtsGl < effNFP) iNew = 0;
	  if (iOld) {Vars[1][0] = T->oldP.PtGl; Vars[1][1] = T->oldP.EtaGl; Vars[1][2] = TMath::RadToDeg()*T->oldP.PhiGl; c[1] = T->oldP.charge();}
	  if (iNew) {Vars[0][0] = T->newP.PtGl; Vars[0][1] = T->newP.EtaGl; Vars[0][2] = TMath::RadToDeg()*T->newP.PhiGl; c[0] = T->newP.charge();}
	} else {
	  if (iOld && T->oldP.FitPtsPr < effNFP) iOld = 0;
	  if (iNew && T->newP.FitPtsPr < effNFP) iNew = 0;
	  if (iOld) {Vars[1][0] = T->oldP.PtPr; Vars[1][1] = T->oldP.EtaPr; Vars[1][2] = TMath::RadToDeg()*T->oldP.PhiPr; c[1] = T->oldP.charge();}
	  if (iNew) {Vars[0][0] = T->newP.PtPr; Vars[0][1] = T->newP.EtaPr; Vars[0][2] = TMath::RadToDeg()*T->newP.PhiPr; c[0] = T->newP.charge();}
	}
	if (iOld == 0 && iNew == 0) continue;
	Double_t LostOld = iOld == 0 ? 1 : 0, MatchedOld = iOld >= 1 ? 1 : 0, CloneOld = iNew > 1 ? 1 : 0;
	Double_t LostNew = iNew == 0 ? 1 : 0, MatchedNew = iNew >= 1 ? 1 : 0, CloneNew = iOld > 1 ? 1 : 0;
	if (_debug) {
	  cout << ientry << "\tgp = " << gp << "\tOld " << iOld << " Id = " << T->oldP.Id << "\tL/M/C = " << LostOld << "/" << MatchedOld << "/" << CloneOld 
	       << "\tc/pT/eta/phi" << c[1] << "/" << Vars[1][0] << "/" << Vars[1][1] << "/" << Vars[1][2] << endl; 
	  cout << ientry << "\tgp = " << gp << "\tNew " << iNew << " Id = " << T->newP.Id << "\tL/M/C = " << LostNew << "/" << MatchedNew << "/" << CloneNew 
	       << "\tc/pT/eta/phi" << c[0] << "/" << Vars[0][0] << "/" << Vars[0][0] << "/" << Vars[0][2] << endl; 
	}
	for (Int_t var = 0; var < 3 ; var++) {
	  if (iNew) Eff[gp][c[0]][1][0][var]->Fill(Vars[0][var],CloneNew);
	  if (iOld) Eff[gp][c[1]][1][1][var]->Fill(Vars[1][var],CloneOld);
	  if (CloneOld < 0.5 && CloneNew < 0.5) {
	    if (iNew) {
	      Eff[gp][c[0]][0][0][var]->Fill(Vars[0][var],MatchedOld);
	      Eff[gp][c[0]][2][0][var]->Fill(Vars[0][var],LostOld);
	    }
	    if (iOld) {
	      Eff[gp][c[1]][0][1][var]->Fill(Vars[1][var],MatchedNew);
	      Eff[gp][c[1]][2][1][var]->Fill(Vars[1][var],LostNew);
	    }
	  }
	}
      }
#if 0	
      if (T->newP.FitPtsGl <= 0 && T->oldP.FitPtsGl <= 0) continue;
      if ((T->newP.FitPtsGl > 0 && TMath::Abs(T->newP.EtaGl) > 0.5) ||
	  (T->oldP.FitPtsGl > 0 && TMath::Abs(T->oldP.EtaGl) > 0.5)) continue;
      fitPtsHist->Fill(T->newP.FitPtsGl,T->oldP.FitPtsGl);
      Double_t chargeOld = 0;
      if (T->oldP.Charge > 0) chargeOld = 1;
      if (T->oldP.Charge < 0) chargeOld = 2;
      if (chargeOld > 0.5) {
	if (T->oldP.FitPtsGl > 0) HftPattern->Fill(T->oldP.hitMap,chargeOld);
	if (T->oldP.FitPtsPr > 0) HftPattern->Fill(T->oldP.hitMap,chargeOld+2);
      }
      Double_t chargeNew = 0;
      if (T->newP.Charge > 0) chargeNew = 1;
      if (T->newP.Charge < 0) chargeNew = 2;
      if (chargeNew > 0.5) {
	if (T->newP.FitPtsGl > 0) HftPattern->Fill(T->newP.hitMap,chargeNew+4);
	if (T->newP.FitPtsPr > 0) HftPattern->Fill(T->newP.hitMap,chargeNew+6);
      }
      Double_t pTdiffGl  = -9999;
      Double_t pTdiffGlR = -9999;
      Double_t pTInvdiffGlR = -9999;
      Double_t pTdiffPr  = -9999;
      Double_t pTdiffPrR = -9999;
      Double_t pTInvdiffPrR = -9999;
      if (T->newP.FitPtsGl >= effNFP) { // New GL
	//	pTEf[0][0]->Fill(T->newP.PtGl,refMult); 
	Double_t phi = TMath::RadToDeg()*T->newP.PhiGl;
	//	PhiEf[0][0]->Fill(phi,refMult);
      }
      if (T->oldP.FitPtsGl >= effNFP) { // Old GL
	//	pTEf[0][1]->Fill(T->oldP.PtGl,refMult); 
	Double_t phi = TMath::RadToDeg()*T->oldP.PhiGl;
	//	PhiEf[0][1]->Fill(phi,refMult);
      }
      Double_t phiNew = TMath::RadToDeg()*T->newP.PhiPr;
      Double_t phiOld = TMath::RadToDeg()*T->oldP.PhiPr;
      //      if (T->newP.FitPtsPr >= effNFP) {pTEf[1][0]->Fill(T->newP.PtPr,refMult); PhiEf[1][0]->Fill(phiNew,refMult);}
      //      if (T->oldP.FitPtsPr >= effNFP) {pTEf[1][1]->Fill(T->oldP.PtPr,refMult); PhiEf[1][1]->Fill(phiOld,refMult);}
      // Matched 
      //      if (data.maxPing < 0) continue;
#if 0
      if (T->newP.FitPtsGl > 2*data.maxPing ||
	  T->oldP.FitPtsGl > 2*data.maxPing ) continue;
      if (T->newP.FitPtsGl < minNFP || T->oldP.FitPtsGl < minNFP) continue;
#endif
      //      if (data.firstHitsDist < 0 || data.firstHitsDist> 1) continue;
      fitPtsHistPing->Fill(T->newP.FitPtsGl,T->oldP.FitPtsGl);
      pTdiffGl  = T->oldP.PtGl - T->newP.PtGl;
      pTdiffGlR = T->oldP.PtGl/T->newP.PtGl - 1;
      pTInvdiffGlR = T->newP.PtGl/T->oldP.PtGl - 1;
      if (T->newP.FitPtsPr >= effNFP && T->oldP.FitPtsPr >= effNFP) {
	//	if (RefMultOld != refMult) {
	//	  RefMultOld = refMult;
	  if (! (T->newP.PrimX == 0 && T->newP.PrimY == 0 && T->newP.PrimZ == 0) &&
	      ! (T->oldP.PrimX == 0 && T->oldP.PrimY == 0 && T->oldP.PrimZ == 0)) {
	    DifPvX->Fill(T->newP.PrimX - T->oldP.PrimX);
	    DifPvY->Fill(T->newP.PrimY - T->oldP.PrimY);
	    DifPvZ->Fill(T->newP.PrimZ - T->oldP.PrimZ);
	  }
	  //      }
	Charge->Fill(T->oldP.Charge,T->newP.Charge);
	if (T->newP.FitPtsGl >= 15 && T->oldP.FitPtsGl >= 15) Charge15->Fill(T->oldP.Charge,T->newP.Charge);
	Charge15->Fill(T->oldP.Charge,T->newP.Charge);
	pTdiffPr = T->oldP.PtPr - T->newP.PtPr;
	pTdiffPrR = T->oldP.PtPr/T->newP.PtPr - 1;
	pTInvdiffPrR = T->newP.PtPr/T->oldP.PtPr - 1;
      }
      Int_t charge = 0;
      if ((T->newP.Charge && T->newP.Charge < 0) ||
	  (T->oldP.Charge && T->oldP.Charge < 0)) charge = 1;
      pTDifNFP[0][charge]->Fill(T->newP.FitPtsGl,pTdiffGlR); 
      if (T->newP.PtGl > 0.5)  pTDifNFP5[0][charge]->Fill(T->newP.FitPtsGl,pTdiffGlR); 
      // Both global
      if (T->newP.FitPtsGl < effNFP || T->oldP.FitPtsGl < effNFP) continue;
      pTdiff[0][charge]->Fill(T->newP.PtGl,pTdiffGl);
      pTdiffR[0][charge]->Fill(T->newP.PtGl,pTdiffGlR);
      pTInvdiffR[0][charge]->Fill(T->newP.PtGl,pTInvdiffGlR);
      PhidiffR[0][0][charge]->Fill(T->newP.PhiGl,pTdiffGlR);
      if (T->newP.EtaGl > 0) PhidiffR[1][0][charge]->Fill(T->newP.PhiGl,pTdiffGlR);
      else                   PhidiffR[2][0][charge]->Fill(T->newP.PhiGl,pTdiffGlR);
      if (T->newP.PtGl > 0.5) {
	Phi5diffR[0][0][charge]->Fill(T->newP.PhiGl,pTdiffGlR);
	if (T->newP.EtaGl > 0) Phi5diffR[1][0][charge]->Fill(T->newP.PhiGl,pTdiffGlR);
	else                   Phi5diffR[2][0][charge]->Fill(T->newP.PhiGl,pTdiffGlR);
      }
      phiNew = TMath::RadToDeg()*T->newP.PhiGl;
      phiOld = TMath::RadToDeg()*T->oldP.PhiGl;
      
      //      pTEf[0][2]->Fill(T->newP.PtGl,refMult); PhiEf[0][2]->Fill(phiNew,refMult);
      //      pTEf[0][3]->Fill(T->oldP.PtGl,refMult); PhiEf[0][3]->Fill(phiOld,refMult);
      // Both primaries
      if (T->newP.FitPtsPr < effNFP || T->oldP.FitPtsPr < effNFP) continue;
      pTDifNFP[1][charge]->Fill(T->newP.FitPtsPr,pTdiffPrR); 
      if (T->newP.PtPr > 0.5)  pTDifNFP5[1][charge]->Fill(T->newP.FitPtsPr,pTdiffPrR); 
      pTdiff[1][charge]->Fill(T->newP.PtPr,pTdiffPr);
      pTdiffR[1][charge]->Fill(T->newP.PtPr,pTdiffPrR);
      pTInvdiffR[1][charge]->Fill(T->newP.PtPr,pTInvdiffPrR);
      PhidiffR[0][1][charge]->Fill(T->newP.PhiPr,pTdiffPrR);
      if (T->newP.EtaPr > 0) PhidiffR[1][1][charge]->Fill(T->newP.PhiPr,pTdiffPrR);
      else                   PhidiffR[2][1][charge]->Fill(T->newP.PhiPr,pTdiffPrR);
      if (T->newP.PtPr > 0.5) {
	Phi5diffR[0][1][charge]->Fill(T->newP.PhiPr,pTdiffPrR);
	if (T->newP.EtaPr > 0) Phi5diffR[1][1][charge]->Fill(T->newP.PhiPr,pTdiffPrR);
	else                   Phi5diffR[2][1][charge]->Fill(T->newP.PhiPr,pTdiffPrR);
      }
      phiNew = TMath::RadToDeg()*T->newP.PhiPr;
      phiOld = TMath::RadToDeg()*T->oldP.PhiPr;
      //      pTEf[1][2]->Fill(T->newP.PtPr,refMult); PhiEf[1][2]->Fill(phiNew,refMult);
      //      pTEf[1][3]->Fill(T->oldP.PtPr,refMult); PhiEf[1][3]->Fill(phiOld,refMult);
#endif
    }
  }
  if (fOut) {
    fOut->Write();
    fOut->cd();
  }
  Draw(Out);
}

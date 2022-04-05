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
#include "StBichsel/Bichsel.h"
#include "BetheBloch.h"
#include "TDirIter.h"
#include "TTreeIter.h"
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
class Bichsel;
class BetheBloch;
class TDirIter;
class TTreeIter;
#endif
TChain *chain = 0;
const Char_t *Set[2] = {"tpt","sti"};
TFile *fIn[2] = {0,0};
const Char_t *charge[2] = {"P","N"};
const Char_t *scharge[2] = {"+","-"};
const Char_t *PG[2]     = {"f","Gl"};
static Int_t nPng = 0;
//________________________________________________________________________________
Int_t accept(Float_t pT, Float_t eta, Int_t nMcHit,Int_t GeantId, Int_t mFitPts, 
	     Float_t Qual, Int_t mNAssocGl, Int_t mNAssocPr, Char_t validMc=1, Short_t PrimaryMc=1 ) {
  if (! validMc || ! PrimaryMc) return 0;
  //  if (pT < 0.2 || TMath::Abs(eta) > 0.5 || nMcHit < 40) return 0;
#if 0
  if (TMath::Abs(eta) > 0.5 || nMcHit < 40) return 0;
  if (mFitPts > 0 && mFitPts < 25) return 0;
#else
  if (TMath::Abs(eta) > 0.5 || nMcHit < 20) return 0;
  if (mFitPts > 0 && mFitPts < 15) return 0;
#endif
  if (Qual > 0 && Qual < 90.0) return 0; 
  //  if (mNAssocGl > 1 || mNAssocPr > 1) return 0;
  if (GeantId == 8) return -1;
  if (GeantId == 9) return  1;
  return 0;
}
//________________________________________________________________________________
void DrawPng(TCanvas *c) {
  TString pngName("");
  if (c) {
    c->Update(); pngName = c->GetName();
    pngName.ReplaceAll(" ","_");
    pngName.ReplaceAll("(","_");
    pngName.ReplaceAll(")","_");
    pngName.ReplaceAll("{","_");
    pngName.ReplaceAll("}","_");
    pngName.ReplaceAll("<","lt");
    pngName.ReplaceAll(">","gt");
    pngName.ReplaceAll("tpt_sti_","");
    pngName.ReplaceAll("tpt_sti","");
    pngName.ReplaceAll("GeV/c","");
    pngName.ReplaceAll(".","_");
    pngName.ReplaceAll("/","_");
    pngName.ReplaceAll("^","_");
    pngName.ReplaceAll("__","_");
    pngName.ReplaceAll("__","_");
    pngName += ".png"; 
    TVirtualX::Instance()->WritePixmap(c->GetCanvasID(),-1,-1,(Char_t *)pngName.Data());
    nPng++;
    cout << "Draw #\t" << nPng << "\t" << pngName << endl;
  }
}
//________________________________________________________________________________
void DrawpTdif(const Char_t *plot = "pTDiff") {
  TString Plot(plot);
  TString c1tit = Plot;
  Double_t yMin[2] = { 99999.,  99999.};
  Double_t yMax[2] = {-99999., -99999.};
  Double_t xmin = 0;
  Double_t xmax = 2.5;
  TFile *f = 0;
  for (Int_t c = 0; c < 2; c++) {
    for (Int_t k = 1; k >= 0; k--) {
      f = fIn[k];
      if (! f) continue;
      cout << "File \t" << f->GetName() << endl;
      TString name = Plot;  name += charge[c];
      f->cd();
      TH2D * pTDiff = (TH2D*) f->Get(name);
      if (! pTDiff) continue;
      pTDiff->FitSlicesY();
      TString Mu(name); Mu += "_1";
      TString Sigma(name); Sigma += "_2";
      TH1D *mu = (TH1D*)gDirectory->Get(Mu); 
      TH1D *sigma = (TH1D*)gDirectory->Get(Sigma); 
      if (! mu || ! sigma) continue;
      Int_t nx = mu->GetNbinsX();
      for (Int_t i = 1; i <= nx; i++) {
	Double_t x = mu->GetBinCenter(i);
	if (x < xmin || x > xmax) continue;
	Double_t y = mu->GetBinContent(i);
	Double_t dy = TMath::Abs(mu->GetBinError(i));
	if (dy > 0 && TMath::Abs(y) > dy) {
	  if (y - dy < yMin[0]) yMin[0] = y - dy;
	  if (y + dy > yMax[0]) yMax[0] = y + dy;
	}
	//	cout << "x0\t" << x << "\ty\t" << y << "+-" << dy << "\tmin,max\t" << yMin[0] << "\t" << yMax[0] << endl;
	y = sigma->GetBinContent(i);
	dy = TMath::Abs(sigma->GetBinError(i));
	if (dy > 0 && TMath::Abs(y) > dy) {
	  if (y - dy < yMin[1]) yMin[1] = y - dy;
	  if (y + dy > yMax[1]) yMax[1] = y + dy;
	}
	//	cout << "x1\t" << x << "\ty\t" << y << "+-" << dy << "\tmin,max\t" << yMin[1] << "\t" << yMax[1] << endl;
      }
    }
  }
  //  cout << "ymin\t" << yMin[0] << "\t" << yMin[1] << endl;
  //  cout << "ymax\t" << yMax[0] << "\t" << yMax[1] << endl;
  for (Int_t i = 0; i < 2; i++) {
    if (yMin[i] < 0) yMin[i] *= 1.1; else yMin[i] *= 0.9;
    if (yMax[i] > 0) yMax[i] *= 1.1; else yMax[i] *= 0.9;
  }
  TCanvas *c1 = new TCanvas(c1tit,c1tit,400,400); //c1->SetLeftMargin(0.24);
  TString c2tit = c1tit;
  c2tit += "Sigma";
  TCanvas *c2 = new TCanvas(c2tit,c2tit,400,400); //c2->SetLeftMargin(0.24);
  TString same1("e");
  TString same2("e");
  TString title1, title2;
  Double_t scale = 1.e3;
  TLegend *l1 = 0;
  TLegend *l2 = 0;
  if (Plot == "pTDiff" || Plot == "pTDifGl") {
    l1 = new TLegend(.2,.15,.35,.35);
    l1->SetBorderSize(0);
    l1->SetFillColor(0);
    l1->SetTextSize(0.033);
    l2 = new TLegend(.2,.65,.45,.85);
    l2->SetBorderSize(0);
    l2->SetFillColor(0);
    l2->SetTextSize(0.033);
    Double_t ymin = -20; Double_t ymax = 5;
    title1 = "pT diff. #Delta p_{T} (MeV/c) for ";
    title2 = "sigma of pT diff.#sigma #Delta p_{T} (MeV/c) for ";
    if (Plot == "pTDifGl") {
      title1 += "Globals"; title2 += "Globals";
    } else {
      title1 += "Primaries"; title2 += "Primaries";
    }
    if (scale*yMin[0] > ymin) ymin = scale*yMin[0];
    if (scale*yMax[0] < ymax) ymax = scale*yMax[0];
    c1->cd();
    if (Plot == "pTDifGl") {ymin = -10; ymax =  2.;}
    else                   {ymin = -15.; ymax = 5;}
    TH1F *dummy = c1->DrawFrame(xmin ,ymin, xmax,ymax, title1);
    dummy->SetName("hframe1");
    dummy->SetXTitle("p_{TMc} (GeV/c)");
    dummy->SetYTitle("Delta p_{T} MeV/c");
    if (scale*yMin[1] > ymin) ymin = scale*yMin[1];
    if (scale*yMax[1] < ymax) ymax = scale*yMax[1];
    ymin = 0; ymax = 50;
    dummy->Draw();
    c2->cd();
    if (Plot == "pTDifGl") {ymin = 0; ymax = 50.;}
    TH1F *dummy2 = c2->DrawFrame(xmin ,ymin, xmax,ymax,title2);
    dummy2->SetName("hframe2");
    dummy2->SetXTitle("p_{TMc} (GeV/c)");
    dummy2->SetYTitle("sigma Delta p_{T} [MeV/c]");
    dummy2->Draw();
  } else if (Plot == "pTRel" || Plot == "pTRelGl") {
    l1 = new TLegend(.6,.20,.8,.45);
    l1->SetBorderSize(0);
    l1->SetFillColor(0);
    l1->SetTextSize(0.033);
    l2 = new TLegend(.2,.65,.45,.90);
    l2->SetBorderSize(0);
    l2->SetFillColor(0);
    l2->SetTextSize(0.033);
    scale = 1e2;
    Double_t ymin = -5; Double_t ymax = 1;
    title1 = "Relative pT diff.#delta p_{T} (%) for ";
    title2 = "sigma of relative pT diff. #sigma #delta p_{T} (%) for ";
    if (Plot == "pTRelGl") {
      title1 += "Globals"; title2 += "Globals";
    } else {
      title1 += "Primaries"; title2 += "Primaries";
    }
    if (scale*yMin[0] > ymin) ymin = scale*yMin[0];
    if (scale*yMax[0] < ymax) ymax = scale*yMax[0];
    c1->cd();
    if (Plot == "pTRelGl") {ymin = -5.; ymax = 1;}
    else                   {ymin = -2.; ymax = 0.5;}
    TH1F *dummy = c1->DrawFrame(xmin ,ymin, xmax,ymax, title1);
    dummy->SetName(c1->GetName());
    dummy->SetXTitle("p_{TMc} (GeV/c)");
    dummy->SetYTitle("#delta p_{T} [%]");
    dummy->Draw();
    c2->cd();
    if (scale*yMin[1] > ymin) ymin = scale*yMin[1];
    if (scale*yMax[1] < ymax) ymax = scale*yMax[1];
    ymin = 0; ymax = 5;
    TH1F *dummy2 = c2->DrawFrame(xmin ,ymin, xmax,ymax, title2);
    dummy2->SetName(c2->GetName());
    dummy2->SetXTitle("p_{TMc} (GeV/c)");
    dummy2->SetYTitle("sigma delta p_{T}) [%]");
    dummy2->Draw();
  } else  return;
  TString opt("e");
  for (Int_t c = 0; c < 2; c++) {
    for (Int_t k = 1; k >= 0; k--) {
      f = fIn[k];
      if (! f) continue;
      cout << "File \t" << f->GetName() << endl;
      TString name = Plot;  name += charge[c];
      f->cd();
      TH2D * pTDiff = (TH2D*) f->Get(name);
      if (! pTDiff) continue;
      pTDiff->FitSlicesY();
      TString Mu(name); Mu += "_1";
      TString Sigma(name); Sigma += "_2";
      TH1D *mu = (TH1D*)gDirectory->Get(Mu); 
      TH1D *sigma = (TH1D*)gDirectory->Get(Sigma); 
      if (! mu || ! sigma) continue;
      mu->SetMarkerStyle(21-k);
      mu->SetMarkerColor(1+c+2*k);
      sigma->SetMarkerStyle(21-k);
      sigma->SetMarkerColor(1+c+2*k);
      TString caption("pi"); caption += scharge[c];
      if (! Mu.Contains("Gl",TString::kIgnoreCase)) caption += " Pr.";
      else                                          caption += " Gl.";
      caption += Set[k];
      cout << mu->GetName() << "\t" << caption << endl;
      l1->AddEntry(mu,caption.Data(),"p");
      mu->Scale(scale);
      c1->cd();
      mu->Draw("same");
      l2->AddEntry(sigma,caption.Data(),"p");
      sigma->Scale(scale);
      c2->cd();
      sigma->Draw("same");
    }
  }
  c1->cd();
  l1->Draw();
  c1->Update();
  DrawPng(c1);
  c2->cd();
  l2->Draw();
  c2->Update();
  DrawPng(c2);
}
//________________________________________________________________________________
void DrawEff() {
#if 1
  TCanvas *caneff = new TCanvas("Efficiency","Efficiency");
  TH1D * pTMcP    = (TH1D*) gDirectory->Get("pTMcP");
  TH1D * pTMcN    = (TH1D*) gDirectory->Get("pTMcN");
  TH1D * pTMcA     = (TH1D*) gDirectory->Get("pTMcA");
  
  TH1D * pTGlP    = (TH1D*) gDirectory->Get("pTGlP");
  TH1D * pTGlN    = (TH1D*) gDirectory->Get("pTGlN");
  TH1D * pTGlA    = (TH1D*) gDirectory->Get("pTGlA");

  TH1D * pTPrP    = (TH1D*) gDirectory->Get("pTPrP");
  TH1D * pTPrN    = (TH1D*) gDirectory->Get("pTPrN");
  TH1D * pTPrA    = (TH1D*) gDirectory->Get("pTPrA");

  TH1D *effN = (TH1D *) pTPrN->Clone();   effN->SetName("effN");    effN->Reset();
  TH1D *effP = (TH1D *) pTPrP->Clone();   effP->SetName("effP");    effP->Reset();
  TH1D *effGlN = (TH1D *) pTGlN->Clone(); effGlN->SetName("effGlN");effGlN->Reset();
  TH1D *effGlP = (TH1D *) pTGlP->Clone(); effGlP->SetName("effGlP");effGlP->Reset();
  Int_t nx = effN->GetNbinsX();
  Double_t val, sum, err;
  for (Int_t l = 1; l <= nx; l++) {
    val = pTPrN->GetBinContent(l);
    sum = pTMcN->GetBinContent(l);
    if (sum < 1.e-7 || val > sum) {val = 0; err = 0;}
    else { val /= sum;     err = TMath::Sqrt(val*(1.-val)/sum);}
    effN->SetBinContent(l,100.*val);
    effN->SetBinError(l,100.*err);
    
    val = pTPrP->GetBinContent(l);
    sum = pTMcP->GetBinContent(l);
    if (sum < 1.e-7 || val > sum) {val = 0; err = 0;}
    else { val /= sum;     err = TMath::Sqrt(val*(1.-val)/sum);}
    effP->SetBinContent(l,100.*val);
    effP->SetBinError(l,100.*err);

    val = pTGlN->GetBinContent(l);
    sum = pTMcN->GetBinContent(l);
    if (sum < 1.e-7 || val > sum) {val = 0; err = 0;}
    else { val /= sum;     err = TMath::Sqrt(val*(1.-val)/sum);}
    effGlN->SetBinContent(l,100.*val);
    effGlN->SetBinError(l,100.*err);
    
    val = pTGlP->GetBinContent(l);
    sum = pTMcP->GetBinContent(l);
    if (sum < 1.e-7 || val > sum) {val = 0; err = 0;}
    else { val /= sum;     err = TMath::Sqrt(val*(1.-val)/sum);}
    effGlP->SetBinContent(l,100.*val);
    effGlP->SetBinError(l,100.*err);
  }
  
  effP->SetMarkerStyle(28);
  effP->SetMarkerColor(4);
  effN->SetMarkerStyle(23);
  effN->SetMarkerColor(3);
  effGlP->SetMarkerStyle(28);
  effGlP->SetMarkerColor(7);
  effGlN->SetMarkerStyle(23);
  effGlN->SetMarkerColor(6);
  TString title("Efficiency (%)."); title += effP->GetTitle();
  effP->SetTitle(title);
  effP->SetXTitle("Mc pT [GeV/c]");
  effP->SetYTitle("Efficiency (%)");
  TLegend *l4 = new TLegend(.5,.2,0.9,.4);
  effP->SetStats(0);
  effP->SetAxisRange(0,2);
  effP->Draw();       l4->AddEntry(effP,"#pi+ Pr.","p");
  effN->Draw("same"); l4->AddEntry(effN,"#pi- Pr.","p");
//   effGlP->Draw();       l4->AddEntry(effGlP,"#pi+ Gl.","p");
//   effGlN->Draw("same"); l4->AddEntry(effGlN,"#pi- Gl.","p");
  l4->Draw();
  DrawPng(caneff);
#endif  
}
//________________________________________________________________________________
void DrawPrimVx() {
  Char_t *XYZ[3] = {"X","Y","Z"};
  for (Int_t i = 0; i < 3; i++) {
    TH1* h = (TH1 *) gDirectory->Get(Form("DifPv%s",XYZ[i]));
    if (! h) continue;
    TCanvas *c = new TCanvas(Form("PrimaryVertex%sdiff",XYZ[i]),Form("PrimaryVertex%sdiff",XYZ[i]));
    h->Draw();
    DrawPng(c);
  }
}
//________________________________________________________________________________
void Draw(const Char_t *fNameTPT, const Char_t *fNameSTI=0) {
  if (fNameTPT) fIn[0] = new TFile(fNameTPT);
  if (fNameSTI) fIn[1] = new TFile(fNameSTI);
  if (! fIn[0] && ! fIn[1] ) return;
  TString tFile;
  if (fNameTPT)  {tFile = fNameTPT; tFile.ReplaceAll(".root","");}
  if (fNameSTI)  {tFile = fNameSTI; tFile.ReplaceAll(".root","");}
  DrawpTdif("pTDiff"); 
  DrawpTdif("pTDifGl");
  DrawpTdif("pTRel"); 
  DrawpTdif("pTRelGl");
  DrawEff();
  DrawPrimVx();
}

//________________________________________________________________________________
void MiniMcPlots(const Char_t *files="*minimc.root") {  // root.exe 'MiniMcPlots.C+("*minimc.root")'
  if (! files) return;
  TDirIter Dir(files);
  TTreeIter iter("StMiniMcTree");
  TFile *f = 0;
  Int_t NFiles = 0;
  Char_t *file = 0;
  Char_t *file1 = 0;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++; file1 = file;}
  cout << files << "\twith " << NFiles << " files" << "\t" << file1 << endl; 
  if (! file1 ) return;
  TString pFile(file1);
  Int_t i1 = pFile.Index("TbyT/"); 
  if (i1 >= 0) i1 += 5; 
  else  {
    i1 = pFile.Index("MC/");
    if (i1 >= 0) i1 += 3;
    else {
      i1 = pFile.Index("./"); 
      if (i1 >= 0) i1 += 2;
      else  i1 = 0; 
    }
  }
  Int_t i2 = pFile.Index(".root");
  cout << pFile.Data() << "\t" << i1 << "\t/" << i2 << endl;
  TString tFile(pFile.Data()+i1,i2-i1);
  tFile.ReplaceAll("/","_"); cout << "Master file\t" << tFile.Data() << endl;
  TString Out(tFile);
  Out += ".15_20.Plots.root";
  TFile *fOut = new TFile(Out.Data(),"recreate");
//   const Int_t                           &mEventId = iter("mEventId");
//   const Int_t                             &mRunId = iter("mRunId");
//   const Int_t                        &mOriginMult = iter("mOriginMult");
//   const Int_t                       &mCentralMult = iter("mCentralMult");
//   const Int_t                        &mCentrality = iter("mCentrality");
//   const Int_t     &mNUncorrectedNegativePrimaries = iter("mNUncorrectedNegativePrimaries");
//   const Int_t             &mNUncorrectedPrimaries = iter("mNUncorrectedPrimaries");
//   const Int_t        &mNFtpcWUncorrectedPrimaries = iter("mNFtpcWUncorrectedPrimaries");
//   const Int_t        &mNFtpcEUncorrectedPrimaries = iter("mNFtpcEUncorrectedPrimaries");
//   const Int_t                            &mMcMult = iter("mMcMult");
//   const Int_t                            &mNMcNch = iter("mNMcNch");
//   const Int_t                       &mNMcFtpcWNch = iter("mNMcFtpcWNch");
//   const Int_t                       &mNMcFtpcENch = iter("mNMcFtpcENch");
//   const Int_t                         &mNMcHminus = iter("mNMcHminus");
//   const Int_t                         &mNMcGlobal = iter("mNMcGlobal");
//   const Int_t                   &mNMcGoodGlobal20 = iter("mNMcGoodGlobal20");
//   const Int_t                         &mNRcGlobal = iter("mNRcGlobal");
//   const Int_t                   &mNRcGoodGlobal20 = iter("mNRcGoodGlobal20");
  const Float_t                           &mVertexX = iter("mVertexX");
  const Float_t                           &mVertexY = iter("mVertexY");
  const Float_t                           &mVertexZ = iter("mVertexZ");
  const Float_t                         &mMcVertexX = iter("mMcVertexX");
  const Float_t                         &mMcVertexY = iter("mMcVertexY");
  const Float_t                         &mMcVertexZ = iter("mMcVertexZ");
//   const Float_t                          &mMagField = iter("mMagField");
//   const Float_t                &mCenterOfMassEnergy = iter("mCenterOfMassEnergy");
//   const Float_t                    &mBackgroundRate = iter("mBackgroundRate");
//   const Short_t                &mBeamMassNumberEast = iter("mBeamMassNumberEast");
//   const Short_t                &mBeamMassNumberWest = iter("mBeamMassNumberWest");
//   const Float_t                               &mCtb = iter("mCtb");
//   const Float_t                              &mZdcE = iter("mZdcE");
//   const Float_t                              &mZdcW = iter("mZdcW");
  const Int_t                          &mNMcTrack = iter("mNMcTrack");
  const Int_t                      &mNMatchedPair = iter("mNMatchedPair");
//   const Int_t                       &mNMergedPair = iter("mNMergedPair");
//   const Int_t                        &mNSplitPair = iter("mNSplitPair");
//   const Int_t                        &mNGhostPair = iter("mNGhostPair");
//   const Int_t                       &mNContamPair = iter("mNContamPair");
//  const Int_t                      &mNMatGlobPair = iter("mNMatGlobPair");
  const Int_t                         &mMcTracks_ = iter("mMcTracks_");
  //yf  const Char_t                *&mMcTracks_mIsValid = iter("mMcTracks.mIsValid");   //[mMcTracks_]
  const Float_t                   *&mMcTracks_mPtMc = iter("mMcTracks.mPtMc");
//   const Float_t                   *&mMcTracks_mPzMc = iter("mMcTracks.mPzMc");
  const Float_t                  *&mMcTracks_mEtaMc = iter("mMcTracks.mEtaMc");
//   const Float_t                  *&mMcTracks_mPhiMc = iter("mMcTracks.mPhiMc");
  const Short_t                 *&mMcTracks_mNHitMc = iter("mMcTracks.mNHitMc");
//   const Short_t              *&mMcTracks_mNSvtHitMc = iter("mMcTracks.mNSvtHitMc");
//   const Short_t              *&mMcTracks_mNSsdHitMc = iter("mMcTracks.mNSsdHitMc");
//   const Short_t             *&mMcTracks_mNFtpcHitMc = iter("mMcTracks.mNFtpcHitMc");
  const Short_t                *&mMcTracks_mGeantId = iter("mMcTracks.mGeantId");
//   const Short_t               *&mMcTracks_mChargeMc = iter("mMcTracks.mChargeMc");
//   const Float_t                  *&mMcTracks_mStopR = iter("mMcTracks.mStopR");
//   const Short_t                    *&mMcTracks_mKey = iter("mMcTracks.mKey");
  const Short_t               *&mMcTracks_mNAssocGl = iter("mMcTracks.mNAssocGl");
  const Short_t               *&mMcTracks_mNAssocPr = iter("mMcTracks.mNAssocPr");
  //yf  const Short_t         *&mMcTracks_mIsPrimary = iter("mMcTracks.mIsPrimary");   //[mMcTracks_]
  const Int_t                     &mMatchedPairs_ = iter("mMatchedPairs_");
//   const Short_t         *&mMatchedPairs_mNCommonHit = iter("mMatchedPairs.mNCommonHit");
//   const Short_t       *&mMatchedPairs_mIsBestContam = iter("mMatchedPairs.mIsBestContam");
//   const Short_t        *&mMatchedPairs_mDominatrack = iter("mMatchedPairs.mDominatrack");
//   const Short_t     *&mMatchedPairs_mDominCommonHit = iter("mMatchedPairs.mDominCommonHit");
  const Float_t         *&mMatchedPairs_mAvgQuality = iter("mMatchedPairs.mAvgQuality");
  //yf  const Char_t          *&mMatchedPairs_mIsValid = iter("mMatchedPairs.mIsValid");   //[mMatchedPairs_]
  const Float_t               *&mMatchedPairs_mPtMc = iter("mMatchedPairs.mPtMc");
//   const Float_t               *&mMatchedPairs_mPzMc = iter("mMatchedPairs.mPzMc");
  const Float_t              *&mMatchedPairs_mEtaMc = iter("mMatchedPairs.mEtaMc");
//   const Float_t              *&mMatchedPairs_mPhiMc = iter("mMatchedPairs.mPhiMc");
  const Short_t             *&mMatchedPairs_mNHitMc = iter("mMatchedPairs.mNHitMc");
//   const Short_t          *&mMatchedPairs_mNSvtHitMc = iter("mMatchedPairs.mNSvtHitMc");
//   const Short_t          *&mMatchedPairs_mNSsdHitMc = iter("mMatchedPairs.mNSsdHitMc");
//   const Short_t         *&mMatchedPairs_mNFtpcHitMc = iter("mMatchedPairs.mNFtpcHitMc");
  const Short_t            *&mMatchedPairs_mGeantId = iter("mMatchedPairs.mGeantId");
//   const Short_t           *&mMatchedPairs_mChargeMc = iter("mMatchedPairs.mChargeMc");
//   const Float_t              *&mMatchedPairs_mStopR = iter("mMatchedPairs.mStopR");
//   const Short_t                *&mMatchedPairs_mKey = iter("mMatchedPairs.mKey");
  const Short_t           *&mMatchedPairs_mNAssocGl = iter("mMatchedPairs.mNAssocGl");
  const Short_t           *&mMatchedPairs_mNAssocPr = iter("mMatchedPairs.mNAssocPr");
  const Float_t               *&mMatchedPairs_mPtPr = iter("mMatchedPairs.mPtPr");
  //yf  const Char_t          *&mMatchedPairs_mIsValidGl = iter("mMatchedPairs.mIsValidGl");   //[mMatchedPairs_]
  //yf  const Short_t         *&mMatchedPairs_mIsPrimary = iter("mMatchedPairs.mIsPrimary");
//   const Float_t               *&mMatchedPairs_mPzPr = iter("mMatchedPairs.mPzPr");
//   const Float_t              *&mMatchedPairs_mEtaPr = iter("mMatchedPairs.mEtaPr");
//   const Float_t              *&mMatchedPairs_mPhiPr = iter("mMatchedPairs.mPhiPr");
//   const Float_t              *&mMatchedPairs_mDcaPr = iter("mMatchedPairs.mDcaPr");
//   const Float_t            *&mMatchedPairs_mDcaXYPr = iter("mMatchedPairs.mDcaXYPr");
//   const Float_t             *&mMatchedPairs_mDcaZPr = iter("mMatchedPairs.mDcaZPr");
//   const Float_t             *&mMatchedPairs_mCurvPr = iter("mMatchedPairs.mCurvPr");
//   const Float_t             *&mMatchedPairs_mTanLPr = iter("mMatchedPairs.mTanLPr");
  // Float_t           **&mMatchedPairs_mErrP    = iter("mMatchedPairs.mErrP[5]");
//   const Float_t             *&mMatchedPairs_mChi2Pr = iter("mMatchedPairs.mChi2Pr");
//   const Short_t               *&mMatchedPairs_mFlag = iter("mMatchedPairs.mFlag");
//   const Float_t               *&mMatchedPairs_mDedx = iter("mMatchedPairs.mDedx");
  const Float_t               *&mMatchedPairs_mPtGl = iter("mMatchedPairs.mPtGl");
//   const Float_t               *&mMatchedPairs_mPzGl = iter("mMatchedPairs.mPzGl");
//   const Float_t              *&mMatchedPairs_mEtaGl = iter("mMatchedPairs.mEtaGl");
//   const Float_t              *&mMatchedPairs_mPhiGl = iter("mMatchedPairs.mPhiGl");
//   const Float_t              *&mMatchedPairs_mDcaGl = iter("mMatchedPairs.mDcaGl");
  const Float_t            *&mMatchedPairs_mDcaXYGl = iter("mMatchedPairs.mDcaXYGl");
//   const Float_t             *&mMatchedPairs_mDcaZGl = iter("mMatchedPairs.mDcaZGl");
//   const Float_t             *&mMatchedPairs_mCurvGl = iter("mMatchedPairs.mCurvGl");
//   const Float_t             *&mMatchedPairs_mTanLGl = iter("mMatchedPairs.mTanLGl");
  // Float_t           **&mMatchedPairs_mErrG    = iter("mMatchedPairs.mErrG[5]");
//   const Float_t            *&mMatchedPairs_mPidPion = iter("mMatchedPairs.mPidPion");
//   const Float_t          *&mMatchedPairs_mPidProton = iter("mMatchedPairs.mPidProton");
//   const Float_t            *&mMatchedPairs_mPidKaon = iter("mMatchedPairs.mPidKaon");
//   const Float_t        *&mMatchedPairs_mPidElectron = iter("mMatchedPairs.mPidElectron");
//   const Float_t             *&mMatchedPairs_mFirstZ = iter("mMatchedPairs.mFirstZ");
//   const Float_t              *&mMatchedPairs_mLastZ = iter("mMatchedPairs.mLastZ");
//   const Short_t        *&mMatchedPairs_mFirstPadrow = iter("mMatchedPairs.mFirstPadrow");
//   const Short_t         *&mMatchedPairs_mLastPadrow = iter("mMatchedPairs.mLastPadrow");
//   const Short_t     *&mMatchedPairs_mFirstFitPadrow = iter("mMatchedPairs.mFirstFitPadrow");
//   const Short_t      *&mMatchedPairs_mLastFitPadrow = iter("mMatchedPairs.mLastFitPadrow");
//   const Short_t        *&mMatchedPairs_mFirstSector = iter("mMatchedPairs.mFirstSector");
//   const Short_t         *&mMatchedPairs_mLastSector = iter("mMatchedPairs.mLastSector");
  const Short_t             *&mMatchedPairs_mFitPts = iter("mMatchedPairs.mFitPts");
//   const Short_t             *&mMatchedPairs_mFitSvt = iter("mMatchedPairs.mFitSvt");
//   const Short_t             *&mMatchedPairs_mFitSsd = iter("mMatchedPairs.mFitSsd");
//   const Short_t            *&mMatchedPairs_mFitFtpc = iter("mMatchedPairs.mFitFtpc");
//   const Short_t            *&mMatchedPairs_mDedxPts = iter("mMatchedPairs.mDedxPts");
//   const Short_t             *&mMatchedPairs_mAllPts = iter("mMatchedPairs.mAllPts");
//   const Short_t             *&mMatchedPairs_mCharge = iter("mMatchedPairs.mCharge");
//   const Short_t           *&mMatchedPairs_mNAssocMc = iter("mMatchedPairs.mNAssocMc");
//   const Short_t          *&mMatchedPairs_mNPossible = iter("mMatchedPairs.mNPossible");
//yf  const Char_t          *&mMatchedPairs_mIsValidPr = iter("mMatchedPairs.mIsValidPr");
  //#include "miniMc.h"
  TH1D *DifPvX = new TH1D("DifPvX","Difference in X for Rc - Mc positions",100,-0.25,0.25);
  TH1D *DifPvY = new TH1D("DifPvY","Difference in Y for Rc - Mc positions",100,-0.25,0.25);
  TH1D *DifPvZ = new TH1D("DifPvZ","Difference in Z for Rc - Mc positions",100,-0.25,0.25);
  TString Title("");
  if (tFile.Contains("tpt",TString::kIgnoreCase)) Title = "TPT :";
  else                                            Title = "ITTF:";
  //  Title +=" pT>0.2 | #eta |<0.5 && McHit>39  FitPts>24 Q>90";
  Title +=" #eta |<0.5 && McHit>39  FitPts>24 Q>90";
  TH2D * pTDiffP = new TH2D("pTDiffP",Title,40,0.,4.,200,-.2,.2);
  TH2D * pTDiffN = new TH2D("pTDiffN",Title,40,0.,4.,200,-.2,.2);
  TH2D * pTRelP  = new TH2D("pTRelP",Title,40,0.,4.,200,-.2,.2);
  TH2D * pTRelN  = new TH2D("pTRelN",Title,40,0.,4.,200,-.2,.2);

  TH2D * pTDifGlP = new TH2D("pTDifGlP",Title,40,0.,4.,200,-.2,.2);
  TH2D * pTDifGlN = new TH2D("pTDifGlN",Title,40,0.,4.,200,-.2,.2);
  TH2D * pTRelGlP  = new TH2D("pTRelGlP",Title,40,0.,4.,200,-.2,.2);
  TH2D * pTRelGlN  = new TH2D("pTRelGlN",Title,40,0.,4.,200,-.2,.2);
  
  TH2D * dcaP     = new TH2D("dcaP",Title,20,0.,4.,120,-3.,3.);
  TH2D * dcaN     = new TH2D("dcaN",Title,20,0.,4.,120,-3.,3.);
  
  TH1D * pTMcP    = new TH1D("pTMcP","pT for MC pion+",100,0.,5.);
  TH1D * pTMcN    = new TH1D("pTMcN","pT for MC pion-",100,0.,5.);
  TH1D * pTMcA     = new TH1D("pTMcA","pT for MC all",100,0.,5.);
  
  TH1D * pTPrP    = new TH1D("pTPrP","pT for Matched pion+",100,0.,5.);
  TH1D * pTPrN    = new TH1D("pTPrN","pT for Matched pion-",100,0.,5.);
  TH1D * pTPrA    = new TH1D("pTPrA","pT for Matched all",100,0.,5.);

  TH1D * pTGlP    = new TH1D("pTGlP","pT for Global pion+",100,0.,5.);
  TH1D * pTGlN    = new TH1D("pTGlN","pT for Global pion-",100,0.,5.);
  TH1D * pTGlA    = new TH1D("pTGlA","pT for Global all",100,0.,5.);
  
  TH1D * EtaMcP    = new TH1D("EtaMcP","Eta for MC pion+",100,-2.5,2.5);
  TH1D * EtaMcN    = new TH1D("EtaMcN","Eta for MC pion-",100,-2.5,2.5);
  TH1D * EtaMcA     = new TH1D("EtaMcA","Eta for MC all",100,-2.5,2.5);
  
  TH1D * EtaPrP    = new TH1D("EtaPrP","Eta for Matched pion+",100,-2.5,2.5);
  TH1D * EtaPrN    = new TH1D("EtaPrN","Eta for Matched pion-",100,-2.5,2.5);
  TH1D * EtaPrA    = new TH1D("EtaPrA","Eta for Matched all",100,-2.5,2.5);

  TH1D * EtaGlP    = new TH1D("EtaGlP","Eta for Global pion+",100,-2.5,2.5);
  TH1D * EtaGlN    = new TH1D("EtaGlN","Eta for Global pion-",100,-2.5,2.5);
  TH1D * EtaGlA    = new TH1D("EtaGlA","Eta for Global all",100,-2.5,2.5);
  
  //  gROOT->LoadMacro("MiniMc.h");
  Int_t nread = 0;
  while (iter.Next()) {
    DifPvX->Fill(mVertexX - mMcVertexX);
    DifPvY->Fill(mVertexY - mMcVertexY);
    DifPvZ->Fill(mVertexZ - mMcVertexZ);
    if (TMath::Abs(mVertexX - mMcVertexX) > 0.25 ||
	TMath::Abs(mVertexY - mMcVertexY) > 0.25 ||
	TMath::Abs(mVertexZ - mMcVertexZ) > 0.25) continue;
    //     cout << "Vertex " << mVertexX << "\t" << mVertexY << "\t" << mVertexZ
    // 	 << "\tMC\t" << mMcVertexX << "\t" << mMcVertexY << "\t" << mMcVertexZ << endl;
    //     cout << "mNMcTrack " << mNMcTrack
    // 	 << "\tmNMatchedPair " << mNMatchedPair
    // 	 << "\tmNMergedPair " << mNMergedPair
    // 	 << "\tmNSplitPair " << mNSplitPair
    // 	 << "\tmNGhostPair " << mNGhostPair
    // 	 << "\tmNContamPair " << mNContamPair
    // 	 << "\tmNMatGlobPair " << mNMatGlobPair << endl;
    Int_t k;
    for (k = 0; k < mNMcTrack; k++) {
      //      cout << "McTrack " << k << "\tpT = " << mMcTracks_mPtMc[k] << "\tpZ = " << mMcTracks_mPzMc[k] << endl;
      Int_t iok = accept(mMcTracks_mPtMc[k],mMcTracks_mEtaMc[k],mMcTracks_mNHitMc[k],mMcTracks_mGeantId[k],-1,-1.,
			 mMcTracks_mNAssocGl[k],mMcTracks_mNAssocPr[k],1,1);
      //			 mMcTracks_mIsValid[k],mMcTracks_mIsPrimary[k]);
      if (! iok) continue;
      pTMcA->Fill(mMcTracks_mPtMc[k]);
      EtaMcA->Fill(mMcTracks_mEtaMc[k]);
      if (iok < 0) {
	pTMcN->Fill(mMcTracks_mPtMc[k]);
	EtaMcN->Fill(mMcTracks_mEtaMc[k]);
      } else {       
	pTMcP->Fill(mMcTracks_mPtMc[k]);
	EtaMcP->Fill(mMcTracks_mEtaMc[k]);
      }
    }
    for (k = 0; k < mNMatchedPair; k++) {
      // cout << "mMatchedPairs " << k << "\tpT = " << mMatchedPairs_mPtMc[k] << "\tpZ = " << mMatchedPairs_mPzMc[k] << endl;
      if (mMatchedPairs_mFitPts[k] < 15) continue;
      Int_t iok = accept(mMatchedPairs_mPtMc[k],mMatchedPairs_mEtaMc[k],mMatchedPairs_mNHitMc[k],
			 mMatchedPairs_mGeantId[k],mMatchedPairs_mFitPts[k],mMatchedPairs_mAvgQuality[k],
			 mMatchedPairs_mNAssocGl[k],mMatchedPairs_mNAssocPr[k]);
      //yf			 mMatchedPairs_mIsValid[k],mMatchedPairs_mIsPrimary[k]);
      if (! iok) continue;
      //yf      if (! mMatchedPairs_mIsValidGl[k]) continue; 
      Double_t EtaMc = mMatchedPairs_mEtaMc[k];
      Double_t pTMc = mMatchedPairs_mPtMc[k];
      Double_t dif = mMatchedPairs_mPtGl[k]-pTMc;
      Double_t rel = dif/pTMc;
      EtaGlA->Fill(EtaMc);
      pTGlA->Fill(pTMc);
      if (iok < 0) {
	pTGlN->Fill(pTMc);
	EtaGlN->Fill(EtaMc);
	pTDifGlN->Fill(pTMc,dif);
	pTRelGlN->Fill(pTMc,rel);
      } else {
        pTGlP->Fill(pTMc);
	EtaGlP->Fill(EtaMc);
 	pTDifGlP->Fill(pTMc,dif);
	pTRelGlP->Fill(pTMc,rel);
      }
      //yf      if (! mMatchedPairs_mIsValidPr[k]) continue;
      dif = mMatchedPairs_mPtPr[k]-pTMc;
      rel = dif/pTMc;
      EtaPrA->Fill(EtaMc);
      pTPrA->Fill(pTMc);
      if (iok < 0) {
	pTPrN->Fill(pTMc);
	EtaPrN->Fill(EtaMc);
	pTDiffN->Fill(pTMc,dif);
	pTRelN->Fill(pTMc,rel);
	dcaN->Fill(pTMc,mMatchedPairs_mDcaXYGl[k]);
      } else {
        pTPrP->Fill(pTMc);
	EtaPrP->Fill(EtaMc);
 	pTDiffP->Fill(pTMc,dif);
	pTRelP->Fill(pTMc,rel);
	dcaP->Fill(pTMc,mMatchedPairs_mDcaXYGl[k]);
      }
    }
    nread++;
    //    if (nread>100) break;
    if (nread%1000 == 1) cout << "read " << nread << " events" << endl;
  }
  fOut->Write();
  delete fOut;
  if (Title.BeginsWith("TPT",TString::kIgnoreCase)) Draw(Out, 0);
  else                                              Draw(0, Out);
}

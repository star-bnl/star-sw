
#include <iostream>

#include "TCanvas.h"
#include "TError.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TLegend.h"
#include "TObject.h"
#include "TPaveText.h"
#include "TPDF.h"
#include "TStyle.h"
#include "TSystem.h"

#include "StEmbeddingQADraw.h"
#include "StEmbeddingQAUtilities.h"

using namespace std ;

ClassImp(StEmbeddingQADraw)

  static Int_t kCanvasId = 1 ;
  static const Double_t kPtMax = 5.0; // Draw up to 5 GeV/c

//____________________________________________________________________________________________________
StEmbeddingQADraw::StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile)
  : isGIFOn(kFALSE), isJPGOn(kFALSE), isEPSOn(kFALSE), isPSOn(kFALSE)
{
  // Open input files
  Open(embeddingFile, realDataFile);

  // Year, production and particle name from input file
  TString fileName(embeddingFile);
  fileName.Remove(fileName.Index(".root"), fileName.Length()); // remove .root

  for(Int_t i=0;i<4;i++){
    const Int_t id    = fileName.First("_") ;
    const Int_t start = 0 ;
    const Int_t stop  = id ;
    TString subString(fileName(start, stop));

    fileName.Remove(start, stop+1);
    cout << Form("i=%3d, sub=%20s, name=%30s", i, subString.Data(), fileName.Data()) << endl;

    if( i == 2 ) kYear = subString.Atoi();
    if( i == 3 ){
      kProduction   = subString;
      kParticleName = fileName ;
    }
  }

  cout << endl;
  cout << Form("  Year       =  %10d", kYear) << endl;
  cout << Form("  Production =  %10s", kProduction.Data()) << endl;
  cout << Form("  Particle   =  %10s", kParticleName.Data()) << endl;
  cout << endl;

  SetStyle();
}

//____________________________________________________________________________________________________
StEmbeddingQADraw::StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile,
    const Int_t year, const TString production, const TString particleName)
  : isGIFOn(kFALSE), isJPGOn(kFALSE), isEPSOn(kFALSE), isPSOn(kFALSE)
{
  // Open input files
  Open(embeddingFile, realDataFile);

  // Year, production and particle name by hand
  kYear         = year ;
  kProduction   = production ;
  kParticleName = particleName ;

  cout << endl;
  cout << Form("  Year       =  %10d", kYear) << endl;
  cout << Form("  Production =  %10s", kProduction.Data()) << endl;
  cout << Form("  Particle   =  %10s", kParticleName.Data()) << endl;
  cout << endl;

  SetStyle();
}

//____________________________________________________________________________________________________
StEmbeddingQADraw::~StEmbeddingQADraw()
{
  mInputEmbedding->Close();
  mInputRealData->Close();
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::SetOutputDirectory(const TString name)
{
  mOutputFigureDirectory = name ;

  // Make sure if it exists
  if( gSystem->AccessPathName(name) == kTRUE ){ // 0 is true
    Error("SetOutputDirectory", "Directory %s does not exist. Make sure you put correct path. Abort()", name.Data());
    abort();
  }

  // Make sure you put '/' at the end of directory name
  if ( mOutputFigureDirectory.Last('/') + 1 != mOutputFigureDirectory.Length() ){
    cout << endl;
    cout << "Put / at the end of output directory name" << endl;
    mOutputFigureDirectory.Append("/");
  }

  cout << "Set output directory for figures : " << mOutputFigureDirectory << endl;
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::Open(const TString embeddingFile, const TString realDataFile)
{
  // OPEN embedding file
  mInputEmbedding = TFile::Open(embeddingFile);

  if(!mInputEmbedding || !mInputEmbedding->IsOpen() || mInputEmbedding->IsZombie() ){
    Error("StEmbeddingQADraw", "can't find %s", embeddingFile.Data());
    return;
  }
  cout << "OPEN input (embedding) : " << mInputEmbedding->GetName() << endl;
  cout << endl;

  // OPEN real data file
  mInputRealData = TFile::Open(realDataFile);

  if(!mInputRealData || !mInputRealData->IsOpen() || mInputRealData->IsZombie() ){
    Error("StEmbeddingQADraw", "can't find %s", realDataFile.Data());
    return;
  }
  cout << "OPEN input (real data) : " << mInputRealData->GetName() << endl;
  cout << endl;
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::Print(const TCanvas& canvas, const TString name)
{
  // Output directory needs "/" at the end of name

  canvas.Print(Form("%s%s_%s.png", mOutputFigureDirectory.Data(), name.Data(), GetBaseName()));
  if( isGIFOn ) canvas.Print(Form("%s%s_%s.gif", mOutputFigureDirectory.Data(), name.Data(), GetBaseName()));
  if( isJPGOn ) canvas.Print(Form("%s%s_%s.jpg", mOutputFigureDirectory.Data(), name.Data(), GetBaseName()));
  if( isEPSOn ) canvas.Print(Form("%s%s_%s.eps", mOutputFigureDirectory.Data(), name.Data(), GetBaseName()));
  if( isPSOn )  canvas.Print(Form("%s%s_%s.ps", mOutputFigureDirectory.Data(), name.Data(), GetBaseName()));
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::SetStyle()
{
  const Int_t font = 42;

  //_______________________________________________________________
  gStyle->SetPalette(1);

  //_______________________________________________________________
  //  Canvas style
  gStyle->SetPadColor(10);
  gStyle->SetCanvasColor(10);
  gStyle->SetFrameLineWidth(2);
  gStyle->SetPadTickX(1);
  gStyle->SetPadTickY(1);
  gStyle->SetPadRightMargin(0.15);
  gStyle->SetPadLeftMargin(0.21);
  gStyle->SetPadTopMargin(0.10);
  gStyle->SetPadBottomMargin(0.20);

  //_______________________________________________________________
  //  Statistics
  gStyle->SetStatColor(10);

  //_______________________________________________________________
  //  Text
  gStyle->SetTextSize(0.07);
  gStyle->SetTextFont(font);

  //_______________________________________________________________
  //  Histogram style
  gStyle->SetNdivisions(505,"XYZ");

  // Label
  gStyle->SetLabelSize(0.07, "XYZ");
  gStyle->SetLabelOffset(0.011, "XYZ");
  gStyle->SetLabelFont(font, "XYZ");

  // Title
  gStyle->SetTitleSize(0.085, "XYZ");
  gStyle->SetTitleOffset(1.05, "X");
  gStyle->SetTitleOffset(1.18, "Y");
  gStyle->SetTitleFont(font, "XYZ");

  // Pad title
  gStyle->SetTitleFont(42, "t"); // Set pad title font if the option is not "X or Y or Z"
  gStyle->SetTitleH(0.07);
  gStyle->SetTitleW(0.6);
  gStyle->SetTitleBorderSize(0);
  gStyle->SetTitleFillColor(10);
  gStyle->SetTitleX(0.1);

  //_______________________________________________________________
  // Legend
  gStyle->SetLegendBorderSize(0);
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::SetStyle(TH1* h)
{
  const Int_t font = 42 ;
  h->GetXaxis()->SetTitleFont(font);
  h->GetYaxis()->SetTitleFont(font);
  h->GetZaxis()->SetTitleFont(font);

  h->SetTitleSize(0.085, "X"); h->SetTitleSize(0.085, "Y"); h->SetTitleSize(0.085, "Z");
  h->SetTitleOffset(1.05, "X");
  h->SetTitleOffset(1.18, "Y");

  h->SetLabelSize(0.07, "X"); h->SetLabelSize(0.07, "Y"); h->SetLabelSize(0.07, "Z");
  h->SetLabelOffset(0.011, "X"); h->SetLabelOffset(0.011, "Y"); h->SetLabelOffset(0.011, "Z");
  h->SetLabelFont(font, "X"); h->SetLabelFont(font, "Y"); h->SetLabelFont(font, "Z");

  h->SetNdivisions(505,"X"); h->SetNdivisions(505,"Y"); h->SetNdivisions(505,"Z");

  h->SetLineWidth(2);
}

//____________________________________________________________________________________________________
Int_t StEmbeddingQADraw::GetEntries() const
{
  return (Int_t)( ((TH1D*)mInputEmbedding->Get("hVz"))->GetEntries() );
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::IsDecay() const
{
  const Int_t particleId = StEmbeddingQAUtilities::GetParticleId(kParticleName);

  // true if particles have decay daughters
  switch ( particleId ){
    case 1   : return kFALSE ; // photon
    case 2   : return kFALSE ; // electron
    case 3   : return kFALSE ; // positron
    case 7   : return kTRUE  ; // pi0
    case 8   : return kFALSE ; // pion+
    case 9   : return kFALSE ; // pion-
    case 11  : return kFALSE ; // kaon+
    case 12  : return kFALSE ; // kaon-
    case 14  : return kFALSE ; // proton
    case 15  : return kFALSE ; // anti-proton
    case 37  : return kTRUE  ; // D0
    case 38  : return kTRUE  ; // D0bar
    case 50  : return kTRUE  ; // phi
    case 160 : return kTRUE  ; // J/Psi
    case 995 : return kTRUE  ; // Lambda(1520)
    default:
      Warning("IsDecay", "Unknown particle id, id=%3d", particleId);
      return kFALSE ;
  }

  return kFALSE ;
}

//____________________________________________________________________________________________________
Int_t StEmbeddingQADraw::GetCategoryId(const Bool_t isEmbedding) const
{
  // IsDecay = kTRUE  --> Contaminated pairs
  // IsDecay = kFALSE --> Matched pairs

  if ( isEmbedding ){
    return (IsDecay()) ? StEmbeddingQAUtilities::GetCategoryId("CONTAM") : StEmbeddingQAUtilities::GetCategoryId("MATCHED") ; 
  }
  else{
    return StEmbeddingQAUtilities::GetCategoryId("PRIMARY") ;
  }
  
}

//____________________________________________________________________________________________________
Int_t StEmbeddingQADraw::GetNDaughters() const
{
  // Number of daughters
  const Int_t particleId = StEmbeddingQAUtilities::GetParticleId(kParticleName);

  return (StEmbeddingQAUtilities::GetNDaughter(particleId)==0) ? 1 : StEmbeddingQAUtilities::GetNDaughter(particleId) ;
}

//____________________________________________________________________________________________________
TObject* StEmbeddingQADraw::GetHistogram(const TString name, const Int_t daughter, const Bool_t isEmbedding)
{
  if( isEmbedding ){
    const Int_t category = GetCategoryId() ;

    TString histoName = (IsDecay()) ? Form("%s_%d_%d", name.Data(), category, daughter)
      : Form("%s_%d", name.Data(), category)
      ;

    return mInputEmbedding->Get(histoName);
  }
  else{
    const Int_t category = GetCategoryId(kFALSE) ;

    return mInputRealData->Get(Form("%s_%d", name.Data(), category));
  }

}

//____________________________________________________________________________________________________
Double_t StEmbeddingQADraw::GetNormalization(const TH1& h) const
{
  const Double_t ntrack   = h.Integral() ;
  if( ntrack == 0 ) return 1.0 ;

  const Double_t binWidth = h.GetBinWidth(1) ;

  return 1.0/(ntrack*binWidth);
}

//____________________________________________________________________________________________________
const Char_t* StEmbeddingQADraw::GetBaseName() const
{
  return Form("%d_%s_%s", kYear, kProduction.Data(), kParticleName.Data());
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawEvent()
{
  cout << "QA for Event-wise informations ..." << endl;

  gStyle->SetOptStat(0);

  //----------------------------------------------------------------------------------------------------
  // Event-wise informations
  TCanvas* canvas = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 1200, 600);
  canvas->Divide(3, 2);

  // z-vertex
  canvas->cd(1);
  TH1* hVz         = (TH1D*) mInputEmbedding->Get("hVz");
  TH1* hVzAccepted = (TH1D*) mInputEmbedding->Get("hVzAccepted");
  SetStyle(hVz);
  SetStyle(hVzAccepted);
  hVzAccepted->SetLineColor(kCyan);
  hVzAccepted->SetFillColor(kCyan);
  hVz->Draw();
  hVzAccepted->Draw("same");
  hVz->Draw("same");

  // y vs x vertices
  canvas->cd(2);
  TH2* hVyVx = (TH2D*) mInputEmbedding->Get("hVyVx");
  SetStyle(hVyVx);
  hVyVx->SetAxisRange(-5, 5, "X");
  hVyVx->SetAxisRange(-5, 5, "Y");
  hVyVx->Draw("colz");

  // dVx, dVy, dVz
  for(Int_t i=0; i<3; i++){
    canvas->cd(i+3);
    TH1* hdV = 0 ;
    if( i == 0 ) hdV = (TH1D*) mInputEmbedding->Get("hdVx");
    if( i == 1 ) hdV = (TH1D*) mInputEmbedding->Get("hdVy");
    if( i == 2 ) hdV = (TH1D*) mInputEmbedding->Get("hdVz");
    SetStyle(hdV);
    hdV->Draw();
  }

  // Statistics
  canvas->cd(6);
  DrawStatistics();

  canvas->cd();
  canvas->Update();

  // Print figures
  Print(*canvas, "eventqa");

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawMcTrack()
{
  cout << "QA for MC tracks (pt, eta, y, phi) ..." << endl;

  gStyle->SetOptStat(0);

  //----------------------------------------------------------------------------------------------------
  // Track-wise informations
  TCanvas* canvas2D = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 800, 600);
  canvas2D->Divide(2, 2);

  // 2D
  canvas2D->cd(1);
  TH2* hPtVsEta = (TH2D*) mInputEmbedding->Get("hPtVsEta_0");
  SetStyle(hPtVsEta);
  hPtVsEta->Draw("colz");
  hPtVsEta->SetAxisRange(0, kPtMax, "Y");

  canvas2D->cd(2);
  TH2* hPtVsY = (TH2D*) mInputEmbedding->Get("hPtVsY_0");
  SetStyle(hPtVsY);
  hPtVsY->Draw("colz");
  hPtVsY->SetAxisRange(0, kPtMax, "Y");

  canvas2D->cd(3);
  TH2* hPtVsPhi = (TH2D*) mInputEmbedding->Get("hPtVsPhi_0");
  SetStyle(hPtVsPhi);
  hPtVsPhi->Draw("colz");
  hPtVsPhi->SetAxisRange(0, kPtMax, "Y");

  // Statistics
  canvas2D->cd(4);
  DrawStatistics();

  canvas2D->cd();
  canvas2D->Update();

  // Print figures
  Print(*canvas2D, "mctrack2d");

  // 1D projections
  TCanvas* canvas = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 800, 600);
  canvas->Divide(2, 2);

  // Projections (pt, eta, y)
  TH1* hPt  = (TH1D*) hPtVsPhi->ProjectionY("hPtMc");
  TH1* hEta = (TH1D*) hPtVsEta->ProjectionX("hEtaMc");
  TH1* hY   = (TH1D*) hPtVsY->ProjectionX("hYMc");
  TH1* hPhi = (TH1D*) hPtVsPhi->ProjectionX("hPhiMc");
  hEta->SetXTitle("#eta, y");

  SetStyle(hPt );
  SetStyle(hEta);
  SetStyle(hY  );
  SetStyle(hPhi);

  hPt ->SetTitle("");
  hEta->SetTitle("");
  hY  ->SetTitle("");
  hPhi->SetTitle("");

  hPt ->Sumw2() ;
  hEta->Sumw2() ;
  hY  ->Sumw2() ;
  hPhi->Sumw2() ;

  hY->SetMarkerStyle(20);
  hEta->SetMarkerStyle(24);
  hPt->SetMaximum(hPt->GetMaximum()*1.2);
  hEta->SetMinimum(0.0);
  hEta->SetMaximum(TMath::Max(hEta->GetMaximum(), hY->GetMaximum())*1.6);
  hPhi->SetMinimum(0.0);
  hPhi->SetMaximum(hPhi->GetMaximum()*1.2);

  canvas->cd(1);
  hPt->SetAxisRange(0, kPtMax, "X");
  hPt->Draw();

  canvas->cd(2);
  hEta->Draw("h");
  hY->Draw("hsame");

  TLegend* leg = new TLegend(0.25, 0.72, 0.51, 0.86);
  leg->SetTextSize(0.07);
  leg->SetBorderSize(1);
  leg->SetFillColor(10);
  leg->AddEntry( hEta, "#eta", "P");
  leg->AddEntry( hY, "y", "P");
  leg->Draw();

  canvas->cd(3);
  hPhi->Draw();

  canvas->cd(4);
  DrawStatistics();

  canvas->cd();
  canvas->Update();

  // Print figures
  Print(*canvas, "mctrack");

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawTrack()
{
  gStyle->SetOptStat(0);

  //----------------------------------------------------------------------------------------------------
  // Track-wise informations (Comparison between MC and reconstructed particles)
  //  1: Geant id
  DrawGeantId();
 
  //  2: (pseudo-)rapidity distributions
  DrawRapidity();
 
  //  3: Momentum and pt
  DrawMomentumAndPt();
 
  //  4: dE/dx
  DrawdEdx();
 
  //  5: Global dca
  DrawDca();
 
  //  6: NHit
  DrawNHit();

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawGeantId()
{
  cout << "QA for geant id ..." << endl;

  gStyle->SetOptStat(0);

  // if IsDecay == kFALSE
  // pad0: MC tracks
  // pad1: Reconstructed tracks
  // pad2: Statistics
  //
  // if IsDecay == kTRUE (NOTE: assume 2 body decay)
  // pad2: Reconstructed tracks (decay daughter 0)
  // pad3: Reconstructed tracks (decay daughter 1)
  // pad4: Statistics
  TCanvas* canvas = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 800, 600);
  if( IsDecay() ) canvas->Divide(2, 2);
  else            canvas->Divide(3, 1);

  // Draw MC tracks
  Int_t ipad = 1 ;
  canvas->cd(ipad++);
  TH1* hGeantIdMc = (TH1D*) mInputEmbedding->Get("hGeantId_0");
  SetStyle(hGeantIdMc);

  // Draw MC id +/- 10
  const Int_t mcIdBin    = hGeantIdMc->GetMaximumBin() ;
  const Double_t mcId    = hGeantIdMc->GetBinCenter(mcIdBin);
  const Double_t mcIdMin = (mcId-10<0) ? 0 : mcId-10 ;
  const Double_t mcIdMax = mcId + 10 ;

  hGeantIdMc->SetMaximum(hGeantIdMc->GetMaximum()*1.2);
  hGeantIdMc->SetAxisRange(mcIdMin, mcIdMax, "X");
  hGeantIdMc->Draw();

  // Draw line at expected particle id
  const Int_t particleId = StEmbeddingQAUtilities::GetParticleId(kParticleName);
  TLine* hGeantIdExpected = new TLine(particleId+0.5, 0, particleId+0.5, hGeantIdMc->GetMaximum()) ; 
  hGeantIdExpected->SetLineColor(kBlue);
  hGeantIdExpected->Draw();

  // Number of daughters
  const Int_t ndaughters = (StEmbeddingQAUtilities::GetNDaughter(particleId)==0) ? 1 : StEmbeddingQAUtilities::GetNDaughter(particleId) ;

  // Reconstructed geantid
  TH1* hGeantIdReco[ndaughters]; 
  Double_t recoIdMax = 0 ;
  Double_t yMax = 0.0 ;
  for(Int_t id=0; id<ndaughters; id++){
    hGeantIdReco[id] = (TH1D*) GetHistogram("hGeantId", id);
    SetStyle(hGeantIdReco[id]);
    hGeantIdReco[id]->SetLineColor(kRed);

    const Int_t recoIdBin = hGeantIdReco[id]->GetMaximumBin() ;
    const Double_t recoId = hGeantIdReco[id]->GetBinCenter(recoIdBin);
    recoIdMax             = TMath::Max(recoIdMax, recoId + 10) ;
    yMax                  = TMath::Max(yMax, hGeantIdReco[id]->GetMaximum());
  }

  for(Int_t id=0; id<ndaughters; id++){
    canvas->cd(ipad++);

    TString title(hGeantIdReco[id]->GetTitle());
    title.Remove(0, title.Last(',')+1);
    hGeantIdReco[id]->SetTitle(title);
    hGeantIdReco[id]->SetMaximum(yMax*1.2);
    hGeantIdReco[id]->SetAxisRange(0, recoIdMax, "X");
    hGeantIdReco[id]->Draw();
  }

  canvas->cd(ipad);
  DrawStatistics();

  canvas->cd();
  canvas->Update();

  // Print figures
  Print(*canvas, "geantid");

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawRapidity()
{
  cout << "QA for (pseudo-)rapidity ..." << endl;

  gStyle->SetOptStat(0);

  for(Int_t id=0; id<GetNDaughters(); id++){
    TCanvas* canvas = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 1200, 400);
    canvas->Divide(3, 1);

    // Embedding
    TH2* hPtVsEtaEmbed = (TH2D*) GetHistogram("hPtVsEta", id);
    TH2* hPtVsYEmbed   = (TH2D*) GetHistogram("hPtVsY", id);
    TH1* hEtaEmbed     = (TH1D*) hPtVsEtaEmbed->ProjectionX(Form("hEtaEmbed_%d", id));
    TH1* hYEmbed       = (TH1D*) hPtVsYEmbed->ProjectionX(Form("hYEmbed_%d", id));
    SetStyle(hEtaEmbed);
    SetStyle(hYEmbed);
    hEtaEmbed->SetLineColor(kRed);
    hYEmbed  ->SetLineColor(kRed);
    hEtaEmbed->Sumw2();
    hYEmbed->Sumw2();
    hEtaEmbed->Scale( GetNormalization(*hEtaEmbed) );
    hYEmbed->Scale( GetNormalization(*hYEmbed) );

    // Real data
    TH2* hPtVsEtaReal = (TH2D*) GetHistogram("hPtVsEta", id, kFALSE);
    TH2* hPtVsYReal   = (TH2D*) GetHistogram("hPtVsY", id, kFALSE);
    TH1* hEtaReal     = (TH1D*) hPtVsEtaReal->ProjectionX(Form("hEtaReal_%d", id));
    TH1* hYReal       = (TH1D*) hPtVsYReal->ProjectionX(Form("hYReal_%d", id));
    SetStyle(hEtaReal);
    SetStyle(hYReal);
    hEtaReal->SetLineColor(kBlue);
    hYReal  ->SetLineColor(kBlue);
    hEtaReal->Scale( GetNormalization(*hEtaReal) );
    hYReal->Scale( GetNormalization(*hYReal) );

    hEtaReal->SetMaximum(TMath::Max(hEtaEmbed->GetMaximum(), hEtaReal->GetMaximum())*1.2) ;
    hYReal->SetMaximum(TMath::Max(hYEmbed->GetMaximum(), hYReal->GetMaximum())*1.2) ;

    // Get particle name
    TString title(hEtaEmbed->GetTitle());
    title.Remove(0, title.Last(',')+1);

    hEtaReal->SetTitle("");
    hYReal->SetTitle("");
    hEtaReal->SetYTitle("(1/N_{trk})dN/d#eta") ;
    hYReal->SetYTitle("(1/N_{trk})dN/dy");

    // pseudo-rapidity
    canvas->cd(1);
    hEtaReal->Draw();
    hEtaEmbed->Draw("same");

    // rapidity
    canvas->cd(2);
    hYReal->Draw();
    hYEmbed->Draw("same");

    canvas->cd(3);

    TLegend* leg = new TLegend(0.1, 0.7, 0.9, 0.9);
    leg->SetFillColor(10);
    leg->SetTextSize(0.05);
    leg->AddEntry( hEtaEmbed, Form("Embedding, %s", title.Data()), "L");
    leg->AddEntry( hEtaReal, "Real data", "L");
    leg->Draw();

    DrawStatistics(0.1, 0.2, 0.9, 0.5);

    canvas->cd();
    canvas->Update();

    if( GetNDaughters() == 1 ){
      Print(*canvas, "eta_and_y");
    }
    else{
      Print(*canvas, Form("eta_and_y_daughter%d", id));
    }
  }

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawMomentumAndPt()
{
  cout << "QA for momentum and pt ..." << endl;

  gStyle->SetOptStat(0);

  for(Int_t id=0; id<GetNDaughters(); id++){
    TCanvas* canvas = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 1200, 400);
    canvas->Divide(3, 1);

    // Embedding
    TH2* hPtVsMomEmbed = (TH2D*) GetHistogram("hPtVsMom", id);
    TH1* hPtEmbed      = (TH1D*) hPtVsMomEmbed->ProjectionY(Form("hPtEmbed_%d", id));
    TH1* hMomEmbed     = (TH1D*) hPtVsMomEmbed->ProjectionX(Form("hMomEmbed_%d", id));
    SetStyle(hPtEmbed);
    SetStyle(hMomEmbed);
    hPtEmbed->SetLineColor(kRed);
    hMomEmbed->SetLineColor(kRed);
    hPtEmbed->Sumw2();
    hMomEmbed->Sumw2();
    hPtEmbed->Scale( GetNormalization(*hPtEmbed) );
    hMomEmbed->Scale( GetNormalization(*hMomEmbed) );

    // Real data
    TH2* hPtVsMomReal = (TH2D*) GetHistogram("hPtVsMom", id, kFALSE);
    TH1* hPtReal      = (TH1D*) hPtVsMomReal->ProjectionY(Form("hPtReal_%d", id));
    TH1* hMomReal     = (TH1D*) hPtVsMomReal->ProjectionX(Form("hMomReal_%d", id));

    SetStyle(hPtReal);
    SetStyle(hMomReal);
    hPtReal->SetLineColor(kBlue);
    hMomReal->SetLineColor(kBlue);
    hPtReal->Scale( GetNormalization(*hPtReal) );
    hMomReal->Scale( GetNormalization(*hMomReal) );

    hPtReal->SetMaximum(TMath::Max(hPtEmbed->GetMaximum(), hPtReal->GetMaximum())*1.2) ;
    hMomReal->SetMaximum(TMath::Max(hMomEmbed->GetMaximum(), hMomReal->GetMaximum())*1.2) ;

    // Get particle name
    TString title(hMomEmbed->GetTitle());
    title.Remove(0, title.Last(',')+1);

    hPtReal->SetTitle("");
    hPtReal->SetYTitle("(1/N_{trk})dN/dp_{T}") ;
    hMomReal->SetTitle("");
    hMomReal->SetYTitle("(1/N_{trk})dN/dp") ;

    // Transverse momentum
    canvas->cd(1);
    hPtReal->SetAxisRange(0, kPtMax, "X");
    hPtEmbed->SetAxisRange(0, kPtMax, "X");

    hPtReal->Draw();
    hPtEmbed->Draw("same");

    // Momentum
    canvas->cd(2);
    hMomReal->SetAxisRange(0, kPtMax, "X");
    hMomEmbed->SetAxisRange(0, kPtMax, "X");
    hMomReal->Draw();
    hMomEmbed->Draw("same");

    canvas->cd(3);
    TLegend* leg = new TLegend(0.1, 0.7, 0.9, 0.9);
    leg->SetFillColor(10);
    leg->SetTextSize(0.05);
    leg->AddEntry( hPtEmbed, Form("Embedding, %s", title.Data()), "L");
    leg->AddEntry( hPtReal, "Real data", "L");
    leg->Draw();

    DrawStatistics(0.1, 0.2, 0.9, 0.5);

    canvas->cd();
    canvas->Update();

    if( GetNDaughters() == 1 ){
      Print(*canvas, "pt_and_mom");
    }
    else{
      Print(*canvas, Form("pt_and_mom_daughter%d", id));
    }
  }


  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawdEdx()
{
  cout << "QA for dE/dx ..." << endl;

  gStyle->SetOptStat(0);

  for(Int_t id=0; id<GetNDaughters(); id++){
    // 2D
    TCanvas* canvas2D = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 1200, 500);
    canvas2D->Divide(2, 1);

    // Embedding
    TH2* hdEdxVsMomEmbed = (TH2D*) GetHistogram("hdEdxVsMom", id);
    SetStyle(hdEdxVsMomEmbed);
    hdEdxVsMomEmbed->SetLineColor(kRed);
    hdEdxVsMomEmbed->SetMarkerColor(kRed);

    // Real data
    TH2* hdEdxVsMomReal = (TH2D*) GetHistogram("hdEdxVsMom", id, kFALSE);
    SetStyle(hdEdxVsMomReal);
    hdEdxVsMomReal->SetLineColor(kBlue);
    hdEdxVsMomReal->SetMarkerColor(kBlue);

    TString title(hdEdxVsMomEmbed->GetTitle());
    title.Remove(0, title.Last(',')+1);
    hdEdxVsMomReal->SetTitle(title);

    // dE/dx vs momentum
    canvas2D->cd(1);
    hdEdxVsMomReal->Draw();
    hdEdxVsMomEmbed->Draw("same");

    canvas2D->cd(2);

    TLegend* leg = new TLegend(0.1, 0.7, 0.9, 0.9);
    leg->SetFillColor(10);
    leg->SetTextSize(0.05);
    leg->AddEntry( hdEdxVsMomEmbed, Form("Embedding, %s", title.Data()), "L");
    leg->AddEntry( hdEdxVsMomReal, "Real data", "L");
    leg->Draw();

    DrawStatistics(0.1, 0.2, 0.9, 0.5);

    canvas2D->cd();
    canvas2D->Update();

    if( GetNDaughters() == 1 ){
      Print(*canvas2D, "dedx_vs_mom");
    }
    else{
      Print(*canvas2D, Form("dedx_vs_mom_daughter%d", id));
    }

    // 1D projections for each momentum bin
    //  From 0.2 GeV/c to 5.0 GeV/c (5*5)
    TCanvas* canvas = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 1200, 800);
    canvas->Divide(5, 5);

    const Int_t npt      = 24 ;
    const Double_t ptBin = 0.2 ;
    for(Int_t ipt=0; ipt<npt; ipt++){
      const Double_t ptMin = 0.2 + ipt*ptBin ;
      const Double_t ptMax = ptMin + ptBin ;
      const Int_t ptMinBin = hdEdxVsMomEmbed->GetXaxis()->FindBin(ptMin);
      const Int_t ptMaxBin = hdEdxVsMomEmbed->GetXaxis()->FindBin(ptMax-0.001);
      if( ptMinBin == ptMaxBin ){
        cout << Form("%1.1f - %1.1f GeV/c : bin = (%4d, %4d)", ptMin, ptMax, ptMinBin, ptMaxBin) << endl;
      }

      // Projections
      TH1* hdEdxEmbed = (TH1D*) hdEdxVsMomEmbed->ProjectionY(Form("hdEdxEmbed_%d_%d", id, ipt), ptMinBin, ptMaxBin);
      TH1* hdEdxReal  = (TH1D*) hdEdxVsMomReal->ProjectionY(Form("hdEdxReal_%d_%d", id, ipt), ptMinBin, ptMaxBin);
      SetStyle(hdEdxEmbed);
      SetStyle(hdEdxReal );
      hdEdxEmbed->Sumw2() ;
      hdEdxReal ->Sumw2() ;
      hdEdxEmbed->Scale( GetNormalization(*hdEdxEmbed) ) ;
      hdEdxReal ->Scale( GetNormalization(*hdEdxReal) ) ;

      hdEdxReal->SetMinimum(0.0);
      hdEdxReal->SetMaximum( TMath::Max(hdEdxReal->GetMaximum(), hdEdxEmbed->GetMaximum())*1.2 );
      hdEdxReal->SetTitle(Form("%1.1f < p_{T} < %1.1f GeV/c", ptMin, ptMax));
      hdEdxReal->SetYTitle("(1/N_{trk})dN/d(dE/dx)");

      canvas->cd(ipt+1);
      hdEdxReal->Draw("h");
      hdEdxEmbed->Draw("hsame");

      // Legend
      if( ipt == npt - 1 ){
        canvas->cd(25);
        TString title(hdEdxVsMomEmbed->GetTitle());
        title.Remove(0, title.Last(',')+1);
        TLegend* leg = new TLegend(0.1, 0.7, 0.9, 0.9);
        leg->SetFillColor(10);
        leg->SetTextSize(0.08);
        leg->AddEntry( hdEdxEmbed, Form("Embedding, %s", title.Data()), "L");
        leg->AddEntry( hdEdxReal,  "Real data", "L");
        leg->Draw();
      
        DrawStatistics(0.1, 0.15, 0.9, 0.55, 0.08);
      }
    }// pt loop

    canvas->cd();
    canvas->Update();

    if( GetNDaughters() == 1 ){
      Print(*canvas, "dedx");
    }
    else{
      Print(*canvas, Form("dedx_daughter%d", id));
    }
  }

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawDca()
{
  cout << "QA for dca distributions ..." << endl;

  return DrawProjection3D("Dca");
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawNHit()
{
  cout << "QA for NHit distributions ..." << endl;

  return DrawProjection3D("NHit") ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawProjection3D(const TString name)
{
  // Plot histograms in each (pt, eta) space
  // Suppose the input histogram is TH3 (x:pt, y:eta, z:variable you want to plot)
  // Force to set batch mode to reduce the processing time

  const Bool_t isBatch = gROOT->IsBatch() ;
  if( !isBatch ){
    cout << "Enter batch mode ..." << endl;
    gROOT->SetBatch(kTRUE);
  }

  TString nameLower(name);
  nameLower.ToLower();

  gStyle->SetOptStat(0);

  TCanvas* canvas = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 600, 800);
  canvas->Divide(2, 3);

  const TString psFileName(Form("%s_%s.pdf", nameLower.Data(), GetBaseName()));
  TPDF* pdf = new TPDF(psFileName);
//  pdf->NewPage();

  const Int_t npad = 5 ;

  for(Int_t id=0; id<GetNDaughters(); id++){
    TH3* h3DEmbed = (TH3D*) GetHistogram(Form("h%s", name.Data()), id);
    TH3* h3DReal  = (TH3D*) GetHistogram(Form("h%s", name.Data()), id, kFALSE);

    const Int_t nPt       = h3DEmbed->GetNbinsX() ;
    const Int_t nEta      = h3DEmbed->GetNbinsY() ;
    const Double_t ptMin  = h3DEmbed->GetXaxis()->GetXmin() ;
    const Double_t ptMax  = h3DEmbed->GetXaxis()->GetXmax() ;
    const Double_t etaMin = h3DEmbed->GetYaxis()->GetXmin() ;
    const Double_t etaMax = h3DEmbed->GetYaxis()->GetXmax() ;
    const Double_t etaBin = (etaMax-etaMin)/(Double_t)nEta ;
    const Double_t ptBin  = (ptMax-ptMin)/(Double_t)nPt ;

    for(Int_t ipt=0; ipt<nPt; ipt++){
      TString pt(Form("%1.1f < p_{T} < %1.1f (GeV/c)", ptMin+ipt*ptBin, ptMin+(ipt+1)*ptBin));
      if( ipt == 0 ) pt = Form("%1.1f < p_{T} < %1.1f (GeV/c)", 0.1, ptMin+(ipt+1)*ptBin);

      Int_t ipad = 1 ;
      for(Int_t ieta=0; ieta<nEta; ieta++){
        if( ipad != 1 && ipad % (npad+1) == 0 ){
          canvas->cd();
          canvas->Update();
          pdf->NewPage();
          ipad = 1 ;
        }

        TString eta(Form("%1.1f < #eta < %1.1f", etaMin+ieta*etaBin, etaMin+(ieta+1)*etaBin));

        TH1* hEmbed = (TH1D*) h3DEmbed->ProjectionZ(Form("h%sEmbed_%d_%d_%d", name.Data(), id, ipt, ieta), ipt+1, ipt+1, ieta+1, ieta+1);
        TH1* hReal  = (TH1D*) h3DReal->ProjectionZ(Form("h%sReal_%d_%d_%d", name.Data(), id, ipt, ieta), ipt+1, ipt+1, ieta+1, ieta+1);
        SetStyle(hEmbed);
        SetStyle(hReal );
        hEmbed->Sumw2();
        hReal ->Sumw2();
        hEmbed->Scale( GetNormalization(*hEmbed) );
        hReal ->Scale( GetNormalization(*hReal) );
        hEmbed->SetLineColor(kRed);
        hReal ->SetLineColor(kBlue);

        hReal->SetMinimum(0.0);
        hReal->SetMaximum( TMath::Max(hEmbed->GetMaximum(), hReal->GetMaximum()) * 1.2 );
        hReal->SetTitle(eta + ", " + pt);
        hReal->SetYTitle(Form("(1/N_{trk})dN/d%s", name.Data())) ;

        canvas->cd(ipad++);
        hReal->Draw("h");
        hEmbed->Draw("same");

        if( ipad % (npad+1) == 0 ){
          canvas->cd(6);
          TString title(h3DEmbed->GetTitle());
          title.Remove(0, title.Last(',')+1);
          TLegend* leg = new TLegend(0.1, 0.7, 0.9, 0.9);
          leg->SetFillColor(10);
          leg->SetTextSize(0.05);
          leg->AddEntry( hEmbed, Form("Embedding, %s", title.Data()), "L");
          leg->AddEntry( hReal,  "Real data", "L");
          leg->Draw();
  
          DrawStatistics(0.1, 0.2, 0.9, 0.5);
        }
      }
      canvas->cd();
      canvas->Update();
      pdf->NewPage();
    }
  }
  pdf->Close();
  cout << "Print PDF file : " << psFileName << endl;

  // Back to the normal
  if( isBatch ) gROOT->SetBatch(kTRUE);  // Stay batch mode if you've started the macro by batch mode
  else          gROOT->SetBatch(kFALSE); // Exit batch mode if you've not

  return kTRUE ;
}


//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::Draw()
{
  gStyle->SetOptStat(0);

  //====================================================================================================
  //
  // Start drawing
  //
  //====================================================================================================

  //----------------------------------------------------------------------------------------------------
  // (1) Event-wise informations
  DrawEvent();
  //----------------------------------------------------------------------------------------------------

  //----------------------------------------------------------------------------------------------------
  // (2-1) Track-wise informations (MC)
  DrawMcTrack();
  //----------------------------------------------------------------------------------------------------

  //----------------------------------------------------------------------------------------------------
  // (2-2) Track-wise informations (Real vs Embedding)
  DrawTrack();
  //----------------------------------------------------------------------------------------------------

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawStatistics(const Double_t x1, const Double_t y1, const Double_t x2, const Double_t y2,
    const Double_t textSize)
{
  TPaveText* statistics = new TPaveText(x1, y1, x2, y2);
  statistics->SetTextFont(42);
  statistics->SetTextSize(textSize);
  statistics->SetBorderSize(1);
  statistics->SetFillColor(10);
  statistics->AddText(Form("N_{evts} = %d", GetEntries()));
  statistics->AddText(Form("Year: %d", kYear));
  statistics->AddText(Form("Production: %s", kProduction.Data()));
  const Int_t particleId = StEmbeddingQAUtilities::GetParticleId(kParticleName.Data()) ;
  statistics->AddText(Form("Particle: %s", StEmbeddingQAUtilities::GetParticleName(particleId).Data()));
  statistics->Draw();

  return kTRUE ;
}


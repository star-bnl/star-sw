
#include <iostream>

#include "TCanvas.h"
#include "TError.h"
#include "TF1.h"
#include "TFile.h"
#include "TGraphErrors.h"
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
  const Int_t category = GetCategoryId(isEmbedding) ;

  if( isEmbedding ){
    return (IsDecay()) ? mInputEmbedding->Get(Form("%s_%d_%d", name.Data(), category, daughter))
      : mInputEmbedding->Get(Form("%s_%d", name.Data(), category))
      ;
  }
  else{
    return (IsDecay()) ?  mInputRealData->Get(Form("%s_%d_%d", name.Data(), category, daughter)) 
      : mInputRealData->Get(Form("%s_%d", name.Data(), category))
      ;
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
  DrawMomentum();
  DrawPt();
 
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
  // Pseudo-rapidity
  DrawProjection2D("eta");

  // Rapidity
  DrawProjection2D("y");

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawMomentum()
{
  return DrawProjection2D("momentum");
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawPt()
{
  return DrawProjection2D("pt");
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawProjection2D(const TString name)
{
  // Input 2D histogram is pt or momentum vs eta or y
  //
  // Projection into eta or y for pt = 0.2 - 5 GeV/c (0.5 GeV/c step)
  // or
  // Projection into pt or momentum for |Delta eta| = 0.5 in |eta| < 2

  TString nameLower(name);
  nameLower.ToLower();

  cout << "QA for " << name << " ..." << endl;

  TString histoName("");
  Bool_t isProjectionX = kFALSE ;

  TString yTitle("");

  if( nameLower.Contains("pt") ){
    yTitle = "p_{T}";
    histoName = "hPtVsEta";
    isProjectionX = kFALSE ;
  }
  else if( nameLower.Contains("momentum") ){
    yTitle = "p";
    histoName = "hMomVsEta";
    isProjectionX = kFALSE ;
  }
  else if( nameLower.Contains("eta") || name.Contains("pseudorapidity") ){
    yTitle = "#eta";
    histoName = "hPtVsEta";
    isProjectionX = kTRUE ;
  }
  else if( nameLower.Contains("y") || name.Contains("rapidity") ){
    yTitle = "y";
    histoName = "hPtVsY";
    isProjectionX = kTRUE ;
  }
  else{
    Error("DrawProjection2D", "Unknown variable, %s", name.Data());
    cout << "  Current implemented variables are" << endl;
    cout << "-----------------------------------------------------------------" << endl;
    cout << "   Input                       variable" << endl;
    cout << "-----------------------------------------------------------------" << endl;
    cout << "   pt                           p_{T}" << endl;
    cout << "   momentum                     p" << endl;
    cout << "   eta or pseudorapidity        eta" << endl;
    cout << "   y or rapidity                y" << endl;
    cout << "-----------------------------------------------------------------" << endl;
    cout << endl;
    cout << "NOTE : Input is case insensitive" << endl;
    cout << endl;

    return kFALSE ;
  }

  gStyle->SetOptStat(0);
  gStyle->SetPadRightMargin(0.05);

  for(Int_t id=0; id<GetNDaughters(); id++){
    TH2* h2DEmbed = (TH2D*) GetHistogram(histoName, id);
    TH2* h2DReal  = (TH2D*) GetHistogram(histoName, id, kFALSE);

    Int_t npad = 0 ;
    TCanvas* canvas = 0;
    if( isProjectionX ){
      canvas = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 1200, 800);
      canvas->Divide(4, 3);
      npad = 12 ;
    }
    else{
      canvas = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 1200, 600);
      canvas->Divide(4, 2);
      npad = 8 ;
    }

    // Get bins
    // eta bins = 6 : |eta| < 1.5
    // pt bins  = 10 : pt = 0.2 - 5 GeV/c
    const Int_t nbins = (isProjectionX) ? 10 : 6 ;
    const Double_t binStep = 0.5 ;
    const Double_t binMin  = (isProjectionX) ? 0.0 : -1.5 ;

    for(Int_t ibin=0; ibin<nbins; ibin++){
      const Double_t xMin = (isProjectionX && ibin==0) ? 0.2 : binMin + binStep * ibin ;
      const Double_t xMax = (isProjectionX && ibin==0) ? 0.5 : binMin + binStep * (ibin+1.0) ;
      const Int_t xMinBin = (isProjectionX) ? h2DEmbed->GetYaxis()->FindBin(xMin)     : h2DEmbed->GetXaxis()->FindBin(xMin) ;
      const Int_t xMaxBin = (isProjectionX) ? h2DEmbed->GetYaxis()->FindBin(xMax) - 1 : h2DEmbed->GetXaxis()->FindBin(xMax) - 1;

      TH1* hEmbed = 0;
      TH1* hReal  = 0;
      if( isProjectionX ){
        hEmbed = (TH1D*) h2DEmbed->ProjectionX(Form("h%sEmbed_%d_%d", name.Data(), id, ibin), xMinBin, xMaxBin);
        hReal  = (TH1D*) h2DReal->ProjectionX(Form("h%sReal_%d_%d", name.Data(), id, ibin), xMinBin, xMaxBin);
      }
      else{
        hEmbed = (TH1D*) h2DEmbed->ProjectionY(Form("h%sEmbed_%d_%d", name.Data(), id, ibin), xMinBin, xMaxBin);
        hReal  = (TH1D*) h2DReal->ProjectionY(Form("h%sReal_%d_%d", name.Data(), id, ibin), xMinBin, xMaxBin);
      }

      SetStyle(hEmbed);
      SetStyle(hReal);
      hEmbed->SetLineColor(kRed);
      hReal->SetLineColor(kBlue);
      hEmbed->Sumw2();
      hReal->Sumw2();
      hEmbed->Scale( GetNormalization(*hEmbed) );
      hReal->Scale( GetNormalization(*hReal) );

      // Set maximum
      Double_t yMax = 0.0 ;
      if ( hEmbed->GetEntries() < 100 ){
        // Use real data if N(embedding) < 100
        yMax = hReal->GetMaximum() ;
      }
      else{
        yMax = TMath::Max(hEmbed->GetMaximum(), hReal->GetMaximum())*1.2 ;
      }
      hReal->SetMaximum( yMax * 1.2 );

      if( isProjectionX ){
        hReal->SetTitle(Form("%1.1f < p_{T} < %1.1f (GeV/c)", xMin, xMax));
      }
      else{
        hReal->SetTitle(Form("%1.1f < #eta < %1.1f", xMin, xMax));
        hReal->SetAxisRange(0, kPtMax, "X");
      }
      hReal->SetYTitle(Form("(1/N_{trk})dN/%s", yTitle.Data()));

      canvas->cd(ibin+1);
      hReal->Draw("h");
      if( hEmbed->GetEntries() >= 100 ) hEmbed->Draw("same");

      if( ibin == 0 ){
        // Get particle name
        TString title(hEmbed->GetTitle());
        title.Remove(0, title.Last(',')+1);

        canvas->cd(npad);
        TLegend* leg = new TLegend(0.1, 0.7, 0.9, 0.9);
        leg->SetFillColor(10);
        leg->SetTextSize(0.05);
        leg->AddEntry( hEmbed, Form("Embedding, %s", title.Data()), "L");
        leg->AddEntry( hReal, "Real data", "L");
        leg->Draw();

        DrawStatistics(0.1, 0.2, 0.9, 0.5);
      }
    }

    canvas->cd();
    canvas->Update();

    if( GetNDaughters() == 1 ){
      Print(*canvas, Form("%s", nameLower.Data()));
    }
    else{
      Print(*canvas, Form("%s_daughter%d", nameLower.Data(), id));
    }
  }

  gStyle->SetPadRightMargin(0.15);

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::DrawdEdx()
{
  cout << "QA for dE/dx ..." << endl;

  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0);
  gStyle->SetPadRightMargin(0.05);

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
    hdEdxVsMomReal->SetLineColor(kBlack);
    hdEdxVsMomReal->SetMarkerColor(kBlack);

    // Real data (with PID)
    TH2* hdEdxVsMomPidReal = (TH2D*) GetHistogram("hdEdxVsMomPidCut", id, kFALSE);
    SetStyle(hdEdxVsMomPidReal);
    hdEdxVsMomPidReal->SetLineColor(kBlue);
    hdEdxVsMomPidReal->SetMarkerColor(kBlue);

    TString title(hdEdxVsMomEmbed->GetTitle());
    title.Remove(0, title.Last(',')+1);
    hdEdxVsMomReal->SetTitle(title);

    // dE/dx vs momentum
    canvas2D->cd(1);
    hdEdxVsMomReal->SetAxisRange(0, kPtMax, "X");

    hdEdxVsMomReal->Draw();
    hdEdxVsMomPidReal->Draw("same");
    hdEdxVsMomEmbed->Draw("same");

    canvas2D->cd(2);

    TLegend* leg = new TLegend(0.1, 0.7, 0.9, 0.9);
    leg->SetFillColor(10);
    leg->SetTextSize(0.05);
    leg->AddEntry( hdEdxVsMomEmbed, Form("Embedding, %s", title.Data()), "L");
    leg->AddEntry( hdEdxVsMomReal, "Real data", "L");
    leg->AddEntry( hdEdxVsMomPidReal, "Real data with PID cut (#sigma<2)", "L");
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
    TCanvas* canvas0 = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 1200, 800);
    canvas0->Divide(5, 5);

    TGraphErrors* gMeanVsMom[2];  // 0:embedding, 1:real data
    TGraphErrors* gSigmaVsMom[2]; // 0:embedding, 1:real data

    for(Int_t i=0; i<2; i++){
      gMeanVsMom[i]  = new TGraphErrors();
      gSigmaVsMom[i] = new TGraphErrors();
      gMeanVsMom[i] ->SetMarkerStyle(24 - i*4);
      gSigmaVsMom[i]->SetMarkerStyle(24 - i*4);
    }
    gMeanVsMom[0] ->SetMarkerColor(kRed);    gMeanVsMom[0] ->SetLineColor(kRed);
    gSigmaVsMom[0]->SetMarkerColor(kRed);    gSigmaVsMom[0]->SetLineColor(kRed);
    gMeanVsMom[1] ->SetMarkerColor(kBlue);   gMeanVsMom[1] ->SetLineColor(kBlue);
    gSigmaVsMom[1]->SetMarkerColor(kBlue);   gSigmaVsMom[1]->SetLineColor(kBlue);

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
      TH1* hdEdxReal  = (TH1D*) hdEdxVsMomPidReal->ProjectionY(Form("hdEdxReal_%d_%d", id, ipt), ptMinBin, ptMaxBin);
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

      canvas0->cd(ipt+1);
      hdEdxReal->Draw("h");
      hdEdxEmbed->Draw("hsame");

      // Extract mean and sigma (Oct/22/2009)
      TF1 fEmbed(Form("fEmbed_%d_%d", id, ipt), "gaus", 0, 10);
      TF1 fReal(Form("fReal_%d_%d", id, ipt), "gaus", 0, 10);
      fEmbed.SetLineColor(kRed);
      fReal.SetLineColor(kBlue);
      fEmbed.SetLineWidth(1);
      fReal.SetLineWidth(1);

      hdEdxReal->Fit(fReal.GetName(), "rq0");
      hdEdxEmbed->Fit(fEmbed.GetName(), "rq0");
      fReal.Draw("same");
      fEmbed.Draw("same");

      const Double_t pt = (ptMin+ptMax)/2.0 ;
      gMeanVsMom[0]->SetPoint(ipt, pt, fEmbed.GetParameter(1));
      gMeanVsMom[0]->SetPointError(ipt, 0.0, fEmbed.GetParError(1));
      gMeanVsMom[1]->SetPoint(ipt, pt, fReal.GetParameter(1));
      gMeanVsMom[1]->SetPointError(ipt, 0.0, fReal.GetParError(1));
      gSigmaVsMom[0]->SetPoint(ipt, pt, fEmbed.GetParameter(2));
      gSigmaVsMom[0]->SetPointError(ipt, 0.0, fEmbed.GetParError(2));
      gSigmaVsMom[1]->SetPoint(ipt, pt, fReal.GetParameter(2));
      gSigmaVsMom[1]->SetPointError(ipt, 0.0, fReal.GetParError(2));

      // Legend
      if( ipt == npt - 1 ){
        canvas0->cd(25);
        TString title(hdEdxVsMomEmbed->GetTitle());
        title.Remove(0, title.Last(',')+1);
        TLegend* leg = new TLegend(0.1, 0.7, 0.9, 0.9);
        leg->SetFillColor(10);
        leg->SetTextSize(0.08);
        leg->SetHeader(title.Data());
        leg->AddEntry( hdEdxEmbed, "Embedding", "L");
        leg->AddEntry( hdEdxReal,  "Real data", "L");
        leg->Draw();
      
        DrawStatistics(0.1, 0.15, 0.9, 0.55, 0.08);
      }
    }// pt loop

    canvas0->cd();
    canvas0->Update();

    if( GetNDaughters() == 1 ){
      Print(*canvas0, "dedx");
    }
    else{
      Print(*canvas0, Form("dedx_daughter%d", id));
    }

    // Mean/Sigma vs momentum (real vs embed)
    TCanvas* canvas1 = new TCanvas(Form("c%d", kCanvasId), Form("c%d", kCanvasId++), 1200, 500);
    canvas1->Divide(2, 1);

    for(Int_t i=0; i<2; i++){
      canvas1->cd(i+1);

      const Double_t ymax = (i==0) ? 5.2 : 1.4 ;
      TH1* frame = canvas1->GetPad(i+1)->DrawFrame(0, 0.0, 5.0, ymax);
      frame->SetXTitle("momentum (GeV/c)");
      if( i == 0 ) frame->SetYTitle("Mean (KeV/cm)");
      if( i == 1 ) frame->SetYTitle("#sigma (KeV/cm)");

      TLegend* leg = new TLegend(0.55, 0.7, 0.88, 0.86);
      leg->SetTextSize(0.05);
      leg->SetFillColor(10);
      TString title(hdEdxVsMomEmbed->GetTitle());
      title.Remove(0, title.Last(',')+1);
      leg->SetHeader(title);

      if( i == 0 ){
        gMeanVsMom[0]->Draw("P");
        gMeanVsMom[1]->Draw("P");

        leg->AddEntry( gMeanVsMom[0], "Embedding", "P");
        leg->AddEntry( gMeanVsMom[1], "Real data", "P");
      }
      if( i == 1 ){
        gSigmaVsMom[0]->Draw("P");
        gSigmaVsMom[1]->Draw("P");

        leg->AddEntry( gSigmaVsMom[0], "Embedding", "P");
        leg->AddEntry( gSigmaVsMom[1], "Real data", "P");
      }
      leg->Draw();
    }

    canvas1->cd();
    canvas1->Update();

    if( GetNDaughters() == 1 ){
      Print(*canvas1, "mean_sigma_dedx");
    }
    else{
      Print(*canvas1, Form("mean_sigma_dedx_daughter%d", id));
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


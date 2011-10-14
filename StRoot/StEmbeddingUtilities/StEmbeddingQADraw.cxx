/****************************************************************************************************
 * $Id: StEmbeddingQADraw.cxx,v 1.7 2009/12/22 21:40:09 hmasui Exp $
 * $Log: StEmbeddingQADraw.cxx,v $
 * Revision 1.7  2009/12/22 21:40:09  hmasui
 * Add comments for functions and members
 *
 ****************************************************************************************************/

#include <assert.h>
#include <string>

#include "TCanvas.h"
#include "TError.h"
#include "TF1.h"
#include "TFile.h"
#include "TGraphErrors.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TLegend.h"
#include "TMath.h"
#include "TObject.h"
#include "TPaveText.h"
#include "TPDF.h"
#include "TROOT.h"
#include "TStyle.h"
#include "TSystem.h"

#include "StEmbeddingQADraw.h"
#include "StEmbeddingQAUtilities.h"
#include "StMessMgr.h"
#include "StParticleDefinition.hh"
#include "StParticleTable.hh"

using namespace std ;

ClassImp(StEmbeddingQADraw)

  /// Initialize static data members
  UInt_t StEmbeddingQADraw::mCanvasId = 0;

//____________________________________________________________________________________________________
StEmbeddingQADraw::StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile, const Int_t geantid)
  : mIsGIFOn(kFALSE), mIsJPGOn(kFALSE), mIsEPSOn(kFALSE), mIsPSOn(kFALSE), mGeantId(geantid)
{
  /// Constructor for StEmbeddingQADraw

  /// Open input files, see Bool_t open(const TString embeddingFile, const TString realDataFile)
  mIsOpen = open(embeddingFile, realDataFile);

  /// Get year and production from input file
  if(mIsOpen){
    TString fileName(embeddingFile);
    fileName.Remove(fileName.Index(".root"), fileName.Length()); // remove .root
 
    for(Int_t i=0;i<3;i++){
      const Int_t start = 0 ;
      const Int_t stop  = fileName.First("_") ;
      const TString subString(fileName(start, stop));
 
      fileName.Remove(start, stop+1);
 
      if( i == 2 ){
        mYear = subString.Atoi(); // Convert character to integer
        mProduction = fileName ;
      }
    }
  }
  else{
    /// Do not define the year and production if input files have not been opened properly
    mYear = -10 ;
    mProduction = "";
  }

  init() ;
}

//____________________________________________________________________________________________________
StEmbeddingQADraw::StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile,
    const Int_t year, const TString production, const Int_t geantid)
  : mIsGIFOn(kFALSE), mIsJPGOn(kFALSE), mIsEPSOn(kFALSE), mIsPSOn(kFALSE), mGeantId(geantid)
{
  /// Constructor for StEmbeddingQADraw
  /// Define year and production from the input arguments

  /// Open input files, see Bool_t open(const TString embeddingFile, const TString realDataFile)
  mIsOpen = open(embeddingFile, realDataFile);

  /// Year, production and particle name by hand
  mYear         = year ;
  mProduction   = production ;

  init();
}

//____________________________________________________________________________________________________
StEmbeddingQADraw::~StEmbeddingQADraw()
{
  /// Destructor

  /// Clear daughter geantid array
  mDaughterGeantId.clear();

  /// Close input files
  mInputEmbedding->Close();
  mInputRealData->Close();
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::open(const TString embeddingFile, const TString realDataFile)
{
  /// Open input embedding and real data files

  // OPEN embedding file
  mInputEmbedding = TFile::Open(embeddingFile);

  if(!mInputEmbedding || !mInputEmbedding->IsOpen() || mInputEmbedding->IsZombie() ){
    Error("StEmbeddingQADraw", "can't find %s", embeddingFile.Data());
    return kFALSE;
  }
  LOG_INFO << "OPEN input (embedding) : " << mInputEmbedding->GetName() << endm;
  LOG_INFO << endm;

  // OPEN real data file
  mInputRealData = TFile::Open(realDataFile);

  if(!mInputRealData || !mInputRealData->IsOpen() || mInputRealData->IsZombie() ){
    Error("StEmbeddingQADraw", "can't find %s", realDataFile.Data());
    return kFALSE;
  }
  LOG_INFO << "OPEN input (real data) : " << mInputRealData->GetName() << endm;
  LOG_INFO << endm;

  return kTRUE ;
}


//____________________________________________________________________________________________________
void StEmbeddingQADraw::init()
{
  /// Initialization of data members

  /// Set maximum pt. Default is 5 GeV/c --> Draw histograms up to 5 GeV/c
  mPtMax = 5.0;

  /// Clear daughter geantid array
  mDaughterGeantId.clear();

  LOG_INFO << "#------------------------------------------------------------" << endm;
  LOG_INFO << Form("  Year       =  %10d", mYear) << endm;
  LOG_INFO << Form("  Production =  %10s", mProduction.Data()) << endm;
  LOG_INFO << Form("  Particle   =  %10s", getParticleName()) << endm;
  LOG_INFO << "#------------------------------------------------------------" << endm;

  /// set global ROOT styles
  StEmbeddingQAUtilities::instance()->setStyle();

  /// Make sure that we have histograms for the input geant id
  checkInputGeantId() ;

  /// Find daughter geant id from Contaminated pair histograms 
  setDaughterGeantId();
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::setOutputDirectory(const TString name)
{
  /// Set output directory

  mOutputFigureDirectory = name ;

  /// Make sure that the directory exists or not
  if( gSystem->AccessPathName(name) == kTRUE ){ // 0 is true, i.e. directory exists
    Error("setOutputDirectory", "Directory %s does not exist. Set current directory as the output location");
    mOutputFigureDirectory = "./";
  }

  /// Make sure you put '/' at the end of directory name. If not, add '/'
  if ( mOutputFigureDirectory.Last('/') + 1 != mOutputFigureDirectory.Length() ){
    LOG_INFO << endm;
    LOG_INFO << "Put / at the end of output directory name" << endm;
    mOutputFigureDirectory.Append("/");
  }

  LOG_INFO << "Set output directory for figures : " << mOutputFigureDirectory << endm;
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::print(const TCanvas& canvas, const TString name) const
{
  /// Print output figure (default is png only)
  /// You can print other file formants, like gif etc by setGIFOn()
  /// See header for other file formants.

  canvas.Print(Form("%s%s_%s.png", mOutputFigureDirectory.Data(), name.Data(), getBaseName()));
  if( mIsGIFOn ) canvas.Print(Form("%s%s_%s.gif", mOutputFigureDirectory.Data(), name.Data(), getBaseName()));
  if( mIsJPGOn ) canvas.Print(Form("%s%s_%s.jpg", mOutputFigureDirectory.Data(), name.Data(), getBaseName()));
  if( mIsEPSOn ) canvas.Print(Form("%s%s_%s.eps", mOutputFigureDirectory.Data(), name.Data(), getBaseName()));
  if( mIsPSOn )  canvas.Print(Form("%s%s_%s.ps", mOutputFigureDirectory.Data(), name.Data(), getBaseName()));
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::checkInputGeantId()
{
  /// Check input geant id
  ///  if we don't have histograms for the input geantid put the geantid from the MC histogram

  TH1* hGeantId = (TH1D*) getHistogram("hGeantId_0");

  /// mGeantId = -1 if geantid histogram doesn't exist
  if(!hGeantId){
    mGeantId = -1 ;
    return ;
  }

  const Int_t mcIdBin      = hGeantId->GetMaximumBin() ;
  const Int_t geantidFound = TMath::Nint(hGeantId->GetBinCenter(mcIdBin));

  if ( mGeantId == geantidFound ) return ; /// do nothing if input geantid is correct
  else{
    LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
    LOG_INFO << "  Input geant id doesn't correspond to the geant id in the MC histogram" << endm ;
    LOG_INFO << "  Use geantid in the MC histogram, geantid = " << geantidFound
             << ",  particle name = " << StParticleTable::instance()->findParticleByGeantId(geantidFound)->name().c_str() << endm;
    LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
    mGeantId = geantidFound ; 
  }
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::isOpen() const
{
  /// Check if
  ///  - input files (both embedding and real data) are found
  ///  - input geantid is correct
  ///
  ///  Stop program by assert if one of them is false

  if(!mIsOpen) LOG_INFO << "No input files found" << endm;
  assert(mIsOpen);

  const Bool_t isGeantIdOk = mGeantId>0 ;
  if(!isGeantIdOk) LOG_INFO << "Cannot find input geantid in the histogram or no histogram in the input" << endm;
  assert(isGeantIdOk);

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::isMatchedPairOk() const
{
  /// Check # of matched pairs > 0
  TH1* hGeantId = (TH1D*) getHistogram("hGeantId_1");
  if(!hGeantId) return kFALSE ;

  return (hGeantId->GetEntries()>0) ;
}


//____________________________________________________________________________________________________
void StEmbeddingQADraw::setDaughterGeantId()
{
  /// Find daughter geantid from histogram

  /// Check # of matched pairs > 0
  if(isMatchedPairOk()){
    /// Use matched pairs, not contaminated pairs. Set input geantid (mGeantid) into daughter geantid array
    mDaughterGeantId.push_back(mGeantId);
    return;
  }

  /// Try to find out the daughter geantid from the histogram
  const StParticleTable* table = StParticleTable::instance() ;

  for(UInt_t id=0; id<1000; id++){
    const Int_t contamCategoryId = StEmbeddingQAUtilities::instance()->getCategoryId("CONTAM") ;
    TH3* hDca = (TH3D*) getHistogram(Form("hDca_%d_%d_%d", contamCategoryId, mGeantId, id));

    if ( hDca ){
      const Char_t* daughterName = table->findParticleByGeantId(id)->name().c_str() ;
      const Char_t* parentName   = table->findParticleByGeantId(mGeantId)->name().c_str() ; 

      LOG_INFO << Form("Find daughter %10s from parent %10s", daughterName, parentName) << endm;

      mDaughterGeantId.push_back(id);
    }
  }

  /// If no daughter particles, set mGeantId
  if ( mDaughterGeantId.empty() ) mDaughterGeantId.push_back(mGeantId);
}

//____________________________________________________________________________________________________
Int_t StEmbeddingQADraw::getEntries() const
{
  /// Get number of events from the z-vertex histogram

  TH1* hVz = (TH1D*) getHistogram("hVz");
  if(!hVz) return 0;

  return (Int_t)( hVz->GetEntries() );
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::isDecay() const
{
  /// Check whether we have decay daughters in our QA histograms

  /// Check matched pairs
  /// Matched pairs > 0, i.e. don't need to look at the contaminated pairs
  if(isMatchedPairOk()) return kFALSE ;

  /// Number of decay daughters should be >= 2
  return ( mDaughterGeantId.size() > 1 ) ;
}

//____________________________________________________________________________________________________
Int_t StEmbeddingQADraw::getGeantIdReal(const Int_t daughter) const
{
  /// Get daughter particle id for the real data
  ///   if daughters are not e/pi/K/p, return pi+ (geantid=8)

  return ( StEmbeddingQAUtilities::instance()->isEPiKP(mDaughterGeantId[daughter]) ) ? mDaughterGeantId[daughter] : 8 ;
}

//____________________________________________________________________________________________________
Int_t StEmbeddingQADraw::getCategoryId(const Bool_t isEmbedding) const
{
  /// For embedding QA
  ///   isDecay = kTRUE  --> Contaminated pairs
  ///   isDecay = kFALSE --> Matched pairs
  /// For real data QA
  ///   Use primary track
  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;

  if ( isEmbedding ){
    return (isDecay()) ? utility->getCategoryId("CONTAM") : utility->getCategoryId("MATCHED") ; 
  }
  else{
    return utility->getCategoryId("PRIMARY") ;
  }
}

//____________________________________________________________________________________________________
TObject* StEmbeddingQADraw::getHistogram(const TString name, const Bool_t isEmbedding) const
{
  /// Get histogram from either embedding or real data ROOT file

  if(!isOpen()) return 0;

  TObject* obj = (isEmbedding) ? mInputEmbedding->Get(name) : mInputRealData->Get(name) ;
  if ( !obj ){
    Error("StEmbeddingQADraw::getHistogram", "Histogram %s doesn't exist", name.Data());
    return 0;
  }

  LOG_DEBUG << "StEmbeddingQADraw::getHistogram()  get histogram = " << name << endm;

  return obj ;
}


//____________________________________________________________________________________________________
TObject* StEmbeddingQADraw::getHistogram(const TString name, const UInt_t daughter, const Bool_t isEmbedding) const
{
  /// Get histogram from either embedding or real data ROOT file
  /// Define histogram name from daughter number and category id

  if ( daughter >= mDaughterGeantId.size() ){
    Error("StEmbeddingQADraw::getHistogram", "Unknown daughter index, index=%3d", daughter);
    return 0;
  }

  const Int_t category = getCategoryId(isEmbedding) ;

  if( isEmbedding ){
    /// Histogram name is 
    ///   - {histogram name}_{category id}_{particle id} for stable particles
    ///   - {histogram name}_{category id}_{parent particle id}_{daughter particle id} for unstable particles
    return (isDecay()) ? getHistogram(Form("%s_%d_%d_%d", name.Data(), category, mGeantId, mDaughterGeantId[daughter])) 
      : getHistogram(Form("%s_%d_%d", name.Data(), category, mGeantId)) ;
  }
  else{
    /// Only primary particles in the real data
    ///   - If mDaughterGeantId is not e/pi/K/p, use pi+
    ///   NOTE: mDaughterGeantId[0] = mGeantId for stable particles
    const Int_t geantidReal = getGeantIdReal(daughter) ;

    return getHistogram(Form("%s_%d_%d", name.Data(), category, geantidReal), kFALSE) ;
  }
}

//____________________________________________________________________________________________________
Double_t StEmbeddingQADraw::getNormalization(const TH1& h) const
{
  /// Get normalization of histogram
  /// Normalization = 1/(Nevts * binWidth)

  const Double_t ntrack   = h.Integral() ;
  if( ntrack == 0 ) return 1.0 ;

  const Double_t binWidth = h.GetBinWidth(1) ;
  if( binWidth == 0.0 ) return 1.0 ;

  return 1.0/(ntrack*binWidth);
}

//____________________________________________________________________________________________________
const Char_t* StEmbeddingQADraw::getBaseName() const
{
  /// Get {year}_{production}_{particle name}

  return Form("%d_%s_%s", mYear, mProduction.Data(), getParticleName());
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawStatistics(const Double_t x1, const Double_t y1, const Double_t x2, const Double_t y2,
    const Double_t textSize) const
{
  /// Print
  ///  - number of events
  ///  - Year
  ///  - Production
  ///  - Particle name

  TPaveText* statistics = new TPaveText(x1, y1, x2, y2);
  statistics->SetTextFont(42);
  statistics->SetTextSize(textSize);
  statistics->SetBorderSize(1);
  statistics->SetFillColor(10);
  statistics->AddText(Form("N_{evts} = %d", getEntries()));
  statistics->AddText(Form("Year: %d", mYear));
  statistics->AddText(Form("Production: %s", mProduction.Data()));
  statistics->AddText(Form("Particle: %s", getParticleName()));
  statistics->Draw();

  return kTRUE ;
}

//____________________________________________________________________________________________________
const Char_t* StEmbeddingQADraw::getParticleName(const Int_t geantid) const
{
  /// Get particle name

  /// Default input geantid = -1 --> return particle name for mGeantid
  const StParticleTable* table = StParticleTable::instance() ;
  const StParticleDefinition* particle = (geantid<0)
    ? table->findParticleByGeantId(mGeantId)
    : table->findParticleByGeantId(geantid) ;
    
  return particle->name().c_str() ;
}


//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawEvent() const
{
  /// Event-wise QA

  LOG_INFO << "QA for Event-wise informations ..." << endm;

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  //----------------------------------------------------------------------------------------------------
  // Event-wise informations
  TCanvas* canvas = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 1200, 600);
  canvas->Divide(3, 2);

  /// z-vertex
  canvas->cd(1);
  TH1* hVz = (TH1D*) getHistogram("hVz");
  if(!hVz) return kFALSE ;

  TH1* hVzAccepted = (TH1D*) getHistogram("hVzAccepted");
  if(!hVzAccepted) return kFALSE ;

  hVzAccepted->SetLineColor(kCyan);
  hVzAccepted->SetFillColor(kCyan);
  hVz->Draw();
  hVzAccepted->Draw("same");
  hVz->Draw("same");

  /// y vs x vertices
  canvas->cd(2);
  TH2* hVyVx = (TH2D*) mInputEmbedding->Get("hVyVx");
  if(!hVyVx) return kFALSE ;

  hVyVx->SetAxisRange(-5, 5, "X");
  hVyVx->SetAxisRange(-5, 5, "Y");
  hVyVx->Draw("colz");

  /// dVx, dVy, dVz
  for(Int_t i=0; i<3; i++){
    canvas->cd(i+3);
    TH1* hdV = 0 ;
    if( i == 0 ) hdV = (TH1D*) mInputEmbedding->Get("hdVx");
    if( i == 1 ) hdV = (TH1D*) mInputEmbedding->Get("hdVy");
    if( i == 2 ) hdV = (TH1D*) mInputEmbedding->Get("hdVz");
    if(!hdV) return kFALSE ;

    hdV->Draw();
  }

  /// Statistics
  canvas->cd(6);
  drawStatistics();

  canvas->cd();
  canvas->Update();

  /// Print figures
  print(*canvas, "eventqa");

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawMcTrack() const
{
  /// MC track QA

  LOG_INFO << "QA for MC tracks (pt, eta, y, phi) ..." << endm;

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  //----------------------------------------------------------------------------------------------------
  // Track-wise informations
  TCanvas* canvas2D = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 800, 600);
  canvas2D->Divide(2, 2);

  /// pT vs eta
  canvas2D->cd(1);
  TH2* hPtVsEta = (TH2D*) getHistogram(Form("hPtVsEta_0_%d", mGeantId));
  if(!hPtVsEta) return kFALSE;

  hPtVsEta->Draw("colz");
  hPtVsEta->SetAxisRange(0, mPtMax, "Y");

  /// pT vs y
  canvas2D->cd(2);
  TH2* hPtVsY = (TH2D*) getHistogram(Form("hPtVsY_0_%d", mGeantId));
  if(!hPtVsY) return kFALSE ;

  hPtVsY->Draw("colz");
  hPtVsY->SetAxisRange(0, mPtMax, "Y");

  /// pT vs phi
  canvas2D->cd(3);
  TH2* hPtVsPhi = (TH2D*) getHistogram(Form("hPtVsPhi_0_%d", mGeantId));
  if(!hPtVsPhi) return kFALSE ;

  hPtVsPhi->Draw("colz");
  hPtVsPhi->SetAxisRange(0, mPtMax, "Y");

  /// Statistics
  canvas2D->cd(4);
  drawStatistics();

  canvas2D->cd();
  canvas2D->Update();

  /// Print figures for 2D histograms
  print(*canvas2D, "mctrack2d");

  // 1D projections
  TCanvas* canvas = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 800, 600);
  canvas->Divide(2, 2);

  /// Projections (pt, eta, y, phi)
  TH1* hPt  = (TH1D*) hPtVsPhi->ProjectionY("hPtMc");
  TH1* hEta = (TH1D*) hPtVsEta->ProjectionX("hEtaMc");
  TH1* hY   = (TH1D*) hPtVsY->ProjectionX("hYMc");
  TH1* hPhi = (TH1D*) hPtVsPhi->ProjectionX("hPhiMc");
  hEta->SetXTitle("#eta, y");

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
  hPt->SetAxisRange(0, mPtMax, "X");
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
  drawStatistics();

  canvas->cd();
  canvas->Update();

  /// Print figures for 1D histograms
  print(*canvas, "mctrack");

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawTrack() const
{
  /// Track-wise QA

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  //----------------------------------------------------------------------------------------------------
  // Track-wise informations (Comparison between MC and reconstructed particles)
  ///  1: Geant id
  const Bool_t isGeantIdOk = drawGeantId();
 
  ///  2: (pseudo-)rapidity distributions
  const Bool_t isRapidityOk = drawRapidity();
 
  ///  3: Momentum and pt
  const Bool_t isMomentumOk = drawMomentum();
  const Bool_t isPtOk       = drawPt();
 
  ///  4: dE/dx
  const Bool_t isdEdxOk     = drawdEdx();
 
  ///  5: Global dca
  const Bool_t isDcaOk      = drawDca();
 
  ///  6: NHit
  const Bool_t isNHitOk     = drawNHit();
  
  return isGeantIdOk && isRapidityOk && isMomentumOk && isPtOk && isdEdxOk && isDcaOk && isNHitOk ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawGeantId() const
{
  /// QA for geantid

  LOG_INFO << "QA for geant id ..." << endm;

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  /// pad0: MC tracks
  /// pad1: Reconstructed tracks
  /// pad2: Statistics
  TCanvas* canvas = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 1200, 800);
  canvas->Divide(2, 2);

  /// Draw MC tracks
  Int_t ipad = 1 ;
  canvas->cd(ipad++);
  TH1* hGeantIdMc = (TH1D*) getHistogram("hGeantId_0");
  if(!hGeantIdMc) return kFALSE ;

  /// Draw MC id +/- 10
  const Int_t mcIdBin    = hGeantIdMc->GetMaximumBin() ;
  const Double_t mcId    = hGeantIdMc->GetBinCenter(mcIdBin);
  const Double_t mcIdMin = (mcId-10<0) ? 0 : mcId-10 ;
  const Double_t mcIdMax = mcId + 10 ;

  hGeantIdMc->SetTitle(Form("MC Geant id for %s", getParticleName()));
  hGeantIdMc->SetMaximum(hGeantIdMc->GetMaximum()*1.2);
  hGeantIdMc->SetAxisRange(mcIdMin, mcIdMax, "X");
  hGeantIdMc->Draw();

  /// Draw line at expected geantid id for MC tracks
  TLine* hGeantIdMcExpected = new TLine(mGeantId+0.5, 0, mGeantId+0.5, hGeantIdMc->GetMaximum()) ; 
  hGeantIdMcExpected->SetLineColor(kBlue);
  hGeantIdMcExpected->Draw();

  /// Draw reconstructed geant id
  canvas->cd(ipad++);
  TH1* hGeantIdReco = (TH1D*) getHistogram(Form("hGeantId_%d", getCategoryId()));
  if(!hGeantIdReco) return kFALSE ;

  hGeantIdReco->SetTitle("Reconstructed geant id");
  hGeantIdReco->SetMaximum(hGeantIdReco->GetMaximum()*1.2);
  hGeantIdReco->SetAxisRange(0, 50, "X"); // Max geant id is 50
  hGeantIdReco->Draw();

  /// Draw expected geant id lines for decay daughters
  TLine** hGeantIdRecoExpected = new TLine*[mDaughterGeantId.size()];

  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    const Int_t geantid = mDaughterGeantId[id] ;
    hGeantIdRecoExpected[id] = new TLine(geantid+0.5, 0, geantid+0.5, hGeantIdReco->GetMaximum()) ; 
    hGeantIdRecoExpected[id]->SetLineColor(kRed);
    hGeantIdRecoExpected[id]->SetLineStyle(id+1);
    hGeantIdRecoExpected[id]->Draw();
  }

  canvas->cd(ipad++);
  TLegend* leg = new TLegend(0.1, 0.2, 0.9, 0.8);
  leg->SetTextSize(0.05);
  leg->SetFillColor(10);
  leg->SetHeader("Particle informations");

  const TString parent   = (isDecay()) ? "Parent " : "";
  const TString daughter = (isDecay()) ? "Daughter " : "";

  leg->AddEntry(hGeantIdMcExpected, Form("%s%s (%s, geantid=%d)", parent.Data(), getParticleName(), "MC", mGeantId), "L");

  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    const Int_t daughterId = mDaughterGeantId[id];
    leg->AddEntry( hGeantIdRecoExpected[id], Form("%s%s (%s, geantid=%d)", daughter.Data(),
          getParticleName(daughterId), StEmbeddingQAUtilities::instance()->getCategoryName(getCategoryId()).Data(), daughterId), "L");
  }
  leg->Draw();

  canvas->cd(ipad);
  drawStatistics();

  canvas->cd();
  canvas->Update();

  /// Print figures
  print(*canvas, "geantid");

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawRapidity() const
{
  /// QA for (pseudo-)rapidity

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  /// Pseudo-rapidity
  const Bool_t isEtaOk = drawProjection2D("eta");

  /// Rapidity
  const Bool_t isYOk   = drawProjection2D("y");

  return isEtaOk && isYOk ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawMomentum() const
{
  /// QA for momentum

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  /// Plot reconstructed momentum vs MC momentum (2D) (Embedding only)
  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    TH2* hRecoPVsMcP = (TH2D*) getHistogram("hRecoPVsMcP", id, kTRUE);
    if(!hRecoPVsMcP) return kFALSE ;

    hRecoPVsMcP->SetAxisRange(0, mPtMax, "X");
    hRecoPVsMcP->SetAxisRange(0, mPtMax, "Y");

    hRecoPVsMcP->SetTitle(getParticleName(mDaughterGeantId[id]));

    TCanvas* canvas = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++));
    hRecoPVsMcP->Draw("colz");

    canvas->cd();
    canvas->Update();
    print(*canvas, Form("recop_vs_momp_daughter%d", id));
  }

  return drawProjection2D("momentum");
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawPt() const
{
  /// QA for pt

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  return drawProjection2D("pt");
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawProjection2D(const TString name) const
{
  /// Utility function to draw 1D projection histgrams from 2D

  /// Input 2D histogram is pt or momentum vs eta or y
  /// Projection into eta or y for pt = 0.2 - 5 GeV/c (0.5 GeV/c step)
  /// or
  /// Projection into pt or momentum for |Delta eta| = 0.5 in |eta| < 2

  TString nameLower(name);
  nameLower.ToLower();

  LOG_INFO << "QA for " << name << " ..." << endm;

  /// Define y-axis title, histogram name and projection axis from the input argument
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
    LOG_INFO << "  Current implemented variables are" << endm;
    LOG_INFO << "-----------------------------------------------------------------" << endm;
    LOG_INFO << "   Input                       variable" << endm;
    LOG_INFO << "-----------------------------------------------------------------" << endm;
    LOG_INFO << "   pt                           p_{T}" << endm;
    LOG_INFO << "   momentum                     p" << endm;
    LOG_INFO << "   eta or pseudorapidity        eta" << endm;
    LOG_INFO << "   y or rapidity                y" << endm;
    LOG_INFO << "-----------------------------------------------------------------" << endm;
    LOG_INFO << endm;
    LOG_INFO << "NOTE : Input is case insensitive" << endm;
    LOG_INFO << endm;

    return kFALSE ;
  }

  gStyle->SetPadRightMargin(0.05);

  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    TH2* h2DEmbed = (TH2D*) getHistogram(histoName, id, kTRUE);
    if(!h2DEmbed) return kFALSE ;

    TH2* h2DReal  = (TH2D*) getHistogram(histoName, id, kFALSE);
    if(!h2DReal) return kFALSE ;

    /// Define canvas Ndivisions
    Int_t npad = 0 ;
    TCanvas* canvas = 0;
    if( isProjectionX ){
      canvas = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 1200, 800);
      canvas->Divide(4, 3);
      npad = 12 ;
    }
    else{
      canvas = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 1200, 600);
      canvas->Divide(4, 2);
      npad = 8 ;
    }

    /// Get bins
    /// eta bins = 6 : |eta| < 1.5
    /// pt bins  = 10 : pt = 0.2 - 5 GeV/c
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

      hEmbed->SetLineColor(kRed);
      hReal->SetLineColor(kBlue);
      hEmbed->Sumw2();
      hReal->Sumw2();
      hEmbed->Scale( getNormalization(*hEmbed) );
      hReal->Scale( getNormalization(*hReal) );

      /// Set maximum of y-axis
      /// Use real data if N(embedding) < 100
      Double_t yMax = 0.0 ;
      if ( hEmbed->GetEntries() < 100 ){
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
        hReal->SetAxisRange(0, mPtMax, "X");
      }
      hReal->SetYTitle(Form("(1/N_{trk})dN/%s", yTitle.Data()));

      canvas->cd(ibin+1);
      hReal->Draw("h");
      hEmbed->Draw("same");

      if( ibin == 0 ){
        canvas->cd(npad);
        TLegend* leg = new TLegend(0.1, 0.7, 0.9, 0.9);
        leg->SetFillColor(10);
        leg->SetTextSize(0.05);
        leg->AddEntry( hEmbed, Form("Embedding (%s)", getParticleName(mDaughterGeantId[id])), "L");
        leg->AddEntry( hReal, Form("Real data (%s)", getParticleName(getGeantIdReal(id))), "L");
        leg->Draw();

        drawStatistics(0.1, 0.2, 0.9, 0.5);
      }
    }

    canvas->cd();
    canvas->Update();

    if( mDaughterGeantId.size() == 1 ){
      print(*canvas, Form("%s", nameLower.Data()));
    }
    else{
      print(*canvas, Form("%s_daughter%d", nameLower.Data(), id));
    }
  }

  gStyle->SetPadRightMargin(0.15);

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawdEdx() const
{
  /// QA for dE/dx

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  /// vs MC momentum
  const Bool_t isMcOk   = drawdEdxVsMomentum(kTRUE) ;

  /// vs Reconstructed momentum
  const Bool_t isRecoOk = drawdEdxVsMomentum(kFALSE) ;

  return isMcOk && isRecoOk ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawdEdxVsMomentum(const Bool_t isMcMomentum) const
{
  /// Draw dE/dx vs momentum variables (either momentum or pt)
  /// Draw projection of dE/dx distribution for each momentum slice
  /// Draw mean/sigma as a function of momentum variables
  //
  /// Compare (1) embedding, (2) real data without PID, (3) real data with PID

  /// Select MC or reconstructed momentum for the embedding data
  /// Use reconstructed momentum for the real data
  const TString momName  = (isMcMomentum) ? "Mc" : "Reco" ;

  LOG_INFO << "QA for dE/dx (" << momName << " momentum ...)" << endm;

  gStyle->SetOptFit(0);
  gStyle->SetPadRightMargin(0.05);

  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    // 2D
    TCanvas* canvas2D = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 1200, 500);
    canvas2D->Divide(2, 1);

    // Embedding
    TH2* hdEdxVsMomEmbed = (TH2D*) getHistogram(Form("hdEdxVsMom%s", momName.Data()), id, kTRUE);
    if(!hdEdxVsMomEmbed);

    hdEdxVsMomEmbed->SetLineColor(kRed);
    hdEdxVsMomEmbed->SetMarkerColor(kRed);

    // Real data
    TH2* hdEdxVsMomReal = (TH2D*) getHistogram("hdEdxVsMomReco", id, kFALSE);
    if(!hdEdxVsMomReal);

    hdEdxVsMomReal->SetLineColor(kBlack);
    hdEdxVsMomReal->SetMarkerColor(kBlack);

    // Real data (with PID)
    TH2* hdEdxVsMomPidReal = (TH2D*) getHistogram("hdEdxVsMomRecoPidCut", id, kFALSE);
    if(!hdEdxVsMomPidReal);

    hdEdxVsMomPidReal->SetLineColor(kBlue);
    hdEdxVsMomPidReal->SetMarkerColor(kBlue);

    // dE/dx vs momentum
    canvas2D->cd(1);
    hdEdxVsMomReal->SetAxisRange(0, mPtMax, "X");
    hdEdxVsMomReal->SetXTitle(Form("%s momentum (GeV/c)", momName.Data()));
    hdEdxVsMomReal->SetTitle("");

    hdEdxVsMomReal->Draw();
    hdEdxVsMomPidReal->Draw("same");
    hdEdxVsMomEmbed->Draw("same");

    canvas2D->cd(2);

    TLegend* leg = new TLegend(0.1, 0.65, 0.9, 0.9);
    leg->SetFillColor(10);
    leg->SetTextSize(0.05);
    leg->AddEntry( hdEdxVsMomEmbed, Form("Embedding (%s)", getParticleName(mDaughterGeantId[id])), "L");
    leg->AddEntry( hdEdxVsMomReal, "Real data", "L");
    leg->AddEntry( hdEdxVsMomPidReal, "Real data with PID cut (#sigma<2)", "L");
    leg->Draw();

    drawStatistics(0.1, 0.2, 0.9, 0.5);

    canvas2D->cd();
    canvas2D->Update();

    if( mDaughterGeantId.size() == 1 ){
      print(*canvas2D, Form("dedx_vs_mom%s_", momName.Data()));
    }
    else{
      print(*canvas2D, Form("dedx_vs_mom%s_daughter%d", momName.Data(), id));
    }

    /// 1D projections for each momentum bin
    ///  From 0.2 GeV/c to 5.0 GeV/c (5*5)
    TCanvas* canvas0 = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 1200, 800);
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
        LOG_INFO << Form("%1.1f - %1.1f GeV/c : bin = (%4d, %4d)", ptMin, ptMax, ptMinBin, ptMaxBin) << endm;
      }

      // Projections
      TH1* hdEdxEmbed = (TH1D*) hdEdxVsMomEmbed->ProjectionY(Form("hdEdxEmbed%s_%d_%d", momName.Data(), id, ipt), ptMinBin, ptMaxBin);
      TH1* hdEdxReal  = (TH1D*) hdEdxVsMomPidReal->ProjectionY(Form("hdEdxReal%s_%d_%d", momName.Data(), id, ipt), ptMinBin, ptMaxBin);
      hdEdxEmbed->Sumw2() ;
      hdEdxReal ->Sumw2() ;
      hdEdxEmbed->Scale( getNormalization(*hdEdxEmbed) ) ;
      hdEdxReal ->Scale( getNormalization(*hdEdxReal) ) ;

      hdEdxReal->SetMinimum(0.0);
      hdEdxReal->SetMaximum( TMath::Max(hdEdxReal->GetMaximum(), hdEdxEmbed->GetMaximum())*1.2 );
      hdEdxReal->SetTitle(Form("%1.1f < %s p < %1.1f GeV/c", ptMin, momName.Data(), ptMax));
      hdEdxReal->SetYTitle("(1/N_{trk})dN/d(dE/dx)");

      canvas0->cd(ipt+1);
      hdEdxReal->Draw("h");
      hdEdxEmbed->Draw("hsame");

      // Extract mean and sigma (Oct/22/2009)
      TF1 fEmbed(Form("fEmbed%s_%d_%d", momName.Data(), id, ipt), "gaus", 0, 10);
      TF1 fReal(Form("fReal%s_%d_%d", momName.Data(), id, ipt), "gaus", 0, 10);
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

        TLegend* leg = new TLegend(0.1, 0.65, 0.9, 0.9);
        leg->SetFillColor(10);
        leg->SetTextSize(0.08);
        leg->AddEntry( hdEdxEmbed, Form("Embedding (%s)", getParticleName(mDaughterGeantId[id])), "L");
        leg->AddEntry( hdEdxReal,  Form("Real data (%s)", getParticleName(getGeantIdReal(id))), "L");
        leg->Draw();
      
        drawStatistics(0.1, 0.15, 0.9, 0.55, 0.08);
      }
    }// pt loop

    canvas0->cd();
    canvas0->Update();

    if( mDaughterGeantId.size() == 1 ){
      print(*canvas0, Form("dedx_1Dprojection_mom%s", momName.Data()));
    }
    else{
      print(*canvas0, Form("dedx_1Dprojection_mom%s_daughter%d", momName.Data(), id));
    }

    // Mean/Sigma vs momentum (real vs embed)
    TCanvas* canvas1 = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 1200, 500);
    canvas1->Divide(2, 1);

    for(Int_t i=0; i<2; i++){
      canvas1->cd(i+1);

      const Double_t ymax = (i==0) ? 5.2 : 1.4 ;
      TH1* frame = canvas1->GetPad(i+1)->DrawFrame(0, 0.0, 5.0, ymax);
      frame->SetXTitle(Form("%s momentum (GeV/c)", momName.Data()));
      if( i == 0 ) frame->SetYTitle("Mean (keV/cm)");
      if( i == 1 ) frame->SetYTitle("#sigma (keV/cm)");

      TLegend* leg = new TLegend(0.48, 0.7, 0.88, 0.86);
      leg->SetTextSize(0.05);
      leg->SetFillColor(10);

      if( i == 0 ){
        gMeanVsMom[0]->Draw("P");
        gMeanVsMom[1]->Draw("P");

        leg->AddEntry( gMeanVsMom[0], Form("Embedding (%s)", getParticleName(mDaughterGeantId[id])), "P");
        leg->AddEntry( gMeanVsMom[1], Form("Real data (%s)", getParticleName(getGeantIdReal(id))), "P");
      }
      if( i == 1 ){
        gSigmaVsMom[0]->Draw("P");
        gSigmaVsMom[1]->Draw("P");

        leg->AddEntry( gSigmaVsMom[0], Form("Embedding (%s)", getParticleName(mDaughterGeantId[id])), "P");
        leg->AddEntry( gSigmaVsMom[1], Form("Real data (%s)", getParticleName(getGeantIdReal(id))), "P");
      }
      leg->Draw();
    }

    canvas1->cd();
    canvas1->Update();

    if( mDaughterGeantId.size() == 1 ){
      print(*canvas1, Form("mean_sigma_dedx_mom%s", momName.Data()));
    }
    else{
      print(*canvas1, Form("mean_sigma_dedx_mom%s_daughter%d", momName.Data(), id));
    }
  }

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawDca() const
{
  /// QA for global DCA distributions for each (pt, eta) slice

  LOG_INFO << "QA for dca distributions ..." << endm;

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  return drawProjection3D("Dca");
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawNHit() const
{
  /// QA for Nfit distributions for each (pt, eta) slice

  LOG_INFO << "QA for NHit distributions ..." << endm;

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  /// QA for NCommon hit vs NHit
  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    TCanvas* canvas = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++));
    TH2* hNCommonHitVsNHit = (TH2D*) getHistogram("hNCommonHitVsNHit", id, kTRUE);
    hNCommonHitVsNHit->Draw("colz");
    print(*canvas, Form("ncommonhit_vs_nhit_daughter%d", id));
  }

  return drawProjection3D("NHit") ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawProjection3D(const TString name) const
{
  /// Utility function to get 1D projection from 3D histogram

  /// Plot histograms in each (pt, eta) space
  /// Suppose the input histogram is TH3 (x:pt, y:eta, z:variable you want to plot)

  /// Force to set batch mode to reduce the processing time
  const Bool_t isBatch = gROOT->IsBatch() ;
  if( !isBatch ){
    LOG_INFO << "Enter batch mode ..." << endm;
    gROOT->SetBatch(kTRUE);
  }

  TString nameLower(name);
  nameLower.ToLower();

  TCanvas* canvas = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 600, 800);
  canvas->Divide(2, 3);

  /// Define output PDF file
  const TString pdfFileName(Form("%s%s_%s.pdf", mOutputFigureDirectory.Data(), nameLower.Data(), getBaseName()));
  TPDF* pdf = new TPDF(pdfFileName);

  const Int_t npad = 5 ;

  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    TH3* h3DEmbed = (TH3D*) getHistogram(Form("h%s", name.Data()), id, kTRUE);
    if(!h3DEmbed) return kFALSE ;

    TH3* h3DReal  = (TH3D*) getHistogram(Form("h%s", name.Data()), id, kFALSE);
    if(!h3DReal) return kFALSE ;

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
        hEmbed->Sumw2();
        hReal ->Sumw2();
        hEmbed->Scale( getNormalization(*hEmbed) );
        hReal ->Scale( getNormalization(*hReal) );
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
          TLegend* leg = new TLegend(0.1, 0.7, 0.9, 0.9);
          leg->SetFillColor(10);
          leg->SetTextSize(0.05);
          leg->AddEntry( hEmbed, Form("Embedding (%s)", getParticleName(mDaughterGeantId[id])), "L");
          leg->AddEntry( hReal,  Form("Real data (%s)", getParticleName(getGeantIdReal(id))), "L");
          leg->Draw();
  
          drawStatistics(0.1, 0.2, 0.9, 0.5);
        }
      }
      canvas->cd();
      canvas->Update();
      pdf->NewPage();
    }
  }
  pdf->Close();
  LOG_INFO << "Print PDF file : " << pdfFileName << endm;

  // Back to the original mode
  if( isBatch ) gROOT->SetBatch(kTRUE);  // Stay batch mode if you've started the macro by batch mode
  else          gROOT->SetBatch(kFALSE); // Exit batch mode if you've not

  return kTRUE ;
}


//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::draw() const
{
  /// QA for event-wise and track-wise (both MC and reconstructed) histograms

  //====================================================================================================
  //
  // Start drawing
  //
  //====================================================================================================

  //----------------------------------------------------------------------------------------------------
  /// (1) Event-wise informations
  const Bool_t isEventOk = drawEvent();
  //----------------------------------------------------------------------------------------------------

  //----------------------------------------------------------------------------------------------------
  /// (2-1) Track-wise informations (MC)
  const Bool_t isMcTrackOk = drawMcTrack();
  //----------------------------------------------------------------------------------------------------

  //----------------------------------------------------------------------------------------------------
  /// (2-2) Track-wise informations (Real vs Embedding)
  const Bool_t isTrackOk = drawTrack();
  //----------------------------------------------------------------------------------------------------

  return isEventOk && isMcTrackOk && isTrackOk ;
}


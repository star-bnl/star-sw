
#include <fstream>
#include <iostream>
#include <string>

#include "TClonesArray.h"
#include "TError.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TObject.h"
#include "TTree.h"
#include "TObjArray.h"

#include "StMiniMcEvent/StMiniMcEvent.h"
#include "StMiniMcEvent/StTinyMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include "StEmbeddingQAMaker.h"
#include "StEmbeddingQAParticle.h"
#include "StEmbeddingQAParticleCollection.h"
#include "StEmbeddingQATrack.h"

using namespace std ;

  const Float_t StEmbeddingQAMaker::kVertexCut = 30.0 ; // define z-vertex cut

ClassImp(StEmbeddingQAMaker)

//__________________________________________________________________________________________
StEmbeddingQAMaker::StEmbeddingQAMaker()
  : kYear(2007), kProduction("P08ic"), kParticleId(8), kIsSimulation(kTRUE)
{
  init() ;
}

//__________________________________________________________________________________________
StEmbeddingQAMaker::StEmbeddingQAMaker(const Int_t year, const TString production, const Int_t particleId, const Bool_t isSimulation)
  : kYear(year), kProduction(production), kParticleId(particleId), kIsSimulation(isSimulation)
{
  init() ;
}

//__________________________________________________________________________________________
StEmbeddingQAMaker::StEmbeddingQAMaker(const Int_t year, const TString production, const TString name, const Bool_t isSimulation)
  : kYear(year), kProduction(production), kParticleId(StEmbeddingQAUtilities::getParticleId(name)), kIsSimulation(isSimulation)
{
  init() ;
}

//__________________________________________________________________________________________
StEmbeddingQAMaker::~StEmbeddingQAMaker()
{
  delete mParticles ;
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::init()
{
  cout << endl;
  cout << Form("   StEmbeddingQAMaker::init() for year       : %10d", kYear) << endl;
  cout << Form("   StEmbeddingQAMaker::init() for production : %10s", kProduction.Data()) << endl;
  cout << Form("   StEmbeddingQAMaker::init() for particle   : %10s (id = %3d )",
    StEmbeddingQAUtilities::getParticleName(kParticleId).Data(), kParticleId)
    << endl;
  cout << endl;

  mOutput = 0;
  mDebug = 0;
  mMuDstMaker = 0;
  mVz = -9999.0 ;

  mParticles = new StEmbeddingQAParticleCollection(kParticleId);
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::isZVertexOk(const StMiniMcEvent& mcevent, const Float_t vertexCut) const
{
  return TMath::Abs(mcevent.vertexZ()) < vertexCut ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::book(const TString outputFileName)
{
  // Book histograms
  TString fileName(outputFileName);

  // Set default file name if output filename is blank
  if( fileName.IsWhitespace() ){
    const TString data = (kIsSimulation) ? "embedding" : "real" ;
    fileName = Form("qa_%s_%d_%s_%s.root", 
        data.Data(), kYear, kProduction.Data(), StEmbeddingQAUtilities::getParticleName(kParticleId, kFALSE).Data()) ;
  }

  mOutput = TFile::Open(fileName, "recreate");
  mOutput->cd();
  cout << "    OPEN " << mOutput->GetName() << endl;

  // Event-wise informations
  hVz         = new TH1D("hVz", "z-vertex", 100, -50, 50);
  hVzAccepted = new TH1D("hVzAccepted", "z-vertex with z-vertex cut", 100, -50, 50);
  hVz->SetXTitle("v_{z} (cm)");
  hVzAccepted->SetXTitle("v_{z} (cm)");

  hVyVx = new TH2D("hVyVx", "v_{y} vs v_{x}", 100, -10, 10, 100, -10, 10);
  hVyVx->SetXTitle("v_{x} (cm)");
  hVyVx->SetYTitle("v_{y} (cm)");

  hdVx = new TH1D("hdVx", "#Delta x = v_{x} - v_{x}(MC)", 100, -10+0.5, 10+0.5);
  hdVy = new TH1D("hdVy", "#Delta y = v_{y} - v_{y}(MC)", 100, -10+0.5, 10+0.5);
  hdVz = new TH1D("hdVz", "#Delta z = v_{z} - v_{z}(MC)", 100, -10+0.5, 10+0.5);
  hdVx->SetXTitle("#Deltav_{x} = v_{x} - v_{x}(MC) (cm)");
  hdVy->SetXTitle("#Deltav_{y} = v_{y} - v_{y}(MC) (cm)");
  hdVz->SetXTitle("#Deltav_{z} = v_{z} - v_{z}(MC) (cm)");

  const Int_t ptBin    = 100 ;
  const Float_t ptMin  = 0.0 ;
  const Float_t ptMax  = 10.0 ;
  const Int_t etaBin   = 100 ;
  const Float_t etaMin = -2.5 ;
  const Float_t etaMax =  2.5 ;

  // Track-wise informations
  TString particleName(mParticles->getParent()->getTitle());

  // Common histograms for all categories
  for(Int_t ic=0; ic<StEmbeddingQAUtilities::kNCategory; ic++){
    TString categoryTitle(StEmbeddingQAUtilities::getCategoryTitle(ic));

    // Initialize histograms
    const Int_t nHistogram = getNumberOfHistograms(ic);

    hGeantId[ic]             = new TH1*[nHistogram] ;
    hNHit[ic]                = new TH3*[nHistogram] ;
    hDca[ic]                 = new TH3*[nHistogram] ;
    hPtVsEta[ic]             = new TH2*[nHistogram] ;
    hPtVsY[ic]               = new TH2*[nHistogram] ;
    hPtVsPhi[ic]             = new TH2*[nHistogram] ;
    hPtVsMom[ic]             = new TH2*[nHistogram] ;
    hdPtVsPt[ic]             = new TH2*[nHistogram] ;
    hMomVsEta[ic]            = new TH2*[nHistogram] ;
    hdEdxVsMomMc[ic]         = new TH2*[nHistogram] ;
    hdEdxVsMomMcPidCut[ic]   = new TH2*[nHistogram] ;
    hdEdxVsMomReco[ic]       = new TH2*[nHistogram] ;
    hdEdxVsMomRecoPidCut[ic] = new TH2*[nHistogram] ;
    hRecoPVsMcP[ic]          = new TH2*[nHistogram] ;
    hEtaVsPhi[ic]            = new TH2*[nHistogram] ;
    hEtaVsVz[ic]             = new TH2*[nHistogram] ;
    hYVsVz[ic]               = new TH2*[nHistogram] ;

    for(Int_t ih=0; ih<nHistogram; ih++){

      TString nameSuffix(Form("_%d", ic));
      if ( nHistogram > 1 ) nameSuffix = Form("_%d_%d", ic, ih);

      TString name(particleName);
      if ( nHistogram > 1 ){
        name = Form("%s (decay daughter)", mParticles->getDaughter(ih)->getTitle().Data());
      }

      TString title(Form("(%s), %s", categoryTitle.Data(), name.Data()));

      // Geant id
      hGeantId[ic][ih] = new TH1D(Form("hGeantId%s", nameSuffix.Data()), "", 1000, 0, 1000);
      hGeantId[ic][ih]->SetXTitle("Geant id");
      hGeantId[ic][ih]->SetTitle( Form("Geant id, %s", title.Data()) );
 
      // NHit
      hNHit[ic][ih] = new TH3D(Form("hNHit%s", nameSuffix.Data()), "", 10, 0, 5, 10, -1.0, 1.0, 50, 0, 50);
      hNHit[ic][ih]->SetXTitle("MC p_{T} (GeV/c)");
      hNHit[ic][ih]->SetYTitle("#eta");
      hNHit[ic][ih]->SetZTitle("N_{hit}");
      if( ic == 0 ) hNHit[ic][ih]->SetTitle( Form("N_{fit} distribution, %s", title.Data()) );
      else          hNHit[ic][ih]->SetTitle( Form("N_{fit} distribution (|dcaGl|<3 cm), %s", title.Data()) );
 
      // Dca
      hDca[ic][ih] = new TH3D(Form("hDca%s", nameSuffix.Data()), "",
          10, 0, 5, 10, -1.0, 1.0, 100, 0, 3.0);
      hDca[ic][ih]->SetXTitle("MC p_{T} (GeV/c)");
      hDca[ic][ih]->SetYTitle("#eta");
      hDca[ic][ih]->SetZTitle("Global dca (cm)");
      if( ic == 0 )                 hDca[ic][ih]->SetTitle( Form("Dca vs #eta vs MC p_{T}, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hDca[ic][ih]->SetTitle( Form("Dca vs #eta vs MC p_{T} (N_{fit}#geq10 & N_{common}#geq10), %s", title.Data()) );
      else                          hDca[ic][ih]->SetTitle( Form("Dca vs #eta vs MC p_{T} (N_{fit}#geq10), %s", title.Data()) );
 
      // pt vs eta
      hPtVsEta[ic][ih] = new TH2D(Form("hPtVsEta%s", nameSuffix.Data()), "", etaBin, etaMin, etaMax, ptBin, ptMin, ptMax);
      hPtVsEta[ic][ih]->SetXTitle("#eta");
      hPtVsEta[ic][ih]->SetYTitle("MC p_{T} (GeV/c)");
      if( ic == 0 )                 hPtVsEta[ic][ih]->SetTitle( Form("MC p_{T} vs #eta, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hPtVsEta[ic][ih]->SetTitle( Form("MC p_{T} vs #eta (N_{fit}#geq10 & N_{common}#geq10 & |dcaGl|<3cm), %s", title.Data()) );
      else                          hPtVsEta[ic][ih]->SetTitle( Form("MC p_{T} vs #eta (N_{fit}#geq10 & |dcaGl|<3cm), %s", title.Data()) );
 
      // pt vs y
      hPtVsY[ic][ih] = new TH2D(Form("hPtVsY%s", nameSuffix.Data()), "", etaBin, etaMin, etaMax, ptBin, ptMin, ptMax);
      hPtVsY[ic][ih]->SetXTitle("rapidity y");
      hPtVsY[ic][ih]->SetYTitle("MC p_{T} (GeV/c)");
      if( ic == 0 )                 hPtVsY[ic][ih]->SetTitle( Form("MC p_{T} vs y, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hPtVsY[ic][ih]->SetTitle( Form("MC p_{T} vs y (N_{fit}#geq10 & N_{common}#geq10 & |dcaGl|<3cm), %s", title.Data()) );
      else                          hPtVsY[ic][ih]->SetTitle( Form("MC p_{T} vs y (N_{fit}#geq10 & |dcaGl|<3cm), %s", title.Data()) );
 
      // pt vs phi
      hPtVsPhi[ic][ih] = new TH2D(Form("hPtVsPhi%s", nameSuffix.Data()), "", 100, -TMath::Pi(), TMath::Pi(), ptBin, ptMin, ptMax);
      hPtVsPhi[ic][ih]->SetXTitle("#phi (rad)");
      hPtVsPhi[ic][ih]->SetYTitle("MC p_{T} (GeV/c)");
      if( ic == 0 )                 hPtVsPhi[ic][ih]->SetTitle( Form("MC p_{T} vs #phi, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hPtVsPhi[ic][ih]->SetTitle( Form("MC p_{T} vs #phi (N_{fit}#geq10 & N_{common}#geq10 & |dcaGl|<3cm), %s", title.Data()) );
      else                          hPtVsPhi[ic][ih]->SetTitle( Form("MC p_{T} vs #phi (N_{fit}#geq10 & |dcaGl|<3cm), %s", title.Data()) );

      // pt vs momentum
      hPtVsMom[ic][ih] = new TH2D(Form("hPtVsMom%s", nameSuffix.Data()), "", ptBin, ptMin, ptMax, ptBin, ptMin, ptMax);
      hPtVsMom[ic][ih]->SetXTitle("momentum (GeV/c)");
      hPtVsMom[ic][ih]->SetYTitle("MC p_{T} (GeV/c)");
      if( ic == 0 )                 hPtVsMom[ic][ih]->SetTitle( Form("MC p_{T} vs momentum, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hPtVsMom[ic][ih]->SetTitle( Form("MC p_{T} vs momentum (N_{fit}#geq10 & N_{common}#geq10 & |dcaGl|<3cm), %s", title.Data()) );
      else                          hPtVsMom[ic][ih]->SetTitle( Form("MC p_{T} vs momentum (N_{fit}#geq10 & |dcaGl|<3cm), %s", title.Data()) );
 
      // Delta pt vs pt
      hdPtVsPt[ic][ih] = new TH2D(Form("hdPtVsPt%s", nameSuffix.Data()), "", 100, -5, 5, ptBin, ptMin, ptMax);
      hdPtVsPt[ic][ih]->SetXTitle("reco. p_{T} (GeV/c)");
      hdPtVsPt[ic][ih]->SetYTitle("reco. p_{T} - MC p_{T} (GeV/c)");
      if( ic == 0 )                 hdPtVsPt[ic][ih]->SetTitle( Form("p_{T} - p_{T} (MC) vs p_{T}, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hdPtVsPt[ic][ih]->SetTitle( Form("p_{T} - p_{T} (MC) vs p_{T} (N_{fit}#geq10 & N_{common}#geq10 & |dcaGl|<3cm), %s", title.Data()) );
      else                          hdPtVsPt[ic][ih]->SetTitle( Form("p_{T} - p_{T} (MC) vs p_{T} (N_{fit}#geq10 & |dcaGl|<3cm), %s", title.Data()) );

      // momentum vs eta
      hMomVsEta[ic][ih] = new TH2D(Form("hMomVsEta%s", nameSuffix.Data()), "", etaBin, etaMin, etaMax, ptBin, ptMin, ptMax);
      hMomVsEta[ic][ih]->SetXTitle("#eta");
      hMomVsEta[ic][ih]->SetYTitle("momentum (GeV/c)");
      if( ic == 0 )                 hMomVsEta[ic][ih]->SetTitle( Form("Momentum vs #eta, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hMomVsEta[ic][ih]->SetTitle( Form("Momentum vs #eta (N_{fit}#geq10 & N_{common}#geq10 & |dcaGl|<3cm), %s", title.Data()) );
      else                          hMomVsEta[ic][ih]->SetTitle( Form("Momentum vs #eta (N_{fit}#geq10 & |dcaGl|<3cm), %s", title.Data()) );
 
      // dE/dx vs MC momentum
      hdEdxVsMomMc[ic][ih] = new TH2D(Form("hdEdxVsMomMc%s", nameSuffix.Data()), "", ptBin, ptMin, ptMax, 100, 0, 10.0);
      hdEdxVsMomMc[ic][ih]->SetXTitle("MC p (GeV/c)");
      hdEdxVsMomMc[ic][ih]->SetYTitle("dE/dx (keV/cm)");
      if( ic == 0 ) hdEdxVsMomMc[ic][ih]->SetTitle( Form("dE/dx vs MC p, %s", title.Data()) );
      else          hdEdxVsMomMc[ic][ih]->SetTitle( Form("dE/dx vs MC p, (N_{fit}#geq10 & |dcaGl|<3cm), %s", title.Data()) );

      // dE/dx vs MC momentum (with pid cut)
      hdEdxVsMomMcPidCut[ic][ih] = new TH2D(Form("hdEdxVsMomMcPidCut%s", nameSuffix.Data()), "", ptBin, ptMin, ptMax, 100, 0, 10.0);
      hdEdxVsMomMcPidCut[ic][ih]->SetXTitle("MC p (GeV/c)");
      hdEdxVsMomMcPidCut[ic][ih]->SetYTitle("dE/dx (keV/cm)");
      if( ic == 0 ) hdEdxVsMomMcPidCut[ic][ih]->SetTitle( Form("dE/dx vs MC p (with 2#sigma pid cut), %s", title.Data()) );
      else          hdEdxVsMomMcPidCut[ic][ih]->SetTitle( Form("dE/dx vs MC p (with 2#sigma pid cut), (N_{fit}#geq10 & |dcaGl|<3cm), %s", title.Data()) );

      // dE/dx vs reconstructed momentum
      hdEdxVsMomReco[ic][ih] = new TH2D(Form("hdEdxVsMomReco%s", nameSuffix.Data()), "", ptBin, ptMin, ptMax, 100, 0, 10.0);
      hdEdxVsMomReco[ic][ih]->SetXTitle("Reconstructed p (GeV/c)");
      hdEdxVsMomReco[ic][ih]->SetYTitle("dE/dx (keV/cm)");
      if( ic == 0 ) hdEdxVsMomReco[ic][ih]->SetTitle( Form("dE/dx vs Reconstructed p, %s", title.Data()) );
      else          hdEdxVsMomReco[ic][ih]->SetTitle( Form("dE/dx vs Reconstructed p, (N_{fit}#geq10 & |dcaGl|<3cm), %s", title.Data()) );

      // dE/dx vs Reconstructed momentum (with pid cut)
      hdEdxVsMomRecoPidCut[ic][ih] = new TH2D(Form("hdEdxVsMomRecoPidCut%s", nameSuffix.Data()), "", ptBin, ptMin, ptMax, 100, 0, 10.0);
      hdEdxVsMomRecoPidCut[ic][ih]->SetXTitle("Reconstructed p (GeV/c)");
      hdEdxVsMomRecoPidCut[ic][ih]->SetYTitle("dE/dx (keV/cm)");
      if( ic == 0 ) hdEdxVsMomRecoPidCut[ic][ih]->SetTitle( Form("dE/dx vs Reconstructed p (with 2#sigma pid cut), %s", title.Data()) );
      else          hdEdxVsMomRecoPidCut[ic][ih]->SetTitle( Form("dE/dx vs Reconstructed p (with 2#sigma pid cut), (N_{fit}#geq10 & |dcaGl|<3cm), %s", title.Data()) );

      // Reconstructed momentum vs MC momentum
      hRecoPVsMcP[ic][ih] = new TH2D(Form("hRecoPVsMcP%s", nameSuffix.Data()), "", ptBin, ptMin, ptMax, ptBin, ptMin, ptMax);
      hRecoPVsMcP[ic][ih]->SetXTitle("MC p (GeV/c)");
      hRecoPVsMcP[ic][ih]->SetYTitle("Reconstructed p (GeV/c)");
      if( ic == 0 ) hRecoPVsMcP[ic][ih]->SetTitle( Form("Reconstructed p vs MC p %s", title.Data()) );
      else          hRecoPVsMcP[ic][ih]->SetTitle( Form("Reconstructed p vs MC p (N_{fit}#geq10 & |dcaGl|<3cm), %s", title.Data()) );

 
      TString pt("0.2 < p_{T} < 5 GeV/c");
 
      // eta vs phi
      hEtaVsPhi[ic][ih] = new TH2D(Form("hEtaVsPhi%s", nameSuffix.Data()), "", 100, -TMath::Pi(), TMath::Pi(), etaBin, etaMin, etaMax);
      hEtaVsPhi[ic][ih]->SetXTitle("#phi (rad)");
      hEtaVsPhi[ic][ih]->SetYTitle("#eta");
      if( ic == 0 )                 hEtaVsPhi[ic][ih]->SetTitle( Form("#eta vs #phi, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hEtaVsPhi[ic][ih]->SetTitle( Form("#eta vs #phi (N_{fit}>=10 & N_{common}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
      else                          hEtaVsPhi[ic][ih]->SetTitle( Form("#eta vs #phi (N_{fit}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
 
      // eta vs vz
      hEtaVsVz[ic][ih] = new TH2D(Form("hEtaVsVz%s", nameSuffix.Data()), "", 200, -50, 50, 200, etaMin, etaMax);
      hEtaVsVz[ic][ih]->SetXTitle("v_{z} (cm)");
      hEtaVsVz[ic][ih]->SetYTitle("#eta");
      if( ic == 0 )                 hEtaVsVz[ic][ih]->SetTitle( Form("#eta vs v_{z}, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hEtaVsVz[ic][ih]->SetTitle( Form("#eta vs v_{z} (N_{fit}>=10 & N_{common}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
      else                          hEtaVsVz[ic][ih]->SetTitle( Form("#eta vs v_{z} (N_{fit}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
 
      // rapidity vs vz
      hYVsVz[ic][ih] = new TH2D(Form("hYVsVz%s", nameSuffix.Data()), "", 200, -50, 50, 200, etaMin, etaMax);
      hYVsVz[ic][ih]->SetXTitle("v_{z} (cm)");
      hYVsVz[ic][ih]->SetYTitle("rapidity y");
      if( ic == 0 )                 hYVsVz[ic][ih]->SetTitle( Form("rapidity y vs v_{z}, %s", pt.Data(), title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hYVsVz[ic][ih]->SetTitle( Form("rapidity y vs v_{z} (N_{fit}>=10 & N_{common}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
      else                          hYVsVz[ic][ih]->SetTitle( Form("rapidity y vs v_{z} (N_{fit}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
    }
  }

  mOutput->GetList()->Sort();
  cout << endl << endl << endl;

  return kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::make(const TString inputFileName, const Bool_t isSimulation)
{
  // Check output file
  if(!mOutput || !mOutput->IsOpen()){
    Error("StEmbeddingQAMaker::Make", "Output file is not opened");
    return kFALSE ;
  }

  if( isSimulation ){
    // Fill embedding outputs from minimc tree
    cout << "------------------------------------------------------------------------------------" << endl;
    cout << "            Fill embedding ..." << endl;
    cout << "------------------------------------------------------------------------------------" << endl;
    fillEmbedding(inputFileName);
  }
  else{
    // Fill real data outputs from MuDST
    cout << "------------------------------------------------------------------------------------" << endl;
    cout << "            Fill real data ..." << endl;
    cout << "------------------------------------------------------------------------------------" << endl;
    fillRealData(inputFileName);
  }

  return kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::fillEmbedding(const TString inputFileName)
{
  // Open input file
  TFile* file = TFile::Open(inputFileName);
  if( !file || !file->IsOpen() ){
    Error("StEmbeddingQAMaker::fillEmbedding", "can't open %s", inputFileName.Data());
    return kFALSE ;
  }

  const TString treeName("StMiniMcTree");
  TTree* tree = (TTree*) file->Get(treeName);
  if( !tree ){
    Error("StEmbeddingQAMaker::fillEmbedding", "can't find %s tree", treeName.Data());
    return kFALSE ;
  }

  StMiniMcEvent* mMiniMcEvent = new StMiniMcEvent();
  TBranch* b_MiniMcEvent = tree->GetBranch("StMiniMcEvent");
  b_MiniMcEvent->SetAddress(&mMiniMcEvent);

  // Tracks in minimc tree
  //     mMcTracks     = new TClonesArray("StTinyMcTrack",nMcTrack);
  //     mMatchedPairs = new TClonesArray("StMiniMcPair",nMatchedPair);
  //     mMergedPairs  = new TClonesArray("StMiniMcPair",nMergedPair);
  //     mSplitPairs   = new TClonesArray("StMiniMcPair",nSplitPair);
  //     
  //     mGhostPairs   = new TClonesArray("StMiniMcPair",nGhostPair);
  //     mContamPairs  = new TClonesArray("StContamPair",nContamPair); 
  //     mMatGlobPairs = new TClonesArray("StMiniMcPair",nMatGlobPair);
  //
  //  Track name definition in StMiniMcEvent.h
  //   enum Category { MC,MATCHED,MERGED,SPLIT,CONTAM,GHOST,MATGLOB};

  const Long64_t nentries = tree->GetEntries();
  cout << Form("    OPEN %s,  # of event = %10d", inputFileName.Data(), nentries) << endl;

  for (Int_t ievent=0; ievent<nentries;ievent++) {
    tree->GetEntry(ievent);

    const Float_t vx   = mMiniMcEvent->vertexX() ;
    const Float_t vy   = mMiniMcEvent->vertexY() ;
    const Float_t vz   = mMiniMcEvent->vertexZ() ;
    const Float_t vxmc = mMiniMcEvent->mcVertexX() ;
    const Float_t vymc = mMiniMcEvent->mcVertexY() ;
    const Float_t vzmc = mMiniMcEvent->mcVertexZ() ;
    hVz->Fill(vz);

    mVz = vz ;

    // z-vertex cut
    if( !isZVertexOk(*mMiniMcEvent, kVertexCut) ) continue ;

    hVzAccepted->Fill(vz);
    hVyVx->Fill(vx, vy);
    hdVx->Fill( vx - vxmc );
    hdVy->Fill( vy - vymc );
    hdVz->Fill( vz - vzmc );

    // Get MC, MATCHED, GHOST, CONTAM and MATGLOB pairs
    for(Int_t trackid=0; trackid<StEmbeddingQAUtilities::kNEmbedding; trackid++){

      Int_t nTrack = 0 ;
      switch ( trackid ) {
        case 0: nTrack = mMiniMcEvent->nMcTrack(); break ;
        case 1: nTrack = mMiniMcEvent->nMatchedPair(); break ;
        case 2: nTrack = mMiniMcEvent->nGhostPair(); break ;
        case 3: nTrack = mMiniMcEvent->nContamPair(); break ;
        // NOTE:
        //   Currently, there is no function to get number of matched global paris in StMiniMcEvent
        //   Skip it
        case 4: nTrack = 0 ; break ;
        default:
          Error("fillEmbedding", "Unkown branch id, id=%3d", trackid);
          return kFALSE ;
      }

      if( mDebug && ievent % 200 == 0 ){
        cout << Form("####  event=%4d, category=%10s, ntrack=%10d",
            ievent, StEmbeddingQAUtilities::getCategoryName(trackid).Data(), nTrack)
          << endl;
      }

      for(Int_t itrk=0; itrk<nTrack; itrk++){
        switch ( trackid ) {
          case 0: fillMcTracks(*mMiniMcEvent, trackid, itrk); break ;
          case 1: fillMatchedPairs(*mMiniMcEvent, trackid, itrk); break ;
          case 2: fillGhostPairs(*mMiniMcEvent, trackid, itrk); break ;
          case 3: fillContamPairs(*mMiniMcEvent, trackid, itrk); break ;
          case 4: fillMatGlobPairs(*mMiniMcEvent, trackid, itrk); break ;
          default:
            Error("fillEmbedding",  "Unkown branch id, id=%3d", trackid);
            return kFALSE ;
        }
      }// track loop
    }// Track category

  }// event loop

  delete mMiniMcEvent ;
  file->Close() ;

  return kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::fillRealData(const TString inputFileName)
{
  Int_t ievent = 0;
  while( !mMuDstMaker->Make() ){ //read event
    StMuEvent* muEvent = mMuDstMaker->muDst()->event() ;
    if(!muEvent) continue ;

    // Vertex cut
    const Double_t vx = muEvent->primaryVertexPosition().x() ;
    const Double_t vy = muEvent->primaryVertexPosition().y() ;
    const Double_t vz = muEvent->primaryVertexPosition().z() ;
    const Bool_t isVertexBad = TMath::Abs(vz) >= kVertexCut
      || ( vx < 1.0e-5 && vy < 1.0e-5 && vz < 1.0e-5 )
      || ( TMath::Abs(vx) > 1000 || TMath::Abs(vy) > 1000 || TMath::Abs(vz) > 1000 )
      ;
    if( isVertexBad ) continue ;

    hVz->Fill(vz);
    hVyVx->Fill(vx, vy);

    mVz = vz ;

    for(Int_t ic=0; ic<StEmbeddingQAUtilities::kNReal; ic++){
      const Int_t trackid = ic + StEmbeddingQAUtilities::kNEmbedding ;
      if( mDebug && ievent % 200 == 0 ){
        cout << Form("%85s ####  event=%4d, category=%10s",
            mMuDstMaker->GetFileName(), ievent, StEmbeddingQAUtilities::getCategoryName(trackid).Data())
          << endl;
      }

      TObjArray* tracks = (ic==0) 
        ?  mMuDstMaker->muDst()->primaryTracks()   // Primary tracks
        :  mMuDstMaker->muDst()->globalTracks() ;  // Global tracks

      TObjArrayIter trackIterator(tracks);
      StMuTrack* track = 0;
      Int_t itrk = 0;
      while ( ( track = (StMuTrack*) trackIterator.Next() ) ){
        fillRealTracks(*track, trackid, itrk);
        itrk++;
      }
    }

    ievent++;
  }// event loop

  cout << "End of real data" << endl;
  cout << "Total # of events = " << ievent << endl;

  return kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::runRealData(const TString inputFileList)
{
  if( mMuDstMaker ) delete mMuDstMaker ;

  cout << "###    Read " << inputFileList << endl;
  mMuDstMaker  = new StMuDstMaker(0, 0, "", inputFileList, "MuDst", 1000);

  make(inputFileList, kFALSE);

  return kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::runEmbedding(const TString inputFileList)
{
  ifstream fEmbedding(inputFileList);
  if(!fEmbedding){
    Error("StEmbeddingQAMaker::runEmbedding", "can't find %s", inputFileList.Data());
    return kFALSE ;
  }
    cout << "###    Read " << inputFileList << endl;

  // Fill embedding outputs
  TString file ;
  while( fEmbedding >> file ){
    cout << "####     Read file : " << file << endl;
    make(file, kTRUE);
  }

  return kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::run(const TString inputFileList)
{
  if( kIsSimulation ){
    // Embedding QA
    return runEmbedding(inputFileList) ;
  }
  else{
    // Real data QA
    return runRealData(inputFileList) ;
  }

  return kFALSE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::end()
{
  cout << "    End of StEmbeddingQAMaker" << endl;
  cout << "    Write output " << mOutput->GetName() << endl;
  mOutput->Write();
  mOutput->Close();

  return kTRUE ;
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::fillMcTracks(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk)
{
  StTinyMcTrack* track = (StTinyMcTrack*) mcevent.tracks(StEmbeddingQAUtilities::getCategory(trackid))->At(itrk) ;
  if(!track){
    Error("fillMcTracks", "Cannot find MC tracks");
    return ;
  }

  // Make sure geant id is equal to the input particle id
  if( track->geantId() != mParticles->getParent()->getParticleId() ) return ;

  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::getCategoryName(trackid), *track, mParticles->getParent()->getMass2());

  fillHistograms(miniTrack, trackid, 0, kParticleId);
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::fillMatchedPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk)
{
  StMiniMcPair* track = (StMiniMcPair*) mcevent.tracks(StEmbeddingQAUtilities::getCategory(trackid))->At(itrk) ;
  if(!track){
    Error("fillMatchedPairs", "Cannot find Matched pairs");
    return ;
  }

  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::getCategoryName(trackid), track, mParticles->getParent()->getMass2());

  fillHistograms(miniTrack, trackid, 0, kParticleId);
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::fillGhostPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk)
{
  StMiniMcPair* track = (StMiniMcPair*) mcevent.tracks(StEmbeddingQAUtilities::getCategory(trackid))->At(itrk) ;
  if(!track){
    // Suppress error messege for the recent embedding samples. There are no ghost tracks
//    Error("fillGhostPairs", "Cannot find Ghost pairs");
    return ;
  }

  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::getCategoryName(trackid), track, mParticles->getParent()->getMass2());
  fillHistograms(miniTrack, trackid, 0, kParticleId);
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::fillContamPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk)
{
  StContamPair* track = (StContamPair*) mcevent.tracks(StEmbeddingQAUtilities::getCategory(trackid))->At(itrk) ;
  if(!track){
    Error("fillContamPairs", "Cannot find Contaminated pairs");
    return ;
  }

  for(UInt_t id=0; id<mParticles->getNDaughter(); id++){
    // Mass2 for daughter particles (fixed on Oct/22/2009)

    StEmbeddingQAParticle* daughter = mParticles->getDaughter(id) ;
    StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::getCategoryName(trackid), track, daughter->getMass2());

    // Make sure parent geantid is correct
    if ( miniTrack.getParentGeantId() != mParticles->getParent()->getParticleId() ) continue ;

    // Daughter id selection
    if ( miniTrack.getGeantId() == daughter->getParticleId() ){
      // Make sure daughter particles have correct charge
      const Bool_t isChargeOk = miniTrack.getCharge() == daughter->getCharge() ;
      if( !isChargeOk ) continue ;

      fillHistograms(miniTrack, trackid, id, daughter->getParticleId());
    }
  }
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::fillMatGlobPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk)
{
  StMiniMcPair* track = (StMiniMcPair*) mcevent.tracks(StEmbeddingQAUtilities::getCategory(trackid))->At(itrk) ;
  if(!track){
    Error("fillMatGlobPair", "Cannot find Matched global pairs");
    return ;
  }

  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::getCategoryName(trackid), track, mParticles->getParent()->getMass2());

  fillHistograms(miniTrack, trackid, 0, kParticleId);
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::fillRealTracks(const StMuTrack& track, const Int_t trackid, const Int_t itrk)
{
  // Fill daughters separately for the comparison with embedded tracks (Oct/21/2009)
  if ( mParticles->getNDaughter() == 0 ){
    // stable particles
    StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::getCategoryName(trackid), track, mParticles->getParent()->getMass2());
 
    fillHistograms(miniTrack, trackid, 0, kParticleId);
  }
  else{
    // decay daughters

    for(UInt_t id=0; id<mParticles->getNDaughter(); id++){
      StEmbeddingQAParticle* daughter = mParticles->getDaughter(id) ;
      StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::getCategoryName(trackid), track, daughter->getMass2());
 
      fillHistograms(miniTrack, trackid, id, daughter->getParticleId());
    }
  }

}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::fillHistograms(const StEmbeddingQATrack& track, const Int_t trackid, const Int_t iparticle, const Int_t geantId)
{
  // Track selections will be made for embedding/real tracks
  // Only pt cut will be applied for MC tracks

  // pt and eta cuts
  if( !track.isPtAndEtaOk() ) return ;

  // Use MC momentum for the embedding tracks
  //     reconstructed momentum for the real tracks
  const Double_t pt    = (track.isReal()) ? track.getPtRc()                : track.getPtMc() ;
  const Double_t mom   = (track.isReal()) ? track.getPRc()                 : track.getPMc() ;
  const Double_t eta   = (track.isReal()) ? track.getEtaRc()               : track.getEtaMc() ;
  const Double_t y     = (track.isReal()) ? track.getVectorRc().rapidity() : track.getVectorMc().rapidity() ;

  // Reconstructed momentum
  const Double_t momRc = track.getPRc() ;

  // Fill geant id
  hGeantId[trackid][iparticle]->Fill(track.getGeantId());

  // dE/dx (no PID cut)
  //  - Add NHit cut (Nov/13/2009)
  if( track.isDcaOk() && track.isNHitOk() ){
    hdEdxVsMomMc[trackid][iparticle]->Fill(mom, track.getdEdx()*1.0e+06);
    hdEdxVsMomReco[trackid][iparticle]->Fill(momRc, track.getdEdx()*1.0e+06);
  }

  //  Oct/21/2009
  // NSigma cut for real data
  // Add particle id from nSigma cuts (2 sigma)
  // for e/pi/K/p
  // 
  // Do not apply nSigma cuts for others

  if ( !track.isNSigmaOk(geantId) ) return ;

  if( track.isDcaOk() ){
    // Fill NHit points
    hNHit[trackid][iparticle]->Fill(pt, eta, track.getNHit());

    if( track.isNHitOk() ){
      const Double_t phi = track.getPhi() ;

      // dE/dx (with PID cut)
      hdEdxVsMomMcPidCut[trackid][iparticle]->Fill(mom, track.getdEdx()*1.0e+06);
      hdEdxVsMomRecoPidCut[trackid][iparticle]->Fill(momRc, track.getdEdx()*1.0e+06);

      // Correlation between reconstructed and MC momentum
      hRecoPVsMcP[trackid][iparticle]->Fill(mom, momRc);

      // Pt, eta, phi
      hPtVsEta[trackid][iparticle]->Fill(eta, pt);
      hPtVsY[trackid][iparticle]->Fill(y, pt);
      hPtVsPhi[trackid][iparticle]->Fill(phi, pt);
      hPtVsMom[trackid][iparticle]->Fill(mom, pt);
      hdPtVsPt[trackid][iparticle]->Fill(pt, pt-track.getPtMc());
      hMomVsEta[trackid][iparticle]->Fill(eta, mom);

      hEtaVsPhi[trackid][iparticle]->Fill(phi, eta);
      hEtaVsVz[trackid][iparticle]->Fill(mVz, eta);
      hYVsVz[trackid][iparticle]->Fill(mVz, y);
    }
  }

  // Fill Dca
  if( track.isNHitOk() ){
    hDca[trackid][iparticle]->Fill(pt, eta, track.getDcaGl());
  }


  if ( mDebug > 2 ){
    cout << Form("     RC:(nhit, pt, eta, phi) = (%5d, %1.4f, %1.4f, %1.4f)  MC:(pt, eta) = (%1.4f, %1.4f)",
        track.getNHit(), track.getPtRc(), track.getEtaRc(), track.getPhi(), track.getPtMc(), track.getEtaMc()
        ) << endl;
  }

}

//__________________________________________________________________________________________
Int_t StEmbeddingQAMaker::getNumberOfHistograms(const Int_t categoryId) const
{
  // Get # of histograms
  //   Contaminated pairs : # of decay daughters
  //   Real data :
  //     if ( ndecay >= 2 ) # of decay daughters
  //     else               1

  Int_t n = 0 ; // number of histograms

  if( StEmbeddingQAUtilities::getCategoryName(categoryId).Contains("CONTAM") ){
    // Contaminated pairs (daughters)
    n = mParticles->getNDaughter() ;
  }
  else if ( StEmbeddingQAUtilities::getCategoryName(categoryId).Contains("PRIMARY") 
      || StEmbeddingQAUtilities::getCategoryName(categoryId).Contains("GLOBAL") ){
    // Real data (daughter)
    n = mParticles->getNDaughter() ; // will be 0 if parent particle is stable
  }
  else{
    // Others
    n = 1 ;
  }

  // Set n = 1
  if ( n == 0 ) n = 1 ;

  return n ;
}



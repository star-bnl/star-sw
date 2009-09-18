
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

  const Float_t StEmbeddingQAMaker::kVertexCut = 30.0 ;

ClassImp(StEmbeddingQAMaker)

//__________________________________________________________________________________________
StEmbeddingQAMaker::StEmbeddingQAMaker()
  : kYear(2007), kProduction("P08ic"), kParticleId(8), kIsSimulation(kTRUE)
{
  Init() ;
}

//__________________________________________________________________________________________
StEmbeddingQAMaker::StEmbeddingQAMaker(const Int_t year, const TString production, const Int_t particleId, const Bool_t isSimulation)
  : kYear(year), kProduction(production), kParticleId(particleId), kIsSimulation(isSimulation)
{
  Init() ;
}

//__________________________________________________________________________________________
StEmbeddingQAMaker::StEmbeddingQAMaker(const Int_t year, const TString production, const TString name, const Bool_t isSimulation)
  : kYear(year), kProduction(production), kParticleId(StEmbeddingQAUtilities::GetParticleId(name)), kIsSimulation(isSimulation)
{
  Init() ;
}

//__________________________________________________________________________________________
StEmbeddingQAMaker::~StEmbeddingQAMaker()
{
  delete mParticles ;
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::Init()
{
  cout << endl;
  cout << Form("   StEmbeddingQAMaker::Init() for year       : %10d", kYear) << endl;
  cout << Form("   StEmbeddingQAMaker::Init() for production : %10s", kProduction.Data()) << endl;
  cout << Form("   StEmbeddingQAMaker::Init() for particle   : %10s (id = %3d )",
    StEmbeddingQAUtilities::GetParticleName(kParticleId).Data(), kParticleId)
    << endl;
  cout << endl;

  mOutput = 0;
  mDebug = 0;
  mMuDstMaker = 0;
  mVz = -9999.0 ;

  mDaughterPositive.clear() ;
  mDaughterNegative.clear() ;
  mDaughterNeutral.clear() ;

  mParticles = new StEmbeddingQAParticleCollection(kParticleId);
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::isZVertexOk(const StMiniMcEvent& mcevent, const Float_t vertexCut) const
{
  return TMath::Abs(mcevent.vertexZ()) < vertexCut ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::Book(const TString outputFileName)
{
  // Book histograms
  TString fileName(outputFileName);

  // Set default file name if output filename is blank
  if( fileName.IsWhitespace() ){
    const TString data = (kIsSimulation) ? "embedding" : "real" ;
    fileName = Form("embeedingqa_%s_%d_%s_%s.root", 
        data.Data(), kYear, kProduction.Data(), StEmbeddingQAUtilities::GetParticleName(kParticleId, kFALSE).Data()) ;
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

  const Int_t ptBin   = 50 ;
  const Float_t ptMin = 0.0 ;
  const Float_t ptMax = 5.0 ;

  // Track-wise informations
//  TString particleName(StEmbeddingQAUtilities::GetParticleName(kParticleId, kTRUE));
  TString particleName(mParticles->GetParent()->GetTitle());

  // Common histograms for all categories
  for(Int_t ic=0; ic<StEmbeddingQAUtilities::kNCategory; ic++){
    TString categoryTitle(StEmbeddingQAUtilities::GetCategoryTitle(ic));

    // Initialize histograms
//    const Int_t nHistogram = ( StEmbeddingQAUtilities::GetCategoryName(ic).Contains("CONTAM") )
//      ? 1 + StEmbeddingQAUtilities::GetNDaughter(kParticleId) : 1 ;
    Int_t nHistogram = ( StEmbeddingQAUtilities::GetCategoryName(ic).Contains("CONTAM") ) ? mParticles->GetNDaughter() : 1 ;
    if ( nHistogram == 0 ) nHistogram = 1 ;

    hGeantId[ic]   = new TH1*[nHistogram] ;
    hNHit[ic]      = new TH3*[nHistogram] ;
    hDca[ic]       = new TH3*[nHistogram] ;
    hPtVsEta[ic]   = new TH2*[nHistogram] ;
    hPtVsY[ic]     = new TH2*[nHistogram] ;
    hPtVsPhi[ic]   = new TH2*[nHistogram] ;
    hPtVsMom[ic]   = new TH2*[nHistogram] ;
    hdPtVsPt[ic]   = new TH2*[nHistogram] ;
    hdEdxVsMom[ic] = new TH2*[nHistogram] ;
    hEtaVsPhi[ic]  = new TH2*[nHistogram] ;
    hEtaVsVz[ic]   = new TH2*[nHistogram] ;
    hYVsVz[ic]     = new TH2*[nHistogram] ;

    for(Int_t ih=0; ih<nHistogram; ih++){

      TString nameSuffix(Form("_%d", ic));
      if ( nHistogram > 1 ) nameSuffix = Form("_%d_%d", ic, ih);

      TString name(particleName);
      if ( nHistogram > 1 ){
        name = Form("%s (decay daughter)", mParticles->GetDaughter(ih)->GetTitle().Data());
      }

      TString title(Form("(%s), %s", categoryTitle.Data(), name.Data()));

      // Geant id
      hGeantId[ic][ih] = new TH1D(Form("hGeantId%s", nameSuffix.Data()), "", 1000, 0, 1000);
      hGeantId[ic][ih]->SetXTitle("Geant id");
      hGeantId[ic][ih]->SetTitle( Form("Geant id, %s", title.Data()) );
 
      // NHit
      hNHit[ic][ih] = new TH3D(Form("hNHit%s", nameSuffix.Data()), "", 10, 0, 5, 10, -1.0, 1.0, 50, 0, 50);
      hNHit[ic][ih]->SetXTitle("N_{fit}");
      if( ic == 0 ) hNHit[ic][ih]->SetTitle( Form("N_{fit} distribution, %s", title.Data()) );
      else          hNHit[ic][ih]->SetTitle( Form("N_{fit} distribution (|dcaGl|<3 cm), %s", title.Data()) );
 
      // Dca
      hDca[ic][ih] = new TH3D(Form("hDca%s", nameSuffix.Data()), "",
          10, 0, 5, 10, -1.0, 1.0, 100, 0, 3.0);
      hDca[ic][ih]->SetXTitle("p_{T} (GeV/c)");
      hDca[ic][ih]->SetYTitle("#eta");
      hDca[ic][ih]->SetZTitle("Global dca (cm)");
      if( ic == 0 )                 hDca[ic][ih]->SetTitle( Form("Dca vs #eta vs p_{T}, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hDca[ic][ih]->SetTitle( Form("Dca vs #eta vs p_{T} (N_{fit}>=10 & N_{common}>=10), %s", title.Data()) );
      else                          hDca[ic][ih]->SetTitle( Form("Dca vs #eta vs p_{T} (N_{fit}>=10), %s", title.Data()) );
 
      // pt vs eta
      hPtVsEta[ic][ih] = new TH2D(Form("hPtVsEta%s", nameSuffix.Data()), "", 100, -1.5, 1.5, ptBin, ptMin, ptMax);
      hPtVsEta[ic][ih]->SetXTitle("#eta");
      hPtVsEta[ic][ih]->SetYTitle("p_{T} (GeV/c)");
      if( ic == 0 )                 hPtVsEta[ic][ih]->SetTitle( Form("p_{T} vs #eta, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hPtVsEta[ic][ih]->SetTitle( Form("p_{T} vs #eta (N_{fit}>=10 & N_{common}>=10 & |dcaGl|<3cm), %s", title.Data()) );
      else                          hPtVsEta[ic][ih]->SetTitle( Form("p_{T} vs #eta (N_{fit}>=10 & |dcaGl|<3cm), %s", title.Data()) );
 
      // pt vs y
      hPtVsY[ic][ih] = new TH2D(Form("hPtVsY%s", nameSuffix.Data()), "", 100, -1.5, 1.5, ptBin, ptMin, ptMax);
      hPtVsY[ic][ih]->SetXTitle("rapidity y");
      hPtVsY[ic][ih]->SetYTitle("p_{T} (GeV/c)");
      if( ic == 0 )                 hPtVsY[ic][ih]->SetTitle( Form("p_{T} vs y, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hPtVsY[ic][ih]->SetTitle( Form("p_{T} vs y (N_{fit}>=10 & N_{common}>=10 & |dcaGl|<3cm), %s", title.Data()) );
      else                          hPtVsY[ic][ih]->SetTitle( Form("p_{T} vs y (N_{fit}>=10 & |dcaGl|<3cm), %s", title.Data()) );
 
      // pt vs phi
      hPtVsPhi[ic][ih] = new TH2D(Form("hPtVsPhi%s", nameSuffix.Data()), "", 100, -TMath::Pi(), TMath::Pi(), ptBin, ptMin, ptMax);
      hPtVsPhi[ic][ih]->SetXTitle("#phi (rad)");
      hPtVsPhi[ic][ih]->SetYTitle("p_{T} (GeV/c)");
      if( ic == 0 )                 hPtVsPhi[ic][ih]->SetTitle( Form("p_{T} vs #phi, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hPtVsPhi[ic][ih]->SetTitle( Form("p_{T} vs #phi (N_{fit}>=10 & N_{common}>=10 & |dcaGl|<3cm), %s", title.Data()) );
      else                          hPtVsPhi[ic][ih]->SetTitle( Form("p_{T} vs #phi (N_{fit}>=10 & |dcaGl|<3cm), %s", title.Data()) );

      // pt vs momentum
      hPtVsMom[ic][ih] = new TH2D(Form("hPtVsMom%s", nameSuffix.Data()), "", ptBin, ptMin, ptMax, ptBin, ptMin, ptMax);
      hPtVsMom[ic][ih]->SetXTitle("momentum (GeV/c)");
      hPtVsMom[ic][ih]->SetYTitle("p_{T} (GeV/c)");
      if( ic == 0 )                 hPtVsMom[ic][ih]->SetTitle( Form("p_{T} vs momentum, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hPtVsMom[ic][ih]->SetTitle( Form("p_{T} vs momentum (N_{fit}>=10 & N_{common}>=10 & |dcaGl|<3cm), %s", title.Data()) );
      else                          hPtVsMom[ic][ih]->SetTitle( Form("p_{T} vs momentum (N_{fit}>=10 & |dcaGl|<3cm), %s", title.Data()) );
 
      // Delta pt vs pt
      hdPtVsPt[ic][ih] = new TH2D(Form("hdPtVsPt%s", nameSuffix.Data()), "", 100, -5, 5, ptBin, ptMin, ptMax);
      hdPtVsPt[ic][ih]->SetXTitle("reco. p_{T} (GeV/c)");
      hdPtVsPt[ic][ih]->SetYTitle("reco. p_{T} - mc. p_{T} (GeV/c)");
      if( ic == 0 )                 hdPtVsPt[ic][ih]->SetTitle( Form("p_{T} - p_{T} (MC) vs p_{T}, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hdPtVsPt[ic][ih]->SetTitle( Form("p_{T} - p_{T} (MC) vs p_{T} (N_{fit}>=10 & N_{common}>=10 & |dcaGl|<3cm), %s", title.Data()) );
      else                          hdPtVsPt[ic][ih]->SetTitle( Form("p_{T} - p_{T} (MC) vs p_{T} (N_{fit}>=10 & |dcaGl|<3cm), %s", title.Data()) );
 
      // dE/dx vs momentum
      hdEdxVsMom[ic][ih] = new TH2D(Form("hdEdxVsMom%s", nameSuffix.Data()), "", ptBin, ptMin, ptMax, 100, 0, 10.0);
      hdEdxVsMom[ic][ih]->SetXTitle("momentum (GeV/c)");
      hdEdxVsMom[ic][ih]->SetYTitle("dE/dx (KeV/cm)");
      if( ic == 0 ) hdEdxVsMom[ic][ih]->SetTitle( Form("dE/dx vs momentum, %s", title.Data()) );
      else          hdEdxVsMom[ic][ih]->SetTitle( Form("dE/dx vs momentum, (|dcaGl|<3cm), %s", title.Data()) );
 
      TString pt("0.2 < p_{T} < 5 GeV/c");
 
      // eta vs phi
      hEtaVsPhi[ic][ih] = new TH2D(Form("hEtaVsPhi%s", nameSuffix.Data()), "", 100, -TMath::Pi(), TMath::Pi(), 100, -1.5, 1.5);
      hEtaVsPhi[ic][ih]->SetXTitle("#phi (rad)");
      hEtaVsPhi[ic][ih]->SetYTitle("#eta");
      if( ic == 0 )                 hEtaVsPhi[ic][ih]->SetTitle( Form("#eta vs #phi, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hEtaVsPhi[ic][ih]->SetTitle( Form("#eta vs #phi (N_{fit}>=10 & N_{common}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
      else                          hEtaVsPhi[ic][ih]->SetTitle( Form("#eta vs #phi (N_{fit}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
 
      // eta vs vz
      hEtaVsVz[ic][ih] = new TH2D(Form("hEtaVsVz%s", nameSuffix.Data()), "", 120, -30, 30, 200, -1.5, 1.5);
      hEtaVsVz[ic][ih]->SetXTitle("v_{z} (cm)");
      hEtaVsVz[ic][ih]->SetYTitle("#eta");
      if( ic == 0 )                 hEtaVsVz[ic][ih]->SetTitle( Form("#eta vs v_{z}, %s", title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hEtaVsVz[ic][ih]->SetTitle( Form("#eta vs v_{z} (N_{fit}>=10 & N_{common}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
      else                          hEtaVsVz[ic][ih]->SetTitle( Form("#eta vs v_{z} (N_{fit}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
 
      // rapidity vs vz
      hYVsVz[ic][ih] = new TH2D(Form("hYVsVz%s", nameSuffix.Data()), "", 120, -30, 30, 200, -1.5, 1.5);
      hYVsVz[ic][ih]->SetXTitle("v_{z} (cm)");
      hYVsVz[ic][ih]->SetYTitle("rapidity y");
      if( ic == 0 )                 hYVsVz[ic][ih]->SetTitle( Form("rapidity y vs v_{z}, %s", pt.Data(), title.Data()) );
      else if( ic >= 1 && ic <= 3 ) hYVsVz[ic][ih]->SetTitle( Form("rapidity y vs v_{z} (N_{fit}>=10 & N_{common}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
      else                          hYVsVz[ic][ih]->SetTitle( Form("rapidity y vs v_{z} (N_{fit}>=10 & |dcaGl|<3cm), %s, %s", pt.Data(), title.Data()) );
    }
  }

  // Reconstructed pt
  hPtReco = new TH1*[mParticles->GetNDaughter()];
  for(UInt_t id=0;id<mParticles->GetNDaughter();id++){
    hPtReco[id] = new TH1D(Form("hPtReco_%d", id), Form("p_{T} (RC), %s (decay daughter)", mParticles->GetDaughter(id)->GetTitle().Data()), 
        50, 0, 5.0);
    hPtReco[id]->SetXTitle("p_{T} (RC) (GeV/c)");
  }

  // Invariant mass vs pt (only for contaminated pairs)
  //  - 1 MeV/c^2 bin up to 4 GeV/c
  for(Int_t id=0; id<2; id++){
    TString charge("Unlike sign pairs");
    if( id == 1 ) charge = "Like sign pairs" ;

    hInvMassVsPt[id] = new TH2D(Form("hInvMassVsPt_%d", id), 
        Form("Invariant mass vs p_{T}, %s", charge.Data()), ptBin, ptMin, ptMax, 4000, 0, 4.0);
    hInvMassVsPt[id]->SetXTitle("p_{T} (GeV/c)");
    hInvMassVsPt[id]->SetYTitle("Invariant mass (GeV/c^{2})");
  }

  mOutput->GetList()->Sort();
  cout << endl << endl << endl;

  return kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::Make(const TString inputFileName, const Bool_t isSimulation)
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
    FillEmbedding(inputFileName);
  }
  else{
    // Fill real data outputs from MuDST
    cout << "------------------------------------------------------------------------------------" << endl;
    cout << "            Fill real data ..." << endl;
    cout << "------------------------------------------------------------------------------------" << endl;
    FillRealData(inputFileName);
  }

  return kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::FillEmbedding(const TString inputFileName)
{
  // Open input file
  TFile* file = TFile::Open(inputFileName);
  if( !file || !file->IsOpen() ){
    Error("StEmbeddingQAMaker::FillEmbedding", "can't open %s", inputFileName.Data());
    return kFALSE ;
  }

  const TString treeName("StMiniMcTree");
  TTree* tree = (TTree*) file->Get(treeName);
  if( !tree ){
    Error("StEmbeddingQAMaker::FillEmbedding", "can't find %s tree", treeName.Data());
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
    // Clear daughter's array
    mDaughterPositive.clear() ;
    mDaughterNegative.clear() ;
    mDaughterNeutral.clear() ;

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
          Error("FillEmbedding", "Unkown branch id, id=%3d", trackid);
          return kFALSE ;
      }

      if( mDebug && ievent % 200 == 0 ){
        cout << Form("####  event=%4d, category=%10s, ntrack=%10d",
            ievent, StEmbeddingQAUtilities::GetCategoryName(trackid).Data(), nTrack)
          << endl;
      }

      for(Int_t itrk=0; itrk<nTrack; itrk++){
        switch ( trackid ) {
          case 0: FillMcTracks(*mMiniMcEvent, trackid, itrk); break ;
          case 1: FillMatchedPairs(*mMiniMcEvent, trackid, itrk); break ;
          case 2: FillGhostPairs(*mMiniMcEvent, trackid, itrk); break ;
          case 3: FillContamPairs(*mMiniMcEvent, trackid, itrk); break ;
          case 4: FillMatGlobPairs(*mMiniMcEvent, trackid, itrk); break ;
          default:
            Error("FillEmbedding",  "Unkown branch id, id=%3d", trackid);
            return kFALSE ;
        }
      }// track loop

      // Make invariant mass for contaminated pairs
      if( trackid == StEmbeddingQAUtilities::GetCategoryId("CONTAM") ){
        FillPair() ;
      }

    }// Track category
  }// event loop

  delete mMiniMcEvent ;
  file->Close() ;

  return kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::FillRealData(const TString inputFileName)
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
            mMuDstMaker->GetFileName(), ievent, StEmbeddingQAUtilities::GetCategoryName(trackid).Data())
          << endl;
      }

      TObjArray* tracks = (ic==0) 
        ?  mMuDstMaker->muDst()->primaryTracks()   // Primary tracks
        :  mMuDstMaker->muDst()->globalTracks() ;  // Global tracks

      TObjArrayIter trackIterator(tracks);
      StMuTrack* track = 0;
      Int_t itrk = 0;
      while ( ( track = (StMuTrack*) trackIterator.Next() ) ){
        FillRealTracks(*track, trackid, itrk);
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
Bool_t StEmbeddingQAMaker::RunRealData(const TString inputFileList)
{
  if( mMuDstMaker ) delete mMuDstMaker ;

  cout << "###    Read " << inputFileList << endl;
  mMuDstMaker  = new StMuDstMaker(0, 0, "", inputFileList, "MuDst", 1000);

  Make(inputFileList, kFALSE);

  return kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::RunEmbedding(const TString inputFileList)
{
  ifstream fEmbedding(inputFileList);
  if(!fEmbedding){
    Error("StEmbeddingQAMaker::Run", "can't find %s", inputFileList.Data());
    return kFALSE ;
  }
    cout << "###    Read " << inputFileList << endl;

  // Fill embedding outputs
  TString file ;
  while( fEmbedding >> file ){
    cout << "####     Read file : " << file << endl;
    Make(file, kTRUE);
  }

  return kTRUE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::Run(const TString inputFileList)
{
  if( kIsSimulation ){
    // Embedding QA
    return RunEmbedding(inputFileList) ;
  }
  else{
    // Real data QA
    return RunRealData(inputFileList) ;
  }

  return kFALSE ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::End()
{
  cout << "    End of StEmbeddingQAMaker" << endl;
  cout << "    Write output " << mOutput->GetName() << endl;
  mOutput->Write();
  mOutput->Close();

  return kTRUE ;
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::FillMcTracks(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk)
{
  StTinyMcTrack* track = (StTinyMcTrack*) mcevent.tracks(StEmbeddingQAUtilities::GetCategory(trackid))->At(itrk) ;
  if(!track){
    Error("FillMcTracks", "Cannot find MC tracks");
    return ;
  }

  // Make sure geant id is equal to the input particle id
  if( track->geantId() != mParticles->GetParent()->GetParticleId() ) return ;

//  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), *track, StEmbeddingQAUtilities::GetMass2(kParticleId)) ;
  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), *track, mParticles->GetParent()->GetMass2());

  FillHistograms(miniTrack, trackid, 0);
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::FillMatchedPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk)
{
  StMiniMcPair* track = (StMiniMcPair*) mcevent.tracks(StEmbeddingQAUtilities::GetCategory(trackid))->At(itrk) ;
  if(!track){
    Error("FillMatchedPairs", "Cannot find Matched pairs");
    return ;
  }

//  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), track, StEmbeddingQAUtilities::GetMass2(kParticleId)) ;
  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), track, mParticles->GetParent()->GetMass2());

  FillHistograms(miniTrack, trackid, 0);
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::FillGhostPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk)
{
  StMiniMcPair* track = (StMiniMcPair*) mcevent.tracks(StEmbeddingQAUtilities::GetCategory(trackid))->At(itrk) ;
  if(!track){
    // Suppress error messege for the recent embedding samples. There are no ghost tracks
//    Error("FillGhostPairs", "Cannot find Ghost pairs");
    return ;
  }

//  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), track, StEmbeddingQAUtilities::GetMass2(kParticleId)) ;
  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), track, mParticles->GetParent()->GetMass2());
  FillHistograms(miniTrack, trackid, 0);
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::FillContamPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk)
{
  StContamPair* track = (StContamPair*) mcevent.tracks(StEmbeddingQAUtilities::GetCategory(trackid))->At(itrk) ;
  if(!track){
    Error("FillContamPairs", "Cannot find Contaminated pairs");
    return ;
  }

//  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), track, StEmbeddingQAUtilities::GetMass2(kParticleId)) ;
  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), track, mParticles->GetParent()->GetMass2());

  for(UInt_t id=0; id<mParticles->GetNDaughter(); id++){
    StEmbeddingQAParticle* daughter = mParticles->GetDaughter(id) ;

    // Daughter id selection
    if ( miniTrack.GetGeantId() == daughter->GetParticleId() ){
      // Make sure daughter particles have correct charge
      const Bool_t isChargeOk = miniTrack.GetCharge() == daughter->GetCharge() ;
      if( !isChargeOk ) continue ;

//      cout << Form("id=%3d, itrk=%3d, Fill daughter geantid=(%3d, %3d), charge=(%3d, %3d)",
//          id, itrk, miniTrack.GetGeantId(), daughter->GetParticleId(),
//          miniTrack.GetCharge(), daughter->GetCharge())
//        << endl;
      FillHistograms(miniTrack, trackid, id);

      // Store arrays for invariant mass
      // NOTE:
      //   Track selections may be different for different analysis.
      const Bool_t isTrackOk = GetTrackSelectionForDaughters(miniTrack);
      if( !isTrackOk ) continue ;

      hPtReco[id]->Fill(miniTrack.GetPtRc());

      if( miniTrack.GetParentGeantId() == kParticleId ) { // Make sure that parent id in contaminated pair is equal to the given particle id
        if( miniTrack.GetCharge() == 0 && daughter->GetCharge() == 0 ){ // neutral
          mDaughterNeutral.push_back( new StEmbeddingQATrack("neutral", track, daughter->GetMass2()) );
        }
        else if ( miniTrack.GetCharge() > 0 && daughter->GetCharge() > 0 ){ // positive
          mDaughterPositive.push_back( new StEmbeddingQATrack("positive", track, daughter->GetMass2()) );
        }
        else if ( miniTrack.GetCharge() < 0 && daughter->GetCharge() < 0 ){ // negative
          mDaughterNegative.push_back( new StEmbeddingQATrack("negative", track, daughter->GetMass2()) );
        }
        else{ // this should not be happend. something is wrong
          Error("FillContamPairs", Form("Charge is different between data and input, (data, input) = (%3d, %3d). abort()",
                miniTrack.GetCharge(), daughter->GetCharge()));
          abort();
        }

      }
    }
  }

  // Fill daughter's array for invariant mass
//  if( miniTrack.GetParentGeantId() == kParticleId ) { // Make sure that parent id in contaminated pair is equal to the given particle id
//    if( miniTrack.GetCharge() == 0 ){
//      mDaughterNeutral.push_back( new StEmbeddingQATrack("neutral", track, StEmbeddingQAUtilities::GetMass2Daughter(kParticleId, kTRUE)) );
//    }
//    else if( miniTrack.GetCharge() > 0 ){
//      mDaughterPositive.push_back( new StEmbeddingQATrack("positive", track, StEmbeddingQAUtilities::GetMass2Daughter(kParticleId, kTRUE)) );
//    }
//    else{
//      mDaughterNegative.push_back( new StEmbeddingQATrack("negative", track, StEmbeddingQAUtilities::GetMass2Daughter(kParticleId, kFALSE)) );
//    }
//  }
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::FillMatGlobPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk)
{
  StMiniMcPair* track = (StMiniMcPair*) mcevent.tracks(StEmbeddingQAUtilities::GetCategory(trackid))->At(itrk) ;
  if(!track){
    Error("FillMatGlobPair", "Cannot find Matched global pairs");
    return ;
  }

//  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), track, StEmbeddingQAUtilities::GetMass2(kParticleId)) ;
  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), track, mParticles->GetParent()->GetMass2());
  FillHistograms(miniTrack, trackid, 0);
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::FillRealTracks(const StMuTrack& track, const Int_t trackid, const Int_t itrk)
{
//  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), track, StEmbeddingQAUtilities::GetMass2(kParticleId)) ;
  StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::GetCategoryName(trackid), track, mParticles->GetParent()->GetMass2());

  FillHistograms(miniTrack, trackid, 0);
}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::FillHistograms(const StEmbeddingQATrack& track, const Int_t trackid, const Int_t iparticle)
{
  // Track selections will be made for embedding/real tracks
  // Only pt cut will be applied for MC tracks

  // pt and eta cuts
  if( !track.IsPtAndEtaOk() ) return ;

  const Double_t pt  = (track.IsMc()) ? track.GetPtMc()  : track.GetPtRc() ;
  const Double_t mom = (track.IsMc()) ? track.GetPMc()   : track.GetPRc() ;
  const Double_t eta = (track.IsMc()) ? track.GetEtaMc() : track.GetEtaRc() ;
  const Double_t y   = (track.IsMc()) ? track.GetVectorMc().rapidity() : track.GetVectorRc().rapidity() ;

  // Fill geant id
  hGeantId[trackid][iparticle]->Fill(track.GetGeantId());

  if( track.IsDcaOk() ){
    // Fill NHit points
    hNHit[trackid][iparticle]->Fill(pt, eta, track.GetNHit());

    // dE/dx
    hdEdxVsMom[trackid][iparticle]->Fill(mom, track.GetdEdx()*1.0e+06);

    // Pt, eta, phi
    if( track.IsNHitOk() ){
      const Double_t phi = track.GetPhi() ;

      hPtVsEta[trackid][iparticle]->Fill(eta, pt);
      hPtVsY[trackid][iparticle]->Fill(y, pt);
      hPtVsPhi[trackid][iparticle]->Fill(phi, pt);
      hPtVsMom[trackid][iparticle]->Fill(phi, mom);
      hdPtVsPt[trackid][iparticle]->Fill(pt, pt-track.GetPtMc());

      if( pt >= 0.2 && pt < 5.0 ){
        hEtaVsPhi[trackid][iparticle]->Fill(phi, eta);
        hEtaVsVz[trackid][iparticle]->Fill(mVz, eta);
        hYVsVz[trackid][iparticle]->Fill(mVz, y);
      }
    }
  }

  // Fill Dca
  if( track.IsNHitOk() ){
    hDca[trackid][iparticle]->Fill(pt, eta, track.GetDcaGl());
  }


  if ( mDebug > 2 ){
    cout << Form("     RC:(nhit, pt, eta, phi) = (%5d, %1.4f, %1.4f, %1.4f)  MC:(pt, eta) = (%1.4f, %1.4f)",
        track.GetNHit(), track.GetPtRc(), track.GetEtaRc(), track.GetPhi(), track.GetPtMc(), track.GetEtaMc()
        ) << endl;
  }

}

//__________________________________________________________________________________________
void StEmbeddingQAMaker::FillPair()
{
  for(Int_t ich=0; ich<3; ich++){
    // id    pair
    // 0     Unlike sign
    // 1     Like sign (positive)
    // 2     Like sign (negative)
    vector<StEmbeddingQATrack*> pair0 ;
    vector<StEmbeddingQATrack*> pair1 ;
    UInt_t nPairStop = 0 ;
    Int_t histoId = -1 ;

    switch ( ich ) {
      case 0:
        histoId = 0 ;
        pair0 = mDaughterPositive ;
        pair1 = mDaughterNegative ; nPairStop = pair1.size() ;
        break ;

      case 1:
        histoId = 1 ;
        pair0 = mDaughterPositive ;
        pair1 = mDaughterPositive ; nPairStop = pair1.size() - 1;
        break ;

      case 2:
        histoId = 1 ;
        pair0 = mDaughterNegative ;
        pair1 = mDaughterNegative ; nPairStop = pair1.size() - 1;
        break ;
    }

    for(UInt_t itrk=0; itrk<pair0.size(); itrk++){
      const UInt_t nPairStart = (ich==0) ? 0 : itrk + 1 ;

      for(UInt_t jtrk=nPairStart; jtrk<nPairStop; jtrk++){
        // Reconstructed parent
        const StLorentzVectorD parent = pair0[itrk]->GetVectorRc() + pair1[jtrk]->GetVectorRc() ;

        hInvMassVsPt[histoId]->Fill(parent.perp(), parent.m());
      }
    }
  }

}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAMaker::GetTrackSelectionForDaughters(const StEmbeddingQATrack& track) const
{
  // Track selection for daughters
  //  NOTE:
  //    Cuts are probably different from different analysis.
  //    We may need to implement specific track selections for each analysis
  //
  const Bool_t isD0CuCu2005 = kYear == 2005 && (kProduction == "P06ib" || kProduction == "P07ie")
    && kParticleId == StEmbeddingQAUtilities::GetParticleId("D0") ;

  Bool_t isTrackOk = kFALSE ;
  if ( isD0CuCu2005 ){ // D0 in Cu + Cu at 200 GeV (2005)
    isTrackOk = track.GetNCommonHit() > 7 && track.GetNHit() > 15
      && ((Double_t)track.GetNHitPoss()/(Double_t)track.GetNHit()>0.55)
      && track.GetPtRc() >= 0.15
      && TMath::Abs(track.GetEtaRc()) < 1.0
      ;
  }
  else{ // default track selection
    isTrackOk = track.GetNCommonHit() > 10 && track.GetNHit() > 20
      && ((Double_t)track.GetNHitPoss()/(Double_t)track.GetNHit()>0.55)
      && track.GetPtRc() >= 0.20
      && TMath::Abs(track.GetEtaRc()) < 1.0
      ;
  }

  return isTrackOk ;
}


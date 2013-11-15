/****************************************************************************************************
 * $Id: StEmbeddingQA.cxx,v 1.1 2009/12/22 21:41:18 hmasui Exp $
 * $Log: StEmbeddingQA.cxx,v $
 * Revision 1.1  2009/12/22 21:41:18  hmasui
 * Change class name from StEmbeddingQAMaker to StEmbeddingQA
 *
 ****************************************************************************************************/

#include <algorithm>
#include <fstream>
#include <string>

#include "TClonesArray.h"
#include "TError.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TTree.h"
#include "TObjArray.h"

#include "StMessMgr.h"
#include "StMiniMcEvent/StMiniMcEvent.h"
#include "StMiniMcEvent/StTinyMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StParticleDefinition.hh"
#include "StParticleTable.hh"

#include "StEmbeddingQA.h"
#include "StEmbeddingQATrack.h"

using namespace std ;

ClassImp(StEmbeddingQA)

//__________________________________________________________________________________________
StEmbeddingQA::StEmbeddingQA()
  : mYear(2007), mProduction("P08ic"), mIsSimulation(kTRUE)
{
  /// Default constructor
  /// Default year, production are 2007 and P08ic, and default mIsSimulation flag is kTRUE

  init() ;
}

//__________________________________________________________________________________________
StEmbeddingQA::StEmbeddingQA(const Int_t year, const TString production, const Bool_t isSimulation)
  : mYear(year), mProduction(production), mIsSimulation(isSimulation)
{
  /// Define year, production from the input arguments

  init() ;
}

//__________________________________________________________________________________________
StEmbeddingQA::~StEmbeddingQA()
{
  clear();
}

//__________________________________________________________________________________________
void StEmbeddingQA::clear()
{
  /// Clear all histograms
  LOG_DEBUG << "StEmbeddingQA::clear()" << endm;

  /// Delete histograms if they have been defined before
  if ( mhVz ) delete mhVz ;
  if ( mhVzAccepted ) delete mhVzAccepted ;
  if ( mhVyVx ) delete mhVyVx ;
  if ( mhdVx ) delete mhdVx ;
  if ( mhdVy ) delete mhdVy ;
  if ( mhdVz ) delete mhdVz ;

  /// Clear geantid histogram and all maps
  for(UInt_t ic=0; ic<StEmbeddingQAConst::mNCategory; ic++){
    if ( mhGeantId[ic] ) delete mhGeantId[ic] ;

    mGeantId[ic].clear();
    mhNHit[ic].clear();
    mhNCommonHitVsNHit[ic].clear();
    mhDca[ic].clear();
    mhPtVsEta[ic].clear();
    mhPtVsY[ic].clear();
    mhPtVsPhi[ic].clear();
    mhPtVsMom[ic].clear();
    mhdPtVsPt[ic].clear();
    mhMomVsEta[ic].clear();
    mhdEdxVsMomMc[ic].clear();
    mhdEdxVsMomMcPidCut[ic].clear();
    mhdEdxVsMomReco[ic].clear();
    mhdEdxVsMomRecoPidCut[ic].clear();
    mhRecoPVsMcP[ic].clear();
    mhNCommonHitVsNHit[ic].clear();
    mhEtaVsPhi[ic].clear();
    mhEtaVsVz[ic].clear();
    mhYVsVz[ic].clear();
  }
}

//__________________________________________________________________________________________
void StEmbeddingQA::init()
{
  /// Initialization of data members

  LOG_INFO << endm;
  LOG_INFO << Form("   StEmbeddingQA::init() for year       : %10d", mYear) << endm;
  LOG_INFO << Form("   StEmbeddingQA::init() for production : %10s", mProduction.Data()) << endm;
  LOG_INFO << endm;

  mMuDstMaker = 0;

  /// default z-vertex cut
  mVertexCut = 30.0;

  mOutput = 0;
  mVz = -9999. ;

  mhVz = 0 ;
  mhVzAccepted = 0 ;
  mhVyVx = 0 ;
  mhdVx = 0 ;
  mhdVy = 0 ;
  mhdVz = 0 ;

  for(UInt_t ic=0; ic<StEmbeddingQAConst::mNCategory; ic++){
    mhGeantId[ic] = 0 ;
  }

  /// Clear all histograms
  clear();
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::isZVertexOk(const StMiniMcEvent& mcevent) const
{
  /// Apply z-vertex cut for embedding track nodes
  return TMath::Abs(mcevent.vertexZ()) < mVertexCut ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::book(const TString outputFileName)
{
  /// Book event-wise histograms, and geantid histogram
  LOG_DEBUG << "StEmbeddingQA::book()" << endm;

  TString fileName(outputFileName);

  /// Set default file name if output filename is blank
  /// Output filename will be qa_{data}_{year}_{production}.root
  /// where data is 'embedding' or 'real'
  if( fileName.IsWhitespace() ){
    const TString data = (mIsSimulation) ? "embedding" : "real" ;
    fileName = Form("qa_%s_%d_%s.root", data.Data(), mYear, mProduction.Data());
  }

  /// Open output file
  mOutput = TFile::Open(fileName, "recreate");
  mOutput->cd();
  LOG_INFO << "    OPEN " << mOutput->GetName() << endm;

  /// NOTE:
  ///  Initialize only event-wise histograms at this point.
  ///  Track-wise histograms will be defined for the particles
  ///  found in either MC or reconstructed tracks
  ///  in expandHistograms()
  StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;

  // Event-wise informations
  mhVz         = new TH1D("hVz", "z-vertex", 100, -50, 50);
  mhVzAccepted = new TH1D("hVzAccepted", "z-vertex with z-vertex cut", 100, -50, 50);
  mhVz->SetXTitle("v_{z} (cm)");
  mhVzAccepted->SetXTitle("v_{z} (cm)");

  utility->setStyle(mhVz);
  utility->setStyle(mhVzAccepted);

  mhVyVx = new TH2D("hVyVx", "v_{y} vs v_{x}", 100, -10, 10, 100, -10, 10);
  mhVyVx->SetXTitle("v_{x} (cm)");
  mhVyVx->SetYTitle("v_{y} (cm)");

  utility->setStyle(mhVyVx);

  mhdVx = new TH1D("hdVx", "#Delta x = v_{x} - v_{x}(MC)", 100, -10+0.5, 10+0.5);
  mhdVy = new TH1D("hdVy", "#Delta y = v_{y} - v_{y}(MC)", 100, -10+0.5, 10+0.5);
  mhdVz = new TH1D("hdVz", "#Delta z = v_{z} - v_{z}(MC)", 100, -10+0.5, 10+0.5);
  mhdVx->SetXTitle("#Deltav_{x} = v_{x} - v_{x}(MC) (cm)");
  mhdVy->SetXTitle("#Deltav_{y} = v_{y} - v_{y}(MC) (cm)");
  mhdVz->SetXTitle("#Deltav_{z} = v_{z} - v_{z}(MC) (cm)");

  utility->setStyle(mhdVx);
  utility->setStyle(mhdVy);
  utility->setStyle(mhdVz);

  // Initialize geantid histogram

  for(UInt_t ic=0; ic<StEmbeddingQAConst::mNCategory; ic++){
    mhGeantId[ic] = new TH1D(Form("hGeantId_%d", ic), Form("Geantid, %s", utility->getCategoryTitle(ic).Data()), 1000, 0, 1000) ;
    mhGeantId[ic]->SetXTitle("Geantid");

    utility->setStyle(mhGeantId[ic]);
  }

  mOutput->GetList()->Sort();
  LOG_INFO << endm << endm << endm;

  return kStOk ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::make(const TString inputFileName, const Bool_t isSimulation)
{
  /// Fill histograms for either embedding or real data

  /// Check output file has already opened. If not, do not fill output histograms.
  if(!mOutput || !mOutput->IsOpen()){
    Error("StEmbeddingQA::Make", "Output file is not opened");
    return kStErr ;
  }

  if( isSimulation ){
    /// Fill embedding outputs from minimc tree
    LOG_INFO << "------------------------------------------------------------------------------------" << endm;
    LOG_INFO << "            Fill embedding ..." << endm;
    LOG_INFO << "------------------------------------------------------------------------------------" << endm;
    fillEmbedding(inputFileName);
  }
  else{
    /// Fill real data outputs from MuDST
    LOG_INFO << "------------------------------------------------------------------------------------" << endm;
    LOG_INFO << "            Fill real data ..." << endm;
    LOG_INFO << "------------------------------------------------------------------------------------" << endm;
    fillRealData(inputFileName);
  }

  return kStOk ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::fillEmbedding(const TString inputFileName)
{
  /// Read minimc (embedding) and fill histograms

  /// Open input ROOT file
  TFile* file = TFile::Open(inputFileName);
  if( !file || !file->IsOpen() ){
    Error("StEmbeddingQA::fillEmbedding", "can't open %s", inputFileName.Data());
    return kStErr ;
  }

  /// Get StMiniMcTree from the input file
  const TString treeName("StMiniMcTree");
  TTree* tree = (TTree*) file->Get(treeName);
  if( !tree ){
    Error("StEmbeddingQA::fillEmbedding", "can't find %s tree", treeName.Data());
    return kStErr ;
  }

  /// Get StMiniMcEvent node from StMiniMcTree
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
  LOG_INFO << Form("    OPEN %s,  # of event = %10d", inputFileName.Data(), nentries) << endm;

  /// Loop over all events
  for (Int_t ievent=0; ievent<nentries;ievent++) {
    tree->GetEntry(ievent);

    const Float_t vx   = mMiniMcEvent->vertexX() ;
    const Float_t vy   = mMiniMcEvent->vertexY() ;
    const Float_t vz   = mMiniMcEvent->vertexZ() ;
    const Float_t vxmc = mMiniMcEvent->mcVertexX() ;
    const Float_t vymc = mMiniMcEvent->mcVertexY() ;
    const Float_t vzmc = mMiniMcEvent->mcVertexZ() ;
    mhVz->Fill(vz);

    mVz = vz ;

    /// z-vertex cut
    if( !isZVertexOk(*mMiniMcEvent) ) continue ;

    mhVzAccepted->Fill(vz);
    mhVyVx->Fill(vx, vy);
    mhdVx->Fill( vx - vxmc );
    mhdVy->Fill( vy - vymc );
    mhdVz->Fill( vz - vzmc );

    /// Get MC, MATCHED, GHOST, CONTAM and MATGLOB pairs
    for(UInt_t categoryid=0; categoryid<StEmbeddingQAConst::mNEmbedding; categoryid++){
      const Int_t nTrack = getNtrack(categoryid, *mMiniMcEvent) ;

      if( ievent % 1000 == 0 ){
        LOG_INFO << Form("####  event=%4d, category=%10s, ntrack=%10d",
            ievent, StEmbeddingQAUtilities::instance()->getCategoryName(categoryid).Data(), nTrack)
          << endm;
      }

      /// Fill track-wise histograms
      for(Int_t itrk=0; itrk<nTrack; itrk++){
        fillEmbeddingTracks(*mMiniMcEvent, categoryid, itrk) ;
      } // Track loop
    }// Track category

  }// event loop

  /// delete StMiniMcEvent
  delete mMiniMcEvent ;

  /// Close input file
  file->Close() ;

  return kStOk ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::fillRealData(const TString inputFileName)
{
  /// Read muDst (real data) and fill histograms

  /// Loop over all events from muDst
  Int_t ievent = 0;
  while( !mMuDstMaker->Make() ){ //read event
    StMuEvent* muEvent = mMuDstMaker->muDst()->event() ;
    if(!muEvent) continue ;

    /// Vertex cut for real data
    /// |vz| < 30 cm. Avoid (vx,vy,vx)=(0,0,0) and |vx|,|vy|,|vz| > 1000
    const Double_t vx = muEvent->primaryVertexPosition().x() ;
    const Double_t vy = muEvent->primaryVertexPosition().y() ;
    const Double_t vz = muEvent->primaryVertexPosition().z() ;
    const Bool_t isVertexBad = TMath::Abs(vz) >= mVertexCut
      || ( TMath::Abs(vx) < 1.0e-5 && TMath::Abs(vy) < 1.0e-5 && TMath::Abs(vz) < 1.0e-5 )
      || ( TMath::Abs(vx) > 1000 || TMath::Abs(vy) > 1000 || TMath::Abs(vz) > 1000 )
      ;
    if( isVertexBad ) continue ;

    mhVz->Fill(vz);
    mhVyVx->Fill(vx, vy);

    mVz = vz ;

    /// Loop over both Global and Primary tracks
    for(UInt_t ic=0; ic<StEmbeddingQAConst::mNReal; ic++){
      const Int_t categoryid = ic + StEmbeddingQAConst::mNEmbedding ;

      if( ievent % 1000 == 0 ){
        LOG_INFO << Form("%85s ####  event=%4d, category=%10s",
            mMuDstMaker->GetFileName(), ievent, StEmbeddingQAUtilities::instance()->getCategoryName(categoryid).Data())
          << endm;
      }

      const TObjArray* tracks = (ic==0) 
        ?  mMuDstMaker->muDst()->primaryTracks()   // Primary tracks
        :  mMuDstMaker->muDst()->globalTracks() ;  // Global tracks

      TObjArrayIter trackIterator(tracks);
      StMuTrack* track = 0;
      Int_t itrk = 0;
      while ( ( track = (StMuTrack*) trackIterator.Next() ) ){
        fillRealTracks(*track, categoryid, itrk);
        itrk++;
      }
    }

    ievent++;
  }// event loop

  LOG_INFO << "End of real data" << endm;
  LOG_INFO << "Total # of events = " << ievent << endm;

  return kStOk ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::runRealData(const TString inputFileList)
{
  /// Read input muDst file list and fill histograms

  /// Delete StMuDstMaker if it has been defined before
  if( mMuDstMaker ) delete mMuDstMaker ;

  LOG_INFO << "###    Read " << inputFileList << endm;
  mMuDstMaker  = new StMuDstMaker(0, 0, "", inputFileList, "MuDst", 1000);

  /// Define histograms for electrons, pions, kaons and protons for the real data.
  LOG_INFO << "StEmbeddingQA::runRealData()" << endm;
  LOG_INFO << "Add electrons, pions, kaons and protons for the real data QA" << endm;
  for(UInt_t ic=0; ic<StEmbeddingQAConst::mNReal; ic++){
    const Int_t categoryid = ic + StEmbeddingQAConst::mNEmbedding ;

    const Short_t parentid = 0 ; // real tracks are assumed to be primary
    expandHistograms(categoryid, 2, parentid);
    expandHistograms(categoryid, 3, parentid);
    expandHistograms(categoryid, 8, parentid);
    expandHistograms(categoryid, 9, parentid);
    expandHistograms(categoryid, 11, parentid);
    expandHistograms(categoryid, 12, parentid);
    expandHistograms(categoryid, 14, parentid);
    expandHistograms(categoryid, 15, parentid);

    // Make sure the input particle list
    for(vector<Short_t>::iterator iter = mGeantId[categoryid].begin(); iter != mGeantId[categoryid].end(); iter++){
      const Short_t geantid = (*iter) ;
      LOG_DEBUG << Form("  Input geant id = %10d,  name = %10s", geantid,
          StParticleTable::instance()->findParticleByGeantId(geantid)->name().c_str()) << endm ;
    }
  }

  make(inputFileList, kFALSE);

  return kStOk ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::runEmbedding(const TString inputFileList)
{
  /// Read input minimc list and fill histograms

  ifstream fEmbedding(inputFileList);
  if(!fEmbedding){
    Error("StEmbeddingQA::runEmbedding", "can't find %s", inputFileList.Data());
    return kFALSE ;
  }
  LOG_INFO << "###    Read " << inputFileList << endm;

  /// Fill embedding outputs
  TString file ;
  while( fEmbedding >> file ){
    LOG_INFO << "####     Read file : " << file << endm;
    make(file, kTRUE);
  }

  return kStOk ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::run(const TString inputFileList)
{
  /// Read either muDst or minimc file list and fill histograms

  LOG_INFO << "StEmbeddingQA::run()" << endm ;
  LOG_INFO << "  z-vertex cut is |vz| < " << mVertexCut << endm;

  if( mIsSimulation ){
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
Bool_t StEmbeddingQA::end() const
{
  /// Close output ROOT file

  LOG_INFO << "    End of StEmbeddingQA" << endm;
  LOG_INFO << "    Write output " << mOutput->GetName() << endm;
  mOutput->GetList()->Sort();

  mOutput->Write();
  mOutput->Close();

  return kStOk ;
}

//__________________________________________________________________________________________
void StEmbeddingQA::fillEmbeddingTracks(const StMiniMcEvent& mcevent, const Int_t categoryid, const Int_t itrk)
{
  /// Fill embedding data for several different minimc nodes

  /// Get embedding data (StEmbeddingQATrack)
  StEmbeddingQATrack* miniTrack = getEmbeddingQATrack(mcevent, categoryid, itrk);

  /// Do not fill histograms unless geantid was found (see getEmbeddingQATrack())
  if(!miniTrack) return ;

  /// Fill histograms
  fillHistograms(*miniTrack, categoryid);

  /// Delete StEmbeedingQATrack
  delete miniTrack ;
}

//____________________________________________________________________________________________________
StEmbeddingQATrack* StEmbeddingQA::getEmbeddingQATrack(const StMiniMcEvent& mcevent, const Int_t categoryid, const Int_t itrk)
{
  /// Get embedding QA track (StEmbeddingQATrack) from minimc branches
  StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance();

  const TString name(utility->getCategoryName(categoryid));
  const Category category(utility->getCategory(categoryid));

  if ( utility->isMc(name) ){
    /// Get MC tracks
    const StTinyMcTrack* track = (StTinyMcTrack*) mcevent.tracks(category)->At(itrk) ;

    /// Make sure that the input geantid exists in StParticleTable
    if ( !isGeantIdOk(*track) ){
      LOG_DEBUG << Form("StEmbeddingQA::getEmbeddingQATrack", "No geantid = %3d exists in StParticleTable", track->geantId()) << endm ;
      return 0;
    }

    return (new StEmbeddingQATrack(name, *track));
  }
  else if ( utility->isMatched(name) || utility->isGhost(name) || utility->isMatchedGlobal(name) ){
    /// Get Matched or Ghost or Matched global pairs
    StMiniMcPair* track = (StMiniMcPair*) mcevent.tracks(category)->At(itrk) ;

    /// Make sure that the input geantid exists in StParticleTable
    if ( !isGeantIdOk(*track) ){
      LOG_DEBUG << Form("StEmbeddingQA::getEmbeddingQATrack", "No geantid = %3d exists in StParticleTable", track->geantId()) << endm ;
      return 0;
    }

    return (new StEmbeddingQATrack(name, track));
  }
  else if ( utility->isContaminated(name) ){
    /// Get Contaminated pairs
    StContamPair* track = (StContamPair*) mcevent.tracks(category)->At(itrk) ;

    /// Make sure that the input geantid exists in StParticleTable
    if ( !isGeantIdOk(*track) ){
      LOG_DEBUG << Form("StEmbeddingQA::getEmbeddingQATrack", "No geantid = %3d exists in StParticleTable", track->geantId()) << endm ;
      return 0;
    }

    return (new StEmbeddingQATrack(name, track));
  }
  else{
    Warning("StEmbeddingQA::fillEmbeddingTracks", "Unknown category id, id=%3d", categoryid);
    return 0;
  }

  return 0;
}

//__________________________________________________________________________________________
void StEmbeddingQA::fillRealTracks(const StMuTrack& track, const Int_t categoryid, const Int_t itrk)
{
  /// Loop over all registered particles (real tracks)
  for(vector<Short_t>::iterator iter = mGeantId[categoryid].begin(); iter != mGeantId[categoryid].end(); iter++){
    const Short_t geantid = (*iter) ;
    StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::instance()->getCategoryName(categoryid), track, geantid);
 
    fillHistograms(miniTrack, categoryid);
  }
}

//__________________________________________________________________________________________
void StEmbeddingQA::fillHistograms(const StEmbeddingQATrack& track, const Int_t categoryid)
{
  /// Fill track-wise histograms

  /// Track selections will be made for embedding/real tracks
  /// Only pt cut will be applied for MC tracks

  /// do not fill histograms if geantid < 0
  const Short_t geantid = track.getGeantId() ;
  if ( geantid < 0 ) return ;

  /// pt and eta cuts (see StEmbeddingQATrack::isPtAndEtaOk())
  if( !track.isPtAndEtaOk() ) return ;

  /// Use MC momentum for the embedding tracks, reconstructed momentum for the real tracks
  StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  const Double_t pt    = (utility->isReal(track.getName())) ? track.getPtRc()                : track.getPtMc() ;
  const Double_t mom   = (utility->isReal(track.getName())) ? track.getPRc()                 : track.getPMc() ;
  const Double_t eta   = (utility->isReal(track.getName())) ? track.getEtaRc()               : track.getEtaMc() ;
  const Double_t y     = (utility->isReal(track.getName())) ? track.getVectorRc().rapidity() : track.getVectorMc().rapidity() ;

  // Reconstructed momentum
  const Double_t momRc = track.getPRc() ;

  /// Check geant id
  ///   if it's new, expands the histogram array for it
  ///   if it has already exist, do nothing
  expandHistograms(categoryid, geantid, track.getParentGeantId());

  // Fill geant id
  mhGeantId[categoryid]->Fill(track.getGeantId());

  // dE/dx (no PID cut)
  //  - Add NHit cut (Nov/13/2009)
  if( track.isDcaOk() && track.isNHitOk() ){
    mhdEdxVsMomMc[categoryid][geantid]->Fill(mom, track.getdEdxkeV());
    mhdEdxVsMomReco[categoryid][geantid]->Fill(momRc, track.getdEdxkeV());
  }

  //  Oct/21/2009
  /// NSigma cut for real data
  /// Add particle id from nSigma cuts (2 sigma) for e/pi/K/p
  /// Do not apply nSigma cuts for others

  if ( !track.isNSigmaOk(geantid) ) return ;

  if( track.isDcaOk() ){
    // Fill NHit points
    mhNHit[categoryid][geantid]->Fill(pt, eta, track.getNHit());

    if( track.isNHitOk() ){
      const Double_t phi = track.getPhi() ;

      // Fill Ncommon hits vs Nhits
      mhNCommonHitVsNHit[categoryid][geantid]->Fill(track.getNHit(), track.getNCommonHit());
 
      // dE/dx (with PID cut)
      mhdEdxVsMomMcPidCut[categoryid][geantid]->Fill(mom, track.getdEdxkeV());
      mhdEdxVsMomRecoPidCut[categoryid][geantid]->Fill(momRc, track.getdEdxkeV());
 
      // Correlation between reconstructed and MC momentum
      mhRecoPVsMcP[categoryid][geantid]->Fill(mom, momRc);
 
      // Pt, eta, phi
      mhPtVsEta[categoryid][geantid]->Fill(eta, pt);
      mhPtVsY[categoryid][geantid]->Fill(y, pt);
      mhPtVsPhi[categoryid][geantid]->Fill(phi, pt);
      mhPtVsMom[categoryid][geantid]->Fill(mom, pt);
      mhdPtVsPt[categoryid][geantid]->Fill(pt, pt-track.getPtMc());
      mhMomVsEta[categoryid][geantid]->Fill(eta, mom);
 
      mhEtaVsPhi[categoryid][geantid]->Fill(phi, eta);
      mhEtaVsVz[categoryid][geantid]->Fill(mVz, eta);
      mhYVsVz[categoryid][geantid]->Fill(mVz, y);
    }
  }

  // Fill Dca
  if( track.isNHitOk() ){
    mhDca[categoryid][geantid]->Fill(pt, eta, track.getDcaGl());
  }

  LOG_DEBUG << Form("     RC:(nfit, pt, eta, phi) = (%5d, %1.4f, %1.4f, %1.4f)  MC:(pt, eta) = (%1.4f, %1.4f)",
      track.getNHit(), track.getPtRc(), track.getEtaRc(), track.getPhi(), track.getPtMc(), track.getEtaMc()
      ) << endm;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::pushBackGeantId(const Int_t categoryid, const Short_t geantid)
{
  /// Add geantid if it's new. If we have already had input geantid in the array, do nothing.

  if ( mGeantId[categoryid].empty() ){
    /// Push back a new geantid if geantid array is empty
    LOG_INFO << Form("StEmbeddingQA::pushBackGeantId()   Find a new geant id,  geantid = %5d (%10s)", 
        geantid, StEmbeddingQAUtilities::instance()->getCategoryName(categoryid).Data()) << endm;
    mGeantId[categoryid].push_back(geantid);
  }
  else{
    /// Expand histogrm by checking the geant id
    ///   if it's new, expands the histogram array for it
    ///   if it has already exist, do nothing
    const vector<Short_t>::iterator iter = find(mGeantId[categoryid].begin(), mGeantId[categoryid].end(), geantid) ;
 
    if ( iter != mGeantId[categoryid].end() ){
      /// Geant id already exist. do nothing
      return kFALSE;
    }
    else{
      /// Find a new geant id, store id in mGeantId array
      LOG_INFO << Form("StEmbeddingQA::pushBackGeantId()   Find a new geant id,  geantid = %5d (%10s)", 
          geantid, StEmbeddingQAUtilities::instance()->getCategoryName(categoryid).Data()) << endm;
      mGeantId[categoryid].push_back(geantid);
    }
  }

  return kTRUE ;
}


//__________________________________________________________________________________________
void StEmbeddingQA::expandHistograms(const Int_t categoryid, const Short_t geantid, const Short_t parentid)
{
  /// Push back geant id if the input geantid is new (return true)
  /// If the input geant id has already stored in mGeantId array, do nothing (return false)
  if ( !pushBackGeantId(categoryid, geantid) ) return ;

  // Expand histograms
  mOutput->cd();

  const Int_t ptBin    = 100 ;
  const Float_t ptMin  = 0.0 ;
  const Float_t ptMax  = 10.0 ;
  const Int_t etaBin   = 100 ;
  const Float_t etaMin = -2.5 ;
  const Float_t etaMax =  2.5 ;

  /// Add parent geantid information if the particle is decay daughters
  StParticleTable* table = StParticleTable::instance() ;
  StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  const Char_t* categoryTitle(utility->getCategoryTitle(categoryid).Data());

  /// Suffix for each histogram. Put parentid if we have decay daughters
  const TString nameSuffix = (parentid>0) ? Form("_%d_%d_%d", categoryid, parentid, geantid) : Form("_%d_%d", categoryid, geantid);

  /// Category title and particle name put in the histogram title
  const TString CategoryAndGeantId = (parentid>0) ?  Form("%s (%s from %s)", categoryTitle, 
      table->findParticleByGeantId(geantid)->name().c_str(), table->findParticleByGeantId(parentid)->name().c_str())
    : Form("%s (%s)", categoryTitle, table->findParticleByGeantId(geantid)->name().c_str());

  const TString categoryName(utility->getCategoryName(categoryid));
  const Bool_t isMc        = utility->isMc(categoryName);
  const Bool_t isEmbedding = utility->isEmbedding(categoryName);

  // NHit vs eta vs MC pt
  TString title(Form("N_{fit} distribution (|dcaGl|<3cm), %s", CategoryAndGeantId.Data()));
  if( isMc ) title = Form("N_{fit} distribution, %s", CategoryAndGeantId.Data());
  TH3* hNhit = new TH3D(Form("hNHit%s", nameSuffix.Data()), title, 10, 0, 5, 10, -1.0, 1.0, 50, 0, 50) ;
  hNhit->SetXTitle("MC p_{T} (GeV/c)");
  hNhit->SetYTitle("#eta");
  hNhit->SetZTitle("N_{fit}");
  utility->setStyle(hNhit);
  mhNHit[categoryid].insert( pair<Int_t, TH3*>(geantid, hNhit) );

  // Ncommon hit vs Nfit
  title = Form("N_{common} hit vs N_{fit} (|dcaGl|<3cm), %s", CategoryAndGeantId.Data());
  if( isMc ) title = Form("N_{common} hit vs N_{fit}, %s", CategoryAndGeantId.Data());
  TH2* hNCommonHitVsNHit = new TH2D(Form("hNCommonHitVsNHit%s", nameSuffix.Data()), title, 50, 0, 50, 50, 0, 50) ;
  hNCommonHitVsNHit->SetXTitle("N_{fit}");
  hNCommonHitVsNHit->SetYTitle("N_{common}");
  utility->setStyle(hNCommonHitVsNHit);
  mhNCommonHitVsNHit[categoryid].insert( pair<Int_t, TH2*>(geantid, hNCommonHitVsNHit) );

  // Dca vs eta vs MC pt
  title = Form("Dca vs #eta vs MC p_{T} (N_{fit}#geq10), %s", CategoryAndGeantId.Data());
  if( isMc ) title = Form("Dca vs #eta vs MC p_{T}, %s", CategoryAndGeantId.Data());
  else if ( isEmbedding ) title = Form("Dca vs #eta vs MC p_{T} (N_{fit}#geq10 & N_{common}#geq10), %s", CategoryAndGeantId.Data());

  TH3* hDca = new TH3D(Form("hDca%s", nameSuffix.Data()), title, 10, 0, 5, 10, -1.0, 1.0, 100, 0, 3.0);
  hDca->SetXTitle("MC p_{T} (GeV/c)");
  hDca->SetYTitle("#eta");
  hDca->SetZTitle("Global dca (cm)");
  utility->setStyle(hDca);
  mhDca[categoryid].insert( pair<Int_t, TH3*>(geantid, hDca) );

  //--------------------------------------------------
  // Common cuts for the 
  //  - pt vs eta
  //  - pt vs y
  //  - pt vs phi
  //  - pt vs momentum
  //  - delta pt vs pt
  //  - momentum vs eta
  //  - dE/dx vs momentum
  //  - reco. momentum vs MC momentum
  //  - eta vs phi
  //  - eta vs vz
  //  - rapidity vs vz
  //
  //  - dE/dx vs momentum (with pid cuts for the real data)
  //--------------------------------------------------
  TString cut(" (N_{fit}#geq10 & |dcaGl|<3cm)"); // real data
  if( isMc ) cut = "";
  else if ( isEmbedding ) cut = "(N_{fit}#geq10 & N_{common}#geq10 & |dcaGl|<3cm)";

  const TString titleSuffix(Form("%s, %s", cut.Data(), CategoryAndGeantId.Data()));

  // pt vs eta
  TH2* hPtVsEta = new TH2D(Form("hPtVsEta%s", nameSuffix.Data()), Form("MC p_{T} vs #eta%s", titleSuffix.Data()),
      etaBin, etaMin, etaMax, ptBin, ptMin, ptMax);
  hPtVsEta->SetXTitle("#eta");
  hPtVsEta->SetYTitle("MC p_{T} (GeV/c)");
  utility->setStyle(hPtVsEta);
  mhPtVsEta[categoryid].insert( pair<Int_t, TH2*>(geantid, hPtVsEta) );

  // pt vs y
  TH2* hPtVsY = new TH2D(Form("hPtVsY%s", nameSuffix.Data()), Form("MC p_{T} vs y%s", titleSuffix.Data()),
      etaBin, etaMin, etaMax, ptBin, ptMin, ptMax);
  hPtVsY->SetXTitle("rapidity y");
  hPtVsY->SetYTitle("MC p_{T} (GeV/c)");
  utility->setStyle(hPtVsY);
  mhPtVsY[categoryid].insert( pair<Int_t, TH2*>(geantid, hPtVsY) );

  // pt vs phi
  TH2* hPtVsPhi = new TH2D(Form("hPtVsPhi%s", nameSuffix.Data()), Form("MC p_{T} vs #phi%s", titleSuffix.Data()),
      100, -TMath::Pi(), TMath::Pi(), ptBin, ptMin, ptMax);
  hPtVsPhi->SetXTitle("#phi (rad)");
  hPtVsPhi->SetYTitle("MC p_{T} (GeV/c)");
  utility->setStyle(hPtVsPhi);
  mhPtVsPhi[categoryid].insert( pair<Int_t, TH2*>(geantid, hPtVsPhi) );

  // pt vs momentum
  TH2* hPtVsMom = new TH2D(Form("hPtVsMom%s", nameSuffix.Data()), Form("MC p_{T} vs momentum%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, ptBin, ptMin, ptMax);
  hPtVsMom->SetXTitle("momentum (GeV/c)");
  hPtVsMom->SetYTitle("MC p_{T} (GeV/c)");
  utility->setStyle(hPtVsMom);
  mhPtVsMom[categoryid].insert( pair<Int_t, TH2*>(geantid, hPtVsMom) );

  // Delta pt vs pt
  TH2* hdPtVsPt = new TH2D(Form("hdPtVsPt%s", nameSuffix.Data()), Form("p_{T} - p_{T} (MC) vs p_{T}%s", titleSuffix.Data()),
      100, -5, 5, ptBin, ptMin, ptMax);
  hdPtVsPt->SetXTitle("reco. p_{T} (GeV/c)");
  hdPtVsPt->SetYTitle("reco. p_{T} - MC p_{T} (GeV/c)");
  utility->setStyle(hdPtVsPt);
  mhdPtVsPt[categoryid].insert( pair<Int_t, TH2*>(geantid, hdPtVsPt) );

  // momentum vs eta
  TH2* hMomVsEta = new TH2D(Form("hMomVsEta%s", nameSuffix.Data()), Form("Momentum vs #eta%s", titleSuffix.Data()),
      etaBin, etaMin, etaMax, ptBin, ptMin, ptMax);
  hMomVsEta->SetXTitle("#eta");
  hMomVsEta->SetYTitle("momentum (GeV/c)");
  utility->setStyle(hMomVsEta);
  mhMomVsEta[categoryid].insert( pair<Int_t, TH2*>(geantid, hMomVsEta) );

  // dE/dx vs MC momentum
  TH2* hdEdxVsMomMc = new TH2D(Form("hdEdxVsMomMc%s", nameSuffix.Data()), Form("dE/dx vs MC p%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, 100, 0, 10.0);
  hdEdxVsMomMc->SetXTitle("MC p (GeV/c)");
  hdEdxVsMomMc->SetYTitle("dE/dx (keV/cm)");
  utility->setStyle(hdEdxVsMomMc);
  mhdEdxVsMomMc[categoryid].insert( pair<Int_t, TH2*>(geantid, hdEdxVsMomMc) );

  // dE/dx vs reconstructed momentum
  TH2* hdEdxVsMomReco = new TH2D(Form("hdEdxVsMomReco%s", nameSuffix.Data()), Form("dE/dx vs Reconstructed p%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, 100, 0, 10.0);
  hdEdxVsMomReco->SetXTitle("Reconstructed p (GeV/c)");
  hdEdxVsMomReco->SetYTitle("dE/dx (keV/cm)");
  utility->setStyle(hdEdxVsMomReco);
  mhdEdxVsMomReco[categoryid].insert( pair<Int_t, TH2*>(geantid, hdEdxVsMomReco) );

  // Reconstructed momentum vs MC momentum
  TH2* hRecoPVsMcP = new TH2D(Form("hRecoPVsMcP%s", nameSuffix.Data()), Form("Reconstructed p vs MC p%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, ptBin, ptMin, ptMax);
  hRecoPVsMcP->SetXTitle("MC p (GeV/c)");
  hRecoPVsMcP->SetYTitle("Reconstructed p (GeV/c)");
  utility->setStyle(hRecoPVsMcP);
  mhRecoPVsMcP[categoryid].insert( pair<Int_t, TH2*>(geantid, hRecoPVsMcP) );

  // eta vs phi
  TH2* hEtaVsPhi = new TH2D(Form("hEtaVsPhi%s", nameSuffix.Data()), Form("#eta vs #phi%s", titleSuffix.Data()),
      100, -TMath::Pi(), TMath::Pi(), etaBin, etaMin, etaMax);
  hEtaVsPhi->SetXTitle("#phi (rad)");
  hEtaVsPhi->SetYTitle("#eta");
  utility->setStyle(hEtaVsPhi);
  mhRecoPVsMcP[categoryid].insert( pair<Int_t, TH2*>(geantid, hRecoPVsMcP) );
  mhEtaVsPhi[categoryid].insert( pair<Int_t, TH2*>(geantid, hEtaVsPhi) );

  // eta vs vz
  TH2* hEtaVsVz = new TH2D(Form("hEtaVsVz%s", nameSuffix.Data()), Form("#eta vs v_{z}%s", titleSuffix.Data()),
      200, -50, 50, 200, etaMin, etaMax);
  hEtaVsVz->SetXTitle("v_{z} (cm)");
  hEtaVsVz->SetYTitle("#eta");
  utility->setStyle(hEtaVsVz);
  mhEtaVsVz[categoryid].insert( pair<Int_t, TH2*>(geantid, hEtaVsVz) );
 
  // rapidity vs vz
  TH2* hYVsVz = new TH2D(Form("hYVsVz%s", nameSuffix.Data()), Form("rapidity y vs v_{z}%s", titleSuffix.Data()),
      200, -50, 50, 200, etaMin, etaMax);
  hYVsVz->SetXTitle("v_{z} (cm)");
  hYVsVz->SetYTitle("rapidity y");
  utility->setStyle(hYVsVz);
  mhYVsVz[categoryid].insert( pair<Int_t, TH2*>(geantid, hYVsVz) );

  // dE/dx vs MC momentum (with pid cut)
  TH2* hdEdxVsMomMcPidCut = new TH2D(Form("hdEdxVsMomMcPidCut%s", nameSuffix.Data()), Form("dE/dx vs MC p (with 2#sigma pid cut)%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, 100, 0, 10.0);
  hdEdxVsMomMcPidCut->SetXTitle("MC p (GeV/c)");
  hdEdxVsMomMcPidCut->SetYTitle("dE/dx (keV/cm)");
  utility->setStyle(hdEdxVsMomMcPidCut);
  mhdEdxVsMomMcPidCut[categoryid].insert( pair<Int_t, TH2*>(geantid, hdEdxVsMomMcPidCut) );

  // dE/dx vs Reconstructed momentum (with pid cut)
  TH2* hdEdxVsMomRecoPidCut = new TH2D(Form("hdEdxVsMomRecoPidCut%s", nameSuffix.Data()), 
      Form("dE/dx vs Reconstructed p (with 2#sigma pid cut)%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, 100, 0, 10.0);
  hdEdxVsMomRecoPidCut->SetXTitle("Reconstructed p (GeV/c)");
  hdEdxVsMomRecoPidCut->SetYTitle("dE/dx (keV/cm)");
  utility->setStyle(hdEdxVsMomRecoPidCut);
  mhdEdxVsMomRecoPidCut[categoryid].insert( pair<Int_t, TH2*>(geantid, hdEdxVsMomRecoPidCut) );
}

//__________________________________________________________________________________________
Int_t StEmbeddingQA::getNtrack(const Int_t categoryid, const StMiniMcEvent& mcevent) const
{
  switch ( categoryid ) {
    case 0: return mcevent.nMcTrack() ;
    case 1: return mcevent.nMatchedPair() ;
    case 2: return mcevent.nGhostPair() ;
    case 3: return mcevent.nContamPair() ;
    // NOTE:
    //   Currently, there is no function to get number of matched global paris in StMiniMcEvent
    //   Skip it
    case 4: return 0 ;
    default:
      Warning("getNtrack", "Unkown category id, id=%3d", categoryid);
      return 0 ;
  }

  return 0 ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::isGeantIdOk(const StTinyMcTrack& track) const
{
  /// Check geant id in StTinyMcTrack
  ///  can be used for the StMiniMcPair and StContamPair
  ///  since both of them inherit from StTinyMcTrack

  return StParticleTable::instance()->containsGeantId(track.geantId()) ;
}


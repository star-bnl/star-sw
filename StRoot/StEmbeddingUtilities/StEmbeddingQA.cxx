/****************************************************************************************************
 * $Id: StEmbeddingQA.cxx,v 1.24 2019/07/10 05:44:17 zhux Exp $
 * $Log: StEmbeddingQA.cxx,v $
 * Revision 1.24  2019/07/10 05:44:17  zhux
 * added option for btof pid for primary real tracks
 *
 * Revision 1.23  2016/10/27 15:50:20  zhux
 * added an option to set the maximum pT cut, by Zachariah Miller
 *
 * Revision 1.22  2012/03/05 10:32:19  cpowell
 * Functions added to cut on refMult
 *
 * Revision 1.21  2011/08/31 18:04:48  cpowell
 * Extended the range of hGeantId
 *
 * Revision 1.20  2011/04/01 05:05:49  hmasui
 * Track selections by StEmbeddingQAUtilities. Added 1/pt(RC)-1/pt(MC) vs pt, and pt dependent Ncommon vs NhitFit histograms
 *
 * Revision 1.19  2011/02/11 03:55:46  hmasui
 * Change geantid type to integer
 *
 * Revision 1.18  2011/01/31 21:32:12  hmasui
 * Modify histogram keys to TString to take into account parent geantid
 *
 * Revision 1.17  2011/01/14 23:46:16  hmasui
 * Add Ncommon hit cut for NHitFit histograms
 *
 * Revision 1.16  2011/01/12 21:36:29  hmasui
 * Add nHitsFit/nHitsPoss cut
 *
 * Revision 1.15  2010/11/01 03:10:23  hmasui
 * Modify geantid check for MC tracks in order to avoid non primary tracks
 *
 * Revision 1.14  2010/07/12 21:30:23  hmasui
 * Use StEmbeddingQAUtilities::getParticleDefinition(). Increase bin size, maximum for geantid histogram in order to cover geantid > 10k
 *
 * Revision 1.13  2010/06/28 17:33:47  hmasui
 * Added geant process = 6 (photon pair production) in contaminated pairs
 *
 * Revision 1.12  2010/05/14 19:50:12  hmasui
 * Add rapidity and trigger cuts.
 *
 * Revision 1.11  2010/04/24 20:21:21  hmasui
 * Add geant process check for contaminated pairs
 *
 * Revision 1.10  2010/03/15 21:03:56  hmasui
 * Modify binning for histograms of vertices
 *
 * Revision 1.9  2010/03/12 19:27:02  hmasui
 * Reduce bin size for event number histograms, and allow automatic bin extention
 *
 * Revision 1.8  2010/02/19 18:06:39  hmasui
 * Change the vertex range to +/-200 cm for vz histograms
 *
 * Revision 1.7  2010/02/16 02:13:34  hmasui
 * Add parent-parent geant id in the histogram name
 *
 * Revision 1.6  2010/02/12 16:24:13  hmasui
 * Extend the range of vz to +/-150cm for vx(vy) vs vz histograms
 *
 * Revision 1.5  2010/02/01 21:28:14  hmasui
 * Fix bugs for the binning of delta vx, vy, vz histograms
 *
 * Revision 1.4  2010/01/28 21:50:35  hmasui
 * Add Vx vs Vz and Vy vs Vz histograms.
 *
 * Revision 1.3  2010/01/26 18:07:33  hmasui
 * Change the binning for delta v_{x,y,z} from +/- 10 to +/- 1 cm
 *
 * Revision 1.2  2010/01/26 17:47:33  hmasui
 * Add histograms for eventid, runnumber and # of particles. Fix binning for delta pt vs pt
 *
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
  if ( mhRef ) delete mhRef ;
  if ( mhRefAccepted ) delete mhRefAccepted ;
  if ( mhVz ) delete mhVz ;
  if ( mhVzAccepted ) delete mhVzAccepted ;
  if ( mhVyVx ) delete mhVyVx ;
  if ( mhVxVz ) delete mhVxVz ;
  if ( mhVyVz ) delete mhVyVz ;
  if ( mhdVx ) delete mhdVx ;
  if ( mhdVy ) delete mhdVy ;
  if ( mhdVz ) delete mhdVz ;
  if ( mhEventId ) delete mhEventId ;
  if ( mhRunNumber ) delete mhRunNumber ;

  mGeantIdCollection.clear();

  /// Clear geantid histogram and all maps
  for(UInt_t ic=0; ic<StEmbeddingQAConst::mNCategory; ic++){
    if ( mhGeantId[ic] ) delete mhGeantId[ic] ;
    if ( mhNParticles[ic] ) delete mhNParticles[ic] ;

    mGeantId[ic].clear();
    mhNHit[ic].clear();
    mhNCommonHitVsNHit[ic].clear();
    mhDca[ic].clear();
    mhPtVsEta[ic].clear();
    mhPtVsY[ic].clear();
    mhPtVsPhi[ic].clear();
    mhPtVsMom[ic].clear();
    mhdPtVsPt[ic].clear();
    mhdInvPtVsPt[ic].clear();
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

  mOutput = 0;
  mVz = -9999. ;

  mhRef = 0 ;
  mhRefAccepted = 0 ;
  mhVz = 0 ;
  mhVzAccepted = 0 ;
  mhVyVx = 0 ;
  mhVxVz = 0 ;
  mhVyVz = 0 ;
  mhdVx = 0 ;
  mhdVy = 0 ;
  mhdVz = 0 ;
  mhEventId = 0 ;
  mhRunNumber = 0 ;
  mPtMax = 10. ;

  for(UInt_t ic=0; ic<StEmbeddingQAConst::mNCategory; ic++){
    mhNParticles[ic] = 0 ;
    mhGeantId[ic] = 0 ;
  }

  /// Clear all histograms
  clear();
}

//__________________________________________________________________________________________
void StEmbeddingQA::setRefMultMinCut(const Int_t refMultMin)
{
  StEmbeddingQAUtilities::instance()->setRefMultMinCut(refMultMin) ;
}

//__________________________________________________________________________________________
void StEmbeddingQA::setRefMultMaxCut(const Int_t refMultMax)
{
  StEmbeddingQAUtilities::instance()->setRefMultMaxCut(refMultMax) ;
}

//__________________________________________________________________________________________
void StEmbeddingQA::setZVertexCut(const Float_t vz)
{
  StEmbeddingQAUtilities::instance()->setZVertexCut(vz) ;
}

//__________________________________________________________________________________________
void StEmbeddingQA::addTriggerIdCut(const UInt_t id)
{
  /// Add trigger id in the array (NOTE: will be used for real data only)

  StEmbeddingQAUtilities::instance()->addTriggerIdCut(id) ;
}

//__________________________________________________________________________________________
void StEmbeddingQA::setRapidityCut(const Float_t ycut)
{
  /// Set rapidity cut
  StEmbeddingQAUtilities::instance()->setRapidityCut(ycut) ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::isRefMultOk(const StMiniMcEvent& mcevent) const
{
  /// Apply refMult cut for embedding track nodes
  return StEmbeddingQAUtilities::instance()->isRefMultOk(mcevent.nUncorrectedPrimaries()) ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::isZVertexOk(const StMiniMcEvent& mcevent) const
{
  /// Apply z-vertex cut for embedding track nodes
  return StEmbeddingQAUtilities::instance()->isZVertexOk(mcevent.vertexZ()) ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::isTriggerOk(StMuEvent* event) const
{
  /// return true if no trigger is found
  const vector<UInt_t> triggerId(StEmbeddingQAUtilities::instance()->getTriggerIdCut());
  if( triggerId.empty() ) return kTRUE ;

  /// Assume one trigger per event. Need to be revised if we have multiple triggers per event
  for(UInt_t i=0; i<triggerId.size(); i++){
    if( event->triggerIdCollection().nominal().isTrigger( triggerId[i] ) ){
      LOG_DEBUG << "StEmbeddingQA::isTriggerOk  Trigger found: " << triggerId[i] << endm ;
      return kTRUE ;
    }
  }

  return kFALSE ;
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
  mhRef         = new TH1D("hRef", "refMult", 1000, 0, 1000);
  mhRefAccepted = new TH1D("hRefAccepted", "refMult", 1000, 0, 1000);
  mhVz         = new TH1D("hVz", "z-vertex", 400, -200, 200);
  mhVzAccepted = new TH1D("hVzAccepted", "z-vertex with z-vertex cut", 400, -200, 200);
  mhRef->SetXTitle("refMult");
  mhRefAccepted->SetXTitle("refMult");
  mhVz->SetXTitle("v_{z} (cm)");
  mhVzAccepted->SetXTitle("v_{z} (cm)");

  utility->setStyle(mhRef);
  utility->setStyle(mhRefAccepted);
  utility->setStyle(mhVz);
  utility->setStyle(mhVzAccepted);

  // Bins for vertices
  const Int_t vtxBin     = 800 ;
  const Double_t vtxMin  = -2.0 ;
  const Double_t vtxMax  =  2.0 ;
  const Double_t binSize = (vtxMax-vtxMin)/static_cast<Double_t>(vtxBin);

  // Shift vertices range by binSize/2 to locate v=0 at the middle of 0-th bin
  const Double_t vtxMinShift = vtxMin - binSize/2.0 ;
  const Double_t vtxMaxShift = vtxMax - binSize/2.0 ;

  mhVyVx = new TH2D("hVyVx", "v_{y} vs v_{x}", vtxBin, vtxMinShift, vtxMaxShift, vtxBin, vtxMinShift, vtxMaxShift);
  mhVxVz = new TH2D("hVxVz", "v_{x} vs v_{z}", 300, -150, 150, vtxBin, vtxMinShift, vtxMaxShift);
  mhVyVz = new TH2D("hVyVz", "v_{y} vs v_{z}", 300, -150, 150, vtxBin, vtxMinShift, vtxMaxShift);
  mhVyVx->SetXTitle("v_{x} (cm)"); mhVyVx->SetYTitle("v_{y} (cm)");
  mhVxVz->SetXTitle("v_{z} (cm)"); mhVxVz->SetYTitle("v_{x} (cm)");
  mhVyVz->SetXTitle("v_{z} (cm)"); mhVyVz->SetYTitle("v_{y} (cm)");

  utility->setStyle(mhVyVx);
  utility->setStyle(mhVxVz);
  utility->setStyle(mhVyVz);

  mhdVx = new TH1D("hdVx", "#Delta x = v_{x} - v_{x}(MC)",  vtxBin, vtxMinShift, vtxMaxShift);
  mhdVy = new TH1D("hdVy", "#Delta y = v_{y} - v_{y}(MC)",  vtxBin, vtxMinShift, vtxMaxShift);
  mhdVz = new TH1D("hdVz", "#Delta z = v_{z} - v_{z}(MC)",  vtxBin, vtxMinShift, vtxMaxShift);
  mhdVx->SetXTitle("#Deltav_{x} = v_{x} - v_{x}(MC) (cm)");
  mhdVy->SetXTitle("#Deltav_{y} = v_{y} - v_{y}(MC) (cm)");
  mhdVz->SetXTitle("#Deltav_{z} = v_{z} - v_{z}(MC) (cm)");

  utility->setStyle(mhdVx);
  utility->setStyle(mhdVy);
  utility->setStyle(mhdVz);

  mhEventId   = new TH1D("hEventId", "Event id", 1000, 0, 1000);
  mhRunNumber = new TH1D("hRunNumber", "Run id - (Year - 1999)#times10^{6}", 400000, 0, 400000);
  mhEventId->SetXTitle("Event id");
  mhRunNumber->SetXTitle("Run number");

  // Set bit to automatic bin extention
  mhEventId->SetBit(TH1::kCanRebin);

  utility->setStyle( mhEventId   ) ;
  utility->setStyle( mhRunNumber ) ;

  for(UInt_t ic=0; ic<StEmbeddingQAConst::mNCategory; ic++){
    mhNParticles[ic] = new TH1D(Form("hNParticles_%d", ic), 
        Form("Number of particles per event, %s", utility->getCategoryTitle(ic).Data()), 1000, 0, 1000);
    mhNParticles[ic]->SetXTitle("# of particles / event");

    utility->setStyle(mhNParticles[ic]);

    // Initialize geantid histogram. Increase the bin and maximum in order to cover id > 10k
    mhGeantId[ic] = new TH1D(Form("hGeantId_%d", ic), Form("Geantid, %s", utility->getCategoryTitle(ic).Data()), 100000, 0, 100000) ;
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
    const Int_t nprimaries = mMiniMcEvent->nUncorrectedPrimaries(); 

    mhRef->Fill(nprimaries);
    mhVz->Fill(vz);

    mVz = vz ;

    /// z-vertex cut
    if( !isZVertexOk(*mMiniMcEvent) ) continue ;

    /// refMult cut
    if( !isRefMultOk(*mMiniMcEvent) ) continue ;

    /// Event vertecies
    mhRefAccepted->Fill(nprimaries);
    mhVzAccepted->Fill(vz);
    mhVyVx->Fill(vx, vy);
    mhVxVz->Fill(vz, vx);
    mhVyVz->Fill(vz, vy);
    mhdVx->Fill( vx - vxmc );
    mhdVy->Fill( vy - vymc );
    mhdVz->Fill( vz - vzmc );

    /// Event id, trigger id, run number and # of particles
    mhEventId->Fill( mMiniMcEvent->eventId() );
    mhRunNumber->Fill( StEmbeddingQAUtilities::instance()->getRunNumber(mMiniMcEvent->runId(), mYear) );

    /// Get MC, MATCHED, GHOST, CONTAM and MATGLOB pairs
    for(UInt_t categoryid=0; categoryid<StEmbeddingQAConst::mNEmbedding; categoryid++){
      const Int_t nTrack = getNtrack(categoryid, *mMiniMcEvent) ;
      mhNParticles[categoryid]->Fill(nTrack);

      const Int_t nevents = (Int_t)mhVz->GetEntries();
      if( nevents % 100 == 0 && nTrack > 0 ){
        LOG_INFO << Form("####  accept/throw=%10d/%10d, category=%10s, ntrack=%10d",
            (Int_t)mhVzAccepted->GetEntries(), nevents, 
            StEmbeddingQAUtilities::instance()->getCategoryName(categoryid).Data(), nTrack)
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
  Int_t ieventAccept = 0;
  while( !mMuDstMaker->Make() ){ //read event
    ievent++;
    StMuEvent* muEvent = mMuDstMaker->muDst()->event() ;
    if(!muEvent) continue ;

    /// Vertex cut for real data
    /// |vz| < 30 cm. Avoid (vx,vy,vx)=(0,0,0) and |vx|,|vy|,|vz| > 1000
    const Double_t vx = muEvent->primaryVertexPosition().x() ;
    const Double_t vy = muEvent->primaryVertexPosition().y() ;
    const Double_t vz = muEvent->primaryVertexPosition().z() ;
    const Int_t refMult = muEvent->refMult() ;
    mhRef->Fill(refMult);
//    const Bool_t isVertexBad = TMath::Abs(vz) >= mVertexCut
    const Bool_t isVertexBad = !StEmbeddingQAUtilities::instance()->isZVertexOk(vz)
      || !StEmbeddingQAUtilities::instance()->isRefMultOk(refMult)
      || ( TMath::Abs(vx) < 1.0e-5 && TMath::Abs(vy) < 1.0e-5 && TMath::Abs(vz) < 1.0e-5 )
      || ( TMath::Abs(vx) > 1000 || TMath::Abs(vy) > 1000 || TMath::Abs(vz) > 1000 )
      ;
    if( isVertexBad ) continue ;

    /// Trigger id cut
    if( !isTriggerOk(muEvent) ) continue ;

    mhRefAccepted->Fill(refMult);
    mhVz->Fill(vz);
    mhVyVx->Fill(vx, vy);

    mVz = vz ;

    /// Loop over both Global and Primary tracks
    for(UInt_t ic=0; ic<StEmbeddingQAConst::mNReal; ic++){
      const Int_t categoryid = ic + StEmbeddingQAConst::mNEmbedding ;

      if( ieventAccept % 100 == 0 ){
        LOG_INFO << Form("%85s ####  accept/throw=%10d/%10d, category=%10s",
            mMuDstMaker->GetFileName(), ieventAccept, ievent, StEmbeddingQAUtilities::instance()->getCategoryName(categoryid).Data())
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

    ieventAccept++;
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

    const Int_t parentid = 0 ;       // real tracks are assumed to be primary
    const Int_t parentparentid = 0 ; // real tracks are assumed to be primary
    const Int_t geantprocess = 0 ;
    expandHistograms(categoryid, 2, parentid, parentparentid, geantprocess);
    expandHistograms(categoryid, 3, parentid, parentparentid, geantprocess);
    expandHistograms(categoryid, 8, parentid, parentparentid, geantprocess);
    expandHistograms(categoryid, 9, parentid, parentparentid, geantprocess);
    expandHistograms(categoryid, 11, parentid, parentparentid, geantprocess);
    expandHistograms(categoryid, 12, parentid, parentparentid, geantprocess);
    expandHistograms(categoryid, 14, parentid, parentparentid, geantprocess);
    expandHistograms(categoryid, 15, parentid, parentparentid, geantprocess);

    // Make sure the input particle list
    for(vector<Int_t>::iterator iter = mGeantId[categoryid].begin(); iter != mGeantId[categoryid].end(); iter++){
      const Int_t geantid = (*iter) ;
      LOG_DEBUG << Form("  Input geant id = %10d,  name = %10s", geantid,
          StEmbeddingQAUtilities::instance()->getParticleDefinition(geantid)->name().c_str()) << endm ;
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
//  LOG_INFO << "  z-vertex cut is |vz| < " << mVertexCut << endm;
  StEmbeddingQAUtilities::instance()->PrintCuts() ;

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

    /// Remove contamination from MC tracks
    if ( track->isPrimary() != 1 ){
      LOG_DEBUG << Form("StEmbeddingQA::getEmbeddingQATrack", "MC track with GeantID %3d is not a primary track", track->geantId()) << endm ;
      return 0;
    }

    return (new StEmbeddingQATrack(name, *track));
  }
  else if ( utility->isMatched(name) || utility->isGhost(name) || utility->isMatchedGlobal(name) ){
    /// Get Matched or Ghost or Matched global pairs
    StMiniMcPair* track = (StMiniMcPair*) mcevent.tracks(category)->At(itrk) ;

    /// Make sure that the input geantid exists in StParticleTable
    if ( !utility->isGeantIdOk(track->geantId()) ){
      LOG_DEBUG << Form("StEmbeddingQA::getEmbeddingQATrack", "No geantid = %3d exists in StParticleTable", track->geantId()) << endm ;
      return 0;
    }

    return (new StEmbeddingQATrack(name, track));
  }
  else if ( utility->isContaminated(name) ){
    /// Get Contaminated pairs
    StContamPair* track = (StContamPair*) mcevent.tracks(category)->At(itrk) ;

    /// Make sure that the input geantid exists in StParticleTable
    if ( !utility->isGeantIdOk(track->geantId()) ){
      LOG_DEBUG << Form("StEmbeddingQA::getEmbeddingQATrack", "No geantid = %3d exists in StParticleTable", track->geantId()) << endm ;
      return 0;
    }

    /// Make sure geantprocess = 5 (DECAY) for nomal decay daughters
    ///     or    geantprocess = 6 (PAIR) for photon conversion
    const Bool_t isGeantProcessOk = ( track->mGeantProcess == 5 || (track->parentGeantId() == 1 && track->mGeantProcess == 6) );

    if ( !isGeantProcessOk ){
      LOG_DEBUG << Form("StEmbeddingQA::getEmbeddingTrack()  geantprocess = %3d. Skip the track", track->mGeantProcess) << endm;
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
  StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  /// Loop over all registered particles (real tracks)
  for(vector<Int_t>::iterator iter = mGeantId[categoryid].begin(); iter != mGeantId[categoryid].end(); iter++){
    const Int_t geantid = (*iter) ;
    Bool_t btofflag = kFALSE;
    if( (categoryid - StEmbeddingQAConst::mNEmbedding) == 0 && utility->getBTofPid() ) btofflag = kTRUE; //use btof nsigma for primary tracks
    StEmbeddingQATrack miniTrack(StEmbeddingQAUtilities::instance()->getCategoryName(categoryid), track, geantid, btofflag);
 
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
  const Int_t geantid = track.getGeantId() ;
  if ( geantid < 0 ) return ;

  /// pt and eta cuts (see StEmbeddingQATrack::isPtAndEtaOk())
  if( !track.isPtAndEtaOk() ) return ;

  /// Use MC momentum for the embedding tracks, reconstructed momentum for the real tracks
  StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  const Double_t pt    = (utility->isReal(track.getName())) ? track.getPtRc()       : track.getPtMc() ;
  const Double_t mom   = (utility->isReal(track.getName())) ? track.getPRc()        : track.getPMc() ;
  const Double_t eta   = (utility->isReal(track.getName())) ? track.getEtaRc()      : track.getEtaMc() ;
  const Double_t y     = (utility->isReal(track.getName())) ? track.getRapidityRc() : track.getRapidityMc() ;

  // Reconstructed momentum
  const Double_t momRc = track.getPRc() ;

  /// Check geant id
  ///   if it's new, expands the histogram array for it
  ///   if it has already exist, do nothing
  expandHistograms(categoryid, geantid, track.getParentGeantId(), track.getParentParentGeantId(),
      track.getGeantProcess());

  // Fill geant id
  mhGeantId[categoryid]->Fill(track.getGeantId());

  // Rapidity cut, default rapidity cut is 10 (Use setRapidityCut(const Float_t ycut) to restrict y window)
//  if(!track.isRapidityOk(mRapidityCut)) return ;
  if(!utility->isRapidityOk(y)) return ;

  const TString idcollection(getIdCollection(geantid, track.getParentGeantId(), track.getParentParentGeantId()));

  // dE/dx (no PID cut)
  //  - Add NHit cut (Nov/13/2009)
  //  - Add NHitFit/NHitPoss cut
  if( track.isDcaOk() && track.isNHitOk() && track.isNHitToNPossOk() ) {
    mhdEdxVsMomMc[categoryid][idcollection]->Fill(mom, track.getdEdxkeV());
    mhdEdxVsMomReco[categoryid][idcollection]->Fill(momRc, track.getdEdxkeV());
  }

  //  Oct/21/2009
  /// NSigma cut for real data
  /// Add particle id from nSigma cuts (2 sigma) for e/pi/K/p
  /// Do not apply nSigma cuts for others

  if ( !track.isNSigmaOk(geantid) ) return ;

  if( track.isDcaOk() ){
    // Fill NHit points
    //  Added common hit cuts
    if ( track.isCommonHitOk() ) {
      mhNHit[categoryid][idcollection]->Fill(pt, eta, track.getNHit());
    }

    if( track.isNHitOk() ){
      const Double_t phi = track.getPhi() ;

      // Fill Ncommon hits vs Nhits
      mhNCommonHitVsNHit[categoryid][idcollection]->Fill(pt, track.getNHit(), track.getNCommonHit());

      // NHitFit/NHitPoss cut
      if( track.isNHitToNPossOk() ) {
        // dE/dx (with PID cut)
        mhdEdxVsMomMcPidCut[categoryid][idcollection]->Fill(mom, track.getdEdxkeV());
        mhdEdxVsMomRecoPidCut[categoryid][idcollection]->Fill(momRc, track.getdEdxkeV());
  
        // Correlation between reconstructed and MC momentum
        mhRecoPVsMcP[categoryid][idcollection]->Fill(mom, momRc);
  
        // Pt, eta, phi
        mhPtVsEta[categoryid][idcollection]->Fill(eta, pt);
        mhPtVsY[categoryid][idcollection]->Fill(y, pt);
        mhPtVsPhi[categoryid][idcollection]->Fill(phi, pt);
        mhPtVsMom[categoryid][idcollection]->Fill(mom, pt);
        mhdPtVsPt[categoryid][idcollection]->Fill(pt, track.getPtRc()-pt);
        mhdInvPtVsPt[categoryid][idcollection]->Fill(pt, (1.0/track.getVectorGl().perp()-1.0/pt)*1000.);
        mhMomVsEta[categoryid][idcollection]->Fill(eta, mom);
  
        mhEtaVsPhi[categoryid][idcollection]->Fill(phi, eta);
        mhEtaVsVz[categoryid][idcollection]->Fill(mVz, eta);
        mhYVsVz[categoryid][idcollection]->Fill(mVz, y);
      }
    }
  }

  // Fill Dca
  if( track.isNHitOk() && track.isNHitToNPossOk() ){
    mhDca[categoryid][idcollection]->Fill(pt, eta, track.getDcaGl());
  }

  LOG_DEBUG << Form("     RC:(nfit, pt, eta, phi) = (%5d, %1.4f, %1.4f, %1.4f)  MC:(pt, eta) = (%1.4f, %1.4f)",
      track.getNHit(), track.getPtRc(), track.getEtaRc(), track.getPhi(), track.getPtMc(), track.getEtaMc()
      ) << endm;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQA::pushBackGeantId(const Int_t categoryid, const Int_t geantid, const Int_t parentid,
    const Int_t parentparentid, const Int_t geantprocess)
{
  /// Add geantid if it's new. If we have already had input geantid in the array, do nothing.
  StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;

  const Bool_t isContaminated = utility->isContaminated(utility->getCategoryName(categoryid)) ;

  Bool_t isOk = kFALSE ;
  if ( mGeantId[categoryid].empty() ){
    /// Push back a new geantid if geantid array is empty
    LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm ;
    LOG_INFO << Form("StEmbeddingQA::pushBackGeantId()   mGeantId[%d] is empty", categoryid) << endm;
    if( isContaminated ){
      LOG_INFO << Form("StEmbeddingQA::pushBackGeantId()   Find a new geant id,  (geant, parent, parent-parent, process) = (%4d, %4d, %4d, %4d) (%10s)",
          geantid, parentid, parentparentid, geantprocess, utility->getCategoryName(categoryid).Data()) << endm;
    }
    else{
      LOG_INFO << Form("StEmbeddingQA::pushBackGeantId()   Find a new geant id,  geantid = %5d (%10s)", 
          geantid, utility->getCategoryName(categoryid).Data()) << endm;
    }
    LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm ;
    mGeantId[categoryid].push_back(geantid);

    isOk = kTRUE ;
  }
  else{
    /// Expand histogrm by checking the geant id
    ///   if it's new, expands the histogram array for it
    ///   if it has already exist, do nothing
    const vector<Int_t>::iterator iter = find(mGeantId[categoryid].begin(), mGeantId[categoryid].end(), geantid) ;
 
    if ( iter != mGeantId[categoryid].end() ){
      /// Geant id already exist. do nothing
      isOk = kFALSE;
    }
    else{
      /// Find a new geant id, store id in mGeantId array
      if( isContaminated ){
        LOG_INFO << Form("StEmbeddingQA::pushBackGeantId()   Find a new geant id,  (geant, parent, parent-parent, process) = (%4d, %4d, %4d, %4d) (%10s)",
            geantid, parentid, parentparentid, geantprocess, utility->getCategoryName(categoryid).Data()) << endm;
      }
      else{
        LOG_INFO << Form("StEmbeddingQA::pushBackGeantId()   Find a new geant id,  geantid = %5d (%10s)", 
            geantid, utility->getCategoryName(categoryid).Data()) << endm;
      }
      mGeantId[categoryid].push_back(geantid);

      isOk = kTRUE ;
    }
  }

  /// Return here if track != Contaminated pairs
  if( !utility->isContaminated(utility->getCategoryName(categoryid)) ) return isOk ;

  /// Check the parent and parent-parent geantid
  ///   if they are new, add them into the map
  ///   if they have already exist, do nothing

  /// Skip if parentid == 0
  isOk = kFALSE ;
  if( parentid == 0 ){
    isOk = kFALSE ;
  }
  else{
    const TString idcollection(getIdCollection(geantid, parentid, parentparentid));

    if( mGeantIdCollection.empty() ){
      /// Push back a new id collection if the array is empty
      LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm ;
      LOG_INFO << "StEmbeddingQA::pushBackGeantId()  mGeantIdCollection is empty." << endm;
      LOG_INFO << Form("StEmbeddingQA::pushBackGeantId()   Push back             (geant, parent, parent-parent) = (%4d, %4d, %4d) (process=%4d)", 
          geantid, parentid, parentparentid, geantprocess) << endm;
      LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm ;

      mGeantIdCollection.push_back( idcollection );
      return kTRUE ;
    }

    /// Make sure the combination (parent-parent, parent, geantid) has already been added in the array
    const vector<TString>::iterator iterIdCollection = find(mGeantIdCollection.begin(), mGeantIdCollection.end(), idcollection) ;
    if ( iterIdCollection != mGeantIdCollection.end() ){
      /// id collection already exists. do nothing
      isOk = kFALSE;
    }
    else{
      /// Push back a new id collection
      LOG_INFO << Form("StEmbeddingQA::pushBackGeantId()   Push back             (geant, parent, parent-parent) = (%4d, %4d, %4d) (process=%4d)", 
          geantid, parentid, parentparentid, geantprocess) << endm;
      mGeantIdCollection.push_back( idcollection );

      isOk = kTRUE ;
    }
  }// parentid != 0

  return isOk ;
}


//__________________________________________________________________________________________
void StEmbeddingQA::expandHistograms(const Int_t categoryid, const Int_t geantid, const Int_t parentid,
    const Int_t parentparentid, const Int_t geantprocess)
{
  /// Push back geant id if the input geantid is new (return true)
  /// If the input geant id has already stored in mGeantId array, do nothing (return false)
  if ( !pushBackGeantId(categoryid, geantid, parentid, parentparentid, geantprocess) ) return ;

  // Expand histograms
  mOutput->cd();

  const Int_t ptBin    = 10 * mPtMax ;
  const Float_t ptMin  = 0.0 ;
  const Float_t ptMax  = mPtMax ;
  const Int_t etaBin   = 100 ;
  const Float_t etaMin = -2.5 ;
  const Float_t etaMax =  2.5 ;

  /// Add parent geantid information if the particle is decay daughters
  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  const Char_t* categoryTitle(utility->getCategoryTitle(categoryid).Data());

  /// Suffix for each histogram. Put parent-parentid and parentid if we have decay daughters
  const TString nameSuffix = (parentid>0) ? Form("_%d_%d_%d_%d", categoryid, parentparentid, parentid, geantid)
    : Form("_%d_%d", categoryid, geantid);

  /// Category title and particle name put in the histogram title
  TString CategoryAndGeantId = (parentid>0) ?  Form("%s (%s #rightarrow %s)", categoryTitle, 
      utility->getParticleDefinition(parentid)->name().c_str(), utility->getParticleDefinition(geantid)->name().c_str())
    : Form("%s (%s)", categoryTitle, utility->getParticleDefinition(geantid)->name().c_str());

  if ( parentparentid > 0 ){
    CategoryAndGeantId = Form("%s (%s #rightarrow %s #rightarrow %s)", categoryTitle,
        utility->getParticleDefinition(parentparentid)->name().c_str(),
        utility->getParticleDefinition(parentid)->name().c_str(),
        utility->getParticleDefinition(geantid)->name().c_str());
  }

  const TString categoryName(utility->getCategoryName(categoryid));
  const Bool_t isMc        = utility->isMc(categoryName);
  const Bool_t isEmbedding = utility->isEmbedding(categoryName);

  const TString idcollection(getIdCollection(geantid, parentid, parentparentid));

  // NHit vs eta vs MC pt
  TString title(Form("N_{fit} distribution (|dcaGl|<3cm), %s", CategoryAndGeantId.Data()));
  if( isMc ) title = Form("N_{fit} distribution, %s", CategoryAndGeantId.Data());
  TH3* hNhit = new TH3D(Form("hNHit%s", nameSuffix.Data()), title, 10, 0, 5, 10, -1.0, 1.0, 50, 0, 50) ;
  hNhit->SetXTitle("MC p_{T} (GeV/c)");
  hNhit->SetYTitle("#eta");
  hNhit->SetZTitle("N_{fit}");
  utility->setStyle(hNhit);
  mhNHit[categoryid].insert( std::make_pair(idcollection, hNhit) );

  // Ncommon hit vs Nfit
  title = Form("N_{common} hit vs N_{fit} (|dcaGl|<3cm), %s", CategoryAndGeantId.Data());
  if( isMc ) title = Form("N_{common} hit vs N_{fit}, %s", CategoryAndGeantId.Data());
  TH3* hNCommonHitVsNHit = new TH3D(Form("hNCommonHitVsNHit%s", nameSuffix.Data()), title, 10, 0, 5, 50, 0, 50, 50, 0, 50) ;
  hNCommonHitVsNHit->SetXTitle("p_{T} (GeV/c)");
  hNCommonHitVsNHit->SetYTitle("N_{fit}");
  hNCommonHitVsNHit->SetZTitle("N_{common}");
  utility->setStyle(hNCommonHitVsNHit);
  mhNCommonHitVsNHit[categoryid].insert( std::make_pair(idcollection, hNCommonHitVsNHit) );

  // Dca vs eta vs MC pt
  title = Form("Dca vs #eta vs MC p_{T} (N_{fit}#geq10), %s", CategoryAndGeantId.Data());
  if( isMc ) title = Form("Dca vs #eta vs MC p_{T}, %s", CategoryAndGeantId.Data());
  else if ( isEmbedding ) title = Form("Dca vs #eta vs MC p_{T} (N_{fit}#geq10 & N_{common}#geq10), %s", CategoryAndGeantId.Data());

  TH3* hDca = new TH3D(Form("hDca%s", nameSuffix.Data()), title, 10, 0, 5, 10, -1.0, 1.0, 100, 0, 3.0);
  hDca->SetXTitle("MC p_{T} (GeV/c)");
  hDca->SetYTitle("#eta");
  hDca->SetZTitle("Global dca (cm)");
  utility->setStyle(hDca);
  mhDca[categoryid].insert( std::make_pair(idcollection, hDca) );

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
  mhPtVsEta[categoryid].insert( std::make_pair(idcollection, hPtVsEta) );

  // pt vs y
  TH2* hPtVsY = new TH2D(Form("hPtVsY%s", nameSuffix.Data()), Form("MC p_{T} vs y%s", titleSuffix.Data()),
      etaBin, etaMin, etaMax, ptBin, ptMin, ptMax);
  hPtVsY->SetXTitle("rapidity y");
  hPtVsY->SetYTitle("MC p_{T} (GeV/c)");
  utility->setStyle(hPtVsY);
  mhPtVsY[categoryid].insert( std::make_pair(idcollection, hPtVsY) );

  // pt vs phi
  TH2* hPtVsPhi = new TH2D(Form("hPtVsPhi%s", nameSuffix.Data()), Form("MC p_{T} vs #phi%s", titleSuffix.Data()),
      100, -TMath::Pi(), TMath::Pi(), ptBin, ptMin, ptMax);
  hPtVsPhi->SetXTitle("#phi (rad)");
  hPtVsPhi->SetYTitle("MC p_{T} (GeV/c)");
  utility->setStyle(hPtVsPhi);
  mhPtVsPhi[categoryid].insert( std::make_pair(idcollection, hPtVsPhi) );

  // pt vs momentum
  TH2* hPtVsMom = new TH2D(Form("hPtVsMom%s", nameSuffix.Data()), Form("MC p_{T} vs momentum%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, ptBin, ptMin, ptMax);
  hPtVsMom->SetXTitle("momentum (GeV/c)");
  hPtVsMom->SetYTitle("MC p_{T} (GeV/c)");
  utility->setStyle(hPtVsMom);
  mhPtVsMom[categoryid].insert( std::make_pair(idcollection, hPtVsMom) );

  // Delta pt vs pt
  TH2* hdPtVsPt = new TH2D(Form("hdPtVsPt%s", nameSuffix.Data()), Form("p_{T} - p_{T} (MC) vs p_{T}%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, 100, -5, 5);
  hdPtVsPt->SetXTitle("MC p_{T} (GeV/c)");
  hdPtVsPt->SetYTitle("reco. p_{T} - MC p_{T} (GeV/c)");
  utility->setStyle(hdPtVsPt);
  mhdPtVsPt[categoryid].insert( std::make_pair(idcollection, hdPtVsPt) );

  // Delta 1/pt vs pt
  TH2* hdInvPtVsPt = new TH2D(Form("hdInvPtVsPt%s", nameSuffix.Data()), Form("1/p_{T} (Gl) - 1/p_{T} (MC) vs p_{T}%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, 200, -50, 50);
  hdInvPtVsPt->SetXTitle("MC p_{T} (GeV/c)");
  hdInvPtVsPt->SetYTitle("Gl 1/p_{T} - MC 1/p_{T} (c/MeV)");
  utility->setStyle(hdInvPtVsPt);
  mhdInvPtVsPt[categoryid].insert( std::make_pair(idcollection, hdInvPtVsPt) );

  // momentum vs eta
  TH2* hMomVsEta = new TH2D(Form("hMomVsEta%s", nameSuffix.Data()), Form("Momentum vs #eta%s", titleSuffix.Data()),
      etaBin, etaMin, etaMax, ptBin, ptMin, ptMax);
  hMomVsEta->SetXTitle("#eta");
  hMomVsEta->SetYTitle("momentum (GeV/c)");
  utility->setStyle(hMomVsEta);
  mhMomVsEta[categoryid].insert( std::make_pair(idcollection, hMomVsEta) );

  // dE/dx vs MC momentum
  TH2* hdEdxVsMomMc = new TH2D(Form("hdEdxVsMomMc%s", nameSuffix.Data()), Form("dE/dx vs MC p%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, 100, 0, 10.0);
  hdEdxVsMomMc->SetXTitle("MC p (GeV/c)");
  hdEdxVsMomMc->SetYTitle("dE/dx (keV/cm)");
  utility->setStyle(hdEdxVsMomMc);
  mhdEdxVsMomMc[categoryid].insert( std::make_pair(idcollection, hdEdxVsMomMc) );

  // dE/dx vs reconstructed momentum
  TH2* hdEdxVsMomReco = new TH2D(Form("hdEdxVsMomReco%s", nameSuffix.Data()), Form("dE/dx vs Reconstructed p%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, 100, 0, 10.0);
  hdEdxVsMomReco->SetXTitle("Reconstructed p (GeV/c)");
  hdEdxVsMomReco->SetYTitle("dE/dx (keV/cm)");
  utility->setStyle(hdEdxVsMomReco);
  mhdEdxVsMomReco[categoryid].insert( std::make_pair(idcollection, hdEdxVsMomReco) );

  // Reconstructed momentum vs MC momentum
  TH2* hRecoPVsMcP = new TH2D(Form("hRecoPVsMcP%s", nameSuffix.Data()), Form("Reconstructed p vs MC p%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, ptBin, ptMin, ptMax);
  hRecoPVsMcP->SetXTitle("MC p (GeV/c)");
  hRecoPVsMcP->SetYTitle("Reconstructed p (GeV/c)");
  utility->setStyle(hRecoPVsMcP);
  mhRecoPVsMcP[categoryid].insert( std::make_pair(idcollection, hRecoPVsMcP) );

  // eta vs phi
  TH2* hEtaVsPhi = new TH2D(Form("hEtaVsPhi%s", nameSuffix.Data()), Form("#eta vs #phi%s", titleSuffix.Data()),
      100, -TMath::Pi(), TMath::Pi(), etaBin, etaMin, etaMax);
  hEtaVsPhi->SetXTitle("#phi (rad)");
  hEtaVsPhi->SetYTitle("#eta");
  utility->setStyle(hEtaVsPhi);
  mhRecoPVsMcP[categoryid].insert( std::make_pair(idcollection, hRecoPVsMcP) );
  mhEtaVsPhi[categoryid].insert( std::make_pair(idcollection, hEtaVsPhi) );

  // eta vs vz
  TH2* hEtaVsVz = new TH2D(Form("hEtaVsVz%s", nameSuffix.Data()), Form("#eta vs v_{z}%s", titleSuffix.Data()),
      200, -50, 50, 200, etaMin, etaMax);
  hEtaVsVz->SetXTitle("v_{z} (cm)");
  hEtaVsVz->SetYTitle("#eta");
  utility->setStyle(hEtaVsVz);
  mhEtaVsVz[categoryid].insert( std::make_pair(idcollection, hEtaVsVz) );
 
  // rapidity vs vz
  TH2* hYVsVz = new TH2D(Form("hYVsVz%s", nameSuffix.Data()), Form("rapidity y vs v_{z}%s", titleSuffix.Data()),
      200, -50, 50, 200, etaMin, etaMax);
  hYVsVz->SetXTitle("v_{z} (cm)");
  hYVsVz->SetYTitle("rapidity y");
  utility->setStyle(hYVsVz);
  mhYVsVz[categoryid].insert( std::make_pair(idcollection, hYVsVz) );

  // dE/dx vs MC momentum (with pid cut)
  TH2* hdEdxVsMomMcPidCut = new TH2D(Form("hdEdxVsMomMcPidCut%s", nameSuffix.Data()), Form("dE/dx vs MC p (with 2#sigma pid cut)%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, 100, 0, 10.0);
  hdEdxVsMomMcPidCut->SetXTitle("MC p (GeV/c)");
  hdEdxVsMomMcPidCut->SetYTitle("dE/dx (keV/cm)");
  utility->setStyle(hdEdxVsMomMcPidCut);
  mhdEdxVsMomMcPidCut[categoryid].insert( std::make_pair(idcollection, hdEdxVsMomMcPidCut) );

  // dE/dx vs Reconstructed momentum (with pid cut)
  TH2* hdEdxVsMomRecoPidCut = new TH2D(Form("hdEdxVsMomRecoPidCut%s", nameSuffix.Data()), 
      Form("dE/dx vs Reconstructed p (with 2#sigma pid cut)%s", titleSuffix.Data()),
      ptBin, ptMin, ptMax, 100, 0, 10.0);
  hdEdxVsMomRecoPidCut->SetXTitle("Reconstructed p (GeV/c)");
  hdEdxVsMomRecoPidCut->SetYTitle("dE/dx (keV/cm)");
  utility->setStyle(hdEdxVsMomRecoPidCut);
  mhdEdxVsMomRecoPidCut[categoryid].insert( std::make_pair(idcollection, hdEdxVsMomRecoPidCut) );
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
TString StEmbeddingQA::getIdCollection(const Int_t geantid, const Int_t parentid, const Int_t parentparentid) const
{
  return Form("%d_%d_%d", geantid, parentid, parentparentid) ;
}

void StEmbeddingQA::setPtMax(Float_t ptmax)
{
  mPtMax = ptmax;
  LOG_INFO << Form("Maximum p_T for histograms set to %f", mPtMax) << endm; 
}

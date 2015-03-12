#include <assert.h>
#include <Stiostream.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <cmath>

#include "TStyle.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TFile.h"

#include "StMcAnalysisMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"

#include "StMessMgr.h"

#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"

#include "StThreeVectorF.hh"
#include "StEventTypes.h"
#include "StMcEventTypes.hh"
#include "StMcEvent.hh"

// Define data Members for the histograms
const Int_t   StMcAnalysisMaker::mNumDeltaX = 50;
const Int_t   StMcAnalysisMaker::mNumDeltaZ = 50;
const Float_t StMcAnalysisMaker::mMinDeltaX = -0.52;
const Float_t StMcAnalysisMaker::mMaxDeltaX =  0.52;
const Float_t StMcAnalysisMaker::mMinDeltaZ = -0.52;
const Float_t StMcAnalysisMaker::mMaxDeltaZ =  0.52;

struct TpcHitMRPair_t {
  Float_t sector, row, isDet,
    xM, yM, zM, pxM, pyM, pzM, dEM, dSM, nM,
    xR, yR, zR, dER, IdM, IdR, qR, nR;};
static const Char_t *vTpcHitMRPair = "sector:row:isDet:xM:yM:zM:pxM:pyM:pzM:dEM:dSM:nM:xR:yR:zR:dER:IdM:IdR:qR:nR";
static TpcHitMRPair_t TpcHitMRPair;

struct PxlHitMRPair_t {
  Float_t sector, ladder, sensor,
    xM, yM, zM, pxM, pyM, pzM, dEM, dSM, nM,
    xR, yR, zR, dER, IdM, IdR, qR, nR;};
static const Char_t *vPxlHitMRPair = "sector:ladder:sensor:xM:yM:zM:pxM:pyM:pzM:dEM:dSM:nM:xR:yR:zR:dER:IdM:IdR:qR:nR";
static PxlHitMRPair_t PxlHitMRPair;

struct IstHitMRPair_t {
  Float_t ladder, sensor,
    xM, yM, zM, pxM, pyM, pzM, dEM, dSM, nM,
    xR, yR, zR, dER, IdM, IdR, qR, nR;};
static const Char_t *vIstHitMRPair = "ladder:sensor:xM:yM:zM:pxM:pyM:pzM:dEM:dSM:nM:xR:yR:zR:dER:IdM:IdR:qR:nR";
static IstHitMRPair_t IstHitMRPair;

struct SsdHitMRPair_t {
  Float_t ladder, wafer,
    xM, yM, zM, pxM, pyM, pzM, dEM, dSM, nM,
    xR, yR, zR, dER, IdM, IdR, qR, nR;};

static const Char_t *vSsdHitMRPair = "ladder:wafer:xM:yM:zM:pxM:pyM:pzM:dEM:dSM:nM:xR:yR:zR:dER:IdM:IdR:qR:nR";
static SsdHitMRPair_t SsdHitMRPair;

ClassImp(StMcAnalysisMaker)

//_________________________________________________
StMcAnalysisMaker::StMcAnalysisMaker(const char *name, const char *title):StMaker(name,title), mNtupleFile(0){
  //  StMcAnalysisMaker Constructor
  // - zero all pointers defined in the header file
  mAssociationCanvas = 0;
  mMomResolution  = 0;
  mHitResolution  = 0;   
  mPxlHitResolution  = 0;   
  mIstHitResolution  = 0; 
  mSsdHitResolution  = 0;   
  coordRec        = 0;  
  coordMcPartner  = 0;
  mTrackNtuple    = 0;
  mTpcHitNtuple   = 0;
  mPxlHitNtuple   = 0;
  mSsdHitNtuple   = 0;
}

//_________________________________________________
StMcAnalysisMaker::~StMcAnalysisMaker(){
  //  StMcAnalysisMaker Destructor
  //  delete the histograms
  cout << "Inside StMcAnalysisMaker Destructor" << endl;
  SafeDelete(mAssociationCanvas);
  //     SafeDelete(mMomResolution);
  //     SafeDelete(mHitResolution);
  //     SafeDelete(coordRec);
  //     SafeDelete(coordMcPartner);
  //     SafeDelete(mTrackNtuple);
}

//_____________________________________________________________________________

void StMcAnalysisMaker::Clear(const char*){
  // StMcAnalysisMaker - Clear,
  // Don't delete the canvas, so that it stays if needed
  
  StMaker::Clear();
}

//_________________________________________________
Int_t StMcAnalysisMaker::Finish(){
  if (mNtupleFile) {
    mNtupleFile->Write();
    mNtupleFile->Close();
  }
  return StMaker::Finish();
}


//_________________________________________________
Int_t StMcAnalysisMaker::Init(){
  // StMcAnalysisMaker - Init
  SetZones();  // This is my method to set the zones for the canvas.
  
  mNtupleFile = GetTFile();
  if (mNtupleFile) {mNtupleFile->cd(); mNtupleFile = 0;}
  else {mNtupleFile = new TFile("TrackMapNtuple.root","RECREATE","Track Ntuple");}
  // Book Histograms Here so they can be found and deleted by Victor's chain (I hope).
  mHitResolution = new TH2F("hitRes","Delta Z Vs Delta X for Hits",
			    mNumDeltaX,mMinDeltaX,mMaxDeltaX,mNumDeltaZ,mMinDeltaZ,mMaxDeltaZ);
  mHitResolution->SetXTitle("Delta X (cm)");
  mHitResolution->SetYTitle("Delta Z (cm)");
  
  mPxlHitResolution = new TH2F("PxlHitRes","Delta Z Vs Delta X for PxlHits",
			       mNumDeltaX,mMinDeltaX,mMaxDeltaX,mNumDeltaZ,mMinDeltaZ,mMaxDeltaZ);
  mPxlHitResolution->SetXTitle("Delta X (cm)");
  mPxlHitResolution->SetYTitle("Delta Z (cm)");

  mIstHitResolution = new TH2F("IstHitRes","Delta Z Vs Delta X for IstHits",
			       mNumDeltaX,mMinDeltaX,mMaxDeltaX,mNumDeltaZ,mMinDeltaZ,mMaxDeltaZ);
  mIstHitResolution->SetXTitle("Delta X (cm)");
  mIstHitResolution->SetYTitle("Delta Z (cm)");
  
  mSsdHitResolution = new TH2F("SsdHitRes","Delta Z Vs Delta X for SsdHits",
			       mNumDeltaX,mMinDeltaX,mMaxDeltaX,mNumDeltaZ,mMinDeltaZ,mMaxDeltaZ);
  mSsdHitResolution->SetXTitle("Delta X (cm)");
  mSsdHitResolution->SetYTitle("Delta Z (cm)");
  
  mMomResolution = new TH1F("momRes","(|p| - |pmc|)/|pmc|",100,-1.,1.);
  mMomResolution->SetXTitle("Resolution (%)");
  
  coordRec = new TH2F("coordRc","X vs Y pos. of Hits", 100, -200, 200, 100, -200, 200);
  coordRec->SetXTitle("X (cm)");
  coordRec->SetYTitle("Y (cm)");
  
  coordMcPartner = new TH2F("coordMc","X vs Y pos. of Hits", 100, -200, 200, 100, -200, 200);
  coordMcPartner->SetXTitle("X (cm)");
  coordMcPartner->SetYTitle("Y (cm)");
  
  // Define the file for the Ntuple, otherwise it won't be available later.
  // one must define the file _after_ the histograms are booked, otherwise they are
  // not owned by the maker, but are stored in the file, breaking the code in StAssociator.
  
  mNtupleFile = new TFile("TrackMapNtuple.root","RECREATE","Track Ntuple");
  
  const char* vars = "px:py:pz:p:pxrec:pyrec:pzrec:prec:commTpcHits:hitDiffX:hitDiffY:hitDiffZ:mcTrkId:mostCommIdTruth:nHitsIdTruth:nMcHits:nFitPts:nDetPts:quality";
  mTrackNtuple = new TNtuple("TrackNtuple","Track Pair Info",vars);
  mTrackNtuple->SetAutoSave(100000000);
  if (! m_Mode || m_Mode & 0x1) {
    mTpcHitNtuple = new TNtuple("TpcHitNtuple","the TPC hit pairs Info",vTpcHitMRPair);
    mTpcHitNtuple->SetAutoSave(100000000);
  }
  if (! m_Mode || m_Mode & 0x2) {
    mPxlHitNtuple = new TNtuple("PxlHitNtuple","the PXL hit pairs Info",vPxlHitMRPair);
    mPxlHitNtuple->SetAutoSave(100000000);
  }
  if (! m_Mode || m_Mode & 0x3) {
    mIstHitNtuple = new TNtuple("IstHitNtuple","the IST hit pairs Info",vIstHitMRPair);
    mIstHitNtuple->SetAutoSave(100000000);
  }
  if (! m_Mode || m_Mode & 0x4) {
    mSsdHitNtuple = new TNtuple("SsdHitNtuple","the SSD hit pairs Info",vSsdHitMRPair);
    mSsdHitNtuple->SetAutoSave(100000000);
  }
  return StMaker::Init();
}
//_________________________________________________
Int_t StMcAnalysisMaker::Make()
{
  // Get the pointers we need, we have to use the titles we gave them in the
  // macro.  I just used the defaults.
  
  // StEvent
  StEvent* rEvent =  (StEvent*) GetInputDS("StEvent");
  const StMcTrack  *mTrack = 0;
  
  // StMcEvent
  StMcEvent* mEvent = (StMcEvent*) GetDataSet("StMcEvent");
  
  // StAssociationMaker
  StAssociationMaker* assoc = 0;
  assoc = (StAssociationMaker*) GetMaker("StAssociationMaker");
  
  // the Multimaps...
  rcTpcHitMapType* theHitMap   = assoc->rcTpcHitMap();
  mcTpcHitMapType* theMcHitMap = assoc->mcTpcHitMap();
  rcPxlHitMapType* pxlHitMap   = assoc->rcPxlHitMap();
  mcPxlHitMapType* pxlMcHitMap = assoc->mcPxlHitMap();
  rcIstHitMapType* istHitMap   = assoc->rcIstHitMap();
  mcIstHitMapType* istMcHitMap = assoc->mcIstHitMap();
  rcSsdHitMapType* ssdHitMap   = assoc->rcSsdHitMap();
  mcSsdHitMapType* ssdMcHitMap = assoc->mcSsdHitMap();
  rcTrackMapType*  theTrackMap = assoc->rcTrackMap();
  mcV0MapType* theMcV0Map      = assoc->mcV0Map(); 
  
  if (!theHitMap) {
    gMessMgr->Warning() << "----------WARNING----------\n"
			<< "No Hit Map found for this event!" << endm;
    return kStWarn;
  }
  // Example: look at the position of the primary vertex
  //          Map is not needed for this, but it's a good check,
  //          tracking will not be good if primary vertex was not well placed.
  
  // First check whether the Primary Vertex is there at all.
  StThreeVectorD VertexPos(0,0,0);
  if (rEvent->primaryVertex()) {
    VertexPos = rEvent->primaryVertex()->position();
    cout << "Position of Primary Vertex from StEvent:" << endl;
    cout << VertexPos << endl;
  }
  else {
    cout << "----------- WARNING ------------" << endl;
    cout << "No primary vertex from StEvent!!" << endl;
    cout << "Assume it is at (0,0,0)         " << endl;
    
  }
  cout << "Position of Primary Vertex from StMcEvent:" << endl;
  cout << mEvent->primaryVertex()->position() << endl;
  
  // Example: look at hits associated with 1st REC hit in Tpc Hit collection.
  
  StTpcHit*     firstHit;
  Bool_t        gotOneHit;
  StTpcHitCollection* tpcColl = rEvent->tpcHitCollection();
  UInt_t j,k, nhits;
  nhits = tpcColl->numberOfHits();
  if (tpcColl && nhits) {
    gotOneHit = kFALSE;
    memset (&TpcHitMRPair, 0, sizeof(TpcHitMRPair_t));
    for (k=0; !gotOneHit && k<tpcColl->numberOfSectors(); k++)
      for (j=0; !gotOneHit && j<tpcColl->sector(k)->numberOfPadrows(); j++)
	if (tpcColl->sector(k)->padrow(j)->hits().size()) {
	  
	  firstHit = tpcColl->sector(k)->padrow(j)->hits()[0];
	  cout << "First Hit Found in Sector "
	       << firstHit->sector() << " Padrow "
	       << firstHit->padrow() << endl;
	  gotOneHit = kTRUE;
	}
    cout << "This hit has " <<  theHitMap->count(firstHit) << " MC Hits associated with it."<< endl;
    // To get the associated hits of the first hit we use equal_range(key), which returns
    // 2 iterators, the lower bound and upper bound, so that then we can loop over them.
    
    cout << "Position of First Rec. Hit and Associated (if any) MC Hit:" << endl;
    pair<rcTpcHitMapIter,rcTpcHitMapIter> hitBounds = theHitMap->equal_range(firstHit);    
    for (rcTpcHitMapIter it=hitBounds.first; it!=hitBounds.second; ++it) 
      cout << "[" << (*it).first->position() << ", " << (*it).second->position() << "]" << endl;
  }
  else {
    cout << "There are no reconstructed TPC Hits in this event" << endl;
  }
  // Example: Make a histogram using the Hit Map.
  // Fill histogram from map
  
  Float_t DeltaX;
  Float_t DeltaZ;
  if (theHitMap && mTpcHitNtuple) {// TPC
    StTpcHitCollection* recHits = rEvent->tpcHitCollection();
    StMcTpcHitCollection* mcHits = mEvent->tpcHitCollection();
    assert (recHits || mcHits);
    cout << "Making Hit Resolution Histogram..." << endl;
    // Loop over Rec Hits
    
    for (UInt_t iSector=0; iSector< recHits->numberOfSectors(); iSector++) {
      for (UInt_t iPadrow=0; iPadrow<recHits->sector(iSector)->numberOfPadrows();
	   iPadrow++) {
	for (StTpcHitIterator iter = recHits->sector(iSector)->padrow(iPadrow)->hits().begin();
	     iter != recHits->sector(iSector)->padrow(iPadrow)->hits().end();
	     iter++) {
	  const StTpcHit   *rhit = dynamic_cast<const StTpcHit   *> (*iter);
	  assert(rhit);
	  if (rhit->TestBit(StMcHit::kMatched)) 
	  {
	    pair<rcTpcHitMapIter,rcTpcHitMapIter>
	      recBounds = theHitMap->equal_range(rhit);
	    
	    for (rcTpcHitMapIter it2=recBounds.first; it2!=recBounds.second; ++it2){
	      
	      const StMcTpcHit *mhit = dynamic_cast<const StMcTpcHit *> ((*it2).second);
	      assert ( mhit);
	      DeltaX = rhit->position().x() - mhit->position().x();
	      DeltaZ = rhit->position().z() - mhit->position().z();
	      
	      mHitResolution->Fill(DeltaX,DeltaZ);
	      
	      memset (&TpcHitMRPair, 0, sizeof(TpcHitMRPair));
	      TpcHitMRPair.sector   = rhit->sector();
	      TpcHitMRPair.row      = rhit->padrow();
	      TpcHitMRPair.xR       = rhit->position().x();
	      TpcHitMRPair.yR       = rhit->position().y();
	      TpcHitMRPair.zR       = rhit->position().z();
	      TpcHitMRPair.dER      = rhit->charge();
	      TpcHitMRPair.IdR      = rhit->idTruth();
	      TpcHitMRPair.qR       = rhit->qaTruth();
	      TpcHitMRPair.nR       = theHitMap->count(rhit);
	      TpcHitMRPair.isDet    = mhit->isDet();
	      TpcHitMRPair.xM       = mhit->position().x();
	      TpcHitMRPair.yM       = mhit->position().y();
	      TpcHitMRPair.zM       = mhit->position().z();
	      TpcHitMRPair.pxM      = mhit->localMomentum().x();
	      TpcHitMRPair.pyM      = mhit->localMomentum().y();
	      TpcHitMRPair.pzM      = mhit->localMomentum().z();
	      TpcHitMRPair.dEM      = mhit->dE();
	      TpcHitMRPair.dSM      = mhit->dS();
	      mTrack     = mhit->parentTrack();
	      if (mTrack) TpcHitMRPair.IdM = mTrack->key();
	      else        TpcHitMRPair.IdM = 0;
	      TpcHitMRPair.nM     = theMcHitMap->count(mhit);
	      mTpcHitNtuple->Fill(&TpcHitMRPair.sector);
	    }
	  }
	  else {
	    memset (&TpcHitMRPair, 0, sizeof(TpcHitMRPair));
	    TpcHitMRPair.sector   = rhit->sector();
	    TpcHitMRPair.row      = rhit->padrow();
	    TpcHitMRPair.xR       = rhit->position().x();
	    TpcHitMRPair.yR       = rhit->position().y();
	    TpcHitMRPair.zR       = rhit->position().z();
	    TpcHitMRPair.dER      = rhit->charge();
	    TpcHitMRPair.IdR      = rhit->idTruth();
	    TpcHitMRPair.qR       = rhit->qaTruth();
	    TpcHitMRPair.nR       = theHitMap->count(rhit);
	    mTpcHitNtuple->Fill(&TpcHitMRPair.sector);
	  }
	}
      }
    }
    for (UInt_t iSector=0;
	 iSector<mcHits->numberOfSectors(); iSector++) {
      
      if (Debug()) {cout << iSector + 1 << " "; flush(cout);}
      StMcTpcSectorHitCollection* tpcSectHitColl = mcHits->sector(iSector);
      for (UInt_t iPadrow=0;
	   iPadrow<tpcSectHitColl->numberOfPadrows();
	   iPadrow++) {
	StMcTpcPadrowHitCollection* tpcPadRowHitColl = tpcSectHitColl->padrow(iPadrow);
	for (UInt_t iHit=0;
	     iHit<tpcPadRowHitColl->hits().size();
	     iHit++){
	  const StMcTpcHit *mhit = dynamic_cast<const StMcTpcHit *> (tpcPadRowHitColl->hits()[iHit]);
	  assert (mhit);
	  if (mhit->TestBit(StMcHit::kMatched)) continue;
	  memset (&TpcHitMRPair, 0, sizeof(TpcHitMRPair));
	  TpcHitMRPair.sector   = mhit->sector();
	  TpcHitMRPair.row      = mhit->padrow();
	  TpcHitMRPair.isDet    = mhit->isDet();
	  TpcHitMRPair.xM       = mhit->position().x();
	  TpcHitMRPair.yM       = mhit->position().y();
	  TpcHitMRPair.zM       = mhit->position().z();
	  TpcHitMRPair.pxM      = mhit->localMomentum().x();
	  TpcHitMRPair.pyM      = mhit->localMomentum().y();
	  TpcHitMRPair.pzM      = mhit->localMomentum().z();
	  TpcHitMRPair.dEM      = mhit->dE();
	  TpcHitMRPair.dSM      = mhit->dS();
	  mTrack     = mhit->parentTrack();
	  if (mTrack) TpcHitMRPair.IdM = mTrack->key();
	  else        TpcHitMRPair.IdM = 0;
	  mTpcHitNtuple->Fill(&TpcHitMRPair.sector);
	}
      }
    }
  }

  //Pixel detector 
  
  if (pxlHitMap && pxlMcHitMap && mPxlHitNtuple) {  // pxl hits
    StPxlHitCollection* recHits = rEvent->pxlHitCollection();
    StMcPxlHitCollection* mcHits = mEvent->pxlHitCollection();
    if (recHits && mcHits) {
      for (UInt_t iSector=0; iSector < recHits->numberOfSectors(); iSector++) {
	for (UInt_t iLadder=0; iLadder<recHits->sector(iSector)->numberOfLadders(); iLadder++) {
	  for (UInt_t iSensor = 0; iSensor < recHits->sector(iSector)->ladder(iLadder)->numberOfSensors(); iSensor++) {
	    for (StPxlHitIterator iter = recHits->sector(iSector)->ladder(iLadder)->sensor(iSensor)->hits().begin();
		 iter != recHits->sector(iSector)->ladder(iLadder)->sensor(iSensor)->hits().end();
		 iter++) {
	      const StPxlHit   *rhit = dynamic_cast<const StPxlHit   *> (*iter);
	      assert(rhit);
	      if (rhit->TestBit(StMcHit::kMatched)) 
		{
		  pair<rcPxlHitMapIter,rcPxlHitMapIter> recBounds = pxlHitMap->equal_range(rhit);
		  for (rcPxlHitMapIter it2=recBounds.first; it2!=recBounds.second; ++it2){
		    const StMcPxlHit *mhit = dynamic_cast<const StMcPxlHit *> ((*it2).second);
		    assert ( mhit);
		    DeltaX = rhit->position().x() - mhit->position().x();
		    DeltaZ = rhit->position().z() - mhit->position().z();
		    mPxlHitResolution->Fill(DeltaX,DeltaZ);
		    memset (&PxlHitMRPair, 0, sizeof(PxlHitMRPair));
		    PxlHitMRPair.sector   = rhit->sector();
		    PxlHitMRPair.ladder   = rhit->ladder();
		    PxlHitMRPair.sensor   = rhit->sensor();
		    PxlHitMRPair.xR       = rhit->position().x();
		    PxlHitMRPair.yR       = rhit->position().y();
		    PxlHitMRPair.zR       = rhit->position().z();
		    PxlHitMRPair.dER      = rhit->charge();
		    PxlHitMRPair.IdR      = rhit->idTruth();
		    PxlHitMRPair.qR       = rhit->qaTruth();
		    PxlHitMRPair.nR       = pxlHitMap->count(rhit);
		    PxlHitMRPair.xM       = mhit->position().x();
		    PxlHitMRPair.yM       = mhit->position().y();
		    PxlHitMRPair.zM       = mhit->position().z();
		    PxlHitMRPair.pxM      = mhit->localMomentum().x();
		    PxlHitMRPair.pyM      = mhit->localMomentum().y();
		    PxlHitMRPair.pzM      = mhit->localMomentum().z();
		    PxlHitMRPair.dEM      = mhit->dE();
		    PxlHitMRPair.dSM      = mhit->dS();
		    mTrack     = mhit->parentTrack();
		    if (mTrack) PxlHitMRPair.IdM = mTrack->key();
		    else        PxlHitMRPair.IdM = 0;
		    PxlHitMRPair.nM     = pxlMcHitMap->count(mhit);
		    mPxlHitNtuple->Fill(&PxlHitMRPair.sector);
		  }
		}
	      else {
		memset (&PxlHitMRPair, 0, sizeof(PxlHitMRPair));
		PxlHitMRPair.sector   = rhit->sector();
		PxlHitMRPair.ladder   = rhit->ladder();
		PxlHitMRPair.sensor    = rhit->sensor();
		PxlHitMRPair.xR       = rhit->position().x();
		PxlHitMRPair.yR       = rhit->position().y();
		PxlHitMRPair.zR       = rhit->position().z();
		PxlHitMRPair.dER      = rhit->charge();
		PxlHitMRPair.IdR      = rhit->idTruth();
		PxlHitMRPair.qR       = rhit->qaTruth();
		PxlHitMRPair.nR       = pxlHitMap->count(rhit);
		mPxlHitNtuple->Fill(&PxlHitMRPair.sector);
	      }
	    }
	  }
	}
      }
      for (UInt_t iSector=0; iSector< mcHits->numberOfSectors(); iSector++) {
	for (UInt_t iLadder=0; iLadder<mcHits->sector(iSector)->numberOfLadders(); iLadder++) {
	  for (UInt_t iSensor = 0; iSensor < mcHits->sector(iSector)->ladder(iLadder)->numberOfSensors(); iSensor++) {
	    for (StMcPxlHitIterator iter = mcHits->sector(iSector)->ladder(iLadder)->sensor(iSensor)->hits().begin();
	       iter != mcHits->sector(iSector)->ladder(iLadder)->sensor(iSensor)->hits().end();
		 iter++) {
	      const StMcPxlHit   *mhit = dynamic_cast<const StMcPxlHit   *> (*iter);
	      assert (mhit);
	      if (mhit->TestBit(StMcHit::kMatched)) continue;
	      memset (&PxlHitMRPair, 0, sizeof(PxlHitMRPair));
	      PxlHitMRPair.sector   = mhit->sector();
	      PxlHitMRPair.ladder   = mhit->ladder();
	      PxlHitMRPair.sensor    = mhit->sensor();
	      PxlHitMRPair.sector   = mhit->sector();
	      PxlHitMRPair.ladder   = mhit->ladder();
	      PxlHitMRPair.xM       = mhit->position().x();
	      PxlHitMRPair.yM       = mhit->position().y();
	      PxlHitMRPair.zM       = mhit->position().z();
	      PxlHitMRPair.pxM      = mhit->localMomentum().x();
	      PxlHitMRPair.pyM      = mhit->localMomentum().y();
	      PxlHitMRPair.pzM      = mhit->localMomentum().z();
	      PxlHitMRPair.dEM      = mhit->dE();
	      PxlHitMRPair.dSM      = mhit->dS();
	      mTrack     = mhit->parentTrack();
	      if (mTrack) PxlHitMRPair.IdM = mTrack->key();
	      else        PxlHitMRPair.IdM = 0;
	      mPxlHitNtuple->Fill(&PxlHitMRPair.sector);
	    }
	  }
	}
      }
    }
  }

    //IST detector
  if (istHitMap && istMcHitMap && mIstHitNtuple) {  // ist hits
    StIstHitCollection* recHits = rEvent->istHitCollection();
    StMcIstHitCollection* mcHits = mEvent->istHitCollection();
    if (recHits && mcHits) {
      for (UInt_t iLadder=0; iLadder<24; iLadder++) {
	for (UInt_t iSensor = 0; iSensor < 6; iSensor++) {
	  for (StIstHitIterator iter = recHits->ladder(iLadder)->sensor(iSensor)->hits().begin();
	       iter != recHits->ladder(iLadder)->sensor(iSensor)->hits().end();
	       iter++) {
	    const StIstHit   *rhit = dynamic_cast<const StIstHit   *> (*iter);
	    assert(rhit);
	    if (rhit->TestBit(StMcHit::kMatched)) 
	      {
		pair<rcIstHitMapIter,rcIstHitMapIter> recBounds = istHitMap->equal_range(rhit);
		for (rcIstHitMapIter it2=recBounds.first; it2!=recBounds.second; ++it2){
		  const StMcIstHit *mhit = dynamic_cast<const StMcIstHit *> ((*it2).second);
		  assert ( mhit);
		  DeltaX = rhit->position().x() - mhit->position().x();
		  DeltaZ = rhit->position().z() - mhit->position().z();
		  mIstHitResolution->Fill(DeltaX,DeltaZ);
		  memset (&IstHitMRPair, 0, sizeof(IstHitMRPair));
		  IstHitMRPair.ladder   = rhit->getLadder();
		  IstHitMRPair.sensor   = rhit->getSensor();
		  IstHitMRPair.xR       = rhit->position().x();
		  IstHitMRPair.yR       = rhit->position().y();
		  IstHitMRPair.zR       = rhit->position().z();
		  IstHitMRPair.dER      = rhit->charge();
		  IstHitMRPair.IdR      = rhit->idTruth();
		  IstHitMRPair.qR       = rhit->qaTruth();
		  IstHitMRPair.nR       = istHitMap->count(rhit);
		  IstHitMRPair.xM       = mhit->position().x();
		  IstHitMRPair.yM       = mhit->position().y();
		  IstHitMRPair.zM       = mhit->position().z();
		  IstHitMRPair.pxM      = mhit->localMomentum().x();
		  IstHitMRPair.pyM      = mhit->localMomentum().y();
		  IstHitMRPair.pzM      = mhit->localMomentum().z();
		  IstHitMRPair.dEM      = mhit->dE();
		  IstHitMRPair.dSM      = mhit->dS();
		  mTrack     = mhit->parentTrack();
		  if (mTrack) IstHitMRPair.IdM = mTrack->key();
		  else        IstHitMRPair.IdM = 0;
		  IstHitMRPair.nM     = istMcHitMap->count(mhit);
		  mIstHitNtuple->Fill(&IstHitMRPair.ladder);
		}
	      }
	    else {
	      memset (&IstHitMRPair, 0, sizeof(IstHitMRPair));
	      IstHitMRPair.ladder   = rhit->getLadder();
	      IstHitMRPair.sensor   = rhit->getSensor();
	      IstHitMRPair.xR       = rhit->position().x();
	      IstHitMRPair.yR       = rhit->position().y();
	      IstHitMRPair.zR       = rhit->position().z();
	      IstHitMRPair.dER      = rhit->charge();
	      IstHitMRPair.IdR      = rhit->idTruth();
	      IstHitMRPair.qR       = rhit->qaTruth();
	      IstHitMRPair.nR       = istHitMap->count(rhit);
	      mIstHitNtuple->Fill(&IstHitMRPair.ladder);
	    }
	  }
	}
      }
      for (UInt_t iLadder=0; iLadder<24; iLadder++) {
	for (UInt_t iSensor = 0; iSensor < 6; iSensor++) {
	  for (StMcIstHitIterator iter = mcHits->ladder(iLadder)->sensor(iSensor)->hits().begin();
	       iter != mcHits->ladder(iLadder)->sensor(iSensor)->hits().end();
	       iter++) {
	    const StMcIstHit   *mhit = dynamic_cast<const StMcIstHit   *> (*iter);
	    assert (mhit);
	    if (mhit->TestBit(StMcHit::kMatched)) continue;
	    memset (&IstHitMRPair, 0, sizeof(IstHitMRPair));
	    IstHitMRPair.ladder   = mhit->ladder();
	    IstHitMRPair.sensor    = mhit->sensor();
	    IstHitMRPair.ladder   = mhit->ladder();
	    IstHitMRPair.xM       = mhit->position().x();
	    IstHitMRPair.yM       = mhit->position().y();
	    IstHitMRPair.zM       = mhit->position().z();
	    IstHitMRPair.pxM      = mhit->localMomentum().x();
	    IstHitMRPair.pyM      = mhit->localMomentum().y();
	    IstHitMRPair.pzM      = mhit->localMomentum().z();
	    IstHitMRPair.dEM      = mhit->dE();
	    IstHitMRPair.dSM      = mhit->dS();
	    mTrack     = mhit->parentTrack();
	    if (mTrack) IstHitMRPair.IdM = mTrack->key();
	    else        IstHitMRPair.IdM = 0;
	    mIstHitNtuple->Fill(&IstHitMRPair.ladder);
	  }
	}
      }
    }
  }
  //SSD
 if (ssdHitMap && ssdMcHitMap && mSsdHitNtuple) {  // ssd hits
    StSsdHitCollection* recHits = rEvent->ssdHitCollection();
    StMcSsdHitCollection* mcHits = mEvent->ssdHitCollection();
    if (recHits && mcHits) {
      for (unsigned int iLadder=0; iLadder<recHits->numberOfLadders(); iLadder++) {
	for (unsigned int iWafer = 0; iWafer < recHits->ladder(iLadder)->numberOfWafers(); iWafer++) {
	  for (StSsdHitIterator iter = recHits->ladder(iLadder)->wafer(iWafer)->hits().begin();
	       iter != recHits->ladder(iLadder)->wafer(iWafer)->hits().end();
	       iter++) {
	    const StSsdHit   *rhit = dynamic_cast<const StSsdHit   *> (*iter);
	    assert(rhit);
	    if (rhit->TestBit(StMcHit::kMatched)) 
	      {
		pair<rcSsdHitMapIter,rcSsdHitMapIter> recBounds = ssdHitMap->equal_range(rhit);
		for (rcSsdHitMapIter it2=recBounds.first; it2!=recBounds.second; ++it2){
		  const StMcSsdHit *mhit = dynamic_cast<const StMcSsdHit *> ((*it2).second);
		  assert ( mhit);
		  DeltaX = rhit->position().x() - mhit->position().x();
		  DeltaZ = rhit->position().z() - mhit->position().z();
		  mSsdHitResolution->Fill(DeltaX,DeltaZ);
		  memset (&SsdHitMRPair, 0, sizeof(SsdHitMRPair));
		  SsdHitMRPair.ladder   = rhit->ladder();
		  SsdHitMRPair.wafer    = rhit->wafer();
		  SsdHitMRPair.xR       = rhit->position().x();
		  SsdHitMRPair.yR       = rhit->position().y();
		  SsdHitMRPair.zR       = rhit->position().z();
		  SsdHitMRPair.dER      = rhit->charge();
		  SsdHitMRPair.IdR      = rhit->idTruth();
		  SsdHitMRPair.qR       = rhit->qaTruth();
		  SsdHitMRPair.nR       = ssdHitMap->count(rhit);
		  SsdHitMRPair.xM       = mhit->position().x();
		  SsdHitMRPair.yM       = mhit->position().y();
		  SsdHitMRPair.zM       = mhit->position().z();
		  SsdHitMRPair.pxM      = mhit->localMomentum().x();
		  SsdHitMRPair.pyM      = mhit->localMomentum().y();
		  SsdHitMRPair.pzM      = mhit->localMomentum().z();
		  SsdHitMRPair.dEM      = mhit->dE();
		  SsdHitMRPair.dSM      = mhit->dS();
		  mTrack     = mhit->parentTrack();
		  if (mTrack) SsdHitMRPair.IdM = mTrack->key();
		  else        SsdHitMRPair.IdM = 0;
		  SsdHitMRPair.nM     = ssdMcHitMap->count(mhit);
		  mSsdHitNtuple->Fill(&SsdHitMRPair.ladder);
		}
	      }
	    else {
	      memset (&SsdHitMRPair, 0, sizeof(SsdHitMRPair));
	      SsdHitMRPair.ladder   = rhit->ladder();
	      SsdHitMRPair.wafer    = rhit->wafer();
	      SsdHitMRPair.xR       = rhit->position().x();
	      SsdHitMRPair.yR       = rhit->position().y();
	      SsdHitMRPair.zR       = rhit->position().z();
	      SsdHitMRPair.dER      = rhit->charge();
	      SsdHitMRPair.IdR      = rhit->idTruth();
	      SsdHitMRPair.qR       = rhit->qaTruth();
	      SsdHitMRPair.nR       = ssdHitMap->count(rhit);
	      mSsdHitNtuple->Fill(&SsdHitMRPair.ladder);
	    }
	  }
	}
      }
      for (unsigned int iLadder=0; iLadder<mcHits->numberOfLadders(); iLadder++) {
	for (unsigned int iWafer = 0; iWafer < mcHits->ladder(iLadder)->numberOfWafers(); iWafer++) {
	  for (StMcSsdHitIterator iter = mcHits->ladder(iLadder)->wafer(iWafer)->hits().begin();
	       iter != mcHits->ladder(iLadder)->wafer(iWafer)->hits().end();
	       iter++) {
	    const StMcSsdHit   *mhit = dynamic_cast<const StMcSsdHit   *> (*iter);
	    assert (mhit);
	    if (mhit->TestBit(StMcHit::kMatched)) continue;
	    memset (&SsdHitMRPair, 0, sizeof(SsdHitMRPair));
	    SsdHitMRPair.ladder   = mhit->ladder();
	    SsdHitMRPair.wafer    = mhit->wafer();
	    SsdHitMRPair.ladder   = mhit->ladder();
	    SsdHitMRPair.xM       = mhit->position().x();
	    SsdHitMRPair.yM       = mhit->position().y();
	    SsdHitMRPair.zM       = mhit->position().z();
	    SsdHitMRPair.pxM      = mhit->localMomentum().x();
	    SsdHitMRPair.pyM      = mhit->localMomentum().y();
	    SsdHitMRPair.pzM      = mhit->localMomentum().z();
	    SsdHitMRPair.dEM      = mhit->dE();
	    SsdHitMRPair.dSM      = mhit->dS();
	    mTrack     = mhit->parentTrack();
	    if (mTrack) SsdHitMRPair.IdM = mTrack->key();
	    else        SsdHitMRPair.IdM = 0;
	    mSsdHitNtuple->Fill(&SsdHitMRPair.ladder);
	  }
	}
      }
    }
  }
  cout << "Finished Making Histogram." << endl;
  
  if (!theTrackMap) {
    gMessMgr->Warning() << "----------WARNING----------\n"
			<< "No Track Map found for this event!" << endm;
    return kStWarn;
  }
  // Example: look at the magnitude of the momentum of
  //          the MC track associated with first track in track Collection
  
  StSPtrVecTrackNode& rcTrackNodes = rEvent->trackNodes();
  StTrackNode*        firstTrackNode = 0;
  const StGlobalTrack*      firstTrack = 0;
  if (! rcTrackNodes.size()) {
    cout << "Track Nodes List is empty!" << endl;
    return kStWarn;
  }
  firstTrackNode = *(rcTrackNodes.begin());
  if (! firstTrackNode) {
    cout << "No tracks in Track Nodes List!" << endl;
    return kStWarn;
  }
  firstTrack = dynamic_cast<const StGlobalTrack*>(firstTrackNode->track(global));
  if (! firstTrack) {
    cout << "GlobalTrack for first Track Nodehas not been found!" << endl;
    return kStWarn;
  }
  cout << "MC Tracks associated with first Track in collection: " << theTrackMap->count(firstTrack) << endl;
  if (!theTrackMap->count(firstTrack) && theTrackMap->size()) {
      cout << "First track in track node container was not associated.  Pick first track in map." << endl;
      firstTrack = theTrackMap->begin()->first; //first entry in the map is a pair, and first entry of pair is the global track
  }
  pair<rcTrackMapIter,rcTrackMapIter> trackBounds = theTrackMap->equal_range(firstTrack);
  cout << "Momentum of First Track and of Associated Tracks (if there are any):" << endl;
  // Get the momentum of the track and compare it to MC Track.
  // Use primary track if available
  const StPrimaryTrack* pTrk = dynamic_cast<const StPrimaryTrack*>(firstTrack->node()->track(primary));
  StThreeVectorD recMom(0,0,0);
  if (pTrk)
    recMom = pTrk->geometry()->momentum();
  else
    recMom = firstTrack->geometry()->momentum();
  for (rcTrackMapIter trkIt=trackBounds.first; trkIt!=trackBounds.second; ++trkIt) { 
    const StMcTrack*   origTrk = (*trkIt).second->partnerMcTrack();
    cout << "[" << abs(recMom) << ", ";
    cout << abs((*trkIt).second->partnerMcTrack()->momentum()) << "]" << endl;
    cout << "These tracks have : \n";
    cout << (*trkIt).second->commonTpcHits() << " TPC  hits in common, out of " << origTrk->tpcHits().size() << endl;
    cout << (*trkIt).second->commonPxlHits() << " PXL  hits in common, out of " << origTrk->pxlHits().size() << endl;
    cout << (*trkIt).second->commonIstHits() << " IST  hits in common, out of " << origTrk->istHits().size() << endl;
    cout << (*trkIt).second->commonSsdHits() << " SSD  hits in common, out of " << origTrk->ssdHits().size() << endl;
    cout << (*trkIt).second->commonFtpcHits() <<" FTPC hits in common, out of " << origTrk->ftpcHits().size() << endl;
  }
  // Example: Make a histogram of the momentum resolution of the event
  //          Make an Ntuple with rec & monte carlo mom, mean hit difference, and # of common hits
  const StGlobalTrack* recTrack;
  const StPrimaryTrack* primTrk;
  const StMcTrack*     mcTrack;
  StThreeVectorD p(0,0,0);
  StThreeVectorD pmc(0,0,0);
  Float_t diff =0;
  
  Float_t* values = new Float_t[19];
  
  for (rcTrackMapIter tIter=theTrackMap->begin();
       tIter!=theTrackMap->end(); ++tIter){
    
    recTrack = (*tIter).first;
    //yf    if ((*tIter).second->commonTpcHits()<10) continue;
    mcTrack = (*tIter).second->partnerMcTrack();
    pmc = mcTrack->momentum();
    for (Int_t k=0; k<3; k++) values[k] = pmc[k];  	 
    values[3]=pmc.mag(); 	 
    
    primTrk = dynamic_cast<const StPrimaryTrack*>(recTrack->node()->track(primary)); 	 
    if (primTrk) 	 
	p = primTrk->geometry()->momentum(); 	 
    else 	 
	p = recTrack->geometry()->momentum(); 	 
    
    for (Int_t j=0; j<3; j++) values[j+4] = p[j]; 	 
    values[7]=p.mag(); 	 
    values[8]=(*tIter).second->commonTpcHits();
    // Fill 1d Mom. resolution Histogram
    
    diff = (p.mag() - pmc.mag())/pmc.mag();
    mMomResolution->Fill(diff,1.);
    // Loop to get Mean hit position diff.
    StThreeVectorF rHitPos(0,0,0);
    StThreeVectorF mHitPos(0,0,0);
    
    StPtrVecHit recTpcHits = recTrack->detectorInfo()->hits(kTpcId);
    multimap<Int_t,Float_t> idTruths;
    set<Int_t> uniqueIdTruths;
    for (StHitIterator hi=recTpcHits.begin();
	 hi!=recTpcHits.end(); hi++) {
      StHit* hit = *hi;
      StTpcHit* rHit = dynamic_cast<StTpcHit*>(hit);
      if (!rHit) { cout << "This Hit is not a TPC Hit"<< endl; continue;}
      idTruths.insert( multimap<Int_t,Float_t>::value_type(rHit->idTruth(),rHit->qaTruth()));
      uniqueIdTruths.insert(static_cast<Int_t>(rHit->idTruth()));
      pair<rcTpcHitMapIter,rcTpcHitMapIter> rBounds = theHitMap->equal_range(rHit);
      for (rcTpcHitMapIter hIter=rBounds.first; hIter!=rBounds.second; hIter++) {
	const StMcTpcHit* mHit = (*hIter).second;
	if (mHit->parentTrack() != mcTrack) continue;
	rHitPos += rHit->position();
	mHitPos += mHit->position();
      }// Associated Hits Loop.
      
    } // Hits of rec. Track Loop
    
    rHitPos /=(Float_t) (*tIter).second->commonTpcHits();
    mHitPos /=(Float_t) (*tIter).second->commonTpcHits();
    for (Int_t jj=0; jj<3; jj++) values[9+jj] = rHitPos[jj] - mHitPos[jj];
    values[12] = mcTrack->key();
    // Figure out the most common IdTruth; the dominatrix track!
    Int_t mostCommonIdTruth = -9; 
    Int_t cachedNHitsIdTruth = 0;
    for (set<Int_t>::iterator si=uniqueIdTruths.begin(); si!=uniqueIdTruths.end(); ++si) {
	Int_t currentNHitsIdTruth = idTruths.count(static_cast<Int_t>(*si));
	if (currentNHitsIdTruth>cachedNHitsIdTruth) {
	    mostCommonIdTruth = *si; 
	    cachedNHitsIdTruth = currentNHitsIdTruth;
	}
    }
    // at this point we know the most common IdTruth,
    // now calculate the track "quality" for this track, averaging
    // the hit qualities
    Float_t idQuality = 0;
    
    pair<multimap<Int_t,Float_t>::iterator,multimap<Int_t,Float_t>::iterator> mostCommRange = idTruths.equal_range(mostCommonIdTruth);
    for (multimap<Int_t,Float_t>::iterator mi=mostCommRange.first; mi!=mostCommRange.second; ++mi) {
	idQuality+=mi->second;
    }
    idQuality/=cachedNHitsIdTruth;
    
    values[13] = mostCommonIdTruth;
    values[14] = cachedNHitsIdTruth;
    values[15] = mcTrack->tpcHits().size();
    values[16] = recTrack->fitTraits().numberOfFitPoints(kTpcId);
    values[17] = recTpcHits.size();
    values[18] = idQuality;
    mTrackNtuple->Fill(values);
  } // Tracks in Map Loop
  cout << "Finished Track Loop, Made Ntuple" << endl;
  //delete vars;
  delete [] values;
  //mNtupleFile->Write(); // Write the Ntuple to the File in Finish(), not here
  
  // Example: Make 2 Histograms
  // - x and y positions of the hits from the reconstructed track.
  // - x and y positions of the hits from the  Monte Carlo  track.
  
  UInt_t maxCommonTpcHits = 0;
  const StMcTrack* partner = 0;
  trackBounds = theTrackMap->equal_range(firstTrack);
  for (rcTrackMapIter rcIt = trackBounds.first;
       rcIt != trackBounds.second;
       ++rcIt) {
    if ((*rcIt).second->commonTpcHits() >  maxCommonTpcHits) {
      partner = (*rcIt).second->partnerMcTrack();
      maxCommonTpcHits = (*rcIt).second->commonTpcHits();
    }
  }
  StHitIterator rcHitIt;
  StMcTpcHitIterator mcHitIt;
  StMcPxlHitIterator mcSitIt;
  StPtrVecHit theHits = firstTrack->detectorInfo()->hits();
  for (rcHitIt  = theHits.begin();
       rcHitIt != theHits.end();
       rcHitIt++) coordRec->Fill((*rcHitIt)->position().x(),(*rcHitIt)->position().y());
  if (partner) {
    for (mcHitIt  = ((std::vector<StMcTpcHit*> *)&partner->tpcHits() )->begin();
	 mcHitIt != partner->tpcHits().end();
	 mcHitIt++) coordMcPartner->Fill((*mcHitIt)->position().x(),(*mcHitIt)->position().y());
    /* for (mcSitIt  = ((std::vector<StMcPxlHit*> *)&partner->pxlHits())->begin();
	 mcSitIt != partner->pxlHits().end();
	 mcSitIt++) coordMcPartner->Fill((*mcSitIt)->position().x(),(*mcSitIt)->position().y());*/
    
  }
  if (!theMcV0Map) {
    gMessMgr->Warning() << "----------WARNING----------\n"
			<< "No V0 Map found for this event!" << endm;
    return kStWarn;
  }
  //Example: Print out position of V0 vertices that have been associated.
  //         (LSB)
  StSPtrVecMcVertex& mcVertices = mEvent->vertices();
  const StV0Vertex* rcV0Partner;
  StMcVertexIterator mcVertexIt;
  
  //Loop over all MC vertices
  bool foundV0Pair = false;
  for (mcVertexIt = mcVertices.begin(); mcVertexIt != mcVertices.end();
       mcVertexIt++){
    // Get the upper and lower bounds.
    pair<mcV0MapIter,mcV0MapIter> mcV0Bounds = theMcV0Map->equal_range(*mcVertexIt);
    
    // Print out MC vertex position if there is an associated V0.
    
    if (mcV0Bounds.first != mcV0Bounds.second) {
      cout << "Printing Position of a V0 pair:\n";
      cout << "Position of MC V0 vertex: " << (*mcVertexIt)->position() << endl;
      foundV0Pair = true;
    }
    //Now loop over the bounds      
    for(mcV0MapIter mcV0MapIt = mcV0Bounds.first;
	mcV0MapIt != mcV0Bounds.second; ++mcV0MapIt){
      rcV0Partner = (*mcV0MapIt).second;
      cout << "Position of rc V0 vertex: " << rcV0Partner->position() << endl;
      
    }
    if (foundV0Pair) break; // Only print the information of 1 reconstructed vertex, to avoid a lot of output.
  }
  
  
  
  //mAssociationCanvas = new TCanvas("mAssociationCanvas", "Histograms",200,10,900,500);
  
  return kStOK;
}

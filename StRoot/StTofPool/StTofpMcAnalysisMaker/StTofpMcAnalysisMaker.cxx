/*************************************************
 *
 * $Id: StTofpMcAnalysisMaker.cxx,v 1.5 2018/02/26 23:13:20 smirnovd Exp $
 *
 *************************************************/

#include <Stiostream.h>
#include "TH1.h"
#include "TH2.h"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StEventTypes.h"
#include "StMcEventTypes.hh"
#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"
#include "StEventMaker/StEventMaker.h"
#include "StTofUtil/StTofGeometry.h"
#include "StTofUtil/tofPathLength.hh"
#include "StMessMgr.h"
#include "StTofpMcAnalysisMaker.h"

ClassImp(StTofpMcAnalysisMaker)


const Int_t   StTofpMcAnalysisMaker::mPtBin = 50;
const Int_t   StTofpMcAnalysisMaker::mYBin  = 14;
const Float_t StTofpMcAnalysisMaker::mPtMin = 0.;
const Float_t StTofpMcAnalysisMaker::mPtMax = 5.;
const Float_t StTofpMcAnalysisMaker::mYMin  = -1.2;
const Float_t StTofpMcAnalysisMaker::mYMax  =  0.2;



//----------------------------------------------------------------
const StGlobalTrack*  partnerTrack(mcTrackMapType* map, StMcTrack* mT) {
  mcTrackMapIter i = map->find(mT);
  const StGlobalTrack* rT = 0;
  if (i != map->end()) rT = (*i).second->partnerTrack();
  return rT;
}



//_________________________________________________
StTofpMcAnalysisMaker::StTofpMcAnalysisMaker(const char *name, const char *title):StMaker(name,title)
{
  //  StTofpMcAnalysisMaker Constructor

  mOuterTrackGeometry = true;
  mMinHitsPerTrack = 0;
  

  // - zero all pointers defined in the header file
  hMcPionPlus      = 0;
  hMcPionMin       = 0;
  hMcKaonPlus      = 0;
  hMcKaonMin       = 0;
  hMcProton        = 0;
  hMcAntiProton    = 0;
  hMcElectron      = 0;
  hRcPionPlus      = 0;
  hRcPionMin       = 0;
  hRcKaonPlus      = 0;
  hRcKaonMin       = 0;
  hRcProton        = 0;
  hRcAntiProton    = 0;
  hRcElectron      = 0;  
  hMatchPionPlus   = 0;
  hMatchPionMin    = 0;
  hMatchKaonPlus   = 0;
  hMatchKaonMin    = 0;
  hMatchProton     = 0;
  hMatchAntiProton = 0;
  hMatchElectron   = 0;
  //-
  hMomResPtPion = 0;
  hMultRef = 0;
  //-
  hTofpHitMap1  = 0;
  hTofpHitMap2  = 0;
  hTofpHitMap3  = 0;
  hTofpHitMap4  = 0;
  hTofpSlatIdA0 = 0;
  hTofpSlatIdA1 = 0;
  hTofpSlatIdB1 = 0;
  hTofpSlatIdD1 = 0;
  hTofpSlatIdD2 = 0;
  hTofpSlatIdE1 = 0; 
  hTofpSlatIdE2 = 0;
  hTofpSlatIdE3 = 0;
  hTofpSlatIdE4 = 0;
  hTofpSlatIdE5 = 0;
  hTofpSlatIdF1 = 0;
  hTofpSlatHitVecSize = 0;
}

//_________________________________________________
StTofpMcAnalysisMaker::~StTofpMcAnalysisMaker()
{
  //  StTofpMcAnalysisMaker Destructor
  if (Debug()) LOG_INFO << "Inside StTofpMcAnalysisMaker Destructor" << endm;
}

//_____________________________________________________________________________

void StTofpMcAnalysisMaker::Clear(const char*)
{
  StMaker::Clear();
}

//_________________________________________________
Int_t StTofpMcAnalysisMaker::Finish()
{
  return StMaker::Finish();
}


//_________________________________________________
Int_t StTofpMcAnalysisMaker::Init()
{
  // StTofpMcAnalysisMaker - Init
  
  hMomResPtPion = new TH2F("momResMomTPion","(|pmc| - |p|)/|pmc| as a Function of Pt Pion",mPtBin,mPtMin,mPtMax,200,-1.,1.);
  hMomResPtPion->SetXTitle("Pt (GeV/c)");
  hMomResPtPion->SetYTitle("(|pmc| - |p|)/|pmc| (GeV/c)");

  hMultRef = new TH1F("multRef","Mult Ref",100,0.,1000.);
  hMultRef->SetXTitle("uncorrected negative primary tracks");

  //-
  hMcElectron = new TH2F("mcElectron","Mc Electron",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMcElectron->SetXTitle("p_{T} (GeV/c)");
  hMcElectron->SetYTitle("pseudorapidity");
  hMcElectron->Sumw2();
  hRcElectron = new TH2F("rcElectron","RcElectron",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hRcElectron->SetXTitle("p_{T} (GeV/c)");
  hRcElectron->SetYTitle("pseudorapidity");
  hRcElectron->Sumw2();
  hMatchElectron = new TH2F("mElectron","TOF matched electron",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMatchElectron->Sumw2();
  hMatchElectron->SetXTitle("p_{T} (GeV/c)");
  hMatchElectron->SetYTitle("pseudorapidity");

  //-
  hMcPionPlus = new TH2F("mcPionPlus","Mc Pion Plus",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMcPionPlus->SetXTitle("p_{T} (GeV/c)");
  hMcPionPlus->SetYTitle("pseudorapidity");
  hMcPionPlus->Sumw2();
  hRcPionPlus = new TH2F("rcPionPlus","Rc Pion Plus",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hRcPionPlus->SetXTitle("p_{T} (GeV/c)");
  hRcPionPlus->SetYTitle("pseudorapidity");
  hRcPionPlus->Sumw2();
  hMatchPionPlus = new TH2F("mPionPlus","TOF matched pi+",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMatchPionPlus->Sumw2();
  hMatchPionPlus->SetXTitle("p_{T} (GeV/c)");
  hMatchPionPlus->SetYTitle("pseudorapidity");

  //-
  hMcPionMin = new TH2F("mcPionMin","Mc Pion Min",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMcPionMin->SetXTitle("p_{T} (GeV/c)");
  hMcPionMin->SetYTitle("pseudorapidity");
  hMcPionMin->Sumw2();
  hRcPionMin = new TH2F("rcPionMin","Rc Pion Min",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hRcPionMin->SetXTitle("p_{T} (GeV/c)");
  hRcPionMin->SetYTitle("pseudorapidity");
  hRcPionMin->Sumw2();
  hMatchPionMin = new TH2F("mPionMin","TOF matched pi-",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMatchPionMin->Sumw2();
  hMatchPionMin->SetXTitle("p_{T} (GeV/c)");
  hMatchPionMin->SetYTitle("pseudorapidity");

  //-
  hMcKaonPlus = new TH2F("mcKaonPlus","Mc Kaon Plus",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMcKaonPlus->SetXTitle("p_{T} (GeV/c)");
  hMcKaonPlus->SetYTitle("pseudorapidity");
  hMcKaonPlus->Sumw2();
  hRcKaonPlus = new TH2F("rcKaonPlus","Rc Kaon Plus",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hRcKaonPlus->SetXTitle("p_{T} (GeV/c)");
  hRcKaonPlus->SetYTitle("pseudorapidity");
  hRcKaonPlus->Sumw2();
  hMatchKaonPlus = new TH2F("mKaonPlus","TOF matched K+",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMatchKaonPlus->Sumw2();
  hMatchKaonPlus->SetXTitle("p_{T} (GeV/c)");
  hMatchKaonPlus->SetYTitle("pseudorapidity");

  //-
  hMcKaonMin = new TH2F("mcKaonMin","Mc Kaon Min",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMcKaonMin->SetXTitle("p_{T} (GeV/c)");
  hMcKaonMin->SetYTitle("pseudorapidity");
  hMcKaonMin->Sumw2();
  hRcKaonMin = new TH2F("rcKaonMin","Rc Kaon Min",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hRcKaonMin->SetXTitle("p_{T} (GeV/c)");
  hRcKaonMin->SetYTitle("pseudorapidity");
  hRcKaonMin->Sumw2();
  hMatchKaonMin = new TH2F("mKaonMin","TOF matched K-",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMatchKaonMin->Sumw2();
  hMatchKaonMin->SetXTitle("p_{T} (GeV/c)");
  hMatchKaonMin->SetYTitle("pseudorapidity");

  //-
  hMcProton = new TH2F("mcProton","Mc Proton",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMcProton->SetXTitle("p_{T} (GeV/c)");
  hMcProton->SetYTitle("pseudorapidity");
  hMcProton->Sumw2();
  hRcProton = new TH2F("rcProton","RcProton",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hRcProton->SetXTitle("p_{T} (GeV/c)");
  hRcProton->SetYTitle("pseudorapidity");
  hRcProton->Sumw2();
  hMatchProton = new TH2F("mProton","TOF matched proton",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMatchProton->Sumw2();
  hMatchProton->SetXTitle("p_{T} (GeV/c)");
  hMatchProton->SetYTitle("pseudorapidity");

  //-
  hMcAntiProton = new TH2F("mcAntiProton","Mc Antiproton",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMcAntiProton->SetXTitle("p_{T} (GeV/c)");
  hMcAntiProton->SetYTitle("pseudorapidity");
  hMcAntiProton->Sumw2();
  hRcAntiProton = new TH2F("rcAntiProton","Rc Antiproton",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hRcAntiProton->SetXTitle("p_{T} (GeV/c)");
  hRcAntiProton->SetYTitle("pseudorapidity");
  hRcAntiProton->Sumw2();
  hMatchAntiProton = new TH2F("mAntiProton","TOF matched pbar",mPtBin,mPtMin,mPtMax,mYBin,mYMin,mYMax);
  hMatchAntiProton->Sumw2();
  hMatchAntiProton->SetXTitle("p_{T} (GeV/c)");
  hMatchAntiProton->SetYTitle("pseudorapidity");

  // Match QA Histograms
  hTofpSlatIdA0 = new TH1D("SlatIdA0","events per slat",41,0.5,41.5);
  hTofpSlatIdA1 = new TH1D("SlatIdA1","valid slat",41,0.5,41.5);
  hTofpSlatIdB1 = new TH1D("SlatIdB1","#tracks match  valid slat",41,0.5,41.5);
  hTofpSlatIdD1 = new TH1D("SlatIdD1","track match per valid slat",41,0.5,41.5);
  hTofpSlatIdD2 = new TH1D("SlatIdD2","single track match per slat",41,0.5,41.5);
  hTofpSlatIdE1 = new TH1D("SlatIdE1","one slat for one track match",41,0.5,41.5);
  hTofpSlatIdE2 = new TH1D("SlatIdE2","recovered from hitprof-weight",41,0.5,41.5);
  hTofpSlatIdE3 = new TH1D("SlatIdE3","recovered from ss",41,0.5,41.5);
  hTofpSlatIdE4 = new TH1D("SlatIdE4","recovered from closest hitplane",41,0.5,41.5);
  hTofpSlatIdE5 = new TH1D("SlatIdE5","total recovered slat per track match",41,0.5,41.5);
  hTofpSlatIdF1 = new TH1D("SlatIdF1","primary track match per slat",41,0.5,41.5);
  hTofpSlatHitVecSize = new TH1D("SlatMult","Slat Mult per Track",10,0,10);
  hTofpHitMap1 = new TH2D("tofpHitMap1","valid hit positions", 500,-250,0, 120,-1.23, -1.08);
  hTofpHitMap2 = new TH2D("tofpHitMap2","valid hit positions", 500,-250,0, 120,-1.23, -1.08);
  hTofpHitMap3 = new TH2D("tofpHitMap3","valid hit positions", 500,-250,0, 120,-1.23, -1.08);
  hTofpHitMap4 = new TH2D("tofpHitMap4","valid hit positions", 500,-250,0, 120,-1.23, -1.08);

  return StMaker::Init();
}


Int_t StTofpMcAnalysisMaker::InitRun(int runnumber){
  gMessMgr->Info("StTofpMcAnalysisMaker -- reinitializing TofGeometry","OS");
  mTofGeom = new StTofGeometry();
  mTofGeom->init(this);
  return kStOk;
}

Int_t StTofpMcAnalysisMaker::FinishRun(int runnumber){
  gMessMgr->Info("StTofpMcAnalysisMaker -- cleaning up geometry","OS" );
  if (mTofGeom) delete mTofGeom;
  mTofGeom=0;
  return kStOk;
}

//_________________________________________________
Int_t StTofpMcAnalysisMaker::Make(){

  // Get the pointers we need, we have to use the titles we gave them in the
  // macro.  I just used the defaults.

  // StEvent
  StEvent* rEvent = 0;
  rEvent = (StEvent*) GetInputDS("StEvent");

  // StMcEvent
  StMcEvent* mEvent = (StMcEvent*) GetDataSet("StMcEvent");

  // StAssociationMaker
  StAssociationMaker* assoc = 0;
  assoc = (StAssociationMaker*) GetMaker("StAssociationMaker");

  // the Multimaps...
  rcTpcHitMapType* theHitMap = 0;
  theHitMap = assoc->rcTpcHitMap();
  rcTrackMapType* theTrackMap = 0;
  theTrackMap = assoc->rcTrackMap();
  mcV0MapType* theMcV0Map = 0;
  theMcV0Map = assoc->mcV0Map(); 

  if (!theHitMap) {
    gMessMgr->Warning() << "----------WARNING----------\n"
			<< "No Hit Map found for this event!" << endm;
    return kStWarn;
  }

  mcTrackMapType* mcTrackMap = 0;
  mcTrackMap = assoc->mcTrackMap();
  if (!mcTrackMap){
    LOG_INFO << "No mcTrackMap!!! " << endm;
    return 0;
  }


  // Example: look at the position of the primary vertex
  //          Map is not needed for this, but it's a good check,
  //          tracking will not be good if primary vertex was not well placed.


  // First check whether the Primary Vertex is there at all.
  StThreeVectorD VertexPos(0,0,0);
  if (rEvent->primaryVertex()) {
    VertexPos = rEvent->primaryVertex()->position();
    LOG_INFO << "Position of Primary Vertex from StEvent:" << endm;
    LOG_INFO << VertexPos << endm;
  }
  else {
    LOG_INFO << "----------- WARNING ------------" << endm;
    LOG_INFO << "No primary vertex from StEvent!!" << endm;
    LOG_INFO << "Assume it is at (0,0,0)         " << endm;
  }
  LOG_INFO << "Position of Primary Vertex from StMcEvent:" << endm;
  LOG_INFO << mEvent->primaryVertex()->position() << endm;
    
  if (!theTrackMap) {
    gMessMgr->Warning() << "----------WARNING----------\n"
			<< "No Track Map found for this event!" << endm;
    return kStWarn;
  }

  // -----------------------------------------
  // --- Stage I: MATCH RC-TRACKS to SLATS ---
  // -----------------------------------------

  // Note: why not replaced Stage I w/ the actual TOFpMatchMaker and
  //       use the StTofSlat in StEvent as the reference for Stage IIb?


  StEvent *event = rEvent;
  bool mHisto(true);
  int NTOFP(41);
  //.........................................................................
  // A. build vector of candidate slats with valid ADC signals 
  idVector validSlatIdVec;
  for (int i=0;i<NTOFP;i++){
    unsigned short slatId = mTofGeom->daqToSlatId(i);
    hTofpSlatIdA0->Fill(i+1);

    validSlatIdVec.push_back(slatId);
    hTofpSlatIdA1->Fill(i+1);
  }
  gMessMgr->Info("","OST") << "A: #valid slats: " << validSlatIdVec.size() << endm;
  // end of Sect.A


  //.........................................................................
  // B. loop over global tracks and determine all slat-track matches
  //
  tofSlatHitVector allSlatsHitVec;
  allSlatsHitVec.clear();
  StSPtrVecTrackNode& nodes = event->trackNodes();
  int nAllTracks=0;
  for (unsigned int iNode=0; iNode<nodes.size(); iNode++){
    tofSlatHitVector slatHitVec;
    slatHitVec.clear();
    StTrack *theTrack = nodes[iNode]->track(global);

    // make sure we have a track, a miniDST might have removed it...
    if (validTrack(theTrack)){
      nAllTracks++;
      StPhysicalHelixD theHelix = trackGeometry(theTrack)->helix();
      slatHitVec = mTofGeom->tofHelixToArray(theHelix, validSlatIdVec);
      if (slatHitVec.size()>0 && mHisto) hTofpSlatHitVecSize->Fill(slatHitVec.size());

      for (size_t ii = 0; ii < slatHitVec.size(); ii++) {
	slatHitVec[ii].trackIdVec.push_back(iNode);
	allSlatsHitVec.push_back(slatHitVec[ii]);
	if (mHisto){
	  int id = mTofGeom->slatIdToDaq(slatHitVec[ii].slatIndex);
	  float xhit = slatHitVec[ii].hitPosition.x();
	  float yhit = slatHitVec[ii].hitPosition.y();
	  float zhit = slatHitVec[ii].hitPosition.z();
	  float phihit = atan2(yhit,xhit);
	  hTofpSlatIdB1->Fill(id);
	  hTofpHitMap1->Fill(zhit,phihit);
	}

	if (Debug()){ 
	  LOG_INFO << "B: trackid=";
	  idVectorIter ij = slatHitVec[ii].trackIdVec.begin();
	  while (ij != slatHitVec[ii].trackIdVec.end()) {LOG_INFO << " " << *ij; ij++;}
	  LOG_INFO << "\tind=" << mTofGeom->slatIdToDaq(slatHitVec[ii].slatIndex)
	       << "\thitprof="<< slatHitVec[ii].hitProfile 
	       << "\ts="<<slatHitVec[ii].s << "\tthxy="<<slatHitVec[ii].theta_xy 
	       << "\tthzr="<<slatHitVec[ii].theta_zr;
	  if (slatHitVec.size()>1) LOG_INFO << " M" << endm;
	  else LOG_INFO << endm;
	}
      }
    } // existing global track
  } // loop over nodes

  gMessMgr->Info("","OST") << "B: #matched/#avail/#total tracknodes: " 
			   <<allSlatsHitVec.size() << "/" <<nAllTracks 
			   << "/" << nodes.size() << endm;
  // end of Sect.B


  //.........................................................................
  // C Neighbours -- identify crosstalk, geometry shifts (only fill histograms)
  //
  // [ ... ]
  // end of Sect.C


  //.........................................................................
  // D. sort hit vectors  and deal with (discard) slats matched by multiple tracks
  //
  int nSingleHitSlats(0);
  tofSlatHitVector singleHitSlatsVec;
  StructSlatHit slatHit;

  tofSlatHitVector tempVec = allSlatsHitVec;
  tofSlatHitVector erasedVec = tempVec;
  while (tempVec.size() != 0) {
    int nTracks = 0;
    vector<StThreeVectorD> vPosition;
    vector<vector<StThreeVectorD> > vLayerHitPositions;
    vector<Int_t> vHitProfile;
    vector<Float_t> vS, vTheta_xy, vTheta_zr;
    idVector trackIdVec;

    tofSlatHitVectorIter tempIter=tempVec.begin();
    tofSlatHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->slatIndex == erasedIter->slatIndex) {
	nTracks++;
	// save all hit data in temporary vectors
	trackIdVec.push_back(erasedIter->trackIdVec.back());
	vPosition.push_back(erasedIter->hitPosition);
	vLayerHitPositions.push_back(erasedIter->layerHitPositions);
	vHitProfile.push_back(erasedIter->hitProfile);
	vS.push_back(erasedIter->s);
	vTheta_xy.push_back(erasedIter->theta_xy);
	vTheta_zr.push_back(erasedIter->theta_zr);

	if (mHisto){
	 float xhit = erasedIter->hitPosition.x();
	 float yhit = erasedIter->hitPosition.y();
	 float zhit = erasedIter->hitPosition.z();
	 float phihit = atan2(yhit,xhit);
	 hTofpHitMap2->Fill(zhit,phihit);
	}

	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    if (mHisto){
      int daqId = mTofGeom->slatIdToDaq(tempIter->slatIndex);
      hTofpSlatIdD1->Fill(daqId);
    }

    if (nTracks==1){
      nSingleHitSlats++;
      // for singly hit slat, copy data in singleHitSlatsVec
      slatHit.slatIndex = tempIter->slatIndex;
      slatHit.hitPosition = vPosition[0];
      slatHit.layerHitPositions = vLayerHitPositions[0];
      slatHit.trackIdVec = trackIdVec;
      slatHit.hitProfile =  vHitProfile[0];
      slatHit.s    =  vS[0];
      slatHit.theta_xy =  vTheta_xy[0];
      slatHit.theta_zr =  vTheta_zr[0];

      singleHitSlatsVec.push_back(slatHit);

      if (mHisto){
        int daqId = mTofGeom->slatIdToDaq(tempIter->slatIndex);
	float xhit = slatHit.hitPosition.x();
	float yhit = slatHit.hitPosition.y();
	float zhit = slatHit.hitPosition.z();
	float phihit = atan2(yhit,xhit);
	hTofpSlatIdD2->Fill(daqId);
	hTofpHitMap3->Fill(zhit,phihit);
      }

      // debugging output
      if (Debug()) {
	LOG_INFO << "D: ind=" << mTofGeom->slatIdToDaq(slatHit.slatIndex)
	     << "\thitprof="<< slatHit.hitProfile << "\ts="<<slatHit.s
	     << "\tthxy="<<slatHit.theta_xy << "\tthzr="<<slatHit.theta_zr << "\ttrackid:";
	idVectorIter ij=trackIdVec.begin();
	while (ij != trackIdVec.end()) { LOG_INFO << " " << *ij; ij++; }
	LOG_INFO <<endm;
      }
    }
    else if (nTracks>1){
      // for multiple hit slats either discard (yes) or
      // find the most likely candidate.
    } else
       gMessMgr->Warning("","OST")  << "D: no tracks extrapolate to matched slat ... should not happen!" << endm;

    tempVec = erasedVec;
  }
   gMessMgr->Info("","OST") << "D: #before/#after: " << allSlatsHitVec.size() 
			    << "/" << singleHitSlatsVec.size() << endm;
  //end of Sect.D



  //.........................................................................
  // E. sort and deal singleHitSlatsVector for multiple slats associated to single tracks
  //
  tofSlatHitVector allMatchedSlatsVec;
  tempVec = singleHitSlatsVec;
  erasedVec = tempVec;
  while (tempVec.size() != 0) {
    StructSlatHit slatHit;
    int nSlats = 0;
    vector<StThreeVectorD> vPosition;
    vector< vector<StThreeVectorD> > vLayerHitPositions;
    vector<Int_t> vHitProfile;
    vector<Float_t> vS, vTheta_xy, vTheta_zr;
    idVector vTrackId;
    vector<Int_t> slatIndex;

    tofSlatHitVectorIter tempIter=tempVec.begin();
    tofSlatHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->trackIdVec.back() == erasedIter->trackIdVec.back()) {
	nSlats++;
	// save all hit data in temporary vectors
	slatIndex.push_back(erasedIter->slatIndex);
	vTrackId.push_back(erasedIter->trackIdVec.back());
	vPosition.push_back(erasedIter->hitPosition);
	vLayerHitPositions.push_back(erasedIter->layerHitPositions);
	vHitProfile.push_back(erasedIter->hitProfile);
	vS.push_back(erasedIter->s);
	vTheta_xy.push_back(erasedIter->theta_xy);
	vTheta_zr.push_back(erasedIter->theta_zr);

	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    
    if (nSlats==1){
      // for singly hit slat, copy data in singleHitSlatsVec
      slatHit.slatIndex = slatIndex[0];
      slatHit.hitPosition = vPosition[0];
      slatHit.layerHitPositions = vLayerHitPositions[0];
      slatHit.trackIdVec.push_back(vTrackId[0]);
      slatHit.hitProfile =  vHitProfile[0];
      slatHit.s    =  vS[0];
      slatHit.theta_xy =  vTheta_xy[0];
      slatHit.theta_zr =  vTheta_zr[0];
      slatHit.matchFlag = 0; 

      allMatchedSlatsVec.push_back(slatHit);

      if (mHisto){
	int daqId = mTofGeom->slatIdToDaq(slatIndex[0]);
	hTofpSlatIdE1->Fill(daqId);
      }

      // debugging output
      if (Debug()) {
	LOG_INFO << "E: ind=" << mTofGeom->slatIdToDaq(slatHit.slatIndex)
	     << "\thitprof="<< slatHit.hitProfile << "\ts="<<slatHit.s
	     << "\tthxy="<<slatHit.theta_xy << "\tthzr="<<slatHit.theta_zr << "\ttrackid:";
	idVectorIter ij=vTrackId.begin();
	while (ij != vTrackId.end()) { LOG_INFO << " " << *ij; ij++; }
	LOG_INFO <<endm;
      }
    }
    else if (nSlats>1){   // for multiple hit slats  find the most likely candidate.
      int thiscandidate(-99);
      int thisMatchFlag(0);

      // 1. sort on hitprofile weight
      int weight(0);
      vector<int> weightCandidates;
      thisMatchFlag = 1;
      if (Debug()) LOG_INFO << "E: find ... weight ";
      for (int i=0;i<nSlats;i++){
	int hitWeight = vLayerHitPositions[i].size();
	if (Debug()) LOG_INFO << mTofGeom->slatIdToDaq(slatIndex[i]) << "("<<hitWeight<<")"<<" ";
	if (hitWeight>weight) {
	  weight=hitWeight;
	  weightCandidates.clear();
	  weightCandidates.push_back(i);
	} else if (hitWeight == weight)
	  weightCandidates.push_back(i);
      }
      if (weightCandidates.size()==1){
	thiscandidate = weightCandidates[0];
	int daqId = mTofGeom->slatIdToDaq(slatIndex[thiscandidate]);
	if (mHisto) hTofpSlatIdE2->Fill(daqId);
	if (Debug()) LOG_INFO << "candidate =" << daqId << endm;
      }

      // 2. if still undecided check on ss
      if (weightCandidates.size()>1){
	Float_t ss(0);
	vector<int> ssCandidates;
	thisMatchFlag = 2;
	if (Debug()) LOG_INFO << " ss ";
	for (unsigned int i=0;i<weightCandidates.size();i++){
	  int ii=weightCandidates[i];	  
	  if (Debug()) LOG_INFO << mTofGeom->slatIdToDaq(slatIndex[ii]) << " ";
	  if (vS[ii]>ss){
	    ss = vS[ii];
	    ssCandidates.clear();
	    ssCandidates.push_back(ii);
	  }else if  (vS[ii]==ss)
	    ssCandidates.push_back(ii);	  
	}
	if (ssCandidates.size()==1){
	  thiscandidate = ssCandidates[0];
	  int daqId = mTofGeom->slatIdToDaq(slatIndex[thiscandidate]);
	  if (mHisto) hTofpSlatIdE3->Fill(daqId);
    	  if (Debug()) LOG_INFO << "candidate =" << daqId << endm;
	}

	// 3. if still undecided go for closest/first hit
	if (ssCandidates.size()>1){
	  Int_t hitprof(0);
	  vector<int> profileCandidates;
	  thisMatchFlag = 3;
	  if (Debug()) LOG_INFO << " hprof ";
	  for (unsigned int i=0;i<ssCandidates.size();i++){
	    int ii=ssCandidates[i];
	    if (Debug()) LOG_INFO << mTofGeom->slatIdToDaq(slatIndex[ii]) << " ";
	    if (vHitProfile[ii]>hitprof){
	      hitprof = vHitProfile[ii];
	      profileCandidates.clear();
	      profileCandidates.push_back(ii);
	  }else if  (vHitProfile[ii]==hitprof)
	    profileCandidates.push_back(ii);	  
	  } 
	  if (profileCandidates.size()==1){
	    thiscandidate = profileCandidates[0];
	    int daqId = mTofGeom->slatIdToDaq(slatIndex[thiscandidate]);
	    if (mHisto) hTofpSlatIdE4->Fill(daqId);
	    if (Debug()) LOG_INFO << "candidate =" << daqId << endm;
	  }
	  else
	    if (Debug()) LOG_INFO << "none" << endm;
	}


	// forget it, and let user know of the non-decision
	if (thiscandidate == -99 && Debug()){
	  LOG_INFO << "E: ind=";
	  for (unsigned int ii=0;ii<slatIndex.size();ii++) 
	    LOG_INFO << mTofGeom->slatIdToDaq(slatIndex[ii]) << " ";
	  LOG_INFO << "\ttrkid:" << vTrackId[0] << " Unable to decide. "; 
	  LOG_INFO << "(hitprofs:";
	  for (unsigned int ii=0;ii<slatIndex.size();ii++) 
	    LOG_INFO << vHitProfile[ii] << " ";
	  LOG_INFO << " ss:";
	  for (unsigned int ii=0;ii<slatIndex.size();ii++) 
	    LOG_INFO << vS[ii] << " ";
	  LOG_INFO << ")" << endm;
	}

      }


      if (thiscandidate>=0){
	slatHit.slatIndex = slatIndex[thiscandidate];
	slatHit.hitPosition = vPosition[thiscandidate];
	slatHit.layerHitPositions = vLayerHitPositions[thiscandidate];
	//slatHit.trackIdVec.clear();
	slatHit.trackIdVec.push_back(vTrackId[thiscandidate]);
	slatHit.hitProfile =  vHitProfile[thiscandidate];
	slatHit.s    =  vS[thiscandidate];
	slatHit.theta_xy =  vTheta_xy[thiscandidate];
	slatHit.theta_zr =  vTheta_zr[thiscandidate];
	slatHit.matchFlag = thisMatchFlag;
	
	allMatchedSlatsVec.push_back(slatHit);

	if (mHisto){
	  int daqId = mTofGeom->slatIdToDaq(slatIndex[thiscandidate]);
	  hTofpSlatIdE5->Fill(daqId);
	}
	
	// debugging output
	if (Debug()) {
	  LOG_INFO << "E: ind=" << mTofGeom->slatIdToDaq(slatHit.slatIndex)
	       << "\thitprof="<< slatHit.hitProfile << "\ts="<<slatHit.s
	       << "\tthxy="<<slatHit.theta_xy << "\tthzr="<<slatHit.theta_zr << "\ttrackid:"
	       << vTrackId[thiscandidate] << endm;
	}
      }
    } else
       gMessMgr->Warning("","OS")  << "E: no slats belong to this track ... should not happen!" << endm;

    tempVec = erasedVec;
  }
  gMessMgr->Info("","OST") << "E: #before/#after: " << singleHitSlatsVec.size()
			    << "/" << allMatchedSlatsVec.size() << endm;
  // end of Sect.E
  

  // --------------------------------------------------------------------//
  // Note: section F is not required: the primary-track requirement will //
  //       be enforced lateron in the MC-RC track association.           //
  // --------------------------------------------------------------------//


  //.........................................................................
  // F. perform further selection and
  //    fill valid track histograms and SlatCollection
  //
  //StTofSlatCollection *mSlatCollection =  new StTofSlatCollection;
  int nValidSinglePrimHitSlats(0); //nValidSingleHitSlats(0)

  for (size_t ii=0; ii < allMatchedSlatsVec.size(); ii++){
    int daqId = mTofGeom->slatIdToDaq(allMatchedSlatsVec[ii].slatIndex);
    //int jj = daqId-1;

    if (allMatchedSlatsVec[ii].trackIdVec.size()!=1)
       gMessMgr->Warning("","OST") << "F: WHAT!?!  mult.matched slat in single slat list " << daqId << endm;

    // 1. fill valid single track AND valid tdc histograms
    //if (validTdc(mTofpTdc[jj])) nValidSingleHitSlats++;

    // get track-id from slat hit vector
    unsigned int trackNode = allMatchedSlatsVec[ii].trackIdVec[0];
    StTrack *theTrack = nodes[trackNode]->track(primary);

    // 2. continue only if the (primary) track exists
    if (validTofTrack(theTrack)){
      nValidSinglePrimHitSlats++;
      if (mHisto){
	float xhit = allMatchedSlatsVec[ii].hitPosition.x();
	float yhit = allMatchedSlatsVec[ii].hitPosition.y();
	float zhit = allMatchedSlatsVec[ii].hitPosition.z();
	float phihit = atan2(yhit,xhit);
	hTofpHitMap4->Fill(zhit,phihit);
	hTofpSlatIdF1->Fill(daqId);
      }
	  
      //--- store number of hits per track
      int nHitsPerTrack = theTrack->topologyMap().numberOfHits(kTpcId);
      //if (mHisto) hTofpNumberOfTrackHits->Fill(nHitsPerTrack);
	  
      // select the apropriate track geometry
      const StTrackGeometry *theTrackGeometry = trackGeometry(theTrack);

      //--- get momentum from track
      const StThreeVectorF momentum = theTrackGeometry->momentum();
      //if (mHisto) hTofpPtTrack->Fill(momentum.perp());
	    
      //--- calculate flight path
      double pathLength = tofPathLength(&event->primaryVertex()->position(), 
					&allMatchedSlatsVec[ii].hitPosition,
					theTrackGeometry->helix().curvature());
	    

      //--- calculate local hit position on slat based first, last and middle plane
      //    (middle plane is the average of first and last plane, which is mathematically
      //     the same as SlatHitVec.hitPosition ... )
      StThreeVectorD *pInnerLayer, *pOuterLayer;
      pInnerLayer =  &(*(allMatchedSlatsVec[ii].layerHitPositions.begin()));
      pOuterLayer =  &(*(allMatchedSlatsVec[ii].layerHitPositions.end() - 1));

      //--- dig out from the dedx and rich pid traits (only for Debug mode)
      float dedx(0.), cherang(0);
      int dedx_np(0), cherang_nph(0);
      if (Debug()){
	StSPtrVecTrackPidTraits& traits = theTrack->pidTraits();
	for (unsigned int it=0;it<traits.size();it++){
	  if (traits[it]->detector() == kTpcId){
	    StDedxPidTraits *pid = dynamic_cast<StDedxPidTraits*>(traits[it]);
	    if (pid && pid->method() ==kTruncatedMeanId){
	      dedx    = pid->mean();
	      dedx_np =  pid->numberOfPoints();
	    }
	  } else if  (traits[it]->detector() == kRichId){
	    StRichPidTraits *pid = dynamic_cast<StRichPidTraits*>(traits[it]);
	    if (pid){ 
	      StRichSpectra* pidinfo = pid->getRichSpectra();
	      if (pidinfo && pidinfo->getCherenkovPhotons()>2){
		cherang     = pidinfo->getCherenkovAngle();
		cherang_nph = pidinfo->getCherenkovPhotons();
	      }
	    }
	  }
	}
      }

      //--- calculate local hit position on slat based on average hitposition
      //float localHitPos = mTofGeom->slatHitPosition(&allMatchedSlatsVec[ii].hitPosition);

      // Fill TOF Slat Collection
      //StTofSlat *tofSlat = new StTofSlat(jj+1,(int)mTofpAdc[jj],(int)mTofpTdc[jj],theTrack);
      //StTofSlat *tofSlat = new StTofSlat(jj+1,(int)mTofpAdc[jj],(int)mTofpTdc[jj],theTrack,
      //			     localHitPos, allMatchedSlatsVec[ii].hitProfile,
      //			     allMatchedSlatsVec[ii].matchFlag);
      //tofSlat->setPosition(allMatchedSlatsVec[ii].hitPosition);
      //mSlatCollection->push_back(tofSlat);

      // dump debug data
      if (Debug()){
	LOG_INFO << "F: ind=" << mTofGeom->slatIdToDaq(allMatchedSlatsVec[ii].slatIndex) 
	     << "\ttrackid:";
	idVectorIter ij=allMatchedSlatsVec[ii].trackIdVec.begin();
	while (ij != allMatchedSlatsVec[ii].trackIdVec.end()) { LOG_INFO << " " << *ij; ij++; }
	LOG_INFO << "\tR=" << 1/(theTrackGeometry->helix().curvature())
	     << "\tpT=" << momentum.perp() << "\tp=" << momentum.mag()
	     << "\thits="<< nHitsPerTrack << "\ts="<< pathLength
	     << "\t#fitp=" <<theTrack->fitTraits().numberOfFitPoints(kTpcId)
	     << "\t#trkp=" <<theTrack->detectorInfo()->numberOfPoints(kTpcId)
	     << " \tdedx=" << dedx 
	     << " \tdca="<< theTrack->geometry()->helix().distance(event->primaryVertex()->position());
	if (cherang!=0) LOG_INFO  << " \trich="<< cherang << " (" << cherang_nph << ")";
	LOG_INFO << endm;
      }

    } // track exists 
  }


    
  // ---------------------------------------------
  // --- Stage II: FIND RC-MC pairs (TPC eff.) ---
  // ---------------------------------------------
   
  // ----
  // IIa. loop over mcTracks and check for associated rcTrack (TPC tracking eff.)
  // IIb. check matching results for rcTrack (TOFp matching eff.)
  // ----


  const StSPtrVecMcTrack& mcTracks = mEvent->primaryVertex()->daughters();
  if (Debug()) LOG_INFO << "mcTrack Loop: "<< mcTracks.size() << " entries" << endm;
  StMcTrack* mcTrack;
  const StTrack* rcTrack = 0;

  int nRecon(0), nMatch(0);

  //--- Loop over MC tracks
  for (StMcTrackConstIterator mcItr = mcTracks.begin(); mcItr  != mcTracks.end(); mcItr++) {
    mcTrack = *mcItr;
    if (!mcTrack) {
      gMessMgr->Warning() << "No McTrack?!? Should not happen!"<<endm;
      continue;
    }


    //-- Fill histograms with McTrack data 
    StThreeVectorF mcP = mcTrack->momentum();
    if      (mcTrack->particleDefinition()->pdgEncoding()== 11)   hMcElectron->Fill(mcP.perp(),mcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()== 211)  hMcPionPlus->Fill(mcP.perp(),mcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()==-211)  hMcPionMin->Fill(mcP.perp(),mcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()== 321)  hMcKaonPlus->Fill(mcP.perp(),mcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()==-321)  hMcKaonMin->Fill(mcP.perp(),mcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()== 2212) hMcProton->Fill(mcP.perp(),mcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()==-2212) hMcAntiProton->Fill(mcP.perp(),mcP.pseudoRapidity());
    else LOG_INFO << "mcTracks has wrong pdgEncoding: "<< mcTrack->particleDefinition()->pdgEncoding() << endm; 

    //-- get associated reconstructed rctrack
    rcTrack = partnerTrack(mcTrackMap,mcTrack);
    if (!rcTrack) continue;
    if (rcTrack->flag()<0) continue;

    //-- require primary track
    rcTrack = rcTrack->node()->track(primary);
    if (!rcTrack) continue;

    nRecon++;

    //-- Fill histograms for the TPC-reconstructed tracks
    //             (select the approriate trackgometry)

    //StThreeVectorF rcP = rcTrack->geometry()->momentum();
    StThreeVectorF rcP = trackGeometry(rcTrack)->momentum();

    if      (mcTrack->particleDefinition()->pdgEncoding()==   11) hRcElectron->Fill(rcP.perp(),rcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()==  211) hRcPionPlus->Fill(rcP.perp(),rcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()== -211) hRcPionMin->Fill(rcP.perp(),rcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()==  321) hRcKaonPlus->Fill(rcP.perp(),rcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()== -321) hRcKaonMin->Fill(rcP.perp(),rcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()== 2212) hRcProton->Fill(rcP.perp(),rcP.pseudoRapidity());
    else if (mcTrack->particleDefinition()->pdgEncoding()==-2212) hRcAntiProton->Fill(rcP.perp(),rcP.pseudoRapidity());
    else LOG_INFO << "cannot fill rc histo" << endm;
    float diff = (mcP.mag()-rcP.mag()) / mcP.mag();	      
    if (mcTrack->particleDefinition()->pdgEncoding()==-211) hMomResPtPion->Fill(mcP.perp(),diff);




    //-- Loop over matched slats from Stage 1 (allMatchedSlatsVec) and look for similar trackKey
    //

    if (Debug()) LOG_INFO  << "RC/MC track Id="<< rcTrack->key() << "/" << mcTrack->key() << endm;

    //    Note on trackId vs. StTrack->key():
    //      these are *not* the same. trackId (based on the StEvent->trackNodes
    //      array index) is used internally to quickly locate tracks in the trackNodes
    //      array. StTrack->key(), however, should *always* be used to identify tracks
    //      outside the scope of this local code.
    //      To get the proper Key() from the trackId use this:
    //      trackKey = StEvent->trackNodes[trackId]->track()->key();

    for (tofSlatHitVectorIter matchedSlat=allMatchedSlatsVec.begin();
	 matchedSlat != allMatchedSlatsVec.end(); matchedSlat++){

      //- All matches from Stage I should have one and only one associated track
      if (matchedSlat->trackIdVec.size()!=1){
	gMessMgr->Warning() << "Slat with " << matchedSlat->trackIdVec.size() 
			    << "track matches? Should not happen!" << endm;
	continue;
      }

      //- Map trackId to trackKey
      unsigned int thisTrackId = matchedSlat->trackIdVec[0];
      unsigned int thisKey = nodes[thisTrackId]->track(global)->key();

      if (Debug()) LOG_INFO << " match trackKey (trackId): " << thisKey 
			<< " (" << thisTrackId << ")";
      
      if (thisKey == rcTrack->key()){
	if (Debug()) LOG_INFO << "RC-MC-TOFp match";

	nMatch++;

	//- Fill histograms for the reconstructed and matched tracks
	if      (mcTrack->particleDefinition()->pdgEncoding()==   11) hMatchElectron->Fill(rcP.perp(),rcP.pseudoRapidity());
	else if (mcTrack->particleDefinition()->pdgEncoding()==  211) hMatchPionPlus->Fill(rcP.perp(),rcP.pseudoRapidity());
	else if (mcTrack->particleDefinition()->pdgEncoding()== -211) hMatchPionMin->Fill(rcP.perp(),rcP.pseudoRapidity());
	else if (mcTrack->particleDefinition()->pdgEncoding()==  321) hMatchKaonPlus->Fill(rcP.perp(),rcP.pseudoRapidity());
	else if (mcTrack->particleDefinition()->pdgEncoding()== -321) hMatchKaonMin->Fill(rcP.perp(),rcP.pseudoRapidity());
	else if (mcTrack->particleDefinition()->pdgEncoding()== 2212) hMatchProton->Fill(rcP.perp(),rcP.pseudoRapidity());
	else if (mcTrack->particleDefinition()->pdgEncoding()==-2212) hMatchAntiProton->Fill(rcP.perp(),rcP.pseudoRapidity());
	else LOG_INFO << "cannot fill match histo" << endm;
      }
      if (Debug()) LOG_INFO << endm;

    }



    // Alternative check ... reextrapolate the track and compare slatIds

//    // A. make sure the track makes it to the ctb
//    StPhysicalHelixD thisHelix = rcTrack->geometry()->helix();
//    //pairD pairL = thisHelix.pathLength(mTofGeom->tofParam().r + mTofGeom->tofParam().counter_thickness);
//    //if(fabs(pairL.first)>=500.0 && fabs(pairL.second)>=500.0) continue;
//	    
//    // B. only consider slat extrapolations (no Nslat or Ntrack/slat cuts)
//    tofSlatHitVector hitVec = mTofGeom->tofHelixToArray(thisHelix, validSlatIdVec);
//    if (hitVec.size()==0) continue;
//
//    {
//	LOG_INFO << "TOF HitVec ("<< hitVec.size() << ") slatIn reads:";
//	for (tofSlatHitVectorIter ii=hitVec.begin();ii!=hitVec.end();ii++)
//	  LOG_INFO << " " << ii->slatIndex;
//	LOG_INFO << endm;
//    }
//
//
//
//    // C  i. loop over AllHits and select tracks w/  one or more single hit slats
//    //   ii. select on hit profile
//    bool singleHit(false),profiledSingleHit(false);
//    tofSlatHitVectorIter thisHit=hitVec.begin();
//    while (thisHit!=hitVec.end()){
//	tofSlatHitVectorIter allHits = allMatchedSlatsVec.begin();
//	while (allHits!=allMatchedSlatsVec.end()){
//	  if (thisHit->slatIndex == allHits->slatIndex){
//	    LOG_INFO << "TOF found it in allHits "
//		 <<  allHits->trackIdVec.size() << endm;
//	    if (allHits->trackIdVec.size()==1){
//	      singleHit=true;
//	      if (thisHit->hitProfile == 31) profiledSingleHit=true;
//	    }
//	  }
//	  allHits++;
//	}
//	thisHit++;
//    }
//    if (!singleHit) {LOG_INFO << "TOF multiple hit"<<endm;continue;}
//    LOG_INFO << "TOF single hit"<< endm;
//    //if (!profiledSingleHit) continue;
//    //LOG_INFO << "TOF profiled single hit"<< endm;

          
  }
  gMessMgr->Info("","OST") << "Embedding: #mcTrack/#recon/#match: " <<  mcTracks.size()
			   <<"/"<< nRecon << "/" << nMatch << endm;

  return kStOK;
}


//---------------------------------------------------------------------------
bool StTofpMcAnalysisMaker::validTrack(StTrack *track){
  // 1. no track, no go.
  if (!track) return false;

  // 2. track quality flag, should be >0
  if (track->flag()<=0) return false;

  // 3. minimum #hits per track
  if (track->topologyMap().numberOfHits(kTpcId) < mMinHitsPerTrack) return false;

  // 4. rlast (not implemented, yet)

  return true;
}


bool StTofpMcAnalysisMaker::validTofTrack(StTrack *track){
  // select valid tracks for time-of-flight calculations

  // 1. track must exist
  if (!track) return false;

  // 2. track must be a primary track
  if (!dynamic_cast<StPrimaryTrack*>(track)) return false;

  // 3. DCA cut (obsolete?)
  double DCA= track->impactParameter();
  //int charge = track->geometry()->charge();
  //if (mHisto) hTofpDCATrackprimVertex->Fill(DCA*charge);
  double mMaxDCA = 999.;
  if (DCA > mMaxDCA) {
    gMessMgr->Info("","OST") << "dca>max:" << DCA<< endm;
    return false;
  }

  return true;
}  


const StTrackGeometry* StTofpMcAnalysisMaker::trackGeometry(const StTrack* track){
  // returns apropriate StTrackGeometry (standard or outerGeometry)
  if (!track) return 0;
  const StTrackGeometry *thisTrackGeometry;
  if (mOuterTrackGeometry)
    thisTrackGeometry = track->outerGeometry();
  else
    thisTrackGeometry = track->geometry();
  return thisTrackGeometry;
}

//--------------------------------------------------------------------------
/* $Log: StTofpMcAnalysisMaker.cxx,v $
 * Revision 1.5  2018/02/26 23:13:20  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.4  2011/04/03 15:52:57  fisyak
 * Fix effect of constness in StAssociationMaker
 *
 * Revision 1.3  2007/04/17 23:11:04  dongx
 * replaced with standard STAR Loggers
 *
 * Revision 1.2  2005/10/06 19:58:15  fisyak
 * Adjust for persistent StMcEvent
 *
 * Revision 1.1  2004/03/16 04:58:55  geurts
 * *** empty log message ***
 */

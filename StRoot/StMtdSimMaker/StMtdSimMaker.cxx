/***************************************************************************
 *
 * $Id: StMtdSimMaker.cxx,v 1.1 2011/10/11 16:28:11 perev Exp $
 *
 * Author: Frank Geurts
 ***************************************************************************
 *
 * Description: StMtdSimMaker class for Muon Telescope Simulations
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StMtdSimMaker.h"

// SCL
#include <math.h>
#include "TRandom.h"
#include "SystemOfUnits.h"
#include "phys_constants.h"
#include "StThreeVectorD.hh"
#include "Random.h"
#include "RanluxEngine.h"
#include "RandGauss.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TFile.h"

// g2t tables and collections
#include "tables/St_g2t_mtd_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "StMcTrack.hh"

#include "StEventTypes.h"
//fg #include "StEvent/StMtdCollection.h"

static RanluxEngine engine;
static RandGauss ranGauss(engine);

ClassImp(StMtdSimMaker)

	//_____________________________________________________________________________
StMtdSimMaker::StMtdSimMaker(const char *name):StMaker(name) {
  mBookHisto=kTRUE;
  mHistFile="mtdsim.root";
  mWriteStEvent=kTRUE;
  Reset();
}

//_____________________________________________________________________________
StMtdSimMaker::~StMtdSimMaker() {
  // do nothing
}


//_____________________________________________________________________________
Int_t StMtdSimMaker::Init() {  
  if(mBookHisto) bookHistograms();
  return StMaker::Init();
}

//_____________________________________________________________________________
void StMtdSimMaker::Reset() {
  mGeantData = 0;
  mEvent  = 0;
  mMcEvent = 0;
  //mBTofCollection = 0;
  if (mWriteStEvent) { delete mMtdCollection;mMtdCollection=0;}
  ResetFlags();
}

//_____________________________________________________________________________
Int_t StMtdSimMaker::ResetFlags() {
  /// MTD hit occupancy flag
  memset(mMtdHitFlag, 0, sizeof(mMtdHitFlag));
  return kStOk;
}


//_____________________________________________________________________________
Int_t StMtdSimMaker::InitRun(Int_t runnumber) {
  // do nothing
  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdSimMaker::FinishRun(Int_t runnumber) {
  // do nothing
  return kStOk;
}

//_____________________________________________________________________________
Int_t StMtdSimMaker::Finish() {
  if(mBookHisto){
    LOG_INFO << "StMtdSimMaker::Finish  writing mtdsim.root ...";
    TFile theFile(mHistFile.c_str(),"RECREATE","mtdsim");
    theFile.cd();
    writeHistograms();
    theFile.Close();
  }

  return kStOK;
}


//_____________________________________________________________________________
Int_t StMtdSimMaker::Make() 
{
  ResetFlags();
  mMcEvent = (StMcEvent*)GetInputDS("StMcEvent");
  if (!mMcEvent) {
      LOG_ERROR << "No StMcEvent! Bailing out ..." << endm;
      return kStWarn;
  }

  mMcMtdHitCollection = mMcEvent->mtdHitCollection();
  mMcMtdHitCollection->Clear();
  // Check to see that there are GEANT hits
  mGeantData = GetInputDS("geant"); // in bfc chain
  if(!mGeantData) { // when reading the geant.root file
    mGeantData = GetInputDS("geantBranch");
  }
  if(!mGeantData) {
    LOG_WARN << " No GEANT data loaded. Exit! " << endm;
    return kStWarn;
  }
  LOG_INFO << " Found GEANT data -- loading MTD hits... " << endm;

  // Look for MTD hits
  St_g2t_mtd_hit* g2t_mtd_hits = dynamic_cast<St_g2t_mtd_hit*>(mGeantData->Find("g2t_mtd_hit"));
  if(!g2t_mtd_hits){
    LOG_WARN << " No MTD hits in GEANT" << endm; }
  else {
    Int_t nmtdhits = g2t_mtd_hits->GetNRows();
    LOG_INFO << " Found MTD hits: " << nmtdhits << endm; 
    g2t_mtd_hit_st* mtdHitsFromGeant = g2t_mtd_hits->begin();
    for(Int_t i=0; i<nmtdhits; i++, mtdHitsFromGeant++) {
      LOG_INFO << " id:"<< mtdHitsFromGeant->id
	       << "  volume-id:" << mtdHitsFromGeant->volume_id
	       << " dE:" << mtdHitsFromGeant->de
               << " x:" << mtdHitsFromGeant->x[0] << ", y:" << mtdHitsFromGeant->x[1] << ", z:" << mtdHitsFromGeant->x[2]
	       << " p:" << mtdHitsFromGeant->p[0] << "," << mtdHitsFromGeant->p[1] << "," << mtdHitsFromGeant->p[2]
	       << " s:" << mtdHitsFromGeant->s_track
	       << " t:" << mtdHitsFromGeant->tof
	       << endm;

      // Decode GEANT volume_id and calculate cell# based on Ylocal
      vector<Int_t> cellId = CalcCellId(mtdHitsFromGeant->volume_id, mtdHitsFromGeant->x[1]);
      LOG_INFO << "   backleg:" << cellId[0] << " module:" << cellId[1] << " cell:" << cellId[2] << endm;
      FastCellResponse(mtdHitsFromGeant);
    }
}  

  LOG_INFO << " McMtdHit Size = " << mMcMtdHitCollection->hits().size() << endm;

  fillEvent();

  return kStOK;
}


//_____________________________________________________________________________
vector<Int_t> StMtdSimMaker::CalcCellId(Int_t volume_id, Float_t ylocal) {

  // Note: this routine should give vector<pair> and vector<Int_t> since each strip has double readout

  vector<Int_t> cellId;
  Int_t ires    = volume_id;
  
  // Decode GEANT volume_id
  Int_t ibackleg = Int_t(ires/100/100);
  ires = ires - ibackleg*100*100;
  Int_t imodule    = Int_t(ires/100);
  ires = ires - imodule*100;
  //fg  Int_t igap     = Int_t(ires);
  

  // Construct cell ID from local Y coordinate
  Int_t icell = Int_t((ylocal + mMtdPadWidth * mNCell/2) / mMtdPadWidth) + 1;

  // Verify ranges
  if(ibackleg<0 || ibackleg>mNBackleg) ibackleg = -1;
  if(imodule<0 || imodule>mNModule) imodule = -1;  
  if(icell<=0 || icell>mNCell) icell = -1;
  
  // Store results
  cellId.push_back(ibackleg);
  cellId.push_back(imodule);
  cellId.push_back(icell);
  
  return cellId;
}


//_____________________________________________________________________________
Int_t StMtdSimMaker::FastCellResponse(g2t_mtd_hit_st* mtdHitsFromGeant) {
  // Simulate the single cell response for a geant hit
  if(mtdHitsFromGeant->s_track<=0.0 || mtdHitsFromGeant->de <=0.0) {
    LOG_WARN << " No deposit energy in this MTD hit! " << endm;
    return kStWarn;
  }

  if(mBookHisto) {
    mDeGeant->Fill(mtdHitsFromGeant->de / keV);
    mTofGeant->Fill(mtdHitsFromGeant->tof / nanosecond);
  }

 vector<Int_t> cellId   = CalcCellId(mtdHitsFromGeant->volume_id, mtdHitsFromGeant->x[1]);

 Int_t icell, imodule, ibackleg;
  ibackleg   = cellId[0];
  imodule = cellId[1];
  icell   = cellId[2];
  if (ibackleg==-1 || imodule==-1 || icell==-1) {
    LOG_WARN << " Not hit the sensitive MRPC volume !!! " << endm;
    return kStWarn;
  }

  StThreeVectorF local(mtdHitsFromGeant->x[0], mtdHitsFromGeant->x[1], mtdHitsFromGeant->x[2]);

  St_g2t_track *g2t_track = static_cast<St_g2t_track *>(mGeantData->Find("g2t_track"));
  if (!g2t_track) {
    LOG_WARN << " No g2t track table !!! " << endm;
    return kStWarn;
  }
  g2t_track_st *mtd_track = g2t_track->GetTable();
  Int_t no_tracks= g2t_track->GetNRows();

  StMcTrack *partnerTrk = 0;
  Int_t partnerTrkId;
  for(Int_t j=0;j<no_tracks;j++){
    if(mtdHitsFromGeant->track_p==mtd_track[j].id){
      partnerTrk = new StMcTrack(&(mtd_track[j]));
      partnerTrkId=partnerTrk->key();
    }
  }

  /// X-talk
    Int_t icellx = -1;
    Float_t wt = 1.0;
    //fg    if(mCellXtalk)   CellXtalk(icell, local.y(), wt, icellx);

    //fg	Double_t tof= mtdHitsFromGeant->tof*1000./nanosecond + ranGauss.shoot()*mSimDb->timeres_tof()*1000./nanosecond;    //! 85ps per channel
    //fg temporarily fix MTD resolution at 99ps
	Double_t tof= mtdHitsFromGeant->tof*1000./nanosecond + ranGauss.shoot()*99*1000./nanosecond;
	Double_t t0 = mtdHitsFromGeant->tof*1000./nanosecond;
	Double_t de = mtdHitsFromGeant->de * wt;
	Double_t pathL = mtdHitsFromGeant->s_track;
	Double_t q = 0.;

	StMcMtdHit *mcMtdHit = new StMcMtdHit(ibackleg,imodule,icell,de,pathL,t0,tof,q);
	mcMtdHit->setPosition(local);
	mcMtdHit->setParentTrack(partnerTrk);
	storeMcMtdHit(mcMtdHit);

	mMtdHitFlag[ibackleg-1][(imodule-1)*mNCell+(icell-1)] = 1;

	if(icellx <= 0 || icellx > mNCell) return kStOk;  //! no X-talk
	///
	/// X talk signal
	///
        //fg	Double_t tofx = mtdHitsFromGeant->tof + ranGauss.shoot()*mSimDb->timeres_tof();    //! 85ps per channel
	Double_t tofx = mtdHitsFromGeant->tof + ranGauss.shoot()*99;
	Double_t dex = mtdHitsFromGeant->de * (1. - wt);
	Double_t qx = 0.*(1.-wt);

	StMcMtdHit *mcMtdHitx = new StMcMtdHit(ibackleg,imodule,icellx,dex,pathL,t0,tofx,qx);
	mcMtdHitx->setPosition(local);
	mcMtdHitx->setParentTrack(partnerTrk);
	mcMtdHitx->setParentTrackId(partnerTrkId);
	storeMcMtdHit(mcMtdHitx);

	mMtdHitFlag[ibackleg-1][(imodule-1)*mNCell+(icellx-1)] = 1;

	return kStOk;
}


//_____________________________________________________________________________
Int_t StMtdSimMaker::storeMcMtdHit(StMcMtdHit* mcMtdHit)
{
  //this function adds a hit to a previous hit (if they mactch the same cell location), 
  // or it stores the new hit (the last part below)
  Bool_t hitFound = kFALSE;

  int nHits = mMcMtdHitCollection->hits().size();
  for(int j=0;j<nHits;j++) { 
    StMcMtdHit *tempHit = mMcMtdHitCollection->hits()[j];
    if(!tempHit) continue;
    if(mcMtdHit->sameCell(*tempHit)) {
      hitFound = kTRUE;
      Float_t t1 = mcMtdHit->time(); 
      Float_t t2 = tempHit->time();   
      Float_t tof1 = mcMtdHit->tof();
      Float_t dE1 = mcMtdHit->dE();
      Float_t dE2 = tempHit->dE();
      Float_t s1 = mcMtdHit->pathLength();
      Float_t q1 = mcMtdHit->charge();
      Float_t q2 = tempHit->charge();
      StThreeVectorF x1 = mcMtdHit->position(); 
      StThreeVectorF x2 = tempHit->position();   
      StMcTrack *trk1 = mcMtdHit->parentTrack();
      if(t1>t2) {
	//do nothing
      } else {
	tempHit->setTime(t1); 
	tempHit->setTof(tof1);
	tempHit->setPathLength(s1);
	tempHit->setPosition(x1);
	tempHit->setParentTrack(trk1);
      }
      tempHit->setdE(dE1+dE2);  
      tempHit->setCharge(q1+q2);
      break;
    }
  }

  if(!hitFound) {
    mMcMtdHitCollection->addHit(mcMtdHit);
  } else {
    delete mcMtdHit;
  }
  return kStOk;
}


//___________________________________________________________________________
Int_t StMtdSimMaker::fillEvent()
{
  LOG_DEBUG << "Filling McEvent and Event"<<endm;

  // update histograms
  if(mBookHisto) {
    for(Int_t i=0;i<mNBackleg;i++) {
      Int_t ncell = 0;
      for(Int_t j=0;j<mNModule*mNCell;j++) { 
	if(mMtdHitFlag[i][j]) {
	  mCellGeant->Fill(j,i);
	  ncell++;
	}
      }
      mNCellGeant->Fill(ncell,i);
    }
  }
    
    /// send off to StEvent
      if (mWriteStEvent){
	mMtdCollection= new StMtdCollection();
	mEvent = (StEvent*)GetInputDS("StEvent");
	if (!mEvent) {
	  LOG_ERROR << "No StEvent! Bailing out ..." << endm;
	}
	
	/// creat StMtdHit / mtdRawData / mtdData collection
	  for(Int_t jj = 0; jj < (Int_t)mMcMtdHitCollection->hits().size(); jj++) {
	    StMcMtdHit *aMcMtdHit = mMcMtdHitCollection->hits()[jj];

	    if(!aMcMtdHit) continue;
//fg	    Int_t backlegid = aMcMtdHit->backleg();
//fg	    Int_t moduleid = aMcMtdHit->module();
//fg	    Int_t cellid = aMcMtdHit->cell();
//fg
//fg	    Int_t chn = 255;  // default
//fg	    if(trayid>0&&trayid<=120) {
//fg	      chn = mDaqMap->Cell2TDIGChan(moduleid,cellid);
//fg	    } else if(trayid==121) {
//fg	      chn = mDaqMap->WestPMT2TDIGLeChan(cellid);
//fg	    } else if(trayid==122) {
//fg	      chn = mDaqMap->EastPMT2TDIGLeChan(cellid);
//fg	    }

//fg	    Int_t index;
//fg	    if(trayid>0&&trayid<=120) { 
//fg	      index = (trayid-1)*mNTOF + (moduleid-1)*mNCell + (cellid-1);
//fg	    } else if(trayid==121||trayid==122) {
//fg	      index = (trayid-1)*mNTOF + (cellid-1);
//fg	    }


	    /// Efficiency
	    Float_t eff = 1.;
//fg	    if(trayid>0&&trayid<=120) eff = mSimDb->eff_tof(trayid, moduleid, cellid);
//fg	    else if(trayid==121||trayid==122) eff = mSimDb->eff_vpd(trayid, cellid);
//fg	    if (gRandom->Uniform(1.0) > eff){cout<<"REMOVED"<<endl; continue; } //! inefficiency


	    //Fill the StBTofHit
	    StMtdHit aMtdHit;
	    aMtdHit.Clear();

	    Float_t mcTof=aMcMtdHit->tof()/1000.;//from picoseconds to nanoseconds

	    aMtdHit.setBackleg((Int_t)aMcMtdHit->backleg());
	    aMtdHit.setModule((unsigned char)aMcMtdHit->module());
	    aMtdHit.setCell((Int_t)aMcMtdHit->cell());
	    aMtdHit.setLeadingEdgeTime(pair<double,double>(mcTof, mcTof));
	    aMtdHit.setTrailingEdgeTime(pair<double,double>(mcTof, mcTof));
	    aMtdHit.setAssociatedTrack(NULL);//done in StMtdMatchMaker
	    aMtdHit.setIdTruth(aMcMtdHit->parentTrackId(), 0);
	    mMtdCollection->addHit(new StMtdHit(aMtdHit));

	    //Fill the StMtdRawHit
	    StMtdRawHit aMtdRawHit;
	    aMtdRawHit.Clear();
	    aMtdRawHit.setBackleg((Int_t)aMcMtdHit->backleg());
	    aMtdRawHit.setChannel(12*(aMcMtdHit->module() - 1) + (Int_t)aMcMtdHit->cell());
	    aMtdRawHit.setFlag(1);
	    mMtdCollection->addRawHit(new StMtdRawHit(aMtdRawHit));

	  }

	  //Fill StMtdHeader -- 	
	  StMtdHeader aHead;
	  mMtdCollection->setHeader(new StMtdHeader(aHead));

	  //Store Collections
	  mEvent->setMtdCollection(mMtdCollection);

	  LOG_INFO << "... StMtdCollection Stored in StEvent! " << endm;
	}

	/// check StMcEvent and StEvent
//fg	if(Debug()) {
	  LOG_INFO << " ==== Test McMtdHitCollection ==== " << endm;
	  StSPtrVecMcMtdHit& mcMtdHits = mMcEvent->mtdHitCollection()->hits();
	  Int_t nCell[mNModule];
	  for(Int_t i=0;i<mNModule;i++) nCell[i] = 0;
	  for(Int_t i=0;i<(Int_t)mcMtdHits.size();i++) {
	    LOG_INFO << (*mcMtdHits[i]) << endm;

	    if(mBookHisto) {
	      Int_t ibackleg = mcMtdHits[i]->backleg();
	      Int_t imodule = mcMtdHits[i]->module();
	      Int_t icell = mcMtdHits[i]->cell();
	      Float_t t0 = mcMtdHits[i]->time();
	      Float_t tof = mcMtdHits[i]->tof();
	      Float_t de = mcMtdHits[i]->dE();

	      LOG_INFO << "backleg# "<<ibackleg << endm;

	      // fill MTD histograms
	      mCellSeen->Fill((imodule-1)*mNCell+(icell-1),ibackleg-1);
	      mDeSeen->Fill( de / keV );
	      mT0Seen->Fill( t0 /1000 );//ns
	      mTofSeen->Fill( tof / 1000 );//ns
	      mTofResSeen->Fill( (tof-t0) );//ps 
	      nCell[ibackleg-1]++;
	    }
	  }
	  if(mBookHisto) {
	    for(Int_t i=0;i<mNModule;i++) mNCellSeen->Fill(nCell[i],i);
	  }

	  LOG_INFO << " ==== Test MtdRawDataCollection ==== " << endm;
	  for(Int_t i=0;i<mNModule;i++) nCell[i] = 0;

	  if (mWriteStEvent){
	    StSPtrVecMtdHit& mtdHits=mEvent->mtdCollection()->mtdHits();
	    StMtdHit* aHit;
	    for(Int_t aa=0;aa<(int)mtdHits.size();aa++){
	      LOG_INFO << (*mtdHits[aa]) << endm;
	      aHit=mtdHits[aa];
	      Int_t ibackleg=aHit->backleg();
	      Int_t imodule=aHit->module();
	      Int_t icell=aHit->cell();
	      if(mBookHisto) {mCellReco->Fill((imodule-1)*mNCell+(icell-1),ibackleg-1);}
	    }
	  }
//fg	}

	if(Debug()) cout<<"leaving fill event"<<endl;

	return kStOk;
}


Int_t StMtdSimMaker::bookHistograms()
{
  //only done if Histogram setting is turned on
  mBetaHist=new TH1F("mBetaHist","mBetaHist", 100, -2, 2);
  mPathLHist=new TH1F("mPathLHist","mPathLHist", 100, -2, 500);//cm's
  mTofHist=new TH1F("mTofHist","mTofHist", 1000, -10, 10000);
  mRecMass=new TH1F("mRecMass","mRecMass", 1000, -2, 4);

  mCellGeant  = new TH2F("CellGeant","CellGeant",192,0.,192.,120,1.,120.);
  mNCellGeant = new TH2F("NCellGeant","NCellGeant",192,0.,192.,120,1.,120.);   
  mDeGeant    = new TH1F("DeGeant","DeGeant",1000,0.,10.);      //! 10 keV
  mTofGeant   = new TH1F("TofGeant","TofGeant",1000,0.,20.);    //! 20 ns

  mCellSeen   = new TH2F("CellSeen","CellSeen",192,0.,192.,120,1.,120.);
  mNCellSeen  = new TH2F("NCellSeen","NCellSeen",192,0.,192.,120,1.,120.);
  mDeSeen     = new TH1F("DeSeen","DeSeen",1000,0.,10.);        //! 10 kev
  mT0Seen    = new TH1F("T0Seen","T0Seen",1000,0.,20.);      //! ns
  mTofSeen    = new TH1F("TofSeen","TofSeen",1000,0.,20.);      //! 20 ns

  mTofResSeen = new TH1F("TofResSeen","TofResSeen",1001,-500.,500.);//! ps

  mCellReco   = new TH2F("CellReco","CellReco",192,0.,192.,120,1.,120.);
  mNCellReco  = new TH2F("NCellReco","NCellReco",192,0.,192.,120,1.,120.);
  mADCReco    = new TH1F("ADCReco","ADCReco",4096,0.,4096.);
  mTDCReco    = new TH1F("TDCReco","TDCReco",4096,0.,4096.);
  mT0Reco   = new TH1F("T0Reco","T0Reco",1000,0.,20.);      //! ns
  mTofResReco = new TH1F("TofResReco","TofResReco",1000,-300.,300.);//ps
  mTACorr     = new TH2F("TACorr","TACorr",512,0.,4096.,512,0.,4096.);

  mModHist     = new TH1F("ModuleHist","ModuleHist",201,-100,100);
  return kStOk;

}

//_____________________________________________________________________________
Int_t StMtdSimMaker::writeHistograms()
{
  //only done if Histogram setting is turned on

  mBetaHist->Write();
  mPathLHist->Write();
  mTofHist->Write();
  mRecMass->Write();

  mCellGeant->Write();
  mNCellGeant->Write();
  mDeGeant->Write();   
  mTofGeant->Write();

  mCellSeen->Write();
  mNCellSeen->Write();
  mDeSeen->Write();
  mT0Seen->Write();
  mTofSeen->Write();
  mTofResSeen->Write();

  mCellReco->Write();
  mNCellReco->Write();
  mADCReco->Write();
  mTDCReco->Write();
  mT0Reco->Write();
  mTofResReco->Write();
  mTACorr->Write();

  mModHist->Write();
  return kStOk;
}


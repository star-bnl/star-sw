/***************************************************************************
 *
 * $Id: StTofpNtupleMaker.cxx,v 1.8 2018/02/26 23:26:51 smirnovd Exp $
 *
 * Author: Frank Geurts
 ***************************************************************************
 *
 * Description: Maker fills TOFp Tree from StTofCollection
 *             
 **************************************************************************/
#include <iostream>
#include "StEventTypes.h"
#include "Stypes.h"
#include "StThreeVectorF.hh"
#include "StHelix.hh"
#include "StTrackGeometry.h"
#include "StEventUtilities/StuRefMult.hh"
#include "PhysicalConstants.h"
#include "StPhysicalHelixD.hh"
#include "StTofUtil/StTofGeometry.h"
#include "TNtuple.h"
#include "TFile.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"
#include "StTofUtil/tofPathLength.hh"
#include "StTofpNtupleMaker.h"



//---------------------------------------------------------------------------
/// constructor sets default parameters
StTofpNtupleMaker::StTofpNtupleMaker(const Char_t *name){
  mTupleFileName="tofntuple.root";
  setValidAdcRange(30,1200);
  setValidTdcRange(1,2047);
  doPrintMemoryInfo = kFALSE;
  doPrintCpuInfo    = kFALSE;
}

/// default empty destructor
StTofpNtupleMaker::~StTofpNtupleMaker(){ /* nope */}


//---------------------------------------------------------------------------
/// initialize ntuple and daqmap, and reset counters
Int_t StTofpNtupleMaker::Init(){

  if (mTupleFileName!="") bookNtuples();

  mTofGeom = new StTofGeometry();
  mTofGeom->initDaqMap();

  mAcceptedEvents = 0;
  mPvpdEntries = 0;
  mTofpEvents  = 0;
  mTofpEntries = 0;

  return kStOK;
}

/// write and close the ntuple file
Int_t StTofpNtupleMaker::Finish(){
  if (!(mTupleFileName=="")){
   mTupleFile->Write();
   mTupleFile->Close();
   LOG_INFO << "StTofpNtupleMaker::Finish() ntuple file " 
	<< mTupleFileName  << " closed." << endm;
  }

  LOG_INFO << "StTofpNtupleMaker -- statistics" << endm;
  LOG_INFO << " accepted events     : " << mAcceptedEvents << endm;
  LOG_INFO << " pVPD entries        : " << mPvpdEntries << endm;
  LOG_INFO << " TOFp entries/events : " << mTofpEntries << "/" << mTofpEvents << endm;
  return kStOK;
}


//---------------------------------------------------------------------------
/// get tofp slat, pvpd rawdata and global data from StEvent and store in flat TTrees (ntuples)
Int_t StTofpNtupleMaker::Make(){
  LOG_INFO << "StTofpNtupleMaker -- welcome" << endm;

  StEvent *event = (StEvent *) GetInputDS("StEvent");

  //.........................................................................
  // event selection ...
  if (!event ||
      !event->tofCollection() ||
      !event->tofCollection()->dataPresent() ||
      !event->primaryVertex()){
    LOG_INFO << "StTofpNtupleMaker -- nothing to do ... bye-bye"<< endm;
    return kStOK;
  }

  mAcceptedEvents++;

  StTimer timer;
  if (doPrintCpuInfo) timer.start();
  if (doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();

  // determine TOF configuration from run#
  mYear2= (event->runId()<4000000);
  mYear3= (event->runId()>4000000&&event->runId()<5000000);
  mYear4= (event->runId()>5000000);


  //.........................................................................
  // Collect global data for both ntuples

  //-- Primary vertex & trigger information
  float xvtx = event->primaryVertex()->position().x();
  float yvtx = event->primaryVertex()->position().y();
  float zvtx = event->primaryVertex()->position().z();
  StL0Trigger* pTrigger = event->l0Trigger();
  unsigned int triggerWord = 0;
  if (pTrigger) triggerWord = pTrigger->triggerWord();

  //-- Get the ZDC and CTB data (for pVPD ntuple)
  StTriggerDetectorCollection *theTriggers = event->triggerDetectorCollection();
  float zdcSumEast(-1.), zdcSumWest(-1), ctbSum(-1);
  if (theTriggers){
    StCtbTriggerDetector &theCtb = theTriggers->ctb();
    StZdcTriggerDetector &theZdc = theTriggers->zdc();
    zdcSumEast = theZdc.adcSum(east);
    zdcSumWest = theZdc.adcSum(west);
    ctbSum = 0;
    for (unsigned int islat=0; islat<theCtb.numberOfSlats(); islat++) 
      for (unsigned int itray=0; itray<theCtb.numberOfTrays(); itray++)
	ctbSum += theCtb.mips(itray, islat, 0);
  }

  //-- Number of primary tracks
  //  (note: different meaning in event.root and richtof.root)
  int refmult(0);
  bool richTofMuDST = (event->summary()->numberOfExoticTracks() == -999);
  if (richTofMuDST)
    refmult  = event->summary()->numberOfGoodTracks();
  else
    refmult = uncorrectedNumberOfPrimaries(*event);
  if (Debug()){
    LOG_INFO << " #Tracks           :" << event->summary()->numberOfTracks()
	 << "\n #goodPrimaryTracks:" << event->summary()->numberOfGoodPrimaryTracks()
	 << "\n #uncorr.prim.tracks  :" << refmult << endm;
    if (!richTofMuDST)
      LOG_INFO << " #goodTracks (global):" << event->summary()->numberOfGoodTracks() << endm;
  }

  //-- Check and fill local copy of TOFx+pVPD ADC and TDC data
  StTofCollection *theTof = event->tofCollection();
  getTofData(theTof);

  //-- Update TOFp slat counters
  float sumAdcTofp(0.); int nAdcTofp(0), nTdcTofp(0), nAdcTdcTofp(0);
  for (int i=0;i<NTOFP;i++){
    sumAdcTofp+=mTofpAdc[i];
    bool tdcValid=validTdc(mTofpTdc[i]);
    bool adcValid=validAdc(mTofpAdc[i]);
    if (tdcValid) nTdcTofp++;
    if (adcValid) nAdcTofp++;
    if (adcValid && tdcValid)  nAdcTdcTofp++;
  }
  if (Debug())
    LOG_INFO << " TOFp #Adc:" << nAdcTofp << "   #Tdc:" << nTdcTofp << endm;



  //.........................................................................
  // build pVPD ntuple
  if (!(mTupleFileName=="")){
    int k(0);
    float tuple[31];
    tuple[k++] = event->runId();    // the run number
    tuple[k++] = event->id();       // the event number
    tuple[k++] = triggerWord;
    tuple[k++] = event->summary()->magneticField();
    tuple[k++] = zvtx;              // z-vertex
    tuple[k++] = event->primaryVertex()->chiSquared(); // z-vertex chi2
    tuple[k++] = ctbSum;            // CTB sum
    tuple[k++] = zdcSumEast;        // ZDC sum east
    tuple[k++] = zdcSumWest;        // ZDC sum west
    tuple[k++] = refmult;           // STAR reference multiplicity  
    tuple[k++] = event->summary()->numberOfGoodPrimaryTracks();  // #primary tracks
    tuple[k++] = sumAdcTofp;        // TOFp sum
    tuple[k++] = nTdcTofp;          // TOFp hits
    for (int i=0;i<NPVPD;i++) tuple[k++] = mPvpdTdc[i];
    for (int i=0;i<NPVPD;i++) tuple[k++] = mPvpdAdc[i];
    for (int i=0;i<NPVPD;i++) tuple[k++] = mPvpdAdcLoRes[i];

    LOG_INFO << " pVPD update ..." << endm;
    mPvpdTuple->Fill(tuple);
    mPvpdEntries++;
  }



  //.........................................................................
  // build TOFp ntuple

  //-- make sure tofSlats are available
  if (event->tofCollection()->slatsPresent()){

    mTofpEvents++;
    int entriesThisEvent(0);

    //-- Loop over the slat container and retrieve the relevant parameters
    StSPtrVecTofSlat& slatTofVec = theTof->tofSlats();
    for (size_t i = 0; i < slatTofVec.size(); i++) {
      StTofSlat *thisSlat = slatTofVec[i];
      StTrack *thisTrack = thisSlat->associatedTrack();
      StTrackGeometry *theTrackGeometry = 
	(mOuterTrackGeometry)?thisTrack->outerGeometry():thisTrack->geometry();

      //- retrieve and recalculate parameters
      double pathLength = tofPathLength(&event->primaryVertex()->position(), 
					&thisSlat->position(),
					theTrackGeometry->helix().curvature());
      const StThreeVectorF momentum = theTrackGeometry->momentum(); 			      

      //- dig out from the dedx and rich pid traits
      float dedx(0.), cherang(0);
      int dedx_np(0), cherang_nph(0);
      StSPtrVecTrackPidTraits& traits = thisTrack->pidTraits();
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
				      
      //- TOFp Slat Ntuple entry for a single matched slat
      mSlatData.run      = event->runId();
      mSlatData.evt      = event->id();
      mSlatData.trgword  = triggerWord;
      mSlatData.magfield = event->summary()->magneticField();
      mSlatData.ctbsum   = ctbSum;
      mSlatData.zdcsum   = zdcSumEast + zdcSumWest;
      mSlatData.xvtx     = xvtx;
      mSlatData.yvtx     = yvtx;
      mSlatData.zvtx     = zvtx;
      mSlatData.zvtxchi2 = event->primaryVertex()->chiSquared();
      mSlatData.refmult  = refmult;
      mSlatData.nprimary = event->summary()->numberOfGoodPrimaryTracks();
      mSlatData.meanpt   = event->summary()->meanPt();
      mSlatData.te1 = (int)mPvpdTdc[0]; mSlatData.te2 = (int)mPvpdTdc[1];
      mSlatData.te3 = (int)mPvpdTdc[2]; mSlatData.tw1 = (int)mPvpdTdc[3];
      mSlatData.tw2 = (int)mPvpdTdc[4]; mSlatData.tw3 = (int)mPvpdTdc[5];
      if (mYear3||mYear4) {
	mSlatData.ae1 = (int)mPvpdAdcLoRes[0]; mSlatData.ae2 = (int)mPvpdAdcLoRes[1];
	mSlatData.ae3 = (int)mPvpdAdcLoRes[2]; mSlatData.aw1 = (int)mPvpdAdcLoRes[3];
	mSlatData.aw2 = (int)mPvpdAdcLoRes[4]; mSlatData.aw3 = (int)mPvpdAdcLoRes[5];
      } else {
	mSlatData.ae1 = (int)mPvpdAdc[0]; mSlatData.ae2 = (int)mPvpdAdc[1];
	mSlatData.ae3 = (int)mPvpdAdc[2]; mSlatData.aw1 = (int)mPvpdAdc[3];
	mSlatData.aw2 = (int)mPvpdAdc[4]; mSlatData.aw3 = (int)mPvpdAdc[5];
      }
      mSlatData.slat      = thisSlat->slatIndex();//jj+1;
      mSlatData.tdc	= thisSlat->tdc();//mTofpTdc[jj];
      mSlatData.adc	= thisSlat->adc();//(int)mTofpAdc[jj];
      mSlatData.hitprof   = thisSlat->hitProf(); //newerSlatHitVec[ii].hitProfile;
      mSlatData.matchflag = thisSlat->matchFlag(); //newerSlatHitVec[ii].matchFlag;
      mSlatData.zhit      = thisSlat->zHit(); //localHitPos;
      mSlatData.trackId     = (Int_t)thisTrack->key();
      mSlatData.ntrackpoints= thisTrack->detectorInfo()->numberOfPoints(kTpcId);
      mSlatData.nfitpoints  = thisTrack->fitTraits().numberOfFitPoints(kTpcId);
      mSlatData.r_last      = thisTrack->detectorInfo()->lastPoint().perp();
      mSlatData.chi2        = thisTrack->fitTraits().chi2(0);
  
      mSlatData.s           = fabs(pathLength);
      mSlatData.p           = momentum.mag()* theTrackGeometry->charge();
      mSlatData.pt	    = momentum.perp();
      mSlatData.px          = momentum.x();
      mSlatData.py          = momentum.y();
      mSlatData.pz          = momentum.z();
      mSlatData.eta         = momentum.pseudoRapidity();
      mSlatData.dedx        = dedx;
      mSlatData.dedx_np     = dedx_np;
      mSlatData.cherang     = cherang;
      mSlatData.cherang_nph = cherang_nph;
    
      mSlatTuple->Fill();
      mTofpEntries++;
      entriesThisEvent++;
    }

    LOG_INFO << " TOFp update: " << entriesThisEvent << " entries" <<endm;
  }


  //- debug info
  if (doPrintMemoryInfo) {
        StMemoryInfo::instance()->snapshot();
        StMemoryInfo::instance()->print();
  }
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time for StEventMaker::Make(): "
	 << timer.elapsedTime() << " sec\n" << endm;
  }

  LOG_INFO << "StTofpNtupleMaker -- bye-bye" << endm;
  return kStOK;
}



//---------------------------------------------------------------------------
/// create a local copy of the raw tofp data tofData in StEvent's tofCollection
Int_t StTofpNtupleMaker::getTofData(StTofCollection* tofCollection){
  if (!tofCollection) return kStERR;
  StSPtrVecTofData &tofData = tofCollection->tofData();

  // perform consistency check
  bool dataOK(true);
  if (Debug()) LOG_INFO << "TOF raw data consistency test ..."<< endm;
  for (int i=0;i<48;i++){
    if (tofData[i]->dataIndex()  != mTofGeom->daqToSlatId(i)) {
      dataOK = false;
      LOG_INFO << "getTofData===>WARNING: " << tofData[i]->dataIndex() << " " << mTofGeom->daqToSlatId(i) << endm;
    }
    //if (Debug()) LOG_INFO << *tofData[i];
  }
  
  for (int i=0;i<NTOFP;i++){
    mTofpAdc[i] = tofData[i]->adc();
    mTofpTdc[i] = tofData[i]->tdc();
  }

  if (mYear2){
    // swap ADC channels 3 and 4 ... this should move to the DAQreader!
    float tmp   = mTofpAdc[3];
    mTofpAdc[3] = mTofpAdc[2];
    mTofpAdc[2] = tmp;
  }

  for (int i=0;i<NPVPD;i++){
    mPvpdAdc[i] = tofData[42+i]->adc(); 
    mPvpdTdc[i] = tofData[42+i]->tdc(); 
    if (mYear3||mYear4)
      mPvpdAdcLoRes[i] = tofData[54+i]->adc();
  }

  if (!dataOK) return kStWarn;

  return kStOK;
}

//---------------------------------------------------------------------------
/// create and initialize ntuple and TTrees
void StTofpNtupleMaker::bookNtuples(){
  mTupleFile = new TFile(mTupleFileName.c_str(), "RECREATE");
  LOG_INFO << "StTofpNtupleMaker::bookNtuples()  file "
       << mTupleFileName << " opened" << endm;

  // pVPD timing
  string varList = "run:evt:trgwrd:magfield:zvtx:zvtxchi2:ctbsum"
                   ":zdceast:zdcwest:refmult:nprimary:tofpsum" 
                   ":ntofp:te1:te2:te3:tw1:tw2:tw3:ael1:ael2:ael3"
                   ":awl1:awl1:awl3:ae1:ae2:ae3:aw1:aw2:aw3";
  mPvpdTuple = new TNtuple("pvpd","tofp timing",varList.c_str());

  // TOFp calibration ntuple
  mSlatTuple = new TTree("tofp","TOFp slat data");
  mSlatTuple->Branch("run",&mSlatData.run,"run/I");
  mSlatTuple->Branch("evt",&mSlatData.evt,"evt/I");
  mSlatTuple->Branch("trgword",&mSlatData.trgword,"trgword/I");
  mSlatTuple->Branch("magfield",&mSlatData.magfield,"magfield/F");
  mSlatTuple->Branch("ctbsum",&mSlatData.ctbsum,"ctbsum/F");
  mSlatTuple->Branch("zdcsum",&mSlatData.zdcsum,"zdcsum/F");
  mSlatTuple->Branch("xvtx",&mSlatData.xvtx,"xvtx/F");
  mSlatTuple->Branch("yvtx",&mSlatData.yvtx,"yvtx/F");
  mSlatTuple->Branch("zvtx",&mSlatData.zvtx,"zvtx/F");
  mSlatTuple->Branch("zvtxchi2",&mSlatData.zvtxchi2,"zvtx/F");
  mSlatTuple->Branch("refmult",&mSlatData.refmult,"refmult/I");
  mSlatTuple->Branch("nprimary",&mSlatData.nprimary,"nprimary/I");
  mSlatTuple->Branch("meanpt",&mSlatData.meanpt,"meanpt/F");
  mSlatTuple->Branch("tdcstart",&mSlatData.tdcstart,"tdcstart/F");
  mSlatTuple->Branch("pvpd",&mSlatData.te1,"te1/I:te2:te3:tw1:tw2:tw3:ae1:ae2:ae3:aw1:aw2:aw3");
  mSlatTuple->Branch("slat",&mSlatData.slat,"slat/I:tdc:adc:hitprof:matchflag:zhit/F:zhitinner/F:zhitouter/F:ss:theta_xy:theta_zr");
  //mSlatTuple->Branch("track",&mSlatData.ntrackpoints,"ntrackpoints/I:nfitpoints:r_last/F:chi2:s:p:pt:px:py:pz:eta:dedx/F:dedx_np/I:cherangle/F:cherangle_nph/I");
  mSlatTuple->Branch("track",&mSlatData.trackId,"trackId/I:ntrackpoints/I:nfitpoints:r_last/F:chi2:s:p:pt:px:py:pz:eta:dedx/F:dedx_np/I:cherangle/F:cherangle_nph/I");


  // TOFp matching ntuple
  mMatchTuple = new TTree("match","TOFp match data");
  mMatchTuple->Branch("id",&mMatchData.daqid,"daqid/I");
  mMatchTuple->Branch("slat",&mMatchData.nneighbors,"nneighbors/I:zlocal/F:philocal/F:hitprof/I");
  mMatchTuple->Branch("track",&mMatchData.nfitpoints,"nfitpoints/I:ntrackpoints/I:maxpoints/I:r_last/F:p/F:pt/F");

  mNoMatchTuple = new TTree("nomatch","TOFp no-match data");
  mNoMatchTuple->Branch("id",&mMatchData.daqid,"daqid/I");
  mNoMatchTuple->Branch("slat",&mMatchData.nneighbors,"nneighbors/I:zlocal/F:philocal/F:hitprof/I");
  mNoMatchTuple->Branch("track",&mMatchData.nfitpoints,"nfitpoints/I:ntrackpoints/I:maxpoints/I:r_last/F:p/F:pt/F");

  return;
}


//---------------------------------------------------------------------------
/***************************************************************************
 *
 * $Log: StTofpNtupleMaker.cxx,v $
 * Revision 1.8  2018/02/26 23:26:51  smirnovd
 * StTof: Remove outdated ClassImp macro
 *
 * Revision 1.7  2018/02/26 23:13:20  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.6  2007/04/17 23:01:28  dongx
 * replaced with standard STAR Loggers
 *
 * Revision 1.5  2004/04/10 04:36:25  dongx
 * additional update for AdcLoRes in ntuple
 *
 * Revision 1.4  2004/04/09 19:26:25  dongx
 * Add some missing updates for year4, add AdcLoRes in ntuple
 *
 * Revision 1.3  2004/04/01 19:19:00  dongx
 * update for year4 run
 *
 * Revision 1.2  2003/12/04 06:54:08  geurts
 * introduced variables relevant to TOFp flow analysis
 *  * trackId, px and py
 *  * xvtx and yvtx
 *
 * Revision 1.1  2003/08/07 23:55:47  geurts
 * first release
 *
 */

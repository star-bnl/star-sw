/*******************************************************************
 *
 * $Id: StTofrNtupleMaker.cxx,v 1.1 2004/03/11 22:39:54 dongx Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: example maker to get the matched TOFr cells and fill
 *              into TOFr tree.
 *
 *****************************************************************
 *
 * $Log: StTofrNtupleMaker.cxx,v $
 * Revision 1.1  2004/03/11 22:39:54  dongx
 * first release
 *
 *
 *******************************************************************/
#include <iostream>
#include <math.h>
#include <vector>
#include <stdlib.h>
#include <stdio.h>
#include <algorithm>
#include <iterator>

#include "StEventTypes.h"
#include "Stypes.h"
#include "StThreeVectorF.hh"
#include "StMeasuredPoint.h"
#include "StDedxPidTraits.h"
#include "StTrackPidTraits.h"
#include "StarClassLibrary/StParticleTypes.hh"
#include "StarClassLibrary/StParticleDefinition.hh"
#include "StMuDSTMaker/COMMON/StMuUtilities.h"
#include "StHelix.hh"
#include "StTrackGeometry.h"
#include "StParticleTypes.hh"
#include "StTpcDedxPidAlgorithm.h"
#include "StHit.h"
#include "StAssociationMaker/StTrackPairInfo.hh"
#include "StEventUtilities/StuRefMult.hh"
#include "PhysicalConstants.h"
#include "StPhysicalHelixD.hh"
#include "TNtuple.h"
#include "TFile.h"
#include "StMemoryInfo.hh"
#include "StMessMgr.h"
#include "StTimer.hh"
#include "tables/St_pvpdStrobeDef_Table.h"

#include "StTofUtil/tofPathLength.hh"
#include "StTofUtil/StTofrGeometry.h"
#include "StTofUtil/StTofrDaqMap.h"
#include "StTofUtil/StTofCellCollection.h"
#include "StTofUtil/StTofHitCollection.h"
#include "StTofrNtupleMaker.h"


ClassImp(StTofrNtupleMaker)

//---------------------------------------------------------------------------
/// constructor sets default parameters
StTofrNtupleMaker::StTofrNtupleMaker(const Char_t *name){
  mTupleFileName="tofntuple.root";
  setValidAdcRange(1,1200);
  setValidTdcRange(1,2047);
  setOuterTrackGeometry();
  setInitGeomFromOther(kTRUE);
  
  doPrintMemoryInfo = kFALSE;
  doPrintCpuInfo    = kFALSE;
}

/// default empty destructor
StTofrNtupleMaker::~StTofrNtupleMaker(){ /* nope */}

//---------------------------------------------------------------------------
/// initialize ntuple and daqmap, and reset counters
Int_t StTofrNtupleMaker::Init(){

  if (mTupleFileName!="") bookNtuples();

  mAcceptedEvents = 0;
  mPvpdEntries = 0;
  mTofrEvents  = 0;
  mTofrEntries = 0;

  return kStOK;
}

Int_t StTofrNtupleMaker::InitRun(int runnumber) {

  if(mInitGeomFromOther) {
    TDataSet *geom = GetDataSet("tofrGeometry");
    mTofrGeom = (StTofrGeometry *)geom->GetObject();
  } else {
    mTofrGeom = new StTofrGeometry("tofrGeoNtuple","tofGeo in NtuplaMaker");
    if(!mTofrGeom->IsInitDone()) {
      gMessMgr->Info("TofrGemetry initialization..." ,"OS");
      TVolume *starHall = (TVolume *)GetDataSet("HALL");
      mTofrGeom->Init(starHall);
    } 
  }

  gMessMgr->Info("    -- retrieving run parameters from Calibrations_tof","OS");
  TDataSet *mDbDataSet = GetDataBase("Calibrations/tof");
  if (!mDbDataSet){
    gMessMgr->Error("unable to get TOF run parameters","OS");
    assert(mDbDataSet);
  }
  St_pvpdStrobeDef* pvpdStrobeDef = static_cast<St_pvpdStrobeDef*>(mDbDataSet->Find("pvpdStrobeDef"));
  if (!pvpdStrobeDef){
    gMessMgr->Error("unable to find TOF run param table","OS");
    assert(pvpdStrobeDef);
  }
  pvpdStrobeDef_st *strobeDef = static_cast<pvpdStrobeDef_st*>(pvpdStrobeDef->GetArray());
  int numRows = pvpdStrobeDef->GetNRows();
  if (mNPVPD != numRows) gMessMgr->Warning("#tubes inconsistency in dbase");
  for (int i=0;i<mNPVPD;i++){
    int ii = strobeDef[i].id - 1;
    mStrobeTdcMin[ii] = strobeDef[i].strobeTdcMin;
    mStrobeTdcMax[ii] = strobeDef[i].strobeTdcMax;
    if (Debug())
      cout << "tube " << strobeDef[i].id << "  min:"<< strobeDef[i].strobeTdcMin
	   <<" max:"<< strobeDef[i].strobeTdcMax<< endl;
  }
  
  return kStOK;
}

Int_t StTofrNtupleMaker::FinishRun(int runnumber)
{
  if(!mInitGeomFromOther) {
    if(mTofrGeom) delete mTofrGeom;
  }
  mTofrGeom = 0;
  
  return kStOK;
}

/// write and close the ntuple file
Int_t StTofrNtupleMaker::Finish() {

  if (!(mTupleFileName=="")){
    mTupleFile->Write();
    mTupleFile->Close();
    cout << "StTofrNtupleMaker::Finish() ntuple file " 
	 << mTupleFileName  << " closed." << endl;
  }
  
  cout << "StTofrNtupleMaker -- statistics" << endl;
  cout << " accepted events     : " << mAcceptedEvents << endl;
  cout << " pVPD entries        : " << mPvpdEntries << endl;
  cout << " Tofr entries/events : " << mTofrEntries << "/" << mTofrEvents << endl;
  return kStOK;
}


//---------------------------------------------------------------------------
/// get tofr slat, pvpd rawdata and global data from StEvent and store in flat TTrees (ntuples)
Int_t StTofrNtupleMaker::Make(){
  cout << "StTofrNtupleMaker -- welcome" << endl;

  StEvent *event = (StEvent *) GetInputDS("StEvent");

  //.........................................................................
  // event selection ...
  if (!event || !event->primaryVertex() ||
      !event->tofCollection() ||
      !event->tofCollection()->dataPresent()){
    cout << "StTofrNtupleMaker -- nothing to do ... bye-bye"<< endl;
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
    cout << " #Tracks           :" << event->summary()->numberOfTracks()
	 << "\n #goodPrimaryTracks:" << event->summary()->numberOfGoodPrimaryTracks()
	 << "\n #uncorr.prim.tracks  :" << refmult << endl;
    if (!richTofMuDST)
      cout << " #goodTracks (global):" << event->summary()->numberOfGoodTracks() << endl;
  }

  //-- read in pVPD info
  StTofCollection *theTof = event->tofCollection();
  StSPtrVecTofData &tofData = theTof->tofData();
  if(strobeEvent(tofData)) {
    gMessMgr->Info("strobe event","OTS");
    return kStOK;
  }

  for (int i=0;i<mNPVPD;i++){
    mPvpdAdc[i] = tofData[42+i]->adc(); 
    mPvpdTdc[i] = tofData[42+i]->tdc(); 
  }

  // east, west
  int Ieast = 0, Iwest=0;
  float TdcSum, TdcSumEast, TdcSumWest;
  for(int i=0;i<mNPVPD/2;i++) {
    if(validAdc(mPvpdAdc[i])&&validTdc(mPvpdTdc[i])) {
      Ieast++;
      TdcSumEast += mPvpdTdc[i];
      TdcSum += mPvpdTdc[i];
    }
    if(validAdc(mPvpdAdc[i+3])&&validTdc(mPvpdTdc[i+3])) {
      Iwest++;
      TdcSumWest += mPvpdTdc[i+3];
      TdcSum += mPvpdTdc[i+3];
    }
  }

  float TdcStart=(TdcSum*5.0e-02-(Ieast-Iwest)*zvtx/2.9979e+01)/(Ieast+Iwest)-7.4;
  float Tdiff = (TdcSumWest/Iwest-TdcSumEast/Ieast)/2.;

  //.........................................................................
  // build pVPD ntuple
  if (!(mTupleFileName=="")){
    int k(0);
    float tuple[30];
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
    tuple[k++] = TdcStart;          // tdc start
    tuple[k++] = Tdiff;             // time difference
    tuple[k++] = Ieast;             // 
    tuple[k++] = Iwest;
    tuple[k++] = TdcSumEast;
    tuple[k++] = TdcSumWest;
    tuple[k++] = TdcSum;
    for (int i=0;i<mNPVPD;i++) tuple[k++] = mPvpdTdc[i];
    for (int i=0;i<mNPVPD;i++) tuple[k++] = mPvpdAdc[i];

    cout << " pVPD update ..." << endl;
    mPvpdTuple->Fill(tuple);
    mPvpdEntries++;
  }



  //.........................................................................
  // build Tofr ntuple

  //-- make sure tofSlats are available
  if (event->tofCollection()->cellsPresent()){

    mTofrEvents++;
    int entriesThisEvent(0);

    //-- Loop over the cell container and retrieve the relevant parameters
    StSPtrVecTofCell& cellTofVec = theTof->tofCells();
    for (size_t i = 0; i < cellTofVec.size(); i++) {
      StTofCell *thisCell = cellTofVec[i];
      StTrack *thisTrack = thisCell->associatedTrack();
      StTrack *globalTrack = thisTrack->node()->track(global);
      StTrackGeometry *theTrackGeometry = 
	(mOuterTrackGeometry)?thisTrack->outerGeometry():thisTrack->geometry();


      const StThreeVectorF momentum = theTrackGeometry->momentum();

      //- dig out from the dedx and rich pid traits
      float dedx(0.), cherang(0), dedxerror(0);
      int dedx_np(0), cherang_nph(0);
      StSPtrVecTrackPidTraits& traits = thisTrack->pidTraits();
      for (unsigned int it=0;it<traits.size();it++){
	if (traits[it]->detector() == kTpcId){
	  StDedxPidTraits *pid = dynamic_cast<StDedxPidTraits*>(traits[it]);
	  if (pid && pid->method() ==kTruncatedMeanId){
	    dedx    = pid->mean();
	    dedx_np =  pid->numberOfPoints();
	    dedxerror = pid->errorOnMean();
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
				      
      int mNSigmaElectron;
      int mNSigmaPion;
      int mNSigmaKaon;
      int mNSigmaProton; 
      
      static StTpcDedxPidAlgorithm PidAlgorithm;
      static StElectron* Electron = StElectron::instance();
      static StPionPlus* Pion = StPionPlus::instance();
      static StKaonPlus* Kaon = StKaonPlus::instance();
      static StProton* Proton = StProton::instance();
      const StParticleDefinition* pd = thisTrack->pidTraits(PidAlgorithm);
      
      if (pd) {
	mNSigmaElectron = pack2Int( fabsMin(PidAlgorithm.numberOfSigma(Electron),__SIGMA_SCALE__),   __SIGMA_SCALE__ );
	mNSigmaPion =     pack2Int( fabsMin(PidAlgorithm.numberOfSigma(Pion),__SIGMA_SCALE__),    __SIGMA_SCALE__ );
	mNSigmaKaon =     pack2Int( fabsMin(PidAlgorithm.numberOfSigma(Kaon),__SIGMA_SCALE__),     __SIGMA_SCALE__ );
	mNSigmaProton =   pack2Int( fabsMin(PidAlgorithm.numberOfSigma(Proton),__SIGMA_SCALE__),   __SIGMA_SCALE__ );
      }
      
      //- Tofr Cell Ntuple entry for a single matched cell
      mCellData.run      = event->runId();
      mCellData.evt      = event->id();
      mCellData.trgword  = triggerWord;
      mCellData.magfield = event->summary()->magneticField();
      mCellData.ctbsum   = ctbSum;
      mCellData.zdcsum   = zdcSumEast + zdcSumWest;
      mCellData.xvtx     = xvtx;
      mCellData.yvtx     = yvtx;
      mCellData.zvtx     = zvtx;
      mCellData.zvtxchi2 = event->primaryVertex()->chiSquared();
      mCellData.refmult  = refmult;
      mCellData.nprimary = event->summary()->numberOfGoodPrimaryTracks();
      mCellData.meanpt   = event->summary()->meanPt();
      mCellData.TdcStart = TdcStart;
      mCellData.Tdiff    = Tdiff;
      mCellData.Ieast    = Ieast;
      mCellData.Iwest    = Iwest;
      mCellData.TdcSumEast = TdcSumEast;
      mCellData.TdcSumWest = TdcSumWest;
      mCellData.TdcSum     = TdcSum;
      mCellData.te1 = (int)mPvpdTdc[0]; mCellData.te2 = (int)mPvpdTdc[1];
      mCellData.te3 = (int)mPvpdTdc[2]; mCellData.tw1 = (int)mPvpdTdc[3];
      mCellData.tw2 = (int)mPvpdTdc[4]; mCellData.tw3 = (int)mPvpdTdc[5];
      //      if (mYear3 || mYear4) {
      //       	mCellData.ae1 = (int)mPvpdAdcLoRes[0]; mCellData.ae2 = (int)mPvpdAdcLoRes[1];
      //	mCellData.ae3 = (int)mPvpdAdcLoRes[2]; mCellData.aw1 = (int)mPvpdAdcLoRes[3];
      //	mCellData.aw2 = (int)mPvpdAdcLoRes[4]; mCellData.aw3 = (int)mPvpdAdcLoRes[5];
      //      } else {
	mCellData.ae1 = (int)mPvpdAdc[0]; mCellData.ae2 = (int)mPvpdAdc[1];
	mCellData.ae3 = (int)mPvpdAdc[2]; mCellData.aw1 = (int)mPvpdAdc[3];
	mCellData.aw2 = (int)mPvpdAdc[4]; mCellData.aw3 = (int)mPvpdAdc[5];
	//      }
      mCellData.tray    = thisCell->trayIndex();
      mCellData.module  = thisCell->moduleIndex();
      mCellData.cell    = thisCell->cellIndex();
      mCellData.daq     = thisCell->daqIndex();
      mCellData.tdc	= thisCell->tdc();//mTofrTdc[jj];
      mCellData.adc	= thisCell->adc();//(int)mTofrAdc[jj];
      mCellData.hitprof   = 0; //newerCellHitVec[ii].hitProfile;
      mCellData.matchflag = thisCell->matchFlag(); //newerCellHitVec[ii].matchFlag;

      //- retrieve and recalculate parameters
      //--- calculate flight path
      double pathLength = tofPathLength(&event->primaryVertex()->position(),
					&thisCell->position(),
					theTrackGeometry->helix().curvature());
      StThreeVectorD globalhit = thisCell->position();
      StTofrGeomSensor* sensor = mTofrGeom->GetGeomSensor(thisCell->moduleIndex(),thisCell->trayIndex());
      double local[3], global[3];
      global[0] = globalhit.x();
      global[1] = globalhit.y();
      global[2] = globalhit.z();
      sensor->Master2Local(&global[0], &local[0]);
      StThreeVectorD localhit(local[0], local[1], local[2]);      
      float ycenter = (thisCell->cellIndex()-1)*3.45-8.625;
      delete sensor;

      mCellData.xlocal    = (float)localhit.x();
      mCellData.ylocal    = (float)localhit.y();
      mCellData.zlocal    = (float)localhit.z();
      mCellData.deltay    = local[1] - ycenter;

      mCellData.trackId     = (Int_t)thisTrack->key();
      mCellData.charge      = theTrackGeometry->charge();
      mCellData.ntrackpoints= thisTrack->detectorInfo()->numberOfPoints(kTpcId);
      mCellData.nfitpoints  = thisTrack->fitTraits().numberOfFitPoints(kTpcId);
      mCellData.dca         = globalTrack->geometry()->helix().distance(event->primaryVertex()->position());
      mCellData.s           = (float)fabs(pathLength);
      mCellData.p           = momentum.mag();
      mCellData.pt	    = momentum.perp();
      mCellData.px          = momentum.x();
      mCellData.py          = momentum.y();
      mCellData.pz          = momentum.z();
      mCellData.eta         = momentum.pseudoRapidity();
      mCellData.dedx        = dedx;
      mCellData.dedxerror   = dedxerror;
      mCellData.dedx_np     = dedx_np;
      mCellData.cherang     = cherang;
      mCellData.cherang_nph = cherang_nph;
      mCellData.nSigE       = mNSigmaElectron;
      mCellData.nSigPi      = mNSigmaPion;
      mCellData.nSigK       = mNSigmaKaon;
      mCellData.nSigP       = mNSigmaProton;

      mCellTuple->Fill();
      mTofrEntries++;
      entriesThisEvent++;
    }

    cout << " Tofr update: " << entriesThisEvent << " entries" <<endl;
  }


  //- debug info
  if (doPrintMemoryInfo) {
        StMemoryInfo::instance()->snapshot();
        StMemoryInfo::instance()->print();
  }
  if (doPrintCpuInfo) {
    timer.stop();
    cout << "CPU time for StEventMaker::Make(): "
	 << timer.elapsedTime() << " sec\n" << endl;
  }

  cout << "StTofrNtupleMaker -- bye-bye" << endl;
  return kStOK;
}


//---------------------------------------------------------------------------
/// create and initialize ntuple and TTrees
void StTofrNtupleMaker::bookNtuples(){
  mTupleFile = new TFile(mTupleFileName.c_str(), "RECREATE");
  cout << "StTofrNtupleMaker::bookNtuples()  file "
       << mTupleFileName << " opened" << endl;

  // pVPD timing
  string varList = "run:evt:trgwrd:magfield:zvtx:zvtxchi2:ctbsum"
                   ":zdceast:zdcwest:refmult:nprimary"
                   ":TdcStart:Tdiff:Ieast:Iwest:TdcSumEast:TdcSumWest:TdcSum"
                   ":te1:te2:te3:tw1:tw2:tw3:ae1:ae2:ae3:aw1:aw2:aw3";
  mPvpdTuple = new TNtuple("pvpd","tofr timing",varList.c_str());

  // Tofr calibration ntuple
  mCellTuple = new TTree("tofr","Tofr cell data");
  mCellTuple->Branch("run",&mCellData.run,"run/I");
  mCellTuple->Branch("evt",&mCellData.evt,"evt/I");
  mCellTuple->Branch("trgword",&mCellData.trgword,"trgword/I");
  mCellTuple->Branch("magfield",&mCellData.magfield,"magfield/F");
  mCellTuple->Branch("ctbsum",&mCellData.ctbsum,"ctbsum/F");
  mCellTuple->Branch("zdcsum",&mCellData.zdcsum,"zdcsum/F");
  mCellTuple->Branch("xvtx",&mCellData.xvtx,"xvtx/F");
  mCellTuple->Branch("yvtx",&mCellData.yvtx,"yvtx/F");
  mCellTuple->Branch("zvtx",&mCellData.zvtx,"zvtx/F");
  mCellTuple->Branch("zvtxchi2",&mCellData.zvtxchi2,"zvtx/F");
  mCellTuple->Branch("refmult",&mCellData.refmult,"refmult/I");
  mCellTuple->Branch("nprimary",&mCellData.nprimary,"nprimary/I");
  mCellTuple->Branch("meanpt",&mCellData.meanpt,"meanpt/F");
  mCellTuple->Branch("pVPDall",&mCellData.TdcStart,"TdcStart/F:Tdiff/F:Ieast/I:Iwest/I:TdcSumEast/F:TdcSumWest/F:TdcSumEast/F:TdcSumWest/F:TdcSum/F");
  mCellTuple->Branch("pvpd",&mCellData.te1,"te1/I:te2:te3:tw1:tw2:tw3:ae1:ae2:ae3:aw1:aw2:aw3");
  mCellTuple->Branch("cell",&mCellData.tray,"tray/I:module/I:cell/I:daq/I:tdc/I:adc/I:hitprof/I:matchflag/I:xlocal/F:ylocal/F:zlocal/F:deltay/F");
  mCellTuple->Branch("track",&mCellData.trackId,"trackId/I:charge/I:ntrackpoints/I:nfitpoints/I:dca/F:s:p:pt:px:py:pz:eta:dedx/F:dedxerror/F:cherang/F:dedx_np/I:cherangle_nph/I:nSigE/I:nSigPi/I:nSigK/I:nSigP/I");

  return;
}

//---------------------------------------------------------------------------
// determine pVPD event type (strobe or beam)
bool StTofrNtupleMaker::strobeEvent(StSPtrVecTofData& tofData){
  // determine strobe event from pVPD TDC data

  int nStrobedPvpdTdcs=0;
  for(int i=0;i<mNPVPD;i++)
    if((tofData[42+i]->tdc()>mStrobeTdcMin[i]) &&
       (tofData[42+i]->tdc()<mStrobeTdcMax[i]))
  	nStrobedPvpdTdcs++;
  
  if (nStrobedPvpdTdcs==mNPVPD) return true;

  return false;
}

//---------------------------------------------------------------------------

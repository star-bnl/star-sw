/*******************************************************************
 *
 * $Id: StTofrMatchMaker.cxx,v 1.34 2018/02/26 23:26:52 smirnovd Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: Tofr Match Maker to do the matching between the 
 *              fired celles and TPC tracks ( similar to  Frank's
 *              TofpMatchMaker )
 *
 *******************************************************************/
#include <iostream>
#include "StEventTypes.h"
#include "Stypes.h"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"
#include "StHelix.hh"
#include "StTrackGeometry.h"
#include "StDcaGeometry.h"
#include "StDedxPidTraits.h"
#include "StTrackPidTraits.h"
#include "StarClassLibrary/StParticleTypes.hh"
#include "StarClassLibrary/StParticleDefinition.hh"
#include "StTpcDedxPidAlgorithm.h"
#include "StEventUtilities/StuRefMult.hh"
#include "PhysicalConstants.h"
#include "StPhysicalHelixD.hh"
#include "StHelix.hh"
#include "StTofUtil/tofPathLength.hh"
#include "StTofUtil/StTofrDaqMap.h"
#include "StTofUtil/StTofINLCorr.h"
#include "StTofUtil/StTofrGeometry.h"
#include "StTofUtil/StTofCellCollection.h"
#include "tables/St_pvpdStrobeDef_Table.h"
#include "tables/St_tofPedestal_Table.h"
#include "tables/St_tofConfig_Table.h"
#include "tables/St_tofTrayConfig_Table.h"
//#include "tables/St_tofr5INLtable_Table.h"
#include "StTofUtil/tofPathLength.hh"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "TTree.h"
#include "StMessMgr.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"
#include "StTofrMatchMaker.h"
//#include "TMemStat.h"


// Define the  static constants:

const Int_t StTofrMatchMaker::mDAQOVERFLOW = 255;
const Int_t StTofrMatchMaker::mNTOFP       = 41;
const Int_t StTofrMatchMaker::mNPVPD       = 6;
const Int_t StTofrMatchMaker::mNTOFR       = 120;
const Int_t StTofrMatchMaker::mNTOFR5      = 192;

const Int_t StTofrMatchMaker::mNTOF    = 192;    // 192 for tof in Run 8++
const Int_t StTofrMatchMaker::mNModule = 32;  // 32 for tofr5++ 
const Int_t StTofrMatchMaker::mNCell   = 6;
const Int_t StTofrMatchMaker::mNVPD    = 19;    //

const Int_t StTofrMatchMaker::mEastVpdTrayId = 122;
const Int_t StTofrMatchMaker::mWestVpdTrayId = 121;

const Int_t StTofrMatchMaker::mNValidTrays_Run3 = 1;
const Int_t StTofrMatchMaker::mNValidTrays_Run4 = 1;
const Int_t StTofrMatchMaker::mNValidTrays_Run5 = 1;
const Int_t StTofrMatchMaker::mNValidTrays_Run6 = 0;
const Int_t StTofrMatchMaker::mNValidTrays_Run7 = 0;
const Int_t StTofrMatchMaker::mNValidTrays_Run8 = 5;
    
const Int_t StTofrMatchMaker::mTdigBoard  = 10;
const Int_t StTofrMatchMaker::mTdcOnBoard = 4;
const Int_t StTofrMatchMaker::mTdcChannel = 1024;

//---------------------------------------------------------------------------
StTofrMatchMaker::StTofrMatchMaker(const Char_t *name): StMaker(name)
 , mTofrAdc(mNTOFR,0)
 , mTofrTdc(mNTOFR,0)
 , mPvpdAdc(mNPVPD,0)
 , mPvpdAdcLoRes( mNPVPD,0)
 , mPvpdTdc(mNPVPD,0)
 , mStrobeTdcMin(mNPVPD,0)
 , mStrobeTdcMax(mNPVPD,0)
 , mPedTOFr(mNTOFR,0)
    //year 5
 , mPvpdToT(mNPVPD,0)
 , mTofr5Tdc(mNTOFR5,0)
 , mTofr5ToT(mNTOFR5,0) // ToT as adc
 , mHitCorr(mNValidTrays_Run8,(TH2D*)0)
 , mHitCorrModule(mNValidTrays_Run8,(TH2D*)0)
{

  // set default values
  mEventCounter = 0;
  mAcceptedEventCounter = 0;
  mTofEventCounter = 0;
  mTofStrobeEventCounter = 0;
  mAcceptAndStrobe = 0;
  mAcceptAndBeam = 0;

  mTofrGeom = 0;
  mDaqMap = 0;
  mTofINLCorr = 0;

  mWidthPad = 3.45;

  setValidAdcRange(1,1200);
  setValidTdcRange(1,2047);
  setOuterTrackGeometry();
  setMinHitsPerTrack(15);
  setMinFitPointsPerTrack(15);
  setMinFitPointsOverMax(0.52);
  setMaxDCA(9999.);

  setCreateHistoFlag(kFALSE);
  setHistoFileName("tofana.root");
  setCreateTreeFlag(kFALSE);
  setSaveGeometry(kFALSE);
  doPrintMemoryInfo = kFALSE;
  doPrintCpuInfo    = kFALSE;
}

StTofrMatchMaker::~StTofrMatchMaker(){ /* nope */}

//void StTofrMatchMaker::Clear(Option_t *opt){StMaker::Clear();}

//---------------------------------------------------------------------------
Int_t StTofrMatchMaker::Init(){
  gMessMgr->Info("StTofrMatchMaker -- initializing ...","OS");
  LOG_INFO  << "Valid TDC range: " << mMinValidTdc << " " << mMaxValidTdc << endm;
  LOG_INFO  << "Valid ADC range: " << mMinValidAdc << " " << mMaxValidAdc << endm;
  LOG_INFO  << "Minimum hits per track: " << mMinHitsPerTrack << endm;
  LOG_INFO  << "Minimum fitpoints per track: " << mMinFitPointsPerTrack << endm;
  LOG_INFO  << "Maximum DCA: " << mMaxDCA << endm;
  if (!mOuterTrackGeometry)
    gMessMgr->Warning("Warning: using standard trackgeometry()","OS");
  
  // m_Mode can be set by SetMode() method
  if(m_Mode) {
//    setHistoFileName("tofana.root");
  } else {
    setHistoFileName("");
  }

  if (mHisto){
    bookHistograms();
    LOG_INFO  << "Histograms are booked" << endm;
    if (mHistoFileName!="")
      LOG_INFO  << "Histograms will be stored in " << mHistoFileName.c_str() << endm;
  }

  // reset event counters
  mEventCounter = 0;
  mAcceptedEventCounter = 0;
  mTofEventCounter = 0;
  mTofStrobeEventCounter = 0;
  mAcceptAndStrobe = 0;
  mAcceptAndBeam = 0;
  
  return kStOK;
}


//---------------------------------------------------------------------------
Int_t StTofrMatchMaker::InitRun(Int_t runnumber){

  // determine TOF configuration from run#
  mYear2 = (runnumber<4000000);
  mYear3 = (runnumber>4000000&&runnumber<5000000);
  mYear4 = (runnumber>5000000&&runnumber<6000000);
  mYear5 = (runnumber>6000000&&runnumber<7000000);
  mYear8 = (runnumber>9000000&&runnumber<10000000);
  mYearX = (runnumber>10000000);

  if (mYearX){
    Error(":InitRun","Wrong BFC configuration for run %d. Use StBTofHitMaker for Run9+ data.",runnumber);
    return 0;
  }

  gMessMgr->Info("StTofrMatchMaker -- Initializing TofGeometry (InitRun)","OS");
  /////////////////////////////////////////////////////////////////////
  // TOFr geometry initializtion -- from GEANT geometry directly
  //                               need St_geant_Maker be loaded before
  /////////////////////////////////////////////////////////////////////
  mTofrGeom = new StTofrGeometry("tofrGeom","tofrGeom in MatchMaker");
  if(!mTofrGeom->IsInitDone()) {
    gMessMgr->Info("TofrGemetry initialization..." ,"OS");
    TVolume *starHall = (TVolume *)GetDataSet("HALL");
    mTofrGeom->Init(starHall);
  }
  // other makers can get this geometry
  if(mGeometrySave) {
    if(TDataSet *geom = GetDataSet("tofrGeometry")) delete geom;
    AddConst(new TObjectSet("tofrGeometry",mTofrGeom)); 
  }

  if(Debug()) {
    gMessMgr->Info(" Test the TofrGeometry","OS");
    // Test
    TVolumeView* topNode = mTofrGeom->GetTopNode();
    if(!topNode) {
      gMessMgr->Warning("Warning:  no Top node!","OS");
      return kStWarn;
    }
    mTofrGeom->Print();
    
    topNode->ls(9);
    //  TShape *topshape = topNode->GetShape();
    LOG_INFO  << " # of trays = " << topNode->GetListSize() << endm;
    TList *list = topNode->Nodes();
    Int_t ibtoh =0;
    TVolumeView *sectorVolume = 0;
    for(Int_t i=0;i<list->GetSize();i++) {
      sectorVolume = dynamic_cast<TVolumeView*> (list->At(i));
      if ( i>=60 ) ibtoh = 1;
      LOG_INFO  << " test sector size = " <<sectorVolume->GetListSize() << endm;
      StTofrGeomTray *tray = new StTofrGeomTray(ibtoh, sectorVolume, topNode);
      tray->Print();
      TVolumeView *trayVolume = tray->GetfView();
      TList *list1 = trayVolume->Nodes();
      LOG_INFO  << "   # of modules in tray " << tray->Index() << " = " << trayVolume->GetListSize() << endm;
      if (!list1 ) continue;
      TVolumeView *sensorVolume = 0;
      for(Int_t j=0;j<list1->GetSize();j++) {
	sensorVolume = dynamic_cast<TVolumeView*> (list1->At(j));
	StTofrGeomSensor *sensor = new StTofrGeomSensor(sensorVolume, topNode);
	sensor->Print();
	delete sensor;  
      }
      delete tray;
    }
  }

  /////////////////////////////////////////////////////
  // TOFr Daq map initialization -- load from StTofUtil
  //////////////////////////////////////////////////////
  mDaqMap = new StTofrDaqMap();
  mTofINLCorr = new StTofINLCorr();
  if(mYear2||mYear3||mYear4) {
    if(mYear3) mDaqMap->setNValidTrays(mNValidTrays_Run3);
    if(mYear4) mDaqMap->setNValidTrays(mNValidTrays_Run4);
    mDaqMap->init(this);
    //  AddConst(new TObjectSet("tofrDaqMap",mDaqMap));
    LOG_INFO  << " Initialize Daq map for run 2,3,4 ... " << endm;
    
    if(Debug()) {
      for(Int_t i=0;i<mNTOFR;i++) {
	LOG_INFO  << i << " -- adc=" << mDaqMap->DaqChan2ADCChan(i) << " -- tdc=" << mDaqMap->DaqChan2TDCChan(i) << endm;
	IntVec map = mDaqMap->DaqChan2Cell(i);
	LOG_INFO  << "    tray=" << map[0] << "  module=" << map[1] << "  cell=" << map[2] << endm;
      }
    }
    
    ///////////////////////////////////////////////////////
      // Load configuration parameters from dbase
      //    need "[shell] setenv Calibrations_tof reconV0"
      ///////////////////////////////////////////////////////
      gMessMgr->Info("    -- retrieving run parameters from Calibrations_tof","OS");
      TDataSet *mDbDataSet = GetDataBase("Calibrations/tof/pvpdStrobeDef");
      if (!mDbDataSet){
	gMessMgr->Error("unable to get TOF pvpdStrobeDef table","OS");
	//    assert(mDbDataSet);
	return kStErr;
      }
      St_pvpdStrobeDef* pvpdStrobeDef = static_cast<St_pvpdStrobeDef*>(mDbDataSet->Find("pvpdStrobeDef"));
      if (!pvpdStrobeDef){
	gMessMgr->Error("unable to find pVPD strobe def parameters","OS");
	//    assert(pvpdStrobeDef);
	return kStErr;
      }
      pvpdStrobeDef_st *strobeDef = static_cast<pvpdStrobeDef_st*>(pvpdStrobeDef->GetArray());
      Int_t numRows = pvpdStrobeDef->GetNRows();
      if (mNPVPD != numRows) gMessMgr->Warning("#tubes inconsistency in dbase");
      for (Int_t i=0;i<mNPVPD;i++){
	Int_t ii = strobeDef[i].id - 1;
	mStrobeTdcMin[ii] = strobeDef[i].strobeTdcMin;
	mStrobeTdcMax[ii] = strobeDef[i].strobeTdcMax;
	if (Debug())
	  LOG_INFO  << "tube " << strobeDef[i].id << "  min:"<< strobeDef[i].strobeTdcMin
				  <<" max:"<< strobeDef[i].strobeTdcMax<< endm;
      }
      
      //TOFr pedestals
      mDbDataSet = GetDataBase("Calibrations/tof/tofPedestal");
      if (!mDbDataSet){
	gMessMgr->Error("unable to access TOF tofPedestal table","OS");
	//    assert(mDbDataSet);
	return kStErr;
      }
      St_tofPedestal* tofPed = static_cast<St_tofPedestal*>(mDbDataSet->Find("tofPedestal"));
      if (!tofPed){
	gMessMgr->Error("unable to find TOF pedestal table","OS");
	return kStErr;
	//    assert(tofPed);
      }
      tofPedestal_st *pedestal = static_cast<tofPedestal_st*>(tofPed->GetArray());
      Int_t entries = (Int_t)(pedestal[0].entries);
      if (mNTOFR>entries) gMessMgr->Warning("#TOFr channels inconsistency in dbase");
      for (Int_t i=0;i<entries;i++) {
	Int_t ii = pedestal[0].daqChannel[i];
	Int_t adcChan = pedestal[0].adcChan[i];
	if(adcChan>=60) 
	  mPedTOFr[ii] = (Double_t)(pedestal[0].adcPedestal[i]);
	if (Debug())
	  LOG_INFO  << "daqChannel" << ii << " ped:" << pedestal[0].adcPedestal[i] <<endm;
      }
      
      /*   only test
      //TOFr configuration
      St_tofTrayConfig* toftrayconfig = static_cast<St_tofTrayConfig*>(mDbDataSet->Find("tofTrayConfig"));
      if(!toftrayconfig) {
      gMessMgr->Error("unable to find TOF tray configuration parameters","OS");
      //    assert(toftrayconfig);
      return kStErr;    
      }
      tofTrayConfig_st *toftrayconf = static_cast<tofTrayConfig_st*>(toftrayconfig->GetArray());
      entries = toftrayconf[0].entries;
      for(Int_t i=0;i<entries;i++) {
      Int_t iTray = toftrayconf[0].iTray[i];
      Int_t nModules = toftrayconf[0].nModules[i];
      if(Debug())
      LOG_INFO  <<  "   " << iTray << " tray has " << nModules << " modules " << endm;
      }
      */
      
  } else if(mYear5) {
    mDaqMap->setNValidTrays(mNValidTrays_Run5);
    mDaqMap->initFromDbaseY5(this);
    LOG_INFO  << " Initialize Daq map for run 5 ... " << endm;
    
    if(Debug()) {
      //    if(0) {
      for(Int_t i=0;i<mNTOFR5;i++) {
	IntVec map = mDaqMap->Tofr5TDCChan2Cell(i);
	LOG_INFO  << "InitRun():i="<<i<<" tray=" << map[0] << "  module=" << map[1] << "  cell=" << map[2] << endm;
      }
    }
  } else if(mYear8) {
    mDaqMap->setNValidTrays(mNValidTrays_Run8);
    mDaqMap->initFromDbaseGeneral(this);
    LOG_INFO  << " Initialize Daq map for run 8 ... " << endm;

    mTofINLCorr->setNValidTrays(mNValidTrays_Run8);
    mTofINLCorr->initFromDbase(this);
    LOG_INFO  << " Initialize INL table for run 8 ... " << endm;

  }
  
  return kStOK;
}

//----------------------------------------------------------------------------
Int_t StTofrMatchMaker::FinishRun(Int_t runnumber){

  LOG_INFO  << "StTofrMatchMaker -- cleaning up geometry (FinishRun)" << endm;
  if (mTofrGeom) delete mTofrGeom;
  mTofrGeom=0;

  if(mDaqMap) delete mDaqMap;
  mDaqMap = 0;

  if(mTofINLCorr) delete mTofINLCorr;
  mTofINLCorr = 0;

  return kStOK;
}


//---------------------------------------------------------------------------
Int_t StTofrMatchMaker::Finish(){

  LOG_INFO  << "StTofrMatchMaker -----  RUN SUMMARY ----- (Finish)\n"
       << "\tProcessed "  << mEventCounter << " events."
       << " Accepted  "   << mAcceptedEventCounter << " events."
       << " Rejected  "   << mEventCounter - mAcceptedEventCounter << " events\n"
       << "\tTOF events " << mTofEventCounter
       << ". Beam "       << mTofEventCounter - mTofStrobeEventCounter
       << "  Strobe "     << mTofStrobeEventCounter
       << "\n\t Accept & Strobe " << mAcceptAndStrobe << " events\n"
       << "\t Accept & Beam   "   << mAcceptAndBeam   << " events" << endm;
  
  //if (mHisto) writeHistogramsToFile();
  if (mHistoFileName!="") writeHistogramsToFile();
  return kStOK;
}


//---------------------------------------------------------------------------
Int_t StTofrMatchMaker::Make(){
  gMessMgr->Info("StTofrMatchMaker -- welcome","OS");

  Int_t iret = kStOK;
  if(mYear2||mYear3||mYear4) {
    iret = processEventYear2to4();
  } else if(mYear5) {
    iret = processEventYear5();
  } else if(mYear8) {
    iret = processEventYear8();
  }
  return iret;
}

//---------------------------------------------------------------------------
Int_t StTofrMatchMaker::processEventYear2to4(){
  if(mHisto) mEventCounterHisto->Fill(0);
  // event selection ...
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (!validEvent(mEvent)){
    gMessMgr->Info("StTofrMatchMaker -- nothing to do ... bye-bye","OS");
    return kStOK;
  }

  if (mYear2) {
    gMessMgr->Info("StTofrMatchMaker -- no TOFr in Year2!","OS");
    return kStOK;
  }

  // timing & memory info -only when requested-
  StTimer timer;
  if (doPrintCpuInfo) timer.start();
  if (doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();

  //.........................................................................
  // check for tofCollection and fill local copy with ADC and TDC data
  StTofCollection *theTof = mEvent->tofCollection();
  getTofData(theTof);

  //.........................................................................
  // update Tofr cell counters
  Float_t sumAdcTofr(0.); Int_t nAdcTofr(0), nTdcTofr(0), nAdcTdcTofr(0);
  for (Int_t i=0;i<mNTOFR;i++){
    sumAdcTofr += mTofrAdc[i];
    bool tdcValid = validTdc(mTofrTdc[i]);
    bool adcValid = validAdc(mTofrAdc[i]);
    if (tdcValid) nTdcTofr++;
    if (adcValid) nAdcTofr++;
    if (adcValid && tdcValid) nAdcTdcTofr++;
  }
  if (Debug())
    LOG_INFO  << " Tofr #Adc:" << nAdcTofr << "   #Tdc:" << nTdcTofr << endm;

  //..................................
  // update pVPD tubes counters
  Float_t sumAdcPvpd=0; Int_t nAdcPvpd=0, nTdcPvpd=0;
  for (Int_t i=0;i<mNPVPD;i++){
    sumAdcPvpd += mPvpdAdc[i];
    bool tdcValid = validTdc(mPvpdTdc[i]);
    bool adcValid = validAdc(mPvpdAdc[i]);
    if (tdcValid) nTdcPvpd++;
    if (adcValid) nAdcPvpd++;
  }
  if (Debug())
    LOG_INFO  << " pVPD #Adc:" << nAdcPvpd << "   #Tdc:" << nTdcPvpd << endm;


    
  // number of primary tracks
  //  (note: different meaning in event.root and richtof.root)
  Int_t refmult(0);
  bool richTofMuDST = (mEvent->summary()->numberOfExoticTracks() == -999);
  if (richTofMuDST)
    refmult = mEvent->summary()->numberOfGoodTracks();
  else
    refmult = uncorrectedNumberOfPrimaries(*mEvent);

  if (Debug()){
    LOG_INFO  << " #Tracks           :"      << mEvent->summary()->numberOfTracks()
	 << "\n #goodPrimaryTracks:"    << mEvent->summary()->numberOfGoodPrimaryTracks()
	 << "\n #uncorr.prim.tracks  :" << refmult << endm;
    if (!richTofMuDST)
      LOG_INFO  << " #goodTracks (global):"  << mEvent->summary()->numberOfGoodTracks() << endm;
  }

  //.........................................................................
  // A. build vector of candidate cells with valid ADC signals 
  //  idVector validCellIdVec;
  tofCellHitVector daqCellsHitVec;
  //  daqCellsHitVec.clear();
  idVector validModuleVec;

  for (Int_t i=0;i<mNTOFR;i++){
    Float_t rawAdc = mTofrAdc[i];
    Float_t rawTdc = mTofrTdc[i];
    if (mHisto) mDaqOccupancy->Fill(i);

    if (validAdc(rawAdc) && validTdc(rawTdc) && rawAdc>mPedTOFr[i] ){
      IntVec map = mDaqMap->DaqChan2Cell(i);
      Int_t IDtray = map[0];
      Int_t IDmodule = map[1];
      Int_t IDcell = map[2];
      
      StructCellHit aDaqCellHit;
      aDaqCellHit.channel = i;
      aDaqCellHit.tray = IDtray;
      aDaqCellHit.module = IDmodule;
      aDaqCellHit.cell = IDcell;
      daqCellsHitVec.push_back(aDaqCellHit);
      // additional valid number configuration
      int id = IDtray*100+IDmodule;
      bool ifind = kFALSE;
      for(size_t iv=0;iv<validModuleVec.size();iv++) {
	if(id==validModuleVec[iv]) {
	  ifind = kTRUE;
	  break;
	}
      }
      if(!ifind) validModuleVec.push_back(id);

      if (mHisto) {
	mDaqOccupancyValid->Fill(i);
	Int_t adcchan = mDaqMap->DaqChan2ADCChan(i);
	Int_t tdcchan = mDaqMap->DaqChan2TDCChan(i);
	mADCTDCCorelation->Fill(tdcchan, adcchan);
      }
      if(Debug()) {
	LOG_INFO  <<"A: daqId=" << i << "  rawAdc= " << rawAdc << " rawTdc="<< rawTdc <<endm;
      }
    }
  }
  // end of Sect.A
  if(Debug()) {
    LOG_INFO  << "    total # of cells = " << daqCellsHitVec.size() << endm;
    for(size_t iv = 0;iv<validModuleVec.size();iv++) {
      LOG_INFO  << " module # " << validModuleVec[iv] << " Valid! " << endm;
    }
  }
  if(mHisto) {
    mCellsMultInEvent->Fill(daqCellsHitVec.size());
    if(daqCellsHitVec.size()) mEventCounterHisto->Fill(6);
  }
  if(!daqCellsHitVec.size()) return kStOK;

  //.........................................................................
  // B. loop over global tracks and determine all cell-track matches
  //
  tofCellHitVector allCellsHitVec;
  //  allCellsHitVec.clear();
  StructCellHit cellHit;

  StSPtrVecTrackNode& nodes = mEvent->trackNodes();
  Int_t nAllTracks=0;
  for (unsigned int iNode=0; iNode<nodes.size(); iNode++){
    tofCellHitVector cellHitVec;
    //    cellHitVec.clear();
    StTrack *theTrack = nodes[iNode]->track(global);

    // make sure we have a track, a miniDST might have removed it...
    if (validTrack(theTrack)){
      nAllTracks++;
      StPhysicalHelixD theHelix = trackGeometry(theTrack)->helix();

      IntVec projTrayVec;
      if(!mTofrGeom->projTrayVector(theHelix, projTrayVec)) continue;

      IntVec idVec;
      DoubleVec pathVec;
      PointVec  crossVec;

//       idVec.clear();
//       pathVec.clear();
//       crossVec.clear();

      Int_t ncells = 0;
      //      if(mTofrGeom->HelixCrossCellIds(theHelix,idVec,pathVec,crossVec) ) {
      if(mTofrGeom->HelixCrossCellIds(theHelix, validModuleVec, projTrayVec, idVec, pathVec, crossVec)) {
	Int_t cells = idVec.size();
	for (Int_t i=0; i<cells; i++) {
            Int_t icell,imodule,itray;
            Double_t local[3],global[3];
            for(Int_t i2=0;i2<3;i2++){
                 local[i2]=0;
            }
            global[0]=crossVec[i].x();
            global[1]=crossVec[i].y();
            global[2]=crossVec[i].z();
            mTofrGeom->DecodeCellId(idVec[i], icell, imodule, itray);
	    //	    LOG_INFO << " decode " << idVec[i] << "  to tray#" << itray << " module#" << imodule << " cell#" << icell << endm;
	    StTofrGeomSensor* sensor = 
                  mTofrGeom->GetGeomSensor(imodule,itray);
	    if(!sensor) {
	      gMessMgr->Warning("","OS") << " No sensitive module in the projection??? -- Something weird!!! " << endm;
	      continue;
	    }
            sensor->Master2Local(&global[0],&local[0]);
            icell = sensor->FindCellIndex(local);
	    //	    StThreeVectorD glo=sensor->GetCenterPosition();
	    StThreeVectorD glo(global[0], global[1], global[2]);
	    StThreeVectorD hitPos(local[0], local[1], local[2]);
	    delete sensor;
	    if (local[2]<=3.4&&local[2]>=-2.7) {
	      Int_t Iarray = mDaqMap->Cell2DaqChan(itray, imodule, icell);
	      if(Iarray>=mDAQOVERFLOW) continue;
	      ncells++;
	      cellHit.channel = Iarray;
	      cellHit.tray = itray;
	      cellHit.module = imodule;
	      cellHit.cell = icell;
	      cellHit.trackIdVec.push_back(iNode);
	      cellHit.hitPosition = glo;        // global position
	      cellHit.zhit = (Float_t)hitPos.z();
	      cellHit.yhit = (Float_t)hitPos.y();
	      cellHitVec.push_back(cellHit);
	      allCellsHitVec.push_back(cellHit);
	      if(mHisto) {
		mDaqOccupancyProj->Fill(Iarray);
		mHitsPosition->Fill(hitPos.y(), hitPos.z());
	      }
	      
	      if(Debug()) {
		LOG_INFO  <<"B: nodeid=" << iNode << "  projected in " << " tray="<< itray << " module="<<imodule<<" cell="<<icell<<endm;
		LOG_INFO  <<"   hit position " << hitPos << endm;
	      }
	    }
	} // for (Int_t i=0...)
      } // endif(helixcross...)
      if(ncells>0&&mHisto) mHitsMultPerTrack->Fill(ncells);

    } // if(ValidTrack).. 
  } // loop over nodes
  if(Debug())
    LOG_INFO  << "B:  matched/available/total #tracknodes: " <<allCellsHitVec.size() << "/" <<nAllTracks << "/" << nodes.size() << endm;
  if(mHisto) {
    mHitsMultInEvent->Fill(allCellsHitVec.size());
    if(allCellsHitVec.size()) mEventCounterHisto->Fill(7);
  }
  // end of Sect.B
  
  //.........................................................................
  // C. Match find Neighbours -- identify crosstalk
  //
  tofCellHitVector matchHitCellsVec;
  //  matchHitCellsVec.clear();

  tofCellHitVectorIter daqIter = daqCellsHitVec.begin();
  for(unsigned int idaq=0;idaq<daqCellsHitVec.size();idaq++, daqIter++) {
    tofCellHitVectorIter proIter = allCellsHitVec.begin();
    for(unsigned int ipro=0;ipro<allCellsHitVec.size();ipro++, proIter++) {
      if( (daqIter->tray==proIter->tray)&& 
	  (daqIter->module==proIter->module) &&
	  ( ( (proIter->cell==6)&&((proIter->cell==daqIter->cell) ||
				   (proIter->cell==daqIter->cell+1)) )
	    || ( (proIter->cell==1)&&((proIter->cell==daqIter->cell) ||
				      (proIter->cell==daqIter->cell-1)) )
	    || ( (proIter->cell>=2&&proIter->cell<=6) &&
		 ( (proIter->cell==daqIter->cell) ||
		   (proIter->cell==daqIter->cell-1) ||
		   (proIter->cell==daqIter->cell+1) ) ) ) ) {
	cellHit.channel = daqIter->channel;
	cellHit.tray = daqIter->tray;
	cellHit.module = daqIter->module;
	cellHit.cell = daqIter->cell;
	cellHit.hitPosition = proIter->hitPosition;
	cellHit.trackIdVec = proIter->trackIdVec;
	cellHit.zhit = proIter->zhit;
	cellHit.yhit = proIter->yhit;
	matchHitCellsVec.push_back(cellHit);
      }
    }
  } //end {sec. C}
  if(Debug()) {
    LOG_INFO  << "C: before/after: " << allCellsHitVec.size() << "/" << matchHitCellsVec.size() << endm;
  }
  if(mHisto&&matchHitCellsVec.size()) mEventCounterHisto->Fill(8);

  //.........................................................................
  // D. sort hit vectors  and deal with (discard) cells matched by multiple tracks
  //
  Int_t nSingleHitCells(0);
  Int_t nMultiHitsCells(0);

  tofCellHitVector singleHitCellsVec;
  tofCellHitVector multiHitsCellsVec;
//   singleHitCellsVec.clear();
//   multiHitsCellsVec.clear();

  tofCellHitVector tempVec = matchHitCellsVec;
  tofCellHitVector erasedVec = tempVec;
  while (tempVec.size() != 0) {
    Int_t nTracks = 0;
    idVector trackIdVec;

    tofCellHitVectorIter tempIter=tempVec.begin();
    tofCellHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->tray == erasedIter->tray &&
	 tempIter->module == erasedIter->module &&
	 tempIter->cell == erasedIter->cell) {
	nTracks++;
	trackIdVec.push_back(erasedIter->trackIdVec.back());  // merge
	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    cellHit.channel = tempIter->channel;
    cellHit.cell = tempIter->cell;
    cellHit.module = tempIter->module;
    cellHit.tray = tempIter->tray;
    cellHit.hitPosition = tempIter->hitPosition;
    cellHit.trackIdVec = trackIdVec;
    cellHit.zhit = tempIter->zhit;
    cellHit.yhit = tempIter->yhit;

    Float_t ycenter = (tempIter->cell-1-2.5)*mWidthPad;
    Float_t dy = tempIter->yhit - ycenter;
    Float_t dz = tempIter->zhit;

    if(mHisto) {
      mTracksPerCellMatch1->Fill(trackIdVec.size());
      mDaqOccupancyMatch1->Fill(tempIter->channel);
      mDeltaHitMatch1->Fill(dy, dz);
    }

    if (nTracks==1){
      nSingleHitCells++;      
      singleHitCellsVec.push_back(cellHit);
    } else if (nTracks>1){
      nMultiHitsCells++;
      multiHitsCellsVec.push_back(cellHit);
      // for multiple hit cells either discard (yes) or
      // find the most likely candidate.
    } else {
      LOG_INFO  << "D: no tracks extrapolate to matched cell ... should not happen!" << endm;
    }
    
    if (Debug()) {
      LOG_INFO  << "D: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
      idVectorIter ij=trackIdVec.begin();
      while (ij != trackIdVec.end()) { LOG_INFO  << " " << *ij; ij++; }
      LOG_INFO  <<endm;
    }
    
    tempVec = erasedVec;
  }
  if(Debug())
    LOG_INFO  << "D: before/after: " << matchHitCellsVec.size() << "/" << singleHitCellsVec.size() << endm;
  //end of Sect.C
  if(mHisto) {
    mCellsPerEventMatch1->Fill(singleHitCellsVec.size()+multiHitsCellsVec.size());
    if(singleHitCellsVec.size()) mEventCounterHisto->Fill(9);
  } 

  //.........................................................................
  // E. sort and deal singleHitCellsVector for multiple cells associated to single tracks
  //
  tofCellHitVector FinalMatchedCellsVec;
  //  FinalMatchedCellsVec.clear();
  tempVec = singleHitCellsVec;
  if(mHisto) {
    mCellsPerEventMatch2->Fill(tempVec.size());
    for(unsigned int ii=0;ii<tempVec.size();ii++) {
      mTracksPerCellMatch2->Fill(tempVec[ii].trackIdVec.size());
      mDaqOccupancyMatch2->Fill(tempVec[ii].channel);
      Float_t ycenter = (tempVec[ii].cell-1-2.5)*mWidthPad;
      Float_t dy = tempVec[ii].yhit-ycenter;
      Float_t dz = tempVec[ii].zhit;
      mDeltaHitMatch2->Fill(dy, dz);
    }
  }

  erasedVec = tempVec;
  while (tempVec.size() != 0) {
    StructCellHit cellHit;
    Int_t nCells = 0;
    idVector vTrackId;
    vector<StThreeVectorD> vPosition;
    vector<Int_t> vchannel, vtray, vmodule, vcell;
    vector<Float_t> vzhit, vyhit;

    tofCellHitVectorIter tempIter=tempVec.begin();
    tofCellHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->trackIdVec.back() == erasedIter->trackIdVec.back()) {
	nCells++;
	vchannel.push_back(erasedIter->channel);
	vtray.push_back(erasedIter->tray);
	vmodule.push_back(erasedIter->module);
	vcell.push_back(erasedIter->cell);
	vPosition.push_back(erasedIter->hitPosition);
	vTrackId.push_back(erasedIter->trackIdVec.back());
	vzhit.push_back(erasedIter->zhit);
	vyhit.push_back(erasedIter->yhit);

	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    
    if (nCells==1){
      // for singly hit cell, copy data in singleHitCellsVec
      cellHit.channel = vchannel[0];
      cellHit.tray = vtray[0];
      cellHit.module = vmodule[0];
      cellHit.cell = vcell[0];
      cellHit.trackIdVec.push_back(vTrackId[0]);
      cellHit.hitPosition = vPosition[0];
      cellHit.matchFlag = 0; 
      cellHit.zhit = vzhit[0];
      cellHit.yhit = vyhit[0];

      FinalMatchedCellsVec.push_back(cellHit);

      // debugging output
      if (Debug()) {
	LOG_INFO  << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
	idVectorIter ij=vTrackId.begin();
	while (ij != vTrackId.end()) { LOG_INFO  << " " << *ij; ij++; }
	LOG_INFO  <<endm;
      }
    }
    else if (nCells>1){   // for multiple hit cells  find the most likely candidate.
      Int_t thiscandidate(-99);
      Int_t thisMatchFlag(0);

      // 1. sort on adc weight
      Float_t weight(0);
      vector<Int_t> weightCandidates;
      thisMatchFlag = 1;
      if (Debug()) LOG_INFO  << "E: find ... weight ";
      for (Int_t i=0;i<nCells;i++){
	if(vchannel[i]>=0&&vchannel[i]<mNTOFR) {
	  Float_t adcwt = mTofrAdc[vchannel[i]]-mPedTOFr[vchannel[i]];
	  if(adcwt>weight) {
	    weight = adcwt;
	    weightCandidates.clear();
	    weightCandidates.push_back(i);
	  } else if (adcwt == weight) {
	    weightCandidates.push_back(i);
	  }
	}
      }
      if (weightCandidates.size()==1){
	thiscandidate = weightCandidates[0];
	Int_t daqId = vchannel[thiscandidate];
	if (Debug()) LOG_INFO  << "candidate =" << daqId << endm;
      }

      // 2. if still undecided check on hitposition
      if (weightCandidates.size()>1){
	Float_t ss(99.);
	vector<Int_t> ssCandidates;
	thisMatchFlag = 2;
	if (Debug()) LOG_INFO  << " ss ";
	for (unsigned int i=0;i<weightCandidates.size();i++){
	  Int_t ii=weightCandidates[i];	  
	  //	  Int_t daqId = vchannel[ii];
	  StThreeVectorD ahit = vPosition[ii];
	  Float_t yy = ahit.y();
	  Float_t ycell = (vcell[ii]-1-2.5)*mWidthPad;
	  Float_t ll = fabs(yy-ycell);
	  if(ll<ss) {
	    ss = ll;
	    ssCandidates.clear();
	    ssCandidates.push_back(ii);
	  }else if  (ll==ss)
	    ssCandidates.push_back(ii);	  
	}
	if (ssCandidates.size()==1){
	  thiscandidate = ssCandidates[0];
	  Int_t daqId = vchannel[thiscandidate];
    	  if (Debug()) LOG_INFO  << "candidate =" << daqId << endm;
	}

      }

      if (thiscandidate>=0) {
	cellHit.channel = vchannel[thiscandidate];
	cellHit.tray = vtray[thiscandidate];
	cellHit.module = vmodule[thiscandidate];
	cellHit.cell = vcell[thiscandidate];
	cellHit.trackIdVec.push_back(vTrackId[thiscandidate]);
	cellHit.hitPosition = vPosition[thiscandidate];
	cellHit.matchFlag = thisMatchFlag;
	cellHit.zhit = vzhit[thiscandidate];
	cellHit.yhit = vyhit[thiscandidate];

	FinalMatchedCellsVec.push_back(cellHit);
	
	// debugging output
	if (Debug()) {
	  LOG_INFO  << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:" << vTrackId[thiscandidate] << endm;
	}
      }

    } else {
      LOG_INFO  << "E: no cells belong to this track ... should not happen!" << endm;
    }

    tempVec = erasedVec;
  }

  LOG_INFO  << "E: before/after: " << singleHitCellsVec.size() << "/" << FinalMatchedCellsVec.size() << endm;
  // end of Sect.E

  //.........................................................................
  // F. perform further selection and
  //    fill valid track histograms, ntuples and CellCollection
  //
  tempVec.clear();
  tempVec = FinalMatchedCellsVec;
  if(mHisto) {
    if(FinalMatchedCellsVec.size()) mEventCounterHisto->Fill(10);
    mCellsPerEventMatch3->Fill(tempVec.size());
    for(unsigned int ii=0;ii<tempVec.size();ii++) {
      mTracksPerCellMatch3->Fill(tempVec[ii].trackIdVec.size());
      mDaqOccupancyMatch3->Fill(tempVec[ii].channel);
      Float_t ycenter = (tempVec[ii].cell-1-2.5)*mWidthPad;
      Float_t dy = tempVec[ii].yhit - ycenter;
      Float_t dz = tempVec[ii].zhit;
      mDeltaHitMatch3->Fill(dy, dz);
    }
  }

  StTofCellCollection *mCellCollection =  new StTofCellCollection;
  Int_t nValidSingleHitCells(0), nValidSinglePrimHitCells(0);

  for (size_t ii=0; ii < FinalMatchedCellsVec.size(); ii++){
    Int_t daqId = FinalMatchedCellsVec[ii].channel;
    Int_t jj = daqId;
    Int_t tray = FinalMatchedCellsVec[ii].tray;
    Int_t module = FinalMatchedCellsVec[ii].module;
    Int_t cell = FinalMatchedCellsVec[ii].cell;

    Float_t ycenter = (cell-1-2.5)*mWidthPad;
    Float_t dy = FinalMatchedCellsVec[ii].yhit - ycenter;
    if (FinalMatchedCellsVec[ii].trackIdVec.size()!=1)
      LOG_INFO  << "F: WHAT!?!  mult.matched cell in single cell list " << daqId << endm;

    // 1. fill valid single track AND valid tdc histograms
    if (validTdc(mTofrTdc[jj])) nValidSingleHitCells++;

    // get track-id from cell hit vector
    unsigned int trackNode = FinalMatchedCellsVec[ii].trackIdVec[0];
    StTrack *theTrack = nodes[trackNode]->track(primary);
    StTrack *globalTrack = nodes[trackNode]->track(global);

    // 2. continue only if the (primary) track exists
    if (validTofTrack(theTrack) && fabs(dy)<1.9 ){
      nValidSinglePrimHitCells++;

      //--- store number of hits per track
      Int_t nHitsPerTrack = theTrack->topologyMap().numberOfHits(kTpcId);
	  
      // select the apropriate track geometry
      StTrackGeometry *theTrackGeometry = trackGeometry(theTrack);

      //--- get momentum from track
      const StThreeVectorF momentum = theTrackGeometry->momentum();
	    
      //--- calculate flight path
      Double_t pathLength = tofPathLength(&mEvent->primaryVertex()->position(),
					&FinalMatchedCellsVec[ii].hitPosition,
					theTrackGeometry->helix().curvature());

      //--- calculate local hit position on cell based first, last and middle plane
      //    (middle plane is the average of first and last plane, which is mathematically
      //     the same as CellHitVec.hitPosition ... )
//       StThreeVectorD *pInnerLayer, *pOuterLayer;
//       pInnerLayer =  FinalMatchedCellsVec[ii].layerHitPositions.begin();
//       pOuterLayer =  FinalMatchedCellsVec[ii].layerHitPositions.end() - 1;
      
      //--- dig out from the dedx and rich pid traits
      Float_t dedx(0.), cherang(0);
      Int_t dedx_np(0), cherang_nph(0);
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
      
      //--- calculate local hit position on cell based on average hitposition
      //      Float_t localHitPos = mTofGeom->cellHitPosition(&allMatchedCellsVec[ii].hitPosition);
      
      // Fill TOF Cell Collection
      StTofCell *tofCell = new StTofCell(tray, module, cell, daqId, (Int_t)(mTofrAdc[jj]-mPedTOFr[jj]),(Int_t)mTofrTdc[jj],theTrack,FinalMatchedCellsVec[ii].zhit,FinalMatchedCellsVec[ii].matchFlag,FinalMatchedCellsVec[ii].hitPosition);
      mCellCollection->push_back(tofCell);
      
      // dump debug data
      if (Debug()){
	LOG_INFO  << "F: itray=" << tray << " imodule=" << module << " icell=" << cell << "\tnodeid:";
	idVectorIter ij=FinalMatchedCellsVec[ii].trackIdVec.begin();
	while (ij != FinalMatchedCellsVec[ii].trackIdVec.end()) { LOG_INFO  << " " << *ij; ij++; }
	LOG_INFO  << "\tR=" << 1/(theTrackGeometry->helix().curvature())
	     << "\tpT=" << momentum.perp() << "\tp=" << momentum.mag()
	     << "\thits="<< nHitsPerTrack << "\ts="<< pathLength
	     << "\t#fitp=" <<theTrack->fitTraits().numberOfFitPoints(kTpcId)
	  //	     << "\t#trkp=" <<theTrack->detectorInfo()->numberOfPoints(kTpcId)
	     << " \tdedx=" << dedx
	     << " \tdca="<< globalTrack->geometry()->helix().distance(mEvent->primaryVertex()->position())<<" and "<<theTrackGeometry->helix().distance(mEvent->primaryVertex()->position());
	if (cherang!=0) LOG_INFO  << " \trich="<< cherang << " (" << cherang_nph << ")";
	LOG_INFO  << endm;
      }

    } // track exists 
  }
  
  storeMatchData(mCellCollection,theTof);
  delete mCellCollection;
  
  //check StEvent collections --
  if (theTof->dataPresent())
    LOG_INFO  << " TofCollection: raw data container present" << endm;
  if (theTof->cellsPresent()){
    LOG_INFO  << " TofCollection: cell container present."<<endm;
    if (Debug()){
      StSPtrVecTofCell& tmpCellTofVec = theTof->tofCells();
      for (size_t i = 0; i < tmpCellTofVec.size(); i++) {
	StTofCell* p = tmpCellTofVec[i];
	LOG_INFO  << p->trayIndex() << " " << p->moduleIndex() << " " << p->cellIndex() << " " << p->adc() << " " << p->tdc() << " " << p->associatedTrack() << " " << p->matchFlag() << " " << p->position() << endm;
      }
    }
  }
  //-- end check
  
  LOG_INFO  << "F: before/after" << FinalMatchedCellsVec.size() << "/" <<nValidSinglePrimHitCells << endm;
 // end of Sect.F

  LOG_INFO  << "#(cell tracks): " << allCellsHitVec.size()
       << " #(hit cells): " << FinalMatchedCellsVec.size()
       << " #cells (valid tdc): " << nTdcTofr
       << "\n#(single hits): " << nSingleHitCells 
       << " #(single valid hits): " << nValidSingleHitCells
       << " #(single prim valid hits): " << nValidSinglePrimHitCells
       << endm;



  if (doPrintMemoryInfo) {
        StMemoryInfo::instance()->snapshot();
        StMemoryInfo::instance()->print();
  }
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO  << "CPU time for StTofrMatchMaker::Make(): "
	 << timer.elapsedTime() << " sec\n" << endm;
  }

  LOG_INFO  << "StTofrMatchMaker -- bye-bye" << endm;
  return kStOK;
}

//---------------------------------------------------------------------------
Int_t StTofrMatchMaker::processEventYear5(){
  // leave as empty now

  if(mHisto) mEventCounterHisto->Fill(0);
  // event selection ...
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (!validEvent(mEvent)){
    gMessMgr->Info("StTofrMatchMaker -- nothing to do ... bye-bye","OS");
    return kStOK;
  }

  // timing & memory info -only when requested-
  StTimer timer;
  if (doPrintCpuInfo) timer.start();
  if (doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();

  //.........................................................................
  // check for tofCollection and fill local copy with ADC and TDC data
  StTofCollection *theTof = mEvent->tofCollection();
  //  getTofData(theTof);
  // year5
  mSortTofRawData = new StSortTofRawData(theTof);

  IntVec validchannel = mSortTofRawData->GetValidChannel();
  // Any Hits in TOF+PVPD ?
  if(validchannel.size()<=0) {
    LOG_INFO  << " No hits in TOF or pVPD! " << endm;
    // return kStOK;
  } else {
    LOG_INFO  << " Number of TOF+pVPD fired hits: " << validchannel.size() << endm;
  }
  if(Debug()) {
    for(size_t iv=0;iv<validchannel.size();iv++) {
      //if(validchannel[iv]<0||validchannel[iv]>=mNTOFR5) continue; // skip pvpd
      LOG_INFO  << " channel = " << validchannel[iv]<<endm;
      IntVec leTdc = mSortTofRawData->GetLeadingTdc(validchannel[iv]);
      IntVec teTdc = mSortTofRawData->GetTrailingTdc(validchannel[iv]);
      for(size_t iv1=0;iv1<leTdc.size();iv1++) {
        LOG_INFO  << " leading Tdc = " << leTdc[iv1]<<endm;
      }
      for(size_t iv2=0;iv2<teTdc.size();iv2++) {
        LOG_INFO  << " trailing Tdc = " << teTdc[iv2] << endm;
      }
    }
  }   // end mdebug 
    
  // number of primary tracks
  //  (note: different meaning in event.root and richtof.root)
  Int_t refmult(0);
  refmult = uncorrectedNumberOfPrimaries(*mEvent);
  
  if (Debug()){
    LOG_INFO  << " #Tracks           :"      << mEvent->summary()->numberOfTracks()
	 << "\n #goodPrimaryTracks:"    << mEvent->summary()->numberOfGoodPrimaryTracks()
	 << "\n #uncorr.prim.tracks  :" << refmult << endm;
  }

  //.........................................................................
  // A. build vector of candidate cells with valid ADC signals 
  //  idVector validCellIdVec;
  tofCellHitVector daqCellsHitVec;
  //  daqCellsHitVec.clear();
  idVector validModuleVec;

  for(size_t ich=0;ich<validchannel.size();ich++) {
    int ichan = validchannel[ich];
    if(ichan<0||ichan>=mNTOFR5) continue; // tray
    IntVec map = mDaqMap->Tofr5TDCChan2Cell(ichan);
    if(map.size()!=3) continue;

    Int_t IDtray = map[0];
    Int_t IDmodule = map[1];
    Int_t IDcell = map[2];
    
    StructCellHit aDaqCellHit;
    aDaqCellHit.channel = ichan;
    aDaqCellHit.tray = IDtray;
    aDaqCellHit.module = IDmodule;
    aDaqCellHit.cell = IDcell;
    daqCellsHitVec.push_back(aDaqCellHit);

    // additional valid number configuration
    int id = IDtray*100+IDmodule;
    bool ifind = kFALSE;
    for(size_t iv=0;iv<validModuleVec.size();iv++) {
      if(id==validModuleVec[iv]) {
	ifind = kTRUE;
	break;
      }
    }
    if(!ifind) validModuleVec.push_back(id);
  }

  // end of Sect.A
  if(Debug()) {
    LOG_INFO  << "    total # of cells = " << daqCellsHitVec.size() << endm;
    for(size_t iv = 0;iv<validModuleVec.size();iv++) {
      LOG_INFO  << " module # " << validModuleVec[iv] << " Valid! " << endm;
    }
  }
  if(mHisto) {
    mCellsMultInEvent->Fill(daqCellsHitVec.size());
    if(daqCellsHitVec.size()) mEventCounterHisto->Fill(6);
  }
  if(!daqCellsHitVec.size()) return kStOK;

  //.........................................................................
  // B. loop over global tracks and determine all cell-track matches
  //
  tofCellHitVector allCellsHitVec;
  //  allCellsHitVec.clear();
  StructCellHit cellHit;

  StSPtrVecTrackNode& nodes = mEvent->trackNodes();
  Int_t nAllTracks=0;
  for (unsigned int iNode=0; iNode<nodes.size(); iNode++){
    tofCellHitVector cellHitVec;
    //    cellHitVec.clear();
    StTrack *theTrack = nodes[iNode]->track(global);

    // make sure we have a track, a miniDST might have removed it...
    if (validTrack(theTrack)){
      nAllTracks++;
      StPhysicalHelixD theHelix = trackGeometry(theTrack)->helix();

      IntVec projTrayVec;
      if(!mTofrGeom->projTrayVector(theHelix, projTrayVec)) continue;

      IntVec idVec;
      DoubleVec pathVec;
      PointVec  crossVec;

//       idVec.clear();
//       pathVec.clear();
//       crossVec.clear();

      Int_t ncells = 0;
      //      if(mTofrGeom->HelixCrossCellIds(theHelix,idVec,pathVec,crossVec) ) {
      if(mTofrGeom->HelixCrossCellIds(theHelix, validModuleVec, projTrayVec, idVec, pathVec, crossVec)) {
	Int_t cells = idVec.size();
	for (Int_t i=0; i<cells; i++) {
            Int_t icell,imodule,itray;
            Double_t local[3],global[3];
            for(Int_t i2=0;i2<3;i2++){
                 local[i2]=0;
            }
            global[0]=crossVec[i].x();
            global[1]=crossVec[i].y();
            global[2]=crossVec[i].z();
            mTofrGeom->DecodeCellId(idVec[i], icell, imodule, itray);
	    LOG_INFO << " decode " << idVec[i] << "  to tray#" << itray << " module#" << imodule << " cell#" << icell << endm;
	    StTofrGeomSensor* sensor = 
                  mTofrGeom->GetGeomSensor(imodule,itray);
	    if(!sensor) {
	      gMessMgr->Warning("","OS") << " No sensitive module in the projection??? -- Something weird!!! " << endm;
	      continue;
	    }
            sensor->Master2Local(&global[0],&local[0]);
            icell = sensor->FindCellIndex(local);
	    //	    StThreeVectorD glo=sensor->GetCenterPosition();
	    StThreeVectorD glo(global[0], global[1], global[2]);
	    StThreeVectorD hitPos(local[0], local[1], local[2]);
	    delete sensor;
	    //	    if (local[2]<=3.4&&local[2]>=-2.7) {
	      Int_t Iarray = mDaqMap->Tofr5Cell2TDCChan(itray, imodule, icell);
	      if(Iarray>=mDAQOVERFLOW||Iarray<0) continue;
	      ncells++;
	      cellHit.channel = Iarray;
	      cellHit.tray = itray;
	      cellHit.module = imodule;
	      cellHit.cell = icell;
	      cellHit.trackIdVec.push_back(iNode);
	      cellHit.hitPosition = glo;        // global position
	      cellHit.zhit = (Float_t)hitPos.z();
	      cellHit.yhit = (Float_t)hitPos.y();
	      cellHitVec.push_back(cellHit);
	      allCellsHitVec.push_back(cellHit);
	      if(mHisto) {
		mDaqOccupancyProj->Fill(Iarray);
		mHitsPosition->Fill(hitPos.y(), hitPos.z());
	      }
	      
	      if(Debug()) {
		LOG_INFO  <<"B: nodeid=" << iNode << "  projected in " << " tray="<< itray << " module="<<imodule<<" cell="<<icell<<endm;
		LOG_INFO  <<"   hit position " << hitPos << endm;
	      }
	      //	    }
	} // for (Int_t i=0...)
      } // endif(helixcross...)
      if(ncells>0&&mHisto) mHitsMultPerTrack->Fill(ncells);

    } // if(ValidTrack).. 
  } // loop over nodes
  if(Debug())
    LOG_INFO  << "B:  matched/available/total #tracknodes: " <<allCellsHitVec.size() << "/" <<nAllTracks << "/" << nodes.size() << endm;
  if(mHisto) {
    mHitsMultInEvent->Fill(allCellsHitVec.size());
    if(allCellsHitVec.size()) mEventCounterHisto->Fill(7);
  }
  // end of Sect.B
  
  //.........................................................................
  // C. Match find Neighbours -- identify crosstalk
  //
  tofCellHitVector matchHitCellsVec;
  //  matchHitCellsVec.clear();

  tofCellHitVectorIter daqIter = daqCellsHitVec.begin();
  for(unsigned int idaq=0;idaq<daqCellsHitVec.size();idaq++, daqIter++) {
    tofCellHitVectorIter proIter = allCellsHitVec.begin();
    for(unsigned int ipro=0;ipro<allCellsHitVec.size();ipro++, proIter++) {
      if( (daqIter->tray==proIter->tray)&& 
	  (daqIter->module==proIter->module) &&
	  ( ( (proIter->cell==6)&&((proIter->cell==daqIter->cell) ||
				   (proIter->cell==daqIter->cell+1)) )
	    || ( (proIter->cell==1)&&((proIter->cell==daqIter->cell) ||
				      (proIter->cell==daqIter->cell-1)) )
	    || ( (proIter->cell>=2&&proIter->cell<=6) &&
		 ( (proIter->cell==daqIter->cell) ||
		   (proIter->cell==daqIter->cell-1) ||
		   (proIter->cell==daqIter->cell+1) ) ) ) ) {
	cellHit.channel = daqIter->channel;
	cellHit.tray = daqIter->tray;
	cellHit.module = daqIter->module;
	cellHit.cell = daqIter->cell;
	cellHit.hitPosition = proIter->hitPosition;
	cellHit.trackIdVec = proIter->trackIdVec;
	cellHit.zhit = proIter->zhit;
	cellHit.yhit = proIter->yhit;
	matchHitCellsVec.push_back(cellHit);
      }
    }
  } //end {sec. C}
  if(Debug()) {
    LOG_INFO  << "C: before/after: " << allCellsHitVec.size() << "/" << matchHitCellsVec.size() << endm;
  }
  if(mHisto&&matchHitCellsVec.size()) mEventCounterHisto->Fill(8);

  //.........................................................................
  // D. sort hit vectors  and deal with (discard) cells matched by multiple tracks
  //
  Int_t nSingleHitCells(0);
  Int_t nMultiHitsCells(0);

  tofCellHitVector singleHitCellsVec;
  tofCellHitVector multiHitsCellsVec;
//   singleHitCellsVec.clear();
//   multiHitsCellsVec.clear();

  tofCellHitVector tempVec = matchHitCellsVec;
  tofCellHitVector erasedVec = tempVec;
  while (tempVec.size() != 0) {
    Int_t nTracks = 0;
    idVector trackIdVec;

    tofCellHitVectorIter tempIter=tempVec.begin();
    tofCellHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->tray == erasedIter->tray &&
	 tempIter->module == erasedIter->module &&
	 tempIter->cell == erasedIter->cell) {
	nTracks++;
	trackIdVec.push_back(erasedIter->trackIdVec.back());  // merge
	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    cellHit.channel = tempIter->channel;
    cellHit.cell = tempIter->cell;
    cellHit.module = tempIter->module;
    cellHit.tray = tempIter->tray;
    cellHit.hitPosition = tempIter->hitPosition;
    cellHit.trackIdVec = trackIdVec;
    cellHit.zhit = tempIter->zhit;
    cellHit.yhit = tempIter->yhit;

    Float_t ycenter = (tempIter->cell-1-2.5)*mWidthPad;
    Float_t dy = tempIter->yhit - ycenter;
    Float_t dz = tempIter->zhit;

    if(mHisto) {
      mTracksPerCellMatch1->Fill(trackIdVec.size());
      mDaqOccupancyMatch1->Fill(tempIter->channel);
      mDeltaHitMatch1->Fill(dy, dz);
    }

    if (nTracks==1){
      nSingleHitCells++;      
      singleHitCellsVec.push_back(cellHit);
    } else if (nTracks>1){
      nMultiHitsCells++;
      multiHitsCellsVec.push_back(cellHit);
      // for multiple hit cells either discard (yes) or
      // find the most likely candidate.
    } else {
      LOG_INFO  << "D: no tracks extrapolate to matched cell ... should not happen!" << endm;
    }
    
    if (Debug()) {
      LOG_INFO  << "D: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
      idVectorIter ij=trackIdVec.begin();
      while (ij != trackIdVec.end()) { LOG_INFO  << " " << *ij; ij++; }
      LOG_INFO  <<endm;
    }
    
    tempVec = erasedVec;
  }
  if(Debug())
    LOG_INFO  << "D: before/after: " << matchHitCellsVec.size() << "/" << singleHitCellsVec.size() << endm;
  //end of Sect.C
  if(mHisto) {
    mCellsPerEventMatch1->Fill(singleHitCellsVec.size()+multiHitsCellsVec.size());
    if(singleHitCellsVec.size()) mEventCounterHisto->Fill(9);
  } 

  //.........................................................................
  // E. sort and deal singleHitCellsVector for multiple cells associated to single tracks
  //
  tofCellHitVector FinalMatchedCellsVec;
  //  FinalMatchedCellsVec.clear();
  tempVec = singleHitCellsVec;
  if(mHisto) {
    mCellsPerEventMatch2->Fill(tempVec.size());
    for(unsigned int ii=0;ii<tempVec.size();ii++) {
      mTracksPerCellMatch2->Fill(tempVec[ii].trackIdVec.size());
      mDaqOccupancyMatch2->Fill(tempVec[ii].channel);
      Float_t ycenter = (tempVec[ii].cell-1-2.5)*mWidthPad;
      Float_t dy = tempVec[ii].yhit-ycenter;
      Float_t dz = tempVec[ii].zhit;
      mDeltaHitMatch2->Fill(dy, dz);
    }
  }

  erasedVec = tempVec;
  while (tempVec.size() != 0) {
    StructCellHit cellHit;
    Int_t nCells = 0;
    idVector vTrackId;
    vector<StThreeVectorD> vPosition;
    vector<Int_t> vchannel, vtray, vmodule, vcell;
    vector<Float_t> vzhit, vyhit;

    tofCellHitVectorIter tempIter=tempVec.begin();
    tofCellHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->trackIdVec.back() == erasedIter->trackIdVec.back()) {
	nCells++;
	vchannel.push_back(erasedIter->channel);
	vtray.push_back(erasedIter->tray);
	vmodule.push_back(erasedIter->module);
	vcell.push_back(erasedIter->cell);
	vPosition.push_back(erasedIter->hitPosition);
	vTrackId.push_back(erasedIter->trackIdVec.back());
	vzhit.push_back(erasedIter->zhit);
	vyhit.push_back(erasedIter->yhit);

	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    if (nCells==1){
      // for singly hit cell, copy data in singleHitCellsVec
      cellHit.channel = vchannel[0];
      cellHit.tray = vtray[0];
      cellHit.module = vmodule[0];
      cellHit.cell = vcell[0];
      cellHit.trackIdVec.push_back(vTrackId[0]);
      cellHit.hitPosition = vPosition[0];
      cellHit.matchFlag = 0; 
      cellHit.zhit = vzhit[0];
      cellHit.yhit = vyhit[0];

      FinalMatchedCellsVec.push_back(cellHit);

      // debugging output
      if (Debug()) {
	LOG_INFO  << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
	idVectorIter ij=vTrackId.begin();
	while (ij != vTrackId.end()) { LOG_INFO  << " " << *ij; ij++; }
	LOG_INFO  <<endm;
      }
    }
    else if (nCells>1){   // for multiple hit cells  find the most likely candidate.
      Int_t thiscandidate(-99);
      Int_t thisMatchFlag(0);

      // sort on hitposition
      Float_t ss(99.);
      vector<Int_t> ssCandidates;
      thisMatchFlag = 2;
      if (Debug()) LOG_INFO  << " ss " << endm;
      for (Int_t i=0;i<nCells;i++){
	Float_t yy = vyhit[i];
	Float_t ycell = (vcell[i]-1-2.5)*mWidthPad;
	Float_t ll = fabs(yy-ycell);
	if(ll<ss) {
	  ss = ll;
	  ssCandidates.clear();
	  ssCandidates.push_back(i);
	}else if  (ll==ss)
	  ssCandidates.push_back(i);	  
      }
      if (ssCandidates.size()==1){
	thiscandidate = ssCandidates[0];
	Int_t daqId = vchannel[thiscandidate];
	if (Debug()) LOG_INFO  << "candidate =" << daqId << endm;
      }
      

      if (thiscandidate>=0) {
	cellHit.channel = vchannel[thiscandidate];
	cellHit.tray = vtray[thiscandidate];
	cellHit.module = vmodule[thiscandidate];
	cellHit.cell = vcell[thiscandidate];
	cellHit.trackIdVec.push_back(vTrackId[thiscandidate]);
	cellHit.hitPosition = vPosition[thiscandidate];
	cellHit.matchFlag = thisMatchFlag;
	cellHit.zhit = vzhit[thiscandidate];
	cellHit.yhit = vyhit[thiscandidate];

	FinalMatchedCellsVec.push_back(cellHit);
	
	// debugging output
	if (Debug()) {
	LOG_INFO  << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:" << vTrackId[thiscandidate] << endm;
	}
      }

    } else {
      LOG_INFO  << "E: no cells belong to this track ... should not happen!" << endm;
    }

    tempVec = erasedVec;
  }

  LOG_INFO  << "E: before/after: " << singleHitCellsVec.size() << "/" << FinalMatchedCellsVec.size() << endm;
  // end of Sect.E

  //.........................................................................
  // F. perform further selection and
  //    fill valid track histograms, ntuples and CellCollection
  //
  tempVec.clear();
  tempVec = FinalMatchedCellsVec;
  if(mHisto) {
    if(FinalMatchedCellsVec.size()) mEventCounterHisto->Fill(10);
    mCellsPerEventMatch3->Fill(tempVec.size());
    for(unsigned int ii=0;ii<tempVec.size();ii++) {
      mTracksPerCellMatch3->Fill(tempVec[ii].trackIdVec.size());
      mDaqOccupancyMatch3->Fill(tempVec[ii].channel);
      Float_t ycenter = (tempVec[ii].cell-1-2.5)*mWidthPad;
      Float_t dy = tempVec[ii].yhit - ycenter;
      Float_t dz = tempVec[ii].zhit;
      mDeltaHitMatch3->Fill(dy, dz);
    }
  }

  StTofCellCollection *mCellCollection =  new StTofCellCollection;
  Int_t nValidSingleHitCells(0), nValidSinglePrimHitCells(0);

  for (size_t ii=0; ii < FinalMatchedCellsVec.size(); ii++){
    Int_t daqId = FinalMatchedCellsVec[ii].channel;
    Int_t jj = daqId;
    Int_t tray = FinalMatchedCellsVec[ii].tray;
    Int_t module = FinalMatchedCellsVec[ii].module;
    Int_t cell = FinalMatchedCellsVec[ii].cell;

    Float_t ycenter = (cell-1-2.5)*mWidthPad;
    Float_t dy = FinalMatchedCellsVec[ii].yhit - ycenter;
    if (FinalMatchedCellsVec[ii].trackIdVec.size()!=1)
      LOG_INFO  << "F: WHAT!?!  mult.matched cell in single cell list " << daqId << endm;


    /* move INL correction to calibration maker
    // Read in Leading and Trailing edge TDC, apply on INL correction
    int tmptdc = (mSortTofRawData->GetLeadingTdc(jj))[0];
    // do inl correction
    int bin = int(tmptdc)&0x03ff;
    float tmptdc_f = tmptdc + GetINLcorr(4, jj, bin);
    float letime  = tmptdc_f*25./1024; // ns

    tmptdc=(mSortTofRawData->GetTrailingTdc(jj))[0];    
    // do inl correction
    bin = int(tmptdc)&0x0ff;
    tmptdc_f = tmptdc + GetINLcorr(5, jj, bin);
    float tetime  = tmptdc_f*100./1024; // ns
    float tot = tetime - letime; // ns
    */

    int letdc = (mSortTofRawData->GetLeadingTdc(jj))[0];
    int tetdc=(mSortTofRawData->GetTrailingTdc(jj))[0];

    // get track-id from cell hit vector
    unsigned int trackNode = FinalMatchedCellsVec[ii].trackIdVec[0];
    StTrack *theTrack = nodes[trackNode]->track(primary);
    StTrack *globalTrack = nodes[trackNode]->track(global);

    // 2. continue only if the (primary) track exists
    if (validTofTrack(theTrack) && fabs(dy)<1.9 ){
      nValidSinglePrimHitCells++;

      //--- store number of hits per track
      Int_t nHitsPerTrack = theTrack->topologyMap().numberOfHits(kTpcId);
	  
      // select the apropriate track geometry
      StTrackGeometry *theTrackGeometry = trackGeometry(theTrack);

      //--- get momentum from track
      const StThreeVectorF momentum = theTrackGeometry->momentum();
	    
      //--- calculate flight path
      Double_t pathLength = tofPathLength(&mEvent->primaryVertex()->position(),
					&FinalMatchedCellsVec[ii].hitPosition,
					theTrackGeometry->helix().curvature());

      //--- calculate local hit position on cell based first, last and middle plane
      //    (middle plane is the average of first and last plane, which is mathematically
      //     the same as CellHitVec.hitPosition ... )
//       StThreeVectorD *pInnerLayer, *pOuterLayer;
//       pInnerLayer =  FinalMatchedCellsVec[ii].layerHitPositions.begin();
//       pOuterLayer =  FinalMatchedCellsVec[ii].layerHitPositions.end() - 1;
      
      //--- dig out from the dedx and rich pid traits
      Float_t dedx(0.), cherang(0);
      Int_t dedx_np(0), cherang_nph(0);
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
      
      //--- calculate local hit position on cell based on average hitposition
      //      Float_t localHitPos = mTofGeom->cellHitPosition(&allMatchedCellsVec[ii].hitPosition);
      
      // Fill TOF Cell Collection
      // year5, raw letdc and tetdc stored as "tdc" and "adc" in StTofCell
      // 
      //      StTofCell *tofCell = new StTofCell(tray, module, cell, daqId, (Int_t)(tot*1000.),(Int_t)(letime*1000.),theTrack,FinalMatchedCellsVec[ii].zhit,FinalMatchedCellsVec[ii].matchFlag,FinalMatchedCellsVec[ii].hitPosition);
      StTofCell *tofCell = new StTofCell(tray, module, cell, daqId, tetdc,letdc,theTrack,FinalMatchedCellsVec[ii].zhit,FinalMatchedCellsVec[ii].matchFlag,FinalMatchedCellsVec[ii].hitPosition);
      mCellCollection->push_back(tofCell);
      
      // dump debug data
      if (Debug()){
	LOG_INFO  << "F: itray=" << tray << " imodule=" << module << " icell=" << cell << "\tnodeid:";
	idVectorIter ij=FinalMatchedCellsVec[ii].trackIdVec.begin();
	while (ij != FinalMatchedCellsVec[ii].trackIdVec.end()) { LOG_INFO  << " " << *ij; ij++; }
	LOG_INFO  << "\tR=" << 1/(theTrackGeometry->helix().curvature())
	     << "\tpT=" << momentum.perp() << "\tp=" << momentum.mag()
	     << "\thits="<< nHitsPerTrack << "\ts="<< pathLength
	     << "\t#fitp=" <<theTrack->fitTraits().numberOfFitPoints(kTpcId)
	  //	     << "\t#trkp=" <<theTrack->detectorInfo()->numberOfPoints(kTpcId)
	     << " \tdedx=" << dedx
	     << " \tdca="<< globalTrack->geometry()->helix().distance(mEvent->primaryVertex()->position())<<" and "<<theTrackGeometry->helix().distance(mEvent->primaryVertex()->position());
	if (cherang!=0) LOG_INFO  << " \trich="<< cherang << " (" << cherang_nph << ")";
	LOG_INFO  << endm;
      }

    } // track exists 
  } // end final matched cells
  
  storeMatchData(mCellCollection,theTof);
  delete mCellCollection;

  delete mSortTofRawData;
  mSortTofRawData = 0;
  
  //check StEvent collections --
  if (theTof->dataPresent())
    LOG_INFO  << " TofCollection: raw data container present" << endm;
  if (theTof->cellsPresent()){
    LOG_INFO  << " TofCollection: cell container present."<<endm;
    if (Debug()){
      StSPtrVecTofCell& tmpCellTofVec = theTof->tofCells();
      LOG_INFO  << " # of matched cells " << tmpCellTofVec.size() << endm;
      for (size_t i = 0; i < tmpCellTofVec.size(); i++) {
	StTofCell* p = tmpCellTofVec[i];
	LOG_INFO  << p->trayIndex() << " " << p->moduleIndex() << " " << p->cellIndex() << " " << p->trailingEdgeTime() << " " << p->leadingEdgeTime() << " " << p->associatedTrack() << " " << p->matchFlag() << " " << p->position() << endm;
      }
    }
  }
  //-- end check
  
  LOG_INFO  << "F: before/after" << FinalMatchedCellsVec.size() << "/" <<nValidSinglePrimHitCells << endm;
 // end of Sect.F

  LOG_INFO  << "#(cell tracks): " << allCellsHitVec.size()
       << " #(hit cells): " << FinalMatchedCellsVec.size()
       << "\n#(single hits): " << nSingleHitCells 
       << " #(single valid hits): " << nValidSingleHitCells
       << " #(single prim valid hits): " << nValidSinglePrimHitCells
       << endm;



  if (doPrintMemoryInfo) {
        StMemoryInfo::instance()->snapshot();
        StMemoryInfo::instance()->print();
  }
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO  << "CPU time for StTofrMatchMaker::Make(): "
	 << timer.elapsedTime() << " sec\n" << endm;
  }

  LOG_INFO  << "StTofrMatchMaker -- bye-bye" << endm;



  return kStOK;
}

//---------------------------------------------------------------------------
Int_t StTofrMatchMaker::processEventYear8(){
  // leave as empty now

  if(Debug()) LOG_INFO << " processing event in run 8 " << endm;
  if(mHisto) mEventCounterHisto->Fill(0);
  // event selection ...
  mEvent = (StEvent *) GetInputDS("StEvent");
//  if (!validEvent(mEvent)){
  if(!mEvent || !(mEvent->tofCollection()) || !(mEvent->tofCollection()->rawdataPresent()) ) {
    gMessMgr->Info("StTofrMatchMaker -- nothing to do ... bye-bye","OS");
    return kStOK;
  }
  if(mHisto) mEventCounterHisto->Fill(1);

  // timing & memory info -only when requested-
  StTimer timer;
  if (doPrintCpuInfo) timer.start();
  if (doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();

  //.........................................................................
  // check for tofCollection and fill local copy with ADC and TDC data
  StTofCollection *theTof = mEvent->tofCollection();

  //.........................................................................
  // push raw data into StTofData for QA:
  // Be careful, the vpd trayId in TofData is 121 and 122 (**)
  //  and
  //.........................................................................
  // A. build vector of candidate cells with valid ADC signals 
  //  idVector validCellIdVec;
  tofCellHitVector daqCellsHitVec;
  //  daqCellsHitVec.clear();
  idVector validModuleVec;

  mSortTofRawData = new StSortTofRawData(theTof, mDaqMap);

  // multi-tray system
  IntVec validtray = mDaqMap->ValidTrays();
  for(size_t i=0;i<validtray.size();i++) {
    int trayId = validtray[i];
    IntVec validchannel = mSortTofRawData->GetValidChannel(trayId);
    if(Debug()) LOG_INFO  << " Number of fired hits on tray " << trayId << " = " << validchannel.size() << endm;

    for(size_t iv=0;iv<validchannel.size();iv++) {
      IntVec leTdc = mSortTofRawData->GetLeadingTdc(trayId, validchannel[iv], kTRUE);
      IntVec teTdc = mSortTofRawData->GetTrailingTdc(trayId, validchannel[iv], kTRUE);

      if(!leTdc.size() || !teTdc.size()) continue;

      int chan = validchannel[iv];
      IntVec map = mDaqMap->TDIGChan2Cell(chan);
      int moduleId = map[0];
      int cellId = map[1];
      
      StructCellHit aDaqCellHit;
      aDaqCellHit.channel = chan;
      aDaqCellHit.tray = trayId;
      aDaqCellHit.module = moduleId;
      aDaqCellHit.cell = cellId;
      daqCellsHitVec.push_back(aDaqCellHit);
      
      // additional valid number configuration
      int id = trayId*100+moduleId;
      bool ifind = kFALSE;
      for(size_t im=0;im<validModuleVec.size();im++) {
	if(id==validModuleVec[im]) {
	  ifind = kTRUE;
	  break;
	}
      }
      if(!ifind) validModuleVec.push_back(id);

      if(mHisto) {
  //      mDaqOccupancyValid->Fill(chan);
        mDaqOccupancyValid->Fill((moduleId-1)*mNCell+(cellId-1));
        mDaqOccupancyValidAll->Fill((trayId-76)*mNTOF+(moduleId-1)*mNCell+(cellId-1));
      }      
      //
      // store data from trays and vpds into StTofData
      //
      int dataIndex = (trayId-1)*mNTOF + (moduleId-1)*mNCell + (cellId-1);
      StTofData *aData = new StTofData(dataIndex,0,0,0,0,leTdc[0],teTdc[0]);
      theTof->addData(aData);

      if(Debug()) {
	for(size_t iv1=0;iv1<leTdc.size();iv1++) {
	  LOG_INFO  << " leading Tdc = " << leTdc[iv1]<<endm;
	}
	for(size_t iv2=0;iv2<teTdc.size();iv2++) {
	  LOG_INFO  << " trailing Tdc = " << teTdc[iv2] << endm;
	}
      } // end debug
    } // end channel

  } // end tray

  // vpd -> StTofData
  for(int ivpd=0;ivpd<2;ivpd++) { // west and east sides
    int trayId = (ivpd==0) ? mWestVpdTrayId : mEastVpdTrayId;
    IntVec validtube = mSortTofRawData->GetValidChannel(trayId);
    if(Debug()) LOG_INFO  << " Number of fired hits on tray(vpd) " << trayId << " = " << validtube.size() << endm;

    if(!validtube.size()) continue;
    for(int i=0;i<mNVPD;i++) {
      int tubeId = i+1;
      int lechan = (ivpd==0) ? mDaqMap->WestPMT2TDIGLeChan(tubeId) : mDaqMap->EastPMT2TDIGLeChan(tubeId);
      IntVec leTdc = mSortTofRawData->GetLeadingTdc(trayId, lechan, kTRUE);
      IntVec teTdc = mSortTofRawData->GetTrailingTdc(trayId, lechan, kTRUE);  // channel number should be le, sorted in StSortTofRawData

      if(leTdc.size()&&mHisto) mDaqOccupancyVpd->Fill(ivpd*mNVPD+i);

      if(leTdc.size() && teTdc.size()) {
	int dataIndex = (ivpd+120)*mNTOF + (tubeId-1);
	StTofData *aData = new StTofData(dataIndex,0,0,0,0,leTdc[0],teTdc[0]);
	theTof->addData(aData);

        if(mHisto) mDaqOccupancyValidVpd->Fill(ivpd*mNVPD+i);
      }
    }
  }
  //

  // end of Sect.A
  if(Debug()) {
    LOG_INFO  << "    total # of cells = " << daqCellsHitVec.size() << endm;
    for(size_t iv = 0;iv<validModuleVec.size();iv++) {
      LOG_INFO  << " module # " << validModuleVec[iv] << " Valid! " << endm;
    }
  }
  if(mHisto) {
    mCellsMultInEvent->Fill(daqCellsHitVec.size());
    if(daqCellsHitVec.size()) mEventCounterHisto->Fill(6);
  }
//  if(!daqCellsHitVec.size()) return kStOK;

  //.........................................................................
  // B. loop over global tracks and determine all cell-track matches
  //
  tofCellHitVector allCellsHitVec;
  //  allCellsHitVec.clear();
  StructCellHit cellHit;

  StSPtrVecTrackNode& nodes = mEvent->trackNodes();
  Int_t nAllTracks=0;
  for (unsigned int iNode=0; iNode<nodes.size(); iNode++){
    tofCellHitVector cellHitVec;
    //    cellHitVec.clear();
    StGlobalTrack *theTrack = dynamic_cast<StGlobalTrack*>(nodes[iNode]->track(global));
    if(!theTrack) continue;

    StThreeVectorF mom = theTrack->geometry()->momentum();
    float pt = mom.perp();
    float eta = mom.pseudoRapidity();
    float phi = mom.phi();
    if (phi<0.) phi += 2.*3.14159;

    float dEdx = -999.;
    int ndEdxpts = 0;
    float nSigmaPion = -999.;
    static StTpcDedxPidAlgorithm PidAlgorithm;
    static StPionPlus* Pion = StPionPlus::instance();

    const StParticleDefinition* pd = theTrack->pidTraits(PidAlgorithm);
    if (pd) {
      nSigmaPion = PidAlgorithm.numberOfSigma(Pion);
    }
    if ( PidAlgorithm.traits() ) {
      dEdx = PidAlgorithm.traits()->mean();
      ndEdxpts = PidAlgorithm.traits()->numberOfPoints();
    }
    int nfitpts = theTrack->fitTraits().numberOfFitPoints(kTpcId);
//    cout << " dEdx = " << dEdx << endl;
//    cout << " nSigmaPi = " << nSigmaPion << endl;
//    if (pt<0.2) continue;

    // make sure we have a track, a miniDST might have removed it...
    if (validTrackRun8(theTrack)){
      if(mHisto) {
        mTrackPtEta->Fill(pt, eta);
        mTrackPtPhi->Fill(pt, phi);
        mTrackNFitPts->Fill(nfitpts);
        if(dEdx>0.) mTrackdEdxvsp->Fill(mom.mag(), dEdx*1.e6);
        if(fabs(nSigmaPion)<5.) mNSigmaPivsPt->Fill(pt, nSigmaPion+5.*theTrack->geometry()->charge());
      }

      if(mSaveTree) {
        trackTree.pt = pt;
        trackTree.eta = eta;
        trackTree.phi = phi;
        trackTree.nfitpts = nfitpts;
        trackTree.dEdx = dEdx*1.e6;
        trackTree.ndEdxpts = ndEdxpts;
        trackTree.charge = theTrack->geometry()->charge();      
        trackTree.projTrayId = 0;
        trackTree.projCellChan = -1;
        trackTree.projY = -999.;
        trackTree.projZ = -999.;
      }

      nAllTracks++;
      StPhysicalHelixD theHelix = trackGeometry(theTrack)->helix();

//      IntVec projTrayVec;
//      if(!mTofrGeom->projTrayVector(theHelix, projTrayVec)) continue;

      IntVec idVec;
      DoubleVec pathVec;
      PointVec  crossVec;

//       idVec.clear();
//       pathVec.clear();
//       crossVec.clear();

      Int_t ncells = 0;
      if(mTofrGeom->HelixCrossCellIds(theHelix,idVec,pathVec,crossVec) ) {
//      if(mTofrGeom->HelixCrossCellIds(theHelix, validModuleVec, projTrayVec, idVec, pathVec, crossVec)) {
	Int_t cells = idVec.size();
	for (Int_t i=0; i<cells; i++) {
            Int_t icell,imodule,itray;
            Double_t local[3],global[3];
            for(Int_t i2=0;i2<3;i2++){
                 local[i2]=0;
            }
            global[0]=crossVec[i].x();
            global[1]=crossVec[i].y();
            global[2]=crossVec[i].z();
            mTofrGeom->DecodeCellId(idVec[i], icell, imodule, itray);
	    LOG_INFO << " decode " << idVec[i] << "  to tray#" << itray << " module#" << imodule << " cell#" << icell << endm;
	    StTofrGeomSensor* sensor = 
                  mTofrGeom->GetGeomSensor(imodule,itray);
	    if(!sensor) {
	      gMessMgr->Warning("","OS") << " No sensitive module in the projection??? -- Something weird!!! " << endm;
	      continue;
	    }
            sensor->Master2Local(&global[0],&local[0]);
            icell = sensor->FindCellIndex(local);
	    //	    StThreeVectorD glo=sensor->GetCenterPosition();
	    StThreeVectorD glo(global[0], global[1], global[2]);
	    StThreeVectorD hitPos(local[0], local[1], local[2]);
	    delete sensor;
	    if (local[2]<=2.7&&local[2]>=-3.4) {
	      Int_t Iarray = mDaqMap->Cell2TDIGChan(imodule, icell);
	      if(Iarray>=mDAQOVERFLOW||Iarray<0) continue;
	      ncells++;
	      cellHit.channel = Iarray;
	      cellHit.tray = itray;
	      cellHit.module = imodule;
	      cellHit.cell = icell;
	      cellHit.trackIdVec.push_back(iNode);
	      cellHit.hitPosition = glo;        // global position
	      cellHit.zhit = (Float_t)hitPos.z();
	      cellHit.yhit = (Float_t)hitPos.y();
	      cellHitVec.push_back(cellHit);
	      allCellsHitVec.push_back(cellHit);
	      if(mHisto) {
//		mDaqOccupancyProj->Fill(Iarray);
                if(itray>=76&&itray<=80) {
                  mDaqOccupancyProj->Fill((imodule-1)*mNCell+(icell-1));
                  mDaqOccupancyProjAll->Fill((itray-76)*mNTOF+(imodule-1)*mNCell+(icell-1));
		  mHitsPosition->Fill(hitPos.y(), hitPos.z());
                }
	      }
	      
              if(mSaveTree) {
                trackTree.projTrayId = itray;
                trackTree.projCellChan = (imodule-1)*mNCell+(icell-1);
                trackTree.projY = local[1];
                trackTree.projZ = local[2];
              }

	      if(Debug()) {
		LOG_INFO  <<"B: nodeid=" << iNode << "  projected in " << " tray="<< itray << " module="<<imodule<<" cell="<<icell<<endm;
		LOG_INFO  <<"   hit position " << hitPos << endm;
	      }
	    }
	} // for (Int_t i=0...)
      } // endif(helixcross...)
      if(ncells>0&&mHisto) mHitsMultPerTrack->Fill(ncells);

      if(mHisto && mSaveTree) mTrackTree->Fill();

    } // if(ValidTrack).. 
  } // loop over nodes
  if(Debug())
    LOG_INFO  << "B:  matched/available/total #tracknodes: " <<allCellsHitVec.size() << "/" <<nAllTracks << "/" << nodes.size() << endm;
  if(mHisto) {
    mHitsMultInEvent->Fill(allCellsHitVec.size());
    if(allCellsHitVec.size()) mEventCounterHisto->Fill(7);
  }
  // end of Sect.B

  //.........................................................................
  // C. Match find Neighbours -- identify crosstalk
  //
  tofCellHitVector matchHitCellsVec;
  //  matchHitCellsVec.clear();

  tofCellHitVectorIter daqIter = daqCellsHitVec.begin();
  for(unsigned int idaq=0;idaq<daqCellsHitVec.size();idaq++, daqIter++) {
    tofCellHitVectorIter proIter = allCellsHitVec.begin();
    for(unsigned int ipro=0;ipro<allCellsHitVec.size();ipro++, proIter++) {

      int daqIndex = (daqIter->module-1)*6 + (daqIter->cell-1);
      int proIndex = (proIter->module-1)*6 + (proIter->cell-1);
      int hisIndex = daqIter->tray - 76;
      int daqAllIndex = (daqIter->tray - 76)*192 + daqIndex;
      int proAllIndex = (proIter->tray - 76)*192 + proIndex;
      if (mHisto) mHitCorrAll->Fill(proAllIndex,daqAllIndex);
      if(daqIter->tray==proIter->tray) {
	if (mHisto) {
	  if(hisIndex>=0&&hisIndex<5) {
	    mHitCorr[hisIndex]->Fill(proIndex,daqIndex);
	    mHitCorrModule[hisIndex]->Fill(proIter->module-1,daqIter->module-1);
	  } else {
	    cout << " weird tray # " << daqIter->tray << endl;
	  }
	}
      }
      if( (daqIter->tray==proIter->tray)&& 
	  (daqIter->module==proIter->module) &&
	  ( ( (proIter->cell==6)&&((proIter->cell==daqIter->cell) ||
				   (proIter->cell==daqIter->cell+1)) )
	    || ( (proIter->cell==1)&&((proIter->cell==daqIter->cell) ||
				      (proIter->cell==daqIter->cell-1)) )
	    || ( (proIter->cell>=2&&proIter->cell<=6) &&
		 ( (proIter->cell==daqIter->cell) ||
		   (proIter->cell==daqIter->cell-1) ||
		   (proIter->cell==daqIter->cell+1) ) ) ) ) {
	cellHit.channel = daqIter->channel;
	cellHit.tray = daqIter->tray;
	cellHit.module = daqIter->module;
	cellHit.cell = daqIter->cell;
	cellHit.hitPosition = proIter->hitPosition;
	cellHit.trackIdVec = proIter->trackIdVec;
	cellHit.zhit = proIter->zhit;
	cellHit.yhit = proIter->yhit;
	matchHitCellsVec.push_back(cellHit);
      }
    }
  } //end {sec. C}
  if(Debug()) {
    LOG_INFO  << "C: before/after: " << allCellsHitVec.size() << "/" << matchHitCellsVec.size() << endm;
  }
  if(mHisto&&matchHitCellsVec.size()) mEventCounterHisto->Fill(8);

  //.........................................................................
  // D. sort hit vectors  and deal with (discard) cells matched by multiple tracks
  //
  Int_t nSingleHitCells(0);
  Int_t nMultiHitsCells(0);

  tofCellHitVector singleHitCellsVec;
  tofCellHitVector multiHitsCellsVec;
//   singleHitCellsVec.clear();
//   multiHitsCellsVec.clear();

  tofCellHitVector tempVec = matchHitCellsVec;
  tofCellHitVector erasedVec = tempVec;
  while (tempVec.size() != 0) {
    Int_t nTracks = 0;
    idVector trackIdVec;

    tofCellHitVectorIter tempIter=tempVec.begin();
    tofCellHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->tray == erasedIter->tray &&
	 tempIter->module == erasedIter->module &&
	 tempIter->cell == erasedIter->cell) {
	nTracks++;
	trackIdVec.push_back(erasedIter->trackIdVec.back());  // merge
	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    cellHit.channel = tempIter->channel;
    cellHit.cell = tempIter->cell;
    cellHit.module = tempIter->module;
    cellHit.tray = tempIter->tray;
    cellHit.hitPosition = tempIter->hitPosition;
    cellHit.trackIdVec = trackIdVec;
    cellHit.zhit = tempIter->zhit;
    cellHit.yhit = tempIter->yhit;

    Float_t ycenter = (tempIter->cell-1-2.5)*mWidthPad;
    Float_t dy = tempIter->yhit - ycenter;
    Float_t dz = tempIter->zhit;

    if(mHisto) {
      mTracksPerCellMatch1->Fill(trackIdVec.size());
//      mDaqOccupancyMatch1->Fill(tempIter->channel);
      mDaqOccupancyMatch1->Fill((tempIter->module-1)*mNCell+(tempIter->cell-1));
      mDeltaHitMatch1->Fill(dy, dz);
    }

    if (nTracks==1){
      nSingleHitCells++;      
      singleHitCellsVec.push_back(cellHit);
    } else if (nTracks>1){
      nMultiHitsCells++;
      multiHitsCellsVec.push_back(cellHit);
      // for multiple hit cells either discard (yes) or
      // find the most likely candidate.
    } else {
      LOG_INFO  << "D: no tracks extrapolate to matched cell ... should not happen!" << endm;
    }
    
    if (Debug()) {
      LOG_INFO  << "D: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
      idVectorIter ij=trackIdVec.begin();
      while (ij != trackIdVec.end()) { LOG_INFO  << " " << *ij; ij++; }
      LOG_INFO  <<endm;
    }
    
    tempVec = erasedVec;
  }
  if(Debug())
    LOG_INFO  << "D: before/after: " << matchHitCellsVec.size() << "/" << singleHitCellsVec.size() << endm;
  //end of Sect.C
  if(mHisto) {
    mCellsPerEventMatch1->Fill(singleHitCellsVec.size()+multiHitsCellsVec.size());
    if(singleHitCellsVec.size()) mEventCounterHisto->Fill(9);
  } 

  //.........................................................................
  // E. sort and deal singleHitCellsVector for multiple cells associated to single tracks
  //
  tofCellHitVector FinalMatchedCellsVec;
  //  FinalMatchedCellsVec.clear();
  tempVec = singleHitCellsVec;
  if(mHisto) {
    mCellsPerEventMatch2->Fill(tempVec.size());
    for(unsigned int ii=0;ii<tempVec.size();ii++) {
      mTracksPerCellMatch2->Fill(tempVec[ii].trackIdVec.size());
 //     mDaqOccupancyMatch2->Fill(tempVec[ii].channel);
      mDaqOccupancyMatch2->Fill((tempVec[ii].module-1)*mNCell+(tempVec[ii].cell-1));
      Float_t ycenter = (tempVec[ii].cell-1-2.5)*mWidthPad;
      Float_t dy = tempVec[ii].yhit-ycenter;
      Float_t dz = tempVec[ii].zhit;
      mDeltaHitMatch2->Fill(dy, dz);
    }
  }

  erasedVec = tempVec;
  while (tempVec.size() != 0) {
    StructCellHit cellHit;
    Int_t nCells = 0;
    idVector vTrackId;
    vector<StThreeVectorD> vPosition;
    vector<Int_t> vchannel, vtray, vmodule, vcell;
    vector<Float_t> vzhit, vyhit;

    tofCellHitVectorIter tempIter=tempVec.begin();
    tofCellHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->trackIdVec.back() == erasedIter->trackIdVec.back()) {
	nCells++;
	vchannel.push_back(erasedIter->channel);
	vtray.push_back(erasedIter->tray);
	vmodule.push_back(erasedIter->module);
	vcell.push_back(erasedIter->cell);
	vPosition.push_back(erasedIter->hitPosition);
	vTrackId.push_back(erasedIter->trackIdVec.back());
	vzhit.push_back(erasedIter->zhit);
	vyhit.push_back(erasedIter->yhit);

	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    if (nCells==1){
      // for singly hit cell, copy data in singleHitCellsVec
      cellHit.channel = vchannel[0];
      cellHit.tray = vtray[0];
      cellHit.module = vmodule[0];
      cellHit.cell = vcell[0];
      cellHit.trackIdVec.push_back(vTrackId[0]);
      cellHit.hitPosition = vPosition[0];
      cellHit.matchFlag = 0; 
      cellHit.zhit = vzhit[0];
      cellHit.yhit = vyhit[0];

      FinalMatchedCellsVec.push_back(cellHit);

      // debugging output
      if (Debug()) {
	LOG_INFO  << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
	idVectorIter ij=vTrackId.begin();
	while (ij != vTrackId.end()) { LOG_INFO  << " " << *ij; ij++; }
	LOG_INFO  <<endm;
      }
    }
    else if (nCells>1){   // for multiple hit cells  find the most likely candidate.
      Int_t thiscandidate(-99);
      Int_t thisMatchFlag(0);

      // sort on hitposition
      Float_t ss(99.);
      vector<Int_t> ssCandidates;
      thisMatchFlag = 2;
      if (Debug()) LOG_INFO  << " ss " << endm;
      for (Int_t i=0;i<nCells;i++){
	Float_t yy = vyhit[i];
	Float_t ycell = (vcell[i]-1-2.5)*mWidthPad;
	Float_t ll = fabs(yy-ycell);
	if(ll<ss) {
	  ss = ll;
	  ssCandidates.clear();
	  ssCandidates.push_back(i);
	}else if  (ll==ss)
	  ssCandidates.push_back(i);	  
      }
      if (ssCandidates.size()==1){
	thiscandidate = ssCandidates[0];
	Int_t daqId = vchannel[thiscandidate];
	if (Debug()) LOG_INFO  << "candidate =" << daqId << endm;
      }
      

      if (thiscandidate>=0) {
	cellHit.channel = vchannel[thiscandidate];
	cellHit.tray = vtray[thiscandidate];
	cellHit.module = vmodule[thiscandidate];
	cellHit.cell = vcell[thiscandidate];
	cellHit.trackIdVec.push_back(vTrackId[thiscandidate]);
	cellHit.hitPosition = vPosition[thiscandidate];
	cellHit.matchFlag = thisMatchFlag;
	cellHit.zhit = vzhit[thiscandidate];
	cellHit.yhit = vyhit[thiscandidate];

	FinalMatchedCellsVec.push_back(cellHit);
	
	// debugging output
	if (Debug()) {
	LOG_INFO  << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:" << vTrackId[thiscandidate] << endm;
	}
      }

    } else {
      LOG_INFO  << "E: no cells belong to this track ... should not happen!" << endm;
    }

    tempVec = erasedVec;
  }

  LOG_INFO  << "E: before/after: " << singleHitCellsVec.size() << "/" << FinalMatchedCellsVec.size() << endm;
  // end of Sect.E

  //.........................................................................
  // F. perform further selection and
  //    fill valid track histograms, ntuples and CellCollection
  //
  tempVec.clear();
  tempVec = FinalMatchedCellsVec;
  if(mHisto) {
    if(FinalMatchedCellsVec.size()) mEventCounterHisto->Fill(10);
    mCellsPerEventMatch3->Fill(tempVec.size());
    for(unsigned int ii=0;ii<tempVec.size();ii++) {
      mTracksPerCellMatch3->Fill(tempVec[ii].trackIdVec.size());
//      mDaqOccupancyMatch3->Fill(tempVec[ii].channel);
      mDaqOccupancyMatch3->Fill((tempVec[ii].module-1)*mNCell+(tempVec[ii].cell-1));
      Float_t ycenter = (tempVec[ii].cell-1-2.5)*mWidthPad;
      Float_t dy = tempVec[ii].yhit - ycenter;
      Float_t dz = tempVec[ii].zhit;
      mDeltaHitMatch3->Fill(dy, dz);
    }
  }

  StTofCellCollection *mCellCollection =  new StTofCellCollection;
  Int_t nValidSingleHitCells(0), nValidSinglePrimHitCells(0);

  for (size_t ii=0; ii < FinalMatchedCellsVec.size(); ii++){
    Int_t daqId = FinalMatchedCellsVec[ii].channel;
    Int_t jj = daqId;
    Int_t tray = FinalMatchedCellsVec[ii].tray;
    Int_t module = FinalMatchedCellsVec[ii].module;
    Int_t cell = FinalMatchedCellsVec[ii].cell;

//    Float_t ycenter = (cell-1-2.5)*mWidthPad;
//    Float_t dy = FinalMatchedCellsVec[ii].yhit - ycenter;
    if (FinalMatchedCellsVec[ii].trackIdVec.size()!=1)
      LOG_INFO  << "F: WHAT!?!  mult.matched cell in single cell list " << daqId << endm;


    // Read in Leading and Trailing edge TDC, apply on INL correction
    //
    int tmptdc = (mSortTofRawData->GetLeadingTdc(tray,jj,kTRUE))[0];
    int bin = (int)tmptdc&0x3ff;
    double tmptdc_f = tmptdc + mTofINLCorr->getTrayINLCorr(tray, jj, bin);
    double letime = tmptdc_f*VHRBIN2PS / 1000.;

    tmptdc=(mSortTofRawData->GetTrailingTdc(tray,jj,kTRUE))[0];
    bin = (int)tmptdc&0x3ff;
    tmptdc_f = tmptdc + mTofINLCorr->getTrayINLCorr(tray, jj, bin);
    double tetime = tmptdc_f*VHRBIN2PS / 1000.;

    // get track-id from cell hit vector
    unsigned int trackNode = FinalMatchedCellsVec[ii].trackIdVec[0];
//    StTrack *theTrack = nodes[trackNode]->track(primary);
    StTrack *globalTrack = nodes[trackNode]->track(global);

    // 2. continue only if the (primary) track exists
//    if (validTofTrack(theTrack) && fabs(dy)<1.9 ){
      nValidSinglePrimHitCells++;

      //--- store number of hits per track
 //     Int_t nHitsPerTrack = theTrack->topologyMap().numberOfHits(kTpcId);
	  
      // select the apropriate track geometry
 //     StTrackGeometry *theTrackGeometry = trackGeometry(globalTrack);

      //--- calculate local hit position on cell based on average hitposition
      //      Float_t localHitPos = mTofGeom->cellHitPosition(&allMatchedCellsVec[ii].hitPosition);
      
      // Fill TOF Cell Collection
      StTofCell *tofCell = new StTofCell(tray, module, cell, daqId, globalTrack, FinalMatchedCellsVec[ii].zhit, FinalMatchedCellsVec[ii].matchFlag, FinalMatchedCellsVec[ii].hitPosition);
      tofCell->setLeadingEdgeTime(letime);
      tofCell->setTrailingEdgeTime(tetime);
      mCellCollection->push_back(tofCell);
      
      // dump debug data
      if (Debug()){
	LOG_INFO  << "F: itray=" << tray << " imodule=" << module << " icell=" << cell << "\tnodeid:";
	idVectorIter ij=FinalMatchedCellsVec[ii].trackIdVec.begin();
	while (ij != FinalMatchedCellsVec[ii].trackIdVec.end()) { LOG_INFO  << " " << *ij; ij++; }
	LOG_INFO  << endm;
      }

//    } // track exists 
  } // end final matched cells


  // put INL corrected vpd information in StTofCell as well
  StSPtrVecTofData &tofData = theTof->tofData();
  for(size_t ii = 0; ii<tofData.size(); ii++) {
    StTofData *aData = dynamic_cast<StTofData *>(tofData[ii]);
    if(!aData) continue;

    int dataIndex = aData->dataIndex();
    int trayId = dataIndex / mNTOF;
    if(trayId<120) continue;  // only vpd selected
    int ewId = trayId - 120 + 1;  // 1: west,  2: east
    
    int tubeId = dataIndex % mNTOF + 1;
    int lechan = (ewId==1) ? mDaqMap->WestPMT2TDIGLeChan(tubeId) : mDaqMap->EastPMT2TDIGLeChan(tubeId);
    int techan = (ewId==1) ? mDaqMap->WestPMT2TDIGTeChan(tubeId) : mDaqMap->EastPMT2TDIGTeChan(tubeId);

    int tmptdc = aData->leadingTdc();
    int bin = (int)tmptdc&0x3ff;
    double tmptdc_f = tmptdc + mTofINLCorr->getVpdINLCorr(ewId, lechan, bin);
    double letime = tmptdc_f*VHRBIN2PS / 1000.;

    tmptdc = aData->trailingTdc();
    bin = (int)tmptdc&0x3ff;
    tmptdc_f = tmptdc + mTofINLCorr->getVpdINLCorr(ewId, techan, bin);
    double tetime = tmptdc_f*VHRBIN2PS / 1000.;

    StThreeVectorF zero(0.,0.,0.);
    StTofCell *tofCell = new StTofCell(120+ewId, 0, tubeId, lechan, 0, 0, 0, zero);
    tofCell->setLeadingEdgeTime(letime);
    tofCell->setTrailingEdgeTime(tetime);
    mCellCollection->push_back(tofCell);

    if (Debug()){
      LOG_INFO  << "F: itray=" << trayId << " imodule=" << 0 << " itube=" << tubeId << " letime=" << letime << " tetime=" << tetime << endm;
    }
  }

  
  storeMatchData(mCellCollection,theTof);
  delete mCellCollection;

  delete mSortTofRawData;
  mSortTofRawData = 0;
  
  //check StEvent collections --
  if (theTof->dataPresent())
    LOG_INFO  << " TofCollection: raw data container present" << endm;
  if (theTof->cellsPresent()){
    LOG_INFO  << " TofCollection: cell container present."<<endm;
    if (Debug()){
      StSPtrVecTofCell& tmpCellTofVec = theTof->tofCells();
      LOG_INFO  << " # of matched cells " << tmpCellTofVec.size() << endm;
      for (size_t i = 0; i < tmpCellTofVec.size(); i++) {
	StTofCell* p = tmpCellTofVec[i];
	LOG_INFO  << p->trayIndex() << " " << p->moduleIndex() << " " << p->cellIndex() << " " << p->trailingEdgeTime() << " " << p->leadingEdgeTime() << " " << p->associatedTrack() << " " << p->matchFlag() << " " << p->position() << endm;
      }
    }
  }
  //-- end check
  
  LOG_INFO  << "F: before/after" << FinalMatchedCellsVec.size() << "/" <<nValidSinglePrimHitCells << endm;
 // end of Sect.F

  LOG_INFO  << "#(cell tracks): " << allCellsHitVec.size()
       << " #(hit cells): " << FinalMatchedCellsVec.size()
       << "\n#(single hits): " << nSingleHitCells 
       << " #(single valid hits): " << nValidSingleHitCells
       << " #(single prim valid hits): " << nValidSinglePrimHitCells
       << endm;



  if (doPrintMemoryInfo) {
        StMemoryInfo::instance()->snapshot();
        StMemoryInfo::instance()->print();
  }
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO  << "CPU time for StTofrMatchMaker::Make(): "
	 << timer.elapsedTime() << " sec\n" << endm;
  }

  LOG_INFO  << "StTofrMatchMaker -- bye-bye" << endm;



  return kStOK;
}

//---------------------------------------------------------------------------
// store local slat collection in StEvent's tofCollection
Int_t StTofrMatchMaker::storeMatchData(StTofCellCollection *cellCollection,
					 StTofCollection* tofCollection){
  if(!tofCollection){
    LOG_INFO  << "Error: no TofCollection -- returning" << endm;
    return kStErr;
  }

  for (size_t j=0;j<cellCollection->size();j++){
    tofCollection->addCell(cellCollection->getCell(j)); 
    if (Debug())
      LOG_INFO  << "storing " << j << "  " << "  tray:"
	   << cellCollection->getCell(j)->trayIndex() << "  module:"
	   << cellCollection->getCell(j)->moduleIndex() << "  cell:"
	   << cellCollection->getCell(j)->cellIndex() << endm;
  }
  return kStOK;
}


//---------------------------------------------------------------------------
// create a local copy of the raw tofp data tofData in StEvent's tofCollection
Int_t StTofrMatchMaker::getTofData(StTofCollection* tofCollection){
  if (!tofCollection) return kStERR;
  LOG_INFO  << " Read in tof data ... " << endm;
  StSPtrVecTofData &tofData = tofCollection->tofData();

  // perform consistency check
  bool dataOK(true);
  LOG_INFO  << "TOF raw data consistency test ...";

  Int_t tofsize = tofData.size();
  Int_t nTOFr = 0;
  if(mYear4) {
    if(tofsize<184) {
      gMessMgr->Warning("The size of tofData is NOT 184!","OS");
      nTOFr = 72; // DAQ not updated
    } else {
      nTOFr = mNTOFR;
    }
  } else if(mYear3) {
    nTOFr = 72;
  } else if(mYear2) {
    nTOFr = 0;
  }

  for (Int_t i=0;i<nTOFr;i++){
    Int_t iAdc = mDaqMap->DaqChan2ADCChan(i);
    mTofrAdc[i] = tofData[iAdc]->adc();
    Int_t iTdc = mDaqMap->DaqChan2TDCChan(i);
    mTofrTdc[i] = tofData[iTdc]->tdc();
  }

  for (Int_t i=0;i<6;i++){
    mPvpdAdc[i] = tofData[42+i]->adc(); 
    mPvpdTdc[i] = tofData[42+i]->tdc(); 
    if (mYear3||mYear4)
      mPvpdAdcLoRes[i] = tofData[54+i]->adc();
  }
  
  if (!dataOK) return kStWarn;

  return kStOK;
}


//---------------------------------------------------------------------------
// Book histograms and create ordered collection for easy manipulation
void StTofrMatchMaker::bookHistograms(void){

  mEventCounterHisto = new TH1D("eventCounter","eventCounter",20,0,20);

  mADCTDCCorelation = new TH2D("ADCTDCCorelation","ADCTDCCorelation",200,0,200,200,0,200);
  mCellsMultInEvent = new TH1D("cellsPerEvent","cellsPerEvent",100,0,100);
  mHitsMultInEvent  = new TH1D("hitsPerEvent","hitsPerEvent",100,0,100);
  mHitsMultPerTrack = new TH1D("hitsPerTrack","hitsPerTrack",10,0,10);
  mDaqOccupancy     = new TH1D("daqOccupancy","daqOccupancy",192,0,192);
  mDaqOccupancyValid= new TH1D("daqOccupancyValid","daqOccupancyValid",192,0,192);
  mDaqOccupancyProj = new TH1D("daqOccupancyProj","daqOccupancyProj",192,0,192);
  mHitsPosition     = new TH2D("hitsPosition","hitsPositions",300,-15.,15.,200,-5.,5.);
  
  mDaqOccupancyValidAll = new TH1D("daqOccupancyValidAll","",960,0,960);
  mDaqOccupancyProjAll = new TH1D("daqOccupancyProjAll","",960,0,960);

  mDaqOccupancyVpd = new TH1D("daqOccupancyVpd","",38,0,38);
  mDaqOccupancyValidVpd = new TH1D("daqOccupancyValidVpd","",38,0,38);

  // correlation
  for(int i=0;i<mNValidTrays_Run8;i++) {
    char hisname[100];
    sprintf(hisname,"Tray_%d",i+76);
    mHitCorr[i] = new TH2D(hisname,"",192,0,192,192,0,192);
    sprintf(hisname,"Tray_%d_module",i+76);
    mHitCorrModule[i] = new TH2D(hisname,"",32,0,32,32,0,32);
  }
  mHitCorrAll = new TH2D("Tray_All","",960,0,960,960,0,960);

  // TPC tracks
  if(mSaveTree) {
  mTrackTree = new TTree("track","track");
  mTrackTree->Branch("pt",&trackTree.pt,"pt/F");
  mTrackTree->Branch("eta",&trackTree.eta,"eta/F");
  mTrackTree->Branch("phi",&trackTree.phi,"phi/F");
  mTrackTree->Branch("nfitpts",&trackTree.nfitpts,"nfitpts/I");
  mTrackTree->Branch("dEdx",&trackTree.dEdx,"dEdx/F");
  mTrackTree->Branch("ndEdxpts",&trackTree.ndEdxpts,"ndEdxpts/I");
  mTrackTree->Branch("charge",&trackTree.charge,"charge/I");
  mTrackTree->Branch("projTrayId",&trackTree.projTrayId,"projTrayId/I");
  mTrackTree->Branch("projCellChan",&trackTree.projCellChan,"projCellChan/I");
  mTrackTree->Branch("projY",&trackTree.projY,"projY/F");
  mTrackTree->Branch("projZ",&trackTree.projZ,"projZ/F");
  }

  mTrackPtEta = new TH2D("trackPtEta","",100,0.,5.,60,-1.5,1.5);
  mTrackPtPhi = new TH2D("trackPtPhi","",100,0.,5.,120,0.,2*3.14159);
  mTrackNFitPts = new TH1D("trackNFitPts","",50,0.,50.);
  mTrackdEdxvsp = new TH2D("trackdEdxvsp","",500,0.,5.,1000,0.,10.);
  mNSigmaPivsPt = new TH2D("nSigmaPivsPt","",500,0.,5.,1000,-10.,10.);

  // primary association  
  mCellsPerEventMatch1 = new TH1D("cellsPerEventMatch1","cellPerEventMatch1",100,0,100);
  mHitsPerEventMatch1 = new TH1D("hitsPerEventMatch1","hitsPerEventMatch1",100,0,100);
  mCellsPerTrackMatch1 = new TH1D("cellsPerTrackMatch1","cellsPerTrackMatch1",100,0,100);
  mTracksPerCellMatch1 = new TH1D("tracksPerCellMatch1","tracksPerCellMatch1",100,0,100);
  mDaqOccupancyMatch1 = new TH1D("daqOccupancyMatch1","daqOccupancyMatch1",192,0,192);
  mDeltaHitMatch1 = new TH2D("deltaHitMatch1","deltaHitMatch1",300,-15,15,200,-5.,5.);

  // kick out multi-hit
  mCellsPerEventMatch2 = new TH1D("cellsPerEventMatch2","cellPerEventMatch2",100,0,100);
  mHitsPerEventMatch2 = new TH1D("hitsPerEventMatch2","hitsPerEventMatch2",100,0,100);
  mCellsPerTrackMatch2 = new TH1D("cellsPerTrackMatch2","cellsPerTrackMatch2",100,0,100);
  mTracksPerCellMatch2 = new TH1D("tracksPerCellMatch2","tracksPerCellMatch2",100,0,100);
  mDaqOccupancyMatch2 = new TH1D("daqOccupancyMatch2","daqOccupancyMatch2",192,0,192);
  mDeltaHitMatch2 = new TH2D("deltaHitMatch2","deltaHitMatch2",300,-15,15,200,-5.,5.);

  // sort out multi matched cells
  mCellsPerEventMatch3 = new TH1D("cellsPerEventMatch3","cellPerEventMatch3",100,0,100);
  mHitsPerEventMatch3 = new TH1D("hitsPerEventMatch3","hitsPerEventMatch3",100,0,100);
  mCellsPerTrackMatch3 = new TH1D("cellsPerTrackMatch3","cellsPerTrackMatch3",100,0,100);
  mTracksPerCellMatch3 = new TH1D("tracksPerCellMatch3","tracksPerCellMatch3",100,0,100);
  mDaqOccupancyMatch3 = new TH1D("daqOccupancyMatch3","daqOccupancyMatch3",192,0,192);
  mDeltaHitMatch3 = new TH2D("deltaHitMatch3","deltaHitMatch3",300,-15,15,200,-5.,5.);

  return;
}


//---------------------------------------------------------------------------
// store histograms in a seperate root file
void StTofrMatchMaker::writeHistogramsToFile(){
  // Output file
  TFile *theHistoFile =  new TFile(mHistoFileName.c_str(), "RECREATE");
  LOG_INFO  << "StTofrMatchMaker::writeHistogramsToFile()"
       << " histogram file " <<  mHistoFileName << endm;

  theHistoFile->cd();

  if(mHisto) {

    mEventCounterHisto->Write();
    mADCTDCCorelation->Write();
    mCellsMultInEvent->Write();
    mHitsMultInEvent->Write();
    mHitsMultPerTrack->Write();
    mDaqOccupancy->Write();
    mDaqOccupancyValid->Write();
    mDaqOccupancyProj->Write();
    mHitsPosition->Write();

    mDaqOccupancyValidAll->Write();
    mDaqOccupancyProjAll->Write();
    mDaqOccupancyVpd->Write();
    mDaqOccupancyValidVpd->Write();
    
    for(int i=0;i<mNValidTrays_Run8;i++) {
      mHitCorr[i]->Write();
      mHitCorrModule[i]->Write();
    }
    mHitCorrAll->Write();

    mTrackPtEta->Write();
    mTrackPtPhi->Write();
    mTrackNFitPts->Write();
    mTrackdEdxvsp->Write();
    mNSigmaPivsPt->Write();

    mCellsPerEventMatch1->Write();
    mHitsPerEventMatch1->Write();
    mCellsPerTrackMatch1->Write();
    mTracksPerCellMatch1->Write();
    mDaqOccupancyMatch1->Write();
    mDeltaHitMatch1->Write();
    
    mCellsPerEventMatch2->Write();
    mHitsPerEventMatch2->Write();
    mCellsPerTrackMatch2->Write();
    mTracksPerCellMatch2->Write();
    mDaqOccupancyMatch2->Write();
    mDeltaHitMatch2->Write();

    mCellsPerEventMatch3->Write();
    mHitsPerEventMatch3->Write();
    mCellsPerTrackMatch3->Write();
    mTracksPerCellMatch3->Write();
    mDaqOccupancyMatch3->Write();
    mDeltaHitMatch3->Write();
    
    theHistoFile->Write();  
    theHistoFile->Close(); 
   
  }

  if(mSaveTree) {
    TFile *theTreeFile =  new TFile((mHistoFileName+".tree.root").c_str(), "RECREATE");
    theTreeFile->cd();
    mTrackTree->Write();
    theTreeFile->Write();
    theTreeFile->Close();
  }

  return;
}


//---------------------------------------------------------------------------
// determine pVPD event type (strobe or beam)
bool StTofrMatchMaker::strobeEvent(StSPtrVecTofData& tofData){
  // determine strobe event from pVPD TDC data

  Int_t nStrobedPvpdTdcs=0;
  for(Int_t i=0;i<mNPVPD;i++)
    if((tofData[42+i]->tdc()>mStrobeTdcMin[i]) &&
       (tofData[42+i]->tdc()<mStrobeTdcMax[i]))
  	nStrobedPvpdTdcs++;
  
  if (nStrobedPvpdTdcs==mNPVPD) return true;

  return false;
}


//---------------------------------------------------------------------------
// determine whether this is a valid TOF beam event
bool StTofrMatchMaker::validEvent(StEvent *event){
  mEventCounter++;
  // 1. must have non-zero pointer
  if (!event) return false;
  if(mHisto) mEventCounterHisto->Fill(1);

  // 2. must have a valid primary vertex 
//  if (!event->primaryVertex()) return false;
  mAcceptedEventCounter++;
  if(mHisto) mEventCounterHisto->Fill(2);

  // 3a. must have TOF collection
  if (!event->tofCollection()){
    LOG_INFO  << "TOF is not present" << endm;
    return false;
  }
  if(mHisto) mEventCounterHisto->Fill(3);

  // 3b. must have TOF raw data available
  if (!(event->tofCollection()->dataPresent()||event->tofCollection()->rawdataPresent())){
    LOG_INFO  << "TOF is present but no Raw Data" << endm;
    if  (!(event->tofCollection()->cellsPresent())){
      LOG_INFO  << "              and no Cell Data" << endm;
    }
    return false;
  }
  mTofEventCounter++;
  if(mHisto) mEventCounterHisto->Fill(4);

  
  
  // 4. must be a TOF beam event, i.e. a non-strobe event
  if(event->tofCollection()->dataPresent()) {
    StSPtrVecTofData  &tofData = event->tofCollection()->tofData();
    LOG_INFO  << " tofData size = " << tofData.size() << endm;
    if (strobeEvent(tofData)){
      mTofStrobeEventCounter++;
      if (event->primaryVertex()) mAcceptAndStrobe++; // keep track of #valid strobed evts
      gMessMgr->Info("strobe event","OTS");
      return false;
    }
  }
  if(mHisto) mEventCounterHisto->Fill(5);
  
  mAcceptAndBeam++;

  // and we have a winner!
  gMessMgr->Info("TOF present ... and valid beam event","OTS");

  return true;
}


//---------------------------------------------------------------------------
// determine whether this is a valid TPC track
bool StTofrMatchMaker::validTrack(StTrack *track){
  // 1. no track, no go.
  if (!track) return false;

  // 2. track quality flag, should be >0
  if (track->flag()<=0) return false;

  // 3. minimum #hits per track
  if (track->topologyMap().numberOfHits(kTpcId) < mMinHitsPerTrack) return false;
  // 4. minimum #fit points per track
  if (track->fitTraits().numberOfFitPoints(kTpcId) < mMinFitPointsPerTrack) return false;
  // 5. minimum #fit points over #maximum points
  float ratio = (1.0*track->fitTraits().numberOfFitPoints(kTpcId)) / (1.0*track->numberOfPossiblePoints(kTpcId));
  if (ratio < mMinFitPointsOverMax) return false;

  return true;
}

//---------------------------------------------------------------------------
// determine whether this is a valid TPC track
bool StTofrMatchMaker::validTrackRun8(StGlobalTrack *track){
  // 1. no track, no go.
  if (!track) return false;

  // 2. track quality flag, should be >0
  if (track->flag()<=0) return false;

  // 3. minimum #hits per track
  if (track->topologyMap().numberOfHits(kTpcId) < mMinHitsPerTrack) return false;
  // 4. minimum #fit points per track
  if (track->fitTraits().numberOfFitPoints(kTpcId) < mMinFitPointsPerTrack) return false;
  // 5. chi2 per track
  float chi2 = track->fitTraits().chi2(0);
  if ( chi2 > 10. ) return false;
  
  // 6. error
  StDcaGeometry *dcaGeom = track->dcaGeometry();
  if(!dcaGeom) return false;
  float pt = dcaGeom->pt();
  float sigma_pT = sqrt(dcaGeom->errMatrix()[9])*pt*pt;
  if ( sigma_pT/pt > 0.3 ) return false;

  return true;
}


//---------------------------------------------------------------------------
// determine whether this is a valid TOF track
bool StTofrMatchMaker::validTofTrack(StTrack *track){
  // select valid tracks for time-of-flight calculations

  // 1. track must exist
  if (!track) return false;

  // 2. track quality flag, should be >0
  if (track->flag()<=0) return false;

  // 3. track must be a primary track
  if (!dynamic_cast<StPrimaryTrack*>(track)) return false;

  // 4. DCA cut (obsolete?)
  Double_t DCA= track->impactParameter();
  Int_t charge = track->geometry()->charge();
  if (DCA > mMaxDCA) {LOG_INFO  << "dca>max:" << DCA<< endm; return false;}
  if (charge==0) { LOG_INFO  << " neutral charge" << endm; return false; }

  return true;
}  


//---------------------------------------------------------------------------
// returns the proper track geometry, based on a global user setting
StTrackGeometry* StTofrMatchMaker::trackGeometry(StTrack* track){
  // returns apropriate StTrackGeometry (standard or outerGeometry)
  if (!track) return 0;
  StTrackGeometry *thisTrackGeometry;
  if (mOuterTrackGeometry)
    thisTrackGeometry = track->outerGeometry();
  else
    thisTrackGeometry = track->geometry();
  return thisTrackGeometry;
}

/*****************************************************************
 *
 * $Log: StTofrMatchMaker.cxx,v $
 * Revision 1.34  2018/02/26 23:26:52  smirnovd
 * StTof: Remove outdated ClassImp macro
 *
 * Revision 1.33  2018/02/26 23:13:21  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.32  2012/12/17 22:57:29  geurts
 * bugfix (tickets #2456/#2457)
 *
 * Revision 1.31  2012/12/14 06:36:04  geurts
 * Changed global database calls to direct table access and/or removed deprecated database access code.
 *
 * Revision 1.30  2009/07/28 16:06:42  geurts
 * Bug Ticket #1591: explicit initialization of mStrobeTdcMin, mStrobeTdcMax, and mPedTOFr.
 *
 * Revision 1.29  2009/07/24 22:42:08  fine
 * replace the deprecated API of the STAR messenger
 *
 * Revision 1.28  2009/07/24 22:33:33  fine
 * Make the code C++ compliant
 *
 * Revision 1.27  2009/06/09 19:45:35  jeromel
 * Changes for BT#1428
 *
 * Revision 1.26  2008/07/23 19:22:03  dongx
 * New track quality cuts for Run8
 *
 * Revision 1.25  2008/06/24 21:58:13  dongx
 * fixed a bug of crashing due to potential empty track in trackNodes
 *
 * Revision 1.24  2008/06/19 16:11:54  dongx
 * fixed bug of filling an undefined histogram in case of production
 *
 * Revision 1.23  2008/05/12 17:16:37  dongx
 * letime and tetime in StTofCell stored in nano-seconds
 *
 * Revision 1.22  2008/05/06 18:41:39  dongx
 * - Fixed bug in ouput histogram filename switch
 * - Added switch for tpc track tree output
 *
 * Revision 1.21  2008/04/23 18:20:15  dongx
 * vpd letime and tetime stored in double precision
 *
 * Revision 1.20  2008/04/22 22:31:22  dongx
 * leadingEdgeTime and trailingEdgeTime stored as double precision in StTofCell
 *
 * Revision 1.19  2008/04/22 20:55:13  dongx
 * leadingEdgeTime and trailingEdgeTime stored as double precision in StTofCell
 *
 * Revision 1.18  2008/03/27 00:16:03  dongx
 *  update for Run8 finished.
 *
 * Revision 1.17  2007/11/29 22:43:11  dongx
 * changed vpd trayId definition to 121 (East) and 122 (West)
 *
 * Revision 1.16  2007/11/28 02:17:08  dongx
 * trayId for vpd in StTofCell: 901 (E), 902 (W)
 * dataIndex for vpd in StTofData:  use 121 and 122 as trayId
 *
 * Revision 1.15  2007/11/22 00:22:37  dongx
 * update for run8 - first version
 *
 * Revision 1.14  2007/04/17 23:02:20  dongx
 * replaced with standard STAR Loggers
 *
 * Revision 1.13  2007/02/28 23:31:59  dongx
 * completion for Run V matching
 *   - trailing tdc and leading tdc stored as adc and tdc in StTofCell
 *   - multi-hit association cases use hit position priority
 *
 * Revision 1.12  2005/04/12 17:31:56  dongx
 * update for year 5 data - not completed, leave as empty at present
 *
 * Revision 1.11  2004/08/11 18:57:44  dongx
 * deltay quality cut applied
 *
 * Revision 1.10  2004/08/10 19:25:13  dongx
 * updated to be compatible with RunII data
 *
 * Revision 1.9  2004/07/13 16:12:32  dongx
 * continuing update for the DAQ array in Run IV
 *
 * Revision 1.8  2004/07/12 17:21:19  dongx
 * fix the crashing when running events before day 030 (TofDaq not updated to 184 yet)
 *
 * Revision 1.7  2004/05/03 23:08:50  dongx
 * change according to the update of StTofrGeometry, save CPU time by 100 times
 *
 * Revision 1.5  2004/04/01 20:08:54  dongx
 * fix a bug about the hit position stored in TofCell
 *
 * Revision 1.4  2004/03/16 22:30:49  dongx
 * fix the warning message when compiling
 *
 * Revision 1.3  2004/03/11 22:30:34  dongx
 * -move m_Mode control to Init()
 * -clear up
 *
 * Revision 1.2  2004/03/09 17:44:56  dongx
 * first release
 *
 */

/*******************************************************************
 *
 * $Id: StTofrMatchMaker.cxx,v 1.17 2007/11/29 22:43:11 dongx Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: Tofr Match Maker to do the matching between the 
 *              fired celles and TPC tracks ( similar to  Frank's
 *              TofpMatchMaker )
 *
 *****************************************************************
 *
 * $Log: StTofrMatchMaker.cxx,v $
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
 *
 *******************************************************************/
#include <iostream>
#include "StEventTypes.h"
#include "Stypes.h"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"
#include "StHelix.hh"
#include "StTrackGeometry.h"
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
#include "StMessMgr.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"
#include "StTofrMatchMaker.h"
//#include "TMemStat.h"

ClassImp(StTofrMatchMaker)

//---------------------------------------------------------------------------
StTofrMatchMaker::StTofrMatchMaker(const Char_t *name): StMaker(name){
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
  setMinHitsPerTrack(0);
  setMinFitPointsPerTrack(0);
  setMaxDCA(9999.);

  setCreateHistoFlag(kFALSE);
  setSaveGeometry(kFALSE);
  doPrintMemoryInfo = kFALSE;
  doPrintCpuInfo    = kFALSE;
}

StTofrMatchMaker::~StTofrMatchMaker(){ /* nope */}

//void StTofrMatchMaker::Clear(Option_t *opt){StMaker::Clear();}

//---------------------------------------------------------------------------
Int_t StTofrMatchMaker::Init(){
  gMessMgr->Info("StTofrMatchMaker -- initializing ...","OS");
  gMessMgr->Info("","OS") << "Valid TDC range: " << mMinValidTdc << " " << mMaxValidTdc << endm;
  gMessMgr->Info("","OS") << "Valid ADC range: " << mMinValidAdc << " " << mMaxValidAdc << endm;
  gMessMgr->Info("","OS") << "Minimum hits per track: " << mMinHitsPerTrack << endm;
  gMessMgr->Info("","OS") << "Minimum fitpoints per track: " << mMinFitPointsPerTrack << endm;
  gMessMgr->Info("","OS") << "Maximum DCA: " << mMaxDCA << endm;
  if (!mOuterTrackGeometry)
    gMessMgr->Warning("Warning: using standard trackgeometry()","OS");
  
  // m_Mode can be set by SetMode() method
  if(m_Mode) {
    setHistoFileName("tofana.root");
  } else {
    setHistoFileName("");
  }

  if (mHisto){
    bookHistograms();
    gMessMgr->Info("","OS") << "Histograms are booked" << endm;
    if (mHistoFileName!="")
      gMessMgr->Info("","OS") << "Histograms will be stored in " << mHistoFileName.c_str() << endm;
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
    gMessMgr->Info("","OS") << " # of trays = " << topNode->GetListSize() << endm;
    TList *list = topNode->Nodes();
    Int_t ibtoh =0;
    TVolumeView *sectorVolume = 0;
    for(Int_t i=0;i<list->GetSize();i++) {
      sectorVolume = dynamic_cast<TVolumeView*> (list->At(i));
      if ( i>=60 ) ibtoh = 1;
      gMessMgr->Info("","OS") << " test sector size = " <<sectorVolume->GetListSize() << endm;
      StTofrGeomTray *tray = new StTofrGeomTray(ibtoh, sectorVolume, topNode);
      tray->Print();
      TVolumeView *trayVolume = tray->GetfView();
      TList *list1 = trayVolume->Nodes();
      gMessMgr->Info("","OS") << "   # of modules in tray " << tray->Index() << " = " << trayVolume->GetListSize() << endm;
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
  if(mYear2||mYear3||mYear4) {
    mDaqMap->init(this);
    //  AddConst(new TObjectSet("tofrDaqMap",mDaqMap));
    gMessMgr->Info("","OS") << " Initialize Daq map for run 2,3,4 ... " << endm;
    
    if(Debug()) {
      for(Int_t i=0;i<mNTOFR;i++) {
	gMessMgr->Info("","OS") << i << " -- adc=" << mDaqMap->DaqChan2ADCChan(i) << " -- tdc=" << mDaqMap->DaqChan2TDCChan(i) << endm;
	IntVec map = mDaqMap->DaqChan2Cell(i);
	gMessMgr->Info("","OS") << "    tray=" << map[0] << "  module=" << map[1] << "  cell=" << map[2] << endm;
      }
    }
    
    ///////////////////////////////////////////////////////
      // Load configuration parameters from dbase
      //    need "[shell] setenv Calibrations_tof reconV0"
      ///////////////////////////////////////////////////////
      gMessMgr->Info("    -- retrieving run parameters from Calibrations_tof","OS");
      TDataSet *mDbDataSet = GetDataBase("Calibrations/tof");
      if (!mDbDataSet){
	gMessMgr->Error("unable to get TOF run parameters","OS");
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
	  gMessMgr->Info("","OS") << "tube " << strobeDef[i].id << "  min:"<< strobeDef[i].strobeTdcMin
				  <<" max:"<< strobeDef[i].strobeTdcMax<< endm;
      }
      
      //TOFr pedestals
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
	  gMessMgr->Info("","OS") << "daqChannel" << ii << " ped:" << pedestal[0].adcPedestal[i] <<endm;
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
      gMessMgr->Info("","OS") <<  "   " << iTray << " tray has " << nModules << " modules " << endm;
      }
      */
      
  } else if(mYear5) {
    mDaqMap->initFromDbaseY5(this);
    gMessMgr->Info("","OS") << " Initialize Daq map for run 5 ... " << endm;
    
    if(Debug()) {
      //    if(0) {
      for(Int_t i=0;i<mNTOFR5;i++) {
	IntVec map = mDaqMap->Tofr5TDCChan2Cell(i);
	gMessMgr->Info("","OS") << "InitRun():i="<<i<<" tray=" << map[0] << "  module=" << map[1] << "  cell=" << map[2] << endm;
      }
    }
    
    /* move INL calibration to StTofCalibMaker
    // read INL table
    gMessMgr->Info("","OS") << "read in INL table" << endm;
    TDataSet *mDbTOFDataSet = this->GetDataBase("Calibrations/tof");
    if(!mDbTOFDataSet) {
      gMessMgr->Error("unable to access Calibrations TOF parameters","OS");
      //    assert(mDbTOFDataSet);
      return kStErr;
    }
    
    St_tofr5INLtable* tofr5INLtable = static_cast<St_tofr5INLtable*>(mDbTOFDataSet->Find("tofr5INLtable"));
    if(!tofr5INLtable) {
      gMessMgr->Error("unable to get tofr5 INL table parameters","OS");
      //    assert(tofr5INLtable);
      return kStErr;
    }
    tofr5INLtable_st* inltable = static_cast<tofr5INLtable_st*>(tofr5INLtable->GetArray());
    Int_t numRows = tofr5INLtable->GetNRows();
    
    gMessMgr->Info("","OS") << " Number of rows read in: " << numRows << endm;
    
    Char_t *boardName;
    Short_t boardId;
    Short_t tdcId;
    Float_t INLcorr[1024];
    
    for (Int_t i=0;i<numRows;i++) {
      boardName = "";
      boardId = -1;
      tdcId = -1;
      for(int j=0;j<mTdcChannel;j++) {
	INLcorr[j] = 0.0;
      }
      
      boardName = (Char_t *)(inltable[i].boardID);
      boardId = inltable[i].boardNumber;
      tdcId = inltable[i].TDCID;
      if(Debug())
	gMessMgr->Info("","OS") << " name = " << boardName << " bId = " << boardId << " tdcId = " << tdcId << endm;
      for(int j=0;j<mTdcChannel;j++) {
	INLcorr[j] = inltable[i].INLcorrection[j];
	if(Debug()&&j%100==0) gMessMgr->Info("","OS") << " j=" << j << " inlcorr=" << INLcorr[j] << endm;
	mINLtable[boardId][tdcId][j] = INLcorr[j];
      }
            
    }
    */

  } else if(mYear8) {
    mDaqMap->initFromDbaseGeneral(this);
    gMessMgr->Info("","OS") << " Initialize Daq map for run 8 ... " << endm;

    mTofINLCorr->initFromDbase(this);
    gMessMgr->Info("","OS") << " Initialize INL table for run 8 ... " << endm;

  }
  
  return kStOK;
}

//----------------------------------------------------------------------------
Int_t StTofrMatchMaker::FinishRun(Int_t runnumber){

  gMessMgr->Info("","OS") << "StTofrMatchMaker -- cleaning up geometry (FinishRun)" << endm;
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

  gMessMgr->Info("","OS") << "StTofrMatchMaker -----  RUN SUMMARY ----- (Finish)\n"
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
    gMessMgr->Info("","OS") << " Tofr #Adc:" << nAdcTofr << "   #Tdc:" << nTdcTofr << endm;

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
    gMessMgr->Info("","OS") << " pVPD #Adc:" << nAdcPvpd << "   #Tdc:" << nTdcPvpd << endm;


    
  // number of primary tracks
  //  (note: different meaning in event.root and richtof.root)
  Int_t refmult(0);
  bool richTofMuDST = (mEvent->summary()->numberOfExoticTracks() == -999);
  if (richTofMuDST)
    refmult = mEvent->summary()->numberOfGoodTracks();
  else
    refmult = uncorrectedNumberOfPrimaries(*mEvent);

  if (Debug()){
    gMessMgr->Info("","OS") << " #Tracks           :"      << mEvent->summary()->numberOfTracks()
	 << "\n #goodPrimaryTracks:"    << mEvent->summary()->numberOfGoodPrimaryTracks()
	 << "\n #uncorr.prim.tracks  :" << refmult << endm;
    if (!richTofMuDST)
      gMessMgr->Info("","OS") << " #goodTracks (global):"  << mEvent->summary()->numberOfGoodTracks() << endm;
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
	gMessMgr->Info("","OS") <<"A: daqId=" << i << "  rawAdc= " << rawAdc << " rawTdc="<< rawTdc <<endm;
      }
    }
  }
  // end of Sect.A
  if(Debug()) {
    gMessMgr->Info("","OS") << "    total # of cells = " << daqCellsHitVec.size() << endm;
    for(size_t iv = 0;iv<validModuleVec.size();iv++) {
      gMessMgr->Info("","OS") << " module # " << validModuleVec[iv] << " Valid! " << endm;
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
		gMessMgr->Info("","OS") <<"B: nodeid=" << iNode << "  projected in " << " tray="<< itray << " module="<<imodule<<" cell="<<icell<<endm;
		gMessMgr->Info("","OS") <<"   hit position " << hitPos << endm;
	      }
	    }
	} // for (Int_t i=0...)
      } // endif(helixcross...)
      if(ncells>0&&mHisto) mHitsMultPerTrack->Fill(ncells);

    } // if(ValidTrack).. 
  } // loop over nodes
  if(Debug())
    gMessMgr->Info("","OS") << "B:  matched/available/total #tracknodes: " <<allCellsHitVec.size() << "/" <<nAllTracks << "/" << nodes.size() << endm;
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
    gMessMgr->Info("","OS") << "C: before/after: " << allCellsHitVec.size() << "/" << matchHitCellsVec.size() << endm;
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
      gMessMgr->Info("","OS") << "D: no tracks extrapolate to matched cell ... should not happen!" << endm;
    }
    
    if (Debug()) {
      gMessMgr->Info("","OS") << "D: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
      idVectorIter ij=trackIdVec.begin();
      while (ij != trackIdVec.end()) { gMessMgr->Info("","OS") << " " << *ij; ij++; }
      gMessMgr->Info("","OS") <<endm;
    }
    
    tempVec = erasedVec;
  }
  if(Debug())
    gMessMgr->Info("","OS") << "D: before/after: " << matchHitCellsVec.size() << "/" << singleHitCellsVec.size() << endm;
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
	gMessMgr->Info("","OS") << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
	idVectorIter ij=vTrackId.begin();
	while (ij != vTrackId.end()) { gMessMgr->Info("","OS") << " " << *ij; ij++; }
	gMessMgr->Info("","OS") <<endm;
      }
    }
    else if (nCells>1){   // for multiple hit cells  find the most likely candidate.
      Int_t thiscandidate(-99);
      Int_t thisMatchFlag(0);

      // 1. sort on adc weight
      Float_t weight(0);
      vector<Int_t> weightCandidates;
      thisMatchFlag = 1;
      if (Debug()) gMessMgr->Info("","OS") << "E: find ... weight ";
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
	if (Debug()) gMessMgr->Info("","OS") << "candidate =" << daqId << endm;
      }

      // 2. if still undecided check on hitposition
      if (weightCandidates.size()>1){
	Float_t ss(99.);
	vector<Int_t> ssCandidates;
	thisMatchFlag = 2;
	if (Debug()) gMessMgr->Info("","OS") << " ss ";
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
    	  if (Debug()) gMessMgr->Info("","OS") << "candidate =" << daqId << endm;
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
	  gMessMgr->Info("","OS") << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:" << vTrackId[thiscandidate] << endm;
	}
      }

    } else {
      gMessMgr->Info("","OS") << "E: no cells belong to this track ... should not happen!" << endm;
    }

    tempVec = erasedVec;
  }

  gMessMgr->Info("","OS") << "E: before/after: " << singleHitCellsVec.size() << "/" << FinalMatchedCellsVec.size() << endm;
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
      gMessMgr->Info("","OS") << "F: WHAT!?!  mult.matched cell in single cell list " << daqId << endm;

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
	gMessMgr->Info("","OS") << "F: itray=" << tray << " imodule=" << module << " icell=" << cell << "\tnodeid:";
	idVectorIter ij=FinalMatchedCellsVec[ii].trackIdVec.begin();
	while (ij != FinalMatchedCellsVec[ii].trackIdVec.end()) { gMessMgr->Info("","OS") << " " << *ij; ij++; }
	gMessMgr->Info("","OS") << "\tR=" << 1/(theTrackGeometry->helix().curvature())
	     << "\tpT=" << momentum.perp() << "\tp=" << momentum.mag()
	     << "\thits="<< nHitsPerTrack << "\ts="<< pathLength
	     << "\t#fitp=" <<theTrack->fitTraits().numberOfFitPoints(kTpcId)
	  //	     << "\t#trkp=" <<theTrack->detectorInfo()->numberOfPoints(kTpcId)
	     << " \tdedx=" << dedx
	     << " \tdca="<< globalTrack->geometry()->helix().distance(mEvent->primaryVertex()->position())<<" and "<<theTrackGeometry->helix().distance(mEvent->primaryVertex()->position());
	if (cherang!=0) LOG_INFO  << " \trich="<< cherang << " (" << cherang_nph << ")";
	gMessMgr->Info("","OS") << endm;
      }

    } // track exists 
  }
  
  storeMatchData(mCellCollection,theTof);
  delete mCellCollection;
  
  //check StEvent collections --
  if (theTof->dataPresent())
    gMessMgr->Info("","OS") << " TofCollection: raw data container present" << endm;
  if (theTof->cellsPresent()){
    gMessMgr->Info("","OS") << " TofCollection: cell container present."<<endm;
    if (Debug()){
      StSPtrVecTofCell& tmpCellTofVec = theTof->tofCells();
      for (size_t i = 0; i < tmpCellTofVec.size(); i++) {
	StTofCell* p = tmpCellTofVec[i];
	gMessMgr->Info("","OS") << p->trayIndex() << " " << p->moduleIndex() << " " << p->cellIndex() << " " << p->adc() << " " << p->tdc() << " " << p->associatedTrack() << " " << p->matchFlag() << " " << p->position() << endm;
      }
    }
  }
  //-- end check
  
  gMessMgr->Info("","OS") << "F: before/after" << FinalMatchedCellsVec.size() << "/" <<nValidSinglePrimHitCells << endm;
 // end of Sect.F

  gMessMgr->Info("","OS") << "#(cell tracks): " << allCellsHitVec.size()
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
    gMessMgr->Info("","OS") << "CPU time for StTofrMatchMaker::Make(): "
	 << timer.elapsedTime() << " sec\n" << endm;
  }

  gMessMgr->Info("","OS") << "StTofrMatchMaker -- bye-bye" << endm;
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
    gMessMgr->Info("","OS") << " No hits in TOF or pVPD! " << endm;
    // return kStOK;
  } else {
    gMessMgr->Info("","OS") << " Number of TOF+pVPD fired hits: " << validchannel.size() << endm;
  }
  if(Debug()) {
    for(size_t iv=0;iv<validchannel.size();iv++) {
      //if(validchannel[iv]<0||validchannel[iv]>=mNTOFR5) continue; // skip pvpd
      gMessMgr->Info("","OS") << " channel = " << validchannel[iv]<<endm;
      IntVec leTdc = mSortTofRawData->GetLeadingTdc(validchannel[iv]);
      IntVec teTdc = mSortTofRawData->GetTrailingTdc(validchannel[iv]);
      for(size_t iv1=0;iv1<leTdc.size();iv1++) {
        gMessMgr->Info("","OS") << " leading Tdc = " << leTdc[iv1]<<endm;
      }
      for(size_t iv2=0;iv2<teTdc.size();iv2++) {
        gMessMgr->Info("","OS") << " trailing Tdc = " << teTdc[iv2] << endm;
      }
    }
  }   // end mdebug 
    
  // number of primary tracks
  //  (note: different meaning in event.root and richtof.root)
  Int_t refmult(0);
  refmult = uncorrectedNumberOfPrimaries(*mEvent);
  
  if (Debug()){
    gMessMgr->Info("","OS") << " #Tracks           :"      << mEvent->summary()->numberOfTracks()
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
    gMessMgr->Info("","OS") << "    total # of cells = " << daqCellsHitVec.size() << endm;
    for(size_t iv = 0;iv<validModuleVec.size();iv++) {
      gMessMgr->Info("","OS") << " module # " << validModuleVec[iv] << " Valid! " << endm;
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
		gMessMgr->Info("","OS") <<"B: nodeid=" << iNode << "  projected in " << " tray="<< itray << " module="<<imodule<<" cell="<<icell<<endm;
		gMessMgr->Info("","OS") <<"   hit position " << hitPos << endm;
	      }
	      //	    }
	} // for (Int_t i=0...)
      } // endif(helixcross...)
      if(ncells>0&&mHisto) mHitsMultPerTrack->Fill(ncells);

    } // if(ValidTrack).. 
  } // loop over nodes
  if(Debug())
    gMessMgr->Info("","OS") << "B:  matched/available/total #tracknodes: " <<allCellsHitVec.size() << "/" <<nAllTracks << "/" << nodes.size() << endm;
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
    gMessMgr->Info("","OS") << "C: before/after: " << allCellsHitVec.size() << "/" << matchHitCellsVec.size() << endm;
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
      gMessMgr->Info("","OS") << "D: no tracks extrapolate to matched cell ... should not happen!" << endm;
    }
    
    if (Debug()) {
      gMessMgr->Info("","OS") << "D: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
      idVectorIter ij=trackIdVec.begin();
      while (ij != trackIdVec.end()) { gMessMgr->Info("","OS") << " " << *ij; ij++; }
      gMessMgr->Info("","OS") <<endm;
    }
    
    tempVec = erasedVec;
  }
  if(Debug())
    gMessMgr->Info("","OS") << "D: before/after: " << matchHitCellsVec.size() << "/" << singleHitCellsVec.size() << endm;
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
	gMessMgr->Info("","OS") << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
	idVectorIter ij=vTrackId.begin();
	while (ij != vTrackId.end()) { gMessMgr->Info("","OS") << " " << *ij; ij++; }
	gMessMgr->Info("","OS") <<endm;
      }
    }
    else if (nCells>1){   // for multiple hit cells  find the most likely candidate.
      Int_t thiscandidate(-99);
      Int_t thisMatchFlag(0);

      // sort on hitposition
      Float_t ss(99.);
      vector<Int_t> ssCandidates;
      thisMatchFlag = 2;
      if (Debug()) gMessMgr->Info("","OS") << " ss " << endm;
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
	if (Debug()) gMessMgr->Info("","OS") << "candidate =" << daqId << endm;
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
	gMessMgr->Info("","OS") << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:" << vTrackId[thiscandidate] << endm;
	}
      }

    } else {
      gMessMgr->Info("","OS") << "E: no cells belong to this track ... should not happen!" << endm;
    }

    tempVec = erasedVec;
  }

  gMessMgr->Info("","OS") << "E: before/after: " << singleHitCellsVec.size() << "/" << FinalMatchedCellsVec.size() << endm;
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
      gMessMgr->Info("","OS") << "F: WHAT!?!  mult.matched cell in single cell list " << daqId << endm;


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
	gMessMgr->Info("","OS") << "F: itray=" << tray << " imodule=" << module << " icell=" << cell << "\tnodeid:";
	idVectorIter ij=FinalMatchedCellsVec[ii].trackIdVec.begin();
	while (ij != FinalMatchedCellsVec[ii].trackIdVec.end()) { gMessMgr->Info("","OS") << " " << *ij; ij++; }
	gMessMgr->Info("","OS") << "\tR=" << 1/(theTrackGeometry->helix().curvature())
	     << "\tpT=" << momentum.perp() << "\tp=" << momentum.mag()
	     << "\thits="<< nHitsPerTrack << "\ts="<< pathLength
	     << "\t#fitp=" <<theTrack->fitTraits().numberOfFitPoints(kTpcId)
	  //	     << "\t#trkp=" <<theTrack->detectorInfo()->numberOfPoints(kTpcId)
	     << " \tdedx=" << dedx
	     << " \tdca="<< globalTrack->geometry()->helix().distance(mEvent->primaryVertex()->position())<<" and "<<theTrackGeometry->helix().distance(mEvent->primaryVertex()->position());
	if (cherang!=0) LOG_INFO  << " \trich="<< cherang << " (" << cherang_nph << ")";
	gMessMgr->Info("","OS") << endm;
      }

    } // track exists 
  } // end final matched cells
  
  storeMatchData(mCellCollection,theTof);
  delete mCellCollection;
  
  //check StEvent collections --
  if (theTof->dataPresent())
    gMessMgr->Info("","OS") << " TofCollection: raw data container present" << endm;
  if (theTof->cellsPresent()){
    gMessMgr->Info("","OS") << " TofCollection: cell container present."<<endm;
    if (Debug()){
      StSPtrVecTofCell& tmpCellTofVec = theTof->tofCells();
      gMessMgr->Info("","OS") << " # of matched cells " << tmpCellTofVec.size() << endm;
      for (size_t i = 0; i < tmpCellTofVec.size(); i++) {
	StTofCell* p = tmpCellTofVec[i];
	gMessMgr->Info("","OS") << p->trayIndex() << " " << p->moduleIndex() << " " << p->cellIndex() << " " << p->trailingEdgeTime() << " " << p->leadingEdgeTime() << " " << p->associatedTrack() << " " << p->matchFlag() << " " << p->position() << endm;
      }
    }
  }
  //-- end check
  
  gMessMgr->Info("","OS") << "F: before/after" << FinalMatchedCellsVec.size() << "/" <<nValidSinglePrimHitCells << endm;
 // end of Sect.F

  gMessMgr->Info("","OS") << "#(cell tracks): " << allCellsHitVec.size()
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
    gMessMgr->Info("","OS") << "CPU time for StTofrMatchMaker::Make(): "
	 << timer.elapsedTime() << " sec\n" << endm;
  }

  gMessMgr->Info("","OS") << "StTofrMatchMaker -- bye-bye" << endm;



  return kStOK;
}

//---------------------------------------------------------------------------
Int_t StTofrMatchMaker::processEventYear8(){
  // leave as empty now

  if(mHisto) mEventCounterHisto->Fill(0);
  // event selection ...
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (!validEvent(mEvent)){
    gMessMgr->Info("StTofrMatchMaker -- nothing to do ... bye-bye","OS");
    return kStOK;
  }

  // number of primary tracks
  //  (note: different meaning in event.root and richtof.root)
  Int_t refmult(0);
  refmult = uncorrectedNumberOfPrimaries(*mEvent);
  
  if (Debug()){
    gMessMgr->Info("","OS") << " #Tracks           :"      << mEvent->summary()->numberOfTracks()
	 << "\n #goodPrimaryTracks:"    << mEvent->summary()->numberOfGoodPrimaryTracks()
	 << "\n #uncorr.prim.tracks  :" << refmult << endm;
  }

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

  mSortTofRawData = new StSortTofRawData(theTof);

  // multi-tray system
  IntVec validtray = mDaqMap->ValidTrays();
  for(size_t i=0;i<validtray.size();i++) {
    int trayId = validtray[i];
    IntVec validchannel = mSortTofRawData->GetValidChannel(trayId);
    if(Debug()) gMessMgr->Info("","OS") << " Number of fired hits on tray " << trayId << " = " << validchannel.size() << endm;

    for(size_t iv=0;iv<validchannel.size();iv++) {
      IntVec leTdc = mSortTofRawData->GetLeadingTdc(trayId, validchannel[iv]);
      IntVec teTdc = mSortTofRawData->GetTrailingTdc(trayId, validchannel[iv]);

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
      
      //
      // store data from trays and vpds into StTofData
      //
      int dataIndex = (trayId-1)*mNTOF + (moduleId-1)*mNCell + (cellId-1);
      StTofData *aData = new StTofData(dataIndex,0,0,0,0,leTdc[0],teTdc[0]);
      theTof->addData(aData);

      if(Debug()) {
	for(size_t iv1=0;iv1<leTdc.size();iv1++) {
	  gMessMgr->Info("","OS") << " leading Tdc = " << leTdc[iv1]<<endm;
	}
	for(size_t iv2=0;iv2<teTdc.size();iv2++) {
	  gMessMgr->Info("","OS") << " trailing Tdc = " << teTdc[iv2] << endm;
	}
      } // end debug
    } // end channel

  } // end tray

  // vpd -> StTofData
  for(int i=0;i<2;i++) { // east and west sides
    int trayId = (i==0) ? mEastVpdTrayId : mWestVpdTrayId;
    IntVec validtube = mSortTofRawData->GetValidChannel(trayId);
    if(Debug()) gMessMgr->Info("","OS") << " Number of fired hits on tray(vpd) " << trayId << " = " << validtube.size() << endm;

    if(!validtube.size()) continue;
    for(int i=0;i<mNVPD;i++) {
      int tubeId = i+1;
      int lechan = mDaqMap->PMT2TDIGLeChan(tubeId);
      int techan = mDaqMap->PMT2TDIGTeChan(tubeId);
      IntVec leTdc = mSortTofRawData->GetLeadingTdc(trayId, lechan);
      IntVec teTdc = mSortTofRawData->GetTrailingTdc(trayId, techan);

      if(leTdc.size() && teTdc.size()) {
	int dataIndex = (i+120)*mNTOF + (tubeId-1);
	StTofData *aData = new StTofData(dataIndex,0,0,0,0,leTdc[0],teTdc[0]);
	theTof->addData(aData);	
      }
    }
  }
  //

  // end of Sect.A
  if(Debug()) {
    gMessMgr->Info("","OS") << "    total # of cells = " << daqCellsHitVec.size() << endm;
    for(size_t iv = 0;iv<validModuleVec.size();iv++) {
      gMessMgr->Info("","OS") << " module # " << validModuleVec[iv] << " Valid! " << endm;
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
		mDaqOccupancyProj->Fill(Iarray);
		mHitsPosition->Fill(hitPos.y(), hitPos.z());
	      }
	      
	      if(Debug()) {
		gMessMgr->Info("","OS") <<"B: nodeid=" << iNode << "  projected in " << " tray="<< itray << " module="<<imodule<<" cell="<<icell<<endm;
		gMessMgr->Info("","OS") <<"   hit position " << hitPos << endm;
	      }
	      //	    }
	} // for (Int_t i=0...)
      } // endif(helixcross...)
      if(ncells>0&&mHisto) mHitsMultPerTrack->Fill(ncells);

    } // if(ValidTrack).. 
  } // loop over nodes
  if(Debug())
    gMessMgr->Info("","OS") << "B:  matched/available/total #tracknodes: " <<allCellsHitVec.size() << "/" <<nAllTracks << "/" << nodes.size() << endm;
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
    gMessMgr->Info("","OS") << "C: before/after: " << allCellsHitVec.size() << "/" << matchHitCellsVec.size() << endm;
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
      gMessMgr->Info("","OS") << "D: no tracks extrapolate to matched cell ... should not happen!" << endm;
    }
    
    if (Debug()) {
      gMessMgr->Info("","OS") << "D: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
      idVectorIter ij=trackIdVec.begin();
      while (ij != trackIdVec.end()) { gMessMgr->Info("","OS") << " " << *ij; ij++; }
      gMessMgr->Info("","OS") <<endm;
    }
    
    tempVec = erasedVec;
  }
  if(Debug())
    gMessMgr->Info("","OS") << "D: before/after: " << matchHitCellsVec.size() << "/" << singleHitCellsVec.size() << endm;
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
	gMessMgr->Info("","OS") << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
	idVectorIter ij=vTrackId.begin();
	while (ij != vTrackId.end()) { gMessMgr->Info("","OS") << " " << *ij; ij++; }
	gMessMgr->Info("","OS") <<endm;
      }
    }
    else if (nCells>1){   // for multiple hit cells  find the most likely candidate.
      Int_t thiscandidate(-99);
      Int_t thisMatchFlag(0);

      // sort on hitposition
      Float_t ss(99.);
      vector<Int_t> ssCandidates;
      thisMatchFlag = 2;
      if (Debug()) gMessMgr->Info("","OS") << " ss " << endm;
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
	if (Debug()) gMessMgr->Info("","OS") << "candidate =" << daqId << endm;
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
	gMessMgr->Info("","OS") << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:" << vTrackId[thiscandidate] << endm;
	}
      }

    } else {
      gMessMgr->Info("","OS") << "E: no cells belong to this track ... should not happen!" << endm;
    }

    tempVec = erasedVec;
  }

  gMessMgr->Info("","OS") << "E: before/after: " << singleHitCellsVec.size() << "/" << FinalMatchedCellsVec.size() << endm;
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
      gMessMgr->Info("","OS") << "F: WHAT!?!  mult.matched cell in single cell list " << daqId << endm;


    // Read in Leading and Trailing edge TDC, apply on INL correction
    //
    int tmptdc = (mSortTofRawData->GetLeadingTdc(tray,jj))[0];
    int bin = (int)tmptdc&0x3ff;
    float tmptdc_f = tmptdc + mTofINLCorr->getTrayINLCorr(tray, jj, bin);
    float letime = tmptdc_f*VHRBIN2PS;

    tmptdc=(mSortTofRawData->GetTrailingTdc(tray,jj))[0];
    bin = (int)tmptdc&0x3ff;
    tmptdc_f = tmptdc + mTofINLCorr->getTrayINLCorr(tray, jj, bin);
    float tetime = tmptdc_f*VHRBIN2PS;

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
      StTofCell *tofCell = new StTofCell(tray, module, cell, daqId, theTrack, FinalMatchedCellsVec[ii].zhit, FinalMatchedCellsVec[ii].matchFlag, FinalMatchedCellsVec[ii].hitPosition);
      tofCell->setLeadingEdgeTime(letime);
      tofCell->setTrailingEdgeTime(tetime);
      mCellCollection->push_back(tofCell);
      
      // dump debug data
      if (Debug()){
	gMessMgr->Info("","OS") << "F: itray=" << tray << " imodule=" << module << " icell=" << cell << "\tnodeid:";
	idVectorIter ij=FinalMatchedCellsVec[ii].trackIdVec.begin();
	while (ij != FinalMatchedCellsVec[ii].trackIdVec.end()) { gMessMgr->Info("","OS") << " " << *ij; ij++; }
	gMessMgr->Info("","OS") << "\tR=" << 1/(theTrackGeometry->helix().curvature())
	     << "\tpT=" << momentum.perp() << "\tp=" << momentum.mag()
	     << "\thits="<< nHitsPerTrack << "\ts="<< pathLength
	     << "\t#fitp=" <<theTrack->fitTraits().numberOfFitPoints(kTpcId)
	  //	     << "\t#trkp=" <<theTrack->detectorInfo()->numberOfPoints(kTpcId)
	     << " \tdedx=" << dedx
	     << " \tdca="<< globalTrack->geometry()->helix().distance(mEvent->primaryVertex()->position())<<" and "<<theTrackGeometry->helix().distance(mEvent->primaryVertex()->position());
	if (cherang!=0) LOG_INFO  << " \trich="<< cherang << " (" << cherang_nph << ")";
	gMessMgr->Info("","OS") << endm;
      }

    } // track exists 
  } // end final matched cells


  // put INL corrected vpd information in StTofCell as well
  StSPtrVecTofData &tofData = theTof->tofData();
  for(size_t ii = 0; ii<tofData.size(); ii++) {
    StTofData *aData = dynamic_cast<StTofData *>(tofData[ii]);
    if(!aData) continue;

    int dataIndex = aData->dataIndex();
    int trayId = dataIndex / mNTOF;
    if(trayId<120) continue;  // only vpd selected
    int ewId = trayId - 120 + 1;  // 1: east,  2: west
    
    int tubeId = dataIndex % mNTOF + 1;
    int lechan = mDaqMap->PMT2TDIGLeChan(tubeId);
    int techan = mDaqMap->PMT2TDIGTeChan(tubeId);

    int tmptdc = aData->leadingTdc();
    int bin = (int)tmptdc&0x3ff;
    float tmptdc_f = tmptdc + mTofINLCorr->getVpdINLCorr(ewId, lechan, bin);
    float letime = tmptdc_f*VHRBIN2PS;

    tmptdc = aData->trailingTdc();
    bin = (int)tmptdc&0x3ff;
    tmptdc_f = tmptdc + mTofINLCorr->getVpdINLCorr(ewId, techan, bin);
    float tetime = tmptdc_f*VHRBIN2PS;

    StThreeVectorF zero(0.,0.,0.);
    StTofCell *tofCell = new StTofCell(120+ewId, 0, tubeId, lechan, 0, 0, 0, zero);
    tofCell->setLeadingEdgeTime(letime);
    tofCell->setTrailingEdgeTime(tetime);
    mCellCollection->push_back(tofCell);

    if (Debug()){
      gMessMgr->Info("","OS") << "F: itray=" << trayId << " imodule=" << 0 << " itube=" << tubeId << " letime=" << letime << " tetime=" << tetime << endm;
    }
  }

  
  storeMatchData(mCellCollection,theTof);
  delete mCellCollection;
  
  //check StEvent collections --
  if (theTof->dataPresent())
    gMessMgr->Info("","OS") << " TofCollection: raw data container present" << endm;
  if (theTof->cellsPresent()){
    gMessMgr->Info("","OS") << " TofCollection: cell container present."<<endm;
    if (Debug()){
      StSPtrVecTofCell& tmpCellTofVec = theTof->tofCells();
      gMessMgr->Info("","OS") << " # of matched cells " << tmpCellTofVec.size() << endm;
      for (size_t i = 0; i < tmpCellTofVec.size(); i++) {
	StTofCell* p = tmpCellTofVec[i];
	gMessMgr->Info("","OS") << p->trayIndex() << " " << p->moduleIndex() << " " << p->cellIndex() << " " << p->trailingEdgeTime() << " " << p->leadingEdgeTime() << " " << p->associatedTrack() << " " << p->matchFlag() << " " << p->position() << endm;
      }
    }
  }
  //-- end check
  
  gMessMgr->Info("","OS") << "F: before/after" << FinalMatchedCellsVec.size() << "/" <<nValidSinglePrimHitCells << endm;
 // end of Sect.F

  gMessMgr->Info("","OS") << "#(cell tracks): " << allCellsHitVec.size()
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
    gMessMgr->Info("","OS") << "CPU time for StTofrMatchMaker::Make(): "
	 << timer.elapsedTime() << " sec\n" << endm;
  }

  gMessMgr->Info("","OS") << "StTofrMatchMaker -- bye-bye" << endm;



  return kStOK;
}

//---------------------------------------------------------------------------
// store local slat collection in StEvent's tofCollection
Int_t StTofrMatchMaker::storeMatchData(StTofCellCollection *cellCollection,
					 StTofCollection* tofCollection){
  if(!tofCollection){
    gMessMgr->Info("","OS") << "Error: no TofCollection -- returning" << endm;
    return kStErr;
  }

  for (size_t j=0;j<cellCollection->size();j++){
    tofCollection->addCell(cellCollection->getCell(j)); 
    if (Debug())
      gMessMgr->Info("","OS") << "storing " << j << "  " << "  tray:"
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
  gMessMgr->Info("","OS") << " Read in tof data ... " << endm;
  StSPtrVecTofData &tofData = tofCollection->tofData();

  // perform consistency check
  bool dataOK(true);
  gMessMgr->Info("","OS") << "TOF raw data consistency test ...";

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
  mDaqOccupancy     = new TH1D("daqOccupancy","daqOccupancy",120,0,120);
  mDaqOccupancyValid= new TH1D("daqOccupancyValid","daqOccupancyValid",120,0,120);
  mDaqOccupancyProj = new TH1D("daqOccupancyProj","daqOccupancyProj",120,0,120);
  mHitsPosition     = new TH2D("hitsPosition","hitsPositions",300,-15.,15.,200,-5.,5.);

  // primary association  
  mCellsPerEventMatch1 = new TH1D("cellsPerEventMatch1","cellPerEventMatch1",100,0,100);
  mHitsPerEventMatch1 = new TH1D("hitsPerEventMatch1","hitsPerEventMatch1",100,0,100);
  mCellsPerTrackMatch1 = new TH1D("cellsPerTrackMatch1","cellsPerTrackMatch1",100,0,100);
  mTracksPerCellMatch1 = new TH1D("tracksPerCellMatch1","tracksPerCellMatch1",100,0,100);
  mDaqOccupancyMatch1 = new TH1D("daqOccupancyMatch1","daqOccupancyMatch1",120,0,120);
  mDeltaHitMatch1 = new TH2D("deltaHitMatch1","deltaHitMatch1",300,-15,15,200,-5.,5.);

  // kick out multi-hit
  mCellsPerEventMatch2 = new TH1D("cellsPerEventMatch2","cellPerEventMatch2",100,0,100);
  mHitsPerEventMatch2 = new TH1D("hitsPerEventMatch2","hitsPerEventMatch2",100,0,100);
  mCellsPerTrackMatch2 = new TH1D("cellsPerTrackMatch2","cellsPerTrackMatch2",100,0,100);
  mTracksPerCellMatch2 = new TH1D("tracksPerCellMatch2","tracksPerCellMatch2",100,0,100);
  mDaqOccupancyMatch2 = new TH1D("daqOccupancyMatch2","daqOccupancyMatch2",120,0,120);
  mDeltaHitMatch2 = new TH2D("deltaHitMatch2","deltaHitMatch2",300,-15,15,200,-5.,5.);

  // sort out multi matched cells
  mCellsPerEventMatch3 = new TH1D("cellsPerEventMatch3","cellPerEventMatch3",100,0,100);
  mHitsPerEventMatch3 = new TH1D("hitsPerEventMatch3","hitsPerEventMatch3",100,0,100);
  mCellsPerTrackMatch3 = new TH1D("cellsPerTrackMatch3","cellsPerTrackMatch3",100,0,100);
  mTracksPerCellMatch3 = new TH1D("tracksPerCellMatch3","tracksPerCellMatch3",100,0,100);
  mDaqOccupancyMatch3 = new TH1D("daqOccupancyMatch3","daqOccupancyMatch3",120,0,120);
  mDeltaHitMatch3 = new TH2D("deltaHitMatch3","deltaHitMatch3",300,-15,15,200,-5.,5.);

  return;
}


//---------------------------------------------------------------------------
// store histograms in a seperate root file
void StTofrMatchMaker::writeHistogramsToFile(){
  // Output file
  TFile *theHistoFile =  new TFile(mHistoFileName.c_str(), "RECREATE");
  gMessMgr->Info("","OS") << "StTofrMatchMaker::writeHistogramsToFile()"
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
  if (!event->primaryVertex()) return false;
  mAcceptedEventCounter++;
  if(mHisto) mEventCounterHisto->Fill(2);

  // 3a. must have TOF collection
  if (!event->tofCollection()){
    gMessMgr->Info("","OS") << "TOF is not present" << endm;
    return false;
  }
  if(mHisto) mEventCounterHisto->Fill(3);

  // 3b. must have TOF raw data available
  if (!(event->tofCollection()->dataPresent())){
    gMessMgr->Info("","OS") << "TOF is present but no Raw Data" << endm;
    if  (!(event->tofCollection()->cellsPresent())){
      gMessMgr->Info("","OS") << "              and no Cell Data" << endm;
    }
    return false;
  }
  mTofEventCounter++;
  if(mHisto) mEventCounterHisto->Fill(4);

  
  
  // 4. must be a TOF beam event, i.e. a non-strobe event
  StSPtrVecTofData  &tofData = event->tofCollection()->tofData();
  gMessMgr->Info("","OS") << " tofData size = " << tofData.size() << endm;
  if (strobeEvent(tofData)){
    mTofStrobeEventCounter++;
    if (event->primaryVertex()) mAcceptAndStrobe++; // keep track of #valid strobed evts
    gMessMgr->Info("strobe event","OTS");
    return false;
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
  if (DCA > mMaxDCA) {gMessMgr->Info("","OS") << "dca>max:" << DCA<< endm; return false;}
  if (charge==0) { gMessMgr->Info("","OS") << " neutral charge" << endm; return false; }

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

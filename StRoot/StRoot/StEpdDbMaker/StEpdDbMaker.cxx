
#include "StEpdDbMaker.h"
//#include "StMessMgr.h"
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_epdQTMap_Table.h"
#include "tables/St_epdFEEMap_Table.h"
#include "tables/St_epdStatus_Table.h"
#include "tables/St_epdGain_Table.h"


ClassImp(StEpdDbMaker)

StEpdDbMaker::StEpdDbMaker(const Char_t *name) : StMaker(name){
  ResetArrays();
};

StEpdDbMaker::~StEpdDbMaker(){
  ResetArrays();
}

// JL -> there seem to be no reason to define those methods (base
//       class would do the job just fine)
//Int_t StEpdDbMaker::Init(){
//  LOG_DEBUG<<"StEpdDbMaker Init Start"<<endm; 
//  return StMaker::Init();
//}

//Int_t StEpdDbMaker::Make(){
//  LOG_DEBUG<<"StEpdDbMaker Make"<<endm; 
//  return kStOK;
//}

//void StEpdDbMaker::Clear(const Char_t*){
//  LOG_DEBUG<<"StEpdDbMaker Clear"<<endm; 
//  StMaker::Clear();
//}

//Int_t StEpdDbMaker::Finish(){
//  LOG_DEBUG<<"StEpdDbMaker Finish"<<endm; 
//  return kStOK;
//}


Int_t StEpdDbMaker::InitRun( Int_t runNumber ){
  LOG_DEBUG << "StEpdDbMaker::InitRun " << endm;
  ResetArrays();

  //! Accessing DBs
  if(mDebug>=0) {
    St_db_Maker* dbmaker = (St_db_Maker*)GetMaker("db");
    LOG_INFO << "StEpdDbMaker::InitRun - Date&time from St_db_Maker="
	     << dbmaker->GetDate() << "," << dbmaker->GetTime() << endm;
  }

  // Note: the calls below are requesting all epd tables in Geometry 
  //       calibration branches. If only a sub-set of tables is needed,
  //       you may instead access them one by one.
  TDataSet *DbGeometry     = GetInputDB("Geometry/epd");
  TDataSet *DbCalibrations = GetInputDB("Calibrations/epd");

  if(!DbGeometry){
    LOG_ERROR << "StEpdDbMaker::InitRun - No Geometry/epd"<<endm; 
    return kStFatal;
  }
  if(!DbCalibrations){
    LOG_ERROR << "StEpdDbMaker::InitRun - No Calibrations/epd"<<endm; 
    return kStFatal;
  }

  //!Getting DB tables
  St_epdQTMap  *dbQTMap  = (St_epdQTMap*) DbGeometry->Find("epdQTMap");
  St_epdFEEMap *dbFeeMap = (St_epdFEEMap*)DbGeometry->Find("epdFEEMap");
  St_epdStatus *dbStatus = (St_epdStatus*)DbCalibrations->Find("epdStatus");
  St_epdGain   *dbGain   = (St_epdGain*)  DbCalibrations->Find("epdGain");


  if(!dbQTMap){
    LOG_ERROR << "StEpdDbMaker::InitRun - No Geometry/epd/epdQTMap "<<endm; 
    return kStFatal;
  }
  if(!dbFeeMap){
    LOG_ERROR << "StEpdDbMaker::InitRun - No Geometry/epd/epdFEEMap "<<endm; 
    return kStFatal;
  }
  if(!dbStatus){
    LOG_ERROR << "StEpdDbMaker::InitRun - No Calibrations/epd/epdStatus "<<endm;
    return kStFatal;
  }
  if(!dbGain){
    LOG_ERROR << "StEpdDbMaker::InitRun - No Calibrations/epd/epdGain "<<endm; 
    return kStFatal;
  }

  // EPD QT map
  epdQTMap_st*  mEpdQtMapTable  = (epdQTMap_st*)dbQTMap->GetTable();
  epdFEEMap_st* mEpdFeeMapTable = (epdFEEMap_st*)dbFeeMap->GetTable();
  epdStatus_st* mEpdStatusTable = (epdStatus_st*)dbStatus->GetTable();
  epdGain_st*   mEpdGainTable   = (epdGain_st*)dbGain->GetTable();


  int wire1n=0;
  for (Int_t i = 0; i < 768; i++) {
    //Fetch from epdQTMap_st table
    short ew   = mEpdQtMapTable->ew[i];
    short pp   = mEpdQtMapTable->pp[i] -1; // In DB PP starts from 1
    short tile = mEpdQtMapTable->tile[i];

    mCrateAdc[ew][pp][tile]   = mEpdQtMapTable->qt_crate_adc[i];
    mBoardAdc[ew][pp][tile]   = mEpdQtMapTable->qt_board_adc[i];
    mChannelAdc[ew][pp][tile] = mEpdQtMapTable->qt_channel_adc[i];
    mCrateTac[ew][pp][tile]   = mEpdQtMapTable->qt_crate_tac[i];
    mBoardTac[ew][pp][tile]   = mEpdQtMapTable->qt_board_tac[i];
    mChannelTac[ew][pp][tile] = mEpdQtMapTable->qt_channel_tac[i];

    // Fetch from epdFEEMap_st table
    mTuffId[ew][pp][tile]        = mEpdFeeMapTable->tuff_id[i];
    mTuffGroup[ew][pp][tile]     = mEpdFeeMapTable->tuff_group[i];
    mTuffChannel[ew][pp][tile]   = mEpdFeeMapTable->tuff_channel[i];
    mReceiverBoard[ew][pp][tile] = mEpdFeeMapTable->receiver_board[i];
    mReceiverBoardChannel[ew][pp][tile] = mEpdFeeMapTable->receiver_board_channel[i];
    mCamacCrateAddress[ew][pp][tile] = mEpdFeeMapTable->camac_crate_address[i];
    mWireOneId[ew][pp][tile][0] = mEpdFeeMapTable->wire_1_id[wire1n];
    wire1n++;
    mWireOneId[ew][pp][tile][1] = mEpdFeeMapTable->wire_1_id[wire1n];
    wire1n++;

    // Fetch from epdStatus_st table
    mStatus[ew][pp][tile]= mEpdStatusTable->status[i];

    // Fetch from epdGain_st table
    mVPed[ew][pp][tile]        = mEpdGainTable->vped[i];
    mMip[ew][pp][tile]         = mEpdGainTable->mip[i];
    mQtPedestals[ew][pp][tile] = mEpdGainTable->qt_pedestals[i];
    mDarkCurrent[ew][pp][tile] = mEpdGainTable->dark_current[i];
    mQtPedestalsSigma[ew][pp][tile] = mEpdGainTable->qt_pedestals_sigma[i];
    mOffset[ew][pp][tile] = mEpdGainTable->offset[i];
  
  }

  return kStOK;

}

// default values -1
void StEpdDbMaker::ResetArrays(){
  for(int ew=0; ew<2 ; ew++){
    for(int pp=0; pp < 12;pp++){
      for(int tile=0; tile<32 ; tile++){

	mCrateAdc[ew][pp][tile] = -1;
	mBoardAdc[ew][pp][tile] = -1;
	mChannelAdc[ew][pp][tile] = -1;
	mCrateTac[ew][pp][tile] = -1;
	mBoardTac[ew][pp][tile] = -1;
	mChannelTac[ew][pp][tile] = -1;

	// Fetch from epdFEEMap_st table
	mTuffId[ew][pp][tile] = -1;
	mTuffGroup[ew][pp][tile] = -1;
	mTuffChannel[ew][pp][tile] = -1;
	mReceiverBoard[ew][pp][tile] = -1;
	mReceiverBoardChannel[ew][pp][tile] = -1;
	mCamacCrateAddress[ew][pp][tile] = -1;
	mWireOneId[ew][pp][tile][0] = 0x0;
	mWireOneId[ew][pp][tile][1] = 0x0;

	// Fetch from epdStatus_st table
	mStatus[ew][pp][tile]= -1;

	// Fetch from epdGain_st table
	mVPed[ew][pp][tile] = -1;
	mMip[ew][pp][tile] = -1;
	mQtPedestals[ew][pp][tile] = -1; 
	mDarkCurrent[ew][pp][tile] = -1;
	mQtPedestalsSigma[ew][pp][tile] = -1; 
	mOffset[ew][pp][tile] = -1; 

      }
    }
  }

}



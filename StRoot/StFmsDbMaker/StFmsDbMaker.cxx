/***************************************************************************
 * $Id: StFmsDbMaker.cxx,v 1.16 2015/11/10 23:09:45 akio Exp $
 * \author: akio ogawa
 ***************************************************************************
 *
 * Description: This maker is the interface between FMS and the STAR database
 *
 ***************************************************************************
 *
 * $Log: StFmsDbMaker.cxx,v $
 * Revision 1.16  2015/11/10 23:09:45  akio
 * fixed logic flaw
 *
 * Revision 1.15  2015/11/10 22:48:43  akio
 * change default to 1, not -1, for case we do not have LED time dep corr
 *
 * Revision 1.14  2015/11/10 22:35:44  akio
 * fix logic for fmsTimeDepCorr table making
 *
 * Revision 1.13  2015/11/10 22:13:54  akio
 * fix of a fix
 *
 * Revision 1.12  2015/11/10 21:59:03  akio
 * fixing backward compatbility
 *
 * Revision 1.11  2015/11/10 19:06:02  akio
 * Adding TimeDepCorr for LED gain correction based on event#
 *
 * Revision 1.10  2015/10/20 19:49:28  akio
 * Fixing distanceFromEdge()
 * Adding readRecParamFromFile()
 *
 * Revision 1.9  2015/09/23 17:34:01  akio
 * Adding distanceFromEdge() for fiducial volume cut
 *
 * Revision 1.8  2015/09/18 18:34:35  akio
 * Adding getStarXYZfromColumnRow() to convert from local grid space [cell width unit, not cm]
 * Adding protection for fmsGain and fmsGainCorrection when table length get shorter and can
 * overwritten by old values.
 * Removing some error log
 *
 * Revision 1.7  2015/09/02 14:45:14  akio
 * Adding new functions for un-uniform grid cell positions, switched based on DB fmsPositionModel
 *
 * Revision 1.3  2011/01/13 02:56:34  jgma
 * Fixed bug in function nRow and nColumn
 *
 * Revision 1.2  2010/01/11 20:35:30  jgma
 * Added reversed map and some other minor updates
 *
 * Revision 1.1  2009/10/28 16:11:15  jgma
 * This is the first check in of the code.
 *
 **************************************************************************/


#include "StFmsDbMaker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StMessMgr.h"
#include "tables/St_fmsDetectorPosition_Table.h"
#include "tables/St_fmsChannelGeometry_Table.h"
#include "tables/St_fmsMap_Table.h"
#include "tables/St_fmsPatchPanelMap_Table.h"
#include "tables/St_fmsQTMap_Table.h"
#include "tables/St_fmsGain_Table.h"
#include "tables/St_fmsGainCorrection_Table.h"
#include "tables/St_fmsTimeDepCorr_Table.h"
#include "tables/St_fmsRec_Table.h"
#include "tables/St_fmsPositionModel_Table.h"
#include "tables/St_fpsConstant_Table.h"
#include "tables/St_fpsChannelGeometry_Table.h"
#include "tables/St_fpsSlatId_Table.h"
#include "tables/St_fpsPosition_Table.h"
#include "tables/St_fpsMap_Table.h"
#include "tables/St_fpsGain_Table.h"
#include "tables/St_fpsStatus_Table.h"

#include "getCellPosition2015pp.h"
#include "getCellPosition2015pA.h"

#include "StEvent/StFmsHit.h"
#include "StEvent/StFmsPoint.h"
#include "StEvent/StEnumerations.h"

ClassImp(StFmsDbMaker)

StFmsDbMaker::StFmsDbMaker(const Char_t *name) : StMaker(name), mDebug(0),
  mChannelGeometry(0),mDetectorPosition(0),mMap(0),mmMap(0),mPatchPanelMap(0),
  mQTMap(0),mGain(0),mmGain(0),mGainCorrection(0),mmGainCorrection(0),mRecPar(0),
  mRecConfig(StFmsDbConfig::Instance()),
    mForceUniformGain(0.0), mForceUniformGainCorrection(0.0),mReadGainFile(0),mReadRecParam(0),
  mFpsConstant(0),mMaxSlatId(0),mFpsChannelGeometry(0),mFpsSlatId(0),mFpsReverseSlatId(0),
  mFpsPosition(0),mFpsMap(0),mFpsReverseMap(0),mFpsGain(0),mFpsStatus(0)  
{}; //{gStFmsDbMaker = this;}

StFmsDbMaker::~StFmsDbMaker() {deleteArrays(); /*gStFmsDbMaker = 0;*/}
Int_t StFmsDbMaker::Init(){LOG_DEBUG<<"StFmsDbMaker Init Start"<<endm; return StMaker::Init();}
Int_t StFmsDbMaker::Make(){LOG_DEBUG<<"StFmsDbMaker Make"<<endm; return kStOK;}
void StFmsDbMaker::Clear(const Char_t*){LOG_DEBUG<<"StFmsDbMaker Clear"<<endm; StMaker::Clear();}
Int_t StFmsDbMaker::Finish(){LOG_DEBUG<<"StFmsDbMaker Finish"<<endm; return kStOK;}

Int_t StFmsDbMaker::InitRun(Int_t runNumber) {
  LOG_DEBUG << "StFmsDbMaker::InitRun - run = " << runNumber << endm;
  deleteArrays();

  //! Accessing DBs
  if(mDebug>0) {
    St_db_Maker* dbmaker = (St_db_Maker*)GetMaker("db");
    LOG_INFO << "StFmsDbMaker::InitRun - Date&time from St_db_Maker="<<dbmaker->GetDate()<<","<< dbmaker->GetTime() << endm;
  }
  
  TDataSet *DBgeom = 0;
  TDataSet *DBmapping = 0;
  TDataSet *DBcalibration = 0;
  TDataSet *DBFpsGeom = 0;
  TDataSet *DBFpsCalibration = 0;
  DBgeom  = GetInputDB("Geometry/fms");
  DBmapping = GetInputDB("Calibrations/fms/mapping");
  DBcalibration= GetInputDB("Calibrations/fms");
  DBFpsGeom  = GetInputDB("Geometry/fps");
  DBFpsCalibration= GetInputDB("Calibrations/fps");
  if(!DBgeom)          {LOG_ERROR << "StFmsDbMaker::InitRun - No Geometry/fms"<<endm;            return kStFatal;}
  if(!DBmapping)       {LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fms/mapping"<<endm; return kStFatal;} 
  if(!DBcalibration)   {LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fms"<<endm;         return kStFatal;}
  if(!DBFpsGeom)       {LOG_ERROR << "StFmsDbMaker::InitRun - No Geometry/fps"<<endm;            return kStFatal;}
  if(!DBFpsCalibration){LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fps"<<endm;         return kStFatal;}  
 
  //!Getting DB tables
  St_fmsChannelGeometry *dbChannelGeometry   =0;
  St_fmsDetectorPosition *dbDetectorPosition =0;
  St_fmsPositionModel   *dbPositionModel     =0;
  St_fmsMap             *dbMap               =0;
  St_fmsPatchPanelMap   *dbPatchPanelMap     =0;
  St_fmsQTMap           *dbQTMap             =0;
  St_fmsGain            *dbGain              =0;
  St_fmsGainCorrection  *dbGainCorrection    =0;
  St_fmsTimeDepCorr     *dbTimeDepCorr       =0;
  St_fmsRec             *dbRec               =0;
  St_fpsConstant        *dbFpsConstant       =0;
  St_fpsChannelGeometry *dbFpsChannelGeometry=0;
  St_fpsSlatId          *dbFpsSlatId         =0;
  St_fpsPosition        *dbFpsPosition       =0;
  St_fpsMap             *dbFpsMap            =0;   
  St_fpsGain            *dbFpsGain           =0;
  St_fpsStatus          *dbFpsStatus         =0;

  dbChannelGeometry   = (St_fmsChannelGeometry*) DBgeom->Find("fmsChannelGeometry");
  dbDetectorPosition  = (St_fmsDetectorPosition*)DBgeom->Find("fmsDetectorPosition");
  dbPositionModel     = (St_fmsPositionModel*)   DBgeom->Find("fmsPositionModel");
  dbMap               = (St_fmsMap*)             DBmapping->Find("fmsMap");
  dbPatchPanelMap     = (St_fmsPatchPanelMap*)   DBmapping->Find("fmsPatchPanelMap");
  dbQTMap             = (St_fmsQTMap*)           DBmapping->Find("fmsQTMap");
  dbGain              = (St_fmsGain*)            DBcalibration->Find("fmsGain");
  dbGainCorrection    = (St_fmsGainCorrection*)  DBcalibration->Find("fmsGainCorrection");
  dbTimeDepCorr       = (St_fmsTimeDepCorr*)     DBcalibration->Find("fmsTimeDepCorr");
  dbRec               = (St_fmsRec*)             DBcalibration->Find("fmsRec");
  dbFpsConstant       = (St_fpsConstant*)        DBFpsGeom->Find("fpsConstant");
  dbFpsChannelGeometry= (St_fpsChannelGeometry*) DBFpsGeom->Find("fpsChannelGeometry");
  dbFpsSlatId         = (St_fpsSlatId*)          DBFpsGeom->Find("fpsSlatId"); 
  dbFpsPosition       = (St_fpsPosition*)        DBFpsGeom->Find("fpsPosition");
  dbFpsMap            = (St_fpsMap*)             DBFpsGeom->Find("fpsMap");
  dbFpsGain           = (St_fpsGain*)            DBFpsCalibration->Find("fpsGain");
  dbFpsStatus         = (St_fpsStatus*)          DBFpsCalibration->Find("fpsStatus");

  if(!dbChannelGeometry)   {LOG_ERROR << "StFmsDbMaker::InitRun - No Geometry/fms/fmsChannelGeometry"         <<endm; return kStFatal;}
  if(!dbDetectorPosition)  {LOG_ERROR << "StFmsDbMaker::InitRun - No Geometry/fms/fmsDetectorPosition"        <<endm; return kStFatal;}
  if(!dbPositionModel)     {LOG_INFO  << "StFmsDbMaker::InitRun - No Geometry/fms/fmsPositionModel, using default" <<endm;            }
  if(!dbMap)               {LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fms/mapping/fmsMap"          <<endm; return kStFatal;}
  if(!dbPatchPanelMap)     {LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fms/mapping/fmsPatchPanelMap"<<endm; return kStFatal;}
  if(!dbQTMap)             {LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fms/mapping/fmsQTMap"        <<endm; return kStFatal;}
  if(!dbGain)              {LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fms/fmsGain"                 <<endm; return kStFatal;}
  if(!dbGainCorrection)    {LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fms/fmsGainCorrection"       <<endm; return kStFatal;}
  if(!dbTimeDepCorr)       {LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fms/fmsTimeDepCorr"          <<endm; return kStFatal;}
  if(!dbRec)               {LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fms/fmsRec"                  <<endm; return kStFatal;}
  
  if(!dbFpsConstant)       {LOG_ERROR << "StFmsDbMaker::InitRun - No Geometry/fps/fpsConstant"                <<endm;}  
  if(!dbFpsChannelGeometry){LOG_ERROR << "StFmsDbMaker::InitRun - No Geometry/fps/fpsChannelGeometry"         <<endm;}
  if(!dbFpsSlatId)         {LOG_ERROR << "StFmsDbMaker::InitRun - No Geometry/fps/fpsSlatId"                  <<endm;}
  if(!dbFpsPosition)       {LOG_ERROR << "StFmsDbMaker::InitRun - No Geometry/fps/fpsPosition"                <<endm;}
  if(!dbFpsMap)            {LOG_ERROR << "StFmsDbMaker::InitRun - No Geometry/fps/fpsMap"                     <<endm;}
  if(!dbFpsGain)           {LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fps/fpsGain"                 <<endm;}
  if(!dbFpsStatus)         {LOG_ERROR << "StFmsDbMaker::InitRun - No Calibration/fps/fpsStatus"               <<endm;}

  //!fmsChannelGeometry
  fmsChannelGeometry_st *tChannelGeometry = 0;
  tChannelGeometry = (fmsChannelGeometry_st*) dbChannelGeometry->GetTable();
  Int_t max = dbChannelGeometry->GetNRows();
  mMaxDetectorId = 0;
  for(Int_t i=0; i<max; i++){
    if(mMaxDetectorId < tChannelGeometry[i].detectorId) mMaxDetectorId = tChannelGeometry[i].detectorId;     
  }
  mChannelGeometry = new fmsChannelGeometry_st[mMaxDetectorId+1];
  memset(mChannelGeometry,0,sizeof(fmsChannelGeometry_st)*(mMaxDetectorId+1));
  for(Int_t i=0; i<max; i++){ 
    memcpy(&mChannelGeometry[tChannelGeometry[i].detectorId], &tChannelGeometry[i], sizeof(fmsChannelGeometry_st));
  }
  LOG_DEBUG << "StFmsDbMaker::InitRun - Got Geometry/fms/fmsChannelGeometry with maxDetectorId = "<<mMaxDetectorId<< endm;

  //!fmsDetectorPosition
  fmsDetectorPosition_st *tDetectorPosition = 0;
  tDetectorPosition = (fmsDetectorPosition_st*) dbDetectorPosition->GetTable();
  mDetectorPosition = new fmsDetectorPosition_st[mMaxDetectorId+1];
  memset(mDetectorPosition,0,sizeof(fmsDetectorPosition_st)*(mMaxDetectorId+1));
  max = dbDetectorPosition->GetNRows();
  for(Int_t i=0; i<max; i++){
    memcpy(&mDetectorPosition[tDetectorPosition[i].detectorId], &tDetectorPosition[i], sizeof(fmsDetectorPosition_st));
  }
  LOG_DEBUG << "StFmsDbMaker::InitRun - Got Geometry/fms/fmsDetectorPosition with  "<<max<<" detectors"<< endm;

  //!fmsPositionModel
  mPositionModel=0;
  if(dbPositionModel){
    fmsPositionModel_st *tPositionModel = 0;
    tPositionModel = (fmsPositionModel_st*) dbPositionModel->GetTable();
    mPositionModel= tPositionModel[0].model;
  }
  LOG_INFO << "StFmsDbMaker::InitRun - Got Geometry/fms/fmsPositionModel = "<<mPositionModel<< endm;

  //!fmsPatchPanelMap
  mPatchPanelMap = (fmsPatchPanelMap_st*) dbPatchPanelMap->GetTable();
  mMaxModule = dbPatchPanelMap->GetNRows();
  //!LOG_INFO << mMaxModule << "***********" << endm;
  LOG_DEBUG << "StFmsDbMaker::InitRun - Got Calibration/fms/mapping/fmsPatchPanelMap with mMaxModule = "<<mMaxModule<< endm;

  //!fmsQTMap
  mQTMap = (fmsQTMap_st*) dbQTMap->GetTable();
  mMaxNS = dbQTMap->GetNRows();
  LOG_DEBUG << "StFmsDbMaker::InitRun - Got Calibration/fms/mapping/fmsQTMap with mMaxNS = "<<mMaxNS<< endm;
  
  //!fmsMap
  mMap = (fmsMap_st*) dbMap->GetTable();
  mMaxMap = dbMap->GetNRows();
  mmMap = new fmsMap_st* [mMaxDetectorId+1];
  memset(mmMap,0,sizeof(fmsMap_st*)*(mMaxDetectorId+1));
  memset(mReverseMapDetectorId,0,sizeof(mReverseMapDetectorId));
  memset(mReverseMapChannel,0,sizeof(mReverseMapChannel));
  for(Int_t i=0; i<mMaxMap; i++){
    Int_t d=mMap[i].detectorId;
    Int_t c=mMap[i].ch;
    if(d<0 || d>mMaxDetectorId){
      LOG_ERROR << "StFmsDbMaker::InitRun - Calibration/fms/mapping/fmsMap detectorId="<<d<<" exceed max="<<mMaxDetectorId<<endm; 
      return kStFatal;
    }
    if(c<1 || c>maxChannel(d)){
      LOG_ERROR << "StFmsDbMaker::InitRun - Calibration/fms/mapping/fmsMap ch="<<c<<" exceed max="<<maxChannel(d)<<endm; 
      return kStFatal;
    }
    if(mmMap[d]==0){
      mmMap[d] = new fmsMap_st [maxChannel(d)];
      memset(mmMap[d],0,sizeof(fmsMap_st)*maxChannel(d));
    }
    memcpy(&mmMap[d][c-1],&mMap[i],sizeof(fmsMap_st));
    //creating reverse mapping
    Int_t crt,slot,ch;
    getMap(d,c,&crt,&slot,&ch);
    mReverseMapDetectorId[crt][slot][ch]=d;
    mReverseMapChannel[crt][slot][ch]=c;
  }
  
  LOG_DEBUG << "StFmsDbMaker::InitRun - Got Geometry/fms/mapping/fmsMap with mMaxMap = "<<mMaxMap<< endm;
  
  //!fmsGain
  mGain = (fmsGain_st*) dbGain->GetTable();
  mMaxGain = dbGain->GetNRows();
  mmGain = new fmsGain_st* [mMaxDetectorId+1];
  memset(mmGain,0,sizeof(fmsGain_st*)*(mMaxDetectorId+1));
  for(Int_t i=0; i<mMaxGain; i++){
    Int_t d=mGain[i].detectorId;
    Int_t c=mGain[i].ch;
    if(d<0 || d>mMaxDetectorId){
	LOG_DEBUG << "StFmsDbMaker::InitRun - Calibration/fms/fmsGain detectorId="<<d<<" exceed max = "<<mMaxDetectorId<<endm; 
	continue;
    }
    if(maxChannel(d)<1){
      LOG_ERROR << "StFmsDbMaker::InitRun - Calibration/fms/fmsGain invalid max number of channel = "<<maxChannel(d)
		<<"for det="<<d<<endm; 
      continue;
    }
    if(c<1 || c>maxChannel(d)){
      LOG_ERROR << "StFmsDbMaker::InitRun - Calibration/fms/fmsGain detectorId="<<d<<" ch="<<c<<" exceed max = "<<maxChannel(d)<<endm; 
      continue;
    }
    if(mmGain[d]==0){
      mmGain[d] = new fmsGain_st [maxChannel(d)];
      memset(mmGain[d],0,sizeof(fmsGain_st)*maxChannel(d));
    }
    if(mmGain[d][c-1].ch==0){
	memcpy(&mmGain[d][c-1],&mGain[i],sizeof(fmsGain_st));
    }else{
	LOG_ERROR << "StFmsDbMaker::InitRun - Calibration/fms/fmsGain detectorId="<<d<<" ch="<<c<<" double entry, skipping"<<endm;
    }
    if(mForceUniformGain>0.0){
      static int first=0;
      if(first<3){
        LOG_INFO << "StFmsDbMaker::InitRun - Calibration/fms/fmsGain overwritten to uniform value="<<mForceUniformGain<<endm;
        first++;
      }
      mmGain[d][c-1].gain=mForceUniformGain;
    }
  }
  if(mReadGainFile){
    LOG_INFO << "StFmsDbMaker::InitRun - Calibration/fms/fmsGain will be overwritten by FmsGain.txt"<<endm;
    FILE* f=fopen("FmsGain.txt","r");
    if(!f){ 
      LOG_INFO<<"Failed to open FmsGain.txt"<<endm; 
    }else{
      int ew,nstb,ch;
      float gain;
      while(fscanf(f,"%d %d %d %f",&ew,&nstb,&ch,&gain)!=EOF){
	if(ew==2){
	  int dd=nstb+7;
	  //printf("Reading FmsGain.txt  %1d %1d %2d %3d %f\n",ew,nstb,dd,ch,gain);
	  mmGain[dd][ch-1].gain=gain;
	}
       }
    }
    fclose(f);
    LOG_INFO << "StFmsDbMaker::InitRun - Calibration/fms/fmsGain was overwritten by FmsGain.txt"<<endm;
  }
  LOG_DEBUG << "StFmsDbMaker::InitRun - Got Calibration/fms/fmsGain with mMaxGain = "<<mMaxGain<< endm;
  
  //!fmsGainCorrection
  mGainCorrection = (fmsGainCorrection_st*) dbGainCorrection->GetTable();
  mMaxGainCorrection = dbGainCorrection->GetNRows();
  mmGainCorrection = new fmsGainCorrection_st* [mMaxDetectorId+1];
  memset(mmGainCorrection,0,sizeof(fmsGainCorrection_st*)*(mMaxDetectorId+1));
  for(Int_t i=0; i<mMaxGainCorrection; i++){
    Int_t d=mGainCorrection[i].detectorId;
    Int_t c=mGainCorrection[i].ch;
    if(d<0 || d>mMaxDetectorId){
	LOG_DEBUG << "StFmsDbMaker::InitRun - Calibration/fms/fmsGainCorrection detectorId="<<d<<" exceed max="<<mMaxDetectorId<<endm; 
	continue;
    }
    if(maxChannel(d)<1){
      LOG_ERROR << "StFmsDbMaker::InitRun - Calibration/fms/fmsGainCorrection invalid max number of channel = "<<maxChannel(d)
		<<"for det="<<d<<endm; 
      continue;
    }
    if(c<1 || c>maxChannel(d)){
      LOG_ERROR << "StFmsDbMaker::InitRun - Calibration/fms/fmsGainCorrection ch="<<c<<" exceed max="<<maxChannel(d)<<endm; 
      continue;
    }
    if(mmGainCorrection[d]==0){
      mmGainCorrection[d] = new fmsGainCorrection_st [maxChannel(d)];
      memset(mmGainCorrection[d],0,sizeof(fmsGainCorrection_st)*maxChannel(d));
    }
    if(mmGainCorrection[d][c-1].ch==0){
	memcpy(&mmGainCorrection[d][c-1],&mGainCorrection[i],sizeof(fmsGainCorrection_st));
    }else{
	LOG_ERROR << "StFmsDbMaker::InitRun - Calibration/fms/fmsGainCorr detectorId="<<d<<" ch="<<c<<" double entry, skipping"<<endm;
    }
    if(mForceUniformGainCorrection>0.0){
      static int first=0;
      if(first==0){
        LOG_INFO << "StFmsDbMaker::InitRun - Calibration/fms/fmsGainCorrection overwritten to uniform value="<<mForceUniformGainCorrection<<endm;
        first++;
      }
      mmGainCorrection[d][c-1].corr=mForceUniformGainCorrection;
    }
  }
  LOG_DEBUG << "StFmsDbMaker::InitRun - Got Geometry/fms/fmsGainCorrection with mMaxGainCorrection = "<<mMaxGainCorrection<< endm;

  //!fmsTimeDepCorr
  mMaxTimeSlice=0;
  fill_n(&mTimeDep[0][0][0],mFmsTimeDepMaxTimeSlice*mFmsTimeDepMaxDet*mFmsTimeDepMaxCh,1.0);  
  memset(mTimeDepEvt,0,sizeof(mTimeDepEvt));
  mTimeDepCorr = (fmsTimeDepCorr_st*) dbTimeDepCorr->GetTable();
  if(mTimeDepCorr){
      int nrow = dbTimeDepCorr->GetNRows();
      if(nrow!=1) {
	  LOG_DEBUG << "StFmsDbMaker::InitRun - Calibration/fms/fmsTimeDepCorr should have 1 row only, but found " 
		    << nrow << " rows. No TimeDepCorr"<<endm;
      }else{
	  int t=0, ndata=0, keepch=0, keept=0;
	  for(Int_t i=0; i<mFmsTimeDepMaxData; i++){      
	      Int_t d=mTimeDepCorr[0].detectorId[i];
	      Int_t c=mTimeDepCorr[0].ch[i];
	      Int_t e=mTimeDepCorr[0].endEvent[i];
	      Float_t v=mTimeDepCorr[0].corr[i];
	      if(d==0 && c==1){
		  mTimeDepEvt[t]=e;
		  mTimeDep[t][d][c-1]=v;
		  t++;
		  mMaxTimeSlice=t;
	      }else if(d==0 && c==0 && e==0){
		  break;
	      }else{		  
		  if(c!=keepch) keept=0;
		  for(int tt=keept; tt<mMaxTimeSlice; tt++){
		      if(e>=mTimeDepEvt[tt]) {mTimeDep[tt][d][c-1]=v;}
		      else {keept=tt; break;}
		  }
		  keepch=c;
	      }
	      ndata++;
	  }
	  LOG_DEBUG << "StFmsDbMaker::InitRun - Got Geometry/fms/fmsTimeDepCorr with "<<ndata<<" lines abd "<<mMaxTimeSlice<<" timeslices"<<endm;
      }
  }

  
  //!fmsRec
  mMaxRecPar = 80; //dummy
  if(mReadRecParam==0){
      mRecPar = (fmsRec_st*)dbRec->GetTable();
      mRecConfig.readMap(*mRecPar); //read recPar into internal memory
      LOG_DEBUG << "StFmsDbMaker::InitRun - Got Calibration/fms/fmsRec "<< endm;
  }else{
      mRecConfig.fillMap("fmsrecpar.txt");
      LOG_INFO << "StFmsDbMaker::InitRun - read fmsrecpar.txt for FmsRec table! "<< endm;
  }

  //!fpsConstant
  if(dbFpsConstant){
      fpsConstant_st *tFpsConstant = 0;
      tFpsConstant = (fpsConstant_st*) dbFpsConstant->GetTable();
      mFpsConstant = new fpsConstant_st;
      memcpy(mFpsConstant,tFpsConstant,sizeof(fpsConstant_st));
      mMaxSlatId=fpsNQuad()*fpsNLayer()*fpsMaxSlat();
      LOG_DEBUG << "StFmsDbMaker::InitRun - Got Geometry/fms/fpsConstant maxSlatId="<<mMaxSlatId<<endm;
  }
      
  int mI=0, mQ = 0, mL=0, mS=0;
  //!fpsChannelGeometry
  if(dbFpsChannelGeometry){
      fpsChannelGeometry_st *tFpsChannelGeometry = 0;
      tFpsChannelGeometry = (fpsChannelGeometry_st*) dbFpsChannelGeometry->GetTable();
      max = dbFpsChannelGeometry->GetNRows();
      for(Int_t i=0; i<max; i++){
	  if(mQ < tFpsChannelGeometry[i].quad)  mQ=tFpsChannelGeometry[i].quad;
	  if(mL < tFpsChannelGeometry[i].layer) mL=tFpsChannelGeometry[i].layer;
      }
      if(mQ>fpsNQuad())  LOG_WARN << "StFmsDbMaker::InitRun - fpsChannelGeometry has more quad than fpsConstant"<<endm;
      if(mL>fpsNLayer()) LOG_WARN << "StFmsDbMaker::InitRun - fpsChannelGeometry has more layer than fpsConstant"<<endm;
      mFpsChannelGeometry = new fpsChannelGeometry_st*[fpsNQuad()]();
      for(int i=0; i<fpsNQuad(); i++) mFpsChannelGeometry[i]= new fpsChannelGeometry_st[fpsNLayer()]();
      for(Int_t i=0; i<max; i++){ 
	  memcpy(&mFpsChannelGeometry[tFpsChannelGeometry[i].quad-1][tFpsChannelGeometry[i].layer-1], 
		 &tFpsChannelGeometry[i], sizeof(fpsChannelGeometry_st));
      }
      LOG_DEBUG << "StFmsDbMaker::InitRun - Got Geometry/fms/fpsChannelGeometry with maxQuad="<<mQ<<" and maxLayer="<<mL<<endm;
  }

  //!fpsSlatId
  if(dbFpsSlatId){
      fpsSlatId_st *tFpsSlatId = 0;
      tFpsSlatId = (fpsSlatId_st*) dbFpsSlatId->GetTable();
      max = dbFpsSlatId->GetNRows();
      mI=0; mQ=0; mL=0; mS=0;
      for(Int_t i=0; i<max; i++){
	  if(mI < tFpsSlatId[i].slatid) mI=tFpsSlatId[i].slatid;
	  if(mQ < tFpsSlatId[i].quad)   mQ=tFpsSlatId[i].quad;
	  if(mL < tFpsSlatId[i].layer)  mL=tFpsSlatId[i].layer;
	  if(mS < tFpsSlatId[i].slat)   mS=tFpsSlatId[i].slat;
      }
      if(max>fpsMaxSlatId()) LOG_WARN << "StFmsDbMaker::InitRun - fpsSlatId has more row than fpsConstant"<<endm;
      if(mI>fpsMaxSlatId())  LOG_WARN << "StFmsDbMaker::InitRun - fpsSlatId has more slatId than fpsConstant"<<endm;
      if(mQ>fpsNQuad())      LOG_WARN << "StFmsDbMaker::InitRun - fpsSlatId has more quad than fpsConstant"<<endm;
      if(mL>fpsNLayer())     LOG_WARN << "StFmsDbMaker::InitRun - fpsSlatId has more layer than fpsConstant"<<endm;
      if(mS>fpsMaxSlat())    LOG_WARN << "StFmsDbMaker::InitRun - fpsSlatId has more slat than fpsConstant"<<endm;
      mFpsSlatId = new fpsSlatId_st[max];
      mFpsReverseSlatId = new int**[fpsNQuad()]();
      for(int i=0; i<fpsNQuad(); i++) {
	  mFpsReverseSlatId[i] = new int*[fpsNLayer()]();
	  for(int j=0; j<fpsNLayer(); j++) {
	      mFpsReverseSlatId[i][j] = new int[fpsMaxSlat()]();
	      for(int k=0; k<fpsMaxSlat(); k++) mFpsReverseSlatId[i][j][k]=-1;
	  }    
      }
      for(Int_t i=0; i<max; i++){ 
	  memcpy(&mFpsSlatId[tFpsSlatId[i].slatid],&tFpsSlatId[i],sizeof(fpsSlatId_st));
	  if(tFpsSlatId[i].quad>0 && tFpsSlatId[i].layer>0 && tFpsSlatId[i].slat>0){
	      mFpsReverseSlatId[tFpsSlatId[i].quad-1][tFpsSlatId[i].layer-1][tFpsSlatId[i].slat-1]=tFpsSlatId[i].slatid;
	  }
      }
      LOG_DEBUG << "StFmsDbMaker::InitRun - Got Geometry/fms/fpsSlatId with max slat Id="<<max<<endm;
  }

  //!fpsPosition
  if(dbFpsPosition){
      fpsPosition_st *tFpsPosition = 0;
      tFpsPosition = (fpsPosition_st*) dbFpsPosition->GetTable();
      max = dbFpsPosition->GetNRows();
      mI=0;
      for(Int_t i=0; i<max; i++){
	  if(mI < tFpsPosition[i].slatid) mI=tFpsPosition[i].slatid;
      }
      if(max>fpsMaxSlatId()) LOG_WARN << "StFmsDbMaker::InitRun - fpsPosition has more row than fpsConstant"<<endm;
      if( mI>fpsMaxSlatId()) LOG_WARN << "StFmsDbMaker::InitRun - fpsPosition has more slatId than fpsConstant"<<endm;
      mFpsPosition = new fpsPosition_st[max];
      memset(mFpsPosition,0,sizeof(*mFpsPosition));
      for(Int_t i=0; i<max; i++){ 
	  if(tFpsPosition[i].slatid==0 && tFpsPosition[i].xoffset==0.0 && tFpsPosition[i].yoffset==0.0) continue;
	  memcpy(&mFpsPosition[tFpsPosition[i].slatid],&tFpsPosition[i],sizeof(fpsPosition_st));
      }
      LOG_DEBUG << "StFmsDbMaker::InitRun - Got Geometry/fms/fpsPosition with max slat Id="<<max<<endm;
  }

  //!fpsMap
  if(dbFpsMap){
      fpsMap_st *tFpsMap = 0;
      tFpsMap = (fpsMap_st*) dbFpsMap->GetTable();
      max = dbFpsMap->GetNRows();
      if(max>fpsMaxSlatId()) LOG_WARN << "StFmsDbMaker::InitRun - fpsMap has more slatId than fpsConstant"<<endm;
      int mA = 0, mC=0; mI=0;
      for(Int_t i=0; i<max; i++){
	  if(mI < tFpsMap[i].slatid) mI=tFpsMap[i].slatid;
	  if(mA < tFpsMap[i].QTaddr) mA=tFpsMap[i].QTaddr;
	  if(mC < tFpsMap[i].QTch)   mC=tFpsMap[i].QTch;
      }
      if(max>fpsMaxSlat())   LOG_WARN << "StFmsDbMaker::InitRun - fpsMap has more row than fpsConstant"<<endm;
      if(mI >fpsMaxSlat())   LOG_WARN << "StFmsDbMaker::InitRun - fpsMap has more slatid than fpsConstant"<<endm;
      if(mA>=fpsMaxQTaddr()) LOG_WARN << "StFmsDbMaker::InitRun - fpsMap has more QTaddr"<<endm;
      if(mC>=fpsMaxQTch())   LOG_WARN << "StFmsDbMaker::InitRun - fpsMap has more QTch"<<endm;
      mFpsMap = new fpsMap_st[max];
      mFpsReverseMap = new int*[fpsMaxQTaddr()]();
      for(int i=0; i<fpsMaxQTaddr(); i++) mFpsReverseMap[i] = new int[fpsMaxQTch()]();
      for(Int_t i=0; i<max; i++){ 
	  memcpy(&mFpsMap[tFpsMap[i].slatid],&tFpsMap[i],sizeof(fpsMap_st));
	  if(tFpsMap[i].QTaddr>=0 && tFpsMap[i].QTch>=0)
	      mFpsReverseMap[tFpsMap[i].QTaddr][tFpsMap[i].QTch]=tFpsMap[i].slatid;
      }
      LOG_DEBUG << "StFmsDbMaker::InitRun - Got Geometry/fms/fpsMap with max slat Id="<<max<<endm;
  }

  //!fpsGain
  if(dbFpsGain){
      fpsGain_st *tFpsGain = 0;
      tFpsGain = (fpsGain_st*) dbFpsGain->GetTable();
      max = dbFpsGain->GetNRows();
      int mI=0;
      for(Int_t i=0; i<max; i++){
	  if(mI < tFpsGain[i].slatid) mI=tFpsGain[i].slatid;
      }
      if(max>fpsMaxSlatId()) LOG_WARN << "StFmsDbMaker::InitRun - fpsGain has more row than fpsConstant"<<endm;
      if(mI >fpsMaxSlatId()) LOG_WARN << "StFmsDbMaker::InitRun - fpsGain has more slatId than fpsConstant"<<endm;
      mFpsGain = new fpsGain_st[max];
      memset(mFpsGain,0,sizeof(*mFpsGain));
      for(Int_t i=0; i<max; i++){
	  memcpy(&mFpsGain[tFpsGain[i].slatid],&tFpsGain[i],sizeof(fpsGain_st));
      }
      LOG_DEBUG << "StFmsDbMaker::InitRun - Got Calibration/fms/fpsGain with max slat Id="<<max<<endm;
  }

  //!fpsStatus
  if(dbFpsStatus){
      fpsStatus_st *tFpsStatus = 0;
      tFpsStatus = (fpsStatus_st*) dbFpsStatus->GetTable();
      max = dbFpsStatus->GetNRows();
      mI=0;
      for(Int_t i=0; i<max; i++){
	  if(mI < tFpsStatus[i].slatid) mI=tFpsStatus[i].slatid;
      }
      if(max>fpsMaxSlatId()) LOG_WARN << "StFmsDbMaker::InitRun - fpsStatus has more row than fpsConstant"<<endm;
      if(mI >fpsMaxSlatId()) LOG_WARN << "StFmsDbMaker::InitRun - fpsStatus has more slatId than fpsConstant"<<endm;
      mFpsStatus = new fpsStatus_st[max];
      memset(mFpsStatus,0,sizeof(*mFpsStatus));
      for(Int_t i=0; i<max; i++){
	  memcpy(&mFpsStatus[tFpsStatus[i].slatid],&tFpsStatus[i],sizeof(fpsStatus_st));
      }
      LOG_DEBUG << "StFmsDbMaker::InitRun - Got Calibration/fms/fpsStatus with max slat Id="<<max<<endm;
  }
  
  //!Debug
  if(mDebug>0){
    dumpFmsChannelGeometry();
    dumpFmsDetectorPosition();
    dumpFmsMap();
    dumpFmsPatchPanelMap();
    dumpFmsQTMap();
    dumpFmsGain();
    dumpFmsGainCorrection();
    dumpFmsTimeDepCorr();
    dumpFmsRec();
    if(dbFpsChannelGeometry) dumpFpsChannelGeometry(); 
    if(dbFpsSlatId) dumpFpsSlatId();          
    if(dbFpsPosition) dumpFpsPosition();        
    if(dbFpsMap) dumpFpsMap();        
    if(dbFpsGain) dumpFpsGain();            
    if(dbFpsStatus) dumpFpsStatus();            
  }
  return kStOK;
}

StFmsDbConfig& StFmsDbMaker::getRecConfig(){ return mRecConfig; }

void StFmsDbMaker::deleteArrays(){
  if(mChannelGeometry) delete [] mChannelGeometry;  
  if(mDetectorPosition) delete [] mDetectorPosition;  
  if(mmMap){
    for(Int_t d=0; d<=mMaxDetectorId; d++){
      if(mmMap[d]) delete [] mmMap[d];
    }
    delete [] mmMap;
  }    
  if(mmGain){
    for(Int_t d=0; d<=mMaxDetectorId; d++){ 
      if(mmGain[d]) delete [] mmGain[d];
    }
    delete [] mmGain;
  }
  if(mmGainCorrection){
    for(Int_t d=0; d<=mMaxDetectorId; d++){ 
      if(mmGainCorrection[d]) delete [] mmGainCorrection[d];
    }
    delete [] mmGainCorrection;
  }
  //FPS
  if(mFpsGain) delete [] mFpsGain;
  if(mFpsStatus) delete [] mFpsStatus;
  if(mFpsReverseMap){
    for(Int_t i=0; i<fpsNQuad(); i++){
      if(mFpsReverseMap[i]) delete [] mFpsReverseMap[i];
    }
    delete [] mFpsReverseMap;
  }	
  if(mFpsMap) delete [] mFpsMap;
  if(mFpsPosition) delete [] mFpsPosition;
  if(mFpsReverseSlatId){
    for(Int_t i=0; i<fpsNQuad(); i++){
      if(mFpsReverseSlatId[i]){
	for(Int_t j=0; j<fpsNLayer(); j++){
	  if(mFpsReverseSlatId[i][j]) delete [] mFpsReverseSlatId[i][j];
	}
	delete [] mFpsReverseSlatId[i];
      }
    }
    delete [] mFpsReverseSlatId;
  }
  if(mFpsSlatId) delete [] mFpsSlatId;
  if(mFpsChannelGeometry) {
    for(Int_t d=0; d<fpsNQuad(); d++){
      if(mFpsChannelGeometry[d]) delete [] mFpsChannelGeometry[d];
    }
    delete [] mFpsChannelGeometry;
  }
  if(mFpsConstant) delete mFpsConstant; //this comes last since some delete above uses this
}

//! get coordinates of center of the cell in STAR frame from detectorId/ch
StThreeVectorF StFmsDbMaker::getStarXYZ(Int_t detectorId, Int_t ch){
    return getStarXYZ(detectorId,getColumnNumber(detectorId,ch),getRowNumber(detectorId,ch));   
}

//! get coordinates of center of the cell STAR frame from detectorId/row/column
StThreeVectorF StFmsDbMaker::getStarXYZ(Int_t detectorId, Int_t column, Int_t row){
  return getStarXYZfromColumnRow(detectorId,float(column-0.5),float(row-0.5)); 
}


//! get coordinates of center of the cell STAR frame from detectorId/row/column grid space [unit is cell size]
StThreeVectorF StFmsDbMaker::getStarXYZfromColumnRow(Int_t detectorId, Float_t column, Float_t row){
  return getStarXYZ(detectorId,column*getXWidth(detectorId),row*getYWidth(detectorId)); 
}

//! get coordinates of center of the cell STAR frame from StFmsHit
StThreeVectorF StFmsDbMaker::getStarXYZ(StFmsHit* hit){
  return getStarXYZ(hit->detectorId(),hit->channel());
}

//! get coordinates in STAR frame from local XY (in row/column space [cm]) 
StThreeVectorF StFmsDbMaker::getStarXYZ(Int_t detectorId,Double_t FmsX, Double_t FmsY){
  return getStarXYZ(detectorId,float(FmsX),float(FmsY));
}

//! get coordinates in STAR frame from local XY (in row/column space [cm])
StThreeVectorF StFmsDbMaker::getStarXYZ(Int_t detectorId,Float_t FmsX, Float_t FmsY){
  Float_t x = 0.0, y=0.0, z=0.0;
  //printf("getStarXYZ mPositionModel=%d\n",mPositionModel);
  if(mPositionModel==0){ //simple uniform model with xyz offsets and widthes from DB
    //printf("getStarXYZ XOFF=%f YOFF=%f\n",mDetectorPosition[detectorId].xoffset,mDetectorPosition[detectorId].yoffset); 
    if(northSouth(detectorId) == 0) //! north side
	x = mDetectorPosition[detectorId].xoffset - FmsX;
    else  //! south side
	x = mDetectorPosition[detectorId].xoffset + FmsX; 
    //y = mDetectorPosition[detectorId].yoffset - FmsY;
    y = FmsY - mDetectorPosition[detectorId].yoffset; //row# start from bottom 
    z = mDetectorPosition[detectorId].zoffset;
  }else{
    //printf("getStarXYZ input XY=%f %f\n",FmsX,FmsY);
    float x1,x2,y1,y2;
    float lx = FmsX/getXWidth(detectorId);         
    float ly = nRow(detectorId) - FmsY/getYWidth(detectorId); //row# in getCellPosition2015xx start from top, so reverse it
    //printf("getStarXYZ local XY=%f %f\n",lx,ly);
    int   c  = int(lx);
    int   r  = int(ly);
    //printf("getStarXYZ column/row=%d %d\n",c,r);
    float dx = lx-c;
    if (northSouth(detectorId)==0) {dx=1.0-dx;} //north side
    float dy = 1.0-(ly-r);
    if(mPositionModel==1)      {getCellPosition2015pp(detectorId-7,r,c,x1,y1,x2,y2,z);}
    else if(mPositionModel==2) {getCellPosition2015pA(detectorId-7,r,c,x1,y1,x2,y2,z);}
    x = x1*(1.0-dx) + x2*dx;
    y = y1*(1.0-dy) + y2*dy;
    z = z + 15.0; // Detector front face + ShowerMax depth                                                     
    //printf("getStarXYZ star XYZ=%f %f %f\n",x,y,z);
  }
  return StThreeVectorF(x,y,z);
}
Float_t StFmsDbMaker::getPhi(Int_t detectorId,Float_t FmsX, Float_t FmsY){ return (getStarXYZ(detectorId,FmsX,FmsY)).phi();}
Float_t StFmsDbMaker::getEta(Int_t detectorId,Float_t FmsX, Float_t FmsY, Float_t Vertex) { return (getStarXYZ(detectorId,FmsX,FmsY)).pseudoRapidity();}

void StFmsDbMaker::setDebug(Int_t debug) {mDebug = debug;}

//!getting the whole table
fmsDetectorPosition_st* StFmsDbMaker::DetectorPosition()  {return mDetectorPosition;}
fmsChannelGeometry_st*  StFmsDbMaker::ChannelGeometry()   {return mChannelGeometry;}
fmsMap_st*              StFmsDbMaker::Map()               {return mMap;}
fmsPatchPanelMap_st*    StFmsDbMaker::PatchPanelMap()     {return mPatchPanelMap;}
fmsQTMap_st*            StFmsDbMaker::QTMap()             {return mQTMap;}
fmsGain_st*             StFmsDbMaker::Gain()              {return mGain;}
fmsGainCorrection_st*   StFmsDbMaker::GainCorrection()    {return mGainCorrection;}
fmsRec_st*              StFmsDbMaker::RecPar()            {return mRecPar;}
fpsConstant_st*         StFmsDbMaker::FpsConstant()       {return mFpsConstant;}
fpsChannelGeometry_st** StFmsDbMaker::FpsChannelGeometry(){return mFpsChannelGeometry;}
fpsSlatId_st*           StFmsDbMaker::FpsSlatId()         {return mFpsSlatId;}
fpsPosition_st*         StFmsDbMaker::FpsPosition()       {return mFpsPosition;}
fpsMap_st*              StFmsDbMaker::FpsMap()            {return mFpsMap;}
fpsGain_st*             StFmsDbMaker::FpsGain()           {return mFpsGain;}

//!ChannelGeometry
Int_t StFmsDbMaker::maxDetectorId()             {return mMaxDetectorId;}
Int_t StFmsDbMaker::eastWest(Int_t detectorId){
  if(detectorId>=0 && detectorId<=mMaxDetectorId && maxChannel(detectorId)>0) return mChannelGeometry[detectorId].ew;
  else{
    //LOG_WARN<<"StFmsDbMaker::eastWest: Corresponding channel geometry not found."<<endm;
    return -1;
  }
}

Int_t StFmsDbMaker::northSouth(Int_t detectorId){
  if(detectorId>=0 && detectorId<=mMaxDetectorId && maxChannel(detectorId)>0) return mChannelGeometry[detectorId].ns;
  else{
    //LOG_WARN<<"StFmsDbMaker::northSouth: Corresponding channel geometry not found."<<endm;
    return -1;
  }
}

Int_t StFmsDbMaker::largeSmall(Int_t detectorId){
  if(detectorId>=kFmsNorthLargeDetId && detectorId<=kFmsSouthLargeDetId && maxChannel(detectorId)>0) return 0;
  if(detectorId>=kFmsNorthSmallDetId && detectorId<=kFmsSouthSmallDetId && maxChannel(detectorId)>0) return 1;
  //LOG_WARN<<"StFmsDbMaker::largeSmall: Corresponding channel geometry not found."<<endm;
  return -1;
}

Int_t StFmsDbMaker::type(Int_t detectorId){
  if(detectorId>=0 && detectorId<=mMaxDetectorId && maxChannel(detectorId)>0) return mChannelGeometry[detectorId].type;
  else{
    //LOG_WARN<<"StFmsDbMaker::type: Corresponding channel geometry not found."<<endm;
    return -1;
  }
}

Int_t StFmsDbMaker::nRow(Int_t detectorId){
  if(detectorId>=0 && detectorId<=mMaxDetectorId && maxChannel(detectorId)>0) return mChannelGeometry[detectorId].nY;
  else{
    //LOG_WARN<<"StFmsDbMaker::nRow: Corresponding channel geometry not found."<<endm;
    return -1;
  }
}

Int_t StFmsDbMaker::nColumn(Int_t detectorId){
  if(detectorId>=0 && detectorId<=mMaxDetectorId && maxChannel(detectorId)>0)
    return mChannelGeometry[detectorId].nX;
  else{
    //LOG_WARN<<"StFmsDbMaker::nColumn: Corresponding channel geometry not found."<<endm;
    return -1;
  }
}

Int_t StFmsDbMaker::maxChannel(Int_t detectorId){
  if(detectorId>=0 && detectorId<=mMaxDetectorId && mChannelGeometry[detectorId].nX>0)
    return mChannelGeometry[detectorId].nX*mChannelGeometry[detectorId].nY;
  else{
    //LOG_WARN<<"StFmsDbMaker::maxChannel: Corresponding channel geometry not found."<<endm;
    return -1;
  }
}

Int_t StFmsDbMaker::detectorId(Int_t ew, Int_t ns, Int_t type){
  for(Int_t i=0; i<=mMaxDetectorId; i++)
    if((mChannelGeometry+i)){
      if(mChannelGeometry[i].ew   == ew && mChannelGeometry[i].ns   == ns && mChannelGeometry[i].type == type)
	return mChannelGeometry[i].detectorId;
    }
  //LOG_WARN<<"StFmsDbMaker::detectorId: Corresponding channel geometry not found."<<endm;
  return -1;
}

Int_t StFmsDbMaker::getRowNumber(Int_t detectorId, Int_t ch){
  if(maxChannel(detectorId)>0) return mChannelGeometry[detectorId].nY - (ch-1)/mChannelGeometry[detectorId].nX;
  return -1;                   //channel# start from top, but row# start from  bottom
}

Int_t StFmsDbMaker::getColumnNumber(Int_t detectorId, Int_t ch){
  if(maxChannel(detectorId)>0) return (ch-1)%mChannelGeometry[detectorId].nX + 1;
  return -1;
}

Int_t StFmsDbMaker::getChannelNumber(Int_t detectorId, Int_t row, Int_t column){
  if(maxChannel(detectorId)>0) return column + mChannelGeometry[detectorId].nX * (mChannelGeometry[detectorId].nY - row);
  return -1;
}

StThreeVectorF StFmsDbMaker::getDetectorOffset(Int_t detectorId){
  if(detectorId>=0 && detectorId<=mMaxDetectorId && maxChannel(detectorId)>0)
    return StThreeVectorF(mDetectorPosition[detectorId].xoffset, mDetectorPosition[detectorId].yoffset, mDetectorPosition[detectorId].zoffset);
  return StThreeVectorF(0, 0, 0);
}

Float_t StFmsDbMaker::getXWidth(Int_t detectorId){
  if(detectorId>=0 && detectorId<=mMaxDetectorId)
  return mDetectorPosition[detectorId].xwidth; return -1;
}

Float_t StFmsDbMaker::getYWidth(Int_t detectorId){
  if(detectorId>=0 && detectorId<=mMaxDetectorId)
  return mDetectorPosition[detectorId].ywidth; return -1;
}

//!fmsMap
Int_t StFmsDbMaker::maxMap() {return mMaxMap;}
void StFmsDbMaker::getMap(Int_t detectorId, Int_t ch, Int_t* qtCrate, Int_t* qtSlot, Int_t* qtChannel){
  if(detectorId<0 || detectorId>mMaxDetectorId || ch<1 || ch>maxChannel(detectorId) || mmMap[detectorId]==0 ){
    *qtCrate=0; *qtSlot=0; *qtChannel=0;
    return;
  }
  *qtCrate   = mmMap[detectorId][ch-1].qtCrate;
  *qtSlot    = mmMap[detectorId][ch-1].qtSlot;
  *qtChannel = mmMap[detectorId][ch-1].qtChannel;
}
void StFmsDbMaker::getReverseMap(Int_t qtCrate, Int_t qtSlot, Int_t qtChannel, Int_t* detectorId, Int_t* ch){
  if(qtCrate==0 && qtSlot==0 && qtChannel==0) {
    *detectorId = 0;
    *ch         = 0;
  }else{
    *detectorId = mReverseMapDetectorId[qtCrate][qtSlot][qtChannel];
    *ch         = mReverseMapChannel[qtCrate][qtSlot][qtChannel];
  }
}

//!fmsPatchPanelMap
Int_t StFmsDbMaker::maxModule() {return mMaxModule;}

//!fmsQTMap()
Int_t StFmsDbMaker::maxNS() {return mMaxNS;}

//!fmsGain/GainCorrection
Int_t StFmsDbMaker::maxGain() {return mMaxGain;}
Int_t StFmsDbMaker::maxGainCorrection() {return mMaxGainCorrection;}
Float_t StFmsDbMaker::getGain(Int_t detectorId, Int_t ch){
  if(detectorId<0 || detectorId>mMaxDetectorId || ch<1 || ch>maxChannel(detectorId) || mmGain[detectorId]==0) return 0;
  return mmGain[detectorId][ch-1].gain;
}
Float_t StFmsDbMaker::getGainCorrection(Int_t detectorId, Int_t ch){
  if(detectorId<0 || detectorId>mMaxDetectorId || ch<1 || ch>maxChannel(detectorId) || mmGainCorrection[detectorId]==0) return 0;
  return mmGainCorrection[detectorId][ch-1].corr;
}

//! fmsTimeDepCorr
float StFmsDbMaker::getTimeDepCorr(int event, int det, int ch){  //det=0-3, ch=1-574
    static int oldEvent=-1;
    static int timeslice=-1;   
    if(mMaxTimeSlice<=0) {
	LOG_INFO << "getTimeDepCorr did not find time dependent correction, returning 1.0"<<endm;
	return 1.0;
    }
    if(event!=oldEvent){
	for(int i=0; i<mMaxTimeSlice; i++){
	    if(event < mTimeDepEvt[i]) {timeslice=i; break;}
	}
	oldEvent=event;
    }
    if(timeslice<0){
	LOG_INFO << Form("getTimeDepCorr did not find time dependent correction for event=%d in %d time slices",event,mMaxTimeSlice)<<endm;
	return 1.0;
    }    
    return mTimeDep[timeslice][det][ch-1];
}


//!text dump for debugging
void StFmsDbMaker::dumpFmsChannelGeometry(const Char_t* filename) {
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    fprintf(fp,"maxDetectorId = %d\n",maxDetectorId());
    fprintf(fp,"    i detiid  ew   ns type nRow nCol maxCh\n");
    for(Int_t i=0; i<mMaxDetectorId+1; i++){    
      fprintf(fp,"%5d%7d%4d%5d%5d%5d%5d%6d\n",
	      i,detectorId(eastWest(i),northSouth(i),type(i)),eastWest(i),northSouth(i),type(i),
	      nRow(i),nColumn(i),maxChannel(i));
    }
    for(Int_t i=0; i<mMaxDetectorId+1; i++){    
      fprintf(fp,"DetectorId=%d\n",i);
      fprintf(fp,"detiid  ch   getCh  getRow getCol\n");
      for(Int_t j=1; j<=maxChannel(i); j++){
	fprintf(fp,"%6d%4d%8d%8d%7d\n",
		i,j,getChannelNumber(i,getRowNumber(i,j),getColumnNumber(i,j)),getRowNumber(i,j),getColumnNumber(i,j));
      }	    
    }
    fclose(fp);
  }
}

void StFmsDbMaker::dumpFmsDetectorPosition(const Char_t* filename) {
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    fprintf(fp,"maxDetectorId = %d\n",maxDetectorId());
    fprintf(fp,"  detiid   zoffset   xoffset yoffset    xwidth    ywidth\n");
    for(Int_t i=0; i<mMaxDetectorId+1; i++)
      if((mDetectorPosition+i))
	fprintf(fp,"%8d%10.1f%10.2f%8.1f%10.3f%10.3f\n", i,getDetectorOffset(i).z(),getDetectorOffset(i).x(),getDetectorOffset(i).y(),getXWidth(i),getYWidth(i));
    fclose(fp);
  }
}

void StFmsDbMaker::dumpFmsMap(const Char_t* filename) {
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    fprintf(fp,"maxMap = %d\n",maxMap());
    fprintf(fp,"    i DetId    ch  crt  slt qtch    getmap()   getReverseMap\n");
    for(Int_t i=0; i<mMaxMap; i++){
      Int_t d=mMap[i].detectorId;
      Int_t c=mMap[i].ch;
      Int_t crt,slot,ch,dd,cc;
      getMap(d,c,&crt,&slot,&ch);
      getReverseMap(crt,slot,ch,&dd,&cc);
      fprintf(fp,"%5d%6d%6d%5d%5d%5d%5d%5d%5d%5d%5d\n",
	      i,d,c,mMap[i].qtCrate,mMap[i].qtSlot,mMap[i].qtChannel,crt,slot,ch,dd,cc);
      if(mMap[i].qtCrate>0 && (d-dd!=0 || c-cc!=0)) fprintf(fp,"Problem in reverse map!\n");
    }
    fclose(fp);
  }      
}

void StFmsDbMaker::dumpFmsPatchPanelMap(const Char_t* filename) {
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    fprintf(fp,"  mod channel ppPanel ppRow ppColumn\n");
    for(Int_t i=0; i<mMaxModule; i++)
      for(Int_t j=0; j<maxChannel(i+8); j++)
	fprintf(fp,"%5d%8d%8d%6d%9d\n",i+1,j+1,mPatchPanelMap[i].ppPanel[j],mPatchPanelMap[i].ppRow[j],mPatchPanelMap[i].ppColumn[j]);
    fclose(fp);
  }
}

void StFmsDbMaker::dumpFmsQTMap(const Char_t* filename) {
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    fprintf(fp,"ns ppPanel   row column crate slot channel\n");
    for(Int_t ns=0; ns<2; ns++)
      for(Int_t pp=0; pp<2; pp++)
	for(Int_t row=0; row<20; row++)
	  for(Int_t col=0; col<16; col++){
	    if(mQTMap[ns].qtCrate[pp][row][col]==0 && mQTMap[ns].qtSlot[pp][row][col]==0 && mQTMap[ns].qtChannel[pp][row][col]==0)
	      fprintf(fp,"-1      -1    -1     -1    -1   -1      -1\n");
	    else
	      fprintf(fp,"%2d%8d%6d%7d%6d%5d%8d\n",ns+1, pp+1, row+1, col+1,mQTMap[ns].qtCrate[pp][row][col],mQTMap[ns].qtSlot[pp][row][col],
		      mQTMap[ns].qtChannel[pp][row][col]);
	  }
    fclose(fp);
  }
}

void StFmsDbMaker::dumpFmsGain(const Char_t* filename) {
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    fprintf(fp,"maxGain = %d\n",maxGain());
    fprintf(fp,"    i DetId    ch    gain  getGain()\n");
    for(Int_t i=0; i<mMaxGain; i++){
      Int_t d=mGain[i].detectorId;
      Int_t c=mGain[i].ch;
      fprintf(fp,"%5d%6d%6d%8.3f%11.3f\n",
	      i,d,c,mGain[i].gain,getGain(d,c));
    }
    fclose(fp);
  }      
}

void StFmsDbMaker::dumpFmsGainCorrection(const Char_t* filename) {
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    fprintf(fp,"maxGainCorrection = %d\n",maxGainCorrection());
    fprintf(fp,"    i DetId    ch    gain  getGainCorrection()\n");
    for(Int_t i=0; i<mMaxGainCorrection; i++){
      Int_t d=mGainCorrection[i].detectorId;
      Int_t c=mGainCorrection[i].ch;
      fprintf(fp,"%5d%6d%6d%8.3f%21.3f\n",
	      i,d,c,mGainCorrection[i].corr,getGainCorrection(d,c));
    }
    fclose(fp);
  }      
}

void StFmsDbMaker::dumpFmsTimeDepCorr(const Char_t* filename) {
    FILE* fp;
    LOG_INFO << "Writing "<<filename<<endm;
    if((fp=fopen(filename,"w"))){
	fprintf(fp,"maxTimeSlice = %d\n",mMaxTimeSlice);
	for(Int_t t=0; t<mMaxTimeSlice; t++){
	    fprintf(fp,"%3d %10d %6.3f\n",t,mTimeDepEvt[t],getTimeDepCorr(mTimeDepEvt[t]-1,0,1));
	}
	for(Int_t d=0; d<mFmsTimeDepMaxDet; d++){
	    for(Int_t c=1; c<=mFmsTimeDepMaxCh; c++){		
		fprintf(fp,"%1d %3d :",d,c);
		for(Int_t t=0; t<mMaxTimeSlice; t++){
		    fprintf(fp,"%6.3f ",mTimeDep[t][d][c-1]);
		    if(t%10==9) fprintf(fp,"\n      :");
		}
		fprintf(fp,"\n");
	    }
	}
	fclose(fp);
    }    
}

void StFmsDbMaker::dumpFmsRec(const Char_t* filename) {
  LOG_INFO << "writing "<<filename<<endm;
  mRecConfig.writeMap(filename);

}

inline Int_t StFmsDbMaker::fpsNQuad()     {return mFpsConstant->nQuad;}
inline Int_t StFmsDbMaker::fpsNLayer()    {return mFpsConstant->nLayer;}
inline Int_t StFmsDbMaker::fpsMaxSlat()   {return mFpsConstant->maxSlat;}
inline Int_t StFmsDbMaker::fpsMaxQTaddr() {return mFpsConstant->maxQTaddr;}
inline Int_t StFmsDbMaker::fpsMaxQTch()   {return mFpsConstant->maxQTch;}
inline Int_t StFmsDbMaker::fpsMaxSlatId() {return mMaxSlatId;}

Int_t StFmsDbMaker::fpsNSlat(int quad, int layer) {
  if(quad>0 && quad<fpsNQuad() && layer>0 && layer<fpsNLayer()) return mFpsChannelGeometry[quad-1][layer-1].nslat;
  return 0;
}

void StFmsDbMaker::fpsQLSfromSlatId(int slatid, int* quad, int* layer, int* slat){
  if(slatid>=0 && slatid<fpsMaxSlatId()){
    *quad =mFpsSlatId[slatid].quad;
    *layer=mFpsSlatId[slatid].layer;
    *slat =mFpsSlatId[slatid].slat;
  }else{
    *quad=0; *layer=0; *slat=0;
  }
}

Int_t StFmsDbMaker::fpsSlatId(int quad, int layer, int slat) {
  if(quad>0 && quad<=fpsNQuad() && layer>0 && layer<=fpsNLayer() && slat>0 && slat<=fpsMaxSlat()){
    return mFpsReverseSlatId[quad-1][layer-1][slat-1];
  }
  return -1;
}

Int_t StFmsDbMaker::fpsSlatIdFromG2t(int g2tvolid){
  int q = (g2tvolid/1000)%10;
  int l = (g2tvolid/100)%10;
  int s = g2tvolid%100;
  return fpsSlatId(q,l,s);
}

void StFmsDbMaker::fpsPosition(int slatid, float xyz[3], float dxyz[3]){
  if(slatid>=0 && slatid<fpsMaxSlatId()){
    xyz[0]=mFpsPosition[slatid].xoffset;
    xyz[1]=mFpsPosition[slatid].yoffset;
    xyz[2]=mFpsPosition[slatid].zoffset;
    dxyz[0]=mFpsPosition[slatid].xwidth;
    dxyz[1]=mFpsPosition[slatid].ywidth;
    dxyz[2]=mFpsPosition[slatid].zwidth;
    return;
  }
  memset(xyz,0,sizeof(*xyz)); 
  memset(dxyz,0,sizeof(*dxyz));
}

inline void StFmsDbMaker::fpsPosition(int quad, int layer, int slat, float xyz[3], float dxyz[3]){
  fpsPosition(fpsSlatId(quad,layer,slat),xyz,dxyz);
}

void StFmsDbMaker::fpsQTMap(int slatid, int* QTaddr, int* QTch){
  if(slatid>=0 && slatid<fpsMaxSlatId()){
    *QTaddr=mFpsMap[slatid].QTaddr;
    *QTch=mFpsMap[slatid].QTch;
    return;
  }
  *QTaddr=-1; 
  *QTch=-1;
}

Int_t StFmsDbMaker::fpsSlatidFromQT(int QTaddr, int QTch){
  if(QTaddr>=0 && QTaddr<fpsMaxQTaddr() && QTch>=0 && QTch<fpsMaxQTch()){
    return mFpsReverseMap[QTaddr][QTch];
  }
  return -1;
}

inline void StFmsDbMaker::fpsQLSFromQT(int QTaddr, int QTch, int* quad, int* layer, int* slat){
  int slatid=fpsSlatidFromQT(QTaddr,QTch);
  fpsQLSfromSlatId(slatid,quad,layer,slat);    
}

Float_t StFmsDbMaker::fpsGain(int slatid){
  if(slatid>=0 && slatid<fpsMaxSlatId()) return mFpsGain[slatid].MIP;
  return 0.0;
}

inline Float_t StFmsDbMaker::fpsGain(int quad, int layer, int slat){
  return fpsGain(fpsSlatId(quad,layer,slat));
}

UShort_t StFmsDbMaker::fpsStatus(int slatid){
  if(slatid>=0 && slatid<fpsMaxSlatId()) return mFpsStatus[slatid].status;
  return 999;
}

inline UShort_t StFmsDbMaker::fpsStatus(int quad, int layer, int slat){
  return fpsStatus(fpsSlatId(quad,layer,slat));
}

void StFmsDbMaker::dumpFpsConstant(const Char_t* filename){
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    fprintf(fp,"nQuad     = %d\n",fpsNQuad());
    fprintf(fp,"nLayer    = %d\n",fpsNLayer());
    fprintf(fp,"maxSlat   = %d\n",fpsMaxSlat());
    fprintf(fp,"maxQTAddr = %d\n",fpsMaxQTaddr());
    fprintf(fp,"maxQTch   = %d\n",fpsMaxQTch());
    fprintf(fp,"maxSlatId = %d\n",fpsMaxSlatId());
    fclose(fp);
  }
}

void StFmsDbMaker::dumpFpsChannelGeometry (const Char_t* filename){
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    for(int q=1; q<=fpsNQuad(); q++){
      for(int l=1; l<=fpsNLayer(); l++){
	fprintf(fp,"Q=%1d L=%1d NLayer=%2d\n",q,l,fpsNSlat(q,l));
      }
    }
    fclose(fp);
  }
}

void StFmsDbMaker::dumpFpsSlatId (const Char_t* filename){
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){    
    for(int i=0; i<fpsMaxSlatId(); i++){
      int q,l,s,id;
      fpsQLSfromSlatId(i,&q,&l,&s);
      id=fpsSlatId(q,l,s);
      fprintf(fp,"SlatId=%3d Q=%1d L=%1d S=%2d Reversemap=%3d\n",i,q,l,s,id);	      
      if(i!=id) fprintf(fp,"Reversemap did not work!!!\n");
    }
    fclose(fp);
  }
}

void StFmsDbMaker::dumpFpsPosition(const Char_t* filename){
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    for(int i=0; i<fpsMaxSlatId(); i++){
      int q,l,s;
      float x[3],d[3];
      fpsQLSfromSlatId(i,&q,&l,&s);
      fpsPosition(q,l,s,x,d);
      fprintf(fp,"SlatId=%3d Q=%1d L=%1d S=%2d xyz=%8.3f %8.3f %8.3f dxyz=%8.3f %8.3f %8.3f\n",
	      i,q,l,s,x[0],x[1],x[2],d[0],d[1],d[2]);
    }
    fclose(fp);
  }
}

void StFmsDbMaker::dumpFpsMap(const Char_t* filename){
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    fprintf(fp,"SlatId ordered\n");
    for(int i=0; i<fpsMaxSlatId(); i++){
      int a,c,q,l,s;
      fpsQTMap(i,&a,&c);
      fpsQLSFromQT(a,c,&q,&l,&s);
      int id=fpsSlatId(q,l,s);
      fprintf(fp,"SlatId=%3d Q=%1d L=%1d S=%2d QTAddr=%2d QTch=%2d\n",
              i,q,l,s,a,c);      
      if(id!=i)  fprintf(fp,"Reversemap did not work!!!\n");
    }
    fprintf(fp,"QT ordered\n");
    for(int a=0; a<fpsMaxQTaddr(); a++){
      for(int c=0; c<fpsMaxQTch(); c++){
	int q,l,s;
	fpsQLSFromQT(a,c,&q,&l,&s);
	int id=fpsSlatId(q,l,s);
	fprintf(fp,"QTAddr=%2d QTch=%2d SlatId=%3d Q=%1d L=%1d S=%2d\n",
		a,c,id,q,l,s);
      }
    }
    fclose(fp);
  }
}

void StFmsDbMaker::dumpFpsGain(const Char_t* filename){
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    for(int i=0; i<fpsMaxSlatId(); i++){
      int q,l,s;
      fpsQLSfromSlatId(i,&q,&l,&s);
      float g = fpsGain(q,l,s);
      fprintf(fp,"SlatId=%3d Q=%1d L=%1d S=%2d MIP=%8.3f\n",
	      i,q,l,s,g);
    }
    fclose(fp);
  }
}

void StFmsDbMaker::dumpFpsStatus(const Char_t* filename){
  FILE* fp;
  LOG_INFO << "Writing "<<filename<<endm;
  if((fp=fopen(filename,"w"))){
    for(int i=0; i<fpsMaxSlatId(); i++){
      int q,l,s;
      fpsQLSfromSlatId(i,&q,&l,&s);
      int g = fpsStatus(q,l,s);
      fprintf(fp,"SlatId=%3d Q=%1d L=%1d S=%2d Status=%d\n",
              i,q,l,s,g);
    }
    fclose(fp);
  }
}

inline Int_t StFmsDbMaker::nCellHole(Int_t det){
    switch(det){
    case kFmsNorthLargeDetId: 
    case kFmsSouthLargeDetId: return 8; 
    case kFmsNorthSmallDetId:
    case kFmsSouthSmallDetId: return 5;
    default: return 0;
    }
}	   

inline Int_t StFmsDbMaker::nCellCorner(Int_t det){
    switch(det){
    case kFmsNorthLargeDetId: 
    case kFmsSouthLargeDetId: return 7; 
    default: return 0;
    }
}	   

Float_t StFmsDbMaker::distanceFromEdge(StFmsPoint* point, int& edge){
    return distanceFromEdge(point->detectorId(),point->x(),point->y(), edge);
}

Float_t StFmsDbMaker::distanceFromEdge(Int_t det,Float_t x, Float_t y, int& edge){
    //Input x/y should be local coordinate in cm, NOT STAR coordinate
    //  x=0 is closest to beam edge, and x=nColumn(det) is far from beam edge
    //  y=0 is bottom edge, and y=nRow(det) is top edge
    //Return value is distance from edge in cell width unit
    //  negative is inside detector, positive is outside
    //  typical fiducail volume cut would be distance<-0.5.
    //  Return int edge is 0=inside detector, 1=inner edge, 2=outer, 3=north south gap, 4=large/small edge, 5=corner
    edge=-1;
    if(det<kFmsNorthLargeDetId || det>kFmsSouthSmallDetId) return -99.0;
    //convert input float(x/y) to int(column/row) space
    //for row#, subtract nRow/2 so 0 is at beam, and take abs to work on a quadrant  
    float xx=x/getXWidth(det);
    float yy=abs(y/getYWidth(det) - nRow(det)/2.0);    
    int column=int(xx); 
    int row=int(yy);    
    if(column<=0){
	if(row>nCellHole(det) && row<nRow(det)/2-1){ //north-south gap
	    edge=3;
	    return -xx;
	}
	if((row==nCellHole(det)) && ((yy-nCellHole(det))>xx) ){ //has to do diagonal cut for edge
	    edge=3;
            return -xx;
	}
	if( (row==(nRow(det)/2-1)) && ((nRow(det)/2-yy)>xx) ){ //has to do diagonal cut for edge
	    edge=3;
	    return -xx;
	}
    }
    if(row>=nRow(det)/2-1.0){ //top or bottom edge
	if(largeSmall(det)==0) {edge=2;}
	else                   {edge=4;}
	return yy - nRow(det)/2;	
    }
    if(column>=nColumn(det)-1.0){ //far from beam edge
	if(largeSmall(det)==0) {edge=2;}
	else                   {edge=4;}
	return xx - nColumn(det);
    }
    if(xx>yy && column<=nCellHole(det)){ //edge inner hole at side
	if(largeSmall(det)==0) {edge=4;}
	else                   {edge=1;}
	return nCellHole(det)-xx;
    }
    if(xx<yy && row<=nCellHole(det)) { //edge to inner hole below/above beam pipe
	if(largeSmall(det)==0) {edge=4;}
	else                   {edge=1;}
	return nCellHole(det)-yy;
    }
    if(largeSmall(det)==0 && xx+yy>=nRow(det)-nCellCorner(det)-2.0){ //corner for large cell
	edge=5;
	//approximation... just diagonal cut
	// not doing distance from zigzag for now... maybe later	
	return xx + yy - (nRow(det)-nCellCorner(det)-1.0);
    } 
    //more than 1 cell inside
    edge=0;
    return -1.0;
}


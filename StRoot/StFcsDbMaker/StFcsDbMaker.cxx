/***************************************************************************
 * $id: StFcsDbMaker.cxx,v 1.22 2020/12/17 21:01:04 akio Exp $
 * \author: akio ogawa
 ***************************************************************************
 *
 * Description: This maker is the interface between FCS and the STAR database
 *
 ***************************************************************************
 *
 * $Log: StFcsDbMaker.cxx,v $
 * Revision 1.2  2021/05/27 14:02:24  akio
 * clean up Clear and fixGain/corr
 *
 * Revision 1.1  2021/03/30 13:40:07  akio
 * FCS code after peer review and moved from $CVSROOT/offline/upgrades/akio
 *
 * Revision 1.32  2021/02/25 21:53:50  akio
 * Int_t -> int
 *
 * Revision 1.31  2021/02/24 22:56:19  akio
 * Modified for STAR code review (Dmitry)
 *
 * Revision 1.30  2021/02/23 22:18:23  akio
 * Modified for STAr code review (Jason)
 *
 * Revision 1.29  2021/02/12 20:09:50  akio
 * Adding getIdfromSCmap()
 *
 * Revision 1.28  2021/02/09 21:54:23  akio
 * Using StEnumeration
 *
 * Revision 1.27  2021/02/05 17:23:25  akio
 * Adding access to STAR offline DB tables.
 * Adding getFromName/getDetFromName from David.
 *
 * Revision 1.26  2021/01/05 18:15:01  akio
 * added setPedestal()
 *
 * Revision 1.25  2020/12/30 20:45:20  akio
 * fix format
 *
 * Revision 1.24  2020/12/30 20:34:38  akio
 * also modify getName for DEP
 *
 * Revision 1.23  2020/12/30 20:17:55  akio
 * adding SC map access
 *
 * Revision 1.22  2020/12/17 21:01:04  akio
 * fix slt problem in sc map
 *
 * Revision 1.21  2020/09/03 19:43:20  akio
 * Updating SC map and adding patchpanel & cable color map
 *
 * Revision 1.20  2020/07/24 17:23:31  akio
 * EPD mip value from 1.6MeV to 2.0MeV
 *
 * Revision 1.19  2020/05/29 18:53:40  akio
 * Adding EPD as PRES maps, STAR coordinate for 4x4 trigger patch, renming map files to be used for DAQ as Tonko specifies
 *
 * Revision 1.18  2020/05/04 15:49:39  akio
 * adding gain for EPD as PRES
 *
 * Revision 1.17  2020/05/04 15:48:22  akio
 * adding input file for DAQ
 *
 * Revision 1.16  2019/10/23 20:05:39  akio
 * bug fixed for getOffset for det=1
 *
 * Revision 1.15  2019/10/23 19:20:10  akio
 * fix det=0 bug for getGain/gaincorr
 *
 * Revision 1.14  2019/10/23 13:34:38  akio
 * Adding getZDepth, and take out Ecal front space (for SiPM/Fee) from offsets
 * so that x/z offsets are now pointing to actual ecal tower front & near beam corner.
 *
 * Revision 1.13  2019/08/01 18:36:06  akio
 * Bug fix which was causing id=0 to get gain=1.0 always
 *
 * Revision 1.12  2019/07/10 06:13:34  akio
 * Adding reading of gains from text files
 *
 * Revision 1.11  2019/07/08 15:53:28  akio
 * updating sampling fraction number by Ting & Liu's study
 *
 * Revision 1.10  2019/06/27 16:10:32  akio
 * adding getLocalXYinCell
 *
 * Revision 1.9  2019/06/25 16:38:59  akio
 * Fixed y offset for run19
 * Added setting run# and time dependent (for preshower yoffset only for now)
 *
 * Revision 1.8  2019/06/21 17:28:47  akio
 * dealing with 5cm offsent when leakyHcal
 *
 * Revision 1.7  2019/06/07 18:16:54  akio
 * *** empty log message ***
 *
 * Revision 1.6  2019/05/16 16:08:32  akio
 * going back to 2019 to full gepmetry
 *
 * Revision 1.5  2019/03/22 14:28:35  akio
 * adding map for 2019
 *
 * Revision 1.4  2019/03/13 20:46:19  akio
 * formatting
 *
 * Revision 1.3  2019/03/13 20:29:30  akio
 * update for run19
 *
 * Revision 1.2  2019/02/05 22:00:18  akio
 * fix NorthSouth()
 *
 * Revision 1.1  2018/11/14 16:50:13  akio
 * FCS codes in offline/upgrade/akio
 *
 *
 **************************************************************************/

#include "StFcsDbMaker.h"
#include "StFcsDb.h"
#include "StFcsDbPulse.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StMessMgr.h"

#include "tables/St_fcsDetectorPosition_Table.h"
#include "tables/St_fcsEcalGain_Table.h"
#include "tables/St_fcsHcalGain_Table.h"
#include "tables/St_fcsPresGain_Table.h"
#include "tables/St_fcsEcalGainCorr_Table.h"
#include "tables/St_fcsHcalGainCorr_Table.h"
#include "tables/St_fcsPresValley_Table.h"
#include "tables/St_vertexSeed_Table.h"

ClassImp(StFcsDbMaker)

StFcsDbMaker::StFcsDbMaker(const char *name) : StMaker(name){
  LOG_INFO << "******** StFcsDbMaker::StFcsDbMaker = "<<name<<endm;
  mFcsDb = new StFcsDb("fcsDb");
  AddData(mFcsDb,".const");
  mFcsDbPulse = new StFcsDbPulse("fcsPulse");
  AddData(mFcsDbPulse,".const");
}; 

StFcsDbMaker::~StFcsDbMaker() {
  //delete mFcsDb; //already deleted by chain because AddData in constructor
  //delete mFcsDbPulse; //already deleted by chain because AddData in constructor
}

int StFcsDbMaker::Init(){
  mFcsDb->Init();
  mFcsDbPulse->Init();
  return kStOK;
  return StMaker::Init();
}

void StFcsDbMaker::Clear(Option_t *option){
  StMaker::Clear(option);
}

int StFcsDbMaker::Make(){
  return StMaker::Make();
}

int StFcsDbMaker::InitRun(int runNumber) {
  LOG_INFO << "StFcsDbMaker::InitRun - run = " << runNumber << endm;
  
  if(mDbAccess){
    St_db_Maker* dbmaker = (St_db_Maker*)GetMaker("db");
    if(dbmaker){
      LOG_INFO << "StFcsDbMaker::InitRun - Date&time from St_db_Maker="<<dbmaker->GetDate()<<","<< dbmaker->GetTime() << endm;       
    }else{
      LOG_ERROR << "StFcsDbMaker::InitRun - No St_db_Maker"<<endm; return kStFatal;
    }
    
    //Get to Geometry/fcs
    TDataSet *DBgeom = 0;    
    St_fcsDetectorPosition *dbFcsDetPos=0;
    DBgeom = GetInputDB("Geometry/fcs");
    if(!DBgeom){
      LOG_ERROR << "StFcsDbMaker::InitRun - No Geometry/fcs"<<endm;
    }else{
      dbFcsDetPos = (St_fcsDetectorPosition*)DBgeom ->Find("fcsDetectorPosition");
    }
    if(!dbFcsDetPos){
      LOG_ERROR << "StFcsDbMaker::InitRun - No Geometry/fcs/fcsDetectorPosition"<<endm;
      mFcsDb->setFcsDetectorPosition(0);
    }else{
        mFcsDb->setFcsDetectorPosition((fcsDetectorPosition_st*) dbFcsDetPos->GetTable());
    }
    
    //Get to Calibrations/fcs
    TDataSet *DBcalib = 0;    
    St_fcsEcalGain         *dbFcsEcalGain     =0; 
    St_fcsHcalGain         *dbFcsHcalGain     =0; 
    St_fcsPresGain         *dbFcsPresGain     =0; 
    St_fcsEcalGainCorr     *dbFcsEcalGainCorr =0; 
    St_fcsHcalGainCorr     *dbFcsHcalGainCorr =0; 
    St_fcsPresValley       *dbFcsPresValley   =0;	
    DBcalib = GetInputDB("Calibrations/fcs");
    if(!DBcalib){
      LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs"<<endm; 
    }else{	  
      dbFcsEcalGain     = (St_fcsEcalGain*)        DBcalib->Find("fcsEcalGain");
      dbFcsHcalGain     = (St_fcsHcalGain*)        DBcalib->Find("fcsHcalGain");
      dbFcsPresGain     = (St_fcsPresGain*)        DBcalib->Find("fcsPresGain");
      dbFcsEcalGainCorr = (St_fcsEcalGainCorr*)    DBcalib->Find("fcsEcalGainCorr");
      dbFcsHcalGainCorr = (St_fcsHcalGainCorr*)    DBcalib->Find("fcsHcalGainCorr");
      dbFcsPresValley   = (St_fcsPresValley*)      DBcalib->Find("fcsPresValley");
    }
    
    //Ecal Gain
    if(!dbFcsEcalGain) {
      LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsEcalGain"<<endm;
      mFcsDb->setFcsEcalGain(0);
    }else{
      mFcsDb->setFcsEcalGain((fcsEcalGain_st*) dbFcsEcalGain->GetTable());
    }
    //Hcal Gain
    if(!dbFcsHcalGain) {
      LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsHcalGain"<<endm;
      mFcsDb->setFcsHcalGain(0);
    }else{
      mFcsDb->setFcsHcalGain((fcsHcalGain_st*) dbFcsHcalGain->GetTable());
    }
    //Pres Gain
    if(!dbFcsPresGain) {
      LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsPresGain"<<endm;
      mFcsDb->setFcsPresGain(0);
    }else{
      mFcsDb->setFcsPresGain((fcsPresGain_st*) dbFcsPresGain->GetTable());
    }
    //Ecal GainCorr
    if(!dbFcsEcalGainCorr) {
      LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsEcalGainCorr"<<endm;
      mFcsDb->setFcsEcalGainCorr(0);
    }else{
      mFcsDb->setFcsEcalGainCorr((fcsEcalGainCorr_st*) dbFcsEcalGainCorr->GetTable());
    }
    //Hcal GainCorr
    if(!dbFcsHcalGainCorr) {
      LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsHcalGainCorr"<<endm;
      mFcsDb->setFcsHcalGainCorr(0);
    }else{
      mFcsDb->setFcsHcalGainCorr((fcsHcalGainCorr_st*) dbFcsHcalGainCorr->GetTable());
    }
    //Pres Valley
    if(!dbFcsPresValley) {
      LOG_ERROR << "StFcsDbMaker::InitRun - No Calibration/fcs/fcsPresValley"<<endm;
      mFcsDb->setFcsPresValley(0);
    }else{
      mFcsDb->setFcsPresValley((fcsPresValley_st*) dbFcsPresValley->GetTable());
    }

    mFcsDb->InitRun(runNumber);  
  }

  return kStOK;
}

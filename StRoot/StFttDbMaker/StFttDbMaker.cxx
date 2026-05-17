/***************************************************************************
 * StFttDbMaker.cxx
 * jdb & Zhen
 ***************************************************************************
 * Description: This maker is the interface between FTT and the STAR database
 ***************************************************************************/

#include "StFttDbMaker.h"
#include "StFttDb.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StMessMgr.h"

ClassImp(StFttDbMaker)

StFttDbMaker::StFttDbMaker(const char *name) : StMaker(name){
  LOG_INFO << "******** StFttDbMaker::StFttDbMaker = "<<name<<endm;
  mFttDb = new StFttDb("fttDb");
  AddData(mFttDb,".const");
}; 

StFttDbMaker::~StFttDbMaker() {
  delete mFttDb;
}

int StFttDbMaker::Init(){
  mFttDb->Init();
  return StMaker::Init();
}

void StFttDbMaker::Clear(Option_t *option){
  StMaker::Clear(option);
}

int StFttDbMaker::Make(){
  return StMaker::Make();
}

int StFttDbMaker::InitRun(int runNumber) {
  LOG_INFO << "StFttDbMaker::InitRun - run = " << runNumber << endm;

    // Strip geometry (centers / edges / lengths) for the retired FTT hardware
    // is compiled into StFttDb and loaded in its constructor — no external
    // text files are required.

    std::ifstream file("vmm_map.dat");
    if(file.is_open()){ // debugging / calibration only
        file.close();
        LOG_INFO << "Loading Hardware Map from FILE!!" << endm;
        LOG_INFO << "Remove / rename file to load from DB" << endm;
        mFttDb->loadHardwareMapFromFile( "vmm_map.dat" );
        // mFttDb->loadHardwareMapFromFile( "/star/u/wangzhen/sTGC/Commissioning/ClusterFinder/PointMaker_building_test_0616/star-sw-1/StRoot/StFwdTrackMaker/macro/vmm_map.dat" );
    } else { // default

        TDataSet *mDbDataSet = GetDataBase("Geometry/ftt/fttHardwareMap");
        if (mDbDataSet){
          St_fttHardwareMap *dataset = (St_fttHardwareMap*) mDbDataSet->Find("fttHardwareMap");
          mFttDb->loadHardwareMapFromDb( dataset );
        } else {
          LOG_WARN << "Cannot access Geometry/ftt/fttHardwareMap and no local map given" << endm;
        }
    }

    loadDataWindows();

  return kStOK;
}

void StFttDbMaker::loadDataWindows(){
  TDataSet *mDbDataSetDW = GetDataBase("Calibrations/ftt/fttDataWindowsB");

    if ( mDbDataSetDW ) {
        LOG_INFO << "StFttDbMaker::loadDataWindows from database" << endm;
        St_fttDataWindowsB *dataset = (St_fttDataWindowsB*) mDbDataSetDW->Find("fttDataWindowsB");
        mFttDb->loadDataWindowsFromDb( dataset );
    } else {
      LOG_WARN << "Cannot access Calibrations/ftt/fttDataWindowsB" << endm;
    }
}
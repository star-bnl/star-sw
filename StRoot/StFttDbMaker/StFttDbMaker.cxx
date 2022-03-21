/***************************************************************************
 * StFttDbMaker.cxx
 * jdb
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


    std::ifstream file("vmm_map.dat");
    if(file.is_open()){ // debugging / calibration only
        file.close();
        std::cout << "Loading Hardware Map from FILE!!" << std::endl;
        std::cout << "Remove / rename file to load from DB" << std::endl;
        mFttDb->loadHardwareMapFromFile( "vmm_map.dat" );
    } else { // default

        TDataSet *mDbDataSet = GetDataBase("Geometry/ftt/fttHardwareMap");
        St_fttHardwareMap *dataset = (St_fttHardwareMap*) mDbDataSet->Find("fttHardwareMap");
        mFttDb->loadHardwareMapFromDb( dataset );
    }


    TDataSet *mDbDataSetDW = GetDataBase("Calibrations/ftt/fttDataWindows");
    
    if ( mDbDataSetDW ) {
        St_fttDataWindows *dataset = (St_fttDataWindows*) mDbDataSetDW->Find("fttDataWindows");
        mFttDb->loadDataWindowsFromDb( dataset );
    }

  

  return kStOK;
}
#include "StFstDbMaker/StFstDbMaker.h"
#include "StFstDbMaker/StFstDb.h"
#include "St_base/StMessMgr.h"
#include "St_db_Maker/St_db_Maker.h"

#include "tables/St_Survey_Table.h"
#include "tables/St_fstPedNoise_Table.h"
#include "tables/St_fstGain_Table.h"
#include "tables/St_fstMapping_Table.h"
#include "tables/St_fstControl_Table.h"
#include "tables/St_fstChipConfig_Table.h"

ClassImp(StFstDbMaker)


/**
 * \author Yaping Wang
 * \date Oct. 2021
 */
StFstDbMaker::StFstDbMaker(const char *name) : StMaker(name), mFstDb(new StFstDb()), mReady(kStErr)
{
}


Int_t StFstDbMaker::Init()
{
   ToWhiteConst("fst_db", mFstDb);

   return kStOk;
}


Int_t StFstDbMaker::InitRun(Int_t runNumber)
{
   mReady = kStFatal;

   LOG_DEBUG << "StFstDbMaker::InitRun() - Access data from database" << endm;

   // Get FST positionment relative to TPC
   St_Survey *st_fstOnTpc = (St_Survey *) GetDataBase("Geometry/fst/fstOnTpc");

   if (!st_fstOnTpc) {
      LOG_ERROR << "No relevant entry found in 'Geometry/fst/fstOnTpc' table."
         " StFstDb object will not be created" << endm;
      return kStErr;
   }
   st_fstOnTpc->Print(0,1);

   // Get half of the support structure (HSS) positionments relative to FST
   St_Survey *st_fstHssOnFst = (St_Survey *) GetDataBase("Geometry/fst/hssOnFst");

   if (!st_fstHssOnFst) {
      LOG_ERROR << "No relevant entry found in 'Geometry/fst/hssOnFst' table."
         " StFstDb object will not be created" << endm;
      return kStErr;
   }
   st_fstHssOnFst->Print(0,1);

   // Get wedge positionments relative to HSS
   St_Survey *st_fstWedgeOnHss = (St_Survey *) GetDataBase("Geometry/fst/fstWedgeOnHss");

   if (!st_fstWedgeOnHss) {
      LOG_ERROR << "No relevant entry found in 'Geometry/fst/fstWedgeOnHss' table."
         " StFstDb object will not be created" << endm;
      return kStErr;
   }
   st_fstWedgeOnHss->Print(0,1);

   // Get sensor positionments relative to wedge
   St_Survey *st_fstSensorOnWedge = (St_Survey *) GetDataBase("Geometry/fst/fstSensorOnWedge");

   if (!st_fstSensorOnWedge) {
      LOG_ERROR << "No relevant entry found in 'Geometry/fst/fstSensorOnWedge' table."
         " StFstDb object will not be created" << endm;
      return kStErr;
   }
   st_fstSensorOnWedge->Print(0,1);

   Survey_st *tables[4] = {st_fstOnTpc->GetTable(), st_fstHssOnFst->GetTable(),
                           st_fstWedgeOnHss->GetTable(), st_fstSensorOnWedge->GetTable()
                          };
   mFstDb->setGeoHMatrices(tables);

   // Now access FST pedestal and noise tables
   St_fstPedNoise *mPedNoise = (St_fstPedNoise *) GetDataBase("Calibrations/fst/fstPedNoise");

   if (!mPedNoise) {
      LOG_ERROR << "No relevant entry found in 'Calibrations/fst/fstPedNoise' table."
         " StFstDb object will not be created" << endm;
      return kStErr;
   }

   mFstDb->setPedNoise(mPedNoise->GetTable());
   if(Debug()) mPedNoise->Print(0,1);

   // Access FST gain table
   St_fstGain *mGain = (St_fstGain *) GetDataBase("Calibrations/fst/fstGain");

   if (!mGain) {
      LOG_ERROR << "No relevant entry found in 'Calibrations/fst/fstGain' table."
         " StFstDb object will not be created" << endm;
      return kStErr;
   }

   mFstDb->setGain(mGain->GetTable());
   if(Debug()) mGain->Print(0,1);

   St_fstMapping *mMapping = (St_fstMapping *) GetDataBase("Calibrations/fst/fstMapping");

   if (!mMapping) {
      LOG_ERROR << "No relevant entry found in 'Calibrations/fst/fstMapping' table."
         " StFstDb object will not be created" << endm;
      return kStErr;
   }

   mFstDb->setMapping(mMapping->GetTable());
   if(Debug()) mMapping->Print(0,1);

   // Access FST control table
   St_fstControl *mControl = (St_fstControl *) GetDataBase("Calibrations/fst/fstControl");

   if (!mControl) {
      LOG_ERROR << "No relevant entry found in 'Calibrations/fst/fstControl' table."
         " StFstDb object will not be created" << endm;
      return kStErr;
   }

   mFstDb->setControl(mControl->GetTable());
   mControl->Print(0,1);

   // Access FST chip status table
   St_fstChipConfig *mChipConfig = (St_fstChipConfig *) GetDataBase("Calibrations/fst/fstChipConfig");

   if (!mChipConfig) {
      LOG_ERROR << "No relevant entry found in 'Calibrations/fst/fstChipConfig' table."
         " StFstDb object will not be created" << endm;
      return kStErr;
   }

   mFstDb->setChipStatus(mChipConfig->GetTable());
   mChipConfig->Print(0,1);
                                    
   if ( GetDebug() >= 2)
      mFstDb->Print();

   mReady = kStOK;

   return kStOK;
}


Int_t StFstDbMaker::Make()
{
   return mReady;
}

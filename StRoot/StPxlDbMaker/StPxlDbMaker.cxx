/*!
 * \class StPxlDbMaker
 * \author J. Bouchet, M. Lomnitz, May 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDbMaker.cxx,v 1.18 2015/02/04 07:55:41 smirnovd Exp $
 *
 * Author: J. Bouchet, M. Lomnitz, May 2013
 ***************************************************************************
 *
 * Description:
 * Read DB and prepare information on pxl geometry and sensor/row/column status
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlDbMaker.cxx,v $
 * Revision 1.18  2015/02/04 07:55:41  smirnovd
 * Create StPxlDb object in constructor and pass it to the framework via ToWhiteConst() in Init()
 *
 * It makes perfect sense to do it this way because the StPxlDb obect is created
 * once by the maker and later reused/updated only at every new run.
 *
 * Revision 1.17  2014/11/19 18:29:47  genevb
 * Use flags to indicate DbMaker readiness
 *
 * Revision 1.16  2014/10/25 00:51:11  qiuh
 * replace ToWhiteBoard with ToWhiteConst to fix a chain crush
 *
 * Revision 1.15  2014/10/07 19:25:28  smirnovd
 * StPxlDbMaker/: Collected all debugging print statements into a single Print() which is called only when Debug2 option is specified
 *
 * Revision 1.14  2014/08/27 16:52:14  qiuh
 * change pxlRowColumnStatus to pxlBadRowColumns to decrease DB szie
 *
 * Revision 1.13  2014/05/07 22:19:19  smirnovd
 * Change the name of PXL DB dataset to avoid conflict with StPxlDbMaker's name
 *
 * Revision 1.12  2014/04/01 15:28:18  qiuh
 * add single hot pixel masking
 *
 * Revision 1.11  2014/01/28 19:29:37  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#include "StPxlDbMaker.h"
#include "StPxlDb.h"
#include "StMessMgr.h"
#ifndef __NEW_PXLDB__
#include "tables/St_Survey_Table.h"
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_pxlSensorStatus_Table.h"
#include "tables/St_pxlRowColumnStatus_Table.h"
#include "tables/St_pxlBadRowColumns_Table.h"
#include "tables/St_pxlHotPixels_Table.h"
#include "tables/St_pxlControl_Table.h"
#include "tables/St_pxlSensorTps_Table.h"
#endif /* __NEW_PXLDB__ */
#include "TEnv.h"
#include "TSystem.h"

ClassImp(StPxlDbMaker)
//_____________________________________________________________________________
StPxlDbMaker::StPxlDbMaker(const char *name) :
   StMaker(name), mPxlDb(new StPxlDb()), mReady(kStErr)
{
   readAllRowColumnStatus = 0;
}


Int_t StPxlDbMaker::Init()
{
   ToWhiteConst("pxl_db", mPxlDb);
   const Char_t *TableNames[] = {"Geometry/pxl/idsOnTpc",
				 "Geometry/pxl/pstOnIds",
				 "Geometry/pxl/pxlOnPst",
				 "Geometry/pxl/pxlHalfOnPxl",
				 "Geometry/pxl/pxlSectorOnHalf",
				 "Geometry/pxl/pxlLadderOnSector",
				 "Geometry/pxl/pxlSensorOnLadder"};
   return kStOk;
}


//_____________________________________________________________________________
Int_t StPxlDbMaker::InitRun(Int_t runNumber)
{
   mReady = kStFatal;
   // set geoHMatrices
#ifndef __NEW_PXLDB__
   Survey_st *tables[7];
   for (Int_t i = 0; i < 7; i++) {
     St_Survey *survey = (St_Survey *) GetDataBase(TableNames[i]);
     if (! survey)  {LOG_WARN << TableNames[i] << " has not been found"  << endm; return kStErr;}
     Survey_st *tables[i] = survey->GetTable();
   }
   mPxlDb->setGeoHMatrices(tables);
#else /* __NEW_PXLDB__ */
   mPxlDb->setGeoHMatrices();
#endif /* __NEW_PXLDB__ */

   // set status tables
#ifndef __NEW_PXLDB__
   St_pxlSensorStatus *sensorStatus = (St_pxlSensorStatus *)GetDataBase("Calibrations/pxl/pxlSensorStatus");
   if (sensorStatus) mPxlDb->setSensorStatus(sensorStatus->GetTable());
   else {LOG_WARN << " no pxl sensor status table " << endm; return kStErr;}

#endif /* ! __NEW_PXLDB__ */
   if(readAllRowColumnStatus) // old method
       {
#ifndef __NEW_PXLDB__
           St_pxlRowColumnStatus *rowColumnStatus = (St_pxlRowColumnStatus *)GetDataBase("Calibrations/pxl/pxlRowColumnStatus");
           if (rowColumnStatus) mPxlDb->setRowColumnStatus(rowColumnStatus->GetTable());
           else {LOG_WARN << " no pxl row column status table " << endm; return kStErr;}
#endif /* __NEW_PXLDB__ */
       }
   else
       {
#ifndef __NEW_PXLDB__
           St_pxlBadRowColumns *badRowColumns = (St_pxlBadRowColumns *)GetDataBase("Calibrations/pxl/pxlBadRowColumns");
           if (badRowColumns) mPxlDb->setBadRowColumns(badRowColumns->GetTable());
           else {LOG_WARN << " no pxl bad row columns table " << endm; return kStErr;}
#else /* __NEW_PXLDB__ */
           St_pxlBadRowColumns *badRowColumns = (St_pxlBadRowColumns *) St_pxlBadRowColumnsC::instance()->Table();
           if (badRowColumns) mPxlDb->setBadRowColumns(badRowColumns->GetTable());
#endif /* __NEW_PXLDB__ */
       }

#ifndef __NEW_PXLDB__
   St_pxlHotPixels *hotPixels = (St_pxlHotPixels *)GetDataBase("Calibrations/pxl/pxlHotPixels");
   if (hotPixels) mPxlDb->setHotPixels(hotPixels->GetTable());
   else {LOG_WARN << " no pxl hot pixels table " << endm; return kStErr;}
#else /* __NEW_PXLDB__ */
   St_pxlHotPixels *hotPixels = (St_pxlHotPixels *) St_pxlHotPixelsC::instance()->Table();
#endif /* __NEW_PXLDB__ */

#ifndef __NEW_PXLDB__
   // set pxlControl
   St_pxlControl *pxlControl = (St_pxlControl *)GetDataBase("Geometry/pxl/pxlControl");
   if (pxlControl) {
      mPxlDb->setPxlControl(pxlControl->GetTable());
   }
   else {
      LOG_WARN << "InitRun : No access to pxlControl table, abort PXL reconstruction" << endm;
      return kStErr;
   }
#endif /* __NEW_PXLDB__ */

   // create and set thin plate functions
#ifndef __NEW_PXLDB__
   TDataSet *dbTps = 0;
   dbTps = GetDataBase("Geometry/pxl/pxlSensorTps");
   if (!dbTps) {
      LOG_WARN << "no tps table found in db, or malformed local db config " << endm;
      return kStErr;
   }

   St_pxlSensorTps *datasetTps = 0;
   datasetTps = (St_pxlSensorTps *) dbTps->Find("pxlSensorTps");
   if (datasetTps) {
      mPxlDb->setThinPlateSpline(datasetTps->GetTable());
   }
   else {
      LOG_WARN << "ERROR: dataset does not contain tps table" << endm;
      return kStErr;
   }

#else /* __NEW_PXLDB__ */
   St_pxlSensorTps *datasetTps = (St_pxlSensorTps *) St_pxlSensorTpsC::instance()->Table();
   mPxlDb->setThinPlateSpline(datasetTps->GetTable());
#endif /* __NEW_PXLDB__ */
   if ( GetDebug() >= 2)
      mPxlDb->Print();

   mReady = kStOK;

   return kStOK;
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::Make()
{
   return mReady;
}

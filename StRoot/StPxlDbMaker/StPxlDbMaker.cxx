/*!
 * \class StPxlDbMaker
 * \author J. Bouchet, M. Lomnitz, May 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDbMaker.cxx,v 1.21 2018/03/29 23:07:10 dongx Exp $
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
 * Revision 1.21  2018/03/29 23:07:10  dongx
 * Added print-out information to check loaded tables
 *
 * Revision 1.20  2018/03/15 21:33:07  dongx
 * *** empty log message ***
 *
 * Revision 1.19  2017/09/01 03:10:49  dongx
 * Added access functions for pxlDigmapsSim table
 *
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
#include "tables/St_Survey_Table.h"
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_pxlSensorStatus_Table.h"
#include "tables/St_pxlRowColumnStatus_Table.h"
#include "tables/St_pxlBadRowColumns_Table.h"
#include "tables/St_pxlHotPixels_Table.h"
#include "tables/St_pxlControl_Table.h"
#include "tables/St_pxlSensorTps_Table.h"
#include "tables/St_pxlDigmapsSim_Table.h"
#include "tables/St_pxlSimPar_Table.h"

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

   return kStOk;
}


//_____________________________________________________________________________
Int_t StPxlDbMaker::InitRun(Int_t runNumber)
{
   mReady = kStFatal;

   // set geoHMatrices
   St_Survey *idsOnTpc          = (St_Survey *) GetDataBase("Geometry/pxl/idsOnTpc");
   if (! idsOnTpc)          {LOG_WARN << "idsOnTpc has not been found"  << endm; return kStErr;}
   idsOnTpc->Print(0,1);

   St_Survey *pstOnIds          = (St_Survey *) GetDataBase("Geometry/pxl/pstOnIds");
   if (! pstOnIds)          {LOG_WARN << "pstOnIds has not been found"  << endm; return kStErr;}
   pstOnIds->Print(0,1);

   St_Survey *pxlOnPst          = (St_Survey *) GetDataBase("Geometry/pxl/pxlOnPst");
   if (! pxlOnPst)          {LOG_WARN << "pxlOnPst has not been found"  << endm; return kStErr;}
   pxlOnPst->Print(0,1);

   St_Survey *pxlHalfOnPxl         = (St_Survey *) GetDataBase("Geometry/pxl/pxlHalfOnPxl");
   if (! pxlHalfOnPxl)         {LOG_WARN << "pxlHalfOnPxl has not been found"  << endm; return kStErr;}
   pxlHalfOnPxl->Print(0,1);

   St_Survey *pxlSectorOnHalf     = (St_Survey *) GetDataBase("Geometry/pxl/pxlSectorOnHalf");
   if (! pxlSectorOnHalf)     {LOG_WARN << "pxlSectorOnHalf has not been found"  << endm; return kStErr;}
   pxlSectorOnHalf->Print(0,1);

   St_Survey *pxlLadderOnSector  = (St_Survey *) GetDataBase("Geometry/pxl/pxlLadderOnSector");
   if (! pxlLadderOnSector)  {LOG_WARN << "pxladderOnSector has not been found"  << endm; return kStErr;}
   pxlLadderOnSector->Print(0,1);

   St_Survey *pxlSensorOnLadder  = (St_Survey *) GetDataBase("Geometry/pxl/pxlSensorOnLadder");
   if (! pxlSensorOnLadder)  {LOG_WARN << "pxlSensorOnLadder has not been found"  << endm; return kStErr;}
   pxlSensorOnLadder->Print(0,1);

   Survey_st *tables[7] = {idsOnTpc->GetTable(), pstOnIds->GetTable(), pxlOnPst->GetTable(), pxlHalfOnPxl->GetTable(),
                           pxlSectorOnHalf->GetTable(), pxlLadderOnSector->GetTable(), pxlSensorOnLadder->GetTable()
                          };

   mPxlDb->setGeoHMatrices(tables);

   // set status tables
   St_pxlSensorStatus *sensorStatus = (St_pxlSensorStatus *)GetDataBase("Calibrations/pxl/pxlSensorStatus");
   if (sensorStatus) mPxlDb->setSensorStatus(sensorStatus->GetTable());
   else {LOG_WARN << " no pxl sensor status table " << endm; return kStErr;}
   sensorStatus->Print(0,1);

   if(readAllRowColumnStatus) // old method
       {
           St_pxlRowColumnStatus *rowColumnStatus = (St_pxlRowColumnStatus *)GetDataBase("Calibrations/pxl/pxlRowColumnStatus");
           if (rowColumnStatus) mPxlDb->setRowColumnStatus(rowColumnStatus->GetTable());
           else {LOG_WARN << " no pxl row column status table " << endm; return kStErr;}
           if(Debug()) rowColumnStatus->Print(0,1);
       }
   else
       {
           St_pxlBadRowColumns *badRowColumns = (St_pxlBadRowColumns *)GetDataBase("Calibrations/pxl/pxlBadRowColumns");
           if (badRowColumns) mPxlDb->setBadRowColumns(badRowColumns->GetTable());
           else {LOG_WARN << " no pxl bad row columns table " << endm; return kStErr;}
           if(Debug()) badRowColumns->Print(0,1);
       }

   St_pxlHotPixels *hotPixels = (St_pxlHotPixels *)GetDataBase("Calibrations/pxl/pxlHotPixels");
   if (hotPixels) mPxlDb->setHotPixels(hotPixels->GetTable());
   else {LOG_WARN << " no pxl hot pixels table " << endm; return kStErr;}
   if(Debug()) hotPixels->Print(0,1);
   
   // set pxlControl
   St_pxlControl *pxlControl = (St_pxlControl *)GetDataBase("Geometry/pxl/pxlControl");
   if (pxlControl) {
      mPxlDb->setPxlControl(pxlControl->GetTable());
      pxlControl->Print(0,1);
   }
   else {
      LOG_WARN << "InitRun : No access to pxlControl table, abort PXL reconstruction" << endm;
      return kStErr;
   }

   // set pxlDigmapsSim
   St_pxlDigmapsSim *pxlDigmapsSim = (St_pxlDigmapsSim *)GetDataBase("Geometry/pxl/pxlDigmapsSim");
   if (pxlDigmapsSim) {
      mPxlDb->setPxlDigmapsSim(pxlDigmapsSim->GetTable());
      pxlDigmapsSim->Print(0,1);
   }
   else {
      LOG_WARN << "InitRun : No access to pxlDigmapsSim table, abort PXL reconstruction" << endm;
      return kStErr;
   }

   // set pxlSimPar
   St_pxlSimPar *pxlSimPar = (St_pxlSimPar *)GetDataBase("Calibrations/pxl/pxlSimPar");
   if (pxlSimPar) {
      mPxlDb->setPxlSimPar(pxlSimPar->GetTable());
      pxlSimPar->Print(0,1);
   }
   else {
      LOG_WARN << "InitRun : No access to pxlSimPar table, abort PXL reconstruction" << endm;
      return kStErr;
   }

   // create and set thin plate functions
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
      if(Debug()) datasetTps->Print(0,1);
   }
   else {
      LOG_WARN << "ERROR: dataset does not contain tps table" << endm;
      return kStErr;
   }

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

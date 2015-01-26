/*!
 * \class StPxlDbMaker
 * \author J. Bouchet, M. Lomnitz, May 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDbMaker.cxx,v 1.17 2014/11/19 18:29:47 genevb Exp $
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

#include "StDetectorDbMaker/StIstSurveyC.h"
#include "StDetectorDbMaker/StPxlSurveyC.h"
#include "StDetectorDbMaker/St_pxlSensorStatusC.h"
#include "StDetectorDbMaker/St_pxlRowColumnStatusC.h"
#include "StDetectorDbMaker/St_pxlBadRowColumnsC.h"
#include "StDetectorDbMaker/St_pxlHotPixelsC.h"
#include "StDetectorDbMaker/St_pxlControlC.h"
#include "StDetectorDbMaker/St_pxlSensorTpsC.h"

ClassImp(StPxlDbMaker)
//_____________________________________________________________________________
StPxlDbMaker::StPxlDbMaker(const char *name) :
   StMaker(name), mPxlDb(0), mReady(kStErr)
{
   readAllRowColumnStatus = 0;
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::InitRun(Int_t runNumber)
{
   mReady = kStFatal;
   if (!mPxlDb) mPxlDb = new StPxlDb();
   // set geoHMatrices
   St_Survey *idsOnTpc          = (St_Survey *) StidsOnTpc::instance()->Table();
   St_Survey *pstOnIds          = (St_Survey *) StPxlpstOnIds::instance()->Table();
   St_Survey *pxlOnPst          = (St_Survey *) StpxlOnPst::instance()->Table();
   St_Survey *pxlHalfOnPxl      = (St_Survey *) StpxlHalfOnPxl::instance()->Table();
   St_Survey *pxlSectorOnHalf   = (St_Survey *) StpxlSectorOnHalf::instance()->Table();
   St_Survey *pxlLadderOnSector = (St_Survey *) StpxlLadderOnSector::instance()->Table();
   St_Survey *pxlSensorOnLadder = (St_Survey *) StpxlSensorOnLadder::instance()->Table();
   Survey_st *tables[7] = {idsOnTpc->GetTable(), pstOnIds->GetTable(), pxlOnPst->GetTable(), pxlHalfOnPxl->GetTable(),
                           pxlSectorOnHalf->GetTable(), pxlLadderOnSector->GetTable(), pxlSensorOnLadder->GetTable()
                          };

   mPxlDb->setGeoHMatrices(tables);

   // set status tables
   St_pxlSensorStatus *sensorStatus = (St_pxlSensorStatus *) St_pxlSensorStatusC::instance()->Table();
   mPxlDb->setSensorStatus(sensorStatus->GetTable());
   if(readAllRowColumnStatus) // old method
       {
	 St_pxlRowColumnStatus *rowColumnStatus = (St_pxlRowColumnStatus *) St_pxlRowColumnStatusC::instance()->Table();
       }
   else
       {
           St_pxlBadRowColumns *badRowColumns = (St_pxlBadRowColumns *) St_pxlBadRowColumnsC::instance()->Table();
       }

   St_pxlHotPixels *hotPixels = (St_pxlHotPixels *) St_pxlHotPixelsC::instance()->Table();

   // set pxlControl
   St_pxlControl *pxlControl = (St_pxlControl *) St_pxlControlC::instance()->Table();
   mPxlDb->setPxlControl(pxlControl->GetTable());

   // create and set thin plate functions
   St_pxlSensorTps *datasetTps = (St_pxlSensorTps *) St_pxlSensorTpsC::instance()->Table();
   mPxlDb->setThinPlateSpline(datasetTps->GetTable());
   if ( GetDebug() >= 2)
      mPxlDb->Print();

   // finally write the data
   ToWhiteConst("pxl_db", mPxlDb);

   mReady = kStOK;

   return kStOK;
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::Make()
{
   return mReady;
}

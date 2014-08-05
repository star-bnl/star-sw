/***************************************************************************
*
* $Id: StIstDbMaker.cxx,v 1.16 2014/08/05 17:48:58 ypwang Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description:
* See header file.
****************************************************************************
*
* $Log: StIstDbMaker.cxx,v $
* Revision 1.16  2014/08/05 17:48:58  ypwang
* update Print() function to PrintGeoHMatrices()
*
* Revision 1.15  2014/08/01 22:15:04  ypwang
* mIstDb geometry matrices print out when Debug2 enabled
*
* Revision 1.14  2014/07/31 21:00:36  ypwang
* c++ format style improvements; virtual keyword added for destructor
*
* Revision 1.13  2014/07/31 18:24:03  ypwang
* add destructor and deallocate the mIstDb; c++ formatting style improvements and formatted with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.12  2014/07/29 19:50:25  ypwang
* IST DB dataset in order to separate from IST Db maker
*
* Revision 1.11  2014/07/15 23:17:51  smirnovd
* Improved doxygen documentation
*
* Revision 1.10  2014/03/27 22:46:46  smirnovd
* Updated broken style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.9  2014/03/27 22:46:38  smirnovd
* Renamed static data member according to mixed star/root convention
*
* Revision 1.8  2014/03/25 03:01:57  ypwang
* get rid of GetIstPedNoise(), GetIstGain(), GetIstMapping() and GetIstControl() functions; use TDataSet instead of Db table structure
*
* Revision 1.7  2014/03/24 15:49:48  ypwang
* checks added and const pointers returned for GetIstPedNoise, GetIstGain, GetIstMapping and GetIstControl functions
*
* Revision 1.6  2014/03/13 22:10:12  smirnovd
* Move some constants from StIstUtil/StIstConsts.h to StEvent/StEnumerations.h to avoid external dependance of StEvent on StIstUtil
*
* Revision 1.5  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstDbMaker.cxx,v 1.0
* Revision 1.0 2013/11/04 16:15:30 Yaping
* Initial version
****************************************************************************/

#include "StIstDbMaker.h"
#include "StIstDb.h"
#include "StMessMgr.h"
#include "St_db_Maker/St_db_Maker.h"

#include "tables/St_Survey_Table.h"
#include "tables/St_istPedNoise_Table.h"
#include "tables/St_istGain_Table.h"
#include "tables/St_istMapping_Table.h"
#include "tables/St_istControl_Table.h"
#include "tables/St_istChipConfig_Table.h"

ClassImp(StIstDbMaker)
//_____________________________________________________________________________
StIstDbMaker::StIstDbMaker(const char *name) : StMaker(name)
{
   mIstDb = NULL;
}
//_____________________________________________________________________________
StIstDbMaker::~StIstDbMaker()
{
   if ( mIstDb ) { delete mIstDb; }
}
//_____________________________________________________________________________
Int_t StIstDbMaker::InitRun(Int_t runNumber)
{
   if ( !mIstDb ) { mIstDb = new StIstDb(); }

   LOG_DEBUG << " StIstDbMaker::InitRun() --> Set geoHMatrices" << endm;

   //get IDS positionment relative to TPC
   St_Survey *st_idsOnTpc = (St_Survey *) GetDataBase("Geometry/ist/idsOnTpc");

   if (!st_idsOnTpc) {
      LOG_ERROR << "idsOnTpc has not been found"  << endl;
      return kStErr;
   }

   //get PST positionment relative to IDS
   St_Survey *st_pstOnIds = (St_Survey *) GetDataBase("Geometry/ist/pstOnIds");

   if (!st_pstOnIds) {
      LOG_ERROR << "pstOnIds has not been found"  << endl;
      return kStErr;
   }

   //get IST positionment relative to PST
   St_Survey *st_istOnPst = (St_Survey *) GetDataBase("Geometry/ist/istOnPst");

   if (!st_istOnPst) {
      LOG_ERROR << "istOnPst has not been found"  << endl;
      return kStErr;
   }

   //get ladder positionments relative to IST
   St_Survey *st_istLadderOnIst = (St_Survey *) GetDataBase("Geometry/ist/istLadderOnIst");

   if (!st_istLadderOnIst) {
      LOG_ERROR << "istLadderOnIst has not been found"  << endl;
      return kStErr;
   }

   //get sensor positionments relative to ladder
   St_Survey *st_istSensorOnLadder = (St_Survey *) GetDataBase("Geometry/ist/istSensorOnLadder");

   if (!st_istSensorOnLadder) {
      LOG_ERROR << "istSensorOnLadder has not been found"  << endl;
      return kStErr;
   }

   Survey_st *tables[5] = {st_idsOnTpc->GetTable(), st_pstOnIds->GetTable(), st_istOnPst->GetTable(),
                           st_istLadderOnIst->GetTable(), st_istSensorOnLadder->GetTable()
                          };
   mIstDb->SetGeoHMatrices(tables);


   LOG_DEBUG << " StIstDbMaker::InitRun() --> Get IST Pedestal and Noise Table" << endm;
   St_istPedNoise *mPedNoise = (St_istPedNoise *)GetDataBase("Calibrations/ist/istPedNoise");

   if ( mPedNoise ) {
      mIstDb->SetPedNoise(mPedNoise->GetTable());
   }
   else {
      LOG_ERROR << "StIstDbMaker: No input pedestal/noise data set!" << endm;
      return kStErr;
   }

   LOG_DEBUG << " StIstDbMaker::InitRun() --> Get IST Gain Table" << endm;
   St_istGain *mGain = (St_istGain *)GetDataBase("Calibrations/ist/istGain");

   if ( mGain ) {
      mIstDb->SetGain(mGain->GetTable());
   }
   else {
      LOG_ERROR << "StIstDbMaker: No input gain data set!" << endm;
      return kStErr;
   }

   LOG_DEBUG << " StIstDbMaker::InitRun() --> Get IST Mapping Table" << endm;
   St_istMapping *mMapping = (St_istMapping *)GetDataBase("Calibrations/ist/istMapping");

   if ( mMapping ) {
      mIstDb->SetMapping(mMapping->GetTable());
   }
   else {
      LOG_ERROR << "StIstDbMaker: No input mapping data set!" << endm;
      return kStErr;
   }

   LOG_DEBUG << " StIstDbMaker::InitRun() --> Get IST Control Table" << endm;
   St_istControl *mControl = (St_istControl *)GetDataBase("Calibrations/ist/istControl");

   if ( mControl ) {
      mIstDb->SetControl(mControl->GetTable());
   }
   else {
      LOG_ERROR << "StIstDbMaker: No input control parameter data set!" << endm;
      return kStErr;
   }

   LOG_DEBUG << " StIstDbMaker::InitRun() --> Get IST Chip Status Table" << endm;
   St_istChipConfig *mChipConfig = (St_istChipConfig *)GetDataBase("Calibrations/ist/istChipConfig");

   if ( mChipConfig ) {
      mIstDb->SetChipStatus(mChipConfig->GetTable());
   }
   else {
      LOG_ERROR << "StIstDbMaker: No input chip configuration data set!" << endm;
      return kStErr;
   }

   if ( GetDebug() >= 2) {
      mIstDb->PrintGeoHMatrices();
   }

   //write the data
   ToWhiteBoard("ist_db", mIstDb);

   return kStOK;
}

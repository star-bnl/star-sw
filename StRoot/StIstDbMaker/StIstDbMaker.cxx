/* $Id: StIstDbMaker.cxx,v 1.31 2018/03/29 23:07:30 dongx Exp $ */

#include "StIstDbMaker/StIstDbMaker.h"
#include "StIstDbMaker/StIstDb.h"
#include "St_base/StMessMgr.h"
#include "St_db_Maker/St_db_Maker.h"

#include "tables/St_Survey_Table.h"
#include "tables/St_istPedNoise_Table.h"
#include "tables/St_istGain_Table.h"
#include "tables/St_istMapping_Table.h"
#include "tables/St_istControl_Table.h"
#include "tables/St_istChipConfig_Table.h"
#include "tables/St_istSimPar_Table.h"

ClassImp(StIstDbMaker)


/**
 * \author Yaping Wang
 * \date June 2013
 */
StIstDbMaker::StIstDbMaker(const char *name) : StMaker(name), mIstDb(new StIstDb()), mReady(kStErr)
{
}


Int_t StIstDbMaker::Init()
{
   ToWhiteConst("ist_db", mIstDb);

   return kStOk;
}


Int_t StIstDbMaker::InitRun(Int_t runNumber)
{
   mReady = kStFatal;

   LOG_DEBUG << "StIstDbMaker::InitRun() - Access data from database" << endm;

   // Get IDS positionment relative to TPC
   St_Survey *st_idsOnTpc = (St_Survey *) GetDataBase("Geometry/ist/idsOnTpc");

   if (!st_idsOnTpc) {
      LOG_ERROR << "No relevant entry found in 'Geometry/ist/idsOnTpc' table."
         " StIstDb object will not be created" << endm;
      return kStErr;
   }
   st_idsOnTpc->Print(0,1);

   // Get PST positionment relative to IDS
   St_Survey *st_pstOnIds = (St_Survey *) GetDataBase("Geometry/ist/pstOnIds");

   if (!st_pstOnIds) {
      LOG_ERROR << "No relevant entry found in 'Geometry/ist/pstOnIds' table."
         " StIstDb object will not be created" << endm;
      return kStErr;
   }
   st_pstOnIds->Print(0,1);

   // Get IST positionment relative to PST
   St_Survey *st_istOnPst = (St_Survey *) GetDataBase("Geometry/ist/istOnPst");

   if (!st_istOnPst) {
      LOG_ERROR << "No relevant entry found in 'Geometry/ist/istOnPst' table."
         " StIstDb object will not be created" << endm;
      return kStErr;
   }
   st_istOnPst->Print(0,1);

   // Get ladder positionments relative to IST
   St_Survey *st_istLadderOnIst = (St_Survey *) GetDataBase("Geometry/ist/istLadderOnIst");

   if (!st_istLadderOnIst) {
      LOG_ERROR << "No relevant entry found in 'Geometry/ist/istLadderOnIst' table."
         " StIstDb object will not be created" << endm;
      return kStErr;
   }
   st_istLadderOnIst->Print(0,1);

   // Get sensor positionments relative to ladder
   St_Survey *st_istSensorOnLadder = (St_Survey *) GetDataBase("Geometry/ist/istSensorOnLadder");

   if (!st_istSensorOnLadder) {
      LOG_ERROR << "No relevant entry found in 'Geometry/ist/istSensorOnLadder' table."
         " StIstDb object will not be created" << endm;
      return kStErr;
   }
   st_istSensorOnLadder->Print(0,1);

   Survey_st *tables[5] = {st_idsOnTpc->GetTable(), st_pstOnIds->GetTable(), st_istOnPst->GetTable(),
                           st_istLadderOnIst->GetTable(), st_istSensorOnLadder->GetTable()
                          };
   mIstDb->setGeoHMatrices(tables);

   // Now access IST pedestal and noise tables
   St_istPedNoise *mPedNoise = (St_istPedNoise *) GetDataBase("Calibrations/ist/istPedNoise");

   if (!mPedNoise) {
      LOG_ERROR << "No relevant entry found in 'Calibrations/ist/istPedNoise' table."
         " StIstDb object will not be created" << endm;
      return kStErr;
   }

   mIstDb->setPedNoise(mPedNoise->GetTable());
   if(Debug()) mPedNoise->Print(0,1);

   // Access IST gain table
   St_istGain *mGain = (St_istGain *) GetDataBase("Calibrations/ist/istGain");

   if (!mGain) {
      LOG_ERROR << "No relevant entry found in 'Calibrations/ist/istGain' table."
         " StIstDb object will not be created" << endm;
      return kStErr;
   }

   mIstDb->setGain(mGain->GetTable());
   if(Debug()) mGain->Print(0,1);

   St_istMapping *mMapping = (St_istMapping *) GetDataBase("Calibrations/ist/istMapping");

   if (!mMapping) {
      LOG_ERROR << "No relevant entry found in 'Calibrations/ist/istMapping' table."
         " StIstDb object will not be created" << endm;
      return kStErr;
   }

   mIstDb->setMapping(mMapping->GetTable());
   if(Debug()) mMapping->Print(0,1);

   // Access IST control table
   St_istControl *mControl = (St_istControl *) GetDataBase("Calibrations/ist/istControl");

   if (!mControl) {
      LOG_ERROR << "No relevant entry found in 'Calibrations/ist/istControl' table."
         " StIstDb object will not be created" << endm;
      return kStErr;
   }

   mIstDb->setControl(mControl->GetTable());
   mControl->Print(0,1);

   // Access IST chip status table
   St_istChipConfig *mChipConfig = (St_istChipConfig *) GetDataBase("Calibrations/ist/istChipConfig");

   if (!mChipConfig) {
      LOG_ERROR << "No relevant entry found in 'Calibrations/ist/istChipConfig' table."
         " StIstDb object will not be created" << endm;
      return kStErr;
   }

   mIstDb->setChipStatus(mChipConfig->GetTable());
   mChipConfig->Print(0,1);
   
   // set istSimPar
   St_istSimPar *istSimPar = (St_istSimPar *)GetDataBase("Calibrations/ist/istSimPar");
   if (istSimPar) {
     mIstDb->setIstSimPar(istSimPar->GetTable());
   }
   else {
     LOG_WARN << "InitRun : No access to istSimPar table, abort IST reconstruction" << endm;
     return kStErr;
   }
   istSimPar->Print(0,1);
                                    
   if ( GetDebug() >= 2)
      mIstDb->Print();

   mReady = kStOK;

   return kStOK;
}


Int_t StIstDbMaker::Make()
{
   return mReady;
}


/***************************************************************************
*
* $Log: StIstDbMaker.cxx,v $
* Revision 1.31  2018/03/29 23:07:30  dongx
* Added print-out information for loaded tables
*
* Revision 1.30  2018/03/15 21:35:48  dongx
*
* Added the access to new table istSimPar
*
* Revision 1.29  2015/02/04 07:56:31  smirnovd
* StIstDbMaker: Changed ToWhiteBoard() to ToWhiteConst() because we don't want to loose the StIstDb object at every call to StMaker::Clear()
*
* Revision 1.28  2015/02/04 07:56:19  smirnovd
* Create StIstDb object in constructor and pass it to the framework in Init()
*
* It makes perfect sense to do it this way because the StIstDb obect is created
* once by the maker and later reused/updated only at every new run.
*
* Revision 1.27  2015/02/04 07:56:05  smirnovd
* Revert "Move ToWhiteBoard() to Make". Will implement a different solution to avoid losing istDb from this maker's data container
*
* Revision 1.25  2014/11/19 18:29:47  genevb
* Use flags to indicate DbMaker readiness
*
* Revision 1.24  2014/11/19 04:17:31  genevb
* Return fatal if database tables are not found
*
* Revision 1.23  2014/11/18 23:11:44  smirnovd
* [Style] Changes in comments and user feedback only
*
* Revision 1.22  2014/11/18 23:11:35  smirnovd
* [Minor] Coding style clean-up. Removed unconstructive comments
*
* Revision 1.21  2014/11/18 23:10:27  smirnovd
* Do not destruct StIstDb object as the ownership is passed to the framework
*
* Revision 1.20  2014/11/18 23:10:20  smirnovd
* Renamed printGeoHMatrices to customary Print as that what users of ROOT framework normaly expect
*
* Revision 1.19  2014/11/18 23:08:37  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.18  2014/11/18 19:43:24  genevb
* STAR Logger messages need endm, not endl
*
* Revision 1.17  2014/08/06 18:44:21  ypwang
* replace assert statement for gStTpcDb with normal variable check and LOG_WARN printout; non-ROOT methods formatted with STAR coding style
*
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

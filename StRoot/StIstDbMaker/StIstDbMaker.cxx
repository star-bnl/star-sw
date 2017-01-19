/* $Id: StIstDbMaker.cxx,v 1.29 2015/02/04 07:56:31 smirnovd Exp $ */

#include "StIstDbMaker/StIstDbMaker.h"
#include "StIstDbMaker/StIstDb.h"
#include "St_base/StMessMgr.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StDetectorDbMaker/StIstSurveyC.h"
#include "StDetectorDbMaker/St_istPedNoiseC.h"
#include "StDetectorDbMaker/St_istGainC.h"
#include "StDetectorDbMaker/St_istMappingC.h"
#include "StDetectorDbMaker/St_istControlC.h"
#include "StDetectorDbMaker/St_istChipConfigC.h"
#include "TEnv.h"
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
   const Char_t *TabNames[3] = {
     //     "idsOnTpc", //set in PxlDb
     //     "pstOnIds",
     "istOnPst",
     "istLadderOnIst",
     "istSensorOnLadder"};
   if (gEnv->GetValue("IdealHFT",0) != 0) {
     for (Int_t i = 0; i < 3; i++) {
       SetFlavor("sim",TabNames[i]);
       LOG_INFO << "StIstDbMaker::Init Using \"sim\" flavor for " << TabNames[i] << endm;
     }
   }

   return kStOk;
}


Int_t StIstDbMaker::InitRun(Int_t runNumber)
{
   mReady = kStFatal;
   LOG_DEBUG << "StIstDbMaker::InitRun() - Access data from database" << endm;

   // Get IDS positionment relative to TPC
   const St_Survey *st_idsOnTpc = (const St_Survey *) StidsOnTpc::instance()->Table();
   // Get PST positionment relative to IDS
   const St_Survey *st_pstOnIds = (const St_Survey *) StpstOnIds::instance()->Table();
   // Get IST positionment relative to PST
   const St_Survey *st_istOnPst = (const St_Survey *) StistOnPst::instance()->Table();
   // Get ladder positionments relative to IST
   const St_Survey *st_istLadderOnIst = (const St_Survey *) StLadderOnIst::instance()->Table();
   // Get sensor positionments relative to ladder
   const St_Survey *st_istSensorOnLadder = (const St_Survey *) StistSensorOnLadder::instance()->Table();
   Survey_st *tables[5] = {st_idsOnTpc->GetTable(), st_pstOnIds->GetTable(), st_istOnPst->GetTable(),
                           st_istLadderOnIst->GetTable(), st_istSensorOnLadder->GetTable()
                          };
   mIstDb->setGeoHMatrices(tables);  
   // Now access IST pedestal and noise tables
   const St_istPedNoise *mPedNoise = (St_istPedNoise *) St_istPedNoiseC::instance()->Table();
   mIstDb->setPedNoise(mPedNoise->GetTable());
   // Access IST gain table
   St_istGain *mGain = (St_istGain *) St_istGainC::instance()->Table();
   mIstDb->setGain(mGain->GetTable());
   St_istMapping *mMapping = (St_istMapping *) St_istMappingC::instance()->Table();
   mIstDb->setMapping(mMapping->GetTable());
   // Access IST control table
   St_istControl *mControl = (St_istControl *) St_istControlC::instance()->Table();
   mIstDb->setControl(mControl->GetTable());
   // Access IST chip status table
   St_istChipConfig *mChipConfig = (St_istChipConfig *) St_istChipConfigC::instance()->Table();
   mIstDb->setChipStatus(mChipConfig->GetTable());

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

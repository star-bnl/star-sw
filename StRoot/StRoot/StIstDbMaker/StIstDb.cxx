/* $Id: StIstDb.cxx,v 1.15 2018/03/15 21:35:48 dongx Exp $ */

#include <assert.h>
#include "StIstDb.h"
#include "StMessMgr.h"
#include "StTpcDb/StTpcDb.h"
#include "St_db_Maker/St_db_Maker.h"

#include "tables/St_Survey_Table.h"
#include "tables/St_istPedNoise_Table.h"
#include "tables/St_istGain_Table.h"
#include "tables/St_istMapping_Table.h"
#include "tables/St_istControl_Table.h"
#include "tables/St_istChipConfig_Table.h"
#include "tables/St_istSimPar_Table.h"

THashList *StIstDb::mgRotList = 0;

ClassImp(StIstDb)


/**
 * \author Yaping Wang
 * \date June 2013
 */
StIstDb::StIstDb() : StObject()
{
   mGeoHMatrixTpcOnGlobal = NULL;
   mIstPedNoise = NULL;
   mIstGain	= NULL;
   mIstMapping  = NULL;
   mIstControl  = NULL;
   mIstChipStatus = NULL;
   mIstSimPar = 0;
}
//_____________________________________________________________________________
Int_t StIstDb::setGeoHMatrices(Survey_st **tables)
{
   using namespace StIstConsts;

   SafeDelete(mgRotList);
   mgRotList = new THashList(kIstNumLadders * kIstNumSensorsPerLadder, 0);
   mgRotList->SetOwner(kFALSE);

   //get TPC positionement relative to STAR
   if (gStTpcDb) {
      mGeoHMatrixTpcOnGlobal = (TGeoHMatrix *)&gStTpcDb->Tpc2GlobalMatrix();
   }
   else {
      if (mGeoHMatrixTpcOnGlobal) delete mGeoHMatrixTpcOnGlobal;

      mGeoHMatrixTpcOnGlobal = new TGeoHMatrix("tpcOnGlobal");
      LOG_WARN << "No gStTpcDb, use null transformation for tpc on global" << endm;
   }

   //obtain IST geomery tables
   Survey_st *idsOnTpc          = tables[0];
   Survey_st *pstOnIds          = tables[1];
   Survey_st *istOnPst          = tables[2];
   Survey_st *laddersOnIst      = tables[3];
   Survey_st *sensorsOnLadders  = tables[4];

   mGeoHMatrixIdsOnTpc.SetName("idsOnTpc");
   mGeoHMatrixIdsOnTpc.SetRotation(&idsOnTpc->r00);
   mGeoHMatrixIdsOnTpc.SetTranslation(&idsOnTpc->t0);

   mGeoHMatrixPstOnIds.SetName("pstOnIds");
   mGeoHMatrixPstOnIds.SetRotation(&pstOnIds->r00);
   mGeoHMatrixPstOnIds.SetTranslation(&pstOnIds->t0);

   mGeoHMatrixIstOnPst.SetName("istOnPst");
   mGeoHMatrixIstOnPst.SetRotation(&istOnPst->r00);
   mGeoHMatrixIstOnPst.SetTranslation(&istOnPst->t0);

   for (int i = 0; i < kIstNumSensors; i++, sensorsOnLadders++) {
      int id = sensorsOnLadders->Id;
      TGeoHMatrix *comb = (TGeoHMatrix *) mgRotList->FindObject(Form("R%04i", id));

      if (comb) continue;

      comb = new TGeoHMatrix(Form("R%04i", id));
      int ladder = ((id - 1000) - 1) / kIstNumSensorsPerLadder + 1; // 1 <= ladder <= 24
      int sensor = ((id - 1000) - 1) % kIstNumSensorsPerLadder + 1; // 1 <= sensor <= 6

      if (ladder <= 0 || ladder > kIstNumLadders) {
         LOG_WARN << "Ladder ID is out of range (1 - 24)!" << endm;
         continue;
      }

      if (sensor <= 0 || sensor > kIstNumSensorsPerLadder) {
         LOG_WARN << "Sensor ID is out of range (1 - 6)!" << endm;
         continue;
      }

      //setting rotation/translation for sensor geometry matrix
      mGeoHMatrixSensorOnLadder[ladder - 1][sensor - 1].SetName(Form("sensorOnLadder%4i%4i", ladder, sensor));
      mGeoHMatrixSensorOnLadder[ladder - 1][sensor - 1].SetRotation(&sensorsOnLadders->r00);
      mGeoHMatrixSensorOnLadder[ladder - 1][sensor - 1].SetTranslation(&sensorsOnLadders->t0);

      TGeoHMatrix *sensorLocal = (TGeoHMatrix *) mgRotList->FindObject(Form("sensorLocal%04i", id));

      if (!sensorLocal) {
         sensorLocal = new  TGeoHMatrix(Form("sensorLocal%04i", id));
         sensorLocal->SetRotation(mGeoHMatrixSensorOnLadder[ladder - 1][sensor - 1].GetRotationMatrix());
         sensorLocal->SetTranslation(mGeoHMatrixSensorOnLadder[ladder - 1][sensor - 1].GetTranslation());
         mgRotList->Add(sensorLocal);
      }

      //seeting rotation/translation for ladder geometry matrix
      for (int l = 0; l < kIstNumLadders; l++, laddersOnIst++) {
         if (ladder == laddersOnIst->Id) {
            mGeoHMatrixLadderOnIst[ladder - 1].SetName(Form("ladderOnIst%4i", ladder));
            mGeoHMatrixLadderOnIst[ladder - 1].SetRotation(&laddersOnIst->r00);
            mGeoHMatrixLadderOnIst[ladder - 1].SetTranslation(&laddersOnIst->t0);
            break;
         }
      }

      //calculate sensor global position
      TGeoHMatrix sensorGlobal = (*mGeoHMatrixTpcOnGlobal) * mGeoHMatrixIdsOnTpc * mGeoHMatrixPstOnIds * mGeoHMatrixIstOnPst * mGeoHMatrixLadderOnIst[ladder - 1] * mGeoHMatrixSensorOnLadder[ladder - 1][sensor - 1];

      comb->SetRotation(sensorGlobal.GetRotationMatrix());
      comb->SetTranslation(sensorGlobal.GetTranslation());
      mgRotList->Add(comb);
   }

   return kStOk;
}


/**
 * Returns TGeoHMatrix with complete set of transformations from the sensor
 * local coordinate system to the global one. The ladder and sensor id-s are
 * expected to follow the human friendly numbering scheme, i.e. >= 1.
 */
const TGeoHMatrix *StIstDb::getHMatrixSensorOnGlobal(int ladder, int sensor)
{
   if (ladder < 1 || ladder > kIstNumLadders || sensor < 1 || sensor > kIstNumSensorsPerLadder)
      return 0;

   int id = 1000 + (ladder - 1) * kIstNumSensorsPerLadder + sensor;
   return mgRotList ? (const TGeoHMatrix *) mgRotList->FindObject(Form("R%04i", id)) : 0;
}


void StIstDb::Print(Option_t *opt) const
{
   mGeoHMatrixTpcOnGlobal->Print();
   mGeoHMatrixIdsOnTpc.Print();
   mGeoHMatrixPstOnIds.Print();
   mGeoHMatrixIstOnPst.Print();

   for (Int_t iL = 0; iL < kIstNumLadders; iL++) {
      mGeoHMatrixLadderOnIst[iL].Print();
   }

   for (Int_t iL = 0; iL < kIstNumLadders; iL++) {
      for (Int_t iS = 0; iS < kIstNumSensorsPerLadder; iS++) {
         mGeoHMatrixSensorOnLadder[iL][iS].Print();
      }
   }

   for (Int_t iS = 1; iS <= kIstNumSensors; iS++) {
      TGeoHMatrix *sensorOnGlobal = (TGeoHMatrix *) mgRotList->FindObject(Form("R%04i", iS + 1000));
      sensorOnGlobal->Print();
   }
}


/***************************************************************************
*
* $Log: StIstDb.cxx,v $
* Revision 1.15  2018/03/15 21:35:48  dongx
* Added the access to new table istSimPar
*
* Revision 1.14  2015/08/03 14:26:03  smirnovd
* Corrected style with 'astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f'
*
* Revision 1.13  2014/12/17 19:37:47  smirnovd
* StiIstDb: Corrected mapping of ladder/sensor to global aggregate sensor id.
*
* The global sensor index (id) used in the istSensorOnLadder DB table spans the
* range from 1001 to 1144. This bug was reported by Michael Lomnitz
*
* Revision 1.12  2014/12/12 21:41:09  smirnovd
* StIstDb: Modified getter for sensors transormation matrix to accept ladder and sensor id-s using human friendly numbering starting with 1. The input values outside of possible ranges will return a null pointer
*
* Revision 1.11  2014/11/18 23:11:57  smirnovd
* StIstDb: Added method to access transformation matrix for a given IST ladder/sensor pair
*
* Revision 1.10  2014/11/18 23:10:20  smirnovd
* Renamed printGeoHMatrices to customary Print as that what users of ROOT framework normaly expect
*
* Revision 1.9  2014/11/18 23:08:37  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.8  2014/08/06 18:44:21  ypwang
* replace assert statement for gStTpcDb with normal variable check and LOG_WARN printout; non-ROOT methods formatted with STAR coding style
*
* Revision 1.7  2014/08/05 17:48:58  ypwang
* update Print() function to PrintGeoHMatrices()
*
* Revision 1.6  2014/08/05 15:00:45  ypwang
* minor updates on the ladder/sensor ID check output log, using LOG_WARN instead of cout
*
* Revision 1.5  2014/08/01 22:25:48  ypwang
* Add several simple getters and data members for sub-level geometry matrices obtain; Add Print() function which print out all IST geometry matrices
*
* Revision 1.4  2014/07/31 22:40:59  smirnovd
* StIstDb: Reduced the scope of the using namespace
*
* Revision 1.3  2014/07/31 22:40:52  smirnovd
* StIstDb: Removed unused header includes
*
* Revision 1.2  2014/07/31 18:29:51  ypwang
* replace the LOG_INFO with LOG_DEBUG to slim the log file
*
* Revision 1.1  2014/07/29 19:50:25  ypwang
* IST DB dataset in order to separate from IST Db maker
*
*
*
****************************************************************************
* StIstDb.cxx,v 1.0
* Revision 1.0 2014/7/28 16:15:30 Yaping
* Initial version
****************************************************************************/

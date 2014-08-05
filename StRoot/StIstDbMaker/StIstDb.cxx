/***************************************************************************
*
* $Id: StIstDb.cxx,v 1.6 2014/08/05 15:00:45 ypwang Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description:
* See header file.
****************************************************************************
*
* $Log: StIstDb.cxx,v $
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
/*
  relation within STAR frame
  IstOnGlobal = Tpc2Magnet * Ids2Tpc *    Ist2Ids     * Ladder2Ist * Sensor2Ladder * PS
  with
  Ids2Tpc = IstIdsOnTpc
  Ist2Ids = IstIstOnPst * IstPstOnIds

  Naming of roatation matrices in this maker :
  positionGlobal  = tpc2Global * ids2Tpc * pst2Ids * ist2Pst * ladder2Ist * sensor2Ladder * positionOnSensor

  numbering
  Id  = 1000 + (ladder-1)*6 + sensor
  1<= ladder <= 24
  1<= sensor <= 6
*/

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

THashList *StIstDb::mgRotList = 0;

ClassImp(StIstDb)
//_____________________________________________________________________________
StIstDb::StIstDb() : StObject()
{
   mGeoHMatrixTpcOnGlobal = NULL;
   mIstPedNoise = NULL;
   mIstGain	= NULL;
   mIstMapping  = NULL;
   mIstControl  = NULL;
   mIstChipStatus = NULL;
}
//_____________________________________________________________________________
Int_t StIstDb::SetGeoHMatrices(Survey_st **tables)
{
   using namespace StIstConsts;

   SafeDelete(mgRotList);
   mgRotList = new THashList(kIstNumLadders * kIstNumSensorsPerLadder, 0);
   mgRotList->SetOwner(kFALSE);

   //get TPC positionement relative to STAR
   assert(gStTpcDb);
   mGeoHMatrixTpcOnGlobal = (TGeoHMatrix *)&gStTpcDb->Tpc2GlobalMatrix();

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

void StIstDb::Print() const
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

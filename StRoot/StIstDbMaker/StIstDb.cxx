/***************************************************************************
*
* $Id: StIstDb.cxx,v 1.3 2014/07/31 22:40:52 smirnovd Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description:
* See header file.
****************************************************************************
*
* $Log: StIstDb.cxx,v $
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
#include "StIstUtil/StIstConsts.h"
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


using namespace StIstConsts;

THashList *StIstDb::mgRotList = 0;

ClassImp(StIstDb)
//_____________________________________________________________________________
StIstDb::StIstDb() : StObject()
{
   mIstPedNoise = NULL;
   mIstGain	= NULL;
   mIstMapping  = NULL;
   mIstControl  = NULL;
   mIstChipStatus = NULL;
}
//_____________________________________________________________________________
Int_t StIstDb::SetGeoHMatrices(Survey_st **tables)
{
   SafeDelete(mgRotList);
   mgRotList = new THashList(kIstNumLadders * kIstNumSensorsPerLadder, 0);
   mgRotList->SetOwner(kFALSE);

   TGeoHMatrix ids2Tpc, pst2Ids, ist2Pst, ladder2Ist, sensorGlobal;

   //get TPC positionement relative to STAR
   assert(gStTpcDb);
   const TGeoHMatrix &tpc2Global = gStTpcDb->Tpc2GlobalMatrix();

   //obtain IST geomery tables
   Survey_st *idsOnTpc          = tables[0];
   Survey_st *pstOnIds          = tables[1];
   Survey_st *istOnPst          = tables[2];
   Survey_st *laddersOnIst      = tables[3];
   Survey_st *sensorsOnLadders  = tables[4];

   //get the number of rows of each tables
   int nIds                  = 1; // 1
   int nPst                  = 1; // 1
   int nIst                  = 1; // 1
   int nLadders              = kIstNumLadders; //24
   int nSensors              = kIstNumSensors; //144

   LOG_DEBUG << " # of IDS     : " << nIds << endm;
   LOG_DEBUG << " # of PST     : " << nPst << endm;
   LOG_DEBUG << " # of IST     : " << nIst << endm;
   LOG_DEBUG << " # of Ladders : " << nLadders << endm;
   LOG_DEBUG << " # of Sensors : " << nSensors << endm;

   //setting rotation and translation
   ids2Tpc.SetRotation(&idsOnTpc->r00);
   ids2Tpc.SetTranslation(&idsOnTpc->t0);

   pst2Ids.SetRotation(&pstOnIds->r00);
   pst2Ids.SetTranslation(&pstOnIds->t0);

   ist2Pst.SetRotation(&istOnPst->r00);
   ist2Pst.SetTranslation(&istOnPst->t0);

   LOG_DEBUG << "ids2Tpc :" << endm;
   ids2Tpc.Print();
   LOG_DEBUG << "pst2Ids :" << endm;
   pst2Ids.Print();
   LOG_DEBUG << "ist2Pst :" << endm;
   ist2Pst.Print();

   for (int i = 0; i < nSensors; i++, sensorsOnLadders++) {
      int id = sensorsOnLadders->Id;
      TGeoHMatrix *comb = (TGeoHMatrix *) mgRotList->FindObject(Form("R%04i", id));

      if (comb) continue;

      comb = new TGeoHMatrix(Form("R%04i", id));
      int ladder = ((id - 1000) - 1) / kIstNumSensorsPerLadder + 1; // 1 <= ladder <= 24

      //setting rotation/translation for sensor geometry matrix
      TGeoHMatrix sensor2Ladder;
      sensor2Ladder.SetRotation(&sensorsOnLadders->r00);
      sensor2Ladder.SetTranslation(&sensorsOnLadders->t0);
      TGeoHMatrix *sensorLocal = (TGeoHMatrix *) mgRotList->FindObject(Form("sensorLocal%04i", id));

      if (!sensorLocal) {
         sensorLocal = new  TGeoHMatrix(Form("sensorLocal%04i", id));
         sensorLocal->SetRotation(sensor2Ladder.GetRotationMatrix());
         sensorLocal->SetTranslation(sensor2Ladder.GetTranslation());
         mgRotList->Add(sensorLocal);
      }

      //seeting rotation/translation for ladder geometry matrix
      if (ladder <= 0 || ladder > kIstNumLadders) {
         cout << "Ladder has not been defined!" << endl;
         continue;
      }

      for (int l = 0; l < nLadders; l++, laddersOnIst++) {
         if (ladder == laddersOnIst->Id) {
            ladder2Ist.SetRotation(&laddersOnIst->r00);
            ladder2Ist.SetTranslation(&laddersOnIst->t0);
            break;
         }
      }

      //calculate sensor global position
      sensorGlobal = tpc2Global * ids2Tpc * pst2Ids * ist2Pst * ladder2Ist * sensor2Ladder;

      LOG_DEBUG << "sensorGlobal\tR" << id << endm;
      sensorGlobal.Print();

      comb->SetRotation(sensorGlobal.GetRotationMatrix());
      comb->SetTranslation(sensorGlobal.GetTranslation());
      mgRotList->Add(comb);
   }

   return kStOk;
}

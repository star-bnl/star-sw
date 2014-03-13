/***************************************************************************
*
* $Id: StIstDbMaker.cxx,v 1.6 2014/03/13 22:10:12 smirnovd Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description:
* See header file.
****************************************************************************
*
* $Log: StIstDbMaker.cxx,v $
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
#include "StIstDbMaker.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "StTpcDb/StTpcDb.h"
#include "St_db_Maker/St_db_Maker.h"

#include "tables/St_Survey_Table.h"
#include "tables/St_istPedNoise_Table.h"
#include "tables/St_istGain_Table.h"
#include "tables/St_istMapping_Table.h"
#include "tables/St_istControl_Table.h"

#include "TMath.h"
#include "TVector3.h"

using namespace StIstConsts;

THashList *StIstDbMaker::fRotList = 0;

ClassImp(StIstDbMaker)
//_____________________________________________________________________________
StIstDbMaker::StIstDbMaker(const char *name) : StMaker(name), mPedNoise(NULL), mGain(NULL), mMapping(NULL), mControl(NULL)
{
/* no op */
}
//_____________________________________________________________________________
Int_t StIstDbMaker::InitRun(Int_t runNumber)
{
   LOG_DEBUG << " StIstDbMaker::InitRun() --> Calculate Sensor Position" << endm;
   CalculateSensorsPosition();
   LOG_DEBUG << " StIstDbMaker::InitRun() --> Get IST Pedestal and Noise Table" << endm;
   GetIstPedNoise();
   LOG_DEBUG << " StIstDbMaker::InitRun() --> Get IST Gain Table" << endm;
   GetIstGain();
   LOG_DEBUG << " StIstDbMaker::InitRun() --> Get IST Mapping Table" << endm;
   GetIstMapping();
   LOG_DEBUG << " StIstDbMaker::InitRun() --> Get IST Control Table" << endm;
   GetIstControl();

   return kStOK;
}
//_____________________________________________________________________________
Int_t StIstDbMaker::CalculateSensorsPosition()
{
   SafeDelete(fRotList);
   fRotList = new THashList(kIstNumLadders * kIstNumSensorsPerLadder, 0);
   fRotList->SetOwner(kFALSE);

   TGeoHMatrix ids2Tpc, pst2Ids, ist2Pst, ladder2Ist, sensorGlobal;

   //get TPC positionement relative to STAR
   assert(gStTpcDb);
   const TGeoHMatrix &tpc2Global = gStTpcDb->Tpc2GlobalMatrix();

   //get IDS positionment relative to TPC
   St_Survey *st_idsOnTpc          = (St_Survey *) GetDataBase("Geometry/ist/idsOnTpc");       
   if (!st_idsOnTpc)          {cout << "idsOnTpc has not been found"  << endl; return 0;}

   //get PST positionment relative to IDS
   St_Survey *st_pstOnIds          = (St_Survey *) GetDataBase("Geometry/ist/pstOnIds");       
   if (!st_pstOnIds)          {cout << "pstOnIds has not been found"  << endl; return 0;}

   //get IST positionment relative to PST
   St_Survey *st_istOnPst          = (St_Survey *) GetDataBase("Geometry/ist/istOnPst");        
   if (!st_istOnPst)          {cout << "istOnPst has not been found"  << endl; return 0;}

   //get ladder positionments relative to IST
   St_Survey *st_istLadderOnIst    = (St_Survey *) GetDataBase("Geometry/ist/istLadderOnIst"); 
   if (!st_istLadderOnIst)    {cout << "istLadderOnIst has not been found"  << endl; return 0;}

   //get sensor positionments relative to ladder
   St_Survey *st_istSensorOnLadder = (St_Survey *) GetDataBase("Geometry/ist/istSensorOnLadder");
   if (!st_istSensorOnLadder) {cout << "istSensorOnLadder has not been found"  << endl; return 0;}

   //obtain these tables
   Survey_st *idsOnTpc          = st_idsOnTpc->GetTable();
   Survey_st *pstOnIds          = st_pstOnIds->GetTable();
   Survey_st *istOnPst          = st_istOnPst->GetTable();
   Survey_st *laddersOnIst      = st_istLadderOnIst->GetTable();
   Survey_st *sensorsOnLadders  = st_istSensorOnLadder->GetTable();

   //get the number of rows of each tables
   int nIds                  = st_idsOnTpc->GetNRows(); // 1
   int nPst                  = st_pstOnIds->GetNRows(); // 1
   int nIst                  = st_istOnPst->GetNRows(); // 1
   int nLadders              = st_istLadderOnIst->GetNRows(); //24
   int nSensors              = st_istSensorOnLadder->GetNRows(); //144

   if (Debug() > 2) {
      LOG_DEBUG << " # of IDS     : " << nIds << endm;
      LOG_DEBUG << " # of PST     : " << nPst << endm;
      LOG_DEBUG << " # of IST     : " << nIst << endm;
      LOG_DEBUG << " # of Ladders : " << nLadders << endm;
      LOG_DEBUG << " # of Sensors : " << nSensors << endm;
   }

   //setting rotation and translation
   ids2Tpc.SetRotation(&idsOnTpc->r00);
   ids2Tpc.SetTranslation(&idsOnTpc->t0);

   pst2Ids.SetRotation(&pstOnIds->r00);
   pst2Ids.SetTranslation(&pstOnIds->t0);

   ist2Pst.SetRotation(&istOnPst->r00);
   ist2Pst.SetTranslation(&istOnPst->t0);

   if (Debug() > 2) {
      LOG_DEBUG << "IDS on TPC :" << endm;
      ids2Tpc.Print();
      LOG_DEBUG << "PST on IDS :" << endm;
      pst2Ids.Print();
      LOG_DEBUG << "IST on PST :" << endm;
      ist2Pst.Print();
   }

   for (int i = 0; i < nSensors; i++, sensorsOnLadders++) {
      int id = sensorsOnLadders->Id;
      TGeoHMatrix *comb = (TGeoHMatrix *) fRotList->FindObject(Form("R%04i", id));
      if (comb) continue;
      comb = new TGeoHMatrix(Form("R%04i", id));
      int ladder = ((id - 1000) - 1)/kIstNumSensorsPerLadder + 1;  // 1 <= ladder <= 24

      //setting rotation/translation for sensor geometry matrix
      TGeoHMatrix sensor2Ladder;
      sensor2Ladder.SetRotation(&sensorsOnLadders->r00);
      sensor2Ladder.SetTranslation(&sensorsOnLadders->t0);
      TGeoHMatrix *sensorLocal = (TGeoHMatrix *) fRotList->FindObject(Form("sensorLocal%04i", id));
      if (!sensorLocal) {
         sensorLocal = new  TGeoHMatrix(Form("sensorLocal%04i", id));
         sensorLocal->SetRotation(sensor2Ladder.GetRotationMatrix());
         sensorLocal->SetTranslation(sensor2Ladder.GetTranslation());
         fRotList->Add(sensorLocal);
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
      if (Debug() > 2) {
         cout << "sensorGlobal\t";
         sensorGlobal.Print();
      }

      comb->SetRotation(sensorGlobal.GetRotationMatrix());
      comb->SetTranslation(sensorGlobal.GetTranslation());
      fRotList->Add(comb);
      if (Debug() > 2) {
         comb->Print();
      }
   }
   return kStOk;
}
//_____________________________________________________________________________
void StIstDbMaker::GetIstPedNoise()
{
   mPedNoise = (St_istPedNoise *)GetDataBase("Calibrations/ist/istPedNoise");
}
//_____________________________________________________________________________
void StIstDbMaker::GetIstGain()
{
   mGain = (St_istGain *)GetDataBase("Calibrations/ist/istGain");
}
//_____________________________________________________________________________
void StIstDbMaker::GetIstMapping()
{
   mMapping = (St_istMapping *)GetDataBase("Calibrations/ist/istMapping");
}
//_____________________________________________________________________________
void StIstDbMaker::GetIstControl()
{
   mControl = (St_istControl *)GetDataBase("Calibrations/ist/istControl");
}

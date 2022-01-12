/* $Id: StFstDb.cxx,v */

#include <assert.h>
#include "StFstDb.h"
#include "StMessMgr.h"
#include "StTpcDb/StTpcDb.h"
#include "St_db_Maker/St_db_Maker.h"

#include "tables/St_Survey_Table.h"
#include "tables/St_fstPedNoise_Table.h"
#include "tables/St_fstGain_Table.h"
#include "tables/St_fstMapping_Table.h"
#include "tables/St_fstControl_Table.h"
#include "tables/St_fstChipConfig_Table.h"

THashList *StFstDb::mgRotList = 0;

ClassImp(StFstDb)


/**
 * \author Shenghui Zhang
 * \date Oct 2021
 */
StFstDb::StFstDb() : StObject()
{
  mGeoHMatrixTpcOnGlobal = NULL;
  mFstPedNoise           = NULL;
  mFstGain               = NULL;
  mFstMapping            = NULL;
  mFstControl            = NULL;
  mFstChipStatus         = NULL;
}
//_____________________________________________________________________________
Int_t StFstDb::setGeoHMatrices(Survey_st **tables)
{
   //using namespace StFstConsts;

   SafeDelete(mgRotList);
   mgRotList = new THashList(kFstNumWedges * kFstNumSensorsPerWedge, 0);
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

   //obtain FST geomery tables
   Survey_st *fstOnTpc        = tables[0];
   Survey_st *hssOnFst        = tables[1];
   Survey_st *wedgeOnHss      = tables[2];
   Survey_st *sensorsOnWedges = tables[3];

   mGeoHMatrixFstOnTpc.SetName("fstOnTpc");
   mGeoHMatrixFstOnTpc.SetRotation(&fstOnTpc->r00);
   mGeoHMatrixFstOnTpc.SetTranslation(&fstOnTpc->t0);

   mGeoHMatrixHssOnFst.SetName("hssOnFst");
   mGeoHMatrixHssOnFst.SetRotation(&hssOnFst->r00);
   mGeoHMatrixHssOnFst.SetTranslation(&hssOnFst->t0);

   mGeoHMatrixWedgeOnHss.SetName("wedgeOnHss");
   mGeoHMatrixWedgeOnHss.SetRotation(&wedgeOnHss->r00);
   mGeoHMatrixWedgeOnHss.SetTranslation(&wedgeOnHss->t0);
   for (int i = 0; i < kFstNumSensors; i++, sensorsOnWedges++) {
      int id = sensorsOnWedges->Id;
      TGeoHMatrix *comb = (TGeoHMatrix *) mgRotList->FindObject(Form("R%04i", id));

      if (comb) continue;

      comb = new TGeoHMatrix(Form("R%04i", id));
      int wedge = (id - 1000) / kFstNumSensorsPerWedge + 1; // 1 <= wedge <= 36
      int sensor = (id - 1000) % kFstNumSensorsPerWedge; // 0 <= sensor <= 2
	  cout<<"id"<<id<<" "<<"wedge"<<wedge<<"sensor"<<sensor<<endl;

      if (wedge <= 0 || wedge > kFstNumWedges) {
         LOG_WARN << "Wedge ID is out of range (1 - 36)!" << endm;
         continue;
      }

      if (sensor < 0 || sensor >= kFstNumSensorsPerWedge) {
         LOG_WARN << "Sensor ID is out of range (0 - 2)!" << endm;
         continue;
      }

      //setting rotation/translation for sensor geometry matrix
      mGeoHMatrixSensorOnWedge[wedge - 1][sensor].SetName(Form("sensorOnWedge%4i%4i", wedge, sensor));
      mGeoHMatrixSensorOnWedge[wedge - 1][sensor].SetRotation(&sensorsOnWedges->r00);
      mGeoHMatrixSensorOnWedge[wedge - 1][sensor].SetTranslation(&sensorsOnWedges->t0);

      TGeoHMatrix *sensorLocal = (TGeoHMatrix *) mgRotList->FindObject(Form("sensorLocal%04i", id));

      if (!sensorLocal) {
         sensorLocal = new  TGeoHMatrix(Form("sensorLocal%04i", id));
         sensorLocal->SetRotation(mGeoHMatrixSensorOnWedge[wedge - 1][sensor].GetRotationMatrix());
         sensorLocal->SetTranslation(mGeoHMatrixSensorOnWedge[wedge - 1][sensor].GetTranslation());
         mgRotList->Add(sensorLocal);
      }

      //calculate sensor global position
      TGeoHMatrix sensorGlobal = (*mGeoHMatrixTpcOnGlobal) * mGeoHMatrixFstOnTpc * mGeoHMatrixHssOnFst * mGeoHMatrixWedgeOnHss * mGeoHMatrixSensorOnWedge[wedge - 1][sensor];

      comb->SetRotation(sensorGlobal.GetRotationMatrix());
      comb->SetTranslation(sensorGlobal.GetTranslation());
      mgRotList->Add(comb);
   }

   return kStOk;
}


/**
 * Returns TGeoHMatrix with complete set of transformations from the sensor
 * local coordinate system to the global one. The wedge and sensor id-s are
 * expected to follow the human friendly numbering scheme.
 */
const TGeoHMatrix *StFstDb::getHMatrixSensorOnGlobal(int wedge, int sensor)
{
   if (wedge < 1 || wedge > kFstNumWedges || sensor < 0 || sensor >= kFstNumSensorsPerWedge)
      return 0;

   int id = 1000 + (wedge - 1) * kFstNumSensorsPerWedge + sensor;
   return mgRotList ? (const TGeoHMatrix *) mgRotList->FindObject(Form("R%04i", id)) : 0;
}


void StFstDb::Print(Option_t *opt) const
{
   mGeoHMatrixTpcOnGlobal->Print();
   mGeoHMatrixFstOnTpc.Print();
   mGeoHMatrixHssOnFst.Print();
   mGeoHMatrixWedgeOnHss.Print();

   for (Int_t iL = 0; iL < kFstNumWedges; iL++) {
      for (Int_t iS = 0; iS < kFstNumSensorsPerWedge; iS++) {
         mGeoHMatrixSensorOnWedge[iL][iS].Print();
      }
   }

   for (Int_t iS = 0; iS < kFstNumSensors; iS++) {
      TGeoHMatrix *sensorOnGlobal = (TGeoHMatrix *) mgRotList->FindObject(Form("R%04i", iS + 1000));
      sensorOnGlobal->Print();
   }
}

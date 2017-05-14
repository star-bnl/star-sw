/*!
 * \class StPxlDb
 * \author Qiu Hao, Jan 2014
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDb.cxx,v 1.8 2014/10/07 19:25:28 smirnovd Exp $
 *
 * Author: Qiu Hao, Jan 2014
 ***************************************************************************
 *
 * Description:
 * DB information on pxl geometry and sensor/row/column status
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlDb.cxx,v $
 * Revision 1.8  2014/10/07 19:25:28  smirnovd
 * StPxlDbMaker/: Collected all debugging print statements into a single Print() which is called only when Debug2 option is specified
 *
 * Revision 1.7  2014/08/27 16:52:14  qiuh
 * change pxlRowColumnStatus to pxlBadRowColumns to decrease DB szie
 *
 * Revision 1.6  2014/07/15 23:28:48  smirnovd
 * Minor style changes
 *
 * Revision 1.5  2014/07/15 23:28:34  smirnovd
 * .msg
 *
 * Revision 1.4  2014/04/01 15:28:18  qiuh
 * add single hot pixel masking
 *
 * Revision 1.3  2014/02/27 21:40:19  smirnovd
 * Remove unnecessary print out
 *
 * Revision 1.2  2014/01/28 19:29:37  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#include "StPxlDb.h"
#include "StPxlUtil/StPxlConstants.h"
#include "StPxlUtil/StThinPlateSpline.h"
#include "StMessMgr.h"
#include "tables/St_Survey_Table.h"
#include "StTpcDb/StTpcDb.h"
#ifndef  __NEW_PXLDB__
#include "tables/St_pxlSensorStatus_Table.h"
#include "tables/St_pxlRowColumnStatus_Table.h"
#include "tables/St_pxlBadRowColumns_Table.h"
#include "tables/St_pxlHotPixels_Table.h"
#include "tables/St_pxlSensorTps_Table.h"
#include "tables/St_pxlControl_Table.h"
#else /* __NEW_PXLDB__ */
#include "TEnv.h"
#endif /* ! __NEW_PXLDB__ */


StPxlDb* StPxlDb::fgInstance = 0;
ClassImp(StPxlDb)

//_____________________________________________________________________________
StPxlDb::StPxlDb() : StObject()
{
#ifndef __NEW_PXLDB__
   mGeoHMatrixTpcOnGlobal = 0;
   mSensorStatusTable = 0;
#endif /* __NEW_PXLDB__ */
   mRowColumnStatusTable = 0;
#ifndef __NEW_PXLDB__
   mPxlControl = 0;
#endif /* __NEW_PXLDB__ */
   memset(mThinPlateSpline, 0, sizeof(mThinPlateSpline));
   fgInstance = this;
}
//_____________________________________________________________________________
#ifndef __NEW_PXLDB__
void StPxlDb::setGeoHMatrices(Survey_st **tables)
#else /* __NEW_PXLDB__ */
void StPxlDb::setGeoHMatrices()
#endif /* __NEW_PXLDB__ */
{
#ifndef __NEW_PXLDB__
   if (gStTpcDb)
      mGeoHMatrixTpcOnGlobal = (TGeoHMatrix *)&gStTpcDb->Tpc2GlobalMatrix();
   else {
      if (mGeoHMatrixTpcOnGlobal) delete mGeoHMatrixTpcOnGlobal;
      mGeoHMatrixTpcOnGlobal = new TGeoHMatrix("tpcOnGlobal");
      LOG_WARN << "No gStTpcDb, use null transformation for tpc on global" << endm;
   }

   Survey_st *IdsOnTpc          = tables[0];
   Survey_st *PstOnIds          = tables[1];
   Survey_st *PxlOnPst          = tables[2];
   Survey_st *HalfOnPxl         = tables[3];
   Survey_st *SectorsOnHalf     = tables[4];
   Survey_st *LaddersOnSectors  = tables[5];
   Survey_st *SensorsOnLadders  = tables[6];

#else /* __NEW_PXLDB__ */
   Survey_st *IdsOnTpc          = ((St_Survey *) StidsOnTpc::instance()->Table())->GetTable();	 
   Survey_st *PstOnIds          = ((St_Survey *) StPxlpstOnIds::instance()->Table())->GetTable();	 
   Survey_st *PxlOnPst          = ((St_Survey *) StpxlOnPst::instance()->Table())->GetTable();	 
   Survey_st *HalfOnPxl         = ((St_Survey *) StpxlHalfOnPxl::instance()->Table())->GetTable();	 
   Survey_st *SectorsOnHalf     = ((St_Survey *) StpxlSectorOnHalf::instance()->Table())->GetTable();	 
   Survey_st *LaddersOnSectors  = ((St_Survey *) StpxlLadderOnSector::instance()->Table())->GetTable();
   Survey_st *SensorsOnLadders  = ((St_Survey *) StpxlSensorOnLadder::instance()->Table())->GetTable();
#endif /* __NEW_PXLDB__ */
   mGeoHMatrixIdsOnTpc.SetName("idsOnTpc");
   mGeoHMatrixIdsOnTpc.SetRotation(&IdsOnTpc->r00);
   mGeoHMatrixIdsOnTpc.SetTranslation(&IdsOnTpc->t0);

   mGeoHMatrixPstOnIds.SetName("pstOnIds");
   mGeoHMatrixPstOnIds.SetRotation(&PstOnIds->r00);
   mGeoHMatrixPstOnIds.SetTranslation(&PstOnIds->t0);

   mGeoHMatrixPxlOnPst.SetName("pxlOnPst");
   mGeoHMatrixPxlOnPst.SetRotation(&PxlOnPst->r00);
   mGeoHMatrixPxlOnPst.SetTranslation(&PxlOnPst->t0);

   for (Int_t t = 0; t < 2; t++, HalfOnPxl++) {
      mGeoHMatrixHalfOnPxl[HalfOnPxl->Id - 1].SetName(Form("halfOnPxl%03i", HalfOnPxl->Id));
      mGeoHMatrixHalfOnPxl[HalfOnPxl->Id - 1].SetRotation(&HalfOnPxl->r00);
      mGeoHMatrixHalfOnPxl[HalfOnPxl->Id - 1].SetTranslation(&HalfOnPxl->t0);
   }

   for (int i = 0; i < kNumberOfPxlSectors; i++) {
      mGeoHMatrixSectorOnHalf[SectorsOnHalf->Id - 1].SetName(Form("sectorOnHalf%03i", SectorsOnHalf->Id));
      mGeoHMatrixSectorOnHalf[SectorsOnHalf->Id - 1].SetRotation(&SectorsOnHalf->r00);
      mGeoHMatrixSectorOnHalf[SectorsOnHalf->Id - 1].SetTranslation(&SectorsOnHalf->t0);
      SectorsOnHalf++;
   }

   for (int i = 0; i < kNumberOfPxlSectors; i++)
      for (int j = 0; j < kNumberOfPxlLaddersPerSector; j++) {
         int iSector = (LaddersOnSectors->Id - 1) / kNumberOfPxlLaddersPerSector;
         int iLadder = (LaddersOnSectors->Id - 1) % kNumberOfPxlLaddersPerSector;
         mGeoHMatrixLadderOnSector[iSector][iLadder].SetName(Form("ladderOnSector%03i%03i", iSector + 1, iLadder + 1));
         mGeoHMatrixLadderOnSector[iSector][iLadder].SetRotation(&LaddersOnSectors->r00);
         mGeoHMatrixLadderOnSector[iSector][iLadder].SetTranslation(&LaddersOnSectors->t0);
         LaddersOnSectors++;
      }

   for (int i = 0; i < kNumberOfPxlSectors; i++)
      for (int j = 0; j < kNumberOfPxlLaddersPerSector; j++)
         for (int k = 0; k < kNumberOfPxlSensorsPerLadder; k++) {
            int iSector = (SensorsOnLadders->Id - 1) / kNumberOfPxlLaddersPerSector / kNumberOfPxlSensorsPerLadder;
            int iLadder = ((SensorsOnLadders->Id - 1) / kNumberOfPxlSensorsPerLadder) % kNumberOfPxlLaddersPerSector;
            int iSensor = (SensorsOnLadders->Id - 1) % kNumberOfPxlSensorsPerLadder;
            mGeoHMatrixSensorOnLadder[i][j][k].SetName(Form("sensorOnLadder%03i%03i%03i", iSector + 1, iLadder + 1, iSensor + 1));
            mGeoHMatrixSensorOnLadder[iSector][iLadder][iSensor].SetRotation(&SensorsOnLadders->r00);
            mGeoHMatrixSensorOnLadder[iSector][iLadder][iSensor].SetTranslation(&SensorsOnLadders->t0);
            SensorsOnLadders++;
         }

   for (int i = 0; i < kNumberOfPxlSectors; i++)
      for (int j = 0; j < kNumberOfPxlLaddersPerSector; j++)
         for (int k = 0; k < kNumberOfPxlSensorsPerLadder; k++) {
#ifndef  __NEW_PXLDB__
            mGeoHMatrixSensorOnGlobal[i][j][k] = (*mGeoHMatrixTpcOnGlobal) * mGeoHMatrixIdsOnTpc * mGeoHMatrixPstOnIds
                                                 * mGeoHMatrixPxlOnPst * mGeoHMatrixHalfOnPxl[i / 5] * mGeoHMatrixSectorOnHalf[i]
                                                 * mGeoHMatrixLadderOnSector[i][j] * mGeoHMatrixSensorOnLadder[i][j][k];
#else /* __NEW_PXLDB__ */
	    mGeoHMatrixSensorOnGlobal[i][j][k] = (StTpcDb::instance()->Tpc2GlobalMatrix()) * 
	      mGeoHMatrixIdsOnTpc * mGeoHMatrixPstOnIds
	      * mGeoHMatrixPxlOnPst * mGeoHMatrixHalfOnPxl[i / 5] * mGeoHMatrixSectorOnHalf[i]
	      * mGeoHMatrixLadderOnSector[i][j] * mGeoHMatrixSensorOnLadder[i][j][k];
#endif /* ! __NEW_PXLDB__ */
            mGeoHMatrixSensorOnGlobal[i][j][k].SetName(Form("sensorOnGlobal%03i%03i%03i", i + 1, j + 1, k + 1));
         }

}
#ifndef  __NEW_PXLDB__
//_____________________________________________________________________________
Int_t StPxlDb::sensorStatus(Int_t sector, Int_t ladder, Int_t sensor) const
{
   if (!mSensorStatusTable) return 255;
   return mSensorStatusTable->status[(sector - 1) * kNumberOfPxlLaddersPerSector * kNumberOfPxlSensorsPerLadder + (ladder - 1) * kNumberOfPxlSensorsPerLadder + (sensor - 1)];
}
//_____________________________________________________________________________
Int_t StPxlDb::rowStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t row) const
{
   if (!mRowColumnStatusTable) return 255;
   return mRowColumnStatusTable->rows[kNumberOfPxlRowsOnSensor * ((sector - 1) * (kNumberOfPxlSensorsPerLadder * kNumberOfPxlLaddersPerSector) + (ladder - 1) * kNumberOfPxlSensorsPerLadder + (sensor - 1)) + row];
}
//_____________________________________________________________________________
Int_t StPxlDb::columnStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t column) const
{
   if (!mRowColumnStatusTable) return 255;
   return mRowColumnStatusTable->cols[kNumberOfPxlColumnsOnSensor * ((sector - 1) * (kNumberOfPxlSensorsPerLadder * kNumberOfPxlLaddersPerSector) + (ladder - 1) * kNumberOfPxlSensorsPerLadder + (sensor - 1)) + column];
}
//_____________________________________________________________________________
Int_t StPxlDb::pixelHot(Int_t sector, Int_t ladder, Int_t sensor, Int_t row, Int_t column) const
{
  map<unsigned int,short>::const_iterator got;
  got = mMapHotPixels.find(1000000*((sector-1)*40+(ladder-1)*10+sensor) + 1000*row + column);
  if ( got == mMapHotPixels.end() ) {
    return 0;
  }
  else {
    return 1;
  }
}
#endif /* ! __NEW_PXLDB__ */
//_____________________________________________________________________________
void StPxlDb::setThinPlateSpline(pxlSensorTps_st *pxlSensorTps)
{
   for (Int_t i = 0; i < kNumberOfPxlSectors * kNumberOfPxlLaddersPerSector * kNumberOfPxlSensorsPerLadder; i++) {
      int id = pxlSensorTps[i].Id;
      int iSector = (id - 1) / kNumberOfPxlSensorsPerLadder / kNumberOfPxlLaddersPerSector;
      int iLadder = (id - 1) / kNumberOfPxlSensorsPerLadder % kNumberOfPxlLaddersPerSector;
      int iSensor = (id - 1) % kNumberOfPxlSensorsPerLadder;
      int nMeasurements = pxlSensorTps[i].nMeasurements;
      if (mThinPlateSpline[iSector][iLadder][iSensor]) {delete mThinPlateSpline[iSector][iLadder][iSensor];}
      mThinPlateSpline[iSector][iLadder][iSensor] = new StThinPlateSpline(nMeasurements, pxlSensorTps[i].X, pxlSensorTps[i].Y, pxlSensorTps[i].W, pxlSensorTps[i].A);
   }
}
#ifndef  __NEW_PXLDB__
//_____________________________________________________________________________
void StPxlDb::setHotPixels(pxlHotPixels_st *hotPixelsTable)
{
  for(Int_t i=0; i<10000; i++){ 
    if(hotPixelsTable[0].hotPixel[i]>0){ 
      mMapHotPixels.insert ( std::pair<unsigned long, short>(hotPixelsTable[0].hotPixel[i],i) ); 
    } 
    else break;
  }
}
#endif /* ! __NEW_PXLDB__ */
//_____________________________________________________________________________
void StPxlDb::setBadRowColumns(pxlBadRowColumns_st *badRowColumns)
{
    mRowColumnStatusTable = new pxlRowColumnStatus_st;
    memset(mRowColumnStatusTable->rows, 1, 400000);
    memset(mRowColumnStatusTable->cols, 1, 400000);
    for(Int_t i=0; i<10000; i++){
        if(badRowColumns->badRowColumns[i]){
            int isRowOrColumn = badRowColumns->badRowColumns[i]/100000000;
            int sensorId = badRowColumns->badRowColumns[i]/100000%1000;
            int iSector = (sensorId - 1) / kNumberOfPxlSensorsPerLadder / kNumberOfPxlLaddersPerSector;
            int iLadder = (sensorId - 1) / kNumberOfPxlSensorsPerLadder % kNumberOfPxlLaddersPerSector;
            int iSensor = (sensorId - 1) % kNumberOfPxlSensorsPerLadder;
            int rowOrColumn = badRowColumns->badRowColumns[i]/100%1000;
            int status = badRowColumns->badRowColumns[i]%100;
            if(isRowOrColumn==1 && iSector>=0 && iSector<10 && iLadder>=0 && iLadder<10 && iSensor>=0 && iSensor<10 && rowOrColumn>=0 && rowOrColumn<kNumberOfPxlColumnsOnSensor)
                mRowColumnStatusTable->cols[kNumberOfPxlColumnsOnSensor * (iSector * (kNumberOfPxlSensorsPerLadder * kNumberOfPxlLaddersPerSector) + iLadder * kNumberOfPxlSensorsPerLadder + iSensor) + rowOrColumn] = status;
            else if(isRowOrColumn==0 && iSector>=0 && iSector<10 && iLadder>=0 && iLadder<10 && iSensor>=0 && iSensor<10 && rowOrColumn>=0 && rowOrColumn<kNumberOfPxlRowsOnSensor)
                mRowColumnStatusTable->rows[kNumberOfPxlRowsOnSensor * (iSector * (kNumberOfPxlSensorsPerLadder * kNumberOfPxlLaddersPerSector) + iLadder * kNumberOfPxlSensorsPerLadder + iSensor) + rowOrColumn] = status;
            else { LOG_WARN<<"wrong bad row column "<<" [ "<<i<<" ]: "<<badRowColumns->badRowColumns[i]<<endm; }
        }
        else break;
    }
}


void StPxlDb::Print(Option_t *opt) const
{
   LOG_INFO << "Print all StPxlDb matrices:" << endm;
#ifndef __NEW_PXLDB__
   geoHMatrixTpcOnGlobal()->Print();
#else /* __NEW_PXLDB__ */
   gStTpcDb->Tpc2GlobalMatrix().Print();
#endif /* !__NEW_PXLDB__ */
   geoHMatrixIdsOnTpc()->Print();
   geoHMatrixPstOnIds()->Print();
   geoHMatrixPxlOnPst()->Print();

   LOG_INFO << "geoHMatrix pxl half on pxl: " << endm;
   for (int i = 0; i < 2; i++) {
     geoHMatrixHalfOnPxl(i+1)->Print();
   }

   LOG_INFO << "geoHMatrix pxl sector on half: " << endm;
   for (int i = 0; i < kNumberOfPxlSectors; i++) {
     geoHMatrixSectorOnHalf(i+1)->Print();
     TGeoHMatrix sectorOnPxl = (*geoHMatrixHalfOnPxl(i / 5 + 1)) * (*geoHMatrixSectorOnHalf(i+1));
     sectorOnPxl.SetName(Form("sectorOnPxl%02i%03i",i/5,i));
     sectorOnPxl.Print();
   }

   LOG_INFO << "geoHMatrix pxl ladder on sector: " << endm;
   for (int i = 0; i < kNumberOfPxlSectors; i++)
      for (int j = 0; j < kNumberOfPxlLaddersPerSector; j++) {
	geoHMatrixLadderOnSector(i+1,j+1)->Print();
      }

   LOG_INFO << "geoHMatrix pxl sensor on ladder: " << endm;
   for (int i = 0; i < kNumberOfPxlSectors; i++)
      for (int j = 0; j < kNumberOfPxlLaddersPerSector; j++)
         for (int k = 0; k < kNumberOfPxlSensorsPerLadder; k++) {
	   geoHMatrixSensorOnLadder(i+1,j+1,k+1)->Print();
         }
   
   LOG_INFO << "geoHMatrix pxl sensor on global: " << endm;
   for (int i = 0; i < kNumberOfPxlSectors; i++)
     for (int j = 0; j < kNumberOfPxlLaddersPerSector; j++)
       for (int k = 0; k < kNumberOfPxlSensorsPerLadder; k++) {
	 geoHMatrixSensorOnGlobal(i+1,j+1,k+1)->Print();
       }
}

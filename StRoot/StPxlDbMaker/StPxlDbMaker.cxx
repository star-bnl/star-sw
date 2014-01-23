/*!
 * \class StPxlDbMaker
 * \author J. Bouchet, M. Lomnitz, May 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDbMaker.cxx,v 1.8 2014/01/23 01:04:49 qiuh Exp $
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
 * Revision 1.8  2014/01/23 01:04:49  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

/*
  relation within STAR frame
  GlobalXyz = TpcOnGlobal * IdsOnTpc * PxlOnIds * HalfOnPxl * SectorOnHalf * LadderOnSector * SensorOnLadder * SensorLocalXyz
  
  numbering : 
  Id  = (sector-1)*40 + (ladder-1)*10 + sensor 
  1<= sector <= 10
  1<= ladder <= 4
  1<= sensor <= 10
*/

#include "StPxlDbMaker.h"
#include "StPxlUtil/StPxlConstants.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "tables/St_Survey_Table.h"
#include "TMath.h"
#include "TVector3.h"
#include "StTpcDb/StTpcDb.h"
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_pxlSensorStatus_Table.h" 
#include "tables/St_pxlRowColumnStatus_Table.h" 

ClassImp(StPxlDbMaker)
//_____________________________________________________________________________
StPxlDbMaker::StPxlDbMaker(const char *name) : 
  StMaker(name)
{
    mGeoHMatrixTpcOnGlobal = 0;
    mGeoHMatrixIdsOnTpc = 0;
    mGeoHMatrixPstOnIds = 0;
    mGeoHMatrixPxlOnPst = 0;
    for(int i=0; i<2; i++) 
        {
            mGeoHMatrixHalfOnPxl[i] = 0;
        }
    for(int i=0; i<nPxlSectors; i++)
        {
            mGeoHMatrixSectorOnHalf[i] = 0;
        }
    for(int i=0; i<nPxlSectors; i++)
        for(int j=0; j<nPxlLaddersPerSector; j++)
            {
                mGeoHMatrixLadderOnSector[i][j] = 0;
            }
    for(int i=0; i<nPxlSectors; i++)
        for(int j=0; j<nPxlLaddersPerSector; j++)
            for(int k=0; k<nPxlSensorsPerLadder; k++)
                {
                    mGeoHMatrixSensorOnLadder[i][j][k] = 0;
                    mGeoHMatrixSensorOnGlobal[i][j][k] = 0;
                }
    mSensorStatusTable = 0;
    mRowColumnStatusTable = 0;
}
//_____________________________________________________________________________
StPxlDbMaker::~StPxlDbMaker() {}
//_____________________________________________________________________________
Int_t StPxlDbMaker::Init()
{
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::InitRun(Int_t runNumber) {
  getPositions();
 
  St_pxlSensorStatus* sensorStatus = (St_pxlSensorStatus*)GetDataBase("Calibrations/pxl/pxlSensorStatus");
  mSensorStatusTable = 0;
  if(sensorStatus) mSensorStatusTable = sensorStatus->GetTable();
  else {LOG_WARN <<" no pxl sensor status table "<<endm;}

  St_pxlRowColumnStatus* rowColumnStatus = (St_pxlRowColumnStatus*)GetDataBase("Calibrations/pxl/pxlRowColumnStatus");
  mRowColumnStatusTable = 0;
  if(rowColumnStatus) mRowColumnStatusTable = rowColumnStatus->GetTable();
  else {LOG_WARN <<" no pxl row column status table "<<endm;}
  return kStOK;
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::Make()
{
  return kStOK;
}
//_____________________________________________________________________________
void StPxlDbMaker::Clear(const char*)
{
  StMaker::Clear();
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::Finish()
{
  return kStOK;
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::getPositions(){

    if(gStTpcDb)
        mGeoHMatrixTpcOnGlobal = (TGeoHMatrix*)&gStTpcDb->Tpc2GlobalMatrix();
    else
        {
            mGeoHMatrixTpcOnGlobal = new TGeoHMatrix("tpcOnGlobal");
            LOG_WARN << "No gStTpcDb" << endm;
        }

    St_Survey *idsOnTpc          = (St_Survey *) GetDataBase("Geometry/pxl/idsOnTpc");
    if (! idsOnTpc)          {LOG_WARN << "idsOnTpc has not been found"  << endm; return kStErr;}

    St_Survey *pstOnIds          = (St_Survey *) GetDataBase("Geometry/pxl/pstOnIds");
    if (! pstOnIds)          {LOG_WARN << "pstOnIds has not been found"  << endm; return kStErr;}
    
    St_Survey *pxlOnPst          = (St_Survey *) GetDataBase("Geometry/pxl/pxlOnPst");
    if (! pxlOnPst)          {LOG_WARN << "pxlOnPst has not been found"  << endm; return kStErr;}
    
    St_Survey *pxlHalfOnPxl         = (St_Survey *) GetDataBase("Geometry/pxl/pxlHalfOnPxl");
    if (! pxlHalfOnPxl)         {LOG_WARN << "pxlHalfOnPxl has not been found"  << endm; return kStErr;}
    
    St_Survey *pxlSectorOnHalf     = (St_Survey *) GetDataBase("Geometry/pxl/pxlSectorOnHalf");
    if (! pxlSectorOnHalf)     {LOG_WARN << "pxlSectorOnHalf has not been found"  << endm; return kStErr;}
    
    St_Survey *pxlLadderOnSector  = (St_Survey *) GetDataBase("Geometry/pxl/pxlLadderOnSector");
    if (! pxlLadderOnSector)  {LOG_WARN << "pxladderOnSector has not been found"  << endm; return kStErr;}
    
    St_Survey *pxlSensorOnLadder  = (St_Survey *) GetDataBase("Geometry/pxl/pxlSensorOnLadder");
    if (! pxlSensorOnLadder)  {LOG_WARN << "pxlSensorOnLadder has not been found"  << endm; return kStErr;}
    
    /// get tables
    Survey_st *IdsOnTpc          = idsOnTpc->GetTable();   
    Survey_st *PstOnIds          = pstOnIds->GetTable();   
    Survey_st *PxlOnPst          = pxlOnPst->GetTable();   
    Survey_st *HalfOnPxl         = pxlHalfOnPxl->GetTable();   
    Survey_st *SectorsOnHalf     = pxlSectorOnHalf->GetTable();   
    Survey_st *LaddersOnSectors  = pxlLadderOnSector->GetTable();   
    Survey_st *SensorsOnLadders  = pxlSensorOnLadder->GetTable();   
    
    mGeoHMatrixIdsOnTpc = new TGeoHMatrix("idsOnTps");
    mGeoHMatrixIdsOnTpc->SetRotation(&IdsOnTpc->r00);
    mGeoHMatrixIdsOnTpc->SetTranslation(&IdsOnTpc->t0);
    
    mGeoHMatrixPstOnIds = new TGeoHMatrix("pstOnIds");
    mGeoHMatrixPstOnIds->SetRotation(&PstOnIds->r00);
    mGeoHMatrixPstOnIds->SetTranslation(&PstOnIds->t0);
    
    mGeoHMatrixPxlOnPst = new TGeoHMatrix("pxlOnPst");
    mGeoHMatrixPxlOnPst->SetRotation(&PxlOnPst->r00);
    mGeoHMatrixPxlOnPst->SetTranslation(&PxlOnPst->t0);

    if(Debug()>2){
        mGeoHMatrixTpcOnGlobal->Print();
        mGeoHMatrixIdsOnTpc->Print();
        mGeoHMatrixPstOnIds->Print();
        mGeoHMatrixPxlOnPst->Print();
    }

    for (Int_t t = 0; t <2; t++, HalfOnPxl++) {
        mGeoHMatrixHalfOnPxl[HalfOnPxl->Id-1] = new TGeoHMatrix(Form("halfOnPxl%03i", HalfOnPxl->Id));
        mGeoHMatrixHalfOnPxl[HalfOnPxl->Id-1]->SetRotation(&HalfOnPxl->r00);
        mGeoHMatrixHalfOnPxl[HalfOnPxl->Id-1]->SetTranslation(&HalfOnPxl->t0);
    }

    for (int i=0; i<nPxlSectors; i++){
        mGeoHMatrixSectorOnHalf[SectorsOnHalf->Id-1] = new TGeoHMatrix(Form("sectorOnHalf%03i", SectorsOnHalf->Id));
        mGeoHMatrixSectorOnHalf[SectorsOnHalf->Id-1]->SetRotation(&SectorsOnHalf->r00);
        mGeoHMatrixSectorOnHalf[SectorsOnHalf->Id-1]->SetTranslation(&SectorsOnHalf->t0);
        SectorsOnHalf++;
    }

    for (int i=0; i<nPxlSectors; i++)
        for (int j=0; j<nPxlLaddersPerSector; j++)
            {
                int iSector = (LaddersOnSectors->Id-1)/nPxlLaddersPerSector;
                int iLadder = (LaddersOnSectors->Id-1)%nPxlLaddersPerSector;
                mGeoHMatrixLadderOnSector[iSector][iLadder] = new TGeoHMatrix(Form("ladderOnSector%03i%03i", iSector+1, iLadder+1));
                mGeoHMatrixLadderOnSector[iSector][iLadder]->SetRotation(&LaddersOnSectors->r00);
                mGeoHMatrixLadderOnSector[iSector][iLadder]->SetTranslation(&LaddersOnSectors->t0);
                LaddersOnSectors++;
            }

    for (int i=0; i<nPxlSectors; i++)
        for (int j=0; j<nPxlLaddersPerSector; j++)
            for (int k=0; k<nPxlSensorsPerLadder; k++)
                {
                    int iSector = (SensorsOnLadders->Id-1)/nPxlLaddersPerSector/nPxlSensorsPerLadder;
                    int iLadder = ((SensorsOnLadders->Id-1)/nPxlSensorsPerLadder)%nPxlLaddersPerSector;
                    int iSensor = (SensorsOnLadders->Id-1)%nPxlSensorsPerLadder;
                    mGeoHMatrixSensorOnLadder[i][j][k] = new TGeoHMatrix(Form("sensorOnLadder%03i%03i%03i", iSector+1, iLadder+1, iSensor+1));
                    mGeoHMatrixSensorOnLadder[i][j][k]->SetRotation(&SensorsOnLadders->r00);
                    mGeoHMatrixSensorOnLadder[i][j][k]->SetTranslation(&SensorsOnLadders->t0);
                    SensorsOnLadders++;
                }

    LOG_INFO << "pxl sensor on global matrix: " << endm;
    for (int i=0; i<nPxlSectors; i++)
        for (int j=0; j<nPxlLaddersPerSector; j++)
            for (int k=0; k<nPxlSensorsPerLadder; k++)
                {
                    mGeoHMatrixSensorOnGlobal[i][j][k] = new TGeoHMatrix(Form("sensorOnGlobal%03i%03i%03i", i+1, j+1, k+1));
                    *mGeoHMatrixSensorOnGlobal[i][j][k] = (*mGeoHMatrixTpcOnGlobal)*(*mGeoHMatrixIdsOnTpc)*(*mGeoHMatrixPstOnIds)
                        *(*mGeoHMatrixPxlOnPst)*(*mGeoHMatrixHalfOnPxl[i/5])*(*mGeoHMatrixSectorOnHalf[i])
                        *(*mGeoHMatrixLadderOnSector[i][j])*(*mGeoHMatrixSensorOnLadder[i][j][k]);
                    mGeoHMatrixSensorOnGlobal[i][j][k]->Print();
                }
    
  return kStOk;
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::sensorStatus(Int_t sector, Int_t ladder, Int_t sensor) const {
    if(!mSensorStatusTable) return 255;
    return mSensorStatusTable[0].status[(sector-1)*nPxlLaddersPerSector*nPxlSensorsPerLadder + (ladder-1)*nPxlSensorsPerLadder + (sensor-1)];
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::rowStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t row) const {
    if(!mRowColumnStatusTable) return 255;
    return mRowColumnStatusTable[0].rows[nPxlRowsOnSensor*((sector-1)*(nPxlSensorsPerLadder*nPxlLaddersPerSector) + (ladder-1)*nPxlSensorsPerLadder + (sensor-1)) + row];
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::columnStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t column) const {
    if(!mRowColumnStatusTable) return 255;
    return mRowColumnStatusTable[0].cols[nPxlColumnsOnSensor*((sector-1)*(nPxlSensorsPerLadder*nPxlLaddersPerSector) + (ladder-1)*nPxlSensorsPerLadder + (sensor-1)) + column];
}


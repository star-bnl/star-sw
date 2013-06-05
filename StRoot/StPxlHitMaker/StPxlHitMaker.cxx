/*!
 * \class StPxlHitMaker 
 * \author Qiu Hao, Jan 2013
 */
/***************************************************************************
 * 
 * $Id: StPxlHitMaker.cxx,v 1.3 2013/06/05 01:01:46 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013 
 ***************************************************************************
 *
 * Description:
 * Create pxl hits according to clusters and calculate their global position.
 *
 ***************************************************************************
 *
 * $Log: StPxlHitMaker.cxx,v $
 * Revision 1.3  2013/06/05 01:01:46  qiuh
 * *** empty log message ***
 *
 * Revision 1.2  2013/05/24 20:26:00  qiuh
 * *** empty log message ***
 * 
 **************************************************************************/ 

#include "StPxlHitMaker.h"
#include "StMessMgr.h"
#include "StEventTypes.h"
#include "TGeoMatrix.h"
#include "StPxlUtil/Tps.h"
#include "StPxlUtil/StPxlCluster.h"
#include "StPxlUtil/StPxlClusterCollection.h"
#include "StPxlUtil/StPxlConstants.h"
#include "StPxlDbMaker/StPxlDbMaker.h"
#include "tables/St_pxlSensorTps_Table.h"

ClassImp(StPxlHitMaker)

//________________________________________________________________________________
StPxlHitMaker::StPxlHitMaker(const Char_t *name) : StMaker(name) {
}
//________________________________________________________________________________
StPxlHitMaker::~StPxlHitMaker() {
}
//________________________________________________________________________________
Int_t StPxlHitMaker::Init() {
    return StMaker::Init();
}
//________________________________________________________________________________
Int_t StPxlHitMaker::InitRun(Int_t runnumber) {
    listGeoMSensorOnGlobal = gStPxlDbMaker->GetRotations();

    //read Tps DB
    TDataSet *dbTps = 0;
    dbTps = GetDataBase("Geometry/pxl/pxlSensorTps");
    if (!dbTps) {
        LOG_WARN << "no tps table found in db, or malformed local db config " << endm;
    }

    St_pxlSensorTps *datasetTps = 0;
    datasetTps = (St_pxlSensorTps*) dbTps->Find("pxlSensorTps");
    Int_t rowsTps = datasetTps->GetNRows();
    if (rowsTps > 1) {
        LOG_INFO << "found tps table with " << rowsTps << " rows" << endm;
    }

    if (datasetTps) {
        pxlSensorTps_st *tableTps = datasetTps->GetTable();
        for (Int_t i = 0; i < rowsTps; i++) {
            // sample output of first member variable
            int id = tableTps[i].Id;
            int iSector = (id-1)/40;
            int iLadder = (id-1)/10%4;
            int iSensor = (id-1)%10;
            int nMeasurements = tableTps[i].nMeasurements;
            tps[iSector][iLadder][iSensor] = new Tps(nMeasurements);
            for(int j=0; j<3; j++)
                (*tps[iSector][iLadder][iSensor]->A)[j][0] = tableTps[i].A[j];
            for(int j=0; j<nMeasurements; j++)
                (*tps[iSector][iLadder][iSensor]->X)[j][0] = tableTps[i].X[j];
            for(int j=0; j<nMeasurements; j++)
                (*tps[iSector][iLadder][iSensor]->Y)[j][0] = tableTps[i].Y[j];
            for(int j=0; j<nMeasurements; j++)
                (*tps[iSector][iLadder][iSensor]->W)[j][0] = tableTps[i].W[j];
        }
    } else {
        LOG_WARN << "ERROR: dataset does not contain tps table" << endm;
    }
    return kStOk;
}

//________________________________________________________________________________
Int_t StPxlHitMaker::Make() {
    LOG_INFO<<"StPxlHitMaker::Make()"<<endm;

    Bool_t EmbeddingShortCut = IAttr("EmbeddingShortCut");

    StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
    if (! pEvent) {
        LOG_WARN << "StPxlHitMaker::Make there is no StEvent " << endm;
        return kStWarn;
    }

    TObjectSet* pxlClusterDataSet = (TObjectSet*)GetDataSet("pxlCluster");
    StPxlClusterCollection* pxlClusterCollection = 0;
    if(pxlClusterDataSet)
        pxlClusterCollection = (StPxlClusterCollection*)pxlClusterDataSet->GetObject();

    StPxlHitCollection* pxlHitCollection = pEvent->pxlHitCollection();

    if (!pxlClusterCollection && !pxlHitCollection) {
        LOG_WARN << "StPxlHitMaker::Make()  no pxlClusterCollection or pxlHitCollection to work on" << endm;
        return kStWarn;
    }

    if(!pxlHitCollection) pxlHitCollection = new StPxlHitCollection();

    double firstPixelZ = -(nPxlColumnsOnSensor-1)*pxlPixelSize/2;
    double firstPixelX = (nPxlRowsOnSensor-1)*pxlPixelSize/2;

    for (int i=0; i<nPxlSectors; i++)
        for(int j=0; j<nPxlLaddersPerSector; j++)
            for(int k=0; k<nPxlSensorsPerLadder; k++)
                {
                    int sensorId = i*40+j*10+k+1;

                    //add in new hit from cluster
                    if(pxlClusterCollection)
                        {
                            int vecSize = pxlClusterCollection->clusterVec[i][j][k].size();
                            for(int l=0; l<vecSize; l++)
                                {
                                    StPxlCluster* cluster = pxlClusterCollection->clusterVec[i][j][k][l];
                                    StPxlHit* pxlHit = new StPxlHit();
                                    pxlHit->setSector(i+1);
                                    pxlHit->setLadder(j+1);
                                    pxlHit->setSensor(k+1);
                                    pxlHit->setDetectorId(kPxlId);
                                    pxlHit->setMeanRow(cluster->rowCenter);
                                    pxlHit->setMeanColumn(cluster->columnCenter);
                                    pxlHit->setNRawHits(cluster->nRawHits());
                                    pxlHit->setIdTruth(cluster->idTruth);
                                    
                                    pxlHitCollection->addHit(pxlHit);
                                }
                        }

                    //get hit positions
                    TGeoHMatrix *geoMSensorOnGlobal=(TGeoHMatrix*)listGeoMSensorOnGlobal->FindObject(Form("R%03i",sensorId));
                    int nHitsInSensor = pxlHitCollection->sector(i)->ladder(j)->sensor(k)->hits().size();
                    for(int l=0; l<nHitsInSensor; l++)
                        {
                            StPxlHit* pxlHit = pxlHitCollection->sector(i)->ladder(j)->sensor(k)->hits()[l];
                            double local[3];
                            double global[3];
                        
                            local[2] = firstPixelZ + pxlPixelSize*pxlHit->meanColumn();
                            local[0] = firstPixelX - pxlPixelSize*pxlHit->meanRow();
                            
                            if(EmbeddingShortCut && pxlHit->idTruth())
                                local[1] = 0;
                            else
                                local[1] = tps[i][j][k]->Z(local[2], local[0]); // the Tps x, y, z are sensor local z, x, y respectively
                            
                            geoMSensorOnGlobal->LocalToMaster(local, global);
                            pxlHit->setLocalPosition(local[0], local[1], local[2]);
                            StThreeVectorF vecGlobal(global);
                            pxlHit->setPosition(vecGlobal);
                        }
                }

    if(!pEvent->pxlHitCollection())
        pEvent->setPxlHitCollection(pxlHitCollection);
    return kStOK;
}


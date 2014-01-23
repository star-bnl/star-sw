/*!
 * \class StPxlHitMaker 
 * \author Qiu Hao, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 * 
 * $Id: StPxlHitMaker.cxx,v 1.4 2014/01/23 01:04:53 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013 
 ***************************************************************************
 *
 * Description:
 * Create pxl hits according to clusters and calculate pxl hit global positions.
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlHitMaker.cxx,v $
 * Revision 1.4  2014/01/23 01:04:53  qiuh
 * *** empty log message ***
 *
 * 
 **************************************************************************/ 

#include "StPxlHitMaker.h"
#include "StMessMgr.h"
#include "StEventTypes.h"
#include "TGeoMatrix.h"
#include "StPxlUtil/Tps.h"
#include "StPxlClusterMaker/StPxlCluster.h"
#include "StPxlClusterMaker/StPxlClusterCollection.h"
#include "StPxlUtil/StPxlConstants.h"
#include "StPxlDbMaker/StPxlDbMaker.h"
#include "tables/St_pxlSensorTps_Table.h"
#include "tables/St_pxlControl_Table.h"

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
    /// read Tps DB
    TDataSet *dbTps = 0;
    dbTps = GetDataBase("Geometry/pxl/pxlSensorTps");
    if (!dbTps) {
        LOG_WARN << "no tps table found in db, or malformed local db config " << endm;
    }

    St_pxlSensorTps *datasetTps = 0;
    datasetTps = (St_pxlSensorTps*) dbTps->Find("pxlSensorTps");

    if (datasetTps) {
        pxlSensorTps_st *tableTps = datasetTps->GetTable();
        for (Int_t i = 0; i < nPxlSectors*nPxlLaddersPerSector*nPxlSensorsPerLadder; i++) {
            int id = tableTps[i].Id;
            int iSector = (id-1)/nPxlSensorsPerLadder/nPxlLaddersPerSector;
            int iLadder = (id-1)/nPxlSensorsPerLadder%nPxlLaddersPerSector;
            int iSensor = (id-1)%nPxlSensorsPerLadder;
            int nMeasurements = tableTps[i].nMeasurements;
            cout<<"nMeasurements: "<<nMeasurements<<endl;
            mTps[iSector][iLadder][iSensor] = new Tps(nMeasurements, tableTps[i].X, tableTps[i].Y, tableTps[i].W, tableTps[i].A);
            cout<<"tps done"<<endl;
        }
    } else {
        LOG_WARN << "ERROR: dataset does not contain tps table" << endm;
    }

    /// read pxl size from DB
    St_pxlControl* pxlControl = (St_pxlControl*)GetDataBase("Geometry/pxl/pxlControl");
    if (!pxlControl)
        {
            LOG_ERROR << "InitRun : No access to pxlControl table, abort PXL reconstruction" << endm;
            return kStErr;
        }
    pxlControl_st *pxlControlTable = pxlControl->GetTable();
    mPixelSize = pxlControlTable[0].pixelSize;

    return kStOk;
}

//________________________________________________________________________________
Int_t StPxlHitMaker::Make() {
    LOG_INFO<<"StPxlHitMaker::Make()"<<endm;

    Bool_t EmbeddingShortCut = IAttr("EmbeddingShortCut"); ///< 1 for embedding, use ideal geometry with no corrections

    /// get StEvent pointer
    StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
    if (! pEvent) {
        LOG_WARN << "StPxlHitMaker::Make there is no StEvent " << endm;
        return kStWarn;
    }

    /// The input data can be both clusters and pxl hits. 
    /// If there are already pxl hits, their positions will be recalculated. 
    /// If there are clusters but no pxl hits collection, a new pxl hit collection will be created.
    /// If there are both pxl hits and clusters, new pxl hits from clusters will be added to hits collection. 

    /// input pxl cluster collection
    TObjectSet* pxlClusterDataSet = (TObjectSet*)GetDataSet("pxlCluster");
    StPxlClusterCollection* pxlClusterCollection = 0;
    if(pxlClusterDataSet)
        pxlClusterCollection = (StPxlClusterCollection*)pxlClusterDataSet->GetObject();

    /// input pxl hit collection
    StPxlHitCollection* pxlHitCollection = pEvent->pxlHitCollection();

    /// if no pxl hit collection nor pxl cluster collection, nothing to work on
    if (!pxlClusterCollection && !pxlHitCollection) {
        LOG_WARN << "StPxlHitMaker::Make()  no pxlClusterCollection or pxlHitCollection to work on" << endm;
        return kStWarn;
    }

    /// if no pxl hit collection, create one for output
    if(!pxlHitCollection) pxlHitCollection = new StPxlHitCollection();

    double firstPixelZ = -(nPxlColumnsOnSensor-1)*mPixelSize/2;
    double firstPixelX = (nPxlRowsOnSensor-1)*mPixelSize/2;

    StPxlDbMaker* pxlDb = (StPxlDbMaker*)GetMaker("pxlDb");

    for (int i=0; i<nPxlSectors; i++)
        for(int j=0; j<nPxlLaddersPerSector; j++)
            for(int k=0; k<nPxlSensorsPerLadder; k++)
                {
                    /// add in new hits from clusters
                    if(pxlClusterCollection)
                        {
                            int vecSize = pxlClusterCollection->numberOfClusters(i+1, j+1, k+1);
                            for(int l=0; l<vecSize; l++)
                                {
                                    StPxlCluster* cluster = pxlClusterCollection->cluster(i+1, j+1, k+1, l);
                                    StPxlHit* pxlHit = new StPxlHit();
                                    pxlHit->setSector(i+1);
                                    pxlHit->setLadder(j+1);
                                    pxlHit->setSensor(k+1);
                                    pxlHit->setDetectorId(kPxlId);
                                    pxlHit->setMeanRow(cluster->rowCenter());
                                    pxlHit->setMeanColumn(cluster->columnCenter());
                                    pxlHit->setNRawHits(cluster->nRawHits());
                                    pxlHit->setIdTruth(cluster->idTruth());
                                    
                                    pxlHitCollection->addHit(pxlHit);
                                }
                        }

                    /// get hit positions
                    TGeoHMatrix *geoMSensorOnGlobal = pxlDb->geoHMatrixSensorOnGlobal(i+1, j+1, k+1);
                    int nHitsInSensor = pxlHitCollection->sector(i)->ladder(j)->sensor(k)->hits().size();
                    for(int l=0; l<nHitsInSensor; l++)
                        {
                            StPxlHit* pxlHit = pxlHitCollection->sector(i)->ladder(j)->sensor(k)->hits()[l];
                            double local[3];
                            double global[3];
                        
                            local[2] = firstPixelZ + mPixelSize*pxlHit->meanColumn();
                            local[0] = firstPixelX - mPixelSize*pxlHit->meanRow();
                        
                            /// apply Tps correction if not embedding
                            if(EmbeddingShortCut && pxlHit->idTruth())
                                local[1] = 0;
                            else
                                local[1] = mTps[i][j][k]->z(local[2], local[0]); ///< the Tps x, y, z are sensor local z, x, y respectively
                            
                            geoMSensorOnGlobal->LocalToMaster(local, global); ///< rotation and shift from sensor local to STAR global coordinate
                            pxlHit->setLocalPosition(local[0], local[1], local[2]);
                            StThreeVectorF vecGlobal(global);
                            pxlHit->setPosition(vecGlobal);
                        }
                }

    if(!pEvent->pxlHitCollection())
        pEvent->setPxlHitCollection(pxlHitCollection);
    return kStOK;
}


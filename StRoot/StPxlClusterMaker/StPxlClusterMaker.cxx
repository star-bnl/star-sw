/*!
 * \class StPxlClusterMaker 
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 */
/***************************************************************************
 * 
 * $Id: StPxlClusterMaker.cxx,v 1.2 2013/09/25 11:53:59 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013, according codes from Xiangming Sun 
 ***************************************************************************
 *
 * Description:
 * Group neighboring pixel raw hits from into clusters.
 *
 ***************************************************************************
 *
 * $Log: StPxlClusterMaker.cxx,v $
 * Revision 1.2  2013/09/25 11:53:59  qiuh
 * *** empty log message ***
 *
 * Revision 1.1  2013/05/23 20:57:17  qiuh
 * *** empty log message ***
 * 
 **************************************************************************/ 

#include "StPxlClusterMaker.h"
#include "StMessMgr.h"
#include "StPxlUtil/StPxlCluster.h"
#include "StPxlUtil/StPxlClusterCollection.h"
#include "StPxlUtil/StPxlRawHit.h"
#include "StPxlUtil/StPxlRawHitCollection.h"
#include "StPxlUtil/StPxlConstants.h"

ClassImp(StPxlClusterMaker);

//________________________________________________________________________________
StPxlClusterMaker::StPxlClusterMaker(const Char_t *name) : StMaker(name) {
  gMessMgr->Info("StPxlClusterMaker::StPxlClusterMaker: constructor called");
}
//________________________________________________________________________________
StPxlClusterMaker::~StPxlClusterMaker() {
}
//________________________________________________________________________________
Int_t StPxlClusterMaker::Init() {
    return StMaker::Init();
}
//________________________________________________________________________________
Int_t StPxlClusterMaker::Make() {
    LOG_INFO<<"StPxlClusterMaker::Make()"<<endm;

    //input data
    TObjectSet* pxlRawHitDataSet = (TObjectSet*)GetDataSet("pxlRawHit");
    if (! pxlRawHitDataSet) {
        LOG_WARN << "Make() - there is no pxlRawHitDataSet " << endm;
        return kStWarn;
    }
    
    StPxlRawHitCollection* pxlRawHitCollection = (StPxlRawHitCollection*)pxlRawHitDataSet->GetObject();
    if(!pxlRawHitCollection) {
        LOG_WARN << "Make() - no pxlRawHitCollection."<<endm;
        return kStWarn;
    }
    
    //output cluster data structures
    m_pxlClusterDataSet = new TObjectSet("pxlCluster");
    m_DataSet = m_pxlClusterDataSet;
    m_pxlClusterCollection = new StPxlClusterCollection();
    m_pxlClusterDataSet->AddObject(m_pxlClusterCollection);
    
    //real work
    for (int i=0; i<nPxlSectors; i++) 
        for(int j=0; j<nPxlLaddersPerSector; j++)
            for(int k=0; k<nPxlSensorsPerLadder; k++)
                {
                    //clear bitMap
                    for(int l=0;l<nPxlRowsOnSensor;l++){
                        bitMap[l].reset();
                    }
                    memset(mapIdTruth, 0, nPxlRowsOnSensor*nPxlColumnsOnSensor*sizeof(int));
                    
                    //load bitMap
                    int vectorSize = pxlRawHitCollection->pxlRawHitVec[i][j][k].size();
                    for(int l=0; l<vectorSize; l++)
                        {
                            StPxlRawHit* rawHit = pxlRawHitCollection->pxlRawHitVec[i][j][k][l];
                            bitMap[rawHit->row()].set(rawHit->column());
                            if(rawHit->idTruth()) mapIdTruth[rawHit->row()][rawHit->column()] = rawHit->idTruth();
                        }
                    
                    //find clusters
                    for(int l=0; l<vectorSize; l++)
                        {
                            StPxlRawHit* rawHit = pxlRawHitCollection->pxlRawHitVec[i][j][k][l];
                            StPxlCluster* cluster = new StPxlCluster();
                            FindCluster(cluster, rawHit->column(), rawHit->row());
                            if(cluster->nRawHits() > 0)
                                {
                                    cluster->Summarize();
                                    m_pxlClusterCollection->clusterVec[i][j][k].push_back(cluster);
                                }
                            else delete cluster;
                        }
                }

    return kStOK;
}

void StPxlClusterMaker::FindCluster(StPxlCluster* cluster, int column, int row)
{    
    if(bitMap[row][column]==0) return; //already included in another cluster
    bitMap[row][column]=0; //unmark this used raw hit
    if(column-1 >= 0) FindCluster(cluster, column-1, row);
    if(column+1 < nPxlColumnsOnSensor) FindCluster(cluster, column+1, row);
    if(row-1 >= 0) FindCluster(cluster, column, row-1);
    if(row+1 < nPxlRowsOnSensor) FindCluster(cluster, column, row+1);
    if(column-1 >=0 && row-1 >= 0) FindCluster(cluster, column-1, row-1);
    if(column-1 >=0 && row+1 < nPxlRowsOnSensor) FindCluster(cluster, column-1, row+1);
    if(column+1 < nPxlColumnsOnSensor && row-1 >= 0) FindCluster(cluster, column+1, row-1);
    if(column+1 < nPxlColumnsOnSensor && row+1 < nPxlRowsOnSensor) FindCluster(cluster, column+1, row+1);
    cluster->AddRawHit(column, row, mapIdTruth[row][column]);
    
}


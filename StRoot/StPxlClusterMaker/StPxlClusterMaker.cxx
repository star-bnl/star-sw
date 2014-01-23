/*!
 * \class StPxlClusterMaker 
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 * \Initial Revision.
 */
/***************************************************************************
 * 
 * $Id: StPxlClusterMaker.cxx,v 1.3 2014/01/23 01:04:43 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013, according codes from Xiangming Sun 
 ***************************************************************************
 *
 * Description:
 * Group neighboring pixel raw hits from into clusters.
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlClusterMaker.cxx,v $
 * Revision 1.3  2014/01/23 01:04:43  qiuh
 * *** empty log message ***
 *
 * 
 **************************************************************************/ 

#include "StPxlClusterMaker.h"
#include "StMessMgr.h"
#include "StPxlCluster.h"
#include "StPxlClusterCollection.h"
#include "StPxlRawHitMaker/StPxlRawHit.h"
#include "StPxlRawHitMaker/StPxlRawHitCollection.h"
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

    /// input data
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
    
    /// output cluster data structures
    mPxlClusterCollection = new StPxlClusterCollection();
    ToWhiteBoard("pxlCluster", mPxlClusterCollection);
    
    /// real work
    for (int i=0; i<nPxlSectors; i++) 
        for(int j=0; j<nPxlLaddersPerSector; j++)
            for(int k=0; k<nPxlSensorsPerLadder; k++)
                {
                    /// clear bitMap
                    for(int l=0;l<nPxlRowsOnSensor;l++){
                        mBitMap[l].reset();
                    }
                    memset(mMapIdTruth, 0, nPxlRowsOnSensor*nPxlColumnsOnSensor*sizeof(int));
                    
                    /// load bitMap
                    int vectorSize = pxlRawHitCollection->numberOfRawHits(i+1, j+1, k+1);
                    for(int l=0; l<vectorSize; l++)
                        {
                            StPxlRawHit* rawHit = pxlRawHitCollection->rawHit(i+1, j+1, k+1, l);
                            mBitMap[rawHit->row()].set(rawHit->column());
                            if(rawHit->idTruth()) mMapIdTruth[rawHit->row()][rawHit->column()] = rawHit->idTruth();
                        }
                    
                    /// find clusters
                    for(int l=0; l<vectorSize; l++)
                        {
                            StPxlRawHit* rawHit = pxlRawHitCollection->rawHit(i+1, j+1, k+1, l);
                            StPxlCluster* cluster = new StPxlCluster();
                            findCluster(cluster, rawHit->column(), rawHit->row());
                            if(cluster->nRawHits() > 0)
                                {
                                    cluster->summarize();
                                    mPxlClusterCollection->addCluster(i+1, j+1, k+1, cluster);
                                }
                            else delete cluster;
                        }
                }

    return kStOK;
}

void StPxlClusterMaker::findCluster(StPxlCluster* cluster, Int_t column, Int_t row)
{    
    if(mBitMap[row][column]==0) return; ///< skip if already included in another cluster
    mBitMap[row][column]=0; ///< unmark this used raw hit
    /// looking at the 8 neighboring pixels, if fired, continue looking from that pixel
    if(column-1 >= 0) findCluster(cluster, column-1, row);
    if(column+1 < nPxlColumnsOnSensor) findCluster(cluster, column+1, row);
    if(row-1 >= 0) findCluster(cluster, column, row-1);
    if(row+1 < nPxlRowsOnSensor) findCluster(cluster, column, row+1);
    if(column-1 >=0 && row-1 >= 0) findCluster(cluster, column-1, row-1);
    if(column-1 >=0 && row+1 < nPxlRowsOnSensor) findCluster(cluster, column-1, row+1);
    if(column+1 < nPxlColumnsOnSensor && row-1 >= 0) findCluster(cluster, column+1, row-1);
    if(column+1 < nPxlColumnsOnSensor && row+1 < nPxlRowsOnSensor) findCluster(cluster, column+1, row+1);
    /// fill cluster
    cluster->addRawHit(column, row, mMapIdTruth[row][column]);
    
}


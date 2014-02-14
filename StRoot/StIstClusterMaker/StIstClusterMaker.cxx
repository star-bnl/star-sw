/***************************************************************************
*
* $Id: StIstClusterMaker.cxx,v 1.5 2014/02/14 14:45:56 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* See header file.
****************************************************************************
*
* $Log: StIstClusterMaker.cxx,v $
* Revision 1.5  2014/02/14 14:45:56  ypwang
* update due to removal of getNumLadders() member function from StIstCollection
*
* Revision 1.4  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstClusterMaker.cxx,v 1.0
* Revision 1.0 2013/11/04 15:55:30 Yaping
* Initial version
****************************************************************************/

#include "StIstClusterMaker.h"
#include "StEvent.h"
#include "StRoot/StIstUtil/StIstCollection.h"
#include "StRoot/StIstUtil/StIstRawHit.h"
#include "StRoot/StIstUtil/StIstRawHitCollection.h"
#include "StRoot/StIstUtil/StIstCluster.h"
#include "StRoot/StIstUtil/StIstClusterCollection.h"
#include "StRoot/StIstUtil/StIstConsts.h"
#include "StIstIClusterAlgo.h"
#include "StIstSimpleClusterAlgo.h"
#include "StIstScanClusterAlgo.h"

#include "StRoot/StIstDbMaker/StIstDbMaker.h"
#include "tables/St_istControl_Table.h"

StIstClusterMaker::StIstClusterMaker( const char* name ) : StMaker(name), mClusterAlgoPtr(0), mIstDbMaker(0), mTimeBin(-1), mSplitCluster(1)
{
  /* nothing to do */
};

Int_t StIstClusterMaker::Make()
{
  Int_t ierr = kStOk;

  //input data
  TObjectSet* istDataSet = (TObjectSet*)GetDataSet("istRawHitAndCluster");
  if (! istDataSet) {
      LOG_WARN << "Make() - there is no istDataSet (raw hit and cluster) " << endm;
      ierr = kStWarn;
  }

  StIstCollection* istCollectionPtr = (StIstCollection*)istDataSet->GetObject();
  if(!istCollectionPtr) {
      LOG_WARN << "Make() - no istCollection."<<endm;
      ierr = kStWarn;
  }   
 
  if( !ierr ){
      for( unsigned char ladderIdx=0; ladderIdx < kIstNumLadders; ++ladderIdx ){  
           StIstRawHitCollection *rawHitCollectionPtr = istCollectionPtr->getRawHitCollection( ladderIdx );
           StIstClusterCollection *clusterCollectionPtr = istCollectionPtr->getClusterCollection( ladderIdx );
       
           if( rawHitCollectionPtr && clusterCollectionPtr ){
		UShort_t numRawHits = rawHitCollectionPtr->getNumRawHits();
                if(numRawHits<mMinNumOfRawHits)   {
                    LOG_WARN <<"no rawHits found in ladder " << (short) (ladderIdx+1) << "! " <<endl;
                    continue;
                }
                if(numRawHits>mMaxNumOfRawHits)   { // set to 460 per ladder (~10% occupancy)
                    LOG_WARN <<"too large number of raw hits found in ladder " << (short) (ladderIdx+1) << "! " <<endl;
                    continue;
                }

               // clustering and splitting
	       mClusterAlgoPtr->setUsedTimeBin(mTimeBin);
	       mClusterAlgoPtr->setSplitFlag(mSplitCluster);
               Int_t loc_ierr = mClusterAlgoPtr->doClustering(*istCollectionPtr, *rawHitCollectionPtr, *clusterCollectionPtr );
               if(loc_ierr!=kStOk) {
                   LOG_WARN <<"StClusterMaker::Make(): clustering for ladder " << (short) (ladderIdx+1) << " returned " << loc_ierr <<endm;
                   if(loc_ierr>ierr)
                       ierr=loc_ierr;
               }
           }           
      }
  }

  LOG_DEBUG << "End of ist-clust-maker, print all raw hits & clusters: " << endm;
  LOG_DEBUG << "Total raw hits=" <<istCollectionPtr->getNumRawHits()<<", total Clusters=" <<  istCollectionPtr->getNumClusters() <<endm;
    
  if(Debug()>2) {
    Int_t rawHitIdx = 0, clusterIdx = 0;
    for(unsigned char iLadder=0; iLadder < kIstNumLadders; iLadder++) {
        LOG_DEBUG <<"Content: iLadder="<<(short) iLadder+1<< " # of : raw hits="<<istCollectionPtr->getNumRawHits(iLadder) <<"  clusters=" <<istCollectionPtr->getNumClusters( iLadder)<<endm;
        // ..... print all raw hits ....
        StIstRawHitCollection *rawHitPtr = istCollectionPtr->getRawHitCollection(iLadder);
        vector<StIstRawHit*> &rawHitVec = rawHitPtr->getRawHitVec();
        for( std::vector< StIstRawHit* >::iterator it=rawHitVec.begin();it!=rawHitVec.end();++it)    {
	    unsigned char maxTb = (*it)->getMaxTimeBin();
	    if( maxTb < 0 || maxTb >= kIstNumTimeBins)
		maxTb = (*it)->getDefaultTimeBin();

            LOG_DEBUG << "raw hit: Idx=" << rawHitIdx << " elecId=" << (*it)->getChannelId() << " Charge=" << (*it)->getCharge(maxTb) << " ChargeErr=" << (*it)->getChargeErr(maxTb) << " decode0: at ladder=" <<(short)(*it)->getLadder() << " sensor=" << (short)(*it)->getSensor() << " column=" <<(short)(*it)->getColumn() << " row=" << (short)(*it)->getRow() <<endm;
	    ++rawHitIdx;
        }
    
        // ..... print all 1D clusters  ....
        StIstClusterCollection *clustPtr= istCollectionPtr->getClusterCollection(iLadder);
        vector<StIstCluster*> &clustVec = clustPtr->getClusterVec();
        for( std::vector< StIstCluster* >::iterator it=clustVec.begin();it!=clustVec.end();++it)    {
            LOG_DEBUG << "cluster: Idx=" << clusterIdx << " totCharge=" << (*it)->getTotCharge() << " totChargeErr=" << (*it)->getTotChargeErr() << " meanColumn=" <<(*it)->getMeanColumn() << " meanRow= " << (*it)->getMeanRow() << " at ladder=" << (short)(*it)->getLadder() << " sensor=" << (short)(*it)->getSensor() << " clusterSize=" << (short)(*it)->getNRawHits() << " clusterSize(Z)=" << (short)(*it)->getNRawHitsZ() << " clusterSize(R-Phi)="<< (short)(*it)->getNRawHitsRPhi() << endm;
	    ++clusterIdx;
        }
    }
  }
  return ierr;

};

Int_t StIstClusterMaker::setClusterAlgo(StIstIClusterAlgo* algo)
{
    mClusterAlgoPtr=algo;
    return kStOk;
}

Int_t StIstClusterMaker::Init()
{
  Int_t ierr = kStOk;

  if( !mClusterAlgoPtr ){
     LOG_INFO << "IST clustering algorithm: Scanning algorithm" << endm;
     mClusterAlgoPtr=new StIstScanClusterAlgo();
  }

  if( !ierr )
     ierr = mClusterAlgoPtr->Init();

  mIstDbMaker = (StIstDbMaker*)GetMaker("istDb");
  if(!mIstDbMaker) {
      LOG_WARN << "Error getting IST Db maker handler" << endm;
      ierr = kStWarn;
  }

  return ierr;
};

Int_t StIstClusterMaker::InitRun(Int_t runnumber)
{
  Int_t ierr = kStOk;

  // control parameters
  St_istControl *istControl = mIstDbMaker->GetControl();
  if (!istControl)  LOG_WARN << " no istControl table " << endm;
  istControl_st *istControlTable = istControl->GetTable();

  mMinNumOfRawHits = istControlTable[0].kIstMinNumOfRawHits;
  mMaxNumOfRawHits = istControlTable[0].kIstMaxNumOfRawHits;

  return ierr;
};

ClassImp(StIstClusterMaker);

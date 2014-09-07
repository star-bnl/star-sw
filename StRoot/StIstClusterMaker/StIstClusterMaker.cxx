/***************************************************************************
*
* $Id: StIstClusterMaker.cxx,v 1.20 2014/09/07 11:44:52 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* See header file.
***************************************************************************/

#include "StIstClusterMaker.h"
#include "StEvent.h"
#include "StEvent/StEnumerations.h"
#include "StRoot/StIstUtil/StIstCollection.h"
#include "StRoot/StIstUtil/StIstRawHit.h"
#include "StRoot/StIstUtil/StIstRawHitCollection.h"
#include "StRoot/StIstUtil/StIstCluster.h"
#include "StRoot/StIstUtil/StIstClusterCollection.h"
#include "StRoot/StIstUtil/StIstConsts.h"
#include "StIstIClusterAlgo.h"
#include "StIstSimpleClusterAlgo.h"
#include "StIstScanClusterAlgo.h"

StIstClusterMaker::StIstClusterMaker( const char *name ) : StMaker(name), mIstCollectionPtr(0), mClusterAlgoPtr(0), mTimeBin(-1), mSplitCluster(1)
{
   /* nothing to do */
};

StIstClusterMaker::~StIstClusterMaker()
{
   if (mIstCollectionPtr) {
      delete mIstCollectionPtr;
   }

   if (mClusterAlgoPtr) {
      delete mClusterAlgoPtr;
   }
};

void StIstClusterMaker::Clear( Option_t *opts )
{
   if ( mIstCollectionPtr ) {
      for ( unsigned char i = 0; i < kIstNumLadders; ++i ) {
         mIstCollectionPtr->getRawHitCollection(i)->Clear( "" );
         mIstCollectionPtr->getClusterCollection(i)->Clear( "" );
      }
   }
};

Int_t StIstClusterMaker::Make()
{
   Int_t ierr = kStOk;

   //input data
   TObjectSet *istDataSet = (TObjectSet *)GetDataSet("istRawHitAndCluster");

   if (! istDataSet) {
      LOG_WARN << "Make() - there is no istDataSet (raw hit and cluster) " << endm;
      ierr = kStWarn;
   }

   mIstCollectionPtr = (StIstCollection *)istDataSet->GetObject();

   if (!mIstCollectionPtr) {
      LOG_WARN << "Make() - no istCollection." << endm;
      ierr = kStWarn;
   }

   if ( !ierr ) {
      for ( unsigned char ladderIdx = 0; ladderIdx < kIstNumLadders; ++ladderIdx ) {
         StIstRawHitCollection *rawHitCollectionPtr   = mIstCollectionPtr->getRawHitCollection( ladderIdx );
         StIstClusterCollection *clusterCollectionPtr = mIstCollectionPtr->getClusterCollection( ladderIdx );

         if ( rawHitCollectionPtr && clusterCollectionPtr ) {
            UShort_t numRawHits = rawHitCollectionPtr->getNumRawHits();
            LOG_DEBUG << "Number of raw hits found in ladder " << (short) (ladderIdx + 1) << ": " << numRawHits << endm;

            // clustering and splitting
            mClusterAlgoPtr->setUsedTimeBin(mTimeBin);
            mClusterAlgoPtr->setSplitFlag(mSplitCluster);
            Int_t loc_ierr = mClusterAlgoPtr->doClustering(*mIstCollectionPtr, *rawHitCollectionPtr, *clusterCollectionPtr );

            if (loc_ierr != kStOk) {
               LOG_WARN << "StClusterMaker::Make(): clustering for ladder " << (short) (ladderIdx + 1) << " returned " << loc_ierr << endm;

               if (loc_ierr > ierr)
                  ierr = loc_ierr;
            }
         }
      }
   }

   LOG_DEBUG << "End of ist-clust-maker, print all raw hits & clusters: " << endm;
   LOG_DEBUG << "Total raw hits=" << mIstCollectionPtr->getNumRawHits() << ", total Clusters=" <<  mIstCollectionPtr->getNumClusters() << endm;

   if (Debug() > 2) {
      static unsigned char nTimeBin = mIstCollectionPtr->getNumTimeBins();
      Int_t rawHitIdx = 0, clusterIdx = 0;

      for (unsigned char iLadder = 0; iLadder < kIstNumLadders; iLadder++) {
         LOG_DEBUG << "Content: iLadder=" << (short) iLadder + 1 << " # of : raw hits=" << mIstCollectionPtr->getNumRawHits(iLadder) << "  clusters=" << mIstCollectionPtr->getNumClusters( iLadder) << endm;
         // ..... print all raw hits ....
         StIstRawHitCollection *rawHitPtr = mIstCollectionPtr->getRawHitCollection(iLadder);
         vector<StIstRawHit *> &rawHitVec = rawHitPtr->getRawHitVec();

         for ( std::vector< StIstRawHit * >::iterator it = rawHitVec.begin(); it != rawHitVec.end(); ++it)    {
            unsigned char maxTb = (*it)->getMaxTimeBin();

            if ( maxTb < 0 || maxTb >= nTimeBin)
               maxTb = (*it)->getDefaultTimeBin();

            LOG_DEBUG << "raw hit: Idx=" << rawHitIdx << " elecId=" << (*it)->getChannelId() << " Charge=" << (*it)->getCharge(maxTb) << " ChargeErr=" << (*it)->getChargeErr(maxTb) << " decode0: at ladder=" << (short)(*it)->getLadder() << " sensor=" << (short)(*it)->getSensor() << " column=" << (short)(*it)->getColumn() << " row=" << (short)(*it)->getRow() << endm;
            ++rawHitIdx;
         }

         // ..... print all 1D clusters  ....
         StIstClusterCollection *clustPtr = mIstCollectionPtr->getClusterCollection(iLadder);
         vector<StIstCluster *> &clustVec = clustPtr->getClusterVec();

         for ( std::vector< StIstCluster * >::iterator it = clustVec.begin(); it != clustVec.end(); ++it)    {
            LOG_DEBUG << "cluster: Idx=" << clusterIdx << " totCharge=" << (*it)->getTotCharge() << " totChargeErr=" << (*it)->getTotChargeErr() << " meanColumn=" << (*it)->getMeanColumn() << " meanRow= " << (*it)->getMeanRow() << " at ladder=" << (short)(*it)->getLadder() << " sensor=" << (short)(*it)->getSensor() << " clusterSize=" << (short)(*it)->getNRawHits() << " clusterSize(Z)=" << (short)(*it)->getNRawHitsZ() << " clusterSize(R-Phi)=" << (short)(*it)->getNRawHitsRPhi() << endm;
            ++clusterIdx;
         }
      }
   }

   return ierr;

};

void StIstClusterMaker::setClusterAlgo(StIstIClusterAlgo *algo)
{
   mClusterAlgoPtr = algo;
}

Int_t StIstClusterMaker::Init()
{
   Int_t ierr = kStOk;

   if ( !mClusterAlgoPtr ) {
      LOG_INFO << "IST clustering algorithm: Scanning algorithm" << endm;
      mClusterAlgoPtr = new StIstScanClusterAlgo();
   }

   return ierr;
};

ClassImp(StIstClusterMaker);


/***************************************************************************
*
* $Log: StIstClusterMaker.cxx,v $
* Revision 1.20  2014/09/07 11:44:52  ypwang
* remove Init() function for clustering algorithms
*
* Revision 1.19  2014/09/07 11:34:42  ypwang
* update the setClusterAlgo() returning void instead of Int_t type
*
* Revision 1.18  2014/09/07 11:31:29  ypwang
* update the setClusterAlgo() returning void instead of Int_t type
*
* Revision 1.17  2014/09/07 08:15:18  ypwang
* destructor was added for the mIstCollectionPtr and mClusterAlgoPtr objects killing
*
* Revision 1.16  2014/08/22 15:55:15  smirnovd
* Fixed style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.15  2014/08/21 17:51:08  smirnovd
* Moved CVS history to the end of file
*
* Revision 1.14  2014/08/12 23:04:53  ypwang
* remove the raw hit number cut per ladder before doing clustering, due to chip occupancy cut was added in raw hit maker which can do the bad column rejection; simplfy the code by removing the InitRun() function
*
* Revision 1.13  2014/08/06 18:56:52  ypwang
* minor update due to coding style update of the StIstDb method
*
* Revision 1.12  2014/07/29 20:13:31  ypwang
* update the IST DB obtain method
*
* Revision 1.11  2014/04/15 06:46:59  ypwang
* updates for collections clear due to Clear() function removed from StIstCollection
*
* Revision 1.10  2014/03/25 03:06:52  ypwang
* updates on Db table accessory method
*
* Revision 1.9  2014/03/24 15:55:07  ypwang
* minor updates due to returned const pointers in StIstDbMaker
*
* Revision 1.8  2014/03/17 21:51:56  ypwang
* minor update due to some IST constants moved to StEnumurations.h
*
* Revision 1.7  2014/02/16 21:42:54  ypwang
* getting number of time bins used in current event by StIstCollection::getNumTimeBins() function
*
* Revision 1.6  2014/02/15 20:02:37  ypwang
* Clear() member function added, and mIstCollectionPtr data member defined
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

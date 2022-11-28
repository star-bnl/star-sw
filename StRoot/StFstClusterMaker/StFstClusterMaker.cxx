#include <climits>

#include "StFstClusterMaker.h"
#include "StEvent.h"
#include "StEvent/StEnumerations.h"
#include "StFstUtil/StFstCollection.h"
#include "StEvent/StFstRawHit.h"
#include "StFstUtil/StFstRawHitCollection.h"
#include "StFstUtil/StFstCluster.h"
#include "StFstUtil/StFstClusterCollection.h"
#include "StEvent/StFstConsts.h"
#include "StFstClusterMaker/StFstIClusterAlgo.h"
#include "StFstClusterMaker/StFstScanRadiusClusterAlgo.h"
StFstClusterMaker::StFstClusterMaker( const char *name ) : StMaker(name), mFstCollectionPtr(0), mClusterAlgoPtr(0), mTimeBin(UCHAR_MAX), mSplitCluster(true)
{};

StFstClusterMaker::~StFstClusterMaker()
{
//Is deleted as part of structure(VP)   delete mFstCollectionPtr;
   delete mClusterAlgoPtr;
};

void StFstClusterMaker::Clear( Option_t *opts )
{
   if ( mFstCollectionPtr ) {
      for ( unsigned char i = 0; i < kFstNumWedges; ++i ) {
         mFstCollectionPtr->getRawHitCollection(i)->Clear( "" );
         mFstCollectionPtr->getClusterCollection(i)->Clear( "" );
      }
   }
};


/**
 * Takes the StFstRawHitCollection from StFstCollection that is normally created
 * by the StFstRawHitMaker and passes it to the clustering algorithm to build
 * clusters. The clusters are then put in the StFstClusterCollection of the same
 * StFstCollection.
 */
Int_t StFstClusterMaker::Make()
{
   //input data
   TObjectSet *fstDataSet = (TObjectSet*) GetDataSet("fstRawHitAndCluster");

   if (!fstDataSet) {
      LOG_WARN << "Make() - fstRawHitAndCluster dataset not found. No FST clusters will be built" << endm;
      return kStWarn;
   }

   mFstCollectionPtr = (StFstCollection*) fstDataSet->GetObject();

   if (!mFstCollectionPtr) {
      LOG_WARN << "Make() - StFstCollection not found. No FST clusters will be built" << endm;
      return kStWarn;
   }

   mClusterAlgoPtr->setUsedTimeBin(mTimeBin);
   mClusterAlgoPtr->setSplitFlag(mSplitCluster);
   mClusterAlgoPtr->doClustering(*mFstCollectionPtr);


   if (Debug() >= 2) {

      LOG_DEBUG << "End of StFstClusterMaker::Make()" << endm
                << "Total raw hits: " << mFstCollectionPtr->getNumRawHits()
                << ", total clusters: " <<  mFstCollectionPtr->getNumClusters() << endm;

      for (unsigned char iWedge = 0; iWedge < kFstNumWedges; iWedge++)
      {
         LOG_DEBUG << "Content: iWedge=" << (short) iWedge + 1
                   << " # of : raw hits=" << mFstCollectionPtr->getNumRawHits(iWedge)
                   << "  clusters=" << mFstCollectionPtr->getNumClusters( iWedge) << endm;

         // Print all raw hits
         StFstRawHitCollection *rawHitPtr = mFstCollectionPtr->getRawHitCollection(iWedge);
         size_t nTimeBins = mFstCollectionPtr->getNumTimeBins();
         rawHitPtr->Print(nTimeBins);

         // Print all 1D clusters
         StFstClusterCollection *clustPtr = mFstCollectionPtr->getClusterCollection(iWedge);
         clustPtr->Print();
      }
   }

   return kStOk;
}

void StFstClusterMaker::setClusterAlgo(StFstIClusterAlgo *algo)
{
   mClusterAlgoPtr = algo;
}

Int_t StFstClusterMaker::Init()
{
   if ( !mClusterAlgoPtr ) {
      LOG_INFO << "FST clustering algorithm: Scanning Radius algorithm" << endm;
      mClusterAlgoPtr = new StFstScanRadiusClusterAlgo();
   }

   return kStOk;
};

ClassImp(StFstClusterMaker);

#include "StFstClusterMaker/StFstIClusterAlgo.h"
#include "StEvent/StEnumerations.h"
#include "St_base/StMessMgr.h"
#include "StEvent/StFstConsts.h"

/**
 * Calls the actual clustering method that creates a cluster collection from
 * a raw hit collection for each FST wedge.
 *
 * \author Shenghui Zhang
 */
void StFstIClusterAlgo::doClustering(StFstCollection &stFstCollection)
{
   for ( unsigned char wedgeIdx = 0; wedgeIdx < kFstNumWedges; ++wedgeIdx ) {
      StFstRawHitCollection  *stFstRawHitCollection  = stFstCollection.getRawHitCollection( wedgeIdx );
      StFstClusterCollection *stFstClusterCollection = stFstCollection.getClusterCollection( wedgeIdx );

      if ( !stFstRawHitCollection || !stFstClusterCollection ) {
         LOG_WARN << "StClusterMaker::Make(): No valid StFstRawHitCollection or StFstClusterCollection found for wedge "
                  << (short) (wedgeIdx + 1) << endm;
         continue;
      }

      // clustering and splitting
      doClustering(stFstCollection, *stFstRawHitCollection, *stFstClusterCollection);
   }
}

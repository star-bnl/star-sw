#include "StIstClusterMaker/StIstIClusterAlgo.h"
#include "StEvent/StEnumerations.h"
#include "St_base/StMessMgr.h"


/**
 * Calls the actual clustering method that creates a cluster collection from
 * a raw hit collection for each IST ladder.
 *
 * \author Dmitri Smirnov
 */
void StIstIClusterAlgo::doClustering(StIstCollection &stIstCollection)
{
   for ( unsigned char ladderIdx = 0; ladderIdx < StIstConsts::kIstNumLadders; ++ladderIdx ) {
      StIstRawHitCollection  *stIstRawHitCollection  = stIstCollection.getRawHitCollection( ladderIdx );
      StIstClusterCollection *stIstClusterCollection = stIstCollection.getClusterCollection( ladderIdx );

      if ( !stIstRawHitCollection || !stIstClusterCollection ) {
         LOG_WARN << "StClusterMaker::Make(): No valid StIstRawHitCollection or StIstClusterCollection found for ladder "
                  << (short) (ladderIdx + 1) << endm;
         continue;
      }

      // clustering and splitting
      doClustering(stIstCollection, *stIstRawHitCollection, *stIstClusterCollection);
   }
}

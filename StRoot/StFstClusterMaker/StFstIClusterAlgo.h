#ifndef StFstIClusterAlgo_hh
#define StFstIClusterAlgo_hh

#include <climits>

#include "Stypes.h"
#include "StFstUtil/StFstCollection.h"

class StFstRawHitCollection;
class StFstClusterCollection;


/**
 * Abstract interface for concrete implementations of clustering algorithms.
 *
 * \author Shenghui Zhang \date Sep 2021
 */
class StFstIClusterAlgo
{
public:
   void doClustering(StFstCollection &stFstCollection);

   virtual ~StFstIClusterAlgo() = 0;

   void setUsedTimeBin(unsigned char tb = UCHAR_MAX) { mTimeBin = tb; }
   void setSplitFlag( bool splitFlag = true)  { mSplitCluster = splitFlag; }

protected:

   virtual Int_t doClustering(const StFstCollection &, StFstRawHitCollection &, StFstClusterCollection &) = 0;

   Bool_t mSplitCluster;
   UChar_t mTimeBin;
};

#endif
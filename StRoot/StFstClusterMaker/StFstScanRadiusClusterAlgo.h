#ifndef StFstScanRadiusClusterAlgo_hh
#define StFstScanRadiusClusterAlgo_hh

#include "StFstClusterMaker/StFstIClusterAlgo.h"

class StFstCollection;
class StFstRawHitCollection;
class StFstClusterCollection;


/**
 * Concrete implementation of a simple algorithm for clustering of the raw hits
 * registered by the 2D FST sensors. The clustering is done sequentially--first
 * in phistrips and then the phistrip-wise proto-clusters are grouped in neighboring
 * phistrips.
 *
 * 1) Reads all raw hits per wedge (three sensors) and groups into vectors
 * (each vector corresponds to a sensor phistrip).
 * 2) Does clustering in individual phistrip.
 * 3) Does clustering in neighboring phistrips.
 * 4) Fill hit collections.
 *
 * \author Shenghui Zhang \date Sep 2021
 */
class StFstScanRadiusClusterAlgo : public StFstIClusterAlgo
{
   ~StFstScanRadiusClusterAlgo(){}
protected:
   enum {kFstScanRadiusClusterAlgo = 2};

   virtual Int_t doClustering(const StFstCollection &fstCollection, StFstRawHitCollection &rawHits, StFstClusterCollection &clusters );
};

#endif

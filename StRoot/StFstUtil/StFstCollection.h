#ifndef StFstCollection_hh
#define StFstCollection_hh

#include "StObject.h"
#include "StEvent/StEnumerations.h"
#include "StFstUtil/StFstRawHitCollection.h"
#include "StFstUtil/StFstClusterCollection.h"
#include "StEvent/StFstConsts.h"

//using namespace StFstConsts;
/**
 * A data collection for StFstRawHitCollection and StFstClusterCollection
 * classes, and not written into StEvent.
 *
 * \author Shenghui Zhang
 * \date Sep. 2021
 */
class StFstCollection : public StObject
{
public:
   StFstCollection();
   ~StFstCollection();

   size_t getNumRawHits() const;                   // overall
   size_t getNumRawHits( int wedge) const;      // per wedge
   size_t getNumClusters() const;                  // overall
   size_t getNumClusters( int wedge ) const;    // per wedge
   unsigned char getNumTimeBins() const;
   void setNumTimeBins(unsigned char nTimebin);

   StFstRawHitCollection *getRawHitCollection( int wedge );
   const StFstRawHitCollection *getRawHitCollection( int wedge ) const;

   StFstClusterCollection *getClusterCollection( int wedge );
   const StFstClusterCollection *getClusterCollection( int wedge ) const;

protected:
   StFstRawHitCollection mRawHitCollection[kFstNumWedges];
   StFstClusterCollection mClusterCollection[kFstNumWedges];
   unsigned char mNumTimeBins;

   ClassDef(StFstCollection, 1);
};

#endif

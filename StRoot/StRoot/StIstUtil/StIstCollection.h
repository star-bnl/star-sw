#ifndef StIstCollection_hh
#define StIstCollection_hh

#include "StObject.h"
#include "StEvent/StEnumerations.h"
#include "StIstRawHitCollection.h"
#include "StIstClusterCollection.h"
#include "StIstConsts.h"

using namespace StIstConsts;


/**
 * A data collection for StIstRawHitCollection and StIstClusterCollection
 * classes, and not written into StEvent.
 *
 * \author Yaping Wang
 * \date March 2013
 */
class StIstCollection : public StObject
{
public:
   StIstCollection();
   ~StIstCollection();

   size_t getNumRawHits() const;                   // overall
   size_t getNumRawHits( int ladder) const;      // per ladder
   size_t getNumClusters() const;                  // overall
   size_t getNumClusters( int ladder ) const;    // per ladder
   unsigned char getNumTimeBins() const;
   void setNumTimeBins(unsigned char nTimebin);

   StIstRawHitCollection *getRawHitCollection( int ladder );
   const StIstRawHitCollection *getRawHitCollection( int ladder ) const;

   StIstClusterCollection *getClusterCollection( int ladder );
   const StIstClusterCollection *getClusterCollection( int ladder ) const;

protected:
   StIstRawHitCollection mRawHitCollection[kIstNumLadders];
   StIstClusterCollection mClusterCollection[kIstNumLadders];
   unsigned char mNumTimeBins;

   ClassDef(StIstCollection, 1);
};

#endif

/***************************************************************************
* $Id: StIstCollection.h,v 1.4 2014/02/13 02:35:49 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* A data collection for StIstRawHitCollection and StIstClusterCollection
* classes, and not written into StEvent.
****************************************************************************/

#ifndef StIstCollection_hh
#define StIstCollection_hh

#include "StObject.h"
#include "StIstRawHitCollection.h"
#include "StIstClusterCollection.h"
#include "StIstConsts.h"

class StIstCollection : public StObject
{
public:
   //constructors
   StIstCollection();

   //deconstructor
   ~StIstCollection();

   size_t getNumLadders() const;
   size_t getNumRawHits() const;                   // overall
   size_t getNumRawHits( unsigned char ladder) const;      // per ladder
   size_t getNumClusters() const;                  // overall
   size_t getNumClusters( unsigned char ladder ) const;    // per ladder
   size_t getNumTimeBins() const;
   void setNumTimeBins(size_t nTimebin);

   StIstRawHitCollection *getRawHitCollection( unsigned char ladder );
   const StIstRawHitCollection *getRawHitCollection( unsigned char ladder ) const;

   StIstClusterCollection *getClusterCollection( unsigned char ladder );
   const StIstClusterCollection *getClusterCollection( unsigned char ladder ) const;

   void Clear( Option_t *opts = "" );

protected:
   friend class StMuDstMaker; // needed for StMuDstMaker

   StIstRawHitCollection mRawHitCollection[kIstNumLadders];
   StIstClusterCollection mClusterCollection[kIstNumLadders];
   size_t mNumTimeBins;

private:
   ClassDef(StIstCollection, 1);
};

#endif


/***************************************************************************
*
* $Log: StIstCollection.h,v $
* Revision 1.4  2014/02/13 02:35:49  smirnovd
* Moved CVS log to the bottom of the file
*
* Revision 1.3  2014/02/03 16:12:20  ypwang
* updating scripts
*
*
****************************************************************************
* StIstCollection.h,v 1.0
* Revision 1.0 2013/11/04 15:15:30 Yaping
* Initial version
****************************************************************************/

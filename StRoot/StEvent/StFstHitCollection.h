/***************************************************************************
*
* $Id: StFstHitCollection.h $
*
* Author: Shenghui Zhang, Oct. 2021
****************************************************************************
* Description:
* Data collection for FST hits, and is saved to StEvent.
***************************************************************************/

#ifndef StFstHitCollection_hh
#define StFstHitCollection_hh

#include "StObject.h"
#include "StFstWedgeHitCollection.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StFstConsts.h"

class StFstHit;

class StFstHitCollection : public StObject
{
public:
   StFstHitCollection();

   bool          addHit(StFstHit *);
   unsigned int  numberOfHits() const;

   unsigned char getClusteringType() const;
   void          setClusteringType(unsigned char clusteringType);

   StFstWedgeHitCollection       *wedge(unsigned int);
   const StFstWedgeHitCollection *wedge(unsigned int) const;

private:
   StFstWedgeHitCollection mWedges[kFstNumWedges];
   UChar_t mClusteringType;  // clustering algorithm type

   ClassDef(StFstHitCollection, 1)
};
#endif


/***************************************************************************
* StFstHitCollection.h,v 1.0
* Revision 1.0 2021/10/04 Shenghui Zhang
* Initial version
****************************************************************************/

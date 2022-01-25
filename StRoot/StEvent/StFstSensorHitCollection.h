/***************************************************************************
* $Id: StFstSensorHitCollection.h$
*
* Author: Shenghui Zhang, Oct. 2021
****************************************************************************
* Description:
* Data collection for FST hits, and one instance corresponds to one sensor.
***************************************************************************/

#ifndef StFstSensorHitCollection_hh
#define StFstSensorHitCollection_hh

#include "StObject.h"
#include "StContainers.h"

class StFstHit;

class StFstSensorHitCollection : public StObject
{
public:
   StFstSensorHitCollection();
   ~StFstSensorHitCollection();

   unsigned int numberOfHits() const;

   StSPtrVecFstHit       &hits();
   const StSPtrVecFstHit &hits() const;

private:
   StSPtrVecFstHit mHits; ///< Inherits from StStrArray which takes care of deleting the objects
                          ///< pointed by the pointers in this array. This is different from the std::vector

   ClassDef(StFstSensorHitCollection, 1)
};
#endif


/***************************************************************************
* StFstSensorHitCollection.h,v 1.0
* Revision 1.0 2021/10/04 Shenghui Zhang
* Initial version
****************************************************************************/

/***************************************************************************
* $Id: StFstWedgeHitCollection.h$
*
* Author: Shenghui Zhang, Oct. 2021
****************************************************************************
* Description:
* Data collection for FST hits, and one instance corresponds to one wedge.
***************************************************************************/

#ifndef StFstWedgeHitCollection_hh
#define StFstWedgeHitCollection_hh

#include "StObject.h"
#include "StFstSensorHitCollection.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StFstConsts.h"


class StFstWedgeHitCollection : public StObject
{
public:
   StFstWedgeHitCollection();

   unsigned int  numberOfHits() const;

   StFstSensorHitCollection       *sensor(unsigned int);
   const StFstSensorHitCollection *sensor(unsigned int) const;

private:
   StFstSensorHitCollection  mSensors[kFstNumSensorsPerWedge];

   ClassDef(StFstWedgeHitCollection, 1)
};
#endif


/***************************************************************************
* StFstWedgeHitCollection.h,v 1.0
* Revision 1.0 2013/10/04 Shenghui Zhang
* Initial version
****************************************************************************/

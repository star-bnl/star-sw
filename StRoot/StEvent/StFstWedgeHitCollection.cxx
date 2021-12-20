/***************************************************************************
* $Id: StFstWedgeHitCollection.cxx$
*
* Author: Shenghui Zhang, Oct. 2021
****************************************************************************
* Description:
* See header file.
***************************************************************************/

#include "StFstWedgeHitCollection.h"

ClassImp(StFstWedgeHitCollection)

StFstWedgeHitCollection::StFstWedgeHitCollection() : StObject() { /* noop */ }

unsigned int StFstWedgeHitCollection::numberOfHits() const
{
   unsigned int sum = 0;

   for (unsigned int j = 0; j < kFstNumSensorsPerWedge; j++) {
      sum += mSensors[j].hits().size();
   }

   return sum;
}

StFstSensorHitCollection* StFstWedgeHitCollection::sensor(unsigned int i)
{
   if (i < kFstNumSensorsPerWedge)
      return &(mSensors[i]);
   else
      return 0;
}

const StFstSensorHitCollection* StFstWedgeHitCollection::sensor(unsigned int i) const
{
   if (i < kFstNumSensorsPerWedge)
      return &(mSensors[i]);
   else
      return 0;
}


/***************************************************************************
* StFstWedgeHitCollection.cxx,v 1.0
* Revision 1.0 2021/10/04 Shenghui Zhang
* Initial version
****************************************************************************/

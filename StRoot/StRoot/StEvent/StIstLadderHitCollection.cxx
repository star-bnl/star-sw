/***************************************************************************
*
* $Id: StIstLadderHitCollection.cxx,v 2.1 2014/04/10 16:20:09 jeromel Exp $
*
* Author: Yaping Wang, August 2013
****************************************************************************
* Description:
* See header file.
***************************************************************************/

#include "StIstLadderHitCollection.h"

static const char rcsid[] = "$Id: StIstLadderHitCollection.cxx,v 2.1 2014/04/10 16:20:09 jeromel Exp $";

ClassImp(StIstLadderHitCollection)

StIstLadderHitCollection::StIstLadderHitCollection() : StObject() { /* noop */ }

StIstLadderHitCollection::~StIstLadderHitCollection() { /* noop */ }

unsigned int StIstLadderHitCollection::numberOfHits() const
{
   unsigned int sum = 0;

   for (unsigned int j = 0; j < kIstNumSensorsPerLadder; j++) {
      sum += mSensors[j].hits().size();
   }

   return sum;
}

StIstSensorHitCollection* StIstLadderHitCollection::sensor(unsigned int i)
{
   if (i < kIstNumSensorsPerLadder)
      return &(mSensors[i]);
   else
      return 0;
}

const StIstSensorHitCollection* StIstLadderHitCollection::sensor(unsigned int i) const
{
   if (i < kIstNumSensorsPerLadder)
      return &(mSensors[i]);
   else
      return 0;
}


/***************************************************************************
*
* $Log: StIstLadderHitCollection.cxx,v $
* Revision 2.1  2014/04/10 16:20:09  jeromel
* Ist struct (Thomas OK-ed)
*
* Revision 1.7  2014/03/17 20:27:57  ypwang
* remove numOfLadder() and numOfSensor() from StIstHitCollection.h and StIstLadderHitCollection.h, respectively
*
* Revision 1.6  2014/03/13 22:10:21  smirnovd
* Fixed constructor's initialization list
*
* Revision 1.5  2014/03/13 22:10:12  smirnovd
* Move some constants from StIstUtil/StIstConsts.h to StEvent/StEnumerations.h to avoid external dependance of StEvent on StIstUtil
*
* Revision 1.4  2014/03/13 22:05:25  smirnovd
* Style issue: Function return types on same line
*
* Revision 1.3  2014/02/26 21:18:08  smirnovd
* Style corrected with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.2  2014/01/29 18:25:00  ypwang
* updating scripts
*
*
****************************************************************************
* StIstLadderHitCollection.cxx,v 1.0
* Revision 1.0 2013/11/04 15:25:30 Yaping
* Initial version
****************************************************************************/

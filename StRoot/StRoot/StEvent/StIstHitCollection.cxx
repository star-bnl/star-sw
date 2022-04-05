/***************************************************************************
*
* $Id: StIstHitCollection.cxx,v 2.1 2014/04/10 16:17:23 jeromel Exp $
*
* Author: Yaping Wang, August 2013
****************************************************************************
* Description:
* See header file.
***************************************************************************/

#include "StIstHitCollection.h"
#include "StIstLadderHitCollection.h"
#include "StIstSensorHitCollection.h"
#include "StIstHit.h"

static const char rcsid[] = "$Id: StIstHitCollection.cxx,v 2.1 2014/04/10 16:17:23 jeromel Exp $";

ClassImp(StIstHitCollection)

StIstHitCollection::StIstHitCollection() : StObject() { /* no op */ }

StIstHitCollection::~StIstHitCollection() { /* no op */ }

bool StIstHitCollection::addHit(StIstHit *hit)
{
   unsigned int l, w;
   l = (unsigned int)hit->getLadder() - 1;
   w = (unsigned int)hit->getSensor() - 1;

   if (hit &&
         l < kIstNumLadders &&
         w < kIstNumSensorsPerLadder) {
      mLadders[l].sensor(w)->hits().push_back(hit);
      return kTRUE;
   }
   else {
      return kFALSE;
   }
}

unsigned int StIstHitCollection::numberOfHits() const
{
   unsigned int sum = 0;

   for (unsigned int i = 0; i < kIstNumLadders; i++)
      for (unsigned int j = 0; j < kIstNumSensorsPerLadder; j++)
         sum += mLadders[i].sensor(j)->hits().size();

   return sum;
}

unsigned char StIstHitCollection::getClusteringType() const {   return mClusteringType; }
void StIstHitCollection::setClusteringType(unsigned char clusteringType) {   mClusteringType = clusteringType; }

StIstLadderHitCollection* StIstHitCollection::ladder(unsigned int i)
{
   if (i < kIstNumLadders)
      return &(mLadders[i]);
   else
      return 0;
}

const StIstLadderHitCollection* StIstHitCollection::ladder(unsigned int i) const
{
   if (i < kIstNumLadders)
      return &(mLadders[i]);
   else
      return 0;
}


/***************************************************************************
*
* $Log: StIstHitCollection.cxx,v $
* Revision 2.1  2014/04/10 16:17:23  jeromel
* Add hit def for Ist (Thomas OK-ed)
*
* Revision 1.8  2014/03/17 20:27:57  ypwang
* remove numOfLadder() and numOfSensor() from StIstHitCollection.h and StIstLadderHitCollection.h, respectively
*
* Revision 1.7  2014/03/13 22:10:21  smirnovd
* Fixed constructor's initialization list
*
* Revision 1.6  2014/03/13 22:10:12  smirnovd
* Move some constants from StIstUtil/StIstConsts.h to StEvent/StEnumerations.h to avoid external dependance of StEvent on StIstUtil
*
* Revision 1.5  2014/03/13 22:05:24  smirnovd
* Style issue: Function return types on same line
*
* Revision 1.4  2014/02/26 21:18:08  smirnovd
* Style corrected with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.3  2014/02/25 17:08:49  ypwang
* add mClusteringType and its accessory/modifier functions
*
*
****************************************************************************
* StIstHitCollection.cxx,v 1.0
* Revision 1.0 2013/11/04 15:25:30 Yaping
* Initial version
****************************************************************************/

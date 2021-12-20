/***************************************************************************
* $Id: StFstHitCollection.cxx$
*
* Author: Shenghui Zhang, Oct. 2021
****************************************************************************
* Description:
* See header file.
***************************************************************************/

#include "StFstHitCollection.h"
#include "StFstWedgeHitCollection.h"
#include "StFstSensorHitCollection.h"
#include "StFstHit.h"

ClassImp(StFstHitCollection)

StFstHitCollection::StFstHitCollection() : StObject() { /* no op */ }

bool StFstHitCollection::addHit(StFstHit *hit)
{
   unsigned int l, w;
   l = (unsigned int)hit->getWedge() - 1;
   w = (unsigned int)hit->getSensor();

   if (hit &&
         l < kFstNumWedges &&
         w < kFstNumSensorsPerWedge) {
      mWedges[l].sensor(w)->hits().push_back(hit);
      return kTRUE;
   }
   else {
      return kFALSE;
   }
}

unsigned int StFstHitCollection::numberOfHits() const
{
   unsigned int sum = 0;

   for (unsigned int i = 0; i < kFstNumWedges; i++)
      for (unsigned int j = 0; j < kFstNumSensorsPerWedge; j++)
         sum += mWedges[i].sensor(j)->hits().size();

   return sum;
}

unsigned char StFstHitCollection::getClusteringType() const {   return mClusteringType; }
void StFstHitCollection::setClusteringType(unsigned char clusteringType) {   mClusteringType = clusteringType; }

StFstWedgeHitCollection* StFstHitCollection::wedge(unsigned int i)
{
   if (i < kFstNumWedges)
      return &(mWedges[i]);
   else
      return 0;
}

const StFstWedgeHitCollection* StFstHitCollection::wedge(unsigned int i) const
{
   if (i < kFstNumWedges)
      return &(mWedges[i]);
   else
      return 0;
}


/***************************************************************************
* StFstHitCollection.cxx,v 1.0
* Revision 1.0 2021/10/04 Shenghui Zhang
* Initial version
****************************************************************************/

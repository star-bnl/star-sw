/***************************************************************************
* $Id: StFstSensorHitCollection.cxx$
*
* Author: Shenghui Zhang, Oct. 2021
****************************************************************************
* Description:
* See header file.
***************************************************************************/

#include "StFstSensorHitCollection.h"
#include "StFstHit.h"

ClassImp(StFstSensorHitCollection)

StFstSensorHitCollection::StFstSensorHitCollection() : StObject() { /* noop */ }

StFstSensorHitCollection::~StFstSensorHitCollection()
{
   // Usually this wouldn't be necessary but mHits is a polymorphic container and StFstHit provides
   // its own new/delete operator
   for (unsigned int i=0; i<mHits.size(); i++) {
       delete mHits[i];
       mHits[i] = 0;
   }
}

unsigned int StFstSensorHitCollection::numberOfHits() const
{
   return mHits.size();
}

const StSPtrVecFstHit & StFstSensorHitCollection::hits() const { return mHits; }

StSPtrVecFstHit & StFstSensorHitCollection::hits() { return mHits; }


/***************************************************************************
* StFstSensorHitCollection.cxx,v 1.0
* Revision 1.0 2021/10/04 Shenghui Zhang
* Initial version
****************************************************************************/

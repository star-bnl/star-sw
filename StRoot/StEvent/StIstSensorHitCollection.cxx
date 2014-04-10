/***************************************************************************
*
* $Id: StIstSensorHitCollection.cxx,v 2.1 2014/04/10 16:21:04 jeromel Exp $
*
* Author: Yaping Wang, August 2013
****************************************************************************
* Description:
* See header file.
***************************************************************************/

#include "StIstSensorHitCollection.h"
#include "StIstHit.h"

static const char rcsid[] = "$Id: StIstSensorHitCollection.cxx,v 2.1 2014/04/10 16:21:04 jeromel Exp $";

ClassImp(StIstSensorHitCollection)

StIstSensorHitCollection::StIstSensorHitCollection() : StObject() { /* noop */ }

StIstSensorHitCollection::~StIstSensorHitCollection()
{
   // Usually this wouldn't be necessary but mHits is a polymorphic container and StIstHit provides
   // its own new/delete operator
   for (unsigned int i=0; i<mHits.size(); i++) {
       delete mHits[i];
       mHits[i] = 0;
   }
}

const StSPtrVecIstHit & StIstSensorHitCollection::hits() const { return mHits; }

StSPtrVecIstHit & StIstSensorHitCollection::hits() { return mHits; }


/***************************************************************************
*
* $Log: StIstSensorHitCollection.cxx,v $
* Revision 2.1  2014/04/10 16:21:04  jeromel
* Ist struct (Thomas OK-ed)
*
* Revision 1.6  2014/03/13 22:10:36  smirnovd
* Collapse vector after clean up
*
* Revision 1.5  2014/03/13 22:10:21  smirnovd
* Fixed constructor's initialization list
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
* StIstSensorHitCollection.cxx,v 1.0
* Revision 1.0 2013/11/04 15:25:30 Yaping
* Initial version
****************************************************************************/

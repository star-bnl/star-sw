/***************************************************************************
*
* $Id: StFstEvtCollection.h $
*
* Author: Te-Chuan Huang, Aug. 2022
****************************************************************************
* Description:
* Data collection for FST raw hits, and is saved to StEvent.
***************************************************************************/

#ifndef StFstEvtCollection_hh
#define StFstEvtCollection_hh

#include "Stiostream.h"
#include "StObject.h"
#include "StContainers.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StFstConsts.h"

class StFstRawHit;

class StFstEvtCollection : public StObject
{
public:
   StFstEvtCollection();
   ~StFstEvtCollection() {}

   void          addRawHit(StFstRawHit *);
   unsigned int  numberOfRawHits() const;

   StSPtrVecFstRawHit       &rawHits();
   const StSPtrVecFstRawHit &rawHits() const;

   void print(int option=1);

private:
   StSPtrVecFstRawHit mRawHits; ///< Inherits from StStrArray which takes care of deleting the objects
                                ///< pointed by the pointers in this array. This is different from the std::vector

   ClassDef(StFstEvtCollection, 1)
};
#endif

/***************************************************************************
* StFstEvtCollection.h,v 1.0
* Revision 1.0 2022/08/25 Te-Chuan Huang
* Initial version
****************************************************************************/

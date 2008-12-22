/*!
 * \class StTofCollection 
 * \author Xin Dong, Nov 2008
 */
/***************************************************************************
 *
 * $Id: StBTofCollection.h,v 2.1 2008/12/22 20:30:55 ullrich Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description:
 *
 * Persistent data which is written into StEvent
 * directly from the reco chain. All Barrel ToF stuff goes here
 * except the StBTofPidTraits.
 *
 ***************************************************************************
 *
 * $Log: StBTofCollection.h,v $
 * Revision 2.1  2008/12/22 20:30:55  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#ifndef StBTofCollection_hh
#define StBTofCollection_hh

#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"
#include "StBTofHeader.h"
#include "StBTofHit.h"
#include "StBTofRawHit.h"

class StBTofCollection : public StObject {
public: 
    StBTofCollection();
    ~StBTofCollection();

    const StBTofHeader*         tofHeader() const;
    StBTofHeader*               tofHeader();

    const StSPtrVecBTofHit&     tofHits() const;
    StSPtrVecBTofHit&           tofHits();

    const StSPtrVecBTofRawHit&  tofRawHits() const;
    StSPtrVecBTofRawHit&        tofRawHits();

    void setHeader(StBTofHeader*);

    void addHit(const StBTofHit*);
    void addRawHit(const StBTofRawHit*);

    bool hitsPresent()     const;
    bool rawHitsPresent()  const;
    
private:
    StBTofHeader*               mBTofHeader;

    StSPtrVecBTofHit            mBTofHits;
    StSPtrVecBTofRawHit         mBTofRawHits;
  
    ClassDef(StBTofCollection, 1)
};

#endif

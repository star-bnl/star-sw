/***************************************************************************
 *
 * $Id: StTofCollection.h,v 2.2 2001/04/24 18:20:13 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 * Persistent data which is written into StEvent
 * directly from the reco chain. All ToF stuff goes here
 * except the StTofPidTraits and the StTofSoftwareMonitor.
 *
 ***************************************************************************
 *
 * $Log: StTofCollection.h,v $
 * Revision 2.2  2001/04/24 18:20:13  ullrich
 * Added hits and slats to collection.
 *
 * Revision 2.1  2000/12/08 03:52:43  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTofCollection_hh
#define StTofCollection_hh

#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"
#include "StTofHit.h"
#include "StTofSlat.h"

class StTofCollection : public StObject {
public:
    StTofCollection();
    ~StTofCollection();
//  StTofCollection(const StTofCollection&) { /* use default */ }
//  StTofCollection& operator=(const StTofCollection&) {/* use default */}

    const StSPtrVecTofSlat&    tofSlats() const;
    StSPtrVecTofSlat&          tofSlats();
    
    const StSPtrVecTofHit&     tofHits() const;
    StSPtrVecTofHit&           tofHits();

    void addSlat(const StTofSlat*);
    void addHit(const StTofHit*);

    bool slatsPresent()    const;
    bool hitsPresent()     const;
    
private:
    StSPtrVecTofSlat           mTofSlats;
    StSPtrVecTofHit            mTofHits;
  
    ClassDef(StTofCollection, 1)
};
#endif

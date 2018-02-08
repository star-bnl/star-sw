/*!
 * \class StEpdCollection
 * \author Mike Lisa, Jan 2018
 */
/***************************************************************************
 *
 * $Id: StEpdCollection.h,v 2.1 2018/02/08 17:35:02 ullrich Exp $
 *
 * Author: Mike Lisa, Jan 2018
 ***************************************************************************
 *
 * Description:
 *
 * Persistent data which is written into StEvent
 * based on TriggerData object and database information
 * filled by StEpdHitMaker
 *
 * Using StBTofCollection as a template
 *
 ***************************************************************************
 *
 * $Log: StEpdCollection.h,v $
 * Revision 2.1  2018/02/08 17:35:02  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StEpdCollection_hh
#define StEpdCollection_hh

#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"
#include "StEpdHit.h"

class StEpdCollection : public StObject {
public: 
    StEpdCollection();
    ~StEpdCollection();

    const StSPtrVecEpdHit&     epdHits() const;
    StSPtrVecEpdHit&           epdHits();

    void addHit(const StEpdHit*);

    bool hitsPresent() const;
    
private:

    StSPtrVecEpdHit mEpdHits;
  
    ClassDef(StEpdCollection, 1)
};

#endif

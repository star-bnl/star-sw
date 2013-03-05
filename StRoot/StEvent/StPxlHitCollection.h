/*!
 * \class StPxlHitCollection 
 * \author X. Dong, Jan 2013
 */
/***************************************************************************
 *
 * $Id: StPxlHitCollection.h,v 2.1 2013/03/05 14:40:40 ullrich Exp $
 *
 * Author: X. Dong, Jan 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlHitCollection.h,v $
 * Revision 2.1  2013/03/05 14:40:40  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StPxlHitCollection_hh
#define StPxlHitCollection_hh

#include "StObject.h"
#include "StPxlSectorHitCollection.h"

class StPxlHit;

class StPxlHitCollection : public StObject {
public:
    StPxlHitCollection();
    ~StPxlHitCollection();
    
    bool          addHit(StPxlHit*);
    unsigned int  numberOfHits() const;
    unsigned int  numberOfSectors() const;
    
    StPxlSectorHitCollection*       sector(unsigned int);
    const StPxlSectorHitCollection* sector(unsigned int) const;
    
private:
    enum { mNumberOfSectors = 10 };
    StPxlSectorHitCollection mSectors[mNumberOfSectors];
    
    ClassDef(StPxlHitCollection,1)
};

inline unsigned int StPxlHitCollection::numberOfSectors() const { return mNumberOfSectors; }

#endif

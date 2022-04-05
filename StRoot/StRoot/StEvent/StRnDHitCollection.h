/*!
 * \class StRnDHitCollection 
 * \author Thomas Ullrich, Jan 2006
 */
/***************************************************************************
 *
 * $Id: StRnDHitCollection.h,v 2.1 2006/01/19 21:42:06 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 2006
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRnDHitCollection.h,v $
 * Revision 2.1  2006/01/19 21:42:06  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StRnDHitCollection_hh
#define StRnDHitCollection_hh

#include "StObject.h"
#include "StContainers.h"

class StRnDHit;

class StRnDHitCollection : public StObject {
public:
    StRnDHitCollection();
    ~StRnDHitCollection();
    // StRnDHitCollection(const StRnDHitCollection&); use default
    // const StRnDHitCollection&
    // operator=(const StRnDHitCollection&);           use default
    
    StSPtrVecRnDHit&       hits();
    const StSPtrVecRnDHit& hits() const;

    bool          addHit(StRnDHit*);
    unsigned int  numberOfHits() const;
    
private:
    StSPtrVecRnDHit mHits;
    
    ClassDef(StRnDHitCollection,1)
};

inline const StSPtrVecRnDHit& StRnDHitCollection::hits() const { return mHits; }
inline StSPtrVecRnDHit& StRnDHitCollection::hits() { return mHits; }
inline unsigned int StRnDHitCollection::numberOfHits() const {return mHits.size();}

#endif

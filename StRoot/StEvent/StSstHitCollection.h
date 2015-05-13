/*!
 * \class StSstHitCollection 
 * \author Jonathan Bouchet, Thomas Ullrich, May 2015
 */
/***************************************************************************
 *
 * $Id: StSstHitCollection.h,v 2.1 2015/05/13 16:50:59 ullrich Exp $
 *
 * Author: Jonathan Bouchet, Thomas Ullrich, May 2015
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSstHitCollection.h,v $
 * Revision 2.1  2015/05/13 16:50:59  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StSstHitCollection_hh
#define StSstHitCollection_hh

#include "StObject.h"
#include "StSstLadderHitCollection.h"
class StSstHit;

class StSstHitCollection : public StObject {
public:
    StSstHitCollection();
    ~StSstHitCollection();
    // StSstHitCollection(const StSstHitCollection&);            use default
    // StSstHitCollection& operator=(const StSstHitCollection&); use default
    
    bool          addHit(StSstHit*);
    unsigned int  numberOfHits() const;
    unsigned int  numberOfLadders() const;
    
    StSstLadderHitCollection*       ladder(unsigned int);
    const StSstLadderHitCollection* ladder(unsigned int) const;

private:
    enum { mNumberOfLadders = 20 };
    StSstLadderHitCollection mLadders[mNumberOfLadders];
    
    ClassDef(StSstHitCollection,1)
};
#endif

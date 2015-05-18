/*!
 * \class StSstLadderHitCollection 
 * \author Jonathan Bouchet, Thomas Ullrich, May 2015
 */
/***************************************************************************
 *
 * $Id: StSstLadderHitCollection.h,v 2.1 2015/05/13 16:50:59 ullrich Exp $
 *
 * Author: Jonathan Bouchet, Thomas Ullrich, May 2015
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSstLadderHitCollection.h,v $
 * Revision 2.1  2015/05/13 16:50:59  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StSstLadderHitCollection_hh
#define StSstLadderHitCollection_hh

#include "StObject.h"
#include "StSstWaferHitCollection.h"

class StSstLadderHitCollection : public StObject {
public:
    StSstLadderHitCollection();
    ~StSstLadderHitCollection();
    // StSstLadderHitCollection(const StSstLadderHitCollection&); use default
    // const StSstLadderHitCollection&
    // operator=(const StSstLadderHitCollection&);                use default
    
    unsigned int  numberOfHits() const;
    unsigned int  numberOfWafers() const;
    
    StSstWaferHitCollection*       wafer(unsigned int);
    const StSstWaferHitCollection* wafer(unsigned int) const;

private:
    enum { mMaxNumberOfWafers = 16 };
    StSstWaferHitCollection  mWafers[mMaxNumberOfWafers];
    
    ClassDef(StSstLadderHitCollection,1)
};
#endif

/*!
 * \class StSsdHitCollection 
 * \author Lilian Martin, Thomas Ullrich, Dec 1999
 */
/***************************************************************************
 *
 * $Id: StSsdHitCollection.h,v 2.3 2002/02/22 22:56:50 jeromel Exp $
 *
 * Author: Lilian Martin, Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdHitCollection.h,v $
 * Revision 2.3  2002/02/22 22:56:50  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:42  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/01/05 16:00:07  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StSsdHitCollection_hh
#define StSsdHitCollection_hh

#include "StObject.h"
#include "StSsdLadderHitCollection.h"
class StSsdHit;

class StSsdHitCollection : public StObject {
public:
    StSsdHitCollection();
    ~StSsdHitCollection();
    // StSsdHitCollection(const StSsdHitCollection&);            use default
    // StSsdHitCollection& operator=(const StSsdHitCollection&); use default
    
    bool          addHit(StSsdHit*);
    unsigned int  numberOfHits() const;
    unsigned int  numberOfLadders() const;
    
    StSsdLadderHitCollection*       ladder(unsigned int);
    const StSsdLadderHitCollection* ladder(unsigned int) const;

private:
    enum { mNumberOfLadders = 20 };
    StSsdLadderHitCollection mLadders[mNumberOfLadders];
    
    ClassDef(StSsdHitCollection,1)
};
#endif

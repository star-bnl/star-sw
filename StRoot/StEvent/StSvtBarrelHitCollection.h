/*!
 * \class StSvtBarrelHitCollection 
 * \author Thomas Ullrich, Feb 2000
 */
/***************************************************************************
 *
 * $Id: StSvtBarrelHitCollection.h,v 2.3 2002/02/22 22:56:50 jeromel Exp $
 *
 * Author: Thomas Ullrich, Feb 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtBarrelHitCollection.h,v $
 * Revision 2.3  2002/02/22 22:56:50  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:42  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/02/17 18:15:11  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSvtBarrelHitCollection_hh
#define StSvtBarrelHitCollection_hh

#include "StObject.h"
#include "StSvtLadderHitCollection.h"

class StSvtBarrelHitCollection : public StObject {
public:
    StSvtBarrelHitCollection();
    ~StSvtBarrelHitCollection();
    // StSvtBarrelHitCollection(const StSvtBarrelHitCollection&); use default
    // const StSvtBarrelHitCollection&
    // operator=(const StSvtBarrelHitCollection&);                use default
    
    unsigned int numberOfHits() const;
    unsigned int  numberOfLadders() const;
    
    StSvtLadderHitCollection*       ladder(unsigned int);
    const StSvtLadderHitCollection* ladder(unsigned int) const;

    void setBarrelNumber(int);
    
private:
    enum { mMaxNumberOfLadders = 16 };
    Int_t                    mBarrelNumber;
    StSvtLadderHitCollection mLadders[mMaxNumberOfLadders];
    
    ClassDef(StSvtBarrelHitCollection,1)
};
#endif

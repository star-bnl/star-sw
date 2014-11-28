/*!
 * \class StSvtLadderHitCollection 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StSvtLadderHitCollection.h,v 2.4 2002/02/22 22:56:51 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtLadderHitCollection.h,v $
 * Revision 2.4  2002/02/22 22:56:51  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/04/05 04:00:43  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2000/02/17 18:13:19  ullrich
 * Changed the SVT hit storage model. Hits are now stored according
 * to barrel/ladder/wafer not by layer/ladder/wafer.
 *
 * Revision 2.1  1999/10/13 19:43:46  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSvtLadderHitCollection_hh
#define StSvtLadderHitCollection_hh

#include "StObject.h"
#include "StSvtWaferHitCollection.h"

class StSvtLadderHitCollection : public StObject {
public:
    StSvtLadderHitCollection();
    ~StSvtLadderHitCollection();
    // StSvtLadderHitCollection(const StSvtLadderHitCollection&); use default
    // const StSvtLadderHitCollection&
    // operator=(const StSvtLadderHitCollection&);                use default
    
    unsigned int  numberOfHits() const;
    unsigned int  numberOfWafers() const;
    
    StSvtWaferHitCollection*       wafer(unsigned int);
    const StSvtWaferHitCollection* wafer(unsigned int) const;

    void setBarrelNumber(int);
    
private:
    enum { mMaxNumberOfWafers = 7 };
    Int_t                    mBarrelNumber;
    StSvtWaferHitCollection  mWafers[mMaxNumberOfWafers];
    
    ClassDef(StSvtLadderHitCollection,1)
};
#endif

/*!
 * \class StSvtHitCollection 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StSvtHitCollection.h,v 2.4 2002/02/22 22:56:51 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHitCollection.h,v $
 * Revision 2.4  2002/02/22 22:56:51  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/04/05 04:00:43  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2000/02/17 18:13:14  ullrich
 * Changed the SVT hit storage model. Hits are now stored according
 * to barrel/ladder/wafer not by layer/ladder/wafer.
 *
 * Revision 2.1  1999/10/13 19:43:44  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSvtHitCollection_hh
#define StSvtHitCollection_hh

#include "StObject.h"
#include "StSvtBarrelHitCollection.h"
class StSvtHit;

class StSvtHitCollection : public StObject {
public:
    StSvtHitCollection();
    ~StSvtHitCollection();
    // StSvtHitCollection(const StSvtHitCollection&);            use default
    // StSvtHitCollection& operator=(const StSvtHitCollection&); use default
    
    bool          addHit(StSvtHit*);
    unsigned int  numberOfHits() const;
    unsigned int  numberOfBarrels() const;
    
    StSvtBarrelHitCollection*       barrel(unsigned int);
    const StSvtBarrelHitCollection* barrel(unsigned int) const;

private:
    enum { mNumberOfBarrels = 3 };
    StSvtBarrelHitCollection mBarrels[mNumberOfBarrels];
    
    ClassDef(StSvtHitCollection,1)
};
#endif

/*!
 * \class StTpcHitCollection 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StTpcHitCollection.h,v 2.6 2019/04/02 15:32:42 smirnovd Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHitCollection.h,v $
 * Revision 2.6  2019/04/02 15:32:42  smirnovd
 * Add accessors to StTpcHitContainer
 *
 * Revision 2.5  2019/04/02 15:32:33  smirnovd
 * Remove commented code
 *
 * Revision 2.4  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.3  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:44  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  1999/10/13 19:44:00  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcHitCollection_hh
#define StTpcHitCollection_hh

#include "StObject.h"
#include "StTpcSectorHitCollection.h"

class StTpcHit;

class StTpcHitCollection : public StObject {
public:
    StTpcHitCollection();
    ~StTpcHitCollection();
    
    bool          addHit(StTpcHit*);
    unsigned int  numberOfHits() const;
    unsigned int  numberOfSectors() const {return mNumberOfSectors; }
    unsigned int  numberOfPadrows(int sectorId) const { return sector(sectorId) ? sector(sectorId)->numberOfPadrows() : 0; }
    
    StTpcSectorHitCollection*       sector(unsigned int);
    const StTpcSectorHitCollection* sector(unsigned int) const;

    const StSPtrVecTpcHit* hits(int sectorId, int padrowId) const;

private:
    enum { mNumberOfSectors = 24 };
    StTpcSectorHitCollection mSectors[mNumberOfSectors];
    
    ClassDef(StTpcHitCollection,1)
};
#endif

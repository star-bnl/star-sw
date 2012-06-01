/*!
 * \class StTpcSectorHitCollection 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StTpcSectorHitCollection.h,v 2.7 2012/06/01 14:18:38 fisyak Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcSectorHitCollection.h,v $
 * Revision 2.7  2012/06/01 14:18:38  fisyak
 * Increment version to account change in max. no. of pad rows
 *
 * Revision 2.6  2012/05/16 21:35:03  fisyak
 * replace StDigitalPair by its reference
 *
 * Revision 2.5  2012/05/07 14:42:58  fisyak
 * Add handilings for Track to Fast Detectors Matching
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
 * Revision 2.1  1999/10/13 19:44:06  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcSectorHitCollection_hh
#define StTpcSectorHitCollection_hh

#include "StObject.h"
#include "StTpcPadrowHitCollection.h"

class StTpcSectorHitCollection : public StObject {
public:
  StTpcSectorHitCollection() {}
  ~StTpcSectorHitCollection() {}
    // StTpcSectorHitCollection(const StTpcSectorHitCollection&);            use default
    // StTpcSectorHitCollection& operator=(const StTpcSectorHitCollection&); use default
    
    unsigned int numberOfHits() const;
    unsigned int numberOfPadrows() const { return mNumberOfPadrows; }
    
    StTpcPadrowHitCollection*       padrow(unsigned int);
    const StTpcPadrowHitCollection* padrow(unsigned int) const;

private:
    enum { mNumberOfPadrows = 100 }; // Keep in mind that it could be changed
    StTpcPadrowHitCollection mPadrows[mNumberOfPadrows];
    
    ClassDef(StTpcSectorHitCollection,2)
};
#endif

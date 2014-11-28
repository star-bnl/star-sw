/*!
 * \class StFtpcPlaneHitCollection 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StFtpcPlaneHitCollection.h,v 2.3 2002/02/22 22:56:48 jeromel Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcPlaneHitCollection.h,v $
 * Revision 2.3  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:37  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  1999/10/13 19:43:09  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StFtpcPlaneHitCollection_hh
#define StFtpcPlaneHitCollection_hh

#include "StObject.h"
#include "StFtpcSectorHitCollection.h"

class StFtpcPlaneHitCollection : public StObject {
public:
    StFtpcPlaneHitCollection();
    // StFtpcPlaneHitCollection(const StFtpcPlaneHitCollection&);            use default
    // StFtpcPlaneHitCollection& operator=(const StFtpcPlaneHitCollection&); use default
    ~StFtpcPlaneHitCollection();
    
    unsigned int  numberOfHits() const;
    unsigned int  numberOfSectors() const;
    
    StFtpcSectorHitCollection*       sector(unsigned int);
    const StFtpcSectorHitCollection* sector(unsigned int) const;

private:
    enum { mNumberOfSectors = 6 };
    StFtpcSectorHitCollection mSectors[mNumberOfSectors];
    
    ClassDef(StFtpcPlaneHitCollection,1)
};
#endif

/*!
 * \class StFtpcHitCollection 
 * \author Thomas Ullrich, Aug 1999
 */
/***************************************************************************
 *
 * $Id: StFtpcHitCollection.h,v 2.3 2002/02/22 22:56:48 jeromel Exp $
 *
 * Author: Thomas Ullrich, Aug 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHitCollection.h,v $
 * Revision 2.3  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:36  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  1999/10/13 19:43:06  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StFtpcHitCollection_hh
#define StFtpcHitCollection_hh

#include "StObject.h"
#include "StFtpcPlaneHitCollection.h"
class StFtpcHit;

class StFtpcHitCollection : public StObject {
public:
    StFtpcHitCollection();
    // StFtpcHitCollection(const StFtpcHitCollection&);            use default
    // StFtpcHitCollection& operator=(const StFtpcHitCollection&); use default
    ~StFtpcHitCollection();
    
    bool          addHit(StFtpcHit*);
    unsigned int  numberOfHits() const;
    unsigned int  numberOfPlanes() const;
    
    StFtpcPlaneHitCollection*       plane(unsigned int);
    const StFtpcPlaneHitCollection* plane(unsigned int) const;

private:
    enum { mNumberOfPlanes = 20 };
    StFtpcPlaneHitCollection mPlanes[mNumberOfPlanes];
    
    ClassDef(StFtpcHitCollection,1)
};
#endif

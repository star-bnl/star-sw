/*!
 * \class StMeasuredPoint 
 * \author Thomas Ullrich, Sept 1999
 */
/***************************************************************************
 *
 * $Id: StMeasuredPoint.h,v 2.8 2005/07/06 18:58:15 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sept 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMeasuredPoint.h,v $
 * Revision 2.8  2005/07/06 18:58:15  fisyak
 * Add print out
 *
 * Revision 2.7  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.6  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.5  2001/04/05 04:00:38  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2001/03/24 03:34:52  perev
 * clone() -> clone() const
 *
 * Revision 2.3  1999/12/21 15:09:02  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.2  1999/10/28 22:26:05  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:43:26  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StMeasuredPoint_hh
#define StMeasuredPoint_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StThreeVectorF.hh"
#include "StMatrixF.hh"

class StMeasuredPoint : public StObject {
public:
    StMeasuredPoint();
    StMeasuredPoint(const StThreeVectorF&);
    virtual ~StMeasuredPoint();
    // StMeasuredPoint(const StMeasuredPoint&);            use default
    // StMeasuredPoint& operator=(const StMeasuredPoint&); use default
    
    int operator==(const StMeasuredPoint&) const;
    int operator!=(const StMeasuredPoint&) const;
    
    virtual const StThreeVectorF& position() const;
    virtual StThreeVectorF        positionError() const = 0;
    virtual StMatrixF             covariantMatrix() const = 0;
    
    virtual void setPosition(const StThreeVectorF&);
    virtual void                  Print(Option_t *option="") const;
protected:
    StThreeVectorF mPosition;
    ClassDef(StMeasuredPoint,1)
};
ostream&              operator<<(ostream& os, StMeasuredPoint const & v);
#endif

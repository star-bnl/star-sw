/***************************************************************************
 *
 * $Id: StMeasuredPoint.h,v 2.3 1999/12/21 15:09:02 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sept 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMeasuredPoint.h,v $
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
    
    Int_t operator==(const StMeasuredPoint&) const;
    Int_t operator!=(const StMeasuredPoint&) const;
    
    virtual const StThreeVectorF& position() const;
    virtual StThreeVectorF        positionError() const = 0;
    virtual StMatrixF             covariantMatrix() const = 0;
    
    virtual void setPosition(const StThreeVectorF&);
    
protected:
    StThreeVectorF mPosition;

    virtual StObject* clone() = 0;
    ClassDef(StMeasuredPoint,1)
};
#endif

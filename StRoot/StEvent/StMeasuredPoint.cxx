/***************************************************************************
 *
 * $Id: StMeasuredPoint.cxx,v 2.1 1999/10/13 19:44:56 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMeasuredPoint.cxx,v $
 * Revision 2.1  1999/10/13 19:44:56  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StMeasuredPoint.h"

static const char rcsid[] = "$Id: StMeasuredPoint.cxx,v 2.1 1999/10/13 19:44:56 ullrich Exp $";

ClassImp(StMeasuredPoint)

StMeasuredPoint::StMeasuredPoint() {/* noop */}

StMeasuredPoint::StMeasuredPoint(const StThreeVectorF& p)
    : mPosition(p) { /* noop */ }

StMeasuredPoint::~StMeasuredPoint() { /* noop */ }
    
Int_t
StMeasuredPoint::operator==(const StMeasuredPoint& p) const
{
    return p.mPosition == mPosition;
}

Int_t
StMeasuredPoint::operator!=(const StMeasuredPoint& p) const
{
    return !(*this == p);  // use operator==()
}

void
StMeasuredPoint::setPosition(const StThreeVectorF& val) { mPosition = val; }
    
const StThreeVectorF&
StMeasuredPoint::position() const { return mPosition; }

/***************************************************************************
 *
 * $Id: StMeasuredPoint.cxx,v 2.4 2009/11/09 22:38:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMeasuredPoint.cxx,v $
 * Revision 2.4  2009/11/09 22:38:15  ullrich
 * Changed format of print out.
 *
 * Revision 2.3  2005/07/06 18:58:15  fisyak
 * Add print out
 *
 * Revision 2.2  2001/04/05 04:00:51  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  1999/10/13 19:44:56  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StMeasuredPoint.h"
#include "TString.h"

static const char rcsid[] = "$Id: StMeasuredPoint.cxx,v 2.4 2009/11/09 22:38:15 ullrich Exp $";

ClassImp(StMeasuredPoint)

StMeasuredPoint::StMeasuredPoint() {/* noop */}

StMeasuredPoint::StMeasuredPoint(const StThreeVectorF& p)
    : mPosition(p) { /* noop */ }

StMeasuredPoint::~StMeasuredPoint() { /* noop */ }
    
int
StMeasuredPoint::operator==(const StMeasuredPoint& p) const
{
    return p.mPosition == mPosition;
}

int
StMeasuredPoint::operator!=(const StMeasuredPoint& p) const
{
    return !(*this == p);  // use operator==()
}

void
StMeasuredPoint::setPosition(const StThreeVectorF& val) { mPosition = val; }
    
const StThreeVectorF&
StMeasuredPoint::position() const { return mPosition; }

ostream&  operator<<(ostream& os, const StMeasuredPoint& v)
{
  return os << Form("xyz:%10.3f%10.3f%10.3f",
                    v.position().x(),
                    v.position().y(),
                    v.position().z());
}

void StMeasuredPoint::Print(Option_t *option) const {cout << *this << endl;}

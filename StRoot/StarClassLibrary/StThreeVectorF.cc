/***************************************************************************
 *
 * $Id: StThreeVectorF.cc,v 1.1 1999/01/30 03:59:05 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: StThreeVectorF.cc,v $
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.3  1999/10/15 15:46:51  ullrich
 * Changed output format in operator<<
 *
 * Revision 1.2  1999/06/04 18:00:12  ullrich
 * Added new constructor which takes C-style array as argument.
 * New operators operator() and operator[] which can be used
 * as lvalues.
 *
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:24  ullrich
 *
 **************************************************************************/
#include <assert.h>
#include "StThreeVectorF.hh"


StThreeVectorF::StThreeVectorF(const double *a)
{
    mX1 = a[0];
    mX2 = a[1];
    mX3 = a[2];
}

StThreeVectorF& StThreeVectorF::operator=(const StThreeVectorD& v)
{
    mX1 = v.x();  mX2 = v.y();  mX3 = v.z();
    return *this;
}

int StThreeVectorF::operator== (const StThreeVectorD& v) const
{
    return mX1 == v.x() && mX2 == v.y() && mX3 == v.z();
}

int StThreeVectorF::operator!= (const StThreeVectorD& v) const
{
    return !(*this == v);
}

StThreeVectorF& StThreeVectorF::operator+= (const StThreeVectorD& v)
{
    mX1 += v.x(); mX2 += v.y(); mX3 += v.z();
    return *this;
}

StThreeVectorF& StThreeVectorF::operator-= (const StThreeVectorD& v)
{
    mX1 -= v.x(); mX2 -= v.y(); mX3 -= v.z();
    return *this;
}

float StThreeVectorF::dot(const StThreeVectorD& v) const
{
    return mX1*v.x() + mX2*v.y() + mX3*v.z();
}

StThreeVectorF StThreeVectorF::cross(const StThreeVectorD& v) const
{
    return StThreeVectorF(mX2*v.z() - mX3*v.y(),
			    mX3*v.x() - mX1*v.z(),
			    mX1*v.y() - mX2*v.x());
}

float StThreeVectorF::angle(const StThreeVectorD& v) const
{
    return acos(this->dot(v)/this->mag()/v.mag());
}
    return os << '(' << v.x() << ", " << v.y() << ", " << v.z() << ')';
ostream&  operator<<(ostream& os, const StThreeVectorF& v)
{
    return os << v.x() << '\t' << v.y() << '\t' << v.z();
}

istream&  operator>>(istream& is, StThreeVectorF& v)
{
    float  x, y, z;
    is >> x >> y >> z;
    v.setX(x);
      R__b << mX2;
      R__b << mX3;
   }
}



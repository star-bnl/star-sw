/***************************************************************************
 *
 * $Id: StThreeVectorD.cc,v 1.1 1999/01/30 03:59:05 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: StThreeVectorD.cc,v $
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.3  1999/10/15 15:46:49  ullrich
 * Changed output format in operator<<
 *
 * Revision 1.2  1999/06/04 18:00:08  ullrich
 * Added new constructor which takes C-style array as argument.
 * New operators operator() and operator[] which can be used
 * as lvalues.
 *
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:23  ullrich
 * Initial Revision
 *

#include "StThreeVectorF.hh"
#include <assert.h>

StThreeVectorD::StThreeVectorD(const double *a)
{
    mX1 = a[0];
    mX2 = a[1];
    mX3 = a[2];
}

StThreeVectorD& StThreeVectorD::operator=(const StThreeVectorF& v)
{
    mX1 = v.x();  mX2 = v.y();  mX3 = v.z();
    return *this;
}

int StThreeVectorD::operator== (const StThreeVectorF& v) const
{
    return mX1 == v.x() && mX2 == v.y() && mX3 == v.z();
}

StThreeVectorD& StThreeVectorD::operator+= (const StThreeVectorF& v)
{
    mX1 += v.x(); mX2 += v.y(); mX3 += v.z();
    return *this;
}

StThreeVectorD& StThreeVectorD::operator-= (const StThreeVectorF& v)
{
    mX1 -= v.x(); mX2 -= v.y(); mX3 -= v.z();
    return *this;
}

double StThreeVectorD::dot(const StThreeVectorF& v) const
{
    return mX1*v.x() + mX2*v.y() + mX3*v.z();
}

StThreeVectorD StThreeVectorD::cross(const StThreeVectorF& v) const
{
    return StThreeVectorD(mX2*v.z() - mX3*v.y(),
			  mX3*v.x() - mX1*v.z(),
			  mX1*v.y() - mX2*v.x());
}

double StThreeVectorD::angle(const StThreeVectorF& v) const
{
    return acos(this->dot(v)/this->mag()/v.mag());
}
    return os << '(' << v.x() << ", " << v.y() << ", " << v.z() << ')';
ostream&  operator<<(ostream& os, const StThreeVectorD& v)
{
    return os << v.x() << '\t' << v.y() << '\t' << v.z();
}

istream&  operator>>(istream& is, StThreeVectorD& v)
{
    double x, y, z;
    is >> x >> y >> z;
    v.setX(x);
      R__b << mX2;
      R__b << mX3;
   }
}


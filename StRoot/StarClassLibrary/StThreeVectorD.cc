/***************************************************************************
 *
 * $Id: StThreeVectorD.cc,v 1.12 2003/01/22 15:54:24 genevb Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: StThreeVectorD.cc,v $
 * Revision 1.12  2003/01/22 15:54:24  genevb
 * Remove superfluous inline
 *
 * Revision 1.11  2002/06/21 17:47:36  genevb
 * Added pseudoProduct
 *
 * Revision 1.10  2001/05/04 20:01:33  perev
 * Bug, Redaversion added
 *
 * Revision 1.9  2001/04/11 23:03:39  ullrich
 * Removed unused variables in streamer.
 *
 * Revision 1.8  2001/04/09 21:19:33  perev
 * Streamer for ROOT3
 *
 * Revision 1.7  2000/11/27 17:33:51  ullrich
 * Enclosed streamer in macro __ROOT__.
 *
 * Revision 1.6  2000/10/26 21:13:59  perev
 * assert.h include added
 *
 * Revision 1.5  2000/09/30 17:14:27  perev
 * Streame added to ThreeVector
 *
 * Revision 1.4  2000/09/28 02:06:10  perev
 * non automatic streamer added
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
 **************************************************************************/
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include <assert.h>
#ifdef __ROOT__
#include "TBuffer.h"
ClassImp(StThreeVectorD)
#endif

StThreeVectorD::StThreeVectorD(const StThreeVectorF& v)
    : mX1(v.x()), mX2(v.y()), mX3(v.z()) {/* nop */}

StThreeVectorD::StThreeVectorD(const float *a)
{
    mX1 = a[0];
    mX2 = a[1];
    mX3 = a[2];
}

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

StThreeVectorD
StThreeVectorD::pseudoProduct(const StThreeVectorF& v) const
{
    return this->pseudoProduct(v.x(),v.y(),v.z());
}

ostream&  operator<<(ostream& os, const StThreeVectorD& v)
{
    return os << v.x() << '\t' << v.y() << '\t' << v.z();
}

istream&  operator>>(istream& is, StThreeVectorD& v)
{
    double x, y, z;
    is >> x >> y >> z;
    v.setX(x);
    v.setY(y);
    v.setZ(z);
    return is;
}

#ifdef __ROOT__
void StThreeVectorD::Streamer(TBuffer &R__b)
{
//	Stream an object of class StThreeVectorD.
   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion();if (R__v){/*touch*/}; 
      R__b >> mX1;
      R__b >> mX2;
      R__b >> mX3;
   }
   else {
      R__b.WriteVersion(Class());
      R__b << mX1;
      R__b << mX2;
      R__b << mX3;
   }
}
#endif

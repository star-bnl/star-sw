/***************************************************************************
 *
 * $Id: StThreeVectorF.cc,v 1.12 2004/01/27 02:52:26 perev Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: StThreeVectorF.cc,v $
 * Revision 1.12  2004/01/27 02:52:26  perev
 * Add finite() for float
 *
 * Revision 1.11  2003/10/30 20:06:47  perev
 * Check of quality added
 *
 * Revision 1.10  2003/04/30 20:39:42  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.9  2003/01/22 15:54:24  genevb
 * Remove superfluous inline
 *
 * Revision 1.8  2002/06/21 17:47:37  genevb
 * Added pseudoProduct
 *
 * Revision 1.7  2001/04/09 21:19:36  perev
 * Streamer for ROOT3
 *
 * Revision 1.6  2000/11/27 17:33:53  ullrich
 * Enclosed streamer in macro __ROOT__.
 *
 * Revision 1.5  2000/09/30 17:14:28  perev
 * Streame added to ThreeVector
 *
 * Revision 1.4  2000/09/28 02:06:10  perev
 * non automatic streamer added
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
 * Initial Revision
 *
 **************************************************************************/
#include <assert.h>
#include "StMath.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"

#ifdef __ROOT__
#include "TBuffer.h"
ClassImp(StThreeVectorF)
#endif

StThreeVectorF::StThreeVectorF(const StThreeVectorD& v)
    : mX1(v.x()), mX2(v.y()), mX3(v.z()) {/* nop */}

StThreeVectorF::StThreeVectorF(const float *a)
{
    mX1 = a[0];
    mX2 = a[1];
    mX3 = a[2];
}

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

StThreeVectorF
StThreeVectorF::pseudoProduct(const StThreeVectorD& v) const
{
    return this->pseudoProduct(v.x(),v.y(),v.z());
}

ostream&  operator<<(ostream& os, const StThreeVectorF& v)
{
    return os << v.x() << '\t' << v.y() << '\t' << v.z();
}

istream&  operator>>(istream& is, StThreeVectorF& v)
{
    float  x, y, z;
    is >> x >> y >> z;
    v.setX(x);
    v.setY(y);
    v.setZ(z);
    return is;
}

int StThreeVectorF::valid(double world) const
{		
  for (int i=0;i<3;i++) {
    if (!StMath::Finite((&mX1)[i])      ) return 0; 		
    if ( ::fabs  ((&mX1)[i])>world) return 0; 		
  }		
  return 1;		
}

#ifdef __ROOT__
void StThreeVectorF::Streamer(TBuffer &R__b)
{
//	Stream an object of class StThreeVectorF.

   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion();if (R__v){/*touch*/}; 
      R__b >> mX1;
      R__b >> mX2;
      R__b >> mX3;
   } else {
      R__b.WriteVersion(Class());
      R__b << mX1;
      R__b << mX2;
      R__b << mX3;
   }
}
#endif

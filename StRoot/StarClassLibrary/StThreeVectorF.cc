/***************************************************************************
 *
 * $Id: StThreeVectorF.cc,v 1.5 2000/09/30 17:14:28 perev Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: StThreeVectorF.cc,v $
 * Revision 1.5  2000/09/30 17:14:28  perev
 * Streame added to ThreeVector
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
//______________________________________________________________________________
void StThreeVectorF::Streamer(TBuffer &R__b)
{
//	Stream an object of class StThreeVectorF.

   int offset = 0;
   if (R__b.IsReading() && R__b.GetVersion() == 2) {//Special case
      offset = R__b.Length();
      int buffset;
      R__b >> buffset;
      R__b.SetBufferOffset(buffset - 1 + 4);
      Version_t R__v = R__b.ReadVersion();
      assert(R__v==1);
      R__b.SetBufferOffset(R__b.Length()+10);
      R__b >> mX1;
      R__b >> mX2;
      R__b >> mX3;
      R__b.SetBufferOffset(offset+4);
      return;
   }
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



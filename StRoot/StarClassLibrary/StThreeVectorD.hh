/***************************************************************************
 *
 * $Id: StThreeVectorD.hh,v 1.1 1999/01/30 03:59:05 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:  
 *
 * Remarks:   This is a 'handmade' specialisation of StThreeVector<T>
 *            for T=double. This code contains no templates.
 *
 ***************************************************************************
 *
 * $Log: StThreeVectorD.hh,v $
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.2  1999/06/04 18:00:10  ullrich
 * Added new constructor which takes C-style array as argument.
 * New operators operator() and operator[] which can be used
 * as lvalues.
 *
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:28:06  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_THREE_VECTOR_D_HH
#define ST_THREE_VECTOR_D_HH

#include <iostream.h>
#include <math.h>
#ifdef __ROOT__
#include "TObject.h"
#endif

class StThreeVectorF;

class StThreeVectorD 
#ifdef __ROOT__
 : public TObject 
#endif
{
public:    
    StThreeVectorD(double = 0, double = 0, double = 0);
    StThreeVectorD(const StThreeVectorD&);
    
    StThreeVectorD(const float*);
    StThreeVectorD(const double*);
    
    StThreeVectorD& operator=(const StThreeVectorF&);
    StThreeVectorD& operator=(const StThreeVectorD&);
    
    void setX(double);
    void setY(double);
    void setZ(double);

    void setPhi(double);
    void setTheta(double);
    void setMag(double);
    void setMagnitude(double);
    
    double   x()                        const;
    double   y()                        const;
    double   z()                        const;
    double   theta()                    const;
    double   cosTheta()                 const;
    double   phi()                      const;
    double   perp()                     const;
    double   perp2()                    const;
    double   magnitude()                const;
    double   pseudoRapidity()           const;
    double   operator() (size_t)        const;
    double   operator[] (size_t)        const;
    double&  operator() (size_t);
    double&  operator[] (size_t);

    double   massHypothesis(double mass) const;
    
    StThreeVectorD  unit()       const;
    StThreeVectorD  orthogonal() const;

    void  rotateX(double);
    void  rotateY(double);
    void  rotateZ(double);
    
    StThreeVectorD  operator- ();
    StThreeVectorD  operator+ ();
    StThreeVectorD& operator*= (double);
    StThreeVectorD& operator/= (double);
 
    double         angle(const StThreeVectorF&) const;
    StThreeVectorD cross(const StThreeVectorF&) const;
    double         dot  (const StThreeVectorF&) const;
    
    double         angle(const StThreeVectorD&) const;
    double         dot  (const StThreeVectorD&) const;
    StThreeVectorD cross(const StThreeVectorD&) const;
      
    int operator == (const StThreeVectorF& v) const;
    int operator != (const StThreeVectorF& v) const;
    StThreeVectorD& operator+= (const StThreeVectorF&);
    StThreeVectorD& operator-= (const StThreeVectorF&);
    
    int operator == (const StThreeVectorD& v) const;
    int operator != (const StThreeVectorD& v) const;
    StThreeVectorD& operator+= (const StThreeVectorD&);
    StThreeVectorD& operator-= (const StThreeVectorD&);

protected:
    double    mX1, mX2, mX3;
#ifdef __ROOT__
    ClassDef(StThreeVectorD,1)
#endif
};

//
//        Declaration of extern non-member functions and operators
//
ostream&              operator<<(ostream& os, const StThreeVectorD& v);
istream&              operator>>(istream& is, StThreeVectorD& v);
inline double         abs(const StThreeVectorD& v);
inline StThreeVectorD cross_product(const StThreeVectorD& v1, const StThreeVectorD& v2);
inline StThreeVectorD cross_product(const StThreeVectorD& v1, const StThreeVectorF& v2);
inline StThreeVectorD operator+ (const StThreeVectorD& v1, const StThreeVectorD& v2);
inline StThreeVectorD operator+ (const StThreeVectorD& v1, const StThreeVectorF& v2);
inline StThreeVectorD operator- (const StThreeVectorD& v1, const StThreeVectorF& v2);
inline StThreeVectorD operator- (const StThreeVectorD& v1, const StThreeVectorD& v2);
inline double         operator* (const StThreeVectorD& v1, const StThreeVectorF& v2);
inline double         operator* (const StThreeVectorD& v1, const StThreeVectorD& v2);
inline StThreeVectorD operator* (const StThreeVectorD& v, double c);
inline StThreeVectorD operator* (double c, const StThreeVectorD& v);
inline StThreeVectorD operator/ (const StThreeVectorD& v, double c);                        

//
//        Implementation of member functions
//
inline StThreeVectorD::StThreeVectorD(double x, double y, double z)
    : mX1(x), mX2(y), mX3(z) {/* nop */}

inline StThreeVectorD::~StThreeVectorD() {/* nop */}

inline void StThreeVectorD::setX(double x) {mX1 = x;}

inline void StThreeVectorD::setY(double y) {mX2 = y;}

inline void StThreeVectorD::setZ(double z) {mX3 = z;}

inline void StThreeVectorD::setPhi(double angle)
{
    double  r = magnitude();
    double th = theta();
    
    mX1 = r*sin(th)*cos(angle);
    mX2 = r*sin(th)*sin(angle);
}

inline void StThreeVectorD::setTheta(double angle)
{
    double r  = magnitude();
    double ph = phi();

    mX1 = r*sin(angle)*cos(ph);
    mX2 = r*sin(angle)*sin(ph);
    mX3 = r*cos(angle);
}

inline void StThreeVectorD::setMagnitude(double r)
{
    double th = theta();
    double ph = phi();
    
    mX1 = r*sin(th)*cos(ph);
    mX2 = r*sin(th)*sin(ph);
    mX3 = r*cos(th);
}

inline void StThreeVectorD::setMag(double mag)
{
    setMagnitude(mag);
}

inline double StThreeVectorD::x() const {return mX1;}

inline double StThreeVectorD::y() const {return mX2;}

inline double StThreeVectorD::z() const {return mX3;}

inline double StThreeVectorD::theta() const
{
    return acos(mX3/this->mag());
}

inline double StThreeVectorD::cosTheta() const
{
    return mX3/this->mag();
}

inline double StThreeVectorD::phi() const
{
    return atan2(mX2, mX1);
}

inline double StThreeVectorD::pseudoRapidity() const
{
    return -log(tan(theta()/2.));
}

inline StThreeVectorD StThreeVectorD::unit() const
{
    return *this/this->mag();
}

inline double StThreeVectorD::massHypothesis(double mass) const
{
    return sqrt((*this)*(*this) + mass*mass);
}

inline StThreeVectorD StThreeVectorD::orthogonal() const
{
    // Direct copy from CLHEP--it is probably better to
    // use your own dot/cross product code...
    double x = (mX1 < 0.0) ? -mX1 : mX1;
    double y = (mX2 < 0.0) ? -mX2 : mX2;
    double z = (mX3 < 0.0) ? -mX3 : mX3;
    
    if(x<y)
	return x < z ? StThreeVectorD(0,mX3,-mX2) :  StThreeVectorD(mX2,-mX1,0);
    else
	return  mX2 < mX3 ? StThreeVectorD(-mX3,0,mX1) :  StThreeVectorD(mX2,-mX1,0);
}

inline void StThreeVectorD::rotateX(double angle)
{
    // may in the future make use of the StRotation class!
    double yPrime = cos(angle)*mX2 - sin(angle)*mX3;
    double zPrime = sin(angle)*mX2 + cos(angle)*mX3;

    mX2 = yPrime;
    mX3 = zPrime;
}

inline void StThreeVectorD::rotateY(double angle)
{
    // may in the future make use of the StRotation class!
    double zPrime = cos(angle)*mX3 - sin(angle)*mX1;
    double xPrime = sin(angle)*mX3 + cos(angle)*mX1;

    mX1 = xPrime;
    mX3 = zPrime;
}

inline void StThreeVectorD::rotateZ(double angle)
{
    // may in the future make use of the StRotation class!
    double xPrime = cos(angle)*mX1 - sin(angle)*mX2;
    double yPrime = sin(angle)*mX1 + cos(angle)*mX2;

    mX1 = xPrime;
    mX2 = yPrime;
}

inline double StThreeVectorD::perp() const
{
    return sqrt(mX1*mX1+mX2*mX2);
}

inline double StThreeVectorD::perp2() const
{
    return mX1*mX1+mX2*mX2;
}

inline double StThreeVectorD::magnitude() const
{
    return mag();
}

inline double StThreeVectorD::mag() const
{
    return sqrt(mX1*mX1+mX2*mX2+mX3*mX3);
}

inline double StThreeVectorD::mag2() const
{
    return mX1*mX1+mX2*mX2+mX3*mX3;
}

inline double StThreeVectorD::operator() (size_t i) const
{
    if (i == 0)
        return mX1;
    else if (i == 1)
        return mX2;
    else if (i == 2)
        return mX3;
    else {
      cerr << "StThreeVectorD::operator(): bad index" << endl;
      return mX3;
    }
}

inline double StThreeVectorD::operator[] (size_t i) const
{
    if (i == 0)
        return mX1;
    else if (i == 1)
        return mX2;
    else if (i == 2)
        return mX3;
    else {
      cerr << "StThreeVectorD::operator[]: bad index" << endl;
      return mX3;
    }
}

inline StThreeVectorD& StThreeVectorD::operator*= (double c)
{
    mX1 *= c; mX2 *= c; mX3 *= c;
    return *this;
}

inline StThreeVectorD& StThreeVectorD::operator/= (double c)
{
    mX1 /= c; mX2 /= c; mX3 /= c;
    return *this;
}

inline StThreeVectorD StThreeVectorD::operator- ()
{
    return StThreeVectorD(-mX1, -mX2, -mX3);
}

inline StThreeVectorD StThreeVectorD::operator+ ()
{
    return *this;
}

inline StThreeVectorD::StThreeVectorD(const StThreeVectorD& v)
    : mX1(v.x()), mX2(v.y()), mX3(v.z()) {/* nop */}

inline StThreeVectorD&
StThreeVectorD::operator=(const StThreeVectorD& v)
{
    mX1 = v.x();  mX2 = v.y();  mX3 = v.z();
    return *this;
}

inline int
StThreeVectorD::operator== (const StThreeVectorD& v) const
{
    return mX1 == v.x() && mX2 == v.y() && mX3 == v.z();
}

inline int
StThreeVectorD::operator!= (const StThreeVectorF& v) const
{
    return !(*this == v);
}

inline int 
StThreeVectorD::operator!= (const StThreeVectorD& v) const
{
    return !(*this == v);
}

inline StThreeVectorD&
StThreeVectorD::operator+= (const StThreeVectorD& v)
{
    mX1 += v.x(); mX2 += v.y(); mX3 += v.z();
    return *this;
}

inline StThreeVectorD&
StThreeVectorD::operator-= (const StThreeVectorD& v)
{
    mX1 -= v.x(); mX2 -= v.y(); mX3 -= v.z();
    return *this;
}

inline double StThreeVectorD::dot(const StThreeVectorD& v) const
{
    return mX1*v.x() + mX2*v.y() + mX3*v.z();
}

inline StThreeVectorD
StThreeVectorD::cross(const StThreeVectorD& v) const
{
    return StThreeVectorD(mX2*v.z() - mX3*v.y(),
			  mX3*v.x() - mX1*v.z(),
			  mX1*v.y() - mX2*v.x());
}

inline double StThreeVectorD::angle(const StThreeVectorD& v) const
{
    return acos(this->dot(v)/this->mag()/v.mag());
}

//
//        Non-member functions
//
inline double abs(const StThreeVectorD& v) {return v.mag();}

inline StThreeVectorD
cross_product(const StThreeVectorD& v1, const StThreeVectorD& v2)
{
    return v1.cross(v2);
}

inline StThreeVectorD
cross_product(const StThreeVectorD& v1, const StThreeVectorF& v2)
{
    return v1.cross(v2);
}


//
//        Non-member operators
//
inline StThreeVectorD
operator+ (const StThreeVectorD& v1, const StThreeVectorD& v2)
{
    return StThreeVectorD(v1) += v2;
}

inline StThreeVectorD
operator+ (const StThreeVectorD& v1, const StThreeVectorF& v2)
{
    return StThreeVectorD(v1) += v2;
}

inline StThreeVectorD
operator- (const StThreeVectorD& v1, const StThreeVectorF& v2)
{
    return StThreeVectorD(v1) -= v2;
}

inline StThreeVectorD
operator- (const StThreeVectorD& v1, const StThreeVectorD& v2)
{
    return StThreeVectorD(v1) -= v2;
}

inline double operator* (const StThreeVectorD& v1, const StThreeVectorF& v2)
{
    return StThreeVectorD(v1).dot(v2);
}

inline double operator* (const StThreeVectorD& v1, const StThreeVectorD& v2)
{
    return StThreeVectorD(v1).dot(v2);
}

inline StThreeVectorD operator* (const StThreeVectorD& v, double c)
{
    return StThreeVectorD(v) *= c;
}

inline StThreeVectorD operator* (double c, const StThreeVectorD& v)
{
    return StThreeVectorD(v) *= c;
}

inline StThreeVectorD operator/ (const StThreeVectorD& v, double c)
{
    return StThreeVectorD(v) /= c;
}

#endif


/***************************************************************************
 *
 * $Id: StThreeVector.hh,v 1.1 1999/01/30 03:59:05 fisyak Exp $
 *
 * Author: Brian Lasiuk, Thomas Ullrich, April 1998
 ***************************************************************************
 *
 * Description:  
 *
 * Remarks:   Since not all compilers support member templates
 *            we have to specialize the templated member on these
 *            platforms. If member templates are not supported the
 *            ST_NO_MEMBER_TEMPLATES flag has to be set. tu.
 *
 ***************************************************************************
 *
 * $Log: StThreeVector.hh,v $
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.4  1999/06/04 18:00:05  ullrich
 * Added new constructor which takes C-style array as argument.
 * New operators operator() and operator[] which can be used
 * as lvalues.
 *
 * Revision 1.3  1999/02/17 11:42:19  ullrich

 *
 * Revision 1.2  1999/02/14 23:11:48  fisyak
 * Fixes for Rootcint
 *
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:28:04  ullrich
 * Initial Revision
 *
 **************************************************************************/

#define ST_THREE_VECTOR_HH
#ifndef __CINT__
#include <iostream.h>
#include <math.h>
#ifdef GNU_GCC
#   include <stddef.h>
#endif
#ifndef ST_NO_EXCEPTIONS
#   include <stdexcept>
#endif
#ifdef __SUNPRO_CC
#   include <stdcomp.h>
#endif
#endif
#ifdef ST_NO_TEMPLATE_DEF_ARGS
template<class T>
#else
template<class T = double>
class StThreeVector {
public:    
    StThreeVector(T = 0, T = 0, T = 0);
    StThreeVector(const StThreeVector<long double>&);
    ~StThreeVector();

#ifndef ST_NO_MEMBER_TEMPLATES
    StThreeVector<T>& operator=(const StThreeVector<long double>&);
    template<class X> StThreeVector(const StThreeVector<X>&);
    // StThreeVector<T>& operator=(const StThreeVector<T>&);  use default
#else    
    StThreeVector(const StThreeVector<float>&);
    StThreeVector(const StThreeVector<double>&);
    
    StThreeVector(const float*); 
    StThreeVector(const double*);
    
    StThreeVector<T>& operator=(const StThreeVector<float>&);
    StThreeVector<T>& operator=(const StThreeVector<double>&);
#endif
    
    void setX(T);
    void setY(T);
    void setZ(T);

    void setPhi(T);
    void setTheta(T);
    void setMag(T);
    void setMagnitude(T);
    
    T   x()                        const;
    T   y()                        const;
    T   z()                        const;
    T   theta()                    const;
    T   cosTheta()                 const;
    T   phi()                      const;
    T   perp()                     const;
    T   mag2()                     const;
    T   pseudoRapidity()           const;
    T   operator() (size_t)        const;
    T   operator[] (size_t)        const;

    T&  operator() (size_t);
    T&  operator[] (size_t);
    
    T   massHypothesis(T mass)     const;
    
    StThreeVector<T>  unit()       const;
    StThreeVector<T>  orthogonal() const;

    void  rotateX(T);
    void  rotateY(T);
    void  rotateZ(T);
    
    StThreeVector<T>  operator- ();
    StThreeVector<T>  operator+ ();
    StThreeVector<T>& operator*= (double);
    StThreeVector<T>& operator/= (double);
 
#ifndef ST_NO_MEMBER_TEMPLATES
    template<class X> T                angle(const StThreeVector<X>&) const;
    template<class X> StThreeVector<T> cross(const StThreeVector<X>&) const;
    template<class X> T                dot  (const StThreeVector<X>&) const;
    
    template<class X> bool operator == (const StThreeVector<X>& v) const;
    template<class X> bool operator != (const StThreeVector<X>& v) const;
    
    T                angle(const StThreeVector<long double>&) const;
    StThreeVector<T> cross(const StThreeVector<long double>&) const;
    T                dot  (const StThreeVector<long double>&) const;
   
    template<class X> StThreeVector<T>& operator+= (const StThreeVector<X>&);
    template<class X> StThreeVector<T>& operator-= (const StThreeVector<X>&);
#else    
    T                angle(const StThreeVector<float>&) const;
    StThreeVector<T> cross(const StThreeVector<float>&) const;
    T                dot  (const StThreeVector<float>&) const;
    
    T                angle(const StThreeVector<double>&) const;
    T                dot  (const StThreeVector<double>&) const;

    bool operator == (const StThreeVector<long double>& v) const;
    bool operator != (const StThreeVector<long double>& v) const;
    StThreeVector<T>& operator+= (const StThreeVector<long double>&);
    StThreeVector<T>& operator-= (const StThreeVector<long double>&);
    StThreeVector<T> cross(const StThreeVector<double>&) const;
       
    bool operator == (const StThreeVector<float>& v) const;

    StThreeVector<T>& operator+= (const StThreeVector<float>&);
    StThreeVector<T>& operator-= (const StThreeVector<float>&);
    
    bool operator == (const StThreeVector<double>& v) const;
    bool operator != (const StThreeVector<double>& v) const;
    StThreeVector<T>& operator+= (const StThreeVector<double>&);
    StThreeVector<T>& operator-= (const StThreeVector<double>&);
#endif

protected:
    T    mX1, mX2, mX3;
};

#ifndef __CINT__
//
//        Implementation of member functions
//
template<class T>
inline StThreeVector<T>::StThreeVector(T x, T y, T z)
    : mX1(x), mX2(y), mX3(z) {/* nop */}

template<class T>
inline StThreeVector<T>::~StThreeVector() {/* nop */}

template<class T>
inline void StThreeVector<T>::setX(T x) {mX1 = x;}

template<class T>
inline void StThreeVector<T>::setY(T y) {mX2 = y;}

template<class T>
inline void StThreeVector<T>::setZ(T z) {mX3 = z;}

template<class T>
void StThreeVector<T>::setPhi(T angle)
{
    double  r = magnitude();
    double th = theta();
    
    mX1 = r*sin(th)*cos(angle);
    mX2 = r*sin(th)*sin(angle);
}

template <class T>
void StThreeVector<T>::setTheta(T angle)
{
    double r  = magnitude();
    double ph = phi();

    mX1 = r*sin(angle)*cos(ph);
    mX2 = r*sin(angle)*sin(ph);
    mX3 = r*cos(angle);
}

template <class T>
void StThreeVector<T>::setMagnitude(T r)
{
    double th = theta();
    double ph = phi();
    
    mX1 = r*sin(th)*cos(ph);
    mX2 = r*sin(th)*sin(ph);
    mX3 = r*cos(th);
}

template <class T>
void StThreeVector<T>::setMag(T mag)
{
    setMagnitude(mag);
}

template<class T>
inline T StThreeVector<T>::x() const {return mX1;}

template<class T>
inline T StThreeVector<T>::y() const {return mX2;}

template<class T>
inline T StThreeVector<T>::z() const {return mX3;}

template<class T>
inline T StThreeVector<T>::theta() const
{
    return acos(mX3/this->mag());
}

template<class T>
inline T StThreeVector<T>::cosTheta() const
{
    return mX3/this->mag();
}

template<class T>
inline T StThreeVector<T>::phi() const
{
    return atan2(mX2,mX1);
}

template<class T>
inline T StThreeVector<T>::pseudoRapidity() const
{
    //
    // change code to more optimal:
    // double m = mag();
    // return 0.5*log( (m+z())/(m-z()) );
    return -log(tan(theta()/2.));
}

template<class T>
inline StThreeVector<T> StThreeVector<T>::unit() const
{
    return *this/this->mag();
}

template <class T>
T StThreeVector<T>::massHypothesis(T mass) const
{
    return sqrt((*this)*(*this) + mass*mass);
}

template <class T>
StThreeVector<T> StThreeVector<T>::orthogonal() const
{
    // Direct copy from CLHEP--it is probably better to
    // use your own dot/cross product code...
    double x = (mX1 < 0.0) ? -mX1 : mX1;
    double y = (mX2 < 0.0) ? -mX2 : mX2;
    double z = (mX3 < 0.0) ? -mX3 : mX3;
    
    if(x<y)
	return x < z ? StThreeVector<T>(0,mX3,-mX2) :  StThreeVector<T>(mX2,-mX1,0);
    else
	return  mX2 < mX3 ? StThreeVector<T>(-mX3,0,mX1) :  StThreeVector<T>(mX2,-mX1,0);
}

template <class T>
void StThreeVector<T>::rotateX(T angle)
{
    // may in the future make use of the StRotation class!
    double yPrime = cos(angle)*mX2 - sin(angle)*mX3;
    double zPrime = sin(angle)*mX2 + cos(angle)*mX3;

    mX2 = yPrime;
    mX3 = zPrime;
}

template <class T>
void StThreeVector<T>::rotateY(T angle)
{
    // may in the future make use of the StRotation class!
    double zPrime = cos(angle)*mX3 - sin(angle)*mX1;
    double xPrime = sin(angle)*mX3 + cos(angle)*mX1;

    mX1 = xPrime;
    mX3 = zPrime;
}

template <class T>
void StThreeVector<T>::rotateZ(T angle)
{
    // may in the future make use of the StRotation class!
    double xPrime = cos(angle)*mX1 - sin(angle)*mX2;
    double yPrime = sin(angle)*mX1 + cos(angle)*mX2;

    mX1 = xPrime;
    mX2 = yPrime;
}

template<class T>
inline T StThreeVector<T>::perp() const
{
    return sqrt(mX1*mX1+mX2*mX2);
}

template<class T>
inline T StThreeVector<T>::perp2() const
{
    return mX1*mX1+mX2*mX2;
}

template<class T>
inline T StThreeVector<T>::magnitude() const
{
    return mag();
}

template<class T>
inline T StThreeVector<T>::mag() const
{
    return sqrt(mX1*mX1+mX2*mX2+mX3*mX3);
}

template<class T>
inline T StThreeVector<T>::mag2() const
{
    return mX1*mX1+mX2*mX2+mX3*mX3;
}

template<class T>
inline T StThreeVector<T>::operator() (size_t i) const
{
    if (i == 0)
        return mX1;
    else if (i == 1)
        return mX2;
    else if (i == 2)
        return mX3;
    else {
#ifndef ST_NO_EXCEPTIONS
      throw out_of_range("StThreeVector<T>::operator(): bad index");
#else
      cerr << "StThreeVector<T>::operator(): bad index" << endl;
#endif
      return mX3;   // have to return something here ...
    }
}

template<class T>
inline T StThreeVector<T>::operator[] (size_t i) const
{
    if (i == 0)
        return mX1;
    else if (i == 1)
        return mX2;
    else if (i == 2)
        return mX3;
    else {
#ifndef ST_NO_EXCEPTIONS
      throw out_of_range("StThreeVector<T>::operator[]: bad index"); 
#else
      cerr << "StThreeVector<T>::operator[]: bad index" << endl;
#endif
      return mX3;   // have to return something here ...
    }
}

template<class T>
inline StThreeVector<T>& StThreeVector<T>::operator*= (double c)
{
    mX1 *= c; mX2 *= c; mX3 *= c;
    return *this;
}

template<class T>
inline StThreeVector<T>& StThreeVector<T>::operator/= (double c)
{
    mX1 /= c; mX2 /= c; mX3 /= c;
    return *this;
}

template<class T>
StThreeVector<T> StThreeVector<T>::operator- ()
{
    return StThreeVector<T>(-mX1, -mX2, -mX3);
}

template<class T>
StThreeVector<T> StThreeVector<T>::operator+ ()
{
    return *this;
}

#ifndef ST_NO_MEMBER_TEMPLATES
{
    mX1 = a[0];
    mX2 = a[1];
    mX3 = a[2];
}

template<class T>
template<class X>
inline StThreeVector<T>&
StThreeVector<T>::operator=(const StThreeVector<X>& v)
{
    mX1 = v.x();  mX2 = v.y();  mX3 = v.z();
    return *this;
}

template<class T>
template<class X>
inline bool StThreeVector<T>::operator== (const StThreeVector<X>& v) const
{
    return mX1 == v.x() && mX2 == v.y() && mX3 == v.z();
}

template<class T>
template<class X>
inline bool StThreeVector<T>::operator!= (const StThreeVector<X>& v) const
{
    return !(*this == v);
}

template<class T>
template<class X>
inline StThreeVector<T>&
StThreeVector<T>::operator+= (const StThreeVector<X>& v)
{
    mX1 += v.x(); mX2 += v.y(); mX3 += v.z();
    return *this;
}

template<class T>
template<class X>
inline StThreeVector<T>&
StThreeVector<T>::operator-= (const StThreeVector<X>& v)
{
    mX1 -= v.x(); mX2 -= v.y(); mX3 -= v.z();
    return *this;
}

template<class T>
template<class X>
inline T StThreeVector<T>::dot(const StThreeVector<X>& v) const
{
    return mX1*v.x() + mX2*v.y() + mX3*v.z();
}

template<class T>
template<class X>
inline StThreeVector<T>
StThreeVector<T>::cross(const StThreeVector<X>& v) const
{
    return StThreeVector<T>(mX2*v.z() - mX3*v.y(),
			    mX3*v.x() - mX1*v.z(),
			    mX1*v.y() - mX2*v.x());
}

template<class T>
template<class X>
inline T StThreeVector<T>::angle(const StThreeVector<X>& vec) const
{
    double norm = this->mag2()*vec.mag2();
    
    return norm > 0 ? acos(this->dot(vec)/(sqrt(norm))) : 0;
}

inline StThreeVector<T>::StThreeVector(const StThreeVector<long double>& v)
    : mX1(v.x()), mX2(v.y()), mX3(v.z()) {/* nop */}

template<class T>
#else

template<class T>
inline StThreeVector<T>::StThreeVector(const double *a)
{
    mX1 = a[0];
    mX2 = a[1];
    mX3 = a[2];
}

template<class T>
inline StThreeVector<T>&
StThreeVector<T>::operator=(const StThreeVector<float>& v)
{
    mX1 = v.x();  mX2 = v.y();  mX3 = v.z();
    return *this;
inline StThreeVector<T>&
StThreeVector<T>::operator=(const StThreeVector<long double>& v)
{
    mX1 = v.x();  mX2 = v.y();  mX3 = v.z();
    return *this;
}

template<class T>
}

template<class T>
inline StThreeVector<T>&
StThreeVector<T>::operator=(const StThreeVector<double>& v)
{
    mX1 = v.x();  mX2 = v.y();  mX3 = v.z();
    return *this;
}

template<class T>
inline bool
StThreeVector<T>::operator== (const StThreeVector<float>& v) const
{
    return mX1 == v.x() && mX2 == v.y() && mX3 == v.z();
StThreeVector<T>::operator== (const StThreeVector<long double>& v) const
{
    return mX1 == v.x() && mX2 == v.y() && mX3 == v.z();
}

template<class T>
inline bool
}

template<class T>
inline bool
StThreeVector<T>::operator== (const StThreeVector<double>& v) const
{
    return mX1 == v.x() && mX2 == v.y() && mX3 == v.z();
}

template<class T>
inline bool
StThreeVector<T>::operator!= (const StThreeVector<float>& v) const
{
inline bool
StThreeVector<T>::operator!= (const StThreeVector<long double>& v) const
{
    return !(*this == v);
}

template<class T>
    return !(*this == v);
}

template<class T>
inline bool
StThreeVector<T>::operator!= (const StThreeVector<double>& v) const
{
    return !(*this == v);
}

template<class T>
inline StThreeVector<T>&
StThreeVector<T>::operator+= (const StThreeVector<float>& v)
{
    mX1 += v.x(); mX2 += v.y(); mX3 += v.z();
    return *this;
}
StThreeVector<T>::operator+= (const StThreeVector<long double>& v)
{
    mX1 += v.x(); mX2 += v.y(); mX3 += v.z();
    return *this;
}

template<class T>
inline StThreeVector<T>&

template<class T>
inline StThreeVector<T>&
StThreeVector<T>::operator+= (const StThreeVector<double>& v)
{
    mX1 += v.x(); mX2 += v.y(); mX3 += v.z();
    return *this;
}

template<class T>
inline StThreeVector<T>&
StThreeVector<T>::operator-= (const StThreeVector<float>& v)
{
    mX1 -= v.x(); mX2 -= v.y(); mX3 -= v.z();
    return *this;
inline StThreeVector<T>&
StThreeVector<T>::operator-= (const StThreeVector<long double>& v)
{
    mX1 -= v.x(); mX2 -= v.y(); mX3 -= v.z();
    return *this;
}

template<class T>
}

template<class T>
inline StThreeVector<T>&
StThreeVector<T>::operator-= (const StThreeVector<double>& v)
{
    mX1 -= v.x(); mX2 -= v.y(); mX3 -= v.z();
    return *this;
}

template<class T>
inline T StThreeVector<T>::dot(const StThreeVector<float>& v) const
inline T StThreeVector<T>::dot(const StThreeVector<long double>& v) const
{
    return mX1*v.x() + mX2*v.y() + mX3*v.z();
}

template<class T>
{
    return mX1*v.x() + mX2*v.y() + mX3*v.z();
}

template<class T>
inline T StThreeVector<T>::dot(const StThreeVector<double>& v) const
{
    return mX1*v.x() + mX2*v.y() + mX3*v.z();
}

template<class T>
inline StThreeVector<T>
StThreeVector<T>::cross(const StThreeVector<float>& v) const
{
    return StThreeVector<T>(mX2*v.z() - mX3*v.y(),
			    mX3*v.x() - mX1*v.z(),
			    mX1*v.y() - mX2*v.x());
}
inline StThreeVector<T>
StThreeVector<T>::cross(const StThreeVector<long double>& v) const
{
    return StThreeVector<T>(mX2*v.z() - mX3*v.y(),
			    mX3*v.x() - mX1*v.z(),
			    mX1*v.y() - mX2*v.x());
}

template<class T>

template<class T>
inline StThreeVector<T>
StThreeVector<T>::cross(const StThreeVector<double>& v) const
{
    return StThreeVector<T>(mX2*v.z() - mX3*v.y(),
			    mX3*v.x() - mX1*v.z(),
			    mX1*v.y() - mX2*v.x());
}


template<class T>
inline T StThreeVector<T>::angle(const StThreeVector<long double>& v) const
{
    return acos(this->dot(v)/this->mag()/v.mag());
}

template<class T>
inline T StThreeVector<T>::angle(const StThreeVector<float>& v) const
{
    return acos(this->dot(v)/this->mag()/v.mag());
}

template<class T>
inline T StThreeVector<T>::angle(const StThreeVector<double>& v) const
{
    return acos(this->dot(v)/this->mag()/v.mag());
}
#endif  // ST_NO_MEMBER_TEMPLATES

//
//        Non-member functions
//
template<class T>
inline T abs(const StThreeVector<T>& v) {return v.mag();}

template<class T, class X>
inline StThreeVector<T>
cross_product(const StThreeVector<T>& v1, const StThreeVector<X>& v2)
{
    return v1.cross(v2);
}


//
//        Non-member operators
//
template<class T, class X>
inline StThreeVector<T>
operator+ (const StThreeVector<T>& v1, const StThreeVector<X>& v2)
{
    return StThreeVector<T>(v1) += v2;
}

template<class T, class X>
inline StThreeVector<T>
operator- (const StThreeVector<T>& v1, const StThreeVector<X>& v2)
{
    return StThreeVector<T>(v1) -= v2;
}

template<class T, class X>
inline T operator* (const StThreeVector<T>& v1, const StThreeVector<X>& v2)
{
    return StThreeVector<T>(v1).dot(v2);
}

template<class T>
inline StThreeVector<T> operator* (const StThreeVector<T>& v, double c)
{
    return StThreeVector<T>(v) *= c;
}

template<class T>
inline StThreeVector<T> operator* (double c, const StThreeVector<T>& v)
{
    return StThreeVector<T>(v) *= c;
#ifdef __HP_aCC
#ifndef FOR_HELIX
ostream&  operator<<(ostream& os, const StThreeVector<long double>& v)
{
    return os << '(' << (static_cast<double>(v.x())) << ", " << (static_cast<double>(v.y())) << ", " << (static_cast<double>(v.z())) << ')';
}
#endif
#endif
// }
// #endif
// #endif
template<class T, class X>
inline StThreeVector<T> operator/ (const StThreeVector<T>& v, X c)
{
    return StThreeVector<T>(v) /= c;
}

    return os << '(' << v.x() << ", " << v.y() << ", " << v.z() << ')';
ostream&  operator<<(ostream& os, const StThreeVector<T>& v)
{

}

template<class T>
istream&  operator>>(istream& is, StThreeVector<T>& v)
{
    T  x, y, z;
    is >> x >> y >> z;
    v.setX(x);
    v.setY(y);
    v.setZ(z);
    return is;
}

#endif
#endif

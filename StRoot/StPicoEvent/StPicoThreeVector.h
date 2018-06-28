#ifndef ST_PICO_THREE_VECTOR_H
#define ST_PICO_THREE_VECTOR_H

/// ROOT headers
#include "Rtypes.h"
#include "Riostream.h"
#include "RVersion.h"

/// C++ headers
#include <cmath>

#if !defined(__CINT__)

#if defined(GNU_GCC)
#    include <stddef.h>
#endif

#if defined (__SUNPRO_CC) && __SUNPRO_CC < 0x500
#    include <stdcomp.h>
#endif

#if !defined(ST_NO_EXCEPTIONS)
#    include <stdexcept>
#    if !defined(ST_NO_NAMESPACES)
using std::out_of_range;
#    endif
#endif

#endif // __CINT__

//_________________
template<class T> class StPicoThreeVector {
  
 public:    
    StPicoThreeVector();
    StPicoThreeVector(T, T, T);
  //                     ROOT_VERSION(5,03,01)
#if ROOT_VERSION_CODE >= 328449
   StPicoThreeVector(TRootIOCtor*) : mX1(0), mX2(0), mX3(0) {}
#endif
    virtual ~StPicoThreeVector();

#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)
    template<class X> StPicoThreeVector(const StPicoThreeVector<X>&);
    template<class X> StPicoThreeVector(const X*);  
    template<class X> StPicoThreeVector<T>& operator=(const StPicoThreeVector<X>&);
    // StPicoThreeVector(const StPicoThreeVector<T>&);                use default
    // StPicoThreeVector<T>& operator=(const StPicoThreeVector<T>&);  use default
#else    
    StPicoThreeVector(const StPicoThreeVector<float>&);
    StPicoThreeVector(const StPicoThreeVector<double>&);
    
    StPicoThreeVector(const float*); 
    StPicoThreeVector(const double*);
    
    StPicoThreeVector<T>& operator=(const StPicoThreeVector<float>&);
    StPicoThreeVector<T>& operator=(const StPicoThreeVector<double>&);
#endif
    
    void setX(T);
    void setY(T);
    void setZ(T);
    void set(T X,T Y, T Z) {mX1=X;mX2=Y;mX3=Z;}

    void setPhi(T);
    void setTheta(T);
    void setMag(T);
    void setMagnitude(T);
    
    const T& x()                   const;
    const T& y()                   const;
    const T& z()                   const;
    const T* xyz()                 const;
          T* xyz();
    T   theta()                    const;
    T   cosTheta()                 const;
    T   phi()                      const;
    T   perp()                     const;
    T   perp2()                    const;
    T   magnitude()                const;
    T   mag()                      const;
    T   mag2()                     const;
    T   pseudoRapidity()           const;
    T   operator() (size_t)        const;
    T   operator[] (size_t)        const;

    T&  operator() (size_t);
    T&  operator[] (size_t);
    
    T   massHypothesis(T mass)     const;
    
    StPicoThreeVector<T>  unit()       const;
    StPicoThreeVector<T>  orthogonal() const;

    void  rotateX(T);
    void  rotateY(T);
    void  rotateZ(T);
    
    StPicoThreeVector<T>  operator- ();
    StPicoThreeVector<T>  operator+ ();
    StPicoThreeVector<T>& operator*= (double);
    StPicoThreeVector<T>& operator/= (double);
    StPicoThreeVector<T>  pseudoProduct(double,double,double) const;
 
#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)
    template<class X> T                angle(const StPicoThreeVector<X>&) const;
    template<class X> StPicoThreeVector<T> cross(const StPicoThreeVector<X>&) const;
    template<class X> T                dot  (const StPicoThreeVector<X>&) const;
    template<class X> StPicoThreeVector<T> pseudoProduct(const StPicoThreeVector<X>&) const;
    
    template<class X> bool operator == (const StPicoThreeVector<X>& v) const;
    template<class X> bool operator != (const StPicoThreeVector<X>& v) const;

    template<class X> StPicoThreeVector<T>& operator+= (const StPicoThreeVector<X>&);
    template<class X> StPicoThreeVector<T>& operator-= (const StPicoThreeVector<X>&);
#else    
    T                angle(const StPicoThreeVector<float>&) const;
    StPicoThreeVector<T> cross(const StPicoThreeVector<float>&) const;
    T                dot  (const StPicoThreeVector<float>&) const;
    StPicoThreeVector<T> pseudoProduct(const StPicoThreeVector<float>&) const;
    
    T                angle(const StPicoThreeVector<double>&) const;
    T                dot  (const StPicoThreeVector<double>&) const;
    StPicoThreeVector<T> cross(const StPicoThreeVector<double>&) const;
    StPicoThreeVector<T> pseudoProduct(const StPicoThreeVector<double>&) const;

    bool operator == (const StPicoThreeVector<float>& v) const;
    bool operator != (const StPicoThreeVector<float>& v) const;
    StPicoThreeVector<T>& operator+= (const StPicoThreeVector<float>&);
    StPicoThreeVector<T>& operator-= (const StPicoThreeVector<float>&);
    
    bool operator == (const StPicoThreeVector<double>& v) const;
    bool operator != (const StPicoThreeVector<double>& v) const;
    StPicoThreeVector<T>& operator+= (const StPicoThreeVector<double>&);
    StPicoThreeVector<T>& operator-= (const StPicoThreeVector<double>&);
#endif
  int             valid(double world = 1.e+5) const;
  int               bad(double world = 1.e+5) const;
  
 protected:
  T    mX1, mX2, mX3;
    
  ClassDef(StPicoThreeVector,1)
};

//
//        Implementation of member functions
//
template<class T>
inline StPicoThreeVector<T>::StPicoThreeVector()
    : mX1(0), mX2(0), mX3(0) {/* nop */}

template<class T>
inline StPicoThreeVector<T>::StPicoThreeVector(T X, T Y, T Z)
    : mX1(X), mX2(Y), mX3(Z) {/* nop */}
template<class T>
inline StPicoThreeVector<T>::~StPicoThreeVector() {/* nop */}

template<class T>
inline void StPicoThreeVector<T>::setX(T X) {mX1 = X;}

template<class T>
inline void StPicoThreeVector<T>::setY(T Y) {mX2 = Y;}

template<class T>
inline void StPicoThreeVector<T>::setZ(T Z) {mX3 = Z;}

template<class T>
void StPicoThreeVector<T>::setPhi(T Angle)
{
    double  r = magnitude();
    double th = theta();
    
    mX1 = r*sin(th)*cos(Angle);
    mX2 = r*sin(th)*sin(Angle);
}

template <class T>
void StPicoThreeVector<T>::setTheta(T Angle)
{
    double r  = magnitude();
    double ph = phi();

    mX1 = r*sin(Angle)*cos(ph);
    mX2 = r*sin(Angle)*sin(ph);
    mX3 = r*cos(Angle);
}

template <class T>
void StPicoThreeVector<T>::setMagnitude(T r)
{
    double th = theta();
    double ph = phi();
    
    mX1 = r*sin(th)*cos(ph);
    mX2 = r*sin(th)*sin(ph);
    mX3 = r*cos(th);
}

template <class T>
void StPicoThreeVector<T>::setMag(T Mag)
{
    setMagnitude(Mag);
}

template<class T>
inline const T& StPicoThreeVector<T>::x() const {return mX1;}

template<class T>
inline const T& StPicoThreeVector<T>::y() const {return mX2;}

template<class T>
inline const T& StPicoThreeVector<T>::z() const {return mX3;}

template<class T>
inline const T* StPicoThreeVector<T>::xyz() const {return &mX1;}

template<class T>
inline T* StPicoThreeVector<T>::xyz() {return &mX1;}

template<class T>
inline T StPicoThreeVector<T>::theta() const
{
  return acos(cosTheta());
}

template<class T>
inline T StPicoThreeVector<T>::cosTheta() const
{
  return mX3/(mag()+1e-20);
}

template<class T>
inline T StPicoThreeVector<T>::phi() const
{
    return atan2(mX2,mX1);
}

template<class T>
inline T StPicoThreeVector<T>::pseudoRapidity() const
{
    //
    // change code to more optimal:
    // double m = mag();
    // return 0.5*::log( (m+z())/(m-z()) );
    double tmp = tan(theta()/2.); if (tmp <=0.) return 1e20;
    return -::log(tmp);
}

template<class T>
inline StPicoThreeVector<T> StPicoThreeVector<T>::unit() const
{
    double tmp = mag(); if (tmp<=0.) tmp = 1e-20;
    return *this/tmp;
}

template <class T>
T StPicoThreeVector<T>::massHypothesis(T mass) const
{
    return ::sqrt((*this)*(*this) + mass*mass);
}

template <class T>
StPicoThreeVector<T> StPicoThreeVector<T>::orthogonal() const
{
    // Direct copy from CLHEP--it is probably better to
    // use your own dot/cross product code...
    double X = (mX1 < 0.0) ? -mX1 : mX1;
    double Y = (mX2 < 0.0) ? -mX2 : mX2;
    double Z = (mX3 < 0.0) ? -mX3 : mX3;
    
    if(X<Y)
	return X < Z ? StPicoThreeVector<T>(0,mX3,-mX2) :  StPicoThreeVector<T>(mX2,-mX1,0);
    else
	return  mX2 < mX3 ? StPicoThreeVector<T>(-mX3,0,mX1) :  StPicoThreeVector<T>(mX2,-mX1,0);
}

template <class T>
void StPicoThreeVector<T>::rotateX(T Angle)
{
    // may in the future make use of the StRotation class!
    double yPrime = cos(Angle)*mX2 - sin(Angle)*mX3;
    double zPrime = sin(Angle)*mX2 + cos(Angle)*mX3;

    mX2 = yPrime;
    mX3 = zPrime;
}

template <class T>
void StPicoThreeVector<T>::rotateY(T Angle)
{
    // may in the future make use of the StRotation class!
    double zPrime = cos(Angle)*mX3 - sin(Angle)*mX1;
    double xPrime = sin(Angle)*mX3 + cos(Angle)*mX1;

    mX1 = xPrime;
    mX3 = zPrime;
}

template <class T>
void StPicoThreeVector<T>::rotateZ(T Angle)
{
    // may in the future make use of the StRotation class!
    double xPrime = cos(Angle)*mX1 - sin(Angle)*mX2;
    double yPrime = sin(Angle)*mX1 + cos(Angle)*mX2;

    mX1 = xPrime;
    mX2 = yPrime;
}

template<class T>
inline T StPicoThreeVector<T>::perp() const
{
    return ::sqrt(mX1*mX1+mX2*mX2);
}

template<class T>
inline T StPicoThreeVector<T>::perp2() const
{
    return mX1*mX1+mX2*mX2;
}

template<class T>
inline T StPicoThreeVector<T>::magnitude() const
{
    return mag();
}

template<class T>
inline T StPicoThreeVector<T>::mag() const
{
    return ::sqrt(mX1*mX1+mX2*mX2+mX3*mX3);
}

template<class T>
inline T StPicoThreeVector<T>::mag2() const
{
    return mX1*mX1+mX2*mX2+mX3*mX3;
}

template<class T>
inline T StPicoThreeVector<T>::operator() (size_t i) const
{
    if (i <= 2)  return (&mX1)[i];
#ifndef ST_NO_EXCEPTIONS
    throw out_of_range("StPicoThreeVector<T>::operator(): bad index");
#else
    cerr << "StPicoThreeVector<T>::operator(): bad index" << endl;
#endif
    return 0;
}

template<class T>
inline T& StPicoThreeVector<T>::operator() (size_t i) 
{
    if (i <= 2)  return (&mX1)[i];
#ifndef ST_NO_EXCEPTIONS
    throw out_of_range("StPicoThreeVector<T>::operator(): bad index");
#else
    cerr << "StPicoThreeVector<T>::operator(): bad index" << endl;
#endif
    return mX1;
}

template<class T>
inline T StPicoThreeVector<T>::operator[] (size_t i) const
{
    if (i <= 2)  return (&mX1)[i];
#ifndef ST_NO_EXCEPTIONS
      throw out_of_range("StPicoThreeVector<T>::operator[]: bad index"); 
#else
      cerr << "StPicoThreeVector<T>::operator[]: bad index" << endl;
#endif
      return 0;
}

template<class T>
inline T &StPicoThreeVector<T>::operator[] (size_t i) 
{
    if (i <= 2)  return (&mX1)[i];
#ifndef ST_NO_EXCEPTIONS
      throw out_of_range("StPicoThreeVector<T>::operator[]: bad index"); 
#else
      cerr << "StPicoThreeVector<T>::operator[]: bad index" << endl;
#endif
      return mX1;
}
#ifndef __CINT__
template<class T>
inline StPicoThreeVector<T>& StPicoThreeVector<T>::operator*= (double c)
{
    mX1 *= c; mX2 *= c; mX3 *= c;
    return *this;
}
#else
template <> StPicoThreeVector<double>& StPicoThreeVector<double>::operator*= (double c);
template <> StPicoThreeVector<float>& StPicoThreeVector<float>::operator*= (double c);
#endif
template<class T>
inline StPicoThreeVector<T>& StPicoThreeVector<T>::operator/= (double c)
{
    mX1 /= c; mX2 /= c; mX3 /= c;
    return *this;
}

template<class T>
inline StPicoThreeVector<T>
StPicoThreeVector<T>::pseudoProduct(double X,double Y,double Z) const
{
    return StPicoThreeVector<T>(mX1*X,mX2*Y,mX3*Z);
}

template<class T>
StPicoThreeVector<T> StPicoThreeVector<T>::operator- ()
{
    return StPicoThreeVector<T>(-mX1, -mX2, -mX3);
}

template<class T>
StPicoThreeVector<T> StPicoThreeVector<T>::operator+ ()
{
    return *this;
}

#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)

template<class T>
template<class X>
inline StPicoThreeVector<T>::StPicoThreeVector(const StPicoThreeVector<X>& v)
    : mX1(v.x()), mX2(v.y()), mX3(v.z()) {/* nop */}

template<class T>
template<class X>
inline StPicoThreeVector<T>::StPicoThreeVector(const X *a)
{
    mX1 = a[0];
    mX2 = a[1];
    mX3 = a[2];
}

template<class T>
template<class X>
inline StPicoThreeVector<T>&
StPicoThreeVector<T>::operator=(const StPicoThreeVector<X>& v)
{
    mX1 = v.x();  mX2 = v.y();  mX3 = v.z();
    return *this;
}

template<class T>
template<class X>
inline bool StPicoThreeVector<T>::operator== (const StPicoThreeVector<X>& v) const
{
    return mX1 == v.x() && mX2 == v.y() && mX3 == v.z();
}

template<class T>
template<class X>
inline bool StPicoThreeVector<T>::operator!= (const StPicoThreeVector<X>& v) const
{
    return !(*this == v);
}

template<class T>
template<class X>
inline StPicoThreeVector<T>&
StPicoThreeVector<T>::operator+= (const StPicoThreeVector<X>& v)
{
    mX1 += v.x(); mX2 += v.y(); mX3 += v.z();
    return *this;
}

template<class T>
template<class X>
inline StPicoThreeVector<T>&
StPicoThreeVector<T>::operator-= (const StPicoThreeVector<X>& v)
{
    mX1 -= v.x(); mX2 -= v.y(); mX3 -= v.z();
    return *this;
}

template<class T>
template<class X>
inline T StPicoThreeVector<T>::dot(const StPicoThreeVector<X>& v) const
{
    return mX1*v.x() + mX2*v.y() + mX3*v.z();
}

template<class T>
template<class X>
inline StPicoThreeVector<T>
StPicoThreeVector<T>::cross(const StPicoThreeVector<X>& v) const
{
    return StPicoThreeVector<T>(mX2*v.z() - mX3*v.y(),
			    mX3*v.x() - mX1*v.z(),
			    mX1*v.y() - mX2*v.x());
}

template<class T>
template<class X>
inline T StPicoThreeVector<T>::angle(const StPicoThreeVector<X>& vec) const
{
    double norm = this->mag2()*vec.mag2(); 
    
    return norm > 0 ? acos(this->dot(vec)/(::sqrt(norm))) : 0;
}

template<class T>
template<class X>
inline StPicoThreeVector<T>
StPicoThreeVector<T>::pseudoProduct(const StPicoThreeVector<X>& v) const
{
    return this->pseudoProduct(v.x(),v.y(),v.z());
}

#else

template<class T>
inline StPicoThreeVector<T>::StPicoThreeVector(const StPicoThreeVector<float>& v)
    : mX1(v.x()), mX2(v.y()), mX3(v.z()) {/* nop */}

template<class T>
inline StPicoThreeVector<T>::StPicoThreeVector(const StPicoThreeVector<double>& v)
    : mX1(v.x()), mX2(v.y()), mX3(v.z()) {/* nop */}

template<class T>
inline StPicoThreeVector<T>::StPicoThreeVector(const float *a)
{
    mX1 = a[0];
    mX2 = a[1];
    mX3 = a[2];
}

template<class T>
inline StPicoThreeVector<T>::StPicoThreeVector(const double *a)
{
    mX1 = a[0];
    mX2 = a[1];
    mX3 = a[2];
}

template<class T>
inline StPicoThreeVector<T>&
StPicoThreeVector<T>::operator=(const StPicoThreeVector<float>& v)
{
    mX1 = v.x();  mX2 = v.y();  mX3 = v.z();
    return *this;
}

template<class T>
inline StPicoThreeVector<T>&
StPicoThreeVector<T>::operator=(const StPicoThreeVector<double>& v)
{
    mX1 = v.x();  mX2 = v.y();  mX3 = v.z();
    return *this;
}

template<class T>
inline bool
StPicoThreeVector<T>::operator== (const StPicoThreeVector<float>& v) const
{
    return mX1 == v.x() && mX2 == v.y() && mX3 == v.z();
}

template<class T>
inline bool
StPicoThreeVector<T>::operator== (const StPicoThreeVector<double>& v) const
{
    return mX1 == v.x() && mX2 == v.y() && mX3 == v.z();
}

template<class T>
inline bool
StPicoThreeVector<T>::operator!= (const StPicoThreeVector<float>& v) const
{
    return !(*this == v);
}

template<class T>
inline bool
StPicoThreeVector<T>::operator!= (const StPicoThreeVector<double>& v) const
{
    return !(*this == v);
}

template<class T>
inline StPicoThreeVector<T>&
StPicoThreeVector<T>::operator+= (const StPicoThreeVector<float>& v)
{
    mX1 += v.x(); mX2 += v.y(); mX3 += v.z();
    return *this;
}

template<class T>
inline StPicoThreeVector<T>&
StPicoThreeVector<T>::operator+= (const StPicoThreeVector<double>& v)
{
    mX1 += v.x(); mX2 += v.y(); mX3 += v.z();
    return *this;
}

template<class T>
inline StPicoThreeVector<T>&
StPicoThreeVector<T>::operator-= (const StPicoThreeVector<float>& v)
{
    mX1 -= v.x(); mX2 -= v.y(); mX3 -= v.z();
    return *this;
}

template<class T>
inline StPicoThreeVector<T>&
StPicoThreeVector<T>::operator-= (const StPicoThreeVector<double>& v)
{
    mX1 -= v.x(); mX2 -= v.y(); mX3 -= v.z();
    return *this;
}

template<class T>
inline T StPicoThreeVector<T>::dot(const StPicoThreeVector<float>& v) const
{
    return mX1*v.x() + mX2*v.y() + mX3*v.z();
}

template<class T>
inline T StPicoThreeVector<T>::dot(const StPicoThreeVector<double>& v) const
{
    return mX1*v.x() + mX2*v.y() + mX3*v.z();
}

template<class T>
inline StPicoThreeVector<T>
StPicoThreeVector<T>::cross(const StPicoThreeVector<float>& v) const
{
    return StPicoThreeVector<T>(mX2*v.z() - mX3*v.y(),
			    mX3*v.x() - mX1*v.z(),
			    mX1*v.y() - mX2*v.x());
}

template<class T>
inline StPicoThreeVector<T>
StPicoThreeVector<T>::cross(const StPicoThreeVector<double>& v) const
{
    return StPicoThreeVector<T>(mX2*v.z() - mX3*v.y(),
			    mX3*v.x() - mX1*v.z(),
			    mX1*v.y() - mX2*v.x());
}

template<class T>
inline T StPicoThreeVector<T>::angle(const StPicoThreeVector<float>& v) const
{
    double tmp = mag()*v.mag(); if (tmp <=0) tmp = 1e-20;
    return acos(this->dot(v)/tmp);
}

template<class T>
inline T StPicoThreeVector<T>::angle(const StPicoThreeVector<double>& v) const
{
    double tmp = mag()*v.mag(); if (tmp <=0) tmp = 1e-20;
    return acos(this->dot(v)/tmp);
}

template<class T>
inline StPicoThreeVector<T>
StPicoThreeVector<T>::pseudoProduct(const StPicoThreeVector<float>& v) const
{
    return this->pseudoProduct(v.x(),v.y(),v.z());
}

template<class T>
inline StPicoThreeVector<T>
StPicoThreeVector<T>::pseudoProduct(const StPicoThreeVector<double>& v) const
{
    return this->pseudoProduct(v.x(),v.y(),v.z());
}
#endif  // ST_NO_MEMBER_TEMPLATES
template<class T>
inline int
StPicoThreeVector<T>::valid(double world) const  {return !bad(world);}

template<class T>
inline int
StPicoThreeVector<T>::bad(double world) const
{
  for (int i=0;i<3;i++) {
    if (!std::isfinite((&mX1)[i])      ) return 10+i; 		
    if ( std::fabs((&mX1)[i])>world) return 20+i; 		
  }		
  return 0;		
}
//
//        Non-member functions
//
#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)
template<class T>
inline T abs(const StPicoThreeVector<T>& v) {return v.mag();}
template<class T, class X>
inline StPicoThreeVector<T>
cross_product(const StPicoThreeVector<T>& v1, const StPicoThreeVector<X>& v2)
{
    return v1.cross(v2);
}
template<class T, class X>
inline StPicoThreeVector<T>
operator+ (const StPicoThreeVector<T>& v1, const StPicoThreeVector<X>& v2)
{
    return StPicoThreeVector<T>(v1) += v2;
}

template<class T, class X>
inline StPicoThreeVector<T>
operator- (const StPicoThreeVector<T>& v1, const StPicoThreeVector<X>& v2)
{
    return StPicoThreeVector<T>(v1) -= v2;
}

template<class T, class X>
inline T operator* (const StPicoThreeVector<T>& v1, const StPicoThreeVector<X>& v2)
{
    return StPicoThreeVector<T>(v1).dot(v2);
}
#else
template<>
inline double abs(const StPicoThreeVector<double>& v) {return v.mag();}

template<>
inline float abs(const StPicoThreeVector<float>& v) {return v.mag();}

template<class T>
inline StPicoThreeVector<T>
cross_product(const StPicoThreeVector<T>& v1, const StPicoThreeVector<double>& v2)
{
    return v1.cross(v2);
}
template<class T>
inline StPicoThreeVector<T>
cross_product(const StPicoThreeVector<T>& v1, const StPicoThreeVector<float>& v2)
{
    return v1.cross(v2);
}


//
//        Non-member operators
//
template<class T>
inline StPicoThreeVector<T>
operator+ (const StPicoThreeVector<T>& v1, const StPicoThreeVector<double>& v2)
{
    return StPicoThreeVector<T>(v1) += v2;
}

template<class T>
inline StPicoThreeVector<T>
operator- (const StPicoThreeVector<T>& v1, const StPicoThreeVector<double>& v2)
{
    return StPicoThreeVector<T>(v1) -= v2;
}
#ifndef __CINT__
template<class T>
inline T operator* (const StPicoThreeVector<T>& v1, const StPicoThreeVector<double>& v2)
{
    return StPicoThreeVector<T>(v1).dot(v2);
}
template<class T>
inline T operator* (const StPicoThreeVector<T>& v1, const StPicoThreeVector<float>& v2)
{
    return StPicoThreeVector<T>(v1).dot(v2);
}
#else
template<> double operator* (const StPicoThreeVector<double>& v1, const StPicoThreeVector<double>& v2);
template<> double operator* (const StPicoThreeVector<double>& v1, const StPicoThreeVector<float>& v2);
template<> double operator* (const StPicoThreeVector<float>& v1, const StPicoThreeVector<double>& v2);
template<> float  operator* (const StPicoThreeVector<float>& v1, const StPicoThreeVector<float>& v2);
#endif
template<class T>
inline StPicoThreeVector<T>
operator+ (const StPicoThreeVector<T>& v1, const StPicoThreeVector<float>& v2)
{
    return StPicoThreeVector<T>(v1) += v2;
}

template<class T>
inline StPicoThreeVector<T>
operator- (const StPicoThreeVector<T>& v1, const StPicoThreeVector<float>& v2)
{
    return StPicoThreeVector<T>(v1) -= v2;
}

#endif
template<class T>
inline StPicoThreeVector<T> operator* (const StPicoThreeVector<T>& v, double c)
{
    return StPicoThreeVector<T>(v) *= c;
}

template<class T>
inline StPicoThreeVector<T> operator* (double c, const StPicoThreeVector<T>& v)
{
    return StPicoThreeVector<T>(v) *= c;
}

template<class T>
inline StPicoThreeVector<T> operator/ (const StPicoThreeVector<T>& v,double c)
{
    return StPicoThreeVector<T>(v) /= c;
}
#ifndef __CINT__
template<class T>
std::ostream&  operator<<(std::ostream& os, const StPicoThreeVector<T>& v)
{
    return os << v.x() << '\t' << v.y() << '\t' << v.z();
}
#else
template<> std::ostream&  operator<<(std::ostream& os, const StPicoThreeVector<double>& v);
template<> std::ostream&  operator<<(std::ostream& os, const StPicoThreeVector<float>& v);
#endif
template<class T>
std::istream&  operator>>(std::istream& is, StPicoThreeVector<T>& v)
{
    T  x, y, z;
    is >> x >> y >> z;
    v.setX(x);
    v.setY(y);
    v.setZ(z);
    return is;
}
#endif

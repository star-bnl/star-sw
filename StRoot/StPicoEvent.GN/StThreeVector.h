#ifndef ST_THREE_VECTOR_H
#define ST_THREE_VECTOR_H

#ifdef _VANILLA_ROOT_

/// ROOT headers
#include <Rtypes.h>
#include <Riostream.h>
#include <RVersion.h>

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
template<class T> class StThreeVector {
  
 public:    
    StThreeVector();
    StThreeVector(T, T, T);
  //                     ROOT_VERSION(5,03,01)
#if ROOT_VERSION_CODE >= 328449
   StThreeVector(TRootIOCtor*) : mX1(0), mX2(0), mX3(0) {}
#endif
    virtual ~StThreeVector();

#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)
    template<class X> StThreeVector(const StThreeVector<X>&);
    template<class X> StThreeVector(const X*);  
    template<class X> StThreeVector<T>& operator=(const StThreeVector<X>&);
    // StThreeVector(const StThreeVector<T>&);                use default
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
    
    StThreeVector<T>  unit()       const;
    StThreeVector<T>  orthogonal() const;

    void  rotateX(T);
    void  rotateY(T);
    void  rotateZ(T);
    
    StThreeVector<T>  operator- ();
    StThreeVector<T>  operator+ ();
    StThreeVector<T>& operator*= (double);
    StThreeVector<T>& operator/= (double);
    StThreeVector<T>  pseudoProduct(double,double,double) const;
 
#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)
    template<class X> T                angle(const StThreeVector<X>&) const;
    template<class X> StThreeVector<T> cross(const StThreeVector<X>&) const;
    template<class X> T                dot  (const StThreeVector<X>&) const;
    template<class X> StThreeVector<T> pseudoProduct(const StThreeVector<X>&) const;
    
    template<class X> bool operator == (const StThreeVector<X>& v) const;
    template<class X> bool operator != (const StThreeVector<X>& v) const;

    template<class X> StThreeVector<T>& operator+= (const StThreeVector<X>&);
    template<class X> StThreeVector<T>& operator-= (const StThreeVector<X>&);
#else    
    T                angle(const StThreeVector<float>&) const;
    StThreeVector<T> cross(const StThreeVector<float>&) const;
    T                dot  (const StThreeVector<float>&) const;
    StThreeVector<T> pseudoProduct(const StThreeVector<float>&) const;
    
    T                angle(const StThreeVector<double>&) const;
    T                dot  (const StThreeVector<double>&) const;
    StThreeVector<T> cross(const StThreeVector<double>&) const;
    StThreeVector<T> pseudoProduct(const StThreeVector<double>&) const;

    bool operator == (const StThreeVector<float>& v) const;
    bool operator != (const StThreeVector<float>& v) const;
    StThreeVector<T>& operator+= (const StThreeVector<float>&);
    StThreeVector<T>& operator-= (const StThreeVector<float>&);
    
    bool operator == (const StThreeVector<double>& v) const;
    bool operator != (const StThreeVector<double>& v) const;
    StThreeVector<T>& operator+= (const StThreeVector<double>&);
    StThreeVector<T>& operator-= (const StThreeVector<double>&);
#endif
  int             valid(double world = 1.e+5) const;
  int               bad(double world = 1.e+5) const;
  
 protected:
  T    mX1, mX2, mX3;
  //#ifdef __ROOT__    
  ClassDef(StThreeVector,3)
    //#endif
};

//
//        Implementation of member functions
//
template<class T>
inline StThreeVector<T>::StThreeVector()
    : mX1(0), mX2(0), mX3(0) {/* nop */}

template<class T>
inline StThreeVector<T>::StThreeVector(T X, T Y, T Z)
    : mX1(X), mX2(Y), mX3(Z) {/* nop */}
template<class T>
inline StThreeVector<T>::~StThreeVector() {/* nop */}

template<class T>
inline void StThreeVector<T>::setX(T X) {mX1 = X;}

template<class T>
inline void StThreeVector<T>::setY(T Y) {mX2 = Y;}

template<class T>
inline void StThreeVector<T>::setZ(T Z) {mX3 = Z;}

template<class T>
void StThreeVector<T>::setPhi(T Angle)
{
    double  r = magnitude();
    double th = theta();
    
    mX1 = r*sin(th)*cos(Angle);
    mX2 = r*sin(th)*sin(Angle);
}

template <class T>
void StThreeVector<T>::setTheta(T Angle)
{
    double r  = magnitude();
    double ph = phi();

    mX1 = r*sin(Angle)*cos(ph);
    mX2 = r*sin(Angle)*sin(ph);
    mX3 = r*cos(Angle);
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
void StThreeVector<T>::setMag(T Mag)
{
    setMagnitude(Mag);
}

template<class T>
inline const T& StThreeVector<T>::x() const {return mX1;}

template<class T>
inline const T& StThreeVector<T>::y() const {return mX2;}

template<class T>
inline const T& StThreeVector<T>::z() const {return mX3;}

template<class T>
inline const T* StThreeVector<T>::xyz() const {return &mX1;}

template<class T>
inline T* StThreeVector<T>::xyz() {return &mX1;}

template<class T>
inline T StThreeVector<T>::theta() const
{
  return acos(cosTheta());
}

template<class T>
inline T StThreeVector<T>::cosTheta() const
{
  return mX3/(mag()+1e-20);
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
    // return 0.5*::log( (m+z())/(m-z()) );
    double tmp = tan(theta()/2.); if (tmp <=0.) return 1e20;
    return -::log(tmp);
}

template<class T>
inline StThreeVector<T> StThreeVector<T>::unit() const
{
    double tmp = mag(); if (tmp<=0.) tmp = 1e-20;
    return *this/tmp;
}

template <class T>
T StThreeVector<T>::massHypothesis(T mass) const
{
    return ::sqrt((*this)*(*this) + mass*mass);
}

template <class T>
StThreeVector<T> StThreeVector<T>::orthogonal() const
{
    // Direct copy from CLHEP--it is probably better to
    // use your own dot/cross product code...
    double X = (mX1 < 0.0) ? -mX1 : mX1;
    double Y = (mX2 < 0.0) ? -mX2 : mX2;
    double Z = (mX3 < 0.0) ? -mX3 : mX3;
    
    if(X<Y)
	return X < Z ? StThreeVector<T>(0,mX3,-mX2) :  StThreeVector<T>(mX2,-mX1,0);
    else
	return  mX2 < mX3 ? StThreeVector<T>(-mX3,0,mX1) :  StThreeVector<T>(mX2,-mX1,0);
}

template <class T>
void StThreeVector<T>::rotateX(T Angle)
{
    // may in the future make use of the StRotation class!
    double yPrime = cos(Angle)*mX2 - sin(Angle)*mX3;
    double zPrime = sin(Angle)*mX2 + cos(Angle)*mX3;

    mX2 = yPrime;
    mX3 = zPrime;
}

template <class T>
void StThreeVector<T>::rotateY(T Angle)
{
    // may in the future make use of the StRotation class!
    double zPrime = cos(Angle)*mX3 - sin(Angle)*mX1;
    double xPrime = sin(Angle)*mX3 + cos(Angle)*mX1;

    mX1 = xPrime;
    mX3 = zPrime;
}

template <class T>
void StThreeVector<T>::rotateZ(T Angle)
{
    // may in the future make use of the StRotation class!
    double xPrime = cos(Angle)*mX1 - sin(Angle)*mX2;
    double yPrime = sin(Angle)*mX1 + cos(Angle)*mX2;

    mX1 = xPrime;
    mX2 = yPrime;
}

template<class T>
inline T StThreeVector<T>::perp() const
{
    return ::sqrt(mX1*mX1+mX2*mX2);
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
    return ::sqrt(mX1*mX1+mX2*mX2+mX3*mX3);
}

template<class T>
inline T StThreeVector<T>::mag2() const
{
    return mX1*mX1+mX2*mX2+mX3*mX3;
}

template<class T>
inline T StThreeVector<T>::operator() (size_t i) const
{
    if (i <= 2)  return (&mX1)[i];
#ifndef ST_NO_EXCEPTIONS
    throw out_of_range("StThreeVector<T>::operator(): bad index");
#else
    cerr << "StThreeVector<T>::operator(): bad index" << endl;
#endif
    return 0;
}

template<class T>
inline T& StThreeVector<T>::operator() (size_t i) 
{
    if (i <= 2)  return (&mX1)[i];
#ifndef ST_NO_EXCEPTIONS
    throw out_of_range("StThreeVector<T>::operator(): bad index");
#else
    cerr << "StThreeVector<T>::operator(): bad index" << endl;
#endif
    return mX1;
}

template<class T>
inline T StThreeVector<T>::operator[] (size_t i) const
{
    if (i <= 2)  return (&mX1)[i];
#ifndef ST_NO_EXCEPTIONS
      throw out_of_range("StThreeVector<T>::operator[]: bad index"); 
#else
      cerr << "StThreeVector<T>::operator[]: bad index" << endl;
#endif
      return 0;
}

template<class T>
inline T &StThreeVector<T>::operator[] (size_t i) 
{
    if (i <= 2)  return (&mX1)[i];
#ifndef ST_NO_EXCEPTIONS
      throw out_of_range("StThreeVector<T>::operator[]: bad index"); 
#else
      cerr << "StThreeVector<T>::operator[]: bad index" << endl;
#endif
      return mX1;
}
#ifndef __CINT__
template<class T>
inline StThreeVector<T>& StThreeVector<T>::operator*= (double c)
{
    mX1 *= c; mX2 *= c; mX3 *= c;
    return *this;
}
#else
template <> StThreeVector<double>& StThreeVector<double>::operator*= (double c);
template <> StThreeVector<float>& StThreeVector<float>::operator*= (double c);
#endif
template<class T>
inline StThreeVector<T>& StThreeVector<T>::operator/= (double c)
{
    mX1 /= c; mX2 /= c; mX3 /= c;
    return *this;
}

template<class T>
inline StThreeVector<T>
StThreeVector<T>::pseudoProduct(double X,double Y,double Z) const
{
    return StThreeVector<T>(mX1*X,mX2*Y,mX3*Z);
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

#if !defined(ST_NO_MEMBER_TEMPLATES) && !defined(__CINT__)

template<class T>
template<class X>
inline StThreeVector<T>::StThreeVector(const StThreeVector<X>& v)
    : mX1(v.x()), mX2(v.y()), mX3(v.z()) {/* nop */}

template<class T>
template<class X>
inline StThreeVector<T>::StThreeVector(const X *a)
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
    
    return norm > 0 ? acos(this->dot(vec)/(::sqrt(norm))) : 0;
}

template<class T>
template<class X>
inline StThreeVector<T>
StThreeVector<T>::pseudoProduct(const StThreeVector<X>& v) const
{
    return this->pseudoProduct(v.x(),v.y(),v.z());
}

#else

template<class T>
inline StThreeVector<T>::StThreeVector(const StThreeVector<float>& v)
    : mX1(v.x()), mX2(v.y()), mX3(v.z()) {/* nop */}

template<class T>
inline StThreeVector<T>::StThreeVector(const StThreeVector<double>& v)
    : mX1(v.x()), mX2(v.y()), mX3(v.z()) {/* nop */}

template<class T>
inline StThreeVector<T>::StThreeVector(const float *a)
{
    mX1 = a[0];
    mX2 = a[1];
    mX3 = a[2];
}

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

template<class T>
inline StThreeVector<T>
StThreeVector<T>::cross(const StThreeVector<double>& v) const
{
    return StThreeVector<T>(mX2*v.z() - mX3*v.y(),
			    mX3*v.x() - mX1*v.z(),
			    mX1*v.y() - mX2*v.x());
}

template<class T>
inline T StThreeVector<T>::angle(const StThreeVector<float>& v) const
{
    double tmp = mag()*v.mag(); if (tmp <=0) tmp = 1e-20;
    return acos(this->dot(v)/tmp);
}

template<class T>
inline T StThreeVector<T>::angle(const StThreeVector<double>& v) const
{
    double tmp = mag()*v.mag(); if (tmp <=0) tmp = 1e-20;
    return acos(this->dot(v)/tmp);
}

template<class T>
inline StThreeVector<T>
StThreeVector<T>::pseudoProduct(const StThreeVector<float>& v) const
{
    return this->pseudoProduct(v.x(),v.y(),v.z());
}

template<class T>
inline StThreeVector<T>
StThreeVector<T>::pseudoProduct(const StThreeVector<double>& v) const
{
    return this->pseudoProduct(v.x(),v.y(),v.z());
}
#endif  // ST_NO_MEMBER_TEMPLATES
template<class T>
inline int
StThreeVector<T>::valid(double world) const  {return !bad(world);}

template<class T>
inline int
StThreeVector<T>::bad(double world) const
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
inline T abs(const StThreeVector<T>& v) {return v.mag();}
template<class T, class X>
inline StThreeVector<T>
cross_product(const StThreeVector<T>& v1, const StThreeVector<X>& v2)
{
    return v1.cross(v2);
}
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
#else
template<>
inline double abs(const StThreeVector<double>& v) {return v.mag();}

template<>
inline float abs(const StThreeVector<float>& v) {return v.mag();}

template<class T>
inline StThreeVector<T>
cross_product(const StThreeVector<T>& v1, const StThreeVector<double>& v2)
{
    return v1.cross(v2);
}
template<class T>
inline StThreeVector<T>
cross_product(const StThreeVector<T>& v1, const StThreeVector<float>& v2)
{
    return v1.cross(v2);
}


//
//        Non-member operators
//
template<class T>
inline StThreeVector<T>
operator+ (const StThreeVector<T>& v1, const StThreeVector<double>& v2)
{
    return StThreeVector<T>(v1) += v2;
}

template<class T>
inline StThreeVector<T>
operator- (const StThreeVector<T>& v1, const StThreeVector<double>& v2)
{
    return StThreeVector<T>(v1) -= v2;
}
#ifndef __CINT__
template<class T>
inline T operator* (const StThreeVector<T>& v1, const StThreeVector<double>& v2)
{
    return StThreeVector<T>(v1).dot(v2);
}
template<class T>
inline T operator* (const StThreeVector<T>& v1, const StThreeVector<float>& v2)
{
    return StThreeVector<T>(v1).dot(v2);
}
#else
template<> double operator* (const StThreeVector<double>& v1, const StThreeVector<double>& v2);
template<> double operator* (const StThreeVector<double>& v1, const StThreeVector<float>& v2);
template<> double operator* (const StThreeVector<float>& v1, const StThreeVector<double>& v2);
template<> float  operator* (const StThreeVector<float>& v1, const StThreeVector<float>& v2);
#endif
template<class T>
inline StThreeVector<T>
operator+ (const StThreeVector<T>& v1, const StThreeVector<float>& v2)
{
    return StThreeVector<T>(v1) += v2;
}

template<class T>
inline StThreeVector<T>
operator- (const StThreeVector<T>& v1, const StThreeVector<float>& v2)
{
    return StThreeVector<T>(v1) -= v2;
}

#endif
template<class T>
inline StThreeVector<T> operator* (const StThreeVector<T>& v, double c)
{
    return StThreeVector<T>(v) *= c;
}

template<class T>
inline StThreeVector<T> operator* (double c, const StThreeVector<T>& v)
{
    return StThreeVector<T>(v) *= c;
}

template<class T>
inline StThreeVector<T> operator/ (const StThreeVector<T>& v,double c)
{
    return StThreeVector<T>(v) /= c;
}
#ifndef __CINT__
template<class T>
std::ostream&  operator<<(std::ostream& os, const StThreeVector<T>& v)
{
    return os << v.x() << '\t' << v.y() << '\t' << v.z();
}
#else
template<> std::ostream&  operator<<(std::ostream& os, const StThreeVector<double>& v);
template<> std::ostream&  operator<<(std::ostream& os, const StThreeVector<float>& v);
#endif
template<class T>
std::istream&  operator>>(std::istream& is, StThreeVector<T>& v)
{
    T  x, y, z;
    is >> x >> y >> z;
    v.setX(x);
    v.setY(y);
    v.setZ(z);
    return is;
}

#endif // ifdef _VANILLA_ROOT_

#endif

/***************************************************************************
 *
 * $Id: StLorentzVectorOLD.hh,v 1.1 1999/01/30 03:59:03 fisyak Exp $
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
 *            In the near future when all compilers can handle member
 *            templates this class should be cleaned up. A lot of
 *            redundant code can be removed as soon as the compilers
 *            are up-to-date. tu
 *
 ***************************************************************************
 *
 * $Log: StLorentzVectorOLD.hh,v $
 * Revision 1.1  1999/01/30 03:59:03  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/30 03:59:03  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:55  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_LORENTZ_VECTOR_HH
#define ST_LORENTZ_VECTOR_HH

#include "StThreeVector.hh"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
template<class T>
#else
template<class T = double>
#endif
class StLorentzVector : protected StThreeVector<T> {
public:
    StLorentzVector(T = 0, T = 0, T = 0, T = 0);
    ~StLorentzVector();
    
#ifndef ST_NO_MEMBER_TEMPLATES
    template<class X> StLorentzVector(const StThreeVector<X>&, T);
    template<class X> StLorentzVector(T, const StThreeVector<X>&);   

    template<class X> StLorentzVector(const StLorentzVector<X>&);
    template<class X> StLorentzVector<T>& operator=(const StLorentzVector<X>&);
    // StLorentzVector(const StLorentzVector<T>&);                use default
    // StLorentzVector<T>& operator=(const StLorentzVector<T>&);  use default
#else
    StLorentzVector(const StThreeVector<float>&, T);
    StLorentzVector(T, const StThreeVector<float>&);   
    StLorentzVector(const StLorentzVector<float>&);
    
    StLorentzVector(const StThreeVector<double>&, T);
    StLorentzVector(T, const StThreeVector<double>&);   
    StLorentzVector(const StLorentzVector<double>&);
    
    StLorentzVector(const StThreeVector<long double>&, T);    
    StLorentzVector(T, const StThreeVector<long double>&);
    StLorentzVector(const StLorentzVector<long double>&);
    
    StLorentzVector<T>& operator=(const StLorentzVector<float>&);
    StLorentzVector<T>& operator=(const StLorentzVector<double>&);
    StLorentzVector<T>& operator=(const StLorentzVector<long double>&);    
#endif
    
    T t()                     const;
    T px()                    const;
    T py()                    const;
    T pz()                    const;
    T e()                     const;
    T operator()  (size_t)    const;
    T operator[]  (size_t)    const;

    const StThreeVector<T>& vect() const;    
    
    void setPx(T);
    void setPy(T);
    void setPz(T);
    void setE(T);
    void setT(T);
    
#ifndef ST_NO_MEMBER_TEMPLATES
    template <class X> void setVect(const StThreeVector<X>&);
#else
    void setVect(const StThreeVector<float>&);
    void setVect(const StThreeVector<double>&);
    void setVect(const StThreeVector<long double>&);    
#endif   
    
    T plus()               const;
    T minus()              const;
    
    T m()                  const; 
    T m2()                 const; 
    T mt()                 const;
    T mt2()                const;
    T rapidity()           const;
    
#ifndef ST_NO_MEMBER_TEMPLATES
    template<class X> StLorentzVector<T> boost(const StLorentzVector<X>&) const;
#else
    StLorentzVector<T> boost(const StLorentzVector<float>&) const;
    StLorentzVector<T> boost(const StLorentzVector<double>&) const;
    StLorentzVector<T> boost(const StLorentzVector<long double>&) const;
#endif   
    
    StLorentzVector<T>  operator- ();
    StLorentzVector<T>  operator+ ();
    StLorentzVector<T>& operator*= (double);
    StLorentzVector<T>& operator/= (double);

#ifndef ST_NO_MEMBER_TEMPLATES
    template<class X> bool operator == (const StLorentzVector<X>&) const;
    template<class X> bool operator != (const StLorentzVector<X>&) const;
    template<class X> StLorentzVector<T>& operator+= (const StLorentzVector<X>&);
    template<class X> StLorentzVector<T>& operator-= (const StLorentzVector<X>&);
#else    
    bool operator == (const StLorentzVector<float>&) const;
    bool operator != (const StLorentzVector<float>&) const;
    bool operator == (const StLorentzVector<double>&) const;
    bool operator != (const StLorentzVector<double>&) const;
    bool operator == (const StLorentzVector<long double>&) const;
    bool operator != (const StLorentzVector<long double>&) const;

    StLorentzVector<T>& operator+= (const StLorentzVector<float>&);
    StLorentzVector<T>& operator-= (const StLorentzVector<float>&);
    StLorentzVector<T>& operator+= (const StLorentzVector<double>&);
    StLorentzVector<T>& operator-= (const StLorentzVector<double>&);
    StLorentzVector<T>& operator+= (const StLorentzVector<long double>&);
    StLorentzVector<T>& operator-= (const StLorentzVector<long double>&);
#endif
    
    // export selected members from base class
    using StThreeVector<T>::x;
    using StThreeVector<T>::y;
    using StThreeVector<T>::z;
    using StThreeVector<T>::setX;
    using StThreeVector<T>::setY;
    using StThreeVector<T>::setZ;
    using StThreeVector<T>::perp;
    using StThreeVector<T>::perp2;
    using StThreeVector<T>::pseudoRapidity;
    using StThreeVector<T>::phi;
    using StThreeVector<T>::theta;
    using StThreeVector<T>::cosTheta;

protected:
    T	mX4;
};

//
//        Implementation of member functions
//
template<class T>
inline StLorentzVector<T>::StLorentzVector(T x, T y, T z, T t)
    : StThreeVector<T>(x, y, z), mX4(t) { /* nop */ }

template<class T>
inline StLorentzVector<T>::~StLorentzVector() { /* nopt */ }    

template<class T>
inline const StThreeVector<T>& StLorentzVector<T>::vect() const 
{
#ifdef __SUNPRO_CC  
    return static_cast<const StThreeVector<T>& const> (*this);
#else
    return static_cast<const StThreeVector<T>& > (*this);
#endif    
}

template<class T>
inline T StLorentzVector<T>::m2() const
{
    return (mX4*mX4 -
	    StThreeVector<T>::mX3*StThreeVector<T>::mX3 -
	    StThreeVector<T>::mX2*StThreeVector<T>::mX2 -
	    StThreeVector<T>::mX1*StThreeVector<T>::mX1);    
}

template<class T>
inline T StLorentzVector<T>::plus() const { return (e() + pz()); }

template<class T>
inline T StLorentzVector<T>::minus() const { return (e() - pz()); }

template<class T>
inline T StLorentzVector<T>::m() const
{
    T mass2 = m2();
    if (mass2 < 0)
	return -sqrt(-mass2);
    else
	return sqrt(mass2);
}

template<class T>
inline T StLorentzVector<T>::mt2() const
{
    return this->perp2() + m2();
}

template<class T>
inline T StLorentzVector<T>::mt() const
{
    //
    // change to more optimal code ?
    // return e()*e() - pz()*pz();
    T massPerp2 = mt2();
    if (massPerp2 < 0)
	return -sqrt(-massPerp2);
    else
	return sqrt(massPerp2);
}

template<class T>
inline void StLorentzVector<T>::setPx(T x) {StThreeVector<T>::mX1 = x;}

template<class T>
inline void StLorentzVector<T>::setPy(T y) {StThreeVector<T>::mX2 = y;}

template<class T>
inline void StLorentzVector<T>::setPz(T z) {StThreeVector<T>::mX3 = z;}

template<class T>
inline void StLorentzVector<T>::setT(T t) {mX4 = t;}

template<class T>
inline void StLorentzVector<T>::setE(T e) {mX4 = e;}

template<class T>
inline T StLorentzVector<T>::px() const {return StThreeVector<T>::mX1;}

template<class T>
inline T StLorentzVector<T>::py() const {return StThreeVector<T>::mX2;}

template<class T>
inline T StLorentzVector<T>::pz() const {return StThreeVector<T>::mX3;}

template<class T>
inline T StLorentzVector<T>::e() const {return mX4;}

template<class T>
inline T StLorentzVector<T>::t() const {return mX4;}

template<class T>
inline T StLorentzVector<T>::operator() (size_t i) const
{
    if (i < 3)
        return StThreeVector<T>::operator() (i);
    else if (i == 3)
        return mX4;
    else {
#ifndef ST_NO_EXCEPTIONS
      throw out_of_range("StLorentzVector<T>::operator(): bad index");  
#else
      cerr << "StLorentzVector<T>::operator(): bad index." << endl;
#endif
      return 0;
    }
}

template<class T>
inline T StLorentzVector<T>::operator[] (size_t i) const
{
    if (i < 3)
        return StThreeVector<T>::operator[] (i);
    else if (i == 3)
        return mX4;
    else {
#ifndef ST_NO_EXCEPTIONS
      throw out_of_range("StLorentzVector<T>::operator[]: bad index"); 
#else
      cerr << "StLorentzVector<T>::operator[]: bad index." << endl;
#endif
      return 0;
    }
}

template<class T>
inline T StLorentzVector<T>::rapidity() const
{
    return 0.5*log((mX4+StThreeVector<T>::mX3)/(mX4-StThreeVector<T>::mX3));
}

template<class T>
StLorentzVector<T> StLorentzVector<T>::operator- ()
{
    StThreeVector<T> v = this->vect();
    return StLorentzVector<T>(-mX4,-v);
}

template<class T>
StLorentzVector<T> StLorentzVector<T>::operator+ ()
{
    return *this;
}

template<class T>
inline StLorentzVector<T>& StLorentzVector<T>::operator*= (double c)
{
    static_cast<StThreeVector<T>&>(*this) *= c;
    mX4 *= c;
    return *this;
}

template<class T>
inline StLorentzVector<T>& StLorentzVector<T>::operator/= (double c)
{
    static_cast<StThreeVector<T>&>(*this) /= c;
    mX4 /= c;
    return *this;
}

#ifndef ST_NO_MEMBER_TEMPLATES

template<class T>
template<class X>
inline StLorentzVector<T>::StLorentzVector(const StThreeVector<X> &vec, T t)
	: StThreeVector<T>(vec), mX4(t) { /* nop */ }

template<class T>
template<class X>
inline StLorentzVector<T>::StLorentzVector(T t, const StThreeVector<X> &vec)
	: StThreeVector<T>(vec), mX4(t) { /* nop */ }

template<class T>
template<class X>
inline StLorentzVector<T>::StLorentzVector(const StLorentzVector<X> &vec)
	: StThreeVector<T>(vec.vect()), mX4(vec.t()) { /* nop */ }

template<class T>
template<class X>
inline StLorentzVector<T>
StLorentzVector<T>::boost(const StLorentzVector<X>& pframe) const
{
    T mass               = abs(pframe);
    StThreeVector<T> eta = (-1./mass)*pframe.vect();            // gamma*beta
    T gamma              = pframe.e()/mass;
    StThreeVector<T> pl  = ((this->vect()*eta)/(eta*eta))*eta;  // longitudinal momentum
    return StLorentzVector<T>(gamma*this->e() - this->vect()*eta,
                              this->vect() + (gamma-1.)*pl - this->e()*eta);
}

template<class T>
template<class X>
inline void StLorentzVector<T>::setVect(const StThreeVector<X>& v)
{
    StThreeVector<T>::mX1 = (T) v.x();
    StThreeVector<T>::mX2 = (T) v.y();
    StThreeVector<T>::mX3 = (T) v.z();
}

template<class T>
template<class X>
inline StLorentzVector<T>&
StLorentzVector<T>::operator=(const StLorentzVector<X>& vec)
{
    static_cast<StThreeVector<T>&>(*this) = vec.vect();
    mX4 = vec.t();
    return *this;
}

template<class T>
template<class X>
inline bool
StLorentzVector<T>::operator== (const StLorentzVector<X>& v) const
{
    return (this->vect() == v.vect()) && (mX4 == v.t());
}

template<class T>
template<class X>
inline bool
StLorentzVector<T>::operator!= (const StLorentzVector<X>& v) const
{
    return !(*this == v);
}

template<class T>
template<class X>
inline StLorentzVector<T>&
StLorentzVector<T>::operator+= (const StLorentzVector<X>& v)
{
    static_cast<StThreeVector<T>&>(*this) += v.vect();
    mX4 += v.t();
    return *this;
}

template<class T>
template<class X>
inline StLorentzVector<T>&
StLorentzVector<T>::operator-= (const StLorentzVector<X>& v)
{
    static_cast<StThreeVector<T>&>(*this) -= v.vect();
    mX4 -= v.t();
    return *this;
}

#else

template<class T>
inline StLorentzVector<T>::StLorentzVector(const StThreeVector<float> &vec, T t)
	: StThreeVector<T>(vec), mX4(t) { /* nop */ }

template<class T>
inline StLorentzVector<T>::StLorentzVector(const StThreeVector<double> &vec, T t)
	: StThreeVector<T>(vec), mX4(t) { /* nop */ }

template<class T>
inline StLorentzVector<T>::StLorentzVector(const StThreeVector<long double> &vec, T t)
	: StThreeVector<T>(vec), mX4(t) { /* nop */ }

template<class T>
inline StLorentzVector<T>::StLorentzVector(T t, const StThreeVector<float> &vec)
	: StThreeVector<T>(vec), mX4(t) { /* nop */ }

template<class T>
inline StLorentzVector<T>::StLorentzVector(T t, const StThreeVector<double> &vec)
	: StThreeVector<T>(vec), mX4(t) { /* nop */ }

template<class T>
inline StLorentzVector<T>::StLorentzVector(T t, const StThreeVector<long double> &vec)
	: StThreeVector<T>(vec), mX4(t) { /* nop */ }

template<class T>
inline StLorentzVector<T>::StLorentzVector(const StLorentzVector<float> &vec)
	: StThreeVector<T>(vec.vect()), mX4(vec.t()) { /* nop */ }
    
template<class T>
inline StLorentzVector<T>::StLorentzVector(const StLorentzVector<double> &vec)
	: StThreeVector<T>(vec.vect()), mX4(vec.t()) { /* nop */ }
    
template<class T>
inline StLorentzVector<T>::StLorentzVector(const StLorentzVector<long double> &vec)
	: StThreeVector<T>(vec.vect()), mX4(vec.t()) { /* nop */ }

template<class T>
inline StLorentzVector<T>
StLorentzVector<T>::boost(const StLorentzVector<float>& pframe) const
{
    T mass               = abs(pframe);
    StThreeVector<T> eta = (-1./mass)*pframe.vect();            // gamma*beta
    T gamma              = pframe.e()/mass;
    StThreeVector<T> pl  = ((this->vect()*eta)/(eta*eta))*eta;  // longitudinal momentum
    return StLorentzVector<T>(gamma*this->e() - this->vect()*eta,
                              this->vect() + (gamma-1.)*pl - this->e()*eta);
}

template<class T>
inline StLorentzVector<T>
StLorentzVector<T>::boost(const StLorentzVector<double>& pframe) const
{
    T mass               = abs(pframe);
    StThreeVector<T> eta = (-1./mass)*pframe.vect();            // gamma*beta
    T gamma              = pframe.e()/mass;
    StThreeVector<T> pl  = ((this->vect()*eta)/(eta*eta))*eta;  // longitudinal momentum
    return StLorentzVector<T>(gamma*this->e() - this->vect()*eta,
                              this->vect() + (gamma-1.)*pl - this->e()*eta);
}

template<class T>
inline StLorentzVector<T>
StLorentzVector<T>::boost(const StLorentzVector<long double>& pframe) const
{
    T mass               = abs(pframe);
    StThreeVector<T> eta = (-1./mass)*pframe.vect();            // gamma*beta
    T gamma              = pframe.e()/mass;
    StThreeVector<T> pl  = ((this->vect()*eta)/(eta*eta))*eta;  // longitudinal momentum
    return StLorentzVector<T>(gamma*this->e() - this->vect()*eta,
                              this->vect() + (gamma-1.)*pl - this->e()*eta);
}

template<class T>
inline void StLorentzVector<T>::setVect(const StThreeVector<float>& v)
{
    StThreeVector<T>::mX1 = (T) v.x();
    StThreeVector<T>::mX2 = (T) v.y();
    StThreeVector<T>::mX3 = (T) v.z();
}

template<class T>
inline void StLorentzVector<T>::setVect(const StThreeVector<double>& v)
{
    StThreeVector<T>::mX1 = (T) v.x();
    StThreeVector<T>::mX2 = (T) v.y();
    StThreeVector<T>::mX3 = (T) v.z();
}

template<class T>
inline void StLorentzVector<T>::setVect(const StThreeVector<long double>& v)
{
    StThreeVector<T>::mX1 = (T) v.x();
    StThreeVector<T>::mX2 = (T) v.y();
    StThreeVector<T>::mX3 = (T) v.z();
}

template<class T>
inline StLorentzVector<T>&
StLorentzVector<T>::operator=(const StLorentzVector<float>& vec)
{
    static_cast<StThreeVector<T>&>(*this) = vec.vect();
    mX4 = vec.t();
    return *this;
}

template<class T>
inline StLorentzVector<T>&
StLorentzVector<T>::operator=(const StLorentzVector<double>& vec)
{
    static_cast<StThreeVector<T>&>(*this) = vec.vect();
    mX4 = vec.t();
    return *this;
}

template<class T>
inline StLorentzVector<T>&
StLorentzVector<T>::operator=(const StLorentzVector<long double>& vec)
{
    static_cast<StThreeVector<T>&>(*this) = vec.vect();
    mX4 = vec.t();
    return *this;
}

template<class T>
inline bool
StLorentzVector<T>::operator== (const StLorentzVector<float>& v) const
{
    return (this->vect() == v.vect()) && (mX4 == v.t());
}

template<class T>
inline bool
StLorentzVector<T>::operator== (const StLorentzVector<double>& v) const
{
    return (this->vect() == v.vect()) && (mX4 == v.t());
}

template<class T>
inline bool
StLorentzVector<T>::operator== (const StLorentzVector<long double>& v) const
{
    return (this->vect() == v.vect()) && (mX4 == v.t());
}

template<class T>
inline bool
StLorentzVector<T>::operator!= (const StLorentzVector<float>& v) const
{
    return !(*this == v);
}

template<class T>
inline bool
StLorentzVector<T>::operator!= (const StLorentzVector<double>& v) const
{
    return !(*this == v);
}

template<class T>
inline bool
StLorentzVector<T>::operator!= (const StLorentzVector<long double>& v) const
{
    return !(*this == v);
}

template<class T>
inline StLorentzVector<T>&
StLorentzVector<T>::operator+= (const StLorentzVector<float>& v)
{
    static_cast<StThreeVector<T>&>(*this) += v.vect();
    mX4 += v.t();
    return *this;
}

template<class T>
inline StLorentzVector<T>&
StLorentzVector<T>::operator+= (const StLorentzVector<double>& v)
{
    static_cast<StThreeVector<T>&>(*this) += v.vect();
    mX4 += v.t();
    return *this;
}

template<class T>
inline StLorentzVector<T>&
StLorentzVector<T>::operator+= (const StLorentzVector<long double>& v)
{
    static_cast<StThreeVector<T>&>(*this) += v.vect();
    mX4 += v.t();
    return *this;
}

template<class T>
inline StLorentzVector<T>&
StLorentzVector<T>::operator-= (const StLorentzVector<float>& v)
{
    static_cast<StThreeVector<T>&>(*this) -= v.vect();
    mX4 -= v.t();
    return *this;
}

template<class T>
inline StLorentzVector<T>&
StLorentzVector<T>::operator-= (const StLorentzVector<double>& v)
{
    static_cast<StThreeVector<T>&>(*this) -= v.vect();
    mX4 -= v.t();
    return *this;
}

template<class T>
inline StLorentzVector<T>&
StLorentzVector<T>::operator-= (const StLorentzVector<long double>& v)
{
    static_cast<StThreeVector<T>&>(*this) -= v.vect();
    mX4 -= v.t();
    return *this;
}
#endif // ST_NO_MEMBER_TEMPLATES

//
//   Non-member operators
//
template<class T, class X>
inline StLorentzVector<T>
operator+ (const StLorentzVector<T>& v1, const StLorentzVector<X>& v2)
{
    return StLorentzVector<T>(v1) += v2;
}

template<class T, class X>
inline StLorentzVector<T>
operator- (const StLorentzVector<T>& v1, const StLorentzVector<X>& v2)
{
    return StLorentzVector<T>(v1) -= v2;
}

template<class T, class X>
inline T
operator* (const StLorentzVector<T>& v1, const StLorentzVector<X>& v2)
{
    return v1.t()*v2.t() - v1.vect()*v2.vect();
}

template<class T>
inline StLorentzVector<T>
operator* (const StLorentzVector<T>& v, double c)
{
    return StLorentzVector<T>(v) *= c;
}

template<class T>
inline StLorentzVector<T> operator* (double c, const StLorentzVector<T>& v)
{
    return StLorentzVector<T>(v) *= c;
}

template<class T, class X>
inline StLorentzVector<T> operator/ (const StLorentzVector<T>& v, X c)
{
    return StLorentzVector<T>(v) /= c;
}

template<class T>
ostream& operator<< (ostream& os, const StLorentzVector<T>& v)
{
  return os << '(' << v.vect() << ',' << v.t() << ')';
}
#ifdef __HP_aCC
ostream& operator<< (ostream& os, const StLorentzVector<long double>& v)
{
  return os << '(' << v.vect() << ',' << (static_cast<double>(v.t())) << ')';
}
#endif
//
//        Non-member functions
//
template<class T>
inline T abs(const StLorentzVector<T>& v) {return v.m();}

#endif

/***************************************************************************
 *
 * $Id: StLorentzVectorF.hh,v 1.1 1999/01/30 03:59:03 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 * Remarks:   This is a 'handmade' specialisation of StLorentzVector<T>
 *            for T=float. This code contains no templates.
 *
 ***************************************************************************
 *
 * $Log: StLorentzVectorF.hh,v $
 * Revision 1.1  1999/01/30 03:59:03  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.2  1999/06/04 18:01:41  ullrich
 * New operators operator() and operator[] which can be used
 * as lvalues.
 *
 * Revision 1.1  1999/01/30 03:59:03  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:54  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_LORENTZ_VECTOR_F_HH
#define ST_LORENTZ_VECTOR_F_HH

#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#ifdef __ROOT__
#include "TObject.h"
#endif

class StLorentzVectorD;

class StLorentzVectorF 
#ifdef __ROOT__
 : public TObject 
#endif
{
public:
    StLorentzVectorF(float = 0, float = 0, float = 0, float = 0);
    ~StLorentzVectorF();
    
    StLorentzVectorF(const StThreeVectorF&, float);
    StLorentzVectorF(float, const StThreeVectorF&);   
    StLorentzVectorF(const StLorentzVectorF&);
    
    StLorentzVectorF(const StThreeVectorD&, float);
    StLorentzVectorF(float, const StThreeVectorD&);   
    StLorentzVectorF(const StLorentzVectorD&);
        
    StLorentzVectorF& operator=(const StLorentzVectorF&);
    StLorentzVectorF& operator=(const StLorentzVectorD&);
    
    float x()                     const;
    float y()                     const;
    float z()                     const;
    float t()                     const;
    float px()                    const;
    float py()                    const;
    float operator[]  (size_t)    const;
    
    float& operator()  (size_t);
    float& operator[]  (size_t);

    const StThreeVectorF& vect() const;    
    
    void setX(float);
    void setY(float);
    void setZ(float);
    void setPx(float);
    void setPy(float);
    void setPz(float);
    void setE(float);
    void setT(float);
    
    void setVect(const StThreeVectorF&);
    void setVect(const StThreeVectorD&);

    float perp()               const;
    float perp2()              const;
    float pseudoRapidity()     const;
    float phi()                const;
    float theta()              const;
    float cosTheta()           const;
    
    float plus()               const;
    float minus()              const;
    
    float m()                  const; 
    float m2()                 const; 
    float mt()                 const;
    float mt2()                const;
    float rapidity()           const;
    
    StLorentzVectorF boost(const StLorentzVectorF&) const;
    StLorentzVectorF boost(const StLorentzVectorD&) const;
    
    StLorentzVectorF  operator- ();
    StLorentzVectorF  operator+ ();
    StLorentzVectorF& operator*= (double);
    StLorentzVectorF& operator/= (double);

    int operator == (const StLorentzVectorF&) const;
    int operator != (const StLorentzVectorF&) const;
    int operator == (const StLorentzVectorD&) const;
    int operator != (const StLorentzVectorD&) const;

    StLorentzVectorF& operator+= (const StLorentzVectorF&);
    StLorentzVectorF& operator-= (const StLorentzVectorF&);
    StLorentzVectorF& operator+= (const StLorentzVectorD&);
    StLorentzVectorF& operator-= (const StLorentzVectorD&);

protected:
    StThreeVectorF mThreeVector;
    float          mX4;
#ifdef __ROOT__
    ClassDef(StLorentzVectorF,1)
#endif
};

//
//        Declaration of non-member functions and operators
//
inline StLorentzVectorF operator+ (const StLorentzVectorF&, const StLorentzVectorF&);
inline StLorentzVectorF operator- (const StLorentzVectorF&, const StLorentzVectorF&);
inline float            operator* (const StLorentzVectorF&, const StLorentzVectorF&);
inline StLorentzVectorF operator* (const StLorentzVectorF&, double);
inline StLorentzVectorF operator* (double c, const StLorentzVectorF&);
inline StLorentzVectorF operator/ (const StLorentzVectorF&, double);
inline float            abs(const StLorentzVectorF&);
ostream&                operator<< (ostream&, const StLorentzVectorF&);

//
//        Implementation of member functions
//
inline StLorentzVectorF::StLorentzVectorF(float x, float y, float z, float t)
    : mThreeVector(x, y, z), mX4(t) { /* nop */ }

inline StLorentzVectorF::~StLorentzVectorF() { /* nopt */ }    

inline const StThreeVectorF& StLorentzVectorF::vect() const 
{
    return mThreeVector;
}

inline float StLorentzVectorF::m2() const
{
    return (mX4*mX4 - mThreeVector*mThreeVector);    
}

inline float StLorentzVectorF::plus() const { return (e() + pz()); }

inline float StLorentzVectorF::minus() const { return (e() - pz()); }

inline float StLorentzVectorF::m() const
{
    float mass2 = m2();
    if (mass2 < 0)
	return -sqrt(-mass2);
    else
	return sqrt(mass2);
}

inline float StLorentzVectorF::mt2() const
{
    return this->perp2() + m2();
}

inline float StLorentzVectorF::mt() const
{
    //
    // change to more optimal code ?
    // return e()*e() - pz()*pz();
    float massPerp2 = mt2();
    if (massPerp2 < 0)
	return -sqrt(-massPerp2);
    else
	return sqrt(massPerp2);
}

inline void StLorentzVectorF::setPx(float x) {mThreeVector.setX(x);}

inline void StLorentzVectorF::setPy(float y) {mThreeVector.setY(y);}

inline void StLorentzVectorF::setPz(float z) {mThreeVector.setZ(z);}

inline void StLorentzVectorF::setX(float x) {mThreeVector.setX(x);}

inline void StLorentzVectorF::setY(float y) {mThreeVector.setY(y);}

inline void StLorentzVectorF::setZ(float z) {mThreeVector.setZ(z);}

inline void StLorentzVectorF::setT(float t) {mX4 = t;}

inline void StLorentzVectorF::setE(float e) {mX4 = e;}

inline float StLorentzVectorF::x() const {return mThreeVector.x();}

inline float StLorentzVectorF::y() const {return mThreeVector.y();}

inline float StLorentzVectorF::z() const {return mThreeVector.z();}

inline float StLorentzVectorF::px() const {return mThreeVector.x();}

inline float StLorentzVectorF::py() const {return mThreeVector.y();}

inline float StLorentzVectorF::pz() const {return mThreeVector.z();}

inline float StLorentzVectorF::e() const {return mX4;}

inline float StLorentzVectorF::t() const {return mX4;}

inline float StLorentzVectorF::perp() const {return mThreeVector.perp();}

inline float StLorentzVectorF::perp2() const {return mThreeVector.perp2();}

inline float StLorentzVectorF::pseudoRapidity() const {return mThreeVector.pseudoRapidity();}

inline float StLorentzVectorF::phi() const {return mThreeVector.phi();}

inline float StLorentzVectorF::theta() const {return mThreeVector.theta();}

inline float StLorentzVectorF::cosTheta() const {return mThreeVector.cosTheta();}

inline float StLorentzVectorF::operator() (size_t i) const
{
    if (i < 3)
        return mThreeVector(i);
    else if (i == 3)
        return mX4;
    else {
      cerr << "StLorentzVectorF::operator(): bad index." << endl;
      return mX4;
    }
}

inline float StLorentzVectorF::operator[] (size_t i) const
{
    if (i < 3)
        return mThreeVector[i];
    else if (i == 3)
        return mX4;
    else {
      cerr << "StLorentzVectorF::operator[]: bad index." << endl;
      return mX4;
    }
}

inline float StLorentzVectorF::rapidity() const
{
    return 0.5*log((mX4+mThreeVector.z())/(mX4-mThreeVector.z()));
}

inline StLorentzVectorF StLorentzVectorF::operator- ()
{
    return StLorentzVectorF(-mX4,-mThreeVector);
}

inline StLorentzVectorF StLorentzVectorF::operator+ ()
{
    return *this;
}

inline StLorentzVectorF& StLorentzVectorF::operator*= (double c)
{
    mThreeVector *= c;
    mX4 *= c;
    return *this;
}

inline StLorentzVectorF& StLorentzVectorF::operator/= (double c)
{
    mThreeVector /= c;
    mX4 /= c;
    return *this;
}

inline StLorentzVectorF::StLorentzVectorF(const StThreeVectorF &vec, float t)
	: mThreeVector(vec), mX4(t) { /* nop */ }

inline StLorentzVectorF::StLorentzVectorF(const StThreeVectorD &vec, float t)
	: mThreeVector(vec), mX4(t) { /* nop */ }

inline StLorentzVectorF::StLorentzVectorF(float t, const StThreeVectorF &vec)
	: mThreeVector(vec), mX4(t) { /* nop */ }

inline StLorentzVectorF::StLorentzVectorF(float t, const StThreeVectorD &vec)
	: mThreeVector(vec), mX4(t) { /* nop */ }

inline StLorentzVectorF::StLorentzVectorF(const StLorentzVectorF &vec)
	: mThreeVector(vec.vect()), mX4(vec.t()) { /* nop */ }
    
inline void StLorentzVectorF::setVect(const StThreeVectorF& v)
{
    mThreeVector = v;
}

inline void StLorentzVectorF::setVect(const StThreeVectorD& v)
{
    mThreeVector = v;
}

inline StLorentzVectorF&
StLorentzVectorF::operator=(const StLorentzVectorF& vec)
{
    mThreeVector = vec.vect();
    mX4 = vec.t();
    return *this;
}

inline int
StLorentzVectorF::operator== (const StLorentzVectorF& v) const
{
    return (this->vect() == v.vect()) && (mX4 == v.t());
}

inline int
StLorentzVectorF::operator!= (const StLorentzVectorF& v) const
{
    return !(*this == v);
}

inline StLorentzVectorF&
StLorentzVectorF::operator+= (const StLorentzVectorF& v)
{
    mThreeVector += v.vect();
    mX4 += v.t();
    return *this;
}

inline StLorentzVectorF&
StLorentzVectorF::operator-= (const StLorentzVectorF& v)
{
    mThreeVector -= v.vect();
    mX4 -= v.t();
    return *this;
}

//
//   Non-member operators
//
inline StLorentzVectorF
operator+ (const StLorentzVectorF& v1, const StLorentzVectorF& v2)
{
    return StLorentzVectorF(v1) += v2;
}

inline StLorentzVectorF
operator- (const StLorentzVectorF& v1, const StLorentzVectorF& v2)
{
    return StLorentzVectorF(v1) -= v2;
}

inline float
operator* (const StLorentzVectorF& v1, const StLorentzVectorF& v2)
{
    return v1.t()*v2.t() - v1.vect()*v2.vect();
}

inline StLorentzVectorF
operator* (const StLorentzVectorF& v, double c)
{
    return StLorentzVectorF(v) *= c;
}

inline StLorentzVectorF operator* (double c, const StLorentzVectorF& v)
{
    return StLorentzVectorF(v) *= c;
}

inline StLorentzVectorF operator/ (const StLorentzVectorF& v, double c)
{
    return StLorentzVectorF(v) /= c;
}

//
//        Non-member functions
//
inline float abs(const StLorentzVectorF& v) {return v.m();}

#endif

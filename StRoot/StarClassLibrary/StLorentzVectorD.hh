/***************************************************************************
 *
 * $Id: StLorentzVectorD.hh,v 1.2 1999/06/04 18:01:38 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 * Remarks:   This is a 'handmade' specialisation of StLorentzVector<T>
 *            for T=double. This code contains no templates.
 *
 ***************************************************************************
 *
 * $Log: StLorentzVectorD.hh,v $
 * Revision 1.2  1999/06/04 18:01:38  ullrich
 * New operators operator() and operator[] which can be used
 * as lvalues.
 *
 * Revision 1.2  1999/06/04 18:01:38  ullrich
 * New operators operator() and operator[] which can be used
 * as lvalues.
 *
 * Revision 1.1  1999/01/30 03:59:03  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.2  1999/01/23 01:19:31  ullrich
 * Inline decleration of non-inlined function removed.
 *
 * Revision 1.1  1999/01/23 00:27:53  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_LORENTZ_VECTOR_D_HH
#define ST_LORENTZ_VECTOR_D_HH

#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#ifdef __ROOT__
#include "TObject.h"
#endif

class StLorentzVectorF;

class StLorentzVectorD 
#ifdef __ROOT__
 : public TObject 
#endif
{
public:
    StLorentzVectorD(double = 0, double = 0, double = 0, double = 0);
    ~StLorentzVectorD();
    
    StLorentzVectorD(const StThreeVectorD&, double);
    StLorentzVectorD(double, const StThreeVectorD&);   
    StLorentzVectorD(const StLorentzVectorD&);
    
    StLorentzVectorD(const StThreeVectorF&, double);
    StLorentzVectorD(double, const StThreeVectorF&);   
    StLorentzVectorD(const StLorentzVectorF&);
        
    StLorentzVectorD& operator=(const StLorentzVectorD&);
    StLorentzVectorD& operator=(const StLorentzVectorF&);
    
    double x()                     const;
    double y()                     const;
    double z()                     const;
    double t()                     const;
    double px()                    const;
    double py()                    const;
    double pz()                    const;
    double e()                     const;
    double operator()  (size_t)    const;
    double operator[]  (size_t)    const;
    
    double& operator()  (size_t);
    double& operator[]  (size_t);

    const StThreeVectorD& vect() const;    
    
    void setX(double);
    void setY(double);
    void setZ(double);
    void setPx(double);
    void setPy(double);
    void setPz(double);
    void setE(double);
    void setT(double);
    
    void setVect(const StThreeVectorD&);
    void setVect(const StThreeVectorF&);

    double perp()               const;
    double perp2()              const;
    double pseudoRapidity()     const;
    double phi()                const;
    double theta()              const;
    double cosTheta()           const;
    
    double plus()               const;
    double minus()              const;
    
    double m()                  const; 
    double m2()                 const; 
    double mt()                 const;
    double mt2()                const;
    double rapidity()           const;
    
    StLorentzVectorD boost(const StLorentzVectorD&) const;
    StLorentzVectorD boost(const StLorentzVectorF&) const;
    
    StLorentzVectorD  operator- ();
    StLorentzVectorD  operator+ ();
    StLorentzVectorD& operator*= (double);
    StLorentzVectorD& operator/= (double);

    int operator == (const StLorentzVectorD&) const;
    int operator != (const StLorentzVectorD&) const;
    int operator == (const StLorentzVectorF&) const;
    int operator != (const StLorentzVectorF&) const;

    StLorentzVectorD& operator+= (const StLorentzVectorD&);
    StLorentzVectorD& operator-= (const StLorentzVectorD&);
    StLorentzVectorD& operator+= (const StLorentzVectorF&);
    StLorentzVectorD& operator-= (const StLorentzVectorF&);

protected:
    StThreeVectorD mThreeVector;
    double         mX4;
#ifdef __ROOT__
    ClassDef(StLorentzVectorD,1)
#endif
};

//
//        Declaration of non-member functions and operators
//
inline StLorentzVectorD operator+ (const StLorentzVectorD&, const StLorentzVectorD&);
inline StLorentzVectorD operator+ (const StLorentzVectorF&, const StLorentzVectorD&);
inline StLorentzVectorD operator+ (const StLorentzVectorD&, const StLorentzVectorF&);
inline StLorentzVectorD operator- (const StLorentzVectorD&, const StLorentzVectorD&);
inline StLorentzVectorD operator- (const StLorentzVectorF&, const StLorentzVectorD&);
inline StLorentzVectorD operator- (const StLorentzVectorD&, const StLorentzVectorF&);
inline double           operator* (const StLorentzVectorD&, const StLorentzVectorD&);
double                  operator* (const StLorentzVectorF&, const StLorentzVectorD&);
double                  operator* (const StLorentzVectorD&, const StLorentzVectorF&);
inline StLorentzVectorD operator* (const StLorentzVectorD&, double);
inline StLorentzVectorD operator* (double c, const StLorentzVectorD&);
inline StLorentzVectorD operator/ (const StLorentzVectorD&, double);
inline double           abs(const StLorentzVectorD&);
ostream&                operator<< (ostream&, const StLorentzVectorD&);

//
//        Implementation of member functions
//
inline StLorentzVectorD::StLorentzVectorD(double x, double y, double z, double t)
    : mThreeVector(x, y, z), mX4(t) { /* nop */ }

inline StLorentzVectorD::~StLorentzVectorD() { /* nopt */ }    

inline const StThreeVectorD& StLorentzVectorD::vect() const 
{
    return mThreeVector;
}

inline double StLorentzVectorD::m2() const
{
    return (mX4*mX4 - mThreeVector*mThreeVector);    
}

inline double StLorentzVectorD::plus() const { return (e() + pz()); }

inline double StLorentzVectorD::minus() const { return (e() - pz()); }

inline double StLorentzVectorD::m() const
{
    double mass2 = m2();
    if (mass2 < 0)
	return -sqrt(-mass2);
    else
	return sqrt(mass2);
}

inline double StLorentzVectorD::mt2() const
{
    return this->perp2() + m2();
}

inline double StLorentzVectorD::mt() const
{
    //
    // change to more optimal code ?
    // return e()*e() - pz()*pz();
    double massPerp2 = mt2();
    if (massPerp2 < 0)
	return -sqrt(-massPerp2);
    else
	return sqrt(massPerp2);
}

inline void StLorentzVectorD::setPx(double x) {mThreeVector.setX(x);}

inline void StLorentzVectorD::setPy(double y) {mThreeVector.setY(y);}

inline void StLorentzVectorD::setPz(double z) {mThreeVector.setZ(z);}

inline void StLorentzVectorD::setX(double x) {mThreeVector.setX(x);}

inline void StLorentzVectorD::setY(double y) {mThreeVector.setY(y);}

inline void StLorentzVectorD::setZ(double z) {mThreeVector.setZ(z);}

inline void StLorentzVectorD::setT(double t) {mX4 = t;}

inline void StLorentzVectorD::setE(double e) {mX4 = e;}

inline double StLorentzVectorD::x() const {return mThreeVector.x();}

inline double StLorentzVectorD::y() const {return mThreeVector.y();}

inline double StLorentzVectorD::z() const {return mThreeVector.z();}

inline double StLorentzVectorD::px() const {return mThreeVector.x();}

inline double StLorentzVectorD::py() const {return mThreeVector.y();}

inline double StLorentzVectorD::pz() const {return mThreeVector.z();}

inline double StLorentzVectorD::e() const {return mX4;}

inline double StLorentzVectorD::t() const {return mX4;}

inline double StLorentzVectorD::perp() const {return mThreeVector.perp();}

inline double StLorentzVectorD::perp2() const {return mThreeVector.perp2();}

inline double StLorentzVectorD::pseudoRapidity() const {return mThreeVector.pseudoRapidity();}

inline double StLorentzVectorD::phi() const {return mThreeVector.phi();}

inline double StLorentzVectorD::theta() const {return mThreeVector.theta();}

inline double StLorentzVectorD::cosTheta() const {return mThreeVector.cosTheta();}

inline double StLorentzVectorD::operator() (size_t i) const
{
    if (i < 3)
        return mThreeVector(i);
    else if (i == 3)
        return mX4;
    else {
      cerr << "StLorentzVectorD::operator(): bad index." << endl;
      return 0;
    }
}

inline double& StLorentzVectorD::operator() (size_t i)
{
    if (i < 3)
        return mThreeVector(i);
    else if (i == 3)
        return mX4;
    else {
      cerr << "StLorentzVectorD::operator(): bad index." << endl;
      return mX4;
    }
}

inline double StLorentzVectorD::operator[] (size_t i) const
{
    if (i < 3)
        return mThreeVector[i];
    else if (i == 3)
        return mX4;
    else {
      cerr << "StLorentzVectorD::operator[]: bad index." << endl;
      return 0;
    }
}

inline double& StLorentzVectorD::operator[] (size_t i)
{
    if (i < 3)
        return mThreeVector[i];
    else if (i == 3)
        return mX4;
    else {
      cerr << "StLorentzVectorD::operator[]: bad index." << endl;
      return mX4;
    }
}

inline double StLorentzVectorD::rapidity() const
{
    return 0.5*log((mX4+mThreeVector.z())/(mX4-mThreeVector.z()));
}

inline StLorentzVectorD StLorentzVectorD::operator- ()
{
    return StLorentzVectorD(-mX4,-mThreeVector);
}

inline StLorentzVectorD StLorentzVectorD::operator+ ()
{
    return *this;
}

inline StLorentzVectorD& StLorentzVectorD::operator*= (double c)
{
    mThreeVector *= c;
    mX4 *= c;
    return *this;
}

inline StLorentzVectorD& StLorentzVectorD::operator/= (double c)
{
    mThreeVector /= c;
    mX4 /= c;
    return *this;
}

inline StLorentzVectorD::StLorentzVectorD(const StThreeVectorD &vec, double t)
	: mThreeVector(vec), mX4(t) { /* nop */ }

inline StLorentzVectorD::StLorentzVectorD(const StThreeVectorF &vec, double t)
	: mThreeVector(vec), mX4(t) { /* nop */ }

inline StLorentzVectorD::StLorentzVectorD(double t, const StThreeVectorD &vec)
	: mThreeVector(vec), mX4(t) { /* nop */ }

inline StLorentzVectorD::StLorentzVectorD(double t, const StThreeVectorF &vec)
	: mThreeVector(vec), mX4(t) { /* nop */ }

inline StLorentzVectorD::StLorentzVectorD(const StLorentzVectorD &vec)
	: mThreeVector(vec.vect()), mX4(vec.t()) { /* nop */ }
    
inline void StLorentzVectorD::setVect(const StThreeVectorD& v)
{
    mThreeVector = v;
}

inline void StLorentzVectorD::setVect(const StThreeVectorF& v)
{
    mThreeVector = v;
}

inline StLorentzVectorD&
StLorentzVectorD::operator=(const StLorentzVectorD& vec)
{
    mThreeVector = vec.vect();
    mX4 = vec.t();
    return *this;
}

inline int
StLorentzVectorD::operator== (const StLorentzVectorD& v) const
{
    return (this->vect() == v.vect()) && (mX4 == v.t());
}

inline int
StLorentzVectorD::operator!= (const StLorentzVectorD& v) const
{
    return !(*this == v);
}

inline StLorentzVectorD&
StLorentzVectorD::operator+= (const StLorentzVectorD& v)
{
    mThreeVector += v.vect();
    mX4 += v.t();
    return *this;
}

inline StLorentzVectorD&
StLorentzVectorD::operator-= (const StLorentzVectorD& v)
{
    mThreeVector -= v.vect();
    mX4 -= v.t();
    return *this;
}

//
//   Non-member operators
//
inline StLorentzVectorD
operator+ (const StLorentzVectorD& v1, const StLorentzVectorD& v2)
{
    return StLorentzVectorD(v1) += v2;
}

inline StLorentzVectorD
operator+ (const StLorentzVectorF& v1, const StLorentzVectorD& v2)
{
    return StLorentzVectorD(v1) += v2;
}

inline StLorentzVectorD
operator+ (const StLorentzVectorD& v1, const StLorentzVectorF& v2)
{
    return StLorentzVectorD(v1) += v2;
}

inline StLorentzVectorD
operator- (const StLorentzVectorD& v1, const StLorentzVectorD& v2)
{
    return StLorentzVectorD(v1) -= v2;
}

inline StLorentzVectorD
operator- (const StLorentzVectorF& v1, const StLorentzVectorD& v2)
{
    return StLorentzVectorD(v1) -= v2;
}

inline StLorentzVectorD
operator- (const StLorentzVectorD& v1, const StLorentzVectorF& v2)
{
    return StLorentzVectorD(v1) -= v2;
}

inline double
operator* (const StLorentzVectorD& v1, const StLorentzVectorD& v2)
{
    return v1.t()*v2.t() - v1.vect()*v2.vect();
}

inline StLorentzVectorD
operator* (const StLorentzVectorD& v, double c)
{
    return StLorentzVectorD(v) *= c;
}

inline StLorentzVectorD operator* (double c, const StLorentzVectorD& v)
{
    return StLorentzVectorD(v) *= c;
}

inline StLorentzVectorD operator/ (const StLorentzVectorD& v, double c)
{
    return StLorentzVectorD(v) /= c;
}

//
//        Non-member functions
//
inline double abs(const StLorentzVectorD& v) {return v.m();}

#endif

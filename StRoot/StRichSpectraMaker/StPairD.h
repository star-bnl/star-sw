/***************************************************************************
 *
 * $Id: StPairD.h,v 1.1 2001/02/25 22:11:20 lasiuk Exp $
 *
 * Author:  bl Feb 21, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              two-vector in cartesian plane
 ***************************************************************************
 *
 * $Log: StPairD.h,v $
 * Revision 1.1  2001/02/25 22:11:20  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>

#ifdef __ROOT__
#include "TObject.h"
#endif
    
class StPairD  
#ifdef __ROOT__
    : public TObject
#endif
{
public:    
    StPairD(double = 0, double = 0);
    StPairD(const double*);
    ~StPairD();

    StPairD(const StPairD&);
    StPairD& operator=(const StPairD&);
    
    void setX(double);
    void setY(double);

    void setTheta(double);
    void setMag(double);
    void setMagnitude(double);
    
    double   x()                        const;
    double   y()                        const;

    double   l()                        const;
    double   z()                        const;
    
    double   theta()                    const;
    double   cosTheta()                 const;
    double   magnitude()                const;
    double   mag()                      const;
    double   mag2()                     const;
    double   operator() (size_t)        const;
    double   operator[] (size_t)        const;
    double&  operator() (size_t);
    double&  operator[] (size_t);

    StPairD  unit()       const;

    void  rotate(double);
    
    StPairD  operator- ();
    StPairD  operator+ ();
    StPairD& operator*= (double);
    StPairD& operator/= (double);
 
    double         angle(const StPairD&) const;
      
    int operator == (const StPairD& v) const;
    int operator != (const StPairD& v) const;
    StPairD& operator+= (const StPairD&);
    StPairD& operator-= (const StPairD&);

protected:
    double    mX1, mX2;
#ifdef __ROOT__
    ClassDef(StPairD,1)
#endif
};

//
//        Declaration of extern non-member functions and operators
//
ostream&              operator<<(ostream&, const StPairD&);
istream&              operator>>(istream&, StPairD&);
double         abs(const StPairD&);
StPairD operator+ (const StPairD&, const StPairD&);
StPairD operator- (const StPairD&, const StPairD&);
double         operator* (const StPairD&, const StPairD&);
StPairD operator* (const StPairD&, double);
StPairD operator* (double, const StPairD&);
StPairD operator/ (const StPairD&, double);                        

//
//        Implementation of member functions
//

inline void StPairD::setX(double x) {mX1 = x;}

inline void StPairD::setY(double y) {mX2 = y;}


inline void StPairD::setMagnitude(double th) { this->setMag(th); }

inline double StPairD::x() const {return mX1;}

inline double StPairD::y() const {return mX2;}

inline double StPairD::l() const {return mX1;}

inline double StPairD::z() const {return mX2;}

inline double StPairD::theta() const {return atan2(mX2,mX1);}

inline double StPairD::cosTheta() const {return (mX1/this->mag());}

inline double StPairD::magnitude() const
{
    return mag();
}

inline double StPairD::mag() const
{
    return sqrt(mX1*mX1+mX2*mX2);
}

inline double StPairD::mag2() const
{
    return mX1*mX1+mX2*mX2;
}

inline double StPairD::operator() (size_t i) const
{
    if (i == 0)
        return mX1;
    else if (i == 1)
        return mX2;
    else {
      cerr << "StPairD::operator(): bad index" << endl;
      return 0;
    }
}
inline double& StPairD::operator() (size_t i)
{
    if (i == 0)
        return mX1;
    else if (i == 1)
        return mX2;
    else {
      cerr << "StPairD::operator(): bad index" << endl;
      return mX2;
    }
}

inline double StPairD::operator[] (size_t i) const
{
    if (i == 0)
        return mX1;
    else if (i == 1)
        return mX2;
    else {
      cerr << "StPairD::operator[]: bad index" << endl;
      return 0;
    }
}

inline double& StPairD::operator[] (size_t i)
{
    if (i == 0)
        return mX1;
    else if (i == 1)
        return mX2;
    else {
      cerr << "StPairD::operator[]: bad index" << endl;
      return mX2;
    }
}

inline StPairD StPairD::unit() const
{
    return *this/this->mag();
}

inline StPairD StPairD::operator- ()
{
    return StPairD(-mX1, -mX2);
}

inline StPairD StPairD::operator+ ()
{
    return *this;
}

inline StPairD& StPairD::operator*= (double c)
{
    mX1 *= c; mX2 *= c;
    return *this;
}

inline StPairD& StPairD::operator/= (double c)
{
    mX1 /= c; mX2 /= c;
    return *this;
}

inline int
StPairD::operator== (const StPairD& v) const
{
    return mX1 == v.x() && mX2 == v.y();
}

inline int 
StPairD::operator!= (const StPairD& v) const
{
    return !(*this == v);
}

inline StPairD&
StPairD::operator+= (const StPairD& v)
{
    mX1 += v.x(); mX2 += v.y();
    return *this;
}

inline StPairD&
StPairD::operator-= (const StPairD& v)
{
    mX1 -= v.x(); mX2 -= v.y();
    return *this;
}

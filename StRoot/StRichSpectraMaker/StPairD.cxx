/***************************************************************************
 *
 * $Id: StPairD.cxx,v 1.1 2001/02/25 22:11:11 lasiuk Exp $
 *
 * Author:  bl Feb 21, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              two-vector in cartesian plane
 ***************************************************************************
 *
 * $Log: StPairD.cxx,v $
 * Revision 1.1  2001/02/25 22:11:11  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#include "StPairD.h"

#ifdef __ROOT__
ClassImp(StPairD)
#endif

StPairD::StPairD(double x, double y)
    : mX1(x), mX2(y) {/* nopt */}

StPairD::StPairD(const double *x)
{
    mX1 = x[0];
    mX2 = x[1];
}

StPairD::~StPairD() {/* nopt */}

StPairD::StPairD(const StPairD& p)
    : mX1(p.x()), mX2(p.y()) {/* nopt */}

StPairD& StPairD::operator=(const StPairD& a)
{
    mX1 = a.x();
    mX2 = a.y();
    return *this;
}

void StPairD::setTheta(double th) {

    double r = this->mag();

    mX1 = r*sin(th);
    mX2 = r*cos(th);
}

void StPairD::setMag(double mag) {

    double theta = this->theta();

    mX1 = mag*sin(theta);
    mX2 = mag*cos(theta);
}

//
// not inline functions
void  StPairD::rotate(double theta)
{
    // create the rotation matrix:
    //
    //  (cos a   -sin a)(x)  (x')
    //  (sin a    cos a)(y)  (y')
    //

    //
    // this is a rotation in the normal sense:
    //
    //   |
    //   |  <-
    //   | a  \ 
    //   |_____|__
    double cosTheta = cos(theta);
    double sinTheta = sin(theta);

    double xPrime = cosTheta*mX1 - sinTheta*mX2;
    double yPrime = sinTheta*mX1 + cosTheta*mX2;

    mX1 = xPrime;
    mX2 = yPrime;
}

//
//        Non-member functions
//
inline double abs(const StPairD& v) {return v.mag();}

//
//        Non-member operators
//
StPairD
operator+ (const StPairD& v1, const StPairD& v2)
{
    return StPairD(v1) += v2;
}

StPairD
operator- (const StPairD& v1, const StPairD& v2)
{
    return StPairD(v1) -= v2;
}

StPairD operator* (const StPairD& v, double c)
{
    return StPairD(v) *= c;
}

StPairD operator* (double c, const StPairD& v)
{
    return StPairD(v) *= c;
}

StPairD operator/ (const StPairD& v, double c)
{
    return StPairD(v) /= c;
}

ostream& operator<<(ostream& os, const StPairD& v) {
    return (os << '(' << v.x() << ", " << v.y() << ')');
}
istream& operator>>(istream& is, StPairD& v) {
    double x,y;
    
    is >> x >> y;
    v.setX(x);
    v.setY(y);
    return is;
}

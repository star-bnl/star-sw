/***************************************************************************
 *
 * $Id: StAngle.hh,v 1.1 2001/04/20 04:54:26 ullrich Exp $
 *
 * Author: Thomas Ullrich, April 2001
 ***************************************************************************
 *
 * Description:  
 * Keeps an angle within the range of [-pi, pi] and deals with differences
 * between angles in the right way. Units are radian.
 * Other than that it can be handled pretty much like a 'double'.
 *
 ***************************************************************************
 *
 * $Log: StAngle.hh,v $
 * Revision 1.1  2001/04/20 04:54:26  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StAngle_hh
#define StAngle_hh

#include <cmath>
#include <float.h>

class StAngle {
public:
    StAngle();
    StAngle(const StAngle& val);   
    StAngle(double);
    
    double degree();  // range [0-360]

    operator double() const { return phi; }
    
    StAngle operator= (double);
    StAngle operator+= (StAngle);
    StAngle operator-= (StAngle);
    StAngle operator*= (double);
    StAngle operator/= (double);
    int     operator== (const StAngle&) const;
    
    friend StAngle operator+ (StAngle, StAngle);
    friend StAngle operator- (StAngle, StAngle);
    friend StAngle operator* (StAngle, double);
    friend StAngle operator/ (StAngle, double);
    
    friend StAngle average(StAngle, StAngle);
    
private: 
    double phi;
};

//
// Constructors for angles
//

inline StAngle::StAngle() 
{
   phi = 0.;   // default: set to 0
}

inline StAngle::StAngle(const StAngle& val) 
{
   phi = val.phi;
}

inline StAngle::StAngle(double val) 
{
   phi = val;
   if (fabs(phi) > M_PI) phi = atan2(sin(phi), cos(phi));
}

//
// Here follow the member functions for the operators ==, !=, =, +=, -=, *=, /=
//

inline StAngle StAngle::operator= (double val) 
{
   phi = val;
   if (fabs(phi) > M_PI) phi = atan2(sin(phi), cos(phi));
   return *this;
}

inline StAngle StAngle::operator+= (StAngle val) 
{
   phi += val.phi;
   if (fabs(phi) > M_PI) phi = atan2(sin(phi), cos(phi));
   return *this;
}

inline StAngle StAngle::operator-= (StAngle val) 
{
   phi -= val.phi;
   if (fabs(phi) > M_PI) phi = atan2(sin(phi), cos(phi));
   return *this;
}

inline StAngle StAngle::operator*= (double val) 
{
   phi *= val;
   if (fabs(phi) > M_PI) phi = atan2(sin(phi), cos(phi));
   return *this;
}

inline StAngle StAngle::operator/= (double val) 
{
   phi /= val;
   if (fabs(phi) > M_PI) phi = atan2(sin(phi), cos(phi));
   return *this;
}

inline int StAngle::operator== (const StAngle& a) const
{
   return (fabs(StAngle(phi-a.phi)) < FLT_EPSILON);
}

//
// The friends for operator +, -, *, /
//

inline StAngle operator+ (StAngle angle, StAngle val) 
{
   double res = angle.phi + val.phi;
   if (fabs(res) > M_PI) res = atan2(sin(res), cos(res));
   return res;
}

inline StAngle operator- (StAngle angle, StAngle val) 
{
   double res = angle.phi - val.phi;
   if (fabs(res) > M_PI) res = atan2(sin(res), cos(res));
   return res;
}

inline StAngle operator* (StAngle angle, double val) 
{
   double res = angle.phi * val;
   if (fabs(res) > M_PI) res = atan2(sin(res), cos(res));
   return res;
}

inline StAngle operator/ (StAngle angle, double val) 
{
   double res = angle.phi / val;
   if (fabs(res) > M_PI) res = atan2(sin(res), cos(res));
   return res;
}

inline StAngle average(StAngle a, StAngle b)
{
   return a+(b-a)/2.;
}

//
//  Return angle in degrees [0, 360]
//

inline double StAngle::degree()
{
   double val = phi*180./M_PI;  
   return (val < 0) ? val += 360 : val;
}


#endif

#ifndef DOUBLEAC_H
#define DOUBLEAC_H
/*
DoubleAc is abbreviation of double with accuracy.
Really this is original implementation of interval computations.
This is the way of watching for the numerical errors
by establishing lower and upper limit of each numerical value.
The central the most probable value is also watched for.


Copyright (c) 2001 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text 
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

//#include <math.h>  // at S. Linux 4.1 gives errors like 
// DoubleAc.c:534: error: `sqrt' is not a member of `std'
#ifdef VISUAL_STUDIO
#define _USE_MATH_DEFINES
// see comment in math.h:
/* Define _USE_MATH_DEFINES before including math.h to expose these macro
 * definitions for common math constants.  These are placed under an #ifdef
 * since these commonly-defined names are not part of the C/C++ standards.
 */
#endif
#include <cmath>
//#include <values.h>
#include <climits>
#include <cfloat>
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/stream/prstream.h"
#include "wcpplib/math/minmax.h"

#define DEF_DBL_PREC 1.0e-15   // rounding precision for double
const double one_plus_def_dbl_prec=double(1.0)+DEF_DBL_PREC;
const double one_minus_def_dbl_prec=double(1.0)-DEF_DBL_PREC;

#define DEF_FLT_PREC 1.0e-7   // rounding precision for float
const double one_plus_def_flt_prec=double(1.0)+DEF_FLT_PREC;
const double one_minus_def_flt_prec=double(1.0)-DEF_FLT_PREC;

//#define CHECK_CORRECTNESS_AT_MULT // ..tiplication
/* strange but at Scientific Linux 4.0 with compilation with -O2
multiplication of two numbers 
  DoubleAc a1(-5.134443206949733e-197, -5.134443206949745e-197, 
	      -5.134443206949721e-197);
  DoubleAc a2(4.237289132339917e-176, 4.237289132339908e-176, 
	      4.237289132339927e-176);
  DoubleAc a3 = a1 * a2;
gives error diagnistic:
DoubleAc& DoubleAc::operator*=(const DoubleAc& f): ERROR:
d  <  di at the end of computations
This number:
DoubleAc: d=                  -0 di=                  -0 da=                  -0
Argument:
DoubleAc: d=4.237289132339917e-176 di=4.237289132339908e-176 da=4.237289132339927e-176
if(d == di) is also positive
d - di=0
old value:
DoubleAc: d=-5.134443206949733e-197 di=-5.134443206949745e-197 da=-5.134443206949721e-197
FunNameStack: s_init=1 qname=2
  0  main() 
  1  DoubleAc& DoubleAc::operator*=(const DoubleAc& f) 
File is DoubleAc.c , line number is 214
spexit_action: the streams will be now flushed
spexit_action: the abort function is called
Abort (core dumped)

Intersting that if compiled without optimization, this code works
correctly.

It can be assumed that it is due to some transitions to and from 
internal registers of Intel arithmetic co-processor with 
larger internal precision (80 bits?)
with rounding at backward transition.

There is currently no solution except either compiling without -O2
or switching out control at multiplication of DoubleAc by the macro above.

The same problem and the same macro are applied to division as well.
*/
 

class DoubleAc
{
  double d;   // the main  value
  double di;  // the left limit   di <= d
  double da;  // the right limit  da >= d
public:
  inline DoubleAc(void){di=0.0; d=0.0; da=0.0;}
  inline DoubleAc(const DoubleAc& f)
    {di=f.di; d=f.d; da=f.da;}
  inline DoubleAc(double f);
  inline DoubleAc(float f);
  inline DoubleAc(long f);
  inline DoubleAc(int f);
  DoubleAc(double f, double ffmin, double ffmax); 
  DoubleAc(double f, double relative_prec); 
  inline DoubleAc& operator=(const DoubleAc& f) 
  {d = f.d; di=f.di; da=f.da; return *this; }
  inline DoubleAc& operator=(double f);
  inline DoubleAc& operator=(float f);
  inline DoubleAc& operator=(long f);
  inline DoubleAc& operator=(int f);
  inline operator double(void) const {return d; }
  inline double get(void) const {return d;} 
  inline double get_low_limit(void) const {return di;} 
  inline double get_left_limit(void) const {return di;} 
  inline double left_limit(void) const {return di;} 
  inline double get_min_limit(void) const {return di;} 
  inline double get_high_limit(void) const {return da;} 
  inline double get_right_limit(void) const {return da;} 
  inline double right_limit(void) const {return da;} 
  inline double get_max_limit(void) const {return da;} 
  inline double get_accuracy(void) const  
    {
      return 0.5*(da-di);
    }
  inline DoubleAc& operator+=(const DoubleAc& f);
  inline DoubleAc& operator+=(double f);
  inline DoubleAc& operator+=(float f);
  inline DoubleAc& operator+=(long f);
  inline DoubleAc& operator+=(int f);

  inline DoubleAc& operator-=(const DoubleAc& f);
  inline DoubleAc& operator-=(double f);
  inline DoubleAc& operator-=(float f);
  inline DoubleAc& operator-=(long f);
  inline DoubleAc& operator-=(int f);

  friend inline void change_sign(DoubleAc& f);

  DoubleAc& operator*=(DoubleAc f);
  inline DoubleAc& operator*=(double f);
  inline DoubleAc& operator*=(float f);
  inline DoubleAc& operator*=(long f);
  inline DoubleAc& operator*=(int f);

  DoubleAc& operator/=(DoubleAc f);
  inline DoubleAc& operator/=(double f);
  inline DoubleAc& operator/=(float f);
  inline DoubleAc& operator/=(long f);
  inline DoubleAc& operator/=(int f);

  void print(std::ostream& file, int l=1) const;
  // if(l <= 0) return without print
  // if i == 1, 2, or 3, print without passing to the next line at the end
  // if i == 4, 5, or 6, the same print but with passing to the next line 
  //     (sending  file<<'\n';)
  // 1 - output to file only central value by ordinary operator file<<d;
  // 2 - output central value and both limits in brackets.
  // 3 - output central value, then set e.precision(2),
  //     output differences in brackets d-di and da-d (both positive),
  //     and restore initial precision.
  // 4 - output to file only central value by ordinary operator file<<d;
  //     and passing to the next line.
  // 5 - the same as 3 but passing to the next line at the end. 
  // 6 - output all values with precision 16 and with width 20,
  //     restore initial precision, and passing to the next line.
  // The most convenient options are 3 or 5.
};

inline DoubleAc::DoubleAc(double f)
  // assumes pure numerical uncertanty which is defined via macro above
  // unless f == 0. In this case the precision is assumed absolute.
{
  d=f;
  if(f==0.0)
  {
    di=0.0; da=0.0;
  }
  else
  {
    if(f>0.0)
    {
      if(f<DBL_MAX/one_plus_def_dbl_prec ) da = f * one_plus_def_dbl_prec;
      else da = f;
      if(f>DBL_MIN*one_plus_def_dbl_prec ) di = f / one_plus_def_dbl_prec;
      else di = f;
    }
    else
    {
      if(-f<DBL_MAX/one_plus_def_dbl_prec ) di = f * one_plus_def_dbl_prec;
      else di = f;
      if(-f>DBL_MIN*one_plus_def_dbl_prec ) da = f / one_plus_def_dbl_prec;
      else da = f;
    }
  }
  /*
  mcout<<"DoubleAc::DoubleAc(double f)\n";
  print(mcout, 3);
  if( d < di )
  {
    mcerr<<"error in inline DoubleAc::DoubleAc(double f)\n";
    mcerr<<"d < di\n";
    spexit(mcerr);
  }
  if( d > da )
  {
    mcerr<<"error in inline DoubleAc::DoubleAc(double f)\n";
    mcerr<<"d > da\n";
    spexit(mcerr);
  }
  */
}

inline DoubleAc& DoubleAc::operator=(double f)
{ 
  d=f;
  if(f==0.0)
  {
    di=0.0; da=0.0;
  }
  else
  {
    if(f>0.0)
    {
      if(f<DBL_MAX/one_plus_def_dbl_prec ) da = f * one_plus_def_dbl_prec;
      else da = f;
      if(f>DBL_MIN*one_plus_def_dbl_prec ) di = f / one_plus_def_dbl_prec;
      else di = f;
    }
    else
    {
      if(-f<DBL_MAX/one_plus_def_dbl_prec ) di = f * one_plus_def_dbl_prec;
      else di = f;
      if(-f>DBL_MIN*one_plus_def_dbl_prec ) da = f / one_plus_def_dbl_prec;
      else da = f;
    }
  }
  /*
  if(f > -DBL_MAX/one_minus_def_dbl_prec ) di = f * one_minus_def_dbl_prec;
  else di = f;
  d=f; 
  if(f < DBL_MAX/one_plus_def_dbl_prec ) da = f*one_plus_def_dbl_prec;
  else da = f;
  */
  /*
  mcout<<"DoubleAc::operator=(double f)\n";
  print(mcout, 3);
  if( d < di )
  {
    mcerr<<"error in inline DoubleAc::operator=(double f)\n";
    mcerr<<"d < di\n";
    spexit(mcerr);
  }
  if( d > da )
  {
    mcerr<<"error in inline DoubleAc::operator=(double f)\n";
    mcerr<<"d > da\n";
    spexit(mcerr);
  }
  */
  return *this;
}

inline DoubleAc::DoubleAc(float f)
{
  d=f;
  if(f==0.0)
  {
    di=0.0; da=0.0;
  }
  else if(f>0.0)
  {
    di = f * one_minus_def_flt_prec;
    da = f * one_plus_def_flt_prec;
  }
  else
  {
    da = f * one_minus_def_flt_prec;
    di = f * one_plus_def_flt_prec;
  }
}

inline DoubleAc& DoubleAc::operator=(float f)
{ 
  d=f;
  if(f==0.0)
  {
    di=0.0; da=0.0;
  }
  else if(f>0.0)
  {
    di = f * one_minus_def_flt_prec;
    da = f * one_plus_def_flt_prec;
  }
  else
  {
    da = f * one_minus_def_flt_prec;
    di = f * one_plus_def_flt_prec;
  }
  return *this;
}

inline DoubleAc::DoubleAc(long f)
{
  d=f; di=f; da=f;
}

inline DoubleAc& DoubleAc::operator=(long f)
{ 
  d=f; di=f; da=f; return *this;
}


inline DoubleAc::DoubleAc(int f)
{
  d=f; di=f; da=f;
}
inline DoubleAc& DoubleAc::operator=(int f)
{ 
  d=f; di=f; da=f; return *this;
}

inline DoubleAc& DoubleAc::operator+=(const DoubleAc& f)
{ di+=f.di; d+=f.d; da+=f.da; return *this; }
inline DoubleAc& DoubleAc::operator+=(double f)
{ *this += DoubleAc(f); return *this; }
  //{ di+=f; d+=f; da+=f; return *this; }
inline DoubleAc& DoubleAc::operator+=(float f)
{ *this += DoubleAc(f); return *this; }
  //{ di+=f; d+=f; da+=f; return *this; }
inline DoubleAc& DoubleAc::operator+=(long f)
{ di+=f; d+=f; da+=f; return *this; }
inline DoubleAc& DoubleAc::operator+=(int f)
{ di+=f; d+=f; da+=f; return *this; }

inline DoubleAc& DoubleAc::operator-=(const DoubleAc& f)
{ di-=f.da; d-=f.d; da-=f.di; return *this; }
inline DoubleAc& DoubleAc::operator-=(double f)
{ *this -= DoubleAc(f); return *this; }
 //{ di-=f; d-=f; da-=f; return *this; }
inline DoubleAc& DoubleAc::operator-=(float f)
{ *this -= DoubleAc(f); return *this; }
//{ di-=f; d-=f; da-=f; return *this; }
inline DoubleAc& DoubleAc::operator-=(long f)
{ di-=f; d-=f; da-=f; return *this; }
inline DoubleAc& DoubleAc::operator-=(int f)
{ di-=f; d-=f; da-=f; return *this; }

inline DoubleAc& DoubleAc::operator*=(double f)
{
  d*=f;
  if(f >= 0.0) { di*=f; da*=f; }
  else { double ti=da*f; da=di*f; di=ti; }
  return *this;
}
inline DoubleAc& DoubleAc::operator*=(float f)
{
  d*=f;
  if(f >= 0.0) { di*=f; da*=f; }
  else { double ti=da*f; da=di*f; di=ti; }
  return *this;
}
inline DoubleAc& DoubleAc::operator*=(long f)
{
  d*=f;
  if(f >= 0.0) { di*=f; da*=f; }
  else { double ti=da*f; da=di*f; di=ti; }
  return *this;
}
inline DoubleAc& DoubleAc::operator*=(int f)
{
  d*=f;
  if(f >= 0.0) { di*=f; da*=f; }
  else { double ti=da*f; da=di*f; di=ti; }
  return *this;
}

inline DoubleAc& DoubleAc::operator/=(double f)
{
  if(f == 0.0)
  {
    mcerr<<"inline DoubleAc& DoubleAc::operator/=(double f):\n"
    <<"f = 0.0\n";
    spexit(mcerr);
  }
  d/=f;
  if(f >= 0.0){ di/=f; da/=f; }
  else { double ti=da/f; da=di/f; di=ti; }
  return *this;
}
inline DoubleAc& DoubleAc::operator/=(float f)
{
  if(f == 0.0)
  {
    mcerr<<"inline DoubleAc& DoubleAc::operator/=(float f):\n"
    <<"f = 0.0\n";
    spexit(mcerr);
  }
  d/=f;
  if(f >= 0.0){ di/=f; da/=f; }
  else { double ti=da/f; da=di/f; di=ti; }
  return *this;
}
inline DoubleAc& DoubleAc::operator/=(long f)
{
  if(f == 0)
  {
    mcerr<<"inline DoubleAc& DoubleAc::operator/=(long f):\n"
    <<"f = 0\n";
    spexit(mcerr);
  }
  d/=f;
  if(f >= 0.0){ di/=f; da/=f; }
  else { double ti=da/f; da=di/f; di=ti; }
  return *this;
}
inline DoubleAc& DoubleAc::operator/=(int f)
{
  if(f == 0)
  {
    mcerr<<"inline DoubleAc& DoubleAc::operator/=(int f):\n"
    <<"f = 0\n";
    spexit(mcerr);
  }
  d/=f;
  if(f >= 0.0){ di/=f; da/=f; }
  else { double ti=da/f; da=di/f; di=ti; }
  return *this;
}

inline DoubleAc operator-(const DoubleAc& f)
{
  DoubleAc t(-f.get() , -f.get_right_limit(), -f.get_left_limit());
  return t;
}

inline void change_sign(DoubleAc& f)
{
  f.d = -f.d;
  double temp = f.di;
  f.di = -f.da;
  f.da = -temp;
}


inline DoubleAc operator+(const DoubleAc& f1, const DoubleAc& f2 )
{
  DoubleAc t=f1; t+=f2; return t;
}
inline DoubleAc operator+(const DoubleAc& f1, double f2 )
{
  DoubleAc t=f1; t+=f2; return t;
}
inline DoubleAc operator+(double f1, const DoubleAc& f2 )
{
  DoubleAc t=f2; t+=f1; return t;
}
inline DoubleAc operator+(const DoubleAc& f1, float f2 )
{
  DoubleAc t=f1; t+=f2; return t;
}
inline DoubleAc operator+(float f1, const DoubleAc& f2 )
{
  DoubleAc t=f2; t+=f1; return t;
}
inline DoubleAc operator+(const DoubleAc& f1, long f2 )
{
  DoubleAc t=f1; t+=f2; return t;
}
inline DoubleAc operator+(long f1, const DoubleAc& f2 )
{
  DoubleAc t=f2; t+=f1; return t;
}
inline DoubleAc operator+(const DoubleAc& f1, int f2 )
{
  DoubleAc t=f1; t+=f2; return t;
}
inline DoubleAc operator+(int f1, const DoubleAc& f2 )
{
  DoubleAc t=f2; t+=f1; return t;
}

inline DoubleAc operator-(const DoubleAc& f1, const DoubleAc& f2 )
{
  DoubleAc t=f1; t-=f2; return t;
}
inline DoubleAc operator-(const DoubleAc& f1, double f2 )
{
  DoubleAc t=f1; t-=f2; return t;
}
inline DoubleAc operator-(double f1, const DoubleAc& f2 )
{
  DoubleAc t=-f2; t+=f1; return t;
}
inline DoubleAc operator-(const DoubleAc& f1, float f2 )
{
  DoubleAc t=f1; t-=f2; return t;
}
inline DoubleAc operator-(float f1, const DoubleAc& f2 )
{
  DoubleAc t=-f2; t+=f1; return t;
}
inline DoubleAc operator-(const DoubleAc& f1, long f2 )
{
  DoubleAc t=f1; t-=f2; return t;
}
inline DoubleAc operator-(long f1, const DoubleAc& f2 )
{
  DoubleAc t=-f2; t+=f1; return t;
}
inline DoubleAc operator-(const DoubleAc& f1, int f2 )
{
  DoubleAc t=f1; t-=f2; return t;
}
inline DoubleAc operator-(int f1, const DoubleAc& f2 )
{
  DoubleAc t=-f2; t+=f1; return t;
}

inline DoubleAc operator*(const DoubleAc& f1, const DoubleAc& f2 )
{
  DoubleAc t=f1; t*=f2;
  return t;
}
inline DoubleAc operator*(const DoubleAc& f1, double f2 )
{
  DoubleAc t=f1; t*=f2;
  return t;
}
inline DoubleAc operator*(double f1, const DoubleAc& f2 )
{
  DoubleAc t=f2; t*=f1;
  return t;
}
inline DoubleAc operator*(const DoubleAc& f1, float f2 )
{
  DoubleAc t=f1; t*=f2;
  return t;
}
inline DoubleAc operator*(float f1, const DoubleAc& f2 )
{
  DoubleAc t=f2; t*=f1;
  return t;
}
inline DoubleAc operator*(const DoubleAc& f1, long f2 )
{
  DoubleAc t=f1; t*=f2;
  return t;
}
inline DoubleAc operator*(long f1, const DoubleAc& f2 )
{
  DoubleAc t=f2; t*=f1;
  return t;
}
inline DoubleAc operator*(const DoubleAc& f1, int f2 )
{
  DoubleAc t=f1; t*=f2;
  return t;
}
inline DoubleAc operator*(int f1, const DoubleAc& f2 )
{
  DoubleAc t=f2; t*=f1;
  return t;
}

inline DoubleAc operator/(const DoubleAc& f1, const DoubleAc& f2 )
{
  DoubleAc t=f1; t/=f2;
  return t;
}
inline DoubleAc operator/(const DoubleAc& f1, double f2 )
{
  DoubleAc t=f1; t/=f2;
  return t;
}
inline DoubleAc operator/(double f1, const DoubleAc& f2 )
{
  DoubleAc t=f1; t/=f2;
  return t;
}
inline DoubleAc operator/(const DoubleAc& f1, float f2 )
{
  DoubleAc t=f1; t/=f2;
  return t;
}
inline DoubleAc operator/(float f1, const DoubleAc& f2 )
{
  DoubleAc t=f1; t/=f2;
  return t;
}
inline DoubleAc operator/(const DoubleAc& f1, long f2 )
{
  DoubleAc t=f1; t/=f2;
  return t;
}
inline DoubleAc operator/(long f1, const DoubleAc& f2 )
{
  DoubleAc t=f1; t/=f2;
  return t;
}
inline DoubleAc operator/(const DoubleAc& f1, int f2 )
{
  DoubleAc t=f1; t/=f2;
  return t;
}
inline DoubleAc operator/(int f1, const DoubleAc& f2 )
{
  DoubleAc t=f1; t/=f2;
  return t;
}

inline DoubleAc fabs(const DoubleAc& f)
{
  if(f.left_limit() >= 0) return f;
  else if(f.right_limit() > 0)
  {
    return DoubleAc(fabs(f.get()), 
		    0.0, 
		    find_max(f.right_limit(), -f.left_limit()) );
  }
  else
  {
    return DoubleAc(-f.get(), -f.right_limit(), -f.left_limit() );
  }
}

inline DoubleAc find_min(const DoubleAc& a, const DoubleAc& b )
{ return (a.get() < b.get() ? a : b ); }
inline DoubleAc find_min(const DoubleAc& a, double b )
{ return (a.get() < b ? a : DoubleAc(b) ); }
inline DoubleAc find_min(double a, const DoubleAc& b )
{ return (a < b.get() ? DoubleAc(a) : b ); }
inline DoubleAc find_min(const DoubleAc& a, float b )
{ return (a.get() < b ? a : DoubleAc(b) ); }
inline DoubleAc find_min(float a, const DoubleAc& b )
{ return (a < b.get() ? DoubleAc(a) : b ); }
inline DoubleAc find_min(const DoubleAc& a, long b )
{ return (a.get() < b ? a : DoubleAc(b) ); }
inline DoubleAc find_min(long a, const DoubleAc& b )
{ return (a < b.get() ? DoubleAc(a) : b ); }
inline DoubleAc find_min(const DoubleAc& a, int b )
{ return (a.get() < b ? a : DoubleAc(b) ); }
inline DoubleAc find_min(int a, const DoubleAc& b )
{ return (a < b.get() ? DoubleAc(a) : b ); }

inline DoubleAc find_max(const DoubleAc& a, const DoubleAc& b )
{ return (a.get() > b.get() ? a : b ); }
inline DoubleAc find_max(const DoubleAc& a, double b )
{ return (a.get() > b ? a : DoubleAc(b) ); }
inline DoubleAc find_max(double a, const DoubleAc& b )
{ return (a > b.get() ? DoubleAc(a) : b ); }
inline DoubleAc find_max(const DoubleAc& a, float b )
{ return (a.get() > b ? a : DoubleAc(b) ); }
inline DoubleAc find_max(float a, const DoubleAc& b )
{ return (a > b.get() ? DoubleAc(a) : b ); }
inline DoubleAc find_max(const DoubleAc& a, long b )
{ return (a.get() > b ? a : DoubleAc(b) ); }
inline DoubleAc find_max(long a, const DoubleAc& b )
{ return (a > b.get() ? DoubleAc(a) : b ); }
inline DoubleAc find_max(const DoubleAc& a, int b )
{ return (a.get() > b ? a : DoubleAc(b) ); }
inline DoubleAc find_max(int a, const DoubleAc& b )
{ return (a > b.get() ? DoubleAc(a) : b ); }

DoubleAc sqrt(const DoubleAc& f);

DoubleAc square(const DoubleAc& f);

DoubleAc pow(const DoubleAc& f, double p);

DoubleAc exp(const DoubleAc& f);

DoubleAc sin(const DoubleAc& f);

DoubleAc cos(const DoubleAc& f);

DoubleAc asin(const DoubleAc& f);

DoubleAc acos(const DoubleAc& f);

std::ostream& operator<<(std::ostream& file, const DoubleAc& f);
// Calls f.print(file, 1) (prints only the central value)

#define Iprintda( file , name ) file<<indn<<#name<<"="<<noindent; \
name.print(file, 3); file<<yesindent;

#define Iprintdan( file , name ) file<<indn<<#name<<"="<<noindent; \
name.print(file, 3); file<<yesindent<<'\n';


#endif
 

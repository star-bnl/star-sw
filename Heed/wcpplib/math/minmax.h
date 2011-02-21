#ifndef MINMAX_H
#define MINMAX_H
/*
Copyright (c) 2001 Igor Borisovich Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
This software is distributed in the hope that it will be useful,
but the author makes no representations about the suitability 
of this software for any particular purpose. 
It is provided "as is" without express or implied warranty.
*/

//#ifndef DBL_MAX      // in Solaris they are in limits.h
//#define DBL_MAX MAXDOUBLE
//#endif
//#ifndef DBL_MIN      //
//#define DBL_MIN MINDOUBLE
//#endif

#ifdef VISUAL_STUDIO
#define _USE_MATH_DEFINES
// see comment in math.h:
/* Define _USE_MATH_DEFINES before including math.h to expose these macro
 * definitions for common math constants.  These are placed under an #ifdef
 * since these commonly-defined names are not part of the C/C++ standards.
 */
#endif
#include <cmath>
#include <cfloat>
#include "wcpplib/stream/prstream.h"
#include "wcpplib/util/FunNameStack.h"

// Minimal and maximal power of e, whose result can be represented.
// Similar numbers for a != e can be obtained  in the user's program by     
// double dbl_max_a_exp = dbl_max_e_exp / log(a);
 
static const double dbl_min_e_exp = (DBL_MIN_EXP-1) * log(double(FLT_RADIX));
static const double dbl_max_e_exp = (DBL_MAX_EXP-1) * log(double(FLT_RADIX));
static const double dbl_max_square = sqrt(DBL_MAX);  // maximal number,
// which square can be represented. 
static const double dbl_max_pow3 = pow(DBL_MAX, 1.0/3.0);  // maximal number,
// whose power 3 can be represented. 

static const double flt_min_e_exp = (FLT_MIN_EXP-1) * log(double(FLT_RADIX));
static const double flt_max_e_exp = (FLT_MAX_EXP-1) * log(double(FLT_RADIX));
static const double flt_max_square = sqrt(FLT_MAX);  // maximal number,
// which square can be represented. 
static const double flt_max_pow3 = pow(FLT_MAX, float(1.0/3.0));  // maximal number,
// whose power 3 can be represented. 

//There is a widely used macros min and max making
//the program calculating the expresion twice.
//It is dangerous to define them again, since they can be in include files.
//It looks like no macro with name fmin, fmax.
//Note that there is double function fabs and int function(or macro?) abs.
//Note, the fabs is declared through math.h ( which need to include),
// but with the use of other headers also.
// It is useless to search it through grep, since
// the name fabs is generated via complicated macros.
 
/*
8.11.2005:
Now fmin and fmax are also found in new version g++ 3.2 and 3.2.3
in file /usr/include/bits/mathcalls.h

I am now not sure that they are common (there is no book under hand)
and that int and long versions are also available.
So I will change fmin and fmax to names probably more original
find_min and find_max.
By the way the names fmin were similar to notation of function parameters
in these programs.
This is other reason to get rid of them. 
 

//inline double fmin( double a, double b) { return (a<b ? a : b ); }
//inline double fmax( double a, double b) { return (a>b ? a : b ); }
//inline  float fmin(  float a,  float b) { return (a<b ? a : b ); }
//inline  float fmax(  float a,  float b) { return (a>b ? a : b ); }
//inline   long fmin(   long a,   long b) { return (a<b ? a : b ); }
//inline   long fmax(   long a,   long b) { return (a>b ? a : b ); }
//inline    int fmin(   int a,     int b) { return (a<b ? a : b ); }
//inline    int fmax(   int a,     int b) { return (a>b ? a : b ); }
*/

inline double find_min( double a, double b) { return (a<b ? a : b ); }
inline double find_max( double a, double b) { return (a>b ? a : b ); }
inline  float find_min(  float a,  float b) { return (a<b ? a : b ); }
inline  float find_max(  float a,  float b) { return (a>b ? a : b ); }
inline   long find_min(   long a,   long b) { return (a<b ? a : b ); }
inline   long find_max(   long a,   long b) { return (a>b ? a : b ); }
inline    int find_min(   int a,     int b) { return (a<b ? a : b ); }
inline    int find_max(   int a,     int b) { return (a>b ? a : b ); }

inline long left_round(double f)
{
  if(f>=0) return long(f);
  else return -long(-f)-1;
}
inline int even_num(long n)
{
  long v=n/2;
  long n1=v*2;
  if(n == n1) return 1;
  else return 0;
}

inline int check_value_inside_bounds(double val, double b1, double b2 )  
{ return ((val > b1 && val < b2) ? 1 : 0) ; }
inline int check_value_inside_bounds(float val, float b1, float b2 )  
{ return ((val > b1 && val < b2) ? 1 : 0) ; }
inline int check_value_inside_bounds(long val, long b1, long b2 )  
{ return ((val > b1 && val < b2) ? 1 : 0) ; }
inline int check_value_inside_bounds(int val, int b1, int b2 )  
{ return ((val > b1 && val < b2) ? 1 : 0) ; }

inline int check_value_in_bounds(double val, double b1, double b2 )  
{ return ((val >= b1 && val <= b2) ? 1 : 0) ; }
inline int check_value_in_bounds(float val, float b1, float b2 )  
{ return ((val >= b1 && val <= b2) ? 1 : 0) ; }
inline int check_value_in_bounds(long val, long b1, long b2 )  
{ return ((val >= b1 && val <= b2) ? 1 : 0) ; }
inline int check_value_in_bounds(int val, int b1, int b2 )  
{ return ((val >= b1 && val <= b2) ? 1 : 0) ; }

inline int check_value_inside_bounds(double val, double b[2])  
{ return ((val > b[0] && val < b[1]) ? 1 : 0) ; }
inline int check_value_inside_bounds(float val, float b[2] )  
{ return ((val > b[0] && val < b[1]) ? 1 : 0) ; }
inline int check_value_inside_bounds(long val, long b[2] )  
{ return ((val > b[0] && val < b[1]) ? 1 : 0) ; }
inline int check_value_inside_bounds(int val, int b[2] )  
{ return ((val > b[0] && val < b[1]) ? 1 : 0) ; }

inline int check_value_in_bounds(double val, double b[2] )  
{ return ((val >= b[0] && val <= b[1]) ? 1 : 0) ; }
inline int check_value_in_bounds(float val, float b[2] )  
{ return ((val >= b[0] && val <= b[1]) ? 1 : 0) ; }
inline int check_value_in_bounds(long val, long b[2] )  
{ return ((val >= b[0] && val <= b[1]) ? 1 : 0) ; }
inline int check_value_in_bounds(int val, int b[2] )  
{ return ((val >= b[0] && val <= b[1]) ? 1 : 0) ; }


//inline lmin( long a, long b) { return (a<b ? a : b ); }
//inline lmax( long a, long b) { return (a>b ? a : b ); }
/*
template<class T*>
inline void fcopy(T ar_source, T ar_dest, long q)
{
  long n=0;
  for( n=0; n<q; n++) ar_dest[n] = ar_source[n];
}
*/
template<class T>
inline void fcopy(T ar_source, T ar_dest, long q)
{
  long n;
  for( n=0; n<q; n++) ar_dest[n] = ar_source[n];
}

template<class T>
inline void arr_copy(const T* ar_source, T* ar_dest, long q)
{
  long n;
  for( n=0; n<q; n++) ar_dest[n] = ar_source[n];
}

template<class T>
inline void arr_assign(const T source, T* ar_dest, long q)
{
  long n;
  for( n=0; n<q; n++) ar_dest[n] = source;
}

template<class T>
inline long fmax_ar(T ar, long q)
{
  if( q <= 0 )
  {
    mcerr<<"inline long fmax_ar(T ar, long q): q<=0\n";
    spexit(mcerr);
  }
  long nmval=0;
  long n;
  for( n=1; n<q; n++)
  {
    if(ar[n]>ar[nmval]) { nmval=n; }
  }  
  return nmval;
}

template<class T> 
inline T tabs(const T& x) 
{
  return x >= 0 ? x : -x;
}

template<class T>
int apeq_mant(const T& x1, const T& x2, T prec)
{
  if(x1 == x2) return 1;
  if(prec == 0) return 0;
  if(x1 == 0 && x2 == 0) return 1;
  if(x1 < 0 && x2 > 0 || x1 > 0 && x2 < 0) return 0;
  if(tabs((x1 - x2)/(x1 + x2))<= prec) return 1;
  return 0;
}

#endif

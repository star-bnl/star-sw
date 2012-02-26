#ifndef LINE_H
#define LINE_H

#include "wcpplib/safetl/AbsArr.h"

/*
Package for integration and interpolation
of a function, defined by array.

Copyright (c) 2001 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/

/*
Most of the functions in this file are not contemporary.
Consider the use of tline.h instead.
*/


//#define INCLUDE_OBLITERATE_INTEG_AR_FUNCTIONS

//#include "wcpplib/safetl/AbsArr.h"

template< class T >class T2DPoint
{public:
  T x;
  T y;
  T2DPoint(void): x((T)0), y((T)0) {;}
  T2DPoint(T fx, T fy): x(fx), y(fy) {;}
};

template<class T>
std::ostream& operator<<(std::ostream& file, const T2DPoint<T>& f)
{
  Ifile<<"T2DPoint: x="<<f.x<<" y="<<f.y<<'\n';
  return file;
}

typedef T2DPoint<float> Float2DPoint;


/* 
The common feature of functions find_interval
is that if x is outside the edge elements of array, the functions return -1
*/

template< class T >
long find_interval(T x, DynLinArr< T2DPoint<T> > ar)
{
  long n1,n2,n3;
  long q=ar.get_qel();
  if(q<=0) return -1;
  if(x<ar[0].x || x>ar[q-1].x) return -1;
  if(x==ar[0].x) return 0;
  if(x==ar[q-1].x) return q-2;
  n1=0; n2=q-1;
  while( n2-n1 > 1 )
  {
    n3=n1 + (n2-n1)/2;
    if(x < ar[n3].x)
      n2=n3;
    else
      n1=n3;
  }
  return n1;
}

long find_interval(double x, 
		   DynLinArr< double > coor);

long find_interval(double x, 
		   double coor[], long q);

/* 
The common feature of functions lin_interpolation
is that if x is outside the edge elements of array, the functions return 0.0.
*/

template<class T>
double lin_interpolation(T x, DynLinArr< T2DPoint<T> > ar)
{
  long n;
  long q=ar.get_qel();
  long nstart=find_interval(x , ar);
  if(nstart < 0) return 0.0;
  return ar[nstart].y + 
    (ar[nstart+1].y-ar[nstart].y)/(ar[nstart+1].x-ar[nstart].x)*
    (x-ar[nstart].x);
}

double lin_interpolation(double x, 
			 DynLinArr< double > coor, DynLinArr< double > arr);
double lin_interpolation(double x, 
			 double coor[], double arr[], long q);

double double_parab_interpolation
(double x, 
 DynLinArr< double > coor, DynLinArr< double > arr);

// The following looks like extrapolating to the both sides

template<class T>
double lin_inter_extra_polation(T x, DynLinArr< T2DPoint<T> > ar)
{
  long n;
  long q=ar.get_qel();
  long nstart=find_interval(x , ar);
  if(nstart < 0) 
  {
    if(x<ar[0].x)
      nstart=0;
    else
      nstart=ar.get_qel()-2;
  }
  return ar[nstart].y + 
    (ar[nstart+1].y-ar[nstart].y)/(ar[nstart+1].x-ar[nstart].x)*
    (x-ar[nstart].x);
}
  

/* The next function applies the function lin_integ_ar to each bin
of histogramm and multiplies the content of the bin by the
integral value.
*/
/* Attention: these are legacy functions, therefore
some of them are for float arrays.
Most of them are not very efficient, but proved to work.
They can be used as templates for contruction of more efficient functions.
At least they show which functions are necessary for real work.
But be careful: the implementation of correct end conditions
is not simple and prone to errors. 
*/
/*
These function are obliterate.
It is difficult to assure the absense of subtle errors in them,
since they are all written is dynamic style and there are too many
of them. 
Use template functions from tline.h .
They are more understandable, better debugged, flexible, and therefore
there are much less separate functions.

*/
#ifdef INCLUDE_OBLITERATE_INTEG_AR_FUNCTIONS

extern void hist_lin_integ_ar(float *x, float *y, long q, 
			      // array with should modify the histogram
     float xhist_start, float xhist_step, float *yhist, long qhist);
                              // histogram

extern
 float lin_integ_ar(float *x, float *y, long q, float x1, float x2);
// fit table by a straight line and integrate the area below it.

extern
 double lin_integ_ar(DynLinArr< double > x, DynLinArr< double > y, 
		    long q, double x1, double x2);
// fit table by a straight line and integrate the area below it.

extern
 float step_integ_ar(float *x, float *y, long q, float x1, float x2);
// here x is a left side of interval on which
// fuction f is a constant.
// x and y are arrays with dimen. q and q-1 respectively;
// last point x[q-1] is end of last interval.

extern
 double step_integ_ar(const double *x, const double *y, 
		      long q, double x1, double x2);
// here x is a left side of interval on which
// fuction f is a constant.
// x and y are arrays with dimen. q and q-1 respectively;
// last point x[q-1] is end of last interval.
#endif

extern
 double find_x_for_known_int_hist
(long q, double xmin, double xmax, const double *y, 
 double integ, int* s_err);
extern
 double find_x_for_known_int_hist
(long q, double xmin, double xmax, DynLinArr< double > y, 
 double integ, int* s_err);
// For given histogram (array of bins with equal step width
// find such x that integral form first bin to x gives integral.
// s_err = 1 if integ is more than the total integral
// x is interpreted in steps plus xmin

extern
 double new_step_integ_ar_hist
(const long q, double xmin, double xmax, DynLinArr< double > y, 
 double x1, double x2);

extern
 double new_step_integ_ar_x_hist
(const long q, double xmin, double xmax, DynLinArr< double > y, 
 double x1, double x2);

/*
The following is shortage of
...x_hist / ...hist
s_err = 1 if ..hist ==0 
*/
extern
 double new_step_mean_x_hist
(const long q, double xmin, double xmax, DynLinArr< double > y, 
 double x1, double x2, int& s_err);

#endif

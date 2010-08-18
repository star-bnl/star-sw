//#include <process.h>
#include "wcpplib/stream/prstream.h"
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/math/line.h"
#include "wcpplib/math/parabol.h"

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


long find_interval(double x, 
		   DynLinArr< double > coor)
{
  mfunname("long find_interval(double x, DynLinArr< double > coor)");
  long n1,n2,n3;
  long q=coor.get_qel();
  if(q <= 0) return -1;
  if(x < coor[0] || x > coor[q-1]) return -1;
  if(x == coor[0]) return 0;
  if(x == coor[q-1]) return q-2;
  n1=0; n2=q-1;
  while( n2-n1 > 1 )
  {
    n3=n1 + (n2-n1)/2;
    if(x < coor[n3])
      n2=n3;
    else
      n1=n3;
  }
  return n1;
}

long find_interval(double x, 
		   double coor[], long q)
{
  mfunname("long find_interval(double x, double coor[], long q)");
  long n1,n2,n3;
  if(q <= 0) return -1;
  if(x < coor[0] || x > coor[q-1]) return -1;
  if(x == coor[0]) return 0;
  if(x == coor[q-1]) return q-2;
  n1=0; n2=q-1;
  while( n2-n1 > 1 )
  {
    n3=n1 + (n2-n1)/2;
    if(x < coor[n3])
      n2=n3;
    else
      n1=n3;
  }
  return n1;
}

double lin_interpolation(double x, 
			 DynLinArr< double > coor, DynLinArr< double > arr)
{
  mfunname("double lin_interpolation(T x, DynLinArr< double > coor, DynLinArr< double > arr)");

  long n;
  check_econd12(coor.get_qel() , != , arr.get_qel() , mcerr);
  long q=coor.get_qel();
  long nstart=find_interval(x , coor);
  if(nstart < 0) return 0.0;
  return arr[nstart] + 
    (arr[nstart+1] - arr[nstart]) / (coor[nstart+1] - coor[nstart])*
    (x - coor[nstart]);
}
double lin_interpolation(double x, 
			 double coor[], double arr[], long q)
{
  mfunname("double lin_interpolation(T x, double coor[], double arr[], long q)");

  long n;
  long nstart=find_interval(x , coor, q);
  if(nstart < 0) return 0.0;
  return arr[nstart] + 
    (arr[nstart+1] - arr[nstart]) / (coor[nstart+1] - coor[nstart])*
    (x - coor[nstart]);
}


double double_parab_interpolation
(double x, 
 DynLinArr< double > coor, DynLinArr< double > arr)
{
  mfunname("double double_parab_interpolation(T x, DynLinArr< double > coor, DynLinArr< double > arr)");

  long n;
  check_econd12(coor.get_qel() , != , arr.get_qel() , mcerr);
  long q=coor.get_qel();
  long nstart=find_interval(x , coor);
  if(nstart < 0) return 0.0;
  if(q == 1)  // line
  {
    check_econd12(x , != , coor[0] , mcerr); // provided in find_interval 
    return arr[0];
  }
  else if(q == 2)  // line
  {
    return arr[nstart] + 
      (arr[nstart+1] - arr[nstart]) / (coor[nstart+1] - coor[nstart])*
      (x - coor[nstart]);
  }
  else if(nstart == 0) // one parabola through three first points
  {
    Parabol pr(coor[nstart], coor[nstart+1], coor[nstart+2],
	       arr[nstart], arr[nstart+1], arr[nstart+2]);
    return pr.y(x);
  }
  else if(nstart == q-2) // one parabola through three last points
  {
    Parabol pr(coor[nstart-1], coor[nstart], coor[nstart+1],
	       arr[nstart-1], arr[nstart], arr[nstart+1]);
    return pr.y(x);
  }
  else
  {
    Parabol pr1(coor[nstart], coor[nstart+1], coor[nstart+2],
	       arr[nstart], arr[nstart+1], arr[nstart+2]);
    double y1 = pr1.y(x);
    Parabol pr2(coor[nstart-1], coor[nstart], coor[nstart+1],
	       arr[nstart-1], arr[nstart], arr[nstart+1]);
    double y2 = pr2.y(x);
    double r1 = (x - coor[nstart])/(coor[nstart+1] - coor[nstart]);
    double r2 = (coor[nstart+1] - x)/(coor[nstart+1] - coor[nstart]);
    double y = r1 * y1 + r2 * y2;
    return y;
  }
    
}

#ifdef INCLUDE_OBLITERATE_INTEG_AR_FUNCTIONS


void hist_lin_integ_ar(float *x, float *y, long q, 
	float xhist_start, float xhist_step, float *yhist, long qhist)
{
  long nh;
  float x1,x2;
  x2=xhist_start;
  for( nh=0; nh<qhist; nh++)
  {
    x1=x2;
    x2=xhist_start + xhist_step*(nh+1);
    y[nh]*=lin_integ_ar(x,y,q,x1,x2);
  }
}



float lin_integ_ar(float *x, float *y, long q, float x1, float x2)
// fit table by a straight line and integrate the area below it.
{
  mfunname("float lin_integ_ar(...)");
  long nr,n1,n2,i;
  float xt1,xt2;
  float xr1,xr2;
  float a,b;
  float s=0;
  if(q<=0)return 0;
  if( x2 < x1 || x2 <= x[0] || x1 >= x[q-1] ) return 0;
  for(i=0; i<q; i++ )
    if(x[i] > x1 )
    {	n1=i; break; }
  n2=q-1;
  for(i=n1; i<q; i++ )
    if( x[i] >= x2 )
    {	n2=i; break; }
  if( x1 < x[0] )
  {	xt1=x[0]; nr=0; }
  else
  {	xt1=x1; nr=n1-1; }
  if( x2 > x[q-1] )
  {	xt2=x[q-1];  }
  else
    xt2=x2;
  xr2=xt1;
  for( ; nr<n2; nr++)
  {
    
    //for debug:
    check_econd12(nr , > , q-2 , mcerr);
    
    //if(nr>q-2)
    //{
    //cerr<<"step_integ_ar:wrong nr=",nr,"\n";
    //exit(1);
    //}
    
    xr1=xr2;
    if( nr < q-1 )
      xr2=(xt2<x[nr+1])?xt2:x[nr+1];
    else
      xr2=xt2;
    a = (y[nr+1] - y[nr])/(x[nr+1] - x[nr]);
    b = y[nr];
    s+= 0.5*a*(xr2*xr2 - xr1*xr1) + (b - a*x[nr])*(xr2 - xr1);
  }
  return s;
}

double lin_integ_ar(DynLinArr< double > x, DynLinArr< double > y, 
		    long q, double x1, double x2)
// fit table by a straight line and integrate the area below it.
{
  mfunname("double lin_integ_ar(...)");
  long nr,n1,n2,i;
  double xt1,xt2;
  double xr1,xr2;
  double a,b;
  double s=0;
  if(q<=0)return 0;
  if( x2 < x1 || x2 <= x[0] || x1 >= x[q-1] ) return 0;
  for(i=0; i<q; i++ )
    if(x[i] > x1 )
    {	n1=i; break; }
  n2=q-1;
  for(i=n1; i<q; i++ )
    if( x[i] >= x2 )
    {	n2=i; break; }
  if( x1 < x[0] )
  {	xt1=x[0]; nr=0; }
  else
  {	xt1=x1; nr=n1-1; }
  if( x2 > x[q-1] )
  {	xt2=x[q-1];  }
  else
    xt2=x2;
  xr2=xt1;
  for( ; nr<n2; nr++)
  {
    
    //for debug:
    check_econd12(nr , > , q-2 , mcerr);
    
    //if(nr>q-2)
    //{
    //cerr<<"step_integ_ar:wrong nr=",nr,"\n";
    //exit(1);
    //}
    
    xr1=xr2;
    if( nr < q-1 )
      xr2=(xt2<x[nr+1])?xt2:x[nr+1];
    else
      xr2=xt2;
    a = (y[nr+1] - y[nr])/(x[nr+1] - x[nr]);
    b = y[nr];
    s+= 0.5*a*(xr2*xr2 - xr1*xr1) + (b - a*x[nr])*(xr2 - xr1);
  }
  return s;
}

float step_integ_ar(float *x, float *y, long q, float x1, float x2)
// here x is a left side of interval on which
// fuction f is a constant.
// x and y are arrays with dimen. q and q-1 respectively;
// last point x[q-1] is end of last interval.
{
  mfunname("float step_integ_ar(...)");
  //Imcout<<"step_integ_ar:\n";
  //Iprintn(mcout, q);
  //Iprintn(mcout, x1);
  //Iprintn(mcout, x2);
  long nr,n1,n2,i;
  float xt1,xt2;
  float xr1,xr2;
  //	float a,b;
  float s=0;
  if(q<=0)return 0;
  if( x2 < x1 || x2 <= x[0] || x1 >= x[q-1] ) return 0;
  for(i=0; i<q; i++ )
    if(x[i] > x1 )
    {	n1=i; break; }
  n2=q-1;
  for(i=n1; i<q; i++ )
    if( x[i] >= x2 )
    {	n2=i; break; }
  if( x1 < x[0] )
  {	xt1=x[0]; nr=0; }
  else
  {	xt1=x1; nr=n1-1; }
  if( x2 > x[q-1] )
  {	xt2=x[q-1];  }
  else
    xt2=x2;
  xr2=xt1;
  for( ; nr<n2; nr++)
  {
//for debug:
    check_econd12(nr , > , q-2 , mcerr);
    
    //if(nr>q-2)
    //{
    //cerr<<"step_integ_ar:wrong nr=",nr,"\n";
    //exit(1);
    //}
    
    xr1=xr2;
    if( nr < q-1 )
      xr2=(xt2<x[nr+1])?xt2:x[nr+1];
    else
      xr2=xt2;
    s+=y[nr]*(xr2-xr1);
    //Imcout<<"nr="<<nr<<" y[nr]="<<y[nr]
    //	  <<" xr..="<<xr1<<' '<<xr2
    //	  <<" s="<<s<<'\n';
  }
  return s;
}


double step_integ_ar(const double *x, const double *y, 
		     long q, double x1, double x2)
// here x is a left side of interval on which
// fuction f is a constant.
// x and y is a arrays with dimen. q and q-1 respectively;
// last point x[q-1] is end of last interval.
{
  mfunname("double step_integ_ar(...)");
  //Imcout<<"step_integ_ar:\n";
  //Iprintn(mcout, q);
  //Iprintn(mcout, x1);
  //Iprintn(mcout, x2);
  long nr,n1,n2,i;
  double xt1,xt2;
  double xr1,xr2;
  //	float a,b;
  double s=0;
  if(q<=0)return 0;
  if( x2 < x1 || x2 <= x[0] || x1 >= x[q-1] ) return 0;
  for(i=0; i<q; i++ )
    if(x[i] > x1 )
    {	n1=i; break; }
  n2=q-1;
  //Iprint2n(mcout, n1, n2);
  for(i=n1; i<q; i++ )
  {
    //Iprint3n(mcout, i, q, x[i]);
    if( x[i] >= x2 )
    {	n2=i; break; }
  }
  if( x1 < x[0] )
  {	xt1=x[0]; nr=0; }
  else
  {	xt1=x1; nr=n1-1; }
  if( x2 > x[q-1] )
  {	xt2=x[q-1];  }
  else
    xt2=x2;
  xr2=xt1;
  for( ; nr<n2; nr++)
  {
//for debug:
    check_econd12(nr , > , q-2 , mcerr);
    
    //if(nr>q-2)
    //{
    //cerr<<"step_integ_ar:wrong nr=",nr,"\n";
    //exit(1);
    //}
    
    xr1=xr2;
    if( nr < q-1 )
      xr2=(xt2<x[nr+1])?xt2:x[nr+1];
    else
      xr2=xt2;
    s+=y[nr]*(xr2-xr1);
    //Imcout<<"nr="<<nr<<" y[nr]="<<y[nr]
    //	  <<" xr..="<<xr1<<' '<<xr2
    //	  <<" s="<<s<<'\n';
  }
  return s;
}

#endif

double find_x_for_known_int_hist(long q, double xmin, double xmax, 
				 const double *y, 
				 double integ, int* s_err)
{
  mfunname("double find_x_for_known_int_hist(...)");
  check_econd11(q , < 0 , mcerr);
  check_econd12(xmax , < , xmin , mcerr);
  double step = (xmax - xmin)/q;
  check_econd11(integ , < 0.0 , mcerr);
  double s = 0.0;
  double integ_step = integ/step;
  long n;
  *s_err = 0;
  if(integ == 0.0)
  {
    return xmin;
  }
  for( n=0 ; n<q; n++)
  {
    double s1 = s + y[n];
    //Iprint4n(mcout, n, integ_step, s, s1);
    if(s1 > integ_step)
      break;
    if(s1 == integ_step)
      return xmin + step * (n+1);
    s = s1;
  }
  if(n == q)
  {
    *s_err = 1;
    return xmax;
  }
  s *= step;
  double x = xmin + step * n;
  //Iprint3n(mcout, n, s, x);
  x += (integ - s)/y[n];
  //Iprintn(mcout, x);
  return x;
}
    

double find_x_for_known_int_hist(long q, double xmin, double xmax, 
				 DynLinArr< double > y, 
				 double integ, int* s_err)
{
  mfunname("double find_x_for_known_int_hist(...)");
  check_econd11(q , < 0 , mcerr);
  check_econd12(xmax , < , xmin , mcerr);
  double step = (xmax - xmin)/q;
  check_econd11(integ , < 0.0 , mcerr);
  double s = 0.0;
  double integ_step = integ/step;
  long n;
  *s_err = 0;
  if(integ == 0.0)
  {
    return xmin;
  }
  for( n=0 ; n<q; n++)
  {
    double s1 = s + y[n];
    //Iprint4n(mcout, n, integ_step, s, s1);
    if(s1 > integ_step)
      break;
    if(s1 == integ_step)
      return xmin + step * (n+1);
    s = s1;
  }
  if(n == q)
  {
    *s_err = 1;
    return xmax;
  }
  s *= step;
  double x = xmin + step * n;
  //Iprint3n(mcout, n, s, x);
  x += (integ - s)/y[n];  
  //Iprintn(mcout, x);
  return x;
}
    

 double new_step_integ_ar_hist
(const long q, double xmin, double xmax, DynLinArr< double > y,
 double x1, double x2)
{
  mfunname("double new_step_integ_ar_hist(...)");

  check_econd12(xmin , >= , xmax , mcerr);
  check_econd12(q , < , 0 , mcerr);
  if(q == 0) return 0.0;
  if(x1 > x2) return 0.0;
  if(x2 <= xmin) return 0.0;
  if(x1 >= xmax) return 0.0;

  double step = (xmax - xmin)/q;
  long istart, iafterend; // indexes to sum total intervals
  //Iprint4n(mcout, x1, x2, xmin, step)

  double s=0;
  if(x1 <= xmin) 
  {
    x1 = xmin;
    istart = 0;
  }
  else
  {
    double d = (x1 - xmin)/step;
    long id = long(d);
    double r = d - id;
    //Iprint3n(mcout, d, id, r);
    if(x2 < xmin + (id+1) * step)
    {
      //Iprint4n(mcout, x1, x2, xmin, step)
      //Iprint3n(mcout, d, id, r)
      s = (x2 - x1) * y[id];
      return s;
    }
    if(r == 0)
      istart = long((x1 - xmin)/step);
    else
    {
      istart = long((x1 - xmin)/step) + 1;
      s += (1.0 - r) * y[istart-1];   // here is an error if x2 belong to
      // the same bin
    }
  }
  //Iprintn(mcout, s);
  if(x2 >= xmax) 
  {
    x2 = xmax;
    iafterend = q;
  }
  else
  {
    double d = (x2 - xmin)/step;
    long id = long(d);
    double r = d - id;
    //Iprint3n(mcout, d, id, r);
    if(r == 0)
      iafterend = long((x2 - xmin)/step);
    else
    {
      iafterend = long((x2 - xmin)/step) ;
      s += r * y[iafterend];
    }
  }
  //Iprintn(mcout, s);
  //Iprint2n(mcout, istart, iafterend);
  long i;
  for(i=istart; i<iafterend; i++)
  {
    s += y[i];
  }
  //Iprintn(mcout, s);
  s *= step;
  //Iprintn(mcout, s);
  return s;
}
  
 double new_step_integ_ar_x_hist
(const long q, double xmin, double xmax, DynLinArr< double > y,
 double x1, double x2)
{
  mfunname("double new_step_integ_ar_x_hist(...)");

  check_econd12(xmin , >= , xmax , mcerr);
  check_econd12(q , < , 0 , mcerr);
  if(q == 0) return 0.0;
  if(x1 > x2) return 0.0;
  if(x2 <= xmin) return 0.0;
  if(x1 >= xmax) return 0.0;

  double step = (xmax - xmin)/q;
  long istart, iafterend; // indexes to sum total intervals

  double s=0;
  if(x1 <= xmin) 
  {
    x1 = xmin;
    istart = 0;
  }
  else
  {
    double d = (x1 - xmin)/step;
    long id = long(d);
    double r = d - id;
    if(x2 < xmin + (id+1) * step)
    {
      s = 0.5 * (x2*x2 - x1*x1) * y[id];
      return s;
    }
    if(r == 0)
      istart = long((x1 - xmin)/step);
    else
    {
      istart = long((x1 - xmin)/step) + 1;
      double b = xmin + istart*step;
      double a = b - step + r*step; 
      s += 0.5* ( b * b  - a * a ) * y[istart-1];
    }
  }
  //Iprintn(mcout, s);
  if(x2 >= xmax) 
  {
    x2 = xmax;
    iafterend = q;
  }
  else
  {
    double d = (x2 - xmin)/step;
    long id = long(d);
    double r = d - id;
    //Iprint3n(mcout, d, id, r);
    if(r == 0)
      iafterend = long((x2 - xmin)/step);
    else
    {
      iafterend = long((x2 - xmin)/step) ;
      double a = xmin + iafterend * step; 
      double b = a + r*step;
      //Iprint2n(mcout, a, b);
      s += 0.5* ( b * b  - a * a ) * y[iafterend];
    }
  }
  //Iprintn(mcout, s);
  //Iprint2n(mcout, istart, iafterend);
  long i;
  double b = xmin + istart * step;
  for(i=istart; i<iafterend; i++)
  {
    double a = b;
    b = xmin + (i+1) * step;
    s += 0.5 * ( b * b  - a * a ) * y[i];
  }
  //Iprintn(mcout, s);
  return s;
}
  
 double new_step_mean_x_hist
(const long q, double xmin, double xmax, DynLinArr< double > y,
 double x1, double x2, int& s_err)
{
  mfunname("double new_step_mean_x_hist(...)");

  s_err = 0;
  double integ = new_step_integ_ar_hist(q, xmin, xmax, y, x1, x2);
  if(integ == 0)
  {
    s_err = 1;
    return 0.0;
  }
  double xinteg = new_step_integ_ar_x_hist(q, xmin, xmax, y, x1, x2);

  return xinteg / integ;
}
  

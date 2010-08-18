#ifndef TEMPLMOMENTS_H
#define TEMPLMOMENTS_H
#include <fstream>
#include <iomanip>
//using std::ifstream;
//using std::ofstream;
#include "wcpplib/stream/prstream.h"
#include "wcpplib/stream/definp.h"
/*
Copyright (c) 2007 I. B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU General Public License version 3
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text 
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/
/* There is collection of numbers and weights.
This software calculates their sum, mean, and sigma (RMS)
*/

#define INC_MINMAX


template<class T> class TemplMoments
{public:
  T get_qfill(void) const { return qfill; }
  T get_xnorm(void) const { return xnorm; }
  T get_xmean(void) const { if(s_sync == 0) calc(); return xmean; }
  T get_sigma(void) const { if(s_sync == 0) calc(); return sigma; }
  T get_xmin(void) const { return xmin; }
  T get_xmax(void) const { return xmax; }

private:
  T qfill;  // number of fillings, for informative and debug purposes,
  // in the case of using min/max this is also used to notice when
  // xmin and xmax do not have meaningfull values. 
  T xnorm;  // Norm, sum of weights.
  mutable int s_sync;
  mutable T xmean; // mean.
  mutable T sigma; // sigma, of course.

  mutable T xsum;  // sum of coordinates * weight
  mutable T x2sum; // sum of coordinates^2 * weight
#ifdef INC_MINMAX
  T xmin;   // minimum
  T xmax;   // maximum
  // It takes into account events even with zero weight, as well as qfill.
  // ///// No:If xmin ==1 and xmax == 0, the filling is not yet started
#endif
 public:
  void plus_pilar(T a, T b, T v);
  // add to xnorm, xsum, x2sum
  // If b > a, accounts interval.
  // v is integral along this interval.
  // If b < a, works like plus_event using a and v.

  void plus_event(T a, T v);
  // add to xnorm, xsum, x2sum

  void minus_event(T a, T v);
  // substruct from xnorm, xsum, x2sum

  void retrieve_sum(void) const ;  // retrieves xsum and x2sum
  // Probably it is used at reading of xnorm, xmean, and sigma from disk

  TemplMoments<T>()
    { xnorm=xmean=sigma=xsum=x2sum=0; s_sync = 1; qfill = 0; 
#ifdef INC_MINMAX
    xmax = 0;
    xmin = 0;
#endif
    }
  TemplMoments<T>(T fqfill, T fxnorm, T fxsum, T fx2sum
#ifdef INC_MINMAX
		  , T fxmin, T fxmax
#endif
		  ): 
    qfill(fqfill), xnorm(fxnorm), xsum(fxsum), x2sum(fx2sum)
#ifdef INC_MINMAX
    , xmax(fxmax), xmin(fxmin)
#endif

  { calc(); }
  TemplMoments<T>(T fqfill, T fxnorm, T fxmean, T fsigma, 
#ifdef INC_MINMAX
		  T fxmin, T fxmax,
#endif
		  int anything):  // to differ it from previous constructor 
    qfill(fqfill), xnorm(fxnorm), xmean(fxmean), sigma(fsigma)
#ifdef INC_MINMAX
    , xmin(fxmin), xmax(fxmax)
#endif
  { retrieve_sum(); }
  TemplMoments<T> operator+(const TemplMoments<T>& mom) const ;
  TemplMoments<T>& operator+=(const TemplMoments<T>& mom);
  TemplMoments<T>& operator-=(const TemplMoments<T>& mom);
  TemplMoments<T> operator*(T s) const ; 
  TemplMoments<T>& operator*=(T s); // the both functions change weight,
  // and are equivalent to filling with different weight.
  // Naturally this does not lead to change of mean and sigma.
  TemplMoments<T>& scale_x(T s); // this is as of x was scaled.

  void calc(void) const; // calculate xmean and sigma
  void clear(void) {xnorm=xmean=sigma=xsum=x2sum=0; s_sync = 1; qfill = 0; } 
};

template<class T>
void TemplMoments<T>::calc(void) const
{
  if(xnorm<=0)
  {
    xmean=0;
    sigma=0;
  }
  else
  {
    xmean=xsum/xnorm;
    T fr=x2sum/xnorm-xmean*xmean;
    if(fr<=0)
      sigma=0;
    else
      sigma=T(sqrt(fr));
  }
  s_sync = 1;
}

template<class T>
void TemplMoments<T>::plus_pilar(T a, T b, T v)
{
#ifdef INC_MINMAX
  if(qfill == 0)
  {
    xmin = a;
    xmax = a;
  }
  else
  {
    if(xmin > a)
    {
      xmin = a;
    }
    else if(xmax < a)
    {
      xmax = a;
    }
  }
#endif
  qfill++;
  xnorm+=v;
  if(a>=b)
  {
    xsum+=v*a;
    x2sum+=a*a*v;
  }
  else
  {
//		v=v/(b-a);
//		xsum+=v*(b*b-a*a)/2.0;
    xsum+=v*(b+a)/2.0;
    x2sum+=v*(b*b+b*a+a*a)/3.0;
    // (b*b+b*a+a*a) is equivalent to (b**3 - a**3)/(b-a)
  }
  s_sync = 0;
}

template<class T>
void TemplMoments<T>::plus_event(T a, T v)
{
#ifdef INC_MINMAX
  if(qfill == 0)
  {
    xmin = a;
    xmax = a;
  }
  else
  {
    if(xmin > a)
    {
      xmin = a;
    }
    else if(xmax < a)
    {
      xmax = a;
    }
  }
#endif
  qfill++;
  xnorm+=v;
  xsum+=a*v;
  x2sum+=a*a*v;
  s_sync = 0;
}

template<class T>
void TemplMoments<T>::minus_event(T a, T v)
{
#ifdef INC_MINMAX
  if(qfill == 0)
  {
    xmin = a;
    xmax = a;
  }
  else
  {
    if(xmin > a)
    {
      xmin = a;
    }
    else if(xmax < a)
    {
      xmax = a;
    }
  }
#endif
  qfill++;  // previously it was --, but this seems to be not necessary
  // and also it interfiers the use of this var. for xmin/xmax accounting.
  xnorm-=v;
  xsum-=a*v;
  x2sum-=a*a*v;
  s_sync = 0;
}

template<class T>
void TemplMoments<T>::retrieve_sum() const
{
  xsum=xmean*xnorm;
  x2sum=(sigma*sigma+xmean*xmean)*xnorm;
  s_sync = 1;
}


template<class T>
TemplMoments<T> TemplMoments<T>::operator+(const TemplMoments& mom) const
{
  TemplMoments<T> temp = *this;
  temp.qfill += mom.qfill;
  temp.xnorm+=mom.xnorm;
  temp.xmean=0;
  temp.sigma=0;   //after must be called calc!
  temp.xsum+=mom.xsum;
  temp.x2sum+=mom.x2sum;
#ifdef INC_MINMAX
  if(mom.qfill > 0)
  {
    if(temp.xmin > mom.xmin) temp.xmin = mom.xmin;
    if(temp.xmax < mom.xmax) temp.xmax = mom.xmax;
  }
#endif
  //calc();
  return temp;
}

template<class T>
TemplMoments<T>& TemplMoments<T>::operator+=(const TemplMoments& mom)
{
  qfill += mom.qfill;
  xnorm+=mom.xnorm;
  xmean=sigma=0;   //after must be called calc!
  xsum+=mom.xsum;
  x2sum+=mom.x2sum;
#ifdef INC_MINMAX
  if(mom.qfill > 0)
  {
    if(xmin > mom.xmin) xmin = mom.xmin;
    if(xmax < mom.xmax) xmax = mom.xmax;
  }
#endif
  //calc();
  return *this;
}

template<class T>
TemplMoments<T>& TemplMoments<T>::operator-=(const TemplMoments& mom)
{
  qfill -= mom.qfill;
  xnorm-=mom.xnorm;
  xmean=sigma=0;   //after must be called calc!
  xsum-=mom.xsum;
  x2sum-=mom.x2sum;
#ifdef INC_MINMAX
  if(mom.qfill > 0)
  {
    if(xmin > mom.xmin) xmin = mom.xmin;
    if(xmax < mom.xmax) xmax = mom.xmax;
  }
#endif
  //calc();
  return *this;
}

template<class T>
TemplMoments<T> TemplMoments<T>::operator*(T s) const 
{
  TemplMoments<T> temp = *this;
  temp.xnorm*=s;
  temp.xsum*=s;
  temp.x2sum*=s;
  temp.xmean=0;
  temp.sigma=0;   //after must be called calc!
  return temp;
}

template<class T>
TemplMoments<T>& TemplMoments<T>::operator*=(T s)
{
  xnorm*=s;
  xsum*=s;
  x2sum*=s;
  xmean=sigma=0;   //after must be called calc!
  return *this;
}

template<class T>
TemplMoments<T>& TemplMoments<T>::scale_x(T s)
{
  xsum*=s;
  x2sum*=s*s;
  xmean=sigma=0;   //after must be called calc!
#ifdef INC_MINMAX
  if(qfill > 0)
  {
    xmin *= s;
    xmax *= s;
  }
#endif
  return *this;
}

template<class T>
void clear(TemplMoments<T>& f)
{ f.clear(); }
//extern ifstream& operator>>(ifstream& file, moments& mom);
//extern ofstream& operator<<(ofstream& file, const moments& mom);
// writes for machine reading

//extern ostream& operator<<(ostream& file, const moments& mom);
// writes in readable form


template<class T>
istream& operator>>(istream& file, TemplMoments<T>& mom)
{
  mfunnamep("istream& operator>>(istream& file, TemplMoments<T>& mom)");
  //file>>mom.qfill>>mom.xnorm>>mom.xmean>>mom.sigma;
  //mcout<<"operator>>(istream& file, TemplMoments<T>& mom) is started\n";
  if(s_short_output == 0)
  {
    definp_endpar dep(&file, 0, 1, 0);
    T qfill;
    DEFINPAP(qfill);
    check_econd11(qfill , < 0 , mcerr);
    T norm;
    DEFINPAP(norm);
    //check_econd11(norm , < 0 , mcerr);  // this can be for 
    // non-quite-histograms but for plots of different sense or for
    // background-subtracted histograms
    T mean;
    DEFINPAP(mean);
    //check_econd11(mean , < 0 , mcerr);  // this can be, obviously
    T RMS;
    DEFINPAP(RMS);
    check_econd11(RMS , < 0 , mcerr);
    //mcout<<"operator>>(istream& file, TemplMoments<T>& mom):\n";
    //Iprint4n(mcout, qfill, norm, mean, RMS);
    //mom = TemplMoments<T>(qfill, norm, mean, RMS);
#ifdef INC_MINMAX
    T xmi;
    DEFINPAP(xmi);
    T xma;
    DEFINPAP(xma);
    check_econd12(xmi , > , xma , mcerr);
    mom = TemplMoments<T>(qfill, norm, mean, RMS, xmi, xma, 0);
#else
    mom = TemplMoments<T>(qfill, norm, mean, RMS, 0);
#endif
    //Iprintn(cout, mom);
    //mom.retrieve_sum();
    //Iprintn(cout, mom);
  }
  else
  {
    T qfill;
    file>>qfill;
    check_econd11(qfill , < 0 , mcerr);
    T norm;
    file>>norm;
    //check_econd11(norm , < 0 , mcerr); // see comment above
    T mean;
    file>>mean;
    //check_econd11(mean , < 0 , mcerr);  // this can be, obviously
    T RMS;
    file>>RMS;
    check_econd11(RMS , < 0 , mcerr);
    //mcout<<"operator>>(istream& file, TemplMoments<T>& mom):\n";
    //Iprint4n(mcout, qfill, norm, mean, RMS);
    //mom = TemplMoments<T>(qfill, norm, mean, RMS);
#ifdef INC_MINMAX
    T xmi;
    file>>xmi;
    T xma;
    file>>xma;
    check_econd12(xmi , > , xma , mcerr);
    mom = TemplMoments<T>(qfill, norm, mean, RMS, xmi, xma, 0);
#else
    mom = TemplMoments<T>(qfill, norm, mean, RMS, 0);
#endif
  }
  return file;
}

/*
template<class T>
ofstream& operator<<(ofstream& file, const moments& mom)
{
  moments temp=mom;
  temp.calc();
  file<<temp.xnorm<<"  "<<temp.xmean<<"  "<<temp.sigma<<'\n';
  return file;
}
*/
template<class T>
ostream& operator<<(ostream& file, const TemplMoments<T>& mom)
{
  //TemplMoments<T> temp=mom;
  mom.calc();
  //Ifile<<"TemplMoments<T>: (T is "<<typeid(T).name()
  //     <<") qfill="<<setw(5)<<mom.get_qfill()
  if(s_short_output == 0)
  {
    Ifile<<"TemplMoments<T>: qfill="<<setw(5)<<mom.get_qfill()
	 <<" norm="<<setw(5)<<mom.get_xnorm()
	 <<" mean="<<setw(5)<<mom.get_xmean()
	 <<" RMS="<<setw(5)<<mom.get_sigma()
#ifdef INC_MINMAX
	 <<" xmin/xmax="<<mom.get_xmin()<<' '<<mom.get_xmax()
#endif
	 <<'\n';
  }
  else
  {
    Ifile<<mom.get_qfill()<<' '
	 <<mom.get_xnorm()<<' '
	 <<mom.get_xmean()<<' '
	 <<mom.get_sigma()
#ifdef INC_MINMAX
	 <<' '
	 <<mom.get_xmin()<<' '<<mom.get_xmax()
#endif
	 <<'\n';
  }
  return file;
}

template<class T>
int operator==(const TemplMoments<T>& f1, const TemplMoments<T>& f2)
{
  if(f1.get_qfill() == f2.get_qfill() &&
     f1.get_xnorm() == f2.get_xnorm() &&
     f1.get_xmean() == f2.get_xmean() &&
     f1.get_sigma() == f2.get_sigma()
#ifdef INC_MINMAX
     &&
     f1.get_xmin() == f2.get_xmin() &&
     f1.get_xmax() == f2.get_xmax()
#endif
     ) 
  {
    return 1;
  }
  else
  {
    return 0;
  }
}

template<class T>
int apeq_mant(const TemplMoments<T>& f1, const TemplMoments<T>& f2, T prec)
{
  if(f1.get_qfill() == f2.get_qfill() &&
     apeq_mant(f1.get_xnorm(), f2.get_xnorm(), prec) &&
     apeq_mant(f1.get_xmean(), f2.get_xmean(), prec) &&
     apeq_mant(f1.get_sigma(), f2.get_sigma(), prec)
#ifdef INC_MINMAX
     &&
     apeq_mant(f1.get_xmin(), f2.get_xmin(), prec) &&
     apeq_mant(f1.get_xmin(), f2.get_xmin(), prec) 
#endif
     ) 
  {
    return 1;
  }
  else
  {
    return 0;
  }
}

template<class T>
int operator!=(const TemplMoments<T>& f1, const TemplMoments<T>& f2)
{
  if(f1 == f2) return 0;
  else return 1;
}

#endif

#ifndef MULTIPLY_H
#define MULTIPLY_H
/*
Various matrix operations performed upon arrays of DynLinArr and 
DynArr classes.

Copyright (c) 2001 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text 
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

#include "wcpplib/safetl/AbsArr.h"
#include "wcpplib/math/DoubleAc.h"
/*
In the following functions the preffix "norm" denotes calculation
of sum of squares, not just sum. Be careful.
*/
DoubleAc norm_DynLinArr(const DynLinArr<DoubleAc>& f);

template<class T>
T norm_DynLinArr(const DynLinArr<T>& f)
{
  long q=f.get_qel();
  T s(0);  // assumes that this clears the element
  long n;
  for(n=0; n<q; n++)
  {
    T t(f.acu(n));
    s = s + t*t;
  }
  return sqrt(s);
}

// Uses only selected elements
template<class T>
T norm_DynLinArr_part(const DynLinArr<T>& f, const DynLinArr<int>& s_use)
{
  mfunname("template<class T> T norm_DynLinArr_part(...)");
  long q=f.get_qel();
  check_econd12(q , != , s_use.get_qel() , mcerr);
  T s(0);  // assumes that this clears the element
  long n;
  for(n=0; n<q; n++)
  {
    if(s_use.acu(n) == 1)
    {
      T t(f.acu(n));
      s = s + t*t;
    }
  }
  return sqrt(s);
}

DoubleAc normsq_DynLinArr(const DynLinArr<DoubleAc>& f);

template<class T>
T normsq_DynLinArr(const DynLinArr<T>& f)
{
  long q=f.get_qel();
  T s(0);  // assumes that this clears the element
  long n;
  for(n=0; n<q; n++)
  {
    T t(f.acu(n));
    s = s + t*t;
  }
  return s;
}

template<class T>
T normsq_DynLinArr_part(const DynLinArr<T>& f, const DynLinArr<int>& s_use)
{
  mfunname("template<class T> T normsq_DynLinArr_part(...)");
  long q=f.get_qel();
  check_econd12(q , != , s_use.get_qel() , mcerr);
  T s(0);  // assumes that this clears the element
  long n;
  for(n=0; n<q; n++)
  {
    if(s_use.acu(n) == 1)
    {
      T t(f.acu(n));
      s = s + t*t;
    }
  }
  return s;
}

// Matrix multiplication of two matrises:
  
template<class T>
DynArr<T> operator*(const DynArr<T>& mt1, const DynArr<T>& mt2)
{
  mfunnamep("template<class T> DynArr<T> operator*(const DynArr<T>& mt1, const DynArr<T>& mt2)");
  check_econd11( mt1.get_qdim() , != 2 , mcerr);
  check_econd11( mt2.get_qdim() , > 2 , mcerr);
  check_econd11( mt2.get_qdim() , < 1 , mcerr);
  const DynLinArr<long>& qel_mt1(mt1.get_qel());
  const DynLinArr<long>& qel_mt2(mt2.get_qel());
  check_econd12( qel_mt1[1] , != , qel_mt2[0] , mcerr);
  if(mt2.get_qdim() == 2)  // otherwise 1
  {
    long q1=qel_mt1[0];
    long q2=qel_mt2[1];
    long q3=qel_mt1[1];
    DynArr<T> res(q1, q2);
    long n1, n2, n3;
    for(n1=0; n1<q1; n1++)
    {
      for(n2=0; n2<q2; n2++)
      {
	T t(0.0);
	for(n3=0; n3<q3; n3++)
	{
	  t += mt1.acu(n1,n3) * mt2.acu(n3,n2);
	}
	res.acu(n1,n2) = t;
      }
    }
    return res;
  }
  else
  {
    long q1=qel_mt1[0];
    long q3=qel_mt1[1];
    DynArr<T> res(q1);
    long n1, n2, n3;
    for(n1=0; n1<q1; n1++)
    {
      T t(0.0);
      for(n3=0; n3<q3; n3++)
      {
	t += mt1.acu(n1,n3) * mt2.acu(n3);
      }
      res.acu(n1) = t;
    }
    return res;
  }
}

template<class T>
DynLinArr<T> operator*(const DynArr<T>& mt, const DynLinArr<T>& vc)
{
  const DynLinArr<long>& qel_mt(mt.get_qel());
  if(qel_mt.get_qel() != 2)
  {
    mcerr<<"template<class T>\n"
	 <<"DynLinArr<T> operator*(const DynArr<T>& mt, "
	 <<"const DynLinArr<T>& vc):\n";
    mcerr<<"qel_mt.get_qel() != 2, qel_mt.get_qel() ="<<qel_mt.get_qel()<<'\n';
    spexit(mcerr);
  }
  long q=vc.get_qel();
  if(q != qel_mt[1])
  {
    mcerr<<"template<class T>\n"
	 <<"DynLinArr<T> operator*(const DynArr<T>& mt, "
	 <<"const DynLinArr<T>& vc):\n";
    mcerr<<"q != qel_mt[1], q ="<<q<<"qel_mt[1]="<<qel_mt[1]<<'\n';
    spexit(mcerr);
  }
  T s(0);  // assumes that this clears the element
  DynLinArr<T> res(qel_mt[0], s);
  long n1, n2;
  for(n1=0; n1<qel_mt[0]; n1++)
  {
    for(n2=0; n2<q; n2++)
    {
      res[n1] += mt.acu(n1,n2) * vc.acu(n2);
    }
  }
  return res;
}

//DynLinArr<DoubleAc> operator*(const DynArr<DoubleAc>& mt, 
//			      const DynLinArr<DoubleAc>& vc);


DynLinArr<DoubleAc> operator*(const DynArr<DoubleAc>& mt, 
			      const DynLinArr<double>& vc);
DynLinArr<DoubleAc> operator*(const DynArr<double>& mt, 
			      const DynLinArr<DoubleAc>& vc);


template<class T>
T operator*(const DynLinArr<T>& vc1, const DynLinArr<T>& vc2)
{
  //mcout<<"T operator*(const DynLinArr<T>& vc1, const DynLinArr<T>& vc2):\n";
  long q1=vc1.get_qel();
  long q2=vc2.get_qel();
  if(q1 != q2)
  {
    mcerr<<"template<class T>\n"
	 <<"DynLinArr<T> operator*(const DynArr<T>& mt, "
	 <<"const DynLinArr<T>& vc):\n";
    mcerr<<"q1 != q2, q1 ="<<q1<<"q2="<<q2<<'\n';
    spexit(mcerr);
  }
  T s(0);  // assumes that this clears the element
  long n;
  //mcout<<"s="<<s<<'\n';
  for(n=0; n<q1; n++)
  {
    s += vc1.acu(n) * vc2.acu(n);
    //mcout<<"vc1[n]="<<vc1[n]<<'\n';
    //mcout<<"vc2[n]="<<vc2[n]<<'\n';
    //mcout<<"vc1[n] * vc2[n]="<<vc1[n] * vc2[n]<<'\n';
    //mcout<<"s="<<s<<'\n';
  }
  return s;
}

//DoubleAc operator*(const DynLinArr<DoubleAc>& mt, 
//		   const DynLinArr<DoubleAc>& vc);

DoubleAc operator*(const DynLinArr<DoubleAc>& vc1, 
		   const DynLinArr<double>& vc2);
DoubleAc operator*(const DynLinArr<double>& vc1, 
		   const DynLinArr<DoubleAc>& vc2);


template<class T, class X>
DynLinArr<T> operator*(const DynLinArr<T>& ar, const X& t)
{
  long q = ar.get_qel();
  DynLinArr<T> res(q);
  long n;
  for(n=0; n<q; n++)
  {
    res.acu(n) = ar.acu(n) * t;
  }
  return res;
}

template<class T, class X>
DynLinArr<T>& operator*=(DynLinArr<T>& ar, const X& t)
{
  long q = ar.get_qel();
  long n;
  for(n=0; n<q; n++)
  {
    ar.acu(n) *= t;
  }
  return ar;
}

template<class T, class X>
DynLinArr<T> operator*(const X& t, const DynLinArr<T>& ar)
{
  mfunnamep("template<class T, class X> DynLinArr<T> operator*(const X& t, const DynLinArr<T>& ar)");
  long q = ar.get_qel();
  DynLinArr<T> res(q);
  long n;
  for(n=0; n<q; n++)
  {
    res.acu(n) = t * ar.acu(n);
  }
  return res;
}

template<class T, class X>
DynLinArr<T> operator/(const DynLinArr<T>& ar, const X& t)
{
  mfunname("DynLinArr<T> operator/(const DynLinArr<T>& ar, const X& t)");
  check_econd11(t , == 0 , mcerr);
  long q=ar.get_qel();
  DynLinArr<T> res(q);
  long n;
  for(n=0; n<q; n++)
  {
    res.acu(n) = ar.acu(n) / t;
  }
  return res;
}

template<class T, class X>
DynLinArr<T>& operator/=(DynLinArr<T>& ar, const X& t)
{
  mfunname("DynLinArr<T>& operator/=(DynLinArr<T>& ar, const X& t)");
  check_econd11(t , == 0 , mcerr);
  long q=ar.get_qel();
  long n;
  for(n=0; n<q; n++)
  {
    ar.acu(n) /= t;
  }
  return ar;
}


template<class T, class X>
DynArr<T> operator*(const DynArr<T>& mt, const X& t)
{
  mfunnamep("template<class T, class X> DynArr<T> operator*(const DynArr<T>& mt, const X& t)");
  DynArr<T> ms(mt.get_qel(), NULL);
  long qel_lin = mt.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    ms.acu_lin(n) = mt.acu_lin(n) * t;
  }
  /*
  DynArr<T> ms(mt);
  IterDynArr<T> iter(&ms);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) = (*at) * t ;
  }
  */
  return ms;
}

template<class T, class X>
DynArr<T> operator*(const X& t, const DynArr<T>& mt)
{
  mfunnamep("template<class T, class X> DynArr<T> operator*(const X& t, const DynArr<T>& mt)");
  DynArr<T> ms(mt.get_qel(), NULL);
  long qel_lin = mt.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    ms.acu_lin(n) = t * mt.acu_lin(n);
  }
  /*
  DynArr<T> ms(mt);
  IterDynArr<T> iter(&ms);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) = t * (*at);
  }
  */
  return ms;
}

template<class T, class X>
DynArr<T>& operator*=(DynArr<T>& mt, const X& t)
{
  mfunnamep("template<class T, class X> DynArr<T>& operator*=(DynArr<T>& mt, const X& t)");
  long qel_lin = mt.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    mt.acu_lin(n) *= t;
  }
  /*
  //DynArr<T> ms(mt);
  IterDynArr<T> iter(&mt);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) *=  t ;
  }
  */
  return mt;
}

template<class T, class X>
DynArr<T> operator/(const DynArr<T>& mt, const X& t)
{
  mfunnamep("template<class T, class X> DynArr<T> operator/(const DynArr<T>& mt, const X& t)");
  check_econd11(t , == 0 , mcerr);
  DynArr<T> ms(mt.get_qel(), NULL);
  long qel_lin = mt.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    ms.acu_lin(n) = mt.acu_lin(n) / t;
  }
  /*
  DynArr<T> ms(mt);
  IterDynArr<T> iter(&ms);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) = (*at) / t ;
  }
  */
  return ms;
}
template<class T, class X>
DynArr<T>& operator/=(DynArr<T>& mt, const X& t)
{
  mfunnamep("template<class T, class X> DynArr<T>& operator/(DynArr<T>& mt, const X& t)");
  check_econd11(t , == 0 , mcerr);
  long qel_lin = mt.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    mt.acu_lin(n) /= t;
  }
  /*
  //DynArr<T> ms(mt);
  IterDynArr<T> iter(&mt);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) = (*at) / t ;
  }
  */
  return mt;
}

template<class T>
DynLinArr<T> operator+(const DynLinArr<T>& vc1, const DynLinArr<T>& vc2)
{
  long q1=vc1.get_qel();
  long q2=vc2.get_qel();
  if(q1 != q2)
  {
    mcerr<<"template<class T>\n"
	 <<"DynLinArr<T> operator+(const DynLinArr<T>& vc1, "
	 <<"const DynLinArr<T>& vc2):\n";
    mcerr<<"q1 != q2, q1 ="<<q1<<"q2="<<q2<<'\n';
    spexit(mcerr);
  }
  DynLinArr<T> s(q1);  
  long n;
  for(n=0; n<q1; n++)
  {
    s.acu(n) = vc1.acu(n) + vc2.acu(n);
  }
  return s;
}

template<class T>
DynLinArr<T>& operator+=(DynLinArr<T>& vc1, const DynLinArr<T>& vc2)
{
  long q1=vc1.get_qel();
  long q2=vc2.get_qel();
  if(q1 != q2)
  {
    mcerr<<"template<class T>\n"
	 <<"DynLinArr<T>& operator+=(DynLinArr<T>& vc1, "
	 <<"const DynLinArr<T>& vc2):\n";
    mcerr<<"q1 != q2, q1 ="<<q1<<"q2="<<q2<<'\n';
    spexit(mcerr);
  }
  long n;
  for(n=0; n<q1; n++)
  {
    vc1.acu(n) += vc2.acu(n);
  }
  return vc1;
}

template<class T>
DynLinArr<T> operator-(const DynLinArr<T>& vc1, const DynLinArr<T>& vc2)
{
  long q1=vc1.get_qel();
  long q2=vc2.get_qel();
  if(q1 != q2)
  {
    mcerr<<"template<class T>\n"
	 <<"DynLinArr<T> operator-(const DynLinArr<T>& vc1, "
	 <<"const DynLinArr<T>& vc2):\n";
    mcerr<<"q1 != q2, q1 ="<<q1<<"q2="<<q2<<'\n';
    spexit(mcerr);
  }
  DynLinArr<T> s(q1);  
  long n;
  for(n=0; n<q1; n++)
  {
    s.acu(n) = vc1.acu(n) - vc2.acu(n);
  }
  return s;
}

template<class T>
DynLinArr<T>& operator-=(DynLinArr<T>& vc1, const DynLinArr<T>& vc2)
{
  long q1=vc1.get_qel();
  long q2=vc2.get_qel();
  if(q1 != q2)
  {
    mcerr<<"template<class T>\n"
	 <<"DynLinArr<T>& operator-=(DynLinArr<T>& vc1, "
	 <<"const DynLinArr<T>& vc2):\n";
    mcerr<<"q1 != q2, q1 ="<<q1<<"q2="<<q2<<'\n';
    spexit(mcerr);
  }
  long n;
  for(n=0; n<q1; n++)
  {
    vc1.acu(n) -= vc2.acu(n);
  }
  return vc1;
}

DynLinArr<DoubleAc> operator+(const DynLinArr<DoubleAc>& vc1, 
		   const DynLinArr<double>& vc2);
DynLinArr<DoubleAc> operator+(const DynLinArr<double>& vc1, 
		   const DynLinArr<DoubleAc>& vc2);
DynLinArr<DoubleAc> operator-(const DynLinArr<DoubleAc>& vc1, 
		   const DynLinArr<double>& vc2);
DynLinArr<DoubleAc> operator-(const DynLinArr<double>& vc1, 
		   const DynLinArr<DoubleAc>& vc2);
template<class T>
DynLinArr<T> operator-(const DynLinArr<T>& ar)
{   // creates local copy and returns it - may be inefficient
  long q = ar.get_qel();
  DynLinArr<T> s(q);  
  long n;
  for(n=0; n<q; n++)
  {
    s.acu(n) = -ar.acu(n);
  }
  return s;
}

template<class T>
void change_sign(DynLinArr<T>& ar)
{   // just change sign without copying total content,
  // but correspondent member function should exist for type of elements T
  long q = ar.get_qel();
  DynLinArr<T> s(q);  
  long n;
  for(n=0; n<q; n++)
  {
    change_sign(ar.acu(n));
  }
}

inline void change_sign(float& f)
{
  f = -f;
}

inline void change_sign(double& f)
{
  f = -f;
}

template<class T, class X>
DynLinArr<T>& operator+=(DynLinArr<T>& ar, const X& t)
{
  long q = ar.get_qel();
  long n;
  for(n=0; n<q; n++)
  {
    ar.acu(n) += t;
  }
  return ar;
}

template<class T, class X>
DynLinArr<T>& operator-=(DynLinArr<T>& ar, const X& t)
{
  long q = ar.get_qel();
  long n;
  for(n=0; n<q; n++)
  {
    ar.acu(n) -= t;
  }
  return ar;
}

template<class T>
DynArr<T> operator+(const DynArr<T>& mt1, const DynArr<T>& mt2)
{
  mfunnamep("template<class T> DynArr<T> operator+(const DynArr<T>& mt1, const DynArr<T>& mt2)");
  long qdim1=mt1.get_qdim();
  long qdim2=mt2.get_qdim();
  check_econd12(qdim1 , != , qdim2 , mcerr);
  const DynLinArr<long>& qe1=mt1.get_qel();
  const DynLinArr<long>& qe2=mt2.get_qel();
  check_econd12(qe1 , != , qe2 , mcerr);
  DynArr<T> ms(mt1.get_qel(), NULL);
  long qel_lin = mt1.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    ms.acu_lin(n) = mt1.acu_lin(n) + mt2.acu_lin(n);
  }

  /*
  DynArr<T> ms(mt1);  // initializes array and copy content.
  // it seems to be more economical, than to initialize, and to assign later.
  IterDynArr<T> iter(&ms);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) = (*at) + mt2.acu( iter.get_ncur() );
  }
  */
  return ms;
}

template<class T>
DynArr<T>& operator+=(DynArr<T>& mt1, const DynArr<T>& mt2)
{
  mfunnamep("template<class T> DynArr<T>& operator+(DynArr<T>& mt1, const DynArr<T>& mt2)");
  long qdim1=mt1.get_qdim();
  long qdim2=mt2.get_qdim();
  check_econd12(qdim1 , != , qdim2 , mcerr);
  const DynLinArr<long>& qe1=mt1.get_qel();
  const DynLinArr<long>& qe2=mt2.get_qel();
  check_econd12(qe1 , != , qe2 , mcerr);
  long qel_lin = mt1.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    mt1.acu_lin(n) += mt2.acu_lin(n);
  }
  /*
  IterDynArr<T> iter(&mt1);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) += mt2.acu( iter.get_ncur() );
  }
  */
  return mt1;
}

template<class T>
DynArr<T> operator-(const DynArr<T>& mt1, const DynArr<T>& mt2)
{
  mfunnamep("template<class T> DynArr<T> operator-(const DynArr<T>& mt1, const DynArr<T>& mt2)");
  long qdim1=mt1.get_qdim();
  long qdim2=mt2.get_qdim();
  check_econd12(qdim1 , != , qdim2 , mcerr);
  const DynLinArr<long>& qe1=mt1.get_qel();
  const DynLinArr<long>& qe2=mt2.get_qel();
  check_econd12(qe1 , != , qe2 , mcerr);
  DynArr<T> ms(mt1.get_qel(), NULL);
  long qel_lin = mt1.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    ms.acu_lin(n) = mt1.acu_lin(n) - mt2.acu_lin(n);
  }
  /*
  DynArr<T> ms(mt1);
  IterDynArr<T> iter(&ms);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) = (*at) - mt2.acu( iter.get_ncur() );
  }
  */
  return ms;
}

template<class T>
DynArr<T>& operator-=(DynArr<T>& mt1, const DynArr<T>& mt2)
{
  mfunnamep("template<class T> DynArr<T>& operator-(DynArr<T>& mt1, const DynArr<T>& mt2)");
  long qdim1=mt1.get_qdim();
  long qdim2=mt2.get_qdim();
  check_econd12(qdim1 , != , qdim2 , mcerr);
  const DynLinArr<long>& qe1=mt1.get_qel();
  const DynLinArr<long>& qe2=mt2.get_qel();
  check_econd12(qe1 , != , qe2 , mcerr);
  long qel_lin = mt1.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    mt1.acu_lin(n) -= mt2.acu_lin(n);
  }
  /*
  IterDynArr<T> iter(&mt1);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) = (*at) - mt2.acu( iter.get_ncur() );
  }
  */
  return mt1;
}

template<class T>
DynArr<T> operator-(const DynArr<T>& mt)
{
  mfunnamep("template<class T> DynArr<T> operator-(const DynArr<T>& mt)");
  //long qdim=mt.get_qdim();
  //const DynLinArr<long>& qe=mt.get_qel();
  DynArr<T> ms(mt.get_qel(), NULL);
  long qel_lin = mt.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    ms.acu_lin(n) -= mt.acu_lin(n);
  }
  /*
  DynArr<T> ms(mt);
  IterDynArr<T> iter(&ms);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) = -(*at);
  }
  */
  return ms;
}

template<class T>
void change_sign(DynArr<T>& mt)
{   // just change sign without copying total content,
  // but correspondent member function should exist for type of elements T
  long qel_lin = mt.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    change_sign(mt.acu_lin(n));
  }
  /*
  long qdim=mt.get_qdim();
  const DynLinArr<long>& qe=mt.get_qel();
  IterDynArr<T> iter(&mt);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    change_sign((*at));
  }
  */
}


template<class T, class X>
DynArr<T>& operator+=(DynArr<T>& mt, const X& t)
{
  mfunnamep("template<class T, class X> DynArr<T>& operator+=(DynArr<T>& mt, const X& t)");
  long qel_lin = mt.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    mt.acu_lin(n) += t;
  }
  /*
  //DynArr<T> ms(mt);
  IterDynArr<T> iter(&mt);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) += t ;
  }
  */
  return mt;
}

template<class T, class X>
DynArr<T>& operator-=(DynArr<T>& mt, const X& t)
{
  mfunnamep("template<class T, class X> DynArr<T>& operator-=(DynArr<T>& mt, const X& t)");
  long qel_lin = mt.get_qel_lin();
  long n;
  for(n=0; n<qel_lin; n++)
  {
    mt.acu_lin(n) += t;
  }
  /*
  //DynArr<T> ms(mt);
  IterDynArr<T> iter(&mt);
  T* at;
  while( (at=iter.more()) != NULL )
  {
    (*at) -= t ;
  }
  */
  return mt;
}

DynArr<DoubleAc> operator+(const DynArr<DoubleAc>& mt1, 
		   const DynArr<double>& mt2);
DynArr<DoubleAc> operator+(const DynArr<double>& mt1, 
		   const DynArr<DoubleAc>& mt2);
DynArr<DoubleAc> operator-(const DynArr<DoubleAc>& mt1, 
		   const DynArr<double>& mt2);
DynArr<DoubleAc> operator-(const DynArr<double>& mt1, 
		   const DynArr<DoubleAc>& mt2);



#endif

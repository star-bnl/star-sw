#ifndef ABS_INVERSE_H
#define ABS_INVERSE_H
/*
Copyright (c) 2001 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/

//#include "wcpplib/list/AbsArr.h"
//#include "wcpplib/math/DoubleAc.h"

/*
template<class M, class X> 
void abstract_inverse(M& mi, M& mr, long q, int& serr)
  // Both arrays are changed.
  // If can not inverse (diagonal elements get zero) serr=1, if all OK - serr=0
  // M any matrix class supporting access to elements by the function 
  // M.ac(nrow, ncol), both indexes start from 0.
  // X - class of type of element if this array.
  // It should support fabs(X) and ariphmetic operations.
{
  long nr, nr1, nc;
  for(nr=0; nr<q; nr++)
  {
    long nmax=0;
    double d=0;
    for(nr1=nr; nr1<q; nr1++)
    {
      if(fabs(mi.ac(nr1,nr)) > d)
      {
	d = fabs(mi.ac(nr1,nr));
	nmax = nr1;
      }
    }
    //mcout<<"d="<<d<<'\n';
    //mcout<<"nmax="<<nmax<<'\n';
    if(d == 0)
    {
      serr = 1;
      return;
    }
    if(nmax > nr)
    {
      for(nc=nr; nc<q; nc++)
      {
	X t(mi.ac(nr,nc));
	mi.ac(nr,nc) = mi.ac(nmax,nc);
	mi.ac(nmax,nc) = t;
      }
      for(nc=0; nc<q; nc++)
      {
	X t(mr.ac(nr,nc));
	mr.ac(nr,nc) = mr.ac(nmax,nc);
	mr.ac(nmax,nc) = t;
      }
      //long tl=order[nr];
      //order[nr] = order[nmax];
      //order[nmax] = tl;
    }
    X t=mi.ac(nr,nr);
    for(nr1=0; nr1<q; nr1++)
    {
      if(nr1 != nr)
      {
	X k(mi.ac(nr1,nr)/t);
	//mcout<<"nr1="<<nr1<<" nr="<<nr<<'\n';
	//mcout<<"k="<<k<<'\n';
	for(nc=nr; nc<q; nc++)
	{
	  mi.ac(nr1,nc) -= k * mi.ac(nr,nc);
	}
	for(nc=0; nc<q; nc++)
	{
	  mr.ac(nr1,nc) -= k * mr.ac(nr,nc);
	}
      }
    }
    for(nc=nr; nc<q; nc++)
    {
      mi.ac(nr,nc) /= t;
    }
    for(nc=0; nc<q; nc++)
    {
      mr.ac(nr,nc) /= t;
    }
  }
}
*/    
  
#define ALWAYS_USE_TEMPLATE_PAR_AS_FUN_PAR  // required by sun Solaris,
//                                             unknown version
#ifndef ALWAYS_USE_TEMPLATE_PAR_AS_FUN_PAR
  
template<class M, class X> 
X abstract_determinant(M& mi, long q)

#else

template<class M, class X> 
X abstract_determinant(M& mi, long q, X /*fict*/) // fict - fictitious parameters,
  // any value
#endif

  //X abstract_determinant(M& mi, long q, int& serr)
  // array mi is changed.
  // If can not calc (diagonal elements get zero) serr=1, if all OK - serr=0
  // M any matrix class supporting access to elements by the function 
  // M.ac(nrow, ncol), both indexes start from 0.
  // X - class of type of element if this array.
  // It should support fabs(X) and ariphmetic operations.
{
  if(q == 1)
  {
    return mi.ac(0,0);
  }
  else if(q == 2)
  {
    return mi.ac(0,0)*mi.ac(1,1) - mi.ac(0,1)*mi.ac(1,0);
  }
  else if(q == 3)
  {
    return 
      mi.ac(0,0)*mi.ac(1,1)*mi.ac(2,2) +
      mi.ac(0,2)*mi.ac(1,0)*mi.ac(2,1) +
      mi.ac(0,1)*mi.ac(1,2)*mi.ac(2,0) -
      mi.ac(0,2)*mi.ac(1,1)*mi.ac(2,0) -
      mi.ac(0,0)*mi.ac(1,2)*mi.ac(2,1) -
      mi.ac(0,1)*mi.ac(1,0)*mi.ac(2,2);
  }
  else
  {
    long nr, nr1, nc;
    X koef=1;
    for(nr=0; nr<q; nr++)
    {
      long nmax=0;
      double d=0;
      for(nr1=nr; nr1<q; nr1++)
      {
	if(fabs(mi.ac(nr1,nr)) > d)
	{
	  d = fabs(mi.ac(nr1,nr));
	  nmax = nr1;
	}
      }
      //mcout<<"d="<<d<<'\n';
      //mcout<<"nmax="<<nmax<<'\n';
      if(d == 0)
      {
	//serr = 1;
	return koef*mi.ac(nmax,nr);
      }
      if(nmax > nr)
      {
	for(nc=nr; nc<q; nc++)
	{
	  X t(mi.ac(nr,nc));
	  mi.ac(nr,nc) = mi.ac(nmax,nc);
	  mi.ac(nmax,nc) = t;
	}
	koef *= -1; // trancposition of rows: determinant changes sign
      }
      X t=mi.ac(nr,nr);
      for(nr1=nr+1; nr1<q; nr1++)
      {
	X k(mi.ac(nr1,nr)/t);
	//mcout<<"nr1="<<nr1<<" nr="<<nr<<'\n';
	//mcout<<"k="<<k<<'\n';
	for(nc=nr; nc<q; nc++)
	{
	  mi.ac(nr1,nc) -= k * mi.ac(nr,nc);
	}  // add elements of another row: the main value of
	// determinant is not affected (proven in linear algebra)
	// But the resolution gets worser.
      }  
      for(nc=nr; nc<q; nc++)
      {
	mi.ac(nr,nc) /= t;
      }
      koef *= t;
    }
    return koef;
  }
}
    

#endif

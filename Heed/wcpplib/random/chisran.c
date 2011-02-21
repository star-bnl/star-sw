#include "wcpplib/random/chisran.h"
#include "wcpplib/stream/prstream.h"
#include "wcpplib/util/FunNameStack.h"

// I. B. Smirnov, 2003.

float chispre(float *x, float *p, float *f, long q)
{
  mfunnamep("float chispre(float *x, float *p, float *f, long q)");
  check_econd11(q , <=0 , mcerr);
  long i;
  float r=0;
  for(i=0; i<q; i++)
  {
    check_econd11(p[i] , <0.0 , mcerr);
    r += p[i] * (x[i+1] - x[i]);
    f[i] = r;
  }
  check_econd11(r , <=0 , mcerr);
  for(i=0; i<q; i++)
    f[i] = f[i]/r;
  return r;
}

float chisran(float flat_random_number, float *x, float *f, long q)
{
  mfunnamep("float chisran(float flat_random_number, float *x, float *f, long q)");
  float xr,xl,yr,yl;
  long nl,nr,nc;
  check_econd11(q , <=0 , mcerr);
  check_econd21(flat_random_number , < 0.0 && , > 1.0  , mcerr);
  if(flat_random_number == 0.0)
  {
    long n;
    for(n=0; n<q; n++)
    {
      if(f[n] > 0.0)
      {
	return x[n];
      }
    }
  }
  else 
  {
    if(flat_random_number == 1.0)
    {
      long n;
      for(n=q-1; n>=0; n--)
      {
	if(f[n] < 1.0)
	{
	  return x[n+1];
	}
      }
    }
    else
    {
      if(flat_random_number <= f[0])
      {
	return flat_random_number / f[0];
      }
      else
      {
	nl=0; nr=q-1;
	while(nr-nl > 1)
	{
	  nc=(nr+nl)/2;
	  if(flat_random_number < f[nc])
	    nr=nc;
	  else
	    nl=nc;
	}
	xl=x[nl+1];   xr=x[nr+1];
	yl=f[nl];   yr=f[nr];
	float a=(xr-xl)/(yr-yl);
	float b=xl;
	return a*(flat_random_number-yl)+b;
      }
    }
  }
  funnw.ehdr(mcerr);
  mcerr<<"should never happen\n";
  spexit(mcerr);
  return 0.0;  // to quiet compiler
}

double chispre(DynLinArr< double >& f, int s_allow_zero_f)
{
  mfunnamep("double chispre(DynLinArr< double >& f, int s_allow_zero_f=0)");
  //check_econd12(p.get_qel() , != , f.get_qel() , mcerr);
  long q = f.get_qel();
  check_econd11(q , <=0 , mcerr);
  long i;
  double r=0;
  for(i=0; i<q; i++)
  {
    if(s_allow_zero_f==0)
    {
      check_econd11a(f[i] , <0.0 , "i="<<i<<'\n' , mcerr);
    }
    else
    {
      if(f[i] < 0.0)
      {
	mcout<<"Warning: f[i] < 0.0 in double chispre(DynLinArr< double >& f, int s_allow_zero_f)\n";
	Iprint2n(mcout, i, f[i]);
	f[i] = 0.0;
      }
    }
    r += f[i];
    f[i] = r;
  }
  check_econd11(r , <=0 , mcerr);
  for(i=0; i<q; i++)
    f[i] = f[i]/r;
  return r;
}

double chisran(double flat_random_number, DynLinArr < double >  f)
{
  mfunnamep("double chisran(double flat_random_number, DynLinArr < double >  f)");
  //mcout<<"chisran is started\n";
  //Iprintn(mcout, flat_random_number);
  double xr,xl,yr,yl;
  long nl,nr,nc;
  long q = f.get_qel();
  check_econd11(q , <=0 , mcerr);
  check_econd21(flat_random_number , < 0.0 && , > 1.0  , mcerr);
  if(flat_random_number == 0.0)
  {
    long n;
    for(n=0; n<q; n++)
    {
      if(f[n] > 0.0)
      {
	return double(n);
      }
    }
  }
  else 
  {
    if(flat_random_number == 1.0)
    {
      long n;
      for(n=q-1; n>=0; n--)
      {
	if(f[n] < 1.0)
	{
	  return double(n+1);
	}
      }
    }
    else
    {
      if(flat_random_number <= f[0])
      {
	return flat_random_number / f[0];
      }
      else
      {
	nl=0; nr=q-1;
	while(nr-nl > 1)
	{
	  nc=(nr+nl)/2;
	  if(flat_random_number < f[nc])
	    nr=nc;
	  else
	    nl=nc;
	}
	xl=double(nl+1);   xr=double(nr+1);
	yl=f[nl];   yr=f[nr];
	double a=(xr-xl)/(yr-yl);
	double b=xl;
	//Iprint3n(mcout, nl, nr, nc);
	//Iprint2n(mcout, xl, xr);
	//Iprint2n(mcout, yl, yr);
	//Iprint2n(mcout, a, b);
	return a*(flat_random_number-yl)+b;
      }
    }
  }
  funnw.ehdr(mcerr);
  mcerr<<"should never happen\n";
  spexit(mcerr);
  return 0.0;  // to quiet compiler
}





#ifndef RANLUXINT_H
#define RANLUXINT_H

#ifdef GARFIELD_HEED_INTERFACE

#include "Random.hh"

inline double SRANLUX() {

  return Garfield::RndmUniform();
  
}

#else

#ifdef USE_CPP_SRANLUX
// If this is the case 
// ranluxint.f is unnecessary
// SRANLUX is a function returning double.
// (In the case of fortran SRANLUX is function returning float.)

#include "wcpplib/random/WRandomEngine.h"
//#include <CLHEP/Random/RandomEngine.h>

//using namespace CLHEP;  // uncomment if below is CLHEP variant.

extern WHepRandomEngine& random_engine;

inline double SRANLUX(void)  // trying double
{
  return random_engine.flat();
}

#else // for  ifdef USE_CPP_RANLUX 
// getting rid of everything else

#include "wcpplib/cfortran/ftypes.h"
//#include <string.h>

//#define RANF_INSTEAD_OF_RNDM
#define RANLUX_INSTEAD_OF_RNDM    // currently only this option works

//#define PRINT_RANLUX  // for debug, allows to print in mcout
// sequencial random number which is returned and the number(index) of 
// this number, which allows to check random generator and program
/* Operation with seed for RANLUX:
There is a file with a fixed name ranluxsd.dat.
This name is fixed in fortran subroutines which write or read it:
myiniteranlux - reads,
mysaveranlux - writes.
There is also third subroutine which simply fills a vector: 
myextractranlux(vector).
With it the seed can be extracted into the program and written into its
output stream in order to grant to the user the possibility
to re-start calculations from any point.
From C++ these functions are available through:
inline void INITE_RANLUX(void);
inline void SAVE_RANLUX(void); 
inline void EXTRACT_RANLUX(fint* ivec);
where ivec has to have length 25 words.
For example, you can insert in the program the line:
fint ranlux_seed[25];

To put in the beginning:
  INITE_RANLUX();
In the end:
  SAVE_RANLUX();

and insert into the event loop the following sequence:
      if(eventcount.Gn()%1000 == 0)
      {
	EXTRACT_RANLUX(ranlux_seed); 
	mcout<<"ranlux seed:";
	int n;
	for(n=0; n<25; n++)
	{
	  //if(n % 8 == 0)
	    mcout<<'\n';
	  mcout<< ranlux_seed[n]<<' ';
	}
	mcout<<'\n';
	mcout.flush();
      }

*/



#ifdef PRINT_RANLUX
#include <iomanip.h>
#include <values.h>
#include "wcpplib/stream/prstream.h"
extern unsigned long num_ranlux;
const long step_of_print_ranlux = 1000;  // step of printing
#endif

#ifdef FORT_UNDERSCORES_02
#ifdef RANLUX_INSTEAD_OF_RNDM
extern "C" void myiniteranlux__(void);
extern "C" void mysaveranlux__(void);
extern "C" void myextractranlux__(fint* ivec);
extern "C" void ranlux__(ffloat* rvec, fint* len);
extern "C" ffloat sranlux__(void);
#else
#ifndef RANF_INSTEAD_OF_RNDM
extern "C" ffloat rndm__(fint* arg);
#else
extern "C" ffloat ranfl__(void);
#endif
extern "C" void myiniterndm__(void);
extern "C" void mysaverndm__(void);
extern "C" void myextractrndm__(fint* iseed);
#endif
extern "C" ffloat lranor__(ffloat* arg1, ffloat* arg2);
extern "C" void lspois__(ffloat* amu, fint* n, fint* ierror);
extern "C" void hisran__(ffloat* y, fint* n, ffloat* xlo, ffloat* xwid, 
			 ffloat* xran);
extern "C" ffloat flande__(ffloat* x);
 
#elif FORT_UNDERSCORES_01
#ifdef RANLUX_INSTEAD_OF_RNDM
extern "C" void myiniteranlux_(void);
extern "C" void mysaveranlux_(void);
extern "C" void myextractranlux_(fint* ivec);
extern "C" void ranlux_(ffloat* rvec, fint* len);
extern "C" ffloat sranlux_(void);
#else
#ifndef RANF_INSTEAD_OF_RNDM
extern "C" ffloat rndm_(fint* arg);
#else
extern "C" ffloat ranfl_(void);
#endif
extern "C" void myiniterndm_(void);
extern "C" void mysaverndm_(void);
extern "C" void myextractrndm_(fint* iseed);
#endif
extern "C" ffloat lranor_(ffloat* arg1, ffloat* arg2);
extern "C" void lspois_(ffloat* amu, fint* n, fint* ierror);
extern "C" void hisran_(ffloat* y, fint* n, ffloat* xlo, ffloat* xwid, 
			 ffloat* xran);
extern "C" ffloat flande_(ffloat* x);

#elif FORT_UNDERSCORES_11
#ifdef RANLUX_INSTEAD_OF_RNDM
extern "C" void _myiniteranlux_(void);
extern "C" void _mysaveranlux_(void);
extern "C" void _myextractranlux_(fint* ivec);
extern "C" void _ranlux_(ffloat* rvec, fint* len);
extern "C" ffloat _sranlux_(void);
#else
#ifndef RANF_INSTEAD_OF_RNDM
extern "C" ffloat _rndm_(fint* arg);
#else
extern "C" ffloat _ranfl_(void);
#endif
extern "C" void _myiniterndm_(void);
extern "C" void _mysaverndm_(void);
extern "C" void _myextractrndm_(fint* iseed);
#endif
extern "C" void _lspois_(ffloat* amu, fint* n, fint* ierror);
extern "C" ffloat _lranor_(ffloat* arg1, ffloat* arg2);
extern "C" void _hisran_(ffloat* y, fint* n, ffloat* xlo, ffloat* xwid, 
			 ffloat* xran);
extern "C" ffloat _flande_(ffloat* x);

#else
#ifdef RANLUX_INSTEAD_OF_RNDM
extern "C" void myiniteranlux(void);
extern "C" void mysaveranlux(void);
extern "C" void myextractranlux(fint* ivec);
extern "C" void ranlux(ffloat* rvec, fint* len);
extern "C" ffloat sranlux(void);
#else
#ifndef RANF_INSTEAD_OF_RNDM
extern "C" ffloat rndm(fint* arg);
#else
extern "C" ffloat ranfl(void);
#endif
extern "C" void myiniterndm(void);
extern "C" void mysaverndm(void);
extern "C" void myextractrndm(fint* iseed);
#endif
extern "C" void lspois(ffloat* amu, fint* n, fint* ierror);
extern "C" ffloat lranor(ffloat* arg1, ffloat* arg2);
extern "C" void hisran(ffloat* y, fint* n, ffloat* xlo, ffloat* xwid, 
			 ffloat* xran);
extern "C" ffloat flande(ffloat* x);

#endif

#ifdef RANLUX_INSTEAD_OF_RNDM

inline void INITE_RANLUX(void)
{
#ifdef FORT_UNDERSCORES_02
  myiniteranlux__();
#elif FORT_UNDERSCORES_01
  myiniteranlux_();
#elif FORT_UNDERSCORES_11
  _myiniteranlux_();
#else
  myiniteranlux();
#endif
}

inline void SAVE_RANLUX(void)
{
#ifdef FORT_UNDERSCORES_02
  mysaveranlux__();
#elif FORT_UNDERSCORES_01
  mysaveranlux_();
#elif FORT_UNDERSCORES_11
  _mysaveranlux_();
#else
  mysaveranlux();
#endif
}

inline void EXTRACT_RANLUX(fint* ivec)
{
#ifdef FORT_UNDERSCORES_02
  myextractranlux__(ivec);
#elif FORT_UNDERSCORES_01
  myextractranlux_(ivec);
#elif FORT_UNDERSCORES_11
  _myextractranlux_(ivec);
#else
  myextractranlux(ivec);
#endif
}

 
inline void RANLUX(ffloat* vec, fint len)
{
#ifdef FORT_UNDERSCORES_02
  ranlux__(vec, &len);
#elif FORT_UNDERSCORES_01
  ranlux_(vec, &len);
#elif FORT_UNDERSCORES_11
  _ranlux_(vec, &len);
#else
  ranlux(vec, &len);
#endif
#ifdef PRINT_RANLUX
  if(step_of_print_ranlux > 1)
  {
    int n;
    for(n=0; n<len; n++)
    {
      long rest = num_ranlux % step_of_print_ranlux;
      if(rest == 0)
      {
	int qp=mcout.precision();
	mcout.precision(FLT_DIG);
	mcout<<"RANLUX: num_ranlux="<<num_ranlux<<" r="<<vec[n]<<'\n';
	mcout.precision(qp);
      }
      num_ranlux++;
    }
  }
  else
  {
    mcout<<"RANLUX: len = "<<len<<'\n';
    indn.n+=2;
    int n;
    int qp=mcout.precision();
    mcout.precision(FLT_DIG);
    for(n=0; n<len; n++)
    {
      mcout<<"n="<<n<<"num_ranlux="<<num_ranlux++
	   <<" vec[n]="<<vec[n]<<'\n';
    }
    mcout.precision(qp);
    indn.n-=2;
  }
#endif
}

inline ffloat SRANLUX(void)
{
#ifndef PRINT_RANLUX

#ifdef FORT_UNDERSCORES_02
  return sranlux__();
#elif FORT_UNDERSCORES_01
  return sranlux_();
#elif FORT_UNDERSCORES_11
  return _sranlux_();
#else
  return sranlux();
#endif

#else // PRINT_RANLUX

#ifdef FORT_UNDERSCORES_02
  float r= sranlux__();
#elif FORT_UNDERSCORES_01
  float r= sranlux_();
#elif FORT_UNDERSCORES_11
  float r= _sranlux_();
#else
  float r= sranlux();
#endif
  long rest = 0;
  if(step_of_print_ranlux > 1)
  {
      rest = num_ranlux % step_of_print_ranlux;
  }
  if(rest == 0)
  {
    int qp=mcout.precision();
    mcout.precision(FLT_DIG);
    mcout<<"SRANLUX: num_ranlux="<<num_ranlux<<" r="<<r<<'\n';
    mcout.precision(qp);
  }
  num_ranlux++;
  return r;

#endif
}

#else // ifdef RANLUX_INSTEAD_OF_RNDM

#ifndef RANF_INSTEAD_OF_RNDM

inline ffloat RNDM(void)
{
#ifdef FORT_UNDERSCORES_02
  return rndm__(0);
#elif FORT_UNDERSCORES_01
  return rndm_(0);
#elif FORT_UNDERSCORES_11
  return _rndm_(0);
#else
  return rndm(0);
#endif
}
#else
inline ffloat RNDM(void)
{
#ifdef FORT_UNDERSCORES_02
  return ranfl__();
#elif FORT_UNDERSCORES_01
  return ranfl_();
#elif FORT_UNDERSCORES_11
  return _ranfl_();
#else
  return ranfl();
#endif
}
#endif
inline void INITE_RNDM(void)
{
#ifdef FORT_UNDERSCORES_02
  myiniterndm__();
#elif FORT_UNDERSCORES_01
  myiniterndm_();
#elif FORT_UNDERSCORES_11
  _myiniterndm_();
#else
  myiniterndm();
#endif
}
inline void SAVE_RNDM(void)
{
#ifdef FORT_UNDERSCORES_02
  mysaverndm__();
#elif FORT_UNDERSCORES_01
  mysaverndm_();
#elif FORT_UNDERSCORES_11
  _mysaverndm_();
#else
  mysaverndm();
#endif
}

inline void EXTRACT_RNDM(fint* iseed)
{
#ifdef FORT_UNDERSCORES_02
  myextractrndm__(iseed);
#elif FORT_UNDERSCORES_01
  myextractrndm_(iseed);
#elif FORT_UNDERSCORES_11
  _myextractrndm_(iseed);
#else
  myextractrndm(iseed);
#endif
}

#endif  //  ifdef RANLUX_INSTEAD_OF_RNDM

inline ffloat LRANOR(ffloat* arg1, ffloat* arg2)
{
#ifdef FORT_UNDERSCORES_02
  return lranor__(arg1, arg2);
#elif FORT_UNDERSCORES_01
  return lranor_(arg1, arg2);
#elif FORT_UNDERSCORES_11
  return _lranor_(arg1, arg2);
#else
  return lranor(arg1, arg2);
#endif
}

inline void LSPOIS(ffloat* amu, fint& n, fint& ierror)
{
#ifdef FORT_UNDERSCORES_02
  lspois__(amu, &n, &ierror);
#elif FORT_UNDERSCORES_01
  lspois_(amu, &n, &ierror);
#elif FORT_UNDERSCORES_11
  _lspois_(amu, &n, &ierror);
#else
  lspois(amu, &n, &ierror);
#endif
}

inline void HISRAN(ffloat* y, fint n, ffloat xlo, ffloat xwid, 
	      ffloat& xran)
{
#ifdef FORT_UNDERSCORES_02
  hisran__(y, &n, &xlo, &xwid, &xran);
#elif FORT_UNDERSCORES_01
  hisran_(y, &n, &xlo, &xwid, &xran);
#elif FORT_UNDERSCORES_11
  _hisran_(y, &n, &xlo, &xwid, &xran);
#else
  hisran(y, &n, &xlo, &xwid, &xran);
#endif
}

inline ffloat FLANDE(ffloat* x)
{
#ifdef FORT_UNDERSCORES_02
  return flande__(x);
#elif FORT_UNDERSCORES_01
  return flande_(x);
#elif FORT_UNDERSCORES_11
  return _flande_(x);
#else
  return flande(x);
#endif
}

#endif // for  ifdef USE_CPP_RANLUX


#endif


#endif

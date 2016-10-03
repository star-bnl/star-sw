/*
	clooptools.h
		the C++ header file with all definitions for LoopTools
		this file is part of LoopTools
		last modified 22 Jul 04 th
*/

#ifndef __CLOOPTOOLS_H__
#define __CLOOPTOOLS_H__

#include <complex.h>
#include "ltproto.h"

#define cc0 1
#define cc1 2
#define cc2 3
#define cc00 4
#define cc11 5
#define cc12 6
#define cc22 7
#define cc001 8
#define cc002 9
#define cc111 10
#define cc112 11
#define cc122 12
#define cc222 13

#define dd0 1
#define dd1 2
#define dd2 3
#define dd3 4
#define dd00 5
#define dd11 6
#define dd12 7
#define dd13 8
#define dd22 9
#define dd23 10
#define dd33 11
#define dd001 12
#define dd002 13
#define dd003 14
#define dd111 15
#define dd112 16
#define dd113 17
#define dd122 18
#define dd123 19
#define dd133 20
#define dd222 21
#define dd223 22
#define dd233 23
#define dd333 24
#define dd0000 25
#define dd0011 26
#define dd0012 27
#define dd0013 28
#define dd0022 29
#define dd0023 30
#define dd0033 31
#define dd1111 32
#define dd1112 33
#define dd1113 34
#define dd1122 35
#define dd1123 36
#define dd1133 37
#define dd1222 38
#define dd1223 39
#define dd1233 40
#define dd1333 41
#define dd2222 42
#define dd2223 43
#define dd2233 44
#define dd2333 45
#define dd3333 46


#define Cval(id, pos) Ccache(pos + id)
#define Dval(id, pos) Dcache(pos + id)

#define bcaini ffini
#define bcaexi ffexi


static inline double_complex A0(const double m)
{
  dcomplex result;

  A0(&result, &m);
  return double_complex(result.r, result.i);
}


static inline double_complex B0(const double p,
  const double m1, const double m2)
{
  dcomplex result;

  B0(&result, &p, &m1, &m2);
  return double_complex(result.r, result.i);
}


static inline double_complex DB0(const double p,
  const double m1, const double m2)
{
  dcomplex result;

  DB0(&result, &p, &m1, &m2);
  return double_complex(result.r, result.i);
}


static inline double_complex B1(const double p,
  const double m1, const double m2)
{
  dcomplex result;

  B1(&result, &p, &m1, &m2);
  return double_complex(result.r, result.i);
}


static inline double_complex DB1(const double p,
  const double m1, const double m2)
{
  dcomplex result;

  DB1(&result, &p, &m1, &m2);
  return double_complex(result.r, result.i);
}


static inline double_complex B00(const double p,
  const double m1, const double m2)
{
  dcomplex result;

  B00(&result, &p, &m1, &m2);
  return double_complex(result.r, result.i);
}


static inline double_complex DB00(const double p,
  const double m1, const double m2)
{
  dcomplex result;

  DB00(&result, &p, &m1, &m2);
  return double_complex(result.r, result.i);
}


static inline double_complex B11(const double p,
  const double m1, const double m2)
{
  dcomplex result;

  B11(&result, &p, &m1, &m2);
  return double_complex(result.r, result.i);
}


static inline double_complex DB11(const double p,
  const double m1, const double m2)
{
  dcomplex result;

  DB11(&result, &p, &m1, &m2);
  return double_complex(result.r, result.i);
}


static inline double_complex C0(const double p1,
  const double p2, const double p1p2,
  const double m1, const double m2, const double m3)
{
  dcomplex result;

  C0(&result, &p1, &p2, &p1p2, &m1, &m2, &m3);
  return double_complex(result.r, result.i);
}


static inline double_complex C0i(const int id, const double p1,
  const double p2, const double p1p2,
  const double m1, const double m2, const double m3)
{
  dcomplex result;

  C0i(&result, &id, &p1, &p2, &p1p2, &m1, &m2, &m3);
  return double_complex(result.r, result.i);
}


static inline int Cget(const double p1,
  const double p2, const double p1p2,
  const double m1, const double m2, const double m3)
{
  return Cget(&p1, &p2, &p1p2, &m1, &m2, &m3);
}


static inline double_complex D0(const double p1,
  const double p2, const double p3, const double p4,
  const double p1p2, const double p2p3,
  const double m1, const double m2, const double m3, const double m4)
{
  dcomplex result;

  D0(&result, &p1, &p2, &p3, &p4, &p1p2, &p2p3, &m1, &m2, &m3, &m4);
  return double_complex(result.r, result.i);
}


static inline double_complex D0i(const int id,
  const double p1, const double p2, const double p3, const double p4,
  const double p1p2, const double p2p3,
  const double m1, const double m2, const double m3, const double m4)
{
  dcomplex result;

  D0i(&result, &id, &p1, &p2, &p3, &p4, &p1p2, &p2p3, &m1, &m2, &m3, &m4);
  return double_complex(result.r, result.i);
}


static inline int Dget(const double p1,
  const double p2, const double p3, const double p4,
  const double p1p2, const double p2p3,
  const double m1, const double m2, const double m3, const double m4)
{
  return Dget(&p1, &p2, &p3, &p4, &p1p2, &p2p3, &m1, &m2, &m3, &m4);
}


static inline void setmudim(const double newmudim)
{
  setmudim(&newmudim);
}

static inline void setdelta(const double newdelta)
{
  setdelta(&newdelta);
}

static inline void setlambda(const double newlambda)
{
  setlambda(&newlambda);
}

static inline void setcachelast(const dcomplex *buffer, const int offset)
{
  setcachelast(buffer, &offset);
}

#endif


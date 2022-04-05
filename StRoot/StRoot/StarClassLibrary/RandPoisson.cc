/***************************************************************************
 *
 * $Id: RandPoisson.cc,v 1.4 2016/01/22 17:10:50 smirnovd Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:
 *             RandPoisson.cc,v 1.4 1998/02/04 23:22:00
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                         --- RandPoisson ---
 *                      class implementation file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 ***************************************************************************
 *
 * $Log: RandPoisson.cc,v $
 * Revision 1.4  2016/01/22 17:10:50  smirnovd
 * StarClassLibrary: Removed deprecated storage class specifier 'register'
 *
 * This keyword is deprecated since C++11 and serves no purpose
 *
 * "
 * The register specifier is only allowed for objects declared at block scope and
 * in function parameter lists. It indicates automatic storage duration, which is
 * the default for these kinds of declarations. Additionally, the presence of this
 * keyword may be used as a hint for the optimizer to store the value of this
 * variable in a CPU register.
 * "
 *
 * Revision 1.3  2003/09/02 17:59:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  1999/12/07 23:43:04  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.1  1999/01/30 03:59:00  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:09  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "RandPoisson.h"

RandPoisson::~RandPoisson() {
  if ( deleteEngine ) delete localEngine;
}

HepDouble RandPoisson::operator()() {
  return HepDouble(fire());
}

HepDouble gammln(HepDouble xx) {

// Returns the value ln(Gamma(xx) for xx > 0.  Full accuracy is obtained for 
// xx > 1. For 0 < xx < 1. the reflection formula (6.1.4) can be used first.
// (Adapted from Numerical Recipes in C)

  static HepDouble cof[6] = {76.18009172947146,-86.50532032941677,
                             24.01409824083091, -1.231739572450155,
                             0.1208650973866179e-2, -0.5395239384953e-5};
  HepInt j;
  HepDouble x = xx - 1.0;
  HepDouble tmp = x + 5.5;
  tmp -= (x + 0.5) * ::log(tmp);
  HepDouble ser = 1.000000000190015;

  for ( j = 0; j <= 5; j++ ) {
    x += 1.0;
    ser += cof[j]/x;
  }
  return -tmp + ::log(2.5066282746310005*ser);
}

long RandPoisson::shoot(HepDouble xm) {

// Returns as a floating-point number an integer value that is a random
// deviation drawn from a Poisson distribution of mean xm, using flat()
// as a source of uniform random numbers.
// (Adapted from Numerical Recipes in C)

  HepDouble em;
  HepDouble t, y;
  HepDouble sq, alxm, g;
  HepDouble om = getOldMean();

  HepDouble* status = getPStatus();
  sq = status[0];
  alxm = status[1];
  g = status[2];

  if( xm == -1 ) return 0;
  if( xm < 12.0 ) {
    if( xm != om ) {
      setOldMean(xm);
      g = exp(-xm);
    }
    em = -1;
    t = 1.0;
    do {
      em += 1.0;
      t *= HepRandom::getTheGenerator()->flat();
    } while( t > g );
  }
  else if ( xm < getMaxMean() ) {
    if ( xm != om ) {
      setOldMean(xm);
      sq = ::sqrt(2.0*xm);
      alxm = ::log(xm);
      g = xm*alxm - gammln(xm + 1.0);
    }
    do {
      do {
	y = tan(M_PI*HepRandom::getTheGenerator()->flat());
	em = sq*y + xm;
      } while( em < 0.0 );
      em = floor(em);
      t = 0.9*(1.0 + y*y)* exp(em*alxm - gammln(em + 1.0) - g);
    } while( HepRandom::getTheGenerator()->flat() > t );
  }
  else {
    if ( xm != om ) {
      setOldMean(xm);
      sq = ::sqrt(2.0*xm);
      alxm = ::log(xm);
      g = xm*alxm - gammln(xm + 1.0);
    }
    em = xm;
  }    
  setPStatus(sq,alxm,g);
  return long(em);
}

void RandPoisson::shootArray(const HepInt size, long* vect, HepDouble m)
{
   for (int i=0; i<size; ++i)
     vect[i] = shoot(m);
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandPoisson::shootArray(vector<long>& vec, HepDouble m)
#else
RandPoisson::shootArray(vector<long,allocator<long> >& vec, HepDouble m)
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = shoot(m);
}

long RandPoisson::shoot(HepRandomEngine* anEngine, HepDouble xm) {

// Returns as a floating-point number an integer value that is a random
// deviation drawn from a Poisson distribution of mean xm, using flat()
// of a given Random Engine as a source of uniform random numbers.
// (Adapted from Numerical Recipes in C)

  HepDouble em;
  HepDouble t, y;
  HepDouble sq, alxm, g;
  HepDouble om = getOldMean();

  HepDouble* status = getPStatus();
  sq = status[0];
  alxm = status[1];
  g = status[2];

  if( xm == -1 ) return 0;
  if( xm < 12.0 ) {
    if( xm != om ) {
      setOldMean(xm);
      g = exp(-xm);
    }
    em = -1;
    t = 1.0;
    do {
      em += 1.0;
      t *= anEngine->flat();
    } while( t > g );
  }
  else if ( xm < getMaxMean() ) {
    if ( xm != om ) {
      setOldMean(xm);
      sq = ::sqrt(2.0*xm);
      alxm = ::log(xm);
      g = xm*alxm - gammln(xm + 1.0);
    }
    do {
      do {
	y = tan(M_PI*anEngine->flat());
	em = sq*y + xm;
      } while( em < 0.0 );
      em = floor(em);
      t = 0.9*(1.0 + y*y)* exp(em*alxm - gammln(em + 1.0) - g);
    } while( anEngine->flat() > t );
  }
  else {
    if ( xm != om ) {
      setOldMean(xm);
      sq = ::sqrt(2.0*xm);
      alxm = ::log(xm);
      g = xm*alxm - gammln(xm + 1.0);
    }
    em = xm;
  }    
  setPStatus(sq,alxm,g);
  return long(em);
}

void RandPoisson::shootArray(HepRandomEngine* anEngine, const HepInt size,
                             long* vect, HepDouble m)
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = shoot(anEngine,m);
}
void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandPoisson::shootArray(HepRandomEngine* anEngine,
                             vector<long>& vec, HepDouble m)
#else
RandPoisson::shootArray(HepRandomEngine* anEngine,
                             vector<long,allocator<long> >& vec, HepDouble m)
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = shoot(anEngine,m);
}
long RandPoisson::fire(HepDouble xm) {

// Returns as a floating-point number an integer value that is a random
// deviation drawn from a Poisson distribution of mean xm, using flat()
// as a source of uniform random numbers.
// (Adapted from Numerical Recipes in C)

  HepDouble em;
  HepDouble t, y;
  HepDouble sq, alxm, g;

  sq = status[0];
  alxm = status[1];
  g = status[2];

  if( xm == -1 ) return 0;
  if( xm < 12.0 ) {
    if( xm != oldm ) {
      oldm = xm;
      g = exp(-xm);
    }
    em = -1;
    t = 1.0;
    do {
      em += 1.0;
      t *= localEngine->flat();
    } while( t > g );
  }
  else if ( xm < meanMax ) {
    if ( xm != oldm ) {
      oldm = xm;
      sq = ::sqrt(2.0*xm);
      alxm = ::log(xm);
      g = xm*alxm - gammln(xm + 1.0);
    }
    do {
      do {
	y = tan(M_PI*localEngine->flat());
	em = sq*y + xm;
      } while( em < 0.0 );
      em = floor(em);
      t = 0.9*(1.0 + y*y)* exp(em*alxm - gammln(em + 1.0) - g);
    } while( localEngine->flat() > t );
  }
  else {
    if ( xm != oldm ) {
      oldm = xm;
      sq = ::sqrt(2.0*xm);
      alxm = ::log(xm);
      g = xm*alxm - gammln(xm + 1.0);
    }
    em = xm;
  }    
  status[0] = sq; status[1] = alxm; status[2] = g;
  return long(em);
}

void RandPoisson::fireArray(const HepInt size, long* vect, HepDouble m)
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = fire(m);
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandPoisson::fireArray(vector<long>& vec, HepDouble m)
#else
RandPoisson::fireArray(vector<long,allocator<long> >& vec, HepDouble m)
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = fire(m);
}

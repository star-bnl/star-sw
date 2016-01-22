/***************************************************************************
 *
 * $Id: RandGauss.cc,v 1.4 2016/01/22 17:10:50 smirnovd Exp $
 *
 * Author: Gabriele Cosmo - Created: 5th September 1995
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *               RandGauss.cc,v 1.5 1997/08/12 00:38:43
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                          --- RandGauss ---
 *                      class implementation file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 ***************************************************************************
 *
 * $Log: RandGauss.cc,v $
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
 * Revision 1.1  1999/01/23 00:29:08  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "RandGauss.h"

RandGauss::~RandGauss() {
  if ( deleteEngine ) delete localEngine;
}

HepDouble RandGauss::operator()() {
  return fire();
}

HepDouble RandGauss::shoot()
{
  // Gaussian random numbers are generated two at the time, so every other
  // time this is called we just return a number generated the time before.

  if ( getFlag() ) {
    setFlag(false);
    return getVal();
  }

  HepDouble r;
  HepDouble v1,v2,fac,val;

  do {
    v1 = 2.0 * HepRandom::getTheGenerator()->flat() - 1.0;
    v2 = 2.0 * HepRandom::getTheGenerator()->flat() - 1.0;
    r = v1*v1 + v2*v2;
  } while ( r > 1.0 );

  fac = ::sqrt(-2.0*::log(r)/r);
  val = v1*fac;
  setVal(val);
  setFlag(true);
  return v2*fac;
}

void RandGauss::shootArray( const HepInt size, HepDouble* vect,
                            HepDouble mean, HepDouble stdDev )
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = shoot(mean,stdDev);
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandGauss::shootArray( vector<HepDouble>& vec,
                            HepDouble mean, HepDouble stdDev )
#else
RandGauss::shootArray( vector<HepDouble,allocator<HepDouble> >& vec,
                            HepDouble mean, HepDouble stdDev )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = shoot(mean,stdDev);
}

HepDouble RandGauss::shoot( HepRandomEngine* anEngine )
{
  // Gaussian random numbers are generated two at the time, so every other
  // time this is called we just return a number generated the time before.

  if ( getFlag() ) {
    setFlag(false);
    return getVal();
  }

  HepDouble r;
  HepDouble v1,v2,fac,val;

  do {
    v1 = 2.0 * anEngine->flat() - 1.0;
    v2 = 2.0 * anEngine->flat() - 1.0;
    r = v1*v1 + v2*v2;
  } while ( r > 1.0 );

  fac = ::sqrt( -2.0*::log(r)/r);
  val = v1*fac;
  setVal(val);
  setFlag(true);
  return v2*fac;
}

void RandGauss::shootArray( HepRandomEngine* anEngine,
                            const HepInt size, HepDouble* vect,
                            HepDouble mean, HepDouble stdDev )
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = shoot(anEngine,mean,stdDev);
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandGauss::shootArray( HepRandomEngine* anEngine,
		       vector<HepDouble>& vec,
		       HepDouble mean, HepDouble stdDev )
#else
RandGauss::shootArray( HepRandomEngine* anEngine,
		       vector<HepDouble,allocator<HepDouble> >& vec,
		       HepDouble mean, HepDouble stdDev )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = shoot(anEngine,mean,stdDev);
}


HepDouble RandGauss::fire()
{
  // Gaussian random numbers are generated two at the time, so every other
  // time this is called we just return a number generated the time before.

  if ( set ) {
    set = false;
    return nextGauss;
  }

  HepDouble r;
  HepDouble v1,v2,fac,val;

  do {
    v1 = 2.0 * localEngine->flat() - 1.0;
    v2 = 2.0 * localEngine->flat() - 1.0;
    r = v1*v1 + v2*v2;
  } while ( r > 1.0 );

  fac = ::sqrt(-2.0*::log(r)/r);
  val = v1*fac;
  nextGauss = val;
  set = true;
  return v2*fac;
}

void RandGauss::fireArray( const HepInt size, HepDouble* vect,
                           HepDouble mean, HepDouble stdDev )
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = fire(mean,stdDev);
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandGauss::fireArray( vector<HepDouble>& vec,
		      HepDouble mean, HepDouble stdDev )
#else
RandGauss::fireArray( vector<HepDouble,allocator<HepDouble> >& vec,
		      HepDouble mean, HepDouble stdDev )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = fire(mean,stdDev);
}

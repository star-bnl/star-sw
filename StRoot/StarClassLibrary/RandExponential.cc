/***************************************************************************
 *
 * $Id: RandExponential.cc,v 1.3 2016/01/22 17:10:49 smirnovd Exp $
 *
 * Author: Gabriele Cosmo - Created: 17th May 1996
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *              RandExponential.cc,v 1.3 1997/08/12 00:38:38 gcosmo
 * -----------------------------------------------------------------------
 *                              HEP Random
 *                        --- RandExponential ---
 *                      class implementation file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 ***************************************************************************
 *
 * $Log: RandExponential.cc,v $
 * Revision 1.3  2016/01/22 17:10:49  smirnovd
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
 * Revision 1.2  1999/12/07 23:43:04  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.1  1999/01/30 03:59:00  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:06  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "RandExponential.h"

RandExponential::~RandExponential() {
  if ( deleteEngine ) delete localEngine;
}

HepDouble RandExponential::operator()() {
  return fire();
}

void RandExponential::shootArray( const HepInt size, HepDouble* vect,
                                  HepDouble mean )
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = shoot(mean);
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandExponential::shootArray( vector<HepDouble>& vec,
                                  HepDouble mean )
#else
RandExponential::shootArray( vector<HepDouble,allocator<HepDouble> >& vec,
                                  HepDouble mean )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = shoot(mean);
}

void RandExponential::shootArray( HepRandomEngine* anEngine, const HepInt size,
                                  HepDouble* vect, HepDouble mean )
{
   for (int i=0; i<size; ++i)
     vect[i] = shoot(anEngine, mean);
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandExponential::shootArray( HepRandomEngine* anEngine,
			     vector<HepDouble>& vec, HepDouble mean )
#else
RandExponential::shootArray( HepRandomEngine* anEngine,
			     vector<HepDouble, allocator<HepDouble> >& vec,
			     HepDouble mean )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = shoot(anEngine, mean);
}

void RandExponential::fireArray( const HepInt size, HepDouble* vect,
                                 HepDouble mean )
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = fire(mean);
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandExponential::fireArray( vector<HepDouble>& vec,
                                 HepDouble mean )
#else
RandExponential::fireArray( vector<HepDouble, allocator<HepDouble> >& vec,
                                 HepDouble mean )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = fire(mean);
}

/***************************************************************************
 *
 * $Id: RandFlat.cc,v 1.3 2016/01/22 17:10:50 smirnovd Exp $
 *
 * Author:  Gabriele Cosmo - Created: 17th May 1995
 *          modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *                  RandFlat.cc,v 1.3 1997/08/12 00:38:40
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                          --- RandFlat ---
 *                      class implementation file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 ***************************************************************************
 *
 * $Log: RandFlat.cc,v $
 * Revision 1.3  2016/01/22 17:10:50  smirnovd
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
 * Revision 1.1  1999/01/23 00:29:07  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "RandFlat.h"

const HepInt RandFlat::MSBBits= 15;
const unsigned long RandFlat::MSB= 1ul<<RandFlat::MSBBits;
unsigned long RandFlat::staticRandomInt= 0;
unsigned long RandFlat::staticFirstUnusedBit= 0;

RandFlat::~RandFlat() {
  if ( deleteEngine ) delete localEngine;
}

HepDouble RandFlat::operator()() {
  return fire();
}

void RandFlat::shootArray( const HepInt size, HepDouble* vect,
                           HepDouble lx, HepDouble dx  )
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = shoot(lx,dx);
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandFlat::shootArray( vector<HepDouble>& vec, HepDouble lx, HepDouble dx )
#else
RandFlat::shootArray( vector<HepDouble,allocator<HepDouble> >& vec, HepDouble lx, HepDouble dx )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = shoot(lx,dx);
}

void RandFlat::shootArray( HepRandomEngine* anEngine,
                           const HepInt size, HepDouble* vect,
                           HepDouble lx, HepDouble dx  )
{
   for (int i=0; i<size; ++i)
     vect[i] = shoot(anEngine,lx,dx);
}


#ifndef ST_NO_TEMPLATE_DEF_ARGS
void RandFlat::shootArray( HepRandomEngine* anEngine,
		      vector<HepDouble>& vec,
		      HepDouble lx, HepDouble dx  )
#else
void RandFlat::shootArray( HepRandomEngine* anEngine,
		      vector<HepDouble,allocator<HepDouble> >& vec,
		      HepDouble lx, HepDouble dx  )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = shoot(anEngine,lx,dx);
}

void RandFlat::fireArray( const HepInt size, HepDouble* vect,
                          HepDouble lx, HepDouble dx  )
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = fire(lx,dx);
}

#ifndef ST_NO_TEMPLATE_DEF_ARGS
void RandFlat::fireArray( vector<HepDouble>& vec, HepDouble lx, HepDouble dx )
#else
void RandFlat::fireArray( vector<HepDouble,allocator<HepDouble> >& vec, HepDouble lx, HepDouble dx )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = fire(lx,dx);
}

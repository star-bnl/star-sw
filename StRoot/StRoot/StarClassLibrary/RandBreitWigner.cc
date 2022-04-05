/***************************************************************************
 *
 * $Id: RandBreitWigner.cc,v 1.5 2016/01/22 17:10:49 smirnovd Exp $
 *
 * Author: Gabriele Cosmo - Created: 5th September 1995
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *               RandBreitWigner.cc,v 1.3 1997/08/12 00:38:37
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                       --- RandBreitWigner ---
 *                      class implementation file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 ***************************************************************************
 *
 * $Log: RandBreitWigner.cc,v $
 * Revision 1.5  2016/01/22 17:10:49  smirnovd
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
 * Revision 1.4  2003/09/02 17:59:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  1999/12/21 15:13:53  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.2  1999/12/07 23:43:04  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.1  1999/01/30 03:58:59  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:03  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "RandBreitWigner.h"
#include <algorithm>        // for max(), tu
#if !defined(ST_NO_NAMESPACES)
using std::max;
#endif

RandBreitWigner::~RandBreitWigner() {
  if ( deleteEngine ) delete localEngine;
}

HepDouble RandBreitWigner::operator()() {
   return fire();
}

HepDouble RandBreitWigner::shoot(HepDouble mean, HepDouble gamma)
{
   HepDouble rval, displ;

   rval = 2.0*HepRandom::getTheGenerator()->flat()-1.0;
   displ = 0.5*gamma*tan(rval*M_PI_2);

   return mean + displ;
}

HepDouble RandBreitWigner::shoot(HepDouble mean, HepDouble gamma, HepDouble cut)
{
   HepDouble val, rval, displ;

   if ( gamma == 0.0 ) return mean;
   val = atan(2.0*cut/gamma);
   rval = 2.0*HepRandom::getTheGenerator()->flat()-1.0;
   displ = 0.5*gamma*tan(rval*val);

   return mean + displ;
}

HepDouble RandBreitWigner::shootM2(HepDouble mean, HepDouble gamma )
{
   HepDouble val, rval, displ;

   if ( gamma == 0.0 ) return mean;
   val = atan(-mean/gamma);
   rval = RandFlat::shoot(val, M_PI_2);
   displ = gamma*tan(rval);

   return ::sqrt(mean*mean + mean*displ);
}

HepDouble RandBreitWigner::shootM2(HepDouble mean, HepDouble gamma, HepDouble cut )
{
   HepDouble rval, displ;
   HepDouble lower, upper, tmp;

   if ( gamma == 0.0 ) return mean;
   tmp = max(0.0,(mean-cut));
   lower = atan( (tmp*tmp-mean*mean)/(mean*gamma) );
   upper = atan( ((mean+cut)*(mean+cut)-mean*mean)/(mean*gamma) );
   rval = RandFlat::shoot(lower, upper);
   displ = gamma*tan(rval);

   return ::sqrt(max(0.0, mean*mean + mean*displ));
}


void RandBreitWigner::shootArray ( const HepInt size, HepDouble* vect,
                                   HepDouble a, HepDouble b,
                                   HepDouble c )
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = shoot( a, b, c );
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandBreitWigner::shootArray ( vector<HepDouble>& vec,
                                   HepDouble a, HepDouble b,
                                   HepDouble c )
#else
RandBreitWigner::shootArray ( vector<HepDouble, allocator<HepDouble> >& vec,
                                   HepDouble a, HepDouble b,
                                   HepDouble c )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = shoot( a, b, c );
}

//----------------

HepDouble RandBreitWigner::shoot(HepRandomEngine* anEngine,
                                 HepDouble mean, HepDouble gamma)
{
   HepDouble rval, displ;

   rval = 2.0*anEngine->flat()-1.0;
   displ = 0.5*gamma*tan(rval*M_PI_2);

   return mean + displ;
}

HepDouble RandBreitWigner::shoot(HepRandomEngine* anEngine,
                                 HepDouble mean, HepDouble gamma, HepDouble cut )
{
   HepDouble val, rval, displ;

   if ( gamma == 0.0 ) return mean;
   val = atan(2.0*cut/gamma);
   rval = 2.0*anEngine->flat()-1.0;
   displ = 0.5*gamma*tan(rval*val);

   return mean + displ;
}

HepDouble RandBreitWigner::shootM2(HepRandomEngine* anEngine,
                                   HepDouble mean, HepDouble gamma )
{
   HepDouble val, rval, displ;

   if ( gamma == 0.0 ) return mean;
   val = atan(-mean/gamma);
   rval = RandFlat::shoot(anEngine,val, M_PI_2);
   displ = gamma*tan(rval);

   return ::sqrt(mean*mean + mean*displ);
}

HepDouble RandBreitWigner::shootM2(HepRandomEngine* anEngine,
                                   HepDouble mean, HepDouble gamma, HepDouble cut )
{
   HepDouble rval, displ;
   HepDouble lower, upper, tmp;

   if ( gamma == 0.0 ) return mean;
   tmp = max(0.0,(mean-cut));
   lower = atan( (tmp*tmp-mean*mean)/(mean*gamma) );
   upper = atan( ((mean+cut)*(mean+cut)-mean*mean)/(mean*gamma) );
   rval = RandFlat::shoot(anEngine, lower, upper);
   displ = gamma*tan(rval);

   return ::sqrt( max(0.0, mean*mean+mean*displ) );
}

void RandBreitWigner::shootArray ( HepRandomEngine* anEngine,
                                   const HepInt size, HepDouble* vect,
                                   HepDouble a, HepDouble b,
                                   HepDouble c )
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = shoot( anEngine, a, b, c );
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandBreitWigner::shootArray ( HepRandomEngine* anEngine,
                                   vector<HepDouble>& vec,
                                   HepDouble a, HepDouble b,
                                   HepDouble c )
#else
RandBreitWigner::shootArray ( HepRandomEngine* anEngine,
                                   vector<HepDouble,allocator<HepDouble> >& vec,
                                   HepDouble a, HepDouble b,
                                   HepDouble c )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = shoot( anEngine, a, b, c );
}
//----------------

HepDouble RandBreitWigner::fire(HepDouble mean, HepDouble gamma)
{
   HepDouble rval, displ;

   rval = 2.0*localEngine->flat()-1.0;
   displ = 0.5*gamma*tan(rval*M_PI_2);

   return mean + displ;
}

HepDouble RandBreitWigner::fire(HepDouble mean, HepDouble gamma, HepDouble cut)
{
   HepDouble val, rval, displ;

   if ( gamma == 0.0 ) return mean;
   val = atan(2.0*cut/gamma);
   rval = 2.0*localEngine->flat()-1.0;
   displ = 0.5*gamma*tan(rval*val);

   return mean + displ;
}

HepDouble RandBreitWigner::fireM2(HepDouble mean, HepDouble gamma )
{
   HepDouble val, rval, displ;

   if ( gamma == 0.0 ) return mean;
   val = atan(-mean/gamma);
   rval = RandFlat::shoot(localEngine,val, M_PI_2);
   displ = gamma*tan(rval);

   return ::sqrt(mean*mean + mean*displ);
}

HepDouble RandBreitWigner::fireM2(HepDouble mean, HepDouble gamma, HepDouble cut )
{
   HepDouble rval, displ;
   HepDouble lower, upper, tmp;

   if ( gamma == 0.0 ) return mean;
   tmp = max(0.0,(mean-cut));
   lower = atan( (tmp*tmp-mean*mean)/(mean*gamma) );
   upper = atan( ((mean+cut)*(mean+cut)-mean*mean)/(mean*gamma) );
   rval = RandFlat::shoot(localEngine,lower, upper);
   displ = gamma*tan(rval);

   return ::sqrt(max(0.0, mean*mean + mean*displ));
}

void RandBreitWigner::fireArray ( const HepInt size, HepDouble* vect,
                                  HepDouble a, HepDouble b,
                                  HepDouble c )
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i] = fire( a, b, c );
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RandBreitWigner::fireArray ( vector<HepDouble>& vec,
                                  HepDouble a, HepDouble b,
                                  HepDouble c )
#else
RandBreitWigner::fireArray ( vector<HepDouble, allocator<HepDouble> >& vec,
			     HepDouble a, HepDouble b,
			     HepDouble c )
#endif
{
   for (unsigned int i=0; i<vec.size(); ++i)
     vec[i] = fire( a, b, c );
}

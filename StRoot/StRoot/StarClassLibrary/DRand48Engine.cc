 /***************************************************************************
 *
 * $Id: DRand48Engine.cc,v 1.4 2016/01/22 17:10:49 smirnovd Exp $
 *
 * Author:  Gabriele Cosmo - Created: 5th September 1995
 *          modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *             DRand48Engine.cc,v 1.6 1998/01/23 07:39:56
 *
 *                             HEP Random
 *                        --- DRand48Engine ---
 *                      class implementation file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 ***************************************************************************
 *
 * $Log: DRand48Engine.cc,v $
 * Revision 1.4  2016/01/22 17:10:49  smirnovd
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
 * Revision 1.3  2012/06/11 15:29:26  fisyak
 * std namespace
 *
 * Revision 1.2  1999/12/07 23:43:03  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.1  1999/01/30 03:58:59  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:01  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "DRand48Engine.h"

#ifdef WIN32
   // ********************************************************************
   // Code extracted from GNU C Library 2.0.1

#  include <limits.h>
#  include <sys/types.h>
#  include <wtypes.h>
#  include <string.h>

   /* Global state for non-reentrant functions.  */
   struct drand48_data libc_drand48_data;

   int drand48_iterate (unsigned short int xsubi[3], struct drand48_data *buffer)
   {
     ULONGLONG X, a, result;

     /* Initialize buffer, if not yet done.  */
     if (!buffer->init)
       {
   #if (USHRT_MAX == 0xffffU)
         buffer->a[2] = 0x5;
         buffer->a[1] = 0xdeec;
         buffer->a[0] = 0xe66d;
   #else
         buffer->a[2] = 0x5deecUL;
         buffer->a[1] = 0xe66d0000UL;
         buffer->a[0] = 0;
   #endif
         buffer->c = 0xb;
         buffer->init = 1;
       }

     /* Do the real work.  We choose a data type which contains at least
        48 bits.  Because we compute the modulus it does not care how
        many bits really are computed.  */

     if (sizeof (unsigned short int) == 2)
       {
                 X = (ULONGLONG)xsubi[2] << 32 | (ULONGLONG)xsubi[1] << 16 | xsubi[0];
         a = ((ULONGLONG)buffer->a[2] << 32 | (ULONGLONG)buffer->a[1] << 16
              | buffer->a[0]);

                 result = X * a + buffer->c;

         xsubi[0] = (unsigned short int) (result & 0xffff);
         xsubi[1] = (unsigned short int) ((result >> 16) & 0xffff);
         xsubi[2] = (unsigned short int) ((result >> 32) & 0xffff);
       }
     else
       {
         X = (ULONGLONG)xsubi[2] << 16 | xsubi[1] >> 16;
         a = (ULONGLONG)buffer->a[2] << 16 | buffer->a[1] >> 16;

         result = X * a + buffer->c;

         xsubi[0] = (unsigned short int) (result >> 16 & 0xffffffffl);
         xsubi[1] = (unsigned short int) (result << 16 & 0xffff0000l);
       }

     return 0;
   }

   int seed48_r (unsigned short int seed16v[3], struct drand48_data *buffer)
   {
     /* Save old value at a private place to be used as return value.  */
     memcpy (buffer->old_X, buffer->X, sizeof (buffer->X));

     /* Install new state.  */
     memcpy (buffer->X, seed16v, sizeof (buffer->X));

     return 0;
   }

   unsigned short int * seed48 (unsigned short int seed16v[3])
   {
     (void) seed48_r (seed16v, &libc_drand48_data);

     return libc_drand48_data.old_X;
   }

   int srand48_r (long seedval, struct drand48_data *buffer)
   {
     /* The standards say we only have 32 bits.  */
     if (sizeof (long) > 4)
       seedval &= 0xffffffffl;

   #if (USHRT_MAX == 0xffffU)
     buffer->X[2] = (unsigned short int) (seedval >> 16);
     buffer->X[1] = (unsigned short int) (seedval & 0xffffl);
     buffer->X[0] = 0x330e;
   #else
     buffer->X[2] = seedval;
     buffer->X[1] = 0x330e0000UL;
     buffer->X[0] = 0;
   #endif

     return 0;
   }
   
   void srand48 (long seedval)
   {
     (void) srand48_r (seedval, &libc_drand48_data);
   }

   int erand48_r (unsigned short int xsubi[3], struct drand48_data *buffer, double *result)
   {
     /* Compute next state.  */
     if (drand48_iterate (xsubi, buffer) < 0)
       return -1;

     /* Construct a positive double with the 48 random bits distributed over
        its fractional part so the resulting FP number is [0.0,1.0).  */

   #if USHRT_MAX == 65535
     *result = ((double) xsubi[2] / ((ULONGLONG)1 << 48) +
                    (double) xsubi[1] / ((ULONGLONG)1 << 32) +
                    (double) xsubi[0] / ((ULONGLONG)1 << 16));
   #else
   # error Unsupported size of short int
   #endif

     return 0;
   }

   double drand48 ()
   {
     double result;

     (void) erand48_r (libc_drand48_data.X, &libc_drand48_data, &result);

     return result;
   }

   // End Code extracted from GNU C Library 2.0.1
   // ********************************************************************
#endif  /* WIN32 */

DRand48Engine::DRand48Engine(long seed)
{
   setSeed(seed,0);
   setSeeds(&theSeed,0);
}

DRand48Engine::~DRand48Engine() {}

DRand48Engine::DRand48Engine(const DRand48Engine &p)
{
  // This copy constructor uses saveStatus() and restoreStatus()
  // to make the physical copy of the object preserving its
  // original status.

  if ((this != &p) && (&p)) {
    p.saveStatus();
    restoreStatus();
    setSeeds(&theSeed,0);
  }
}

DRand48Engine & DRand48Engine::operator = (const DRand48Engine &p)
{
  // This operator uses saveStatus() and restoreStatus()
  // to make the physical copy of the object preserving its
  // original status.

  if ((this != &p) && (&p)) {
    p.saveStatus();
    restoreStatus();
    setSeeds(&theSeed,0);
  }
  return *this;
}

void DRand48Engine::setSeed(long seed, HepInt)
{
   srand48( seed );
   theSeed = seed;
}

void DRand48Engine::setSeeds(const long* seeds, HepInt)
{
  setSeed(seeds ? *seeds : 19780503, 0);
  theSeeds = seeds;
}

void DRand48Engine::saveStatus() const
{
   ofstream outFile("DRand48.conf", std::ios::out ) ;

   unsigned short dummy[] = { 0, 0, 0 };
   unsigned short* cseed = seed48(dummy);

   if (!outFile.bad()) {
     outFile << theSeed << endl;
     for (HepInt i=0; i<3; ++i) {
       outFile << cseed[i] << endl;
       dummy[i] = cseed[i];
     }
     seed48(dummy);
   }
}

void DRand48Engine::restoreStatus()
{
   ifstream inFile("DRand48.conf", std::ios::in);
   unsigned short cseed[3];

   if (!inFile.bad() && !inFile.eof()) {
     inFile >> theSeed;
     for (HepInt i=0; i<3; ++i)
       inFile >> cseed[i];
     seed48(cseed);
   }
}

void DRand48Engine::showStatus() const
{
   unsigned short dummy[] = { 0, 0, 0 };
   unsigned short* cseed = seed48(dummy);
   cout << endl;
   cout << "-------- DRand48 engine status ---------" << endl;
   cout << " Initial seed  = " << theSeed << endl;
   cout << " Current seeds = " << cseed[0] << ", ";
   cout                        << cseed[1] << ", ";
   cout                        << cseed[2] << endl;
   cout << "----------------------------------------" << endl;
   for (HepInt i=0; i<3; ++i)
     dummy[i] = cseed[i];
   seed48(dummy);
}

HepDouble DRand48Engine::flat()
{
   HepDouble num = 0.;

   while (num == 0.)
     num = drand48();
   return num;
}

void DRand48Engine::flatArray(const HepInt size, HepDouble* vect)
{
   HepInt i;

   for (i=0; i<size; ++i)
     vect[i]=flat();
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
DRand48Engine::flatArray(vector<HepDouble>& vec)
#else
DRand48Engine::flatArray(vector<HepDouble,allocator<HepDouble> >& vec)
#endif
{
  for (unsigned int i=0; i<vec.size(); ++i)
     vec[i]=flat();
}

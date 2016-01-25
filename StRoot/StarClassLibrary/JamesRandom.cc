/***************************************************************************
 *
 * $Id: JamesRandom.cc,v 1.4 2016/01/22 17:10:49 smirnovd Exp $
 *
 * Author: Gabriele Cosmo - Created: 5th September 1995
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                       --- HepJamesRandom ---
 *                      class implementation file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * This algorithm implements the original universal random number generator
 * as proposed by Marsaglia & Zaman in report FSU-SCRI-87-50 and coded
 * in FORTRAN77 by Fred James as the RANMAR generator, part of the MATHLIB
 * HEP library.
 *
 ***************************************************************************
 *
 * $Log: JamesRandom.cc,v $
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
 * Revision 1.1  1999/01/23 00:29:02  ullrich
 * Initial Revision
 *
 **************************************************************************/

#include "JamesRandom.h"

HepJamesRandom::HepJamesRandom(long seed)
{
  setSeed(seed,0);
  setSeeds(&theSeed,0);
}

HepJamesRandom::~HepJamesRandom() {}

HepJamesRandom::HepJamesRandom(const HepJamesRandom &p)
{
  HepInt ipos, jpos;
  if ((this != &p) && (&p)) {
    theSeed = p.getSeed();
    setSeeds(&theSeed,0);
    for (HepInt i=0; i<97; ++i)
      u[i] = p.u[i];
    c = p.c; cd = p.cd; cm = p.cm;
    jpos = HepInt(p.pj97-p.u);
    ipos = (64+jpos)%97;
    pi97 = &u[ipos]; pj97 = &u[jpos];
  }
}

HepJamesRandom & HepJamesRandom::operator = (const HepJamesRandom &p)
{
  HepInt ipos, jpos;
  if ((this != &p) && (&p)) {
    theSeed = p.getSeed();
    setSeeds(&theSeed,0);
    for (HepInt i=0; i<97; ++i)
      u[i] = p.u[i];
    c = p.c; cd = p.cd; cm = p.cm;
    jpos = HepInt(p.pj97-p.u);
    ipos = (64+jpos)%97;
    pi97 = &u[ipos]; pj97 = &u[jpos];
  }
  return *this;
}

void HepJamesRandom::saveStatus() const
{
   ofstream outFile("JamesRand.conf", std::ios::out ) ;

   if (!outFile.bad()) {
     HepInt pos = HepInt(pj97-u);
     outFile << theSeed << endl;
     for (HepInt i=0; i<97; ++i)
       outFile << u[i] << " ";
     outFile << endl;
     outFile << c << " " << cd << " " << cm << endl;
     outFile << pos << endl;
   }
}

void HepJamesRandom::restoreStatus()
{
   HepInt ipos, jpos;
   ifstream inFile("JamesRand.conf", std::ios::in);

   if (!inFile.bad() && !inFile.eof()) {
     inFile >> theSeed;
     for (HepInt i=0; i<97; ++i)
       inFile >> u[i];
     inFile >> c; inFile >> cd; inFile >> cm;
     inFile >> jpos;
     ipos = (64+jpos)%97;
     pi97 = &u[ipos];
     pj97 = &u[jpos];
   }
}

void HepJamesRandom::showStatus() const
{
   cout << endl;
   cout << "----- HepJamesRandom engine status -----" << endl;
   cout << " Initial seed = " << theSeed << endl;
   cout << " u[] = ";
   for (HepInt i=0; i<97; ++i)
     cout << u[i] << " ";
   cout << endl;
   cout << " c = " << c << ", cd = " << cd << ", cm = " << cm << endl;
   cout << " pi97 = " << pi97 << ", *pi97 = " << *pi97 << endl;
   cout << " pj97 = " << pj97 << ", *pj97 = " << *pj97 << endl;
   cout << "----------------------------------------" << endl;
}

void HepJamesRandom::setSeed(long seed, HepInt)
{
  // The input value for "seed" should be within the range [0,900000000]

  HepInt m, n;
  HepFloat s, t;
  long mm;

  long ij = seed/30082;
  long kl = seed - 30082*ij;
  long i = (ij/177) % 177 + 2;
  long j = ij % 177 + 2;
  long k = (kl/169) % 178 + 1;
  long l = kl % 169;

  theSeed = seed;

  for ( n = 1 ; n < 98 ; n++ ) {
    s = 0.0;
    t = 0.5;
    for ( m = 1 ; m < 25 ; m++) {
      mm = ( ( (i*j) % 179 ) * k ) % 179;
      i = j;
      j = k;
      k = mm;
      l = ( 53 * l + 1 ) % 169;
      if ( (l*mm % 64 ) >= 32 )
        s += t;
      t *= 0.5;
    }
    u[n-1] = s;
  }
  c = 362436.0 / 16777216.0;
  cd = 7654321.0 / 16777216.0;
  cm = 16777213.0 / 16777216.0;
  pi97 = &u[96];
  pj97 = &u[32];
}

void HepJamesRandom::setSeeds(const long* seeds, HepInt)
{
  setSeed(seeds ? *seeds : 19780503, 0);
  theSeeds = seeds;
}

HepDouble HepJamesRandom::flat()
{
   HepDouble uni;

   do {
      uni = *pi97 - *pj97;
      if ( uni < 0.0 ) uni++;
      *pi97 = uni;
      
      if (pi97 == u) pi97 += 96;
      else pi97--;
      
      if (pj97 == u) pj97 += 96;
      else pj97--;
      
      c -= cd;
      if (c < 0.0) c += cm;
      
      uni -= c;
      if (uni < 0.0) uni += 1.0;
   } while ( uni <= 0.0 || uni >= 1.0 );
   
   return uni;
}

void HepJamesRandom::flatArray(const HepInt size, HepDouble* vect)
{
   HepDouble uni;
   HepInt i;

   for (i=0; i<size; ++i) {
      do {
         uni = *pi97 - *pj97;
         if ( uni < 0.0 ) uni++;
         *pi97 = uni;

         if (pi97 == u) pi97 += 96;
         else pi97--;

         if (pj97 == u) pj97 += 96;
         else pj97--;

         c -= cd;
         if (c < 0.0) c += cm;

         uni -= c;
         if (uni < 0.0) uni += 1.0;
      } while ( uni <= 0.0 || uni >= 1.0 );
      vect[i] = uni;
   }   
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
HepJamesRandom::flatArray(vector<HepDouble>& vec)
#else
HepJamesRandom::flatArray(vector<HepDouble,allocator<HepDouble> >& vec)
#endif
{
   HepDouble     uni;

   for (unsigned int i=0; i<vec.size(); ++i) {
      do {
         uni = *pi97 - *pj97;
         if ( uni < 0.0 ) uni++;
         *pi97 = uni;

         if (pi97 == u) pi97 += 96;
         else pi97--;

         if (pj97 == u) pj97 += 96;
         else pj97--;

         c -= cd;
         if (c < 0.0) c += cm;

         uni -= c;
         if (uni < 0.0) uni += 1.0;
      } while ( uni <= 0.0 || uni >= 1.0 );
      vec[i] = uni;
   }   
}

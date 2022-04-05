/***************************************************************************
 *
 * $Id: RanecuEngine.cc,v 1.5 2016/01/22 17:10:50 smirnovd Exp $
 *
 * Author:  Gabriele Cosmo - Created - 2nd February 1996
 *          modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *            RanecuEngine.cc,v 1.10 1998/01/23 07:39:58
 *                             HEP Random
 *                        --- RanecuEngine ---
 *                      class implementation file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * RANECU Random Engine - algorithm originally written in FORTRAN77
 *                        as part of the MATHLIB HEP library.
 *
 ***************************************************************************
 *
 * $Log: RanecuEngine.cc,v $
 * Revision 1.5  2016/01/22 17:10:50  smirnovd
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
 * Revision 1.4  2012/06/11 15:29:26  fisyak
 * std namespace
 *
 * Revision 1.3  1999/12/21 15:13:58  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.2  1999/12/07 23:43:04  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.1  1999/01/30 03:59:01  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:11  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "RanecuEngine.h"
#if __SUNPRO_CC < 0x500
#include <stdlib.h>
#else
#include <cstdlib>          // for abs(), tu
#endif

RanecuEngine::RanecuEngine(HepInt index)
: ecuyer_a(40014),ecuyer_b(53668),ecuyer_c(12211),
  ecuyer_d(40692),ecuyer_e(52774),ecuyer_f(3791),
  shift1(2147483563),shift2(2147483399),maxSeq(215),
  prec(4.6566128E-10)
{
  seq = abs(HepInt(index%maxSeq));
  theSeed = seq;
  for (HepInt i=0; i<2; ++i)
    for (HepInt j=0; j<maxSeq; ++j)
      table[j][i] = seedTable[j][i];
  theSeeds = &table[seq][0];
}

RanecuEngine::~RanecuEngine() {}

RanecuEngine::RanecuEngine(const RanecuEngine &p)
: ecuyer_a(40014),ecuyer_b(53668),ecuyer_c(12211),
  ecuyer_d(40692),ecuyer_e(52774),ecuyer_f(3791),
  shift1(2147483563),shift2(2147483399),maxSeq(215),
  prec(4.6566128E-10)
{
  if ((this != &p) && (&p)) {
    theSeed = p.getSeed();
    seq = p.seq;
    for (HepInt i=0; i<2; ++i)
      for (HepInt j=0; j<maxSeq; ++j)
        table[j][i] = p.table[j][i];
    seq = p.seq;
    theSeeds = &table[seq][0];
  }
}

RanecuEngine & RanecuEngine::operator = (const RanecuEngine &p)
{
  if ((this != &p) && (&p)) {
    theSeed = p.getSeed();
    seq = p.seq;
    for (HepInt i=0; i<2; ++i)
      for (HepInt j=0; j<maxSeq; ++j)
        table[j][i] = p.table[j][i];
    seq = p.seq;
    theSeeds = &table[seq][0];
  }
  return *this;
}

void RanecuEngine::setSeed(long index, HepInt)
{
  seq = abs(HepInt(index%maxSeq));
  theSeed = seq;
  theSeeds = &table[seq][0];
}

void RanecuEngine::setSeeds(const long* seeds, HepInt pos)
{
  if (pos != -1) {
    seq = abs(HepInt(pos%maxSeq));
    theSeed = seq;
  }
  if ((seeds[0] > 0) && (seeds[1] > 0)) {
    table[seq][0] = seeds[0];
    table[seq][1] = seeds[1];
  }
  theSeeds = &table[seq][0];
}

void RanecuEngine::saveStatus() const
{
   ofstream outFile("Ranecu.conf", std::ios::out ) ;

   if (!outFile.bad()) {
     outFile << theSeed << endl;
     for (HepInt i=0; i<2; ++i)
       outFile << table[theSeed][i] << " ";
   }
}

void RanecuEngine::restoreStatus()
{
   ifstream inFile("Ranecu.conf", std::ios::in);

   if (!inFile.bad() && !inFile.eof()) {
     inFile >> theSeed;
     for (HepInt i=0; i<2; ++i)
       inFile >> table[theSeed][i];
     seq = HepInt(theSeed);
  }
}

void RanecuEngine::showStatus() const
{
   cout << endl;
   cout << "--------- Ranecu engine status ---------" << endl;
   cout << " Initial seed (index) = " << theSeed << endl;
   cout << " Current couple of seeds = " << table[theSeed][0]
                                 << ", " << table[theSeed][1] << endl;
   cout << "----------------------------------------" << endl;
}

HepDouble RanecuEngine::flat()
{
   const HepInt index = seq;
   long seed1 = table[index][0];
   long seed2 = table[index][1];

   HepInt k1 = (HepInt)(seed1/ecuyer_b);
   HepInt k2 = (HepInt)(seed2/ecuyer_e);

   seed1 = ecuyer_a*(seed1-k1*ecuyer_b)-k1*ecuyer_c;
   if (seed1 < 0) seed1 += shift1;
   seed2 = ecuyer_d*(seed2-k2*ecuyer_e)-k2*ecuyer_f;
   if (seed2 < 0) seed2 += shift2;

   table[index][0] = seed1;
   table[index][1] = seed2;

   long diff = seed1-seed2;

   if (diff <= 0) diff += (shift1-1);
   return (HepDouble)(diff*prec);
}

void RanecuEngine::flatArray(const HepInt size, HepDouble* vect)
{
   const HepInt index = seq;
   long seed1 = table[index][0];
   long seed2 = table[index][1];
   HepInt k1, k2;
   HepInt i;

   for (i=0; i<size; ++i)
   {
     k1 = (HepInt)(seed1/ecuyer_b);
     k2 = (HepInt)(seed2/ecuyer_e);

     seed1 = ecuyer_a*(seed1-k1*ecuyer_b)-k1*ecuyer_c;
     if (seed1 < 0) seed1 += shift1;
     seed2 = ecuyer_d*(seed2-k2*ecuyer_e)-k2*ecuyer_f;
     if (seed2 < 0) seed2 += shift2;

     long diff = seed1-seed2;
     if (diff <= 0) diff += (shift1-1);

     vect[i] = (HepDouble)(diff*prec);
   }
   table[index][0] = seed1;
   table[index][1] = seed2;
}

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RanecuEngine::flatArray(vector<HepDouble>& vec)
#else
RanecuEngine::flatArray(vector<HepDouble,allocator<HepDouble> >& vec)
#endif
{
   const HepInt index = seq;
   long seed1 = table[index][0];
   long seed2 = table[index][1];
   HepInt k1, k2;

   for (unsigned int i=0; i<vec.size(); ++i)
   {
     k1 = (HepInt)(seed1/ecuyer_b);
     k2 = (HepInt)(seed2/ecuyer_e);

     seed1 = ecuyer_a*(seed1-k1*ecuyer_b)-k1*ecuyer_c;
     if (seed1 < 0) seed1 += shift1;
     seed2 = ecuyer_d*(seed2-k2*ecuyer_e)-k2*ecuyer_f;
     if (seed2 < 0) seed2 += shift2;

     long diff = seed1-seed2;
     if (diff <= 0) diff += (shift1-1);

     vec[i] = (HepDouble)(diff*prec);
   }
   table[index][0] = seed1;
   table[index][1] = seed2;
}

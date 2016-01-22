/***************************************************************************
 *
 * $Id: RanluxEngine.cc,v 1.5 2016/01/22 17:10:50 smirnovd Exp $
 *
 * Author: Original code from CLHEP by G. Cosmo
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                        --- RanluxEngine ---
 *                      class implementation file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * Ranlux random number generator originally implemented in FORTRAN77
 * by Fred James as part of the MATHLIB HEP library.
 * 'RanluxEngine' is designed to fit into the CLHEP random number
 * class structure.
 *
 ***************************************************************************
 *
 * $Log: RanluxEngine.cc,v $
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
 * Revision 1.3  2003/09/02 17:59:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  1999/12/07 23:43:04  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.1  1999/01/30 03:59:01  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:12  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "RanluxEngine.h"

RanluxEngine::RanluxEngine(long seed, HepInt lux)
: int_modulus(0x1000000),
  mantissa_bit_24((HepFloat) ::pow(0.5,24.)),
  mantissa_bit_12((HepFloat) ::pow(0.5,12.))
{
   luxury = lux;
   setSeed(seed, luxury);
   setSeeds(&theSeed, luxury);
}

RanluxEngine::~RanluxEngine() {}

RanluxEngine::RanluxEngine(const RanluxEngine &p)
: int_modulus(0x1000000),
  mantissa_bit_24((HepFloat) ::pow(0.5,24.)),
  mantissa_bit_12((HepFloat) ::pow(0.5,12.))
{
  if ((this != &p) && (&p)) {
    theSeed = p.getSeed();
    setSeeds(&theSeed, p.luxury);
    for (HepInt i=0; i<24; ++i)
      float_seed_table[i] = p.float_seed_table[i];
    nskip = p.nskip;
    luxury = p.luxury;
    i_lag = p.i_lag;  j_lag = p.j_lag;
    carry = p.carry;
    count24 = p.count24;
  }
}

RanluxEngine & RanluxEngine::operator = (const RanluxEngine &p)
{
  if ((this != &p) && (&p)) {
    theSeed = p.getSeed();
    setSeeds(&theSeed, p.luxury);
    for (HepInt i=0; i<24; ++i)
      float_seed_table[i] = p.float_seed_table[i];
    nskip = p.nskip;
    luxury = p.luxury;
    i_lag = p.i_lag;  j_lag = p.j_lag;
    carry = p.carry;
    count24 = p.count24;
  }
  return *this;
}

void RanluxEngine::setSeed(long seed, HepInt lux) {

// The initialisation is carried out using a Multiplicative
// Congruential generator using formula constants of L'Ecuyer 
// as described in "A review of pseudorandom number generators"
// (Fred James) published in Computer Physics Communications 60 (1990)
// pages 329-344

  const HepInt ecuyer_a = 53668;
  const HepInt ecuyer_b = 40014;
  const HepInt ecuyer_c = 12211;
  const HepInt ecuyer_d = 2147483563;

  const HepInt lux_levels[5] = {0,24,73,199,365};  

  long int_seed_table[24];
  long next_seed = seed;
  long k_multiple;
  HepInt i;
  
// number of additional random numbers that need to be 'thrown away'
// every 24 numbers is set using luxury level variable.

  theSeed = seed;
  if( (lux > 4)||(lux < 0) ){
     if(lux >= 24){
        nskip = lux - 24;
     }else{
        nskip = lux_levels[3]; // corresponds to default luxury level
     }
  }else{
     luxury = lux;
     nskip = lux_levels[luxury];
  }

   
  for(i = 0;i != 24;i++){
     k_multiple = next_seed / ecuyer_a;
     next_seed = ecuyer_b * (next_seed - k_multiple * ecuyer_a) 
     - k_multiple * ecuyer_c ;
     if(next_seed < 0)next_seed += ecuyer_d;
     int_seed_table[i] = next_seed % int_modulus;
  }     

  for(i = 0;i != 24;i++)
    float_seed_table[i] = int_seed_table[i] * mantissa_bit_24;

  i_lag = 23;
  j_lag = 9;
  carry = 0. ;

  if( float_seed_table[23] == 0. ) carry = mantissa_bit_24;
  
  count24 = 0;
}

void RanluxEngine::setSeeds(const long *seeds, HepInt lux) {

   const HepInt ecuyer_a = 53668;
   const HepInt ecuyer_b = 40014;
   const HepInt ecuyer_c = 12211;
   const HepInt ecuyer_d = 2147483563;

   const HepInt lux_levels[5] = {0,24,73,199,365};
   HepInt i;
   long int_seed_table[24];
   long k_multiple,next_seed;
   const long *seedptr; 

   theSeeds = seeds;
   seedptr  = seeds;
 
   if(seeds == 0){
      setSeed(theSeed,lux);
      theSeeds = &theSeed;
      return;
   }

   theSeed = *seeds;

// number of additional random numbers that need to be 'thrown away'
// every 24 numbers is set using luxury level variable.

  if( (lux > 4)||(lux < 0) ){
     if(lux >= 24){
        nskip = lux - 24;
     }else{
        nskip = lux_levels[3]; // corresponds to default luxury level
     }
  }else{
     luxury = lux;
     nskip = lux_levels[luxury];
  }
      
  for( i = 0;(i != 24)&&(*seedptr != 0);i++){
      int_seed_table[i] = *seedptr % int_modulus;
      seedptr++;
  }		       

  if(i != 24){
     next_seed = int_seed_table[i-1];
     for(;i != 24;i++){
        k_multiple = next_seed / ecuyer_a;
        next_seed = ecuyer_b * (next_seed - k_multiple * ecuyer_a) 
        - k_multiple * ecuyer_c ;
        if(next_seed < 0)next_seed += ecuyer_d;
        int_seed_table[i] = next_seed % int_modulus;
     }          
  }

  for(i = 0;i != 24;i++)
    float_seed_table[i] = int_seed_table[i] * mantissa_bit_24;

  i_lag = 23;
  j_lag = 9;
  carry = 0. ;

  if( float_seed_table[23] == 0. ) carry = mantissa_bit_24;
  
  count24 = 0;
}

void RanluxEngine::saveStatus() const
{
   ofstream outFile("Ranlux.conf", std::ios::out ) ;

   if (!outFile.bad()) {
     outFile << theSeed << endl;
     for (HepInt i=0; i<24; ++i)
       outFile << float_seed_table[i] << " ";
     outFile << endl;
     outFile << i_lag << " " << j_lag << endl;
     outFile << carry << " " << count24 << endl;
   }
}

void RanluxEngine::restoreStatus()
{
   ifstream inFile("Ranlux.conf", std::ios::in);

   if (!inFile.bad() && !inFile.eof()) {
     inFile >> theSeed;
     for (HepInt i=0; i<24; ++i)
       inFile >> float_seed_table[i];
     inFile >> i_lag; inFile >> j_lag;
     inFile >> carry; inFile >> count24;
   }
}

void RanluxEngine::showStatus() const
{
   cout << endl;
   cout << "--------- Ranlux engine status ---------" << endl;
   cout << " Initial seed = " << theSeed << endl;
   cout << " float_seed_table[] = ";
   for (HepInt i=0; i<24; ++i)
     cout << float_seed_table[i] << " ";
   cout << endl;
   cout << " i_lag = " << i_lag << ", j_lag = " << j_lag << endl;
   cout << " carry = " << carry << ", count24 = " << count24 << endl;
   cout << "----------------------------------------" << endl;
}

HepDouble RanluxEngine::flat() {

  HepFloat next_random;
  HepFloat uni;
  HepInt i;

  uni = float_seed_table[j_lag] - float_seed_table[i_lag] - carry;
  if(uni < 0. ){
     uni += 1.0;
     carry = mantissa_bit_24;
  }else{
     carry = 0.;
  }

  float_seed_table[i_lag] = uni;
  i_lag --;
  j_lag --;
  if(i_lag < 0) i_lag = 23;
  if(j_lag < 0) j_lag = 23;

  if( uni < mantissa_bit_12 ){
     uni += mantissa_bit_24 * float_seed_table[j_lag];
     if( uni == 0) uni = mantissa_bit_24 * mantissa_bit_24;
  }
  next_random = uni;
  count24 ++;

// every 24th number generation, several random numbers are generated
// and wasted depending upon the luxury level.

  if(count24 == 24 ){
     count24 = 0;
     for( i = 0; i != nskip ; i++){
         uni = float_seed_table[j_lag] - float_seed_table[i_lag] - carry;
         if(uni < 0. ){
            uni += 1.0;
            carry = mantissa_bit_24;
         }else{
            carry = 0.;
         }
         float_seed_table[i_lag] = uni;
         i_lag --;
         j_lag --;
         if(i_lag < 0)i_lag = 23;
         if(j_lag < 0) j_lag = 23;
      }
  } 
  return (HepDouble) next_random;
}

void RanluxEngine::flatArray(const HepInt size, HepDouble* vect)
{
  HepFloat next_random;
  HepFloat uni;
  HepInt i;
  HepInt index;

  for (index=0; index<size; ++index) {
    uni = float_seed_table[j_lag] - float_seed_table[i_lag] - carry;
    if(uni < 0. ){
       uni += 1.0;
       carry = mantissa_bit_24;
    }else{
       carry = 0.;
    }

    float_seed_table[i_lag] = uni;
    i_lag --;
    j_lag --;
    if(i_lag < 0) i_lag = 23;
    if(j_lag < 0) j_lag = 23;

    if( uni < mantissa_bit_12 ){
       uni += mantissa_bit_24 * float_seed_table[j_lag];
       if( uni == 0) uni = mantissa_bit_24 * mantissa_bit_24;
    }
    next_random = uni;
    vect[index] = (HepDouble)next_random;
    count24 ++;

// every 24th number generation, several random numbers are generated
// and wasted depending upon the luxury level.

    if(count24 == 24 ){
       count24 = 0;
       for( i = 0; i != nskip ; i++){
           uni = float_seed_table[j_lag] - float_seed_table[i_lag] - carry;
           if(uni < 0. ){
              uni += 1.0;
              carry = mantissa_bit_24;
           }else{
              carry = 0.;
           }
           float_seed_table[i_lag] = uni;
           i_lag --;
           j_lag --;
           if(i_lag < 0)i_lag = 23;
           if(j_lag < 0) j_lag = 23;
        }
    }
  }
} 

void
#ifndef ST_NO_TEMPLATE_DEF_ARGS
RanluxEngine::flatArray(vector<HepDouble>& vec)
#else
RanluxEngine::flatArray(vector<HepDouble,allocator<HepDouble> >& vec)
#endif
{
  HepFloat next_random;
  HepFloat uni;
  HepInt i;

  for (unsigned int index=0; index<vec.size(); ++index) {
    uni = float_seed_table[j_lag] - float_seed_table[i_lag] - carry;
    if(uni < 0. ){
       uni += 1.0;
       carry = mantissa_bit_24;
    }else{
       carry = 0.;
    }

    float_seed_table[i_lag] = uni;
    i_lag --;
    j_lag --;
    if(i_lag < 0) i_lag = 23;
    if(j_lag < 0) j_lag = 23;

    if( uni < mantissa_bit_12 ){
       uni += mantissa_bit_24 * float_seed_table[j_lag];
       if( uni == 0) uni = mantissa_bit_24 * mantissa_bit_24;
    }
    next_random = uni;
    vec[index] = (HepDouble)next_random;
    count24 ++;

// every 24th number generation, several random numbers are generated
// and wasted depending upon the luxury level.

    if(count24 == 24 ){
       count24 = 0;
       for( i = 0; i != nskip ; i++){
           uni = float_seed_table[j_lag] - float_seed_table[i_lag] - carry;
           if(uni < 0. ){
              uni += 1.0;
              carry = mantissa_bit_24;
           }else{
              carry = 0.;
           }
           float_seed_table[i_lag] = uni;
           i_lag --;
           j_lag --;
           if(i_lag < 0)i_lag = 23;
           if(j_lag < 0) j_lag = 23;
        }
    }
  }
} 

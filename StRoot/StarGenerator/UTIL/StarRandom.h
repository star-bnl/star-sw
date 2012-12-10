#ifndef __StarRandom_h__
#define __StarRandom_h__

#include "TObject.h"
#include "Math/GSLRndmEngines.h"
#include "TVector2.h"
#include "TRandom.h"
#include "TArrayC.h"

/**
   \class StarRandom

   \brief A class for providing random number generation

   StarRandom is a singleton class which allows users to implement a common
   random number generator interface across multiple physics event generators.

   Example:  Remap pythia 6's random number generator

   extern "C" {
      Double_t pyr( Int_t *idummy ){
         return StarRandom::Instance().flat();
      };
   };

   \author Jason C. Webb
   
 */

class StarRandom : public TObject
{
  StarRandom();
 public:
  ~StarRandom();

  /// Obtain the single instance of the random number generator
  static StarRandom &Instance();

  /// Set the random number generator engine
  static void set( ROOT::Math::GSLRandomEngine *engine );

  /// Capture gRandom random number generator
  static void capture();

  /// Set the seed for this simulation.  Note:  Most RNG engines in GSL
  /// utilize a 32-bit seed.  Those which use a smaller seed will take
  /// the provided seed modulo 32 bits.  There may not be a warning 
  /// issued when this occurs.
  /// @param seed is an unsigned integer (32-bit word) providing a seed for tne RNG.
  static void seed( UInt_t s );

  /// Set a pair of seeds for this simulation.  The two seeds will be combined
  /// into a single 32-bit word, and passed to the RNG engine.
  /// @param seed1 is the first RNG seed.
  /// @param seed2 is the second RNG seed.
  static void seed( UShort_t seed1, UShort_t seed2 );

  /// Return a random number uniformly distributed between 0 and 1
  Double_t operator()() const;
  /// Return a random number uniformly distributed between mn and mx
  Double_t operator()( const Double_t mn, const Double_t mx ) const;

  /// 
  Double_t operator()( Int_t *idummy ){ return (*this)(); }


  /// Return a random number uniformly distributed between 0 and 1  
  Double_t flat() const;
  /// Return a random number uniformly distributed between mn and mx
  Double_t flat( const Double_t mn, const Double_t mx ) const;

  /// Return a random number distribted according to exp(-mu)
  Double_t expo( const Double_t mu ) const;
  /// Return a random number distributed according to a landau
  Double_t landau() const;
  /// Return a random number distributed according to a gaussian with specified sigma
  Double_t gauss( const Double_t sigma ) const ;
  /// Returns a pair of random numbers generated according to a 2D gaussian
  TVector2 gauss2d( const Double_t sx, const Double_t sy, const Double_t rho ) const;

  /// Returns an integer sampled from a possion distribution with
  /// expected value mu
  UInt_t poisson( const Double_t mu ) const;

 private:
 protected:

  static StarRandom *sInstance;                       //!
  static ROOT::Math::GSLRandomEngine *mEngine;        //!

  UInt_t   mSeed;                                     // Random number seed
  TArrayC  mState;                                    // Random number state
  
  ClassDef(StarRandom,1);

};

#endif

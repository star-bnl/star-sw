#ifndef __StarRandom_h__
#define __StarRandom_h__

#include "TObject.h"
#include "Math/GSLRndmEngines.h"
#include "TVector2.h"
#include "TRandom.h"
#include "TArrayC.h"

/**
   \class StarRandom
   \author Jason C. Webb
   \brief A class for providing random number generation

   StarRandom is a singleton class which allows users to implement a common
   random number generator interface across multiple physics event generators.

   Example:  Remap pythia 6's random number generator

   extern "C" {
      Double_t pyr( Int_t *idummy ){
         return StarRandom::Instance().flat();
      };
   };
   
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

  /// Set the seed for this simulation 
  static void seed( UInt_t s );

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

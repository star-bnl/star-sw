#include "StarRandom.h"
ClassImp(StarRandom);
#include "assert.h"

#include "Math/GSLRndmEngines.h"

#include <iostream>
#include <string>
#include "StMessMgr.h"
#include "TRandom.h"

#include "TSystem.h"

using namespace std;
StarRandom *StarRandom::sInstance = 0;
ROOT::Math::GSLRandomEngine *StarRandom::mEngine = 0;

//
// Lightweight class to remap ROOT's TRandom generators to StarRandom
//
class _Random : public TRandom {
public:
  Double_t Rndm( Int_t i=0 ){ return StarRandom::Instance().flat(); } 
};

_Random            *gStarRandom   = 0;


// Include the GSL RNG setup method and global default seed.
extern "C" 
{
  void gsl_rng_env_setup();
  unsigned long gsl_rng_default_seed;
};


/**
   \todo Allow multiple instances of generator per maker to facilitate debugging, reproducability
 */


// ----------------------------------------------------------------------------
StarRandom::StarRandom() : TObject(), mSeed(0), mState()
{

}
// ----------------------------------------------------------------------------
StarRandom::~StarRandom()
{
  if ( gStarRandom )
    {
      gRandom = 0;
      delete gStarRandom;      
  }
}
// ----------------------------------------------------------------------------
void StarRandom::capture()
{
  if ( !gStarRandom ) gStarRandom = new _Random();
  if ( gRandom )
    {
      Instance().Warning("StarRandom::capture()", "Capturing gRandom");
      delete gRandom;
    }
  gRandom = gStarRandom;
}
// ----------------------------------------------------------------------------
Double_t StarRandom::flat() const { return (*mEngine)();}
Double_t StarRandom::flat( const Double_t mn, const Double_t mx ) const { assert(mx>mn); return mn + (mx - mn) * flat(); }
Double_t StarRandom::expo( const Double_t mu ) const { return mEngine->Exponential( mu ); }
Double_t StarRandom::landau() const { return mEngine->Landau(); }
Double_t StarRandom::gauss( const Double_t sigma ) const { return mEngine->Gaussian( sigma ); }
TVector2 StarRandom::gauss2d( const Double_t sx, const Double_t sy, const Double_t rho )const
{
  Double_t x, y;
  mEngine->Gaussian2D( sx, sy, rho, x, y );
  return TVector2( x, y );
}
UInt_t StarRandom::poisson( const Double_t mu ) const { return mEngine->Poisson(mu); }
// ----------------------------------------------------------------------------
Double_t StarRandom::operator()() const { return flat(); }
Double_t StarRandom::operator()( const Double_t mn, const Double_t mx ) const
{
  return flat(mn,mx);
}
// ----------------------------------------------------------------------------
StarRandom &StarRandom::Instance()
{
  if ( !sInstance )
    {

      gsl_rng_env_setup();

      sInstance = new StarRandom();
      if ( !mEngine ) mEngine = new ROOT::Math::GSLRandomEngine();
      mEngine  -> Initialize();
      LOG_INFO << "Initialize random number generator " << mEngine->Name() << endm;

      if ( !gsl_rng_default_seed ) 
	{
	  // Setup default seed using current time (ms since epoch) randomized by process ID
	  long time = gSystem->Now();
	  long pid  = gSystem->GetPid();
	  seed( time|(pid<<16) );
	}
      else 
	{
	  seed( gsl_rng_default_seed );
	}

      LOG_INFO << "Initialize random number seed " << sInstance->mSeed << endm;
      
    }
  return (*sInstance);
}
// ----------------------------------------------------------------------------
void StarRandom::set( ROOT::Math::GSLRandomEngine *engine ){ mEngine = engine; }
// ----------------------------------------------------------------------------
void StarRandom::seed( UInt_t s ){ 
  if (!sInstance) Instance(); 
  mEngine->SetSeed(s); 
  //sInstance->mSeed = s;
  Instance().mSeed = s;
}
// ----------------------------------------------------------------------------
void StarRandom::seed( UShort_t seed1, UShort_t seed2 ) {
  Int_t myseed1 = seed1;
  Int_t myseed2 = seed2;
  seed( (myseed1<<16)|(myseed2) );
};

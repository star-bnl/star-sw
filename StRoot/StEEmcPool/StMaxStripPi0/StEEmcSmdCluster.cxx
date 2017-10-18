#include "StEEmcSmdCluster.h"
#include <TMath.h>

#include "StEvent/StEmcCluster.h"

ClassImp(StEEmcSmdCluster);

// ----------------------------------------------------------------------------
StEEmcSmdCluster::StEEmcSmdCluster()
{
  mEnergy=0.;
  mSumXW=0.;
  mSumX2W=0.;
  mEmcCluster = 0;
}

void StEEmcSmdCluster::print()
{
  //  std::cout << "smd cluster plane=" << plane() << std::endl;
  std::cout << "key=" << key() << std::endl;
  std::cout << "energy=" << energy() << std::endl;
  std::cout << "mean=" << mean() << std::endl;
}

StEEmcSmdCluster::StEEmcSmdCluster( const StEEmcSmdCluster &c )
{

  mStrips=c.mStrips;
  mWeights=c.mWeights;
  mEnergy=c.mEnergy;
  mSumXW=c.mSumXW;
  mSumX2W=c.mSumX2W;
  mMean=c.mMean;
  mSigma=c.mSigma;
  mSize=c.mSize;
  mKey=c.mKey;
  mMatchedTowers=c.mMatchedTowers;
  mEmcCluster=c.mEmcCluster;
}
// ----------------------------------------------------------------------------
void StEEmcSmdCluster::add( StEEmcStrip strip, Float_t weight )
{

  mWeights.push_back(weight);
  mStrips.push_back(strip);

  Float_t energy = strip.energy() * weight;

  mEnergy += energy;  
  mSumXW  += ( strip.index() + 0.5 ) * energy;
  mSumX2W += ( strip.index() + 0.5 ) * ( strip.index() + 0.5 ) * energy;

  mMean  = mSumXW / mEnergy;
  mSigma = mSumX2W / mEnergy - mMean*mMean;
  mSigma = TMath::Sqrt(mSigma);

  mSize=(Int_t)mStrips.size(); 

}

// ----------------------------------------------------------------------------
StEmcCluster *StEEmcSmdCluster::stemc()
{

  if ( mEmcCluster ) return mEmcCluster;

  mEmcCluster=new StEmcCluster();
  Int_t iuv=( mStrips[0].plane()==1 ); // 0=U, 1=V
  
  if ( iuv==0 ) {
    mEmcCluster->setEta( mean() );
    mEmcCluster->setSigmaEta( sigma() );
    mEmcCluster->setPhi( -1. );
    mEmcCluster->setSigmaPhi( -1. );
  }
  else {
    mEmcCluster->setEta( -1. );
    mEmcCluster->setSigmaEta( -1. );
    mEmcCluster->setPhi( mean() );
    mEmcCluster->setSigmaPhi( sigma() );
  }
  mEmcCluster->setEnergy( energy() );
  
  for ( Int_t i=0; i<numberOfStrips(); i++ ) {
    StEmcRawHit *hit=mStrips[i].stemc();
    assert(hit);
    mEmcCluster->addHit( hit );
  }

  return mEmcCluster;

}

// ----------------------------------------------------------------------------
Float_t StEEmcSmdCluster::energy(Int_t nmax, Option_t *opts)
{

  Float_t mymean;
  if ( TString(opts).Contains("seed") )
    {
      mymean=mStrips[0].index()+0.5;
    }
  else
    {
      mymean=mean();
    }
  Int_t min=(Int_t)(mymean-(Float_t)nmax);
  Int_t max=(Int_t)(mymean+(Float_t)nmax);

  Float_t esum=0.;

  for ( UInt_t i=0;i<mStrips.size();i++ )
    {
      StEEmcStrip &s=mStrips[i];
      if ( s.index() >= min && s.index() <= max ) esum+=s.energy();
    }

  return esum;

}

Float_t StEEmcSmdCluster::sigma(Int_t nmax, Option_t *opts)
{

  Float_t mymean;
  if ( TString(opts).Contains("seed") )
    {
      mymean=mStrips[0].index()+0.5;
    }
  else
    {
      mymean=mean();
    }
  Int_t min=(Int_t)(mymean-(Float_t)nmax);
  Int_t max=(Int_t)(mymean+(Float_t)nmax);

  std::cout << "mymean=" << mymean << std::endl;
  std::cout << "min   =" << min << std::endl;
  std::cout << "max   =" << max << std::endl;

  Float_t esum=0.;
  Float_t xsum=0.;
  Float_t x2sum=0.;

  for ( UInt_t i=0;i<mStrips.size();i++ )
    {
      StEEmcStrip &s=mStrips[i];
      std::cout << i << " " << s.index() << std::endl;
      if ( s.index() >= min && s.index() <= max ) {
	esum+=s.energy();
	xsum+=(s.index()+0.5)*s.energy();
	x2sum+=(s.index()+0.5)*(s.index()+0.5)*s.energy();
      }
    }
  Float_t sig=0.;
  if ( esum>0. ) 
    {
      sig=TMath::Sqrt( x2sum/esum - (xsum/esum)*(xsum/esum) );
    }


  return sig;

}

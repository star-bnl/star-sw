/**
 * \class StEEmcIUSmdCluster
 * \brief A base class for representing clusters of EEMC smd strips
 *
 * This class is designed to represent clusters of smd strips in the endcap.
 *
 * \author Jason C. Webb, Weihong He
 * $Date: 2008/10/30 15:52:14 $
 * $Revision: 1.1 $
 *
 * \section steemcsmdcluster_conventions
 *
 * By convention, the first strip added to the cluster is considered
 * to be the "seed" strip.
 *
 */
#include "StEEmcIUSmdCluster.h"
#include <TMath.h>

#include "StEvent/StEmcCluster.h"

ClassImp(StEEmcIUSmdCluster);

// ----------------------------------------------------------------------------
StEEmcIUSmdCluster::StEEmcIUSmdCluster()
{
  mEnergy=0.;
  mSumXW=0.;
  mSumX2W=0.;
  mEmcCluster = 0;
}

void StEEmcIUSmdCluster::print()
{
  //  std::cout << "smd cluster plane=" << plane() << std::endl;
  std::cout << "key=" << key() << std::endl;
  std::cout << "energy=" << energy() << std::endl;
  std::cout << "mean=" << mean() << std::endl;
}

StEEmcIUSmdCluster::StEEmcIUSmdCluster( const StEEmcIUSmdCluster &c )
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
  mPlane=c.mPlane;
  mSector=c.mSector;
}
// ----------------------------------------------------------------------------
void StEEmcIUSmdCluster::add( StEEmcStrip strip, Float_t weight )
{

  if ( mStrips.size() == 0 ) 
    {
      mPlane=strip.plane();
      mSector=strip.sector();
    }

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
StEmcCluster *StEEmcIUSmdCluster::stemc()
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
Float_t StEEmcIUSmdCluster::energy(Int_t nmax, Option_t *opts)
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

Float_t StEEmcIUSmdCluster::sigma(Int_t nmax, Option_t *opts)
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

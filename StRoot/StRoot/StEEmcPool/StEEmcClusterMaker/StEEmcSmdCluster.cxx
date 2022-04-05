/**
 * \class StEEmcSmdCluster
 * \brief A base class for representing clusters of EEMC smd strips
 *
 * This class is designed to represent clusters of smd strips in the endcap.
 *
 * \author Jason C. Webb
 * $Date: 2010/08/26 22:49:25 $
 * $Revision: 1.5 $
 *
 * \section steemcsmdcluster_conventions
 *
 * By convention, the first strip added to the cluster is considered
 * to be the "seed" strip.
 *
 */
#include "StEEmcSmdCluster.h"
#include <TMath.h>
#include <TH1F.h>

#include "StEvent/StEmcCluster.h"

ClassImp(StEEmcSmdCluster);

// ----------------------------------------------------------------------------
StEEmcSmdCluster::StEEmcSmdCluster() : StEEmcBaseCluster()
{
  mEnergy=0.; 
  mSumXW=0.; 
  mSumX2W=0.;
  mEmcCluster = 0;
  mLeft=999;
  mRight=-999;
}

// ----------------------------------------------------------------------------
StEEmcSmdCluster::StEEmcSmdCluster( const StEEmcSmdCluster &c ) : StEEmcBaseCluster()
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
void StEEmcSmdCluster::print(Option_t *opts) const
{
  const Char_t *names[]={"u","v"};
  std::cout << "smd cluster plane=" << names[plane()] << std::endl;
  std::cout << "key   = " << key() << std::endl;
  std::cout << "energy= " << energy() << std::endl;
  std::cout << "mean  = " << mean() << std::endl;
  if ( TString(opts).Contains("all") ) {
    for ( UInt_t ii=0;ii<mStrips.size();ii++ )
    {
      mStrips[ii].print();
      std::cout <<" w=" << mWeights[ii] << std::endl;  
    }
  } 
}

// ----------------------------------------------------------------------------
void StEEmcSmdCluster::printLine(Bool_t endline) const
{
  std::cout << strip(0).name() << " " 
	    << energy() << " "
	    << mStrips.size() << " "
	    << mean() << " "
	    << sigma();
  if ( endline ) std::cout << std::endl;
}

// ----------------------------------------------------------------------------
void StEEmcSmdCluster::copy( TH1F *h ) const
{
  for ( UInt_t ii=0;ii<mStrips.size();ii++ )
    h->AddBinContent( mStrips[ii].index()+1, mStrips[ii].energy()*1000.*mWeights[ii] );
}

// ----------------------------------------------------------------------------
void StEEmcSmdCluster::add(const StEEmcStripVec_t &strips)
{
  for ( UInt_t ii=0;ii<strips.size();ii++ ) add( strips[ii] );
}

// ----------------------------------------------------------------------------
void StEEmcSmdCluster::add(const StEEmcStrip &strip, Float_t weight)
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
  mNumberOfElements=mSize;

  if ( strip.index()<=mLeft  ) mLeft=strip.index()-1;
  if ( strip.index()>=mRight ) mRight=strip.index()+1;

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
    if (hit) mEmcCluster->addHit( hit );
  }

  return mEmcCluster;

}

// ----------------------------------------------------------------------------
Float_t StEEmcSmdCluster::energy(Int_t nmax, Option_t *opts) const
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
      const StEEmcStrip &s=mStrips[i];
      if ( s.index() >= min && s.index() <= max ) esum+=s.energy();
    }

  return esum;

}

// ----------------------------------------------------------------------------
Float_t StEEmcSmdCluster::sigma(Int_t nmax, Option_t *opts) const
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
      const StEEmcStrip &s=mStrips[i];
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

// ----------------------------------------------------------------------------
ostream& operator<<(ostream &out, const StEEmcSmdCluster &c)
{
  out << "seed=" 
      << c.strip(0).name() << " " 
      << "key=" 
      << c.key() << " "
      << c.energy() << " "
      << c.size() << " "
      << c.mean() << " "
      << c.sigma();
  return out;
}

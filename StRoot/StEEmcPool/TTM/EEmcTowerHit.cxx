#include <TObject.h>
#include <TMath.h>

#include "EEmcTowerHit.h"


ClassImp(EEmcTowerHit)

EEmcTowerHit::EEmcTowerHit(Int_t s, Int_t ss, Int_t e, Int_t adc, Float_t edep) 
{
  mSec=s; mSub=ss; mEta=e; mADC=adc; mEdep=edep;
};


void    
EEmcTowerHit::setTower (Int_t s, Int_t ss, Int_t e)   
{
  mSec=s; mSub=ss; mEta=e; 
  return;
}
  

Float_t 
EEmcTowerHit::setEnergyADC(Int_t adc, Float_t ped, Float_t gain)
{
  const double kEpsilon=1E-13;

  mADC  = adc; 
  mEdep = Float_t(adc) - ped;  
  if(TMath::Abs(gain)>kEpsilon) mEdep /= gain;
  return mEdep;
} 



#if 0


ClassImp(EEmcHit)



EEmcTrackHit::EEmcTrackHit()
{
  mSec=mSub=mEta=0;
  mADC=0;
  mEdep=0.0;    

  mNumTrig=0;
  for(Int_t i=0;i<kEEmcTowerTrackHitMaxTrigger; i++) mTrigID[i]=0;

  mxVert=myVert=mzVert=0;

  mNHits=0;

  mPt=mPtot=mdEdx=mLength=0.0;

  mEtaSMD=mPhiSMD=mdEtaSMD=mdPhiSMD=0.0;

  mdEtaPRES=mdPhiPRES=mdEtaPOST=mdPhiPOST=0.0; 
}
 
#endif

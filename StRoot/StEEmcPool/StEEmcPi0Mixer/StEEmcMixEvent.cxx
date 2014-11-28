#include "StEEmcMixEvent.h"
ClassImp(StEEmcMixEvent);

// ----------------------------------------------------------------------------
StEEmcMixEvent::StEEmcMixEvent()
{
  for ( Int_t ii=0;ii<90;ii++ )
    {
      pedEEmcDSM_HT[ii]=1;
      pedEEmcDSM_TP[ii]=1;    
    }

  for ( Int_t ii=0;ii<10;ii++ )
    {
      pedEEmcDSM_TP[ii+00]  = (ii%2)? 3 : 5;
      pedEEmcDSM_TP[ii+20]  = (ii%2)? 3 : 5;
      pedEEmcDSM_TP[ii+30]  = (ii%2)? 3 : 5;
      pedEEmcDSM_TP[ii+50]  = (ii%2)? 3 : 5;
      pedEEmcDSM_TP[ii+60]  = (ii%2)? 3 : 5;
      pedEEmcDSM_TP[ii+80]  = (ii%2)? 3 : 5;
    }
  Clear();
}

// ----------------------------------------------------------------------------
void StEEmcMixEvent::setEvent( StMuEvent *event )
{

  mEventId               = event->eventId();
  mEventNumber           = event->eventNumber();
  mRunId                 = event->runId();
  mRunNumber             = event->runNumber();
  mEventInfo             = event->eventInfo();
  mRunInfo               = event->runInfo();
  mL0trigger             = event->l0Trigger();
  mMuTriggerIdCollection = event->triggerIdCollection();
  mMagneticField         = event->magneticField();
  mBbcTrigger            = event->bbcTriggerDetector();
  mEmcTrigger            = event->emcTriggerDetector();

}


// ----------------------------------------------------------------------------
void StEEmcMixEvent::Clear(Option_t *o)
{
  nPairs=0;
  /***
  for ( Int_t i=0;i<MAX_PAIRS;i++ )
  {
      mMass[i]=0.;
      mPT[i]=0.;
      mEta[i]=-10.;
      mPhi[i]=-10.; 
      mZgg[i]=0.;
      mPhigg[i]=0.; 
      mEnergy[i]=0.;
      mEpre1[i]=0.;
      mEpre2[i]=0.;
      mEpost[i]=0.;
      mEsmdu[i]=0.;
      mEsmdv[i]=0.;
      mZvertex[i]=0.;

      mTower1[i]=0;
      mTower2[i]=0;
      mEnergy1[i]=0.;
      mEnergy2[i]=0.;

      mNumberT[i]=0;
      mNumberR[i]=0;
      mNumberU[i]=0;
      mNumberV[i]=0;
  } 
  **/

  mMass.clear();
  mPT.clear();
  mEta.clear();
  mPhi.clear();
  mZgg.clear();
  mPhigg.clear();
  mEnergy.clear();
  mEpre1.clear();
  mEpre2.clear();
  mEpost.clear();
  mEsmdu.clear();
  mEsmdv.clear();
  mZvertex.clear();
  
  mTower1.clear();
  mTower2.clear();
  mEnergy1.clear();
  mEnergy2.clear();
  
  mNumberT.clear();
  mNumberR.clear();
  mNumberU.clear();
  mNumberV.clear();

  mNumberOfTracks.clear();
  mNumberOfPoints.clear();

  mNumberOfTowerClusters.clear();
  mNumberOfSmduClusters.clear();
  mNumberOfSmdvClusters.clear();

  for ( Int_t ii=0;ii<720;ii++ ) {
    mADC[ii]=0.;
    mGain[ii]=-1.;
    mStat[ii]=0;
  }
    
    

  mTotalEnergyT=0.;
  mTotalEnergyP=0.;
  mTotalEnergyQ=0.;
  mTotalEnergyR=0.;
  mTotalEnergyU=0.;
  mTotalEnergyV=0.; 

}

// ----------------------------------------------------------------------------
void StEEmcMixEvent::addPair ( StEEmcPair  p ) { 

  //    mMass[nPairs]   = p.mass();

  mMass.push_back ( p.mass() );
  mPT.push_back( p.pt() );
  mEta.push_back( p.momentum().Eta() );
  mPhi.push_back( p.momentum().Phi() );
  mPhigg.push_back( p.phigg() );
  mZgg.push_back( p.zgg() );
  mEnergy.push_back( p.energy() );

  Float_t esmdu=0.;
  Float_t esmdv=0.;
  esmdu += p.point(0).cluster(0).energy();
  esmdu += p.point(1).cluster(0).energy();
  esmdv += p.point(0).cluster(1).energy();
  esmdv += p.point(1).cluster(1).energy();

  Int_t index1=p.point(0).tower(0).index();
  Int_t index2=p.point(1).tower(0).index();

  Float_t epre1 = 0.;
  epre1 += p.point(0).energy(1);
  if (index1!=index2) epre1 += p.point(1).energy(1);

  Float_t epre2 = 0.;
  epre2 += p.point(0).energy(2);
  if (index1!=index2) epre2 += p.point(1).energy(2);

  Float_t epost = 0.;
  epost += p.point(0).energy(3);
  if (index1!=index2) epost += p.point(1).energy(3);

  mEpre1.push_back( epre1 );
  mEpre2.push_back( epre2 );
  mEpost.push_back( epost );
  mEsmdu.push_back( esmdu );
  mEsmdv.push_back( esmdv );

  mZvertex.push_back( p.vertex().Z() );

  mTower1.push_back( p.point(0).tower(0).index() );
  mTower2.push_back( p.point(1).tower(0).index() );
  mEnergy1.push_back( p.point(0).energy() );
  mEnergy2.push_back( p.point(1).energy() );

  nPairs++; 

}

// ----------------------------------------------------------------------------
Float_t StEEmcMixEvent::sum3x3(Int_t index)
{
  Float_t sum=this->mADC[index];
  Int_t ieta = index%12;
  Int_t iphi = index/12;
  /// loop over adjacent towers
  for ( Int_t jeta=ieta-1;jeta<=ieta+1;jeta++ ) {
    if ( jeta<0||jeta>11 ) continue;
    for ( Int_t jphi=iphi-1;jphi<=iphi+1;jphi++ ) {
      Int_t myeta=jeta;
      Int_t myphi=(jphi+60)%60;
      if ( myeta==ieta&&myphi==iphi ) continue;
      Float_t myEt=this->mADC[ myeta + 12*myphi ];
      sum += myEt;
    }
  }
  return sum;
}
Float_t StEEmcMixEvent::sum3x3()
{
  Float_t max=0.;
  Int_t index = 0;
  for ( Int_t ii=0;ii<720;ii++ )
    if ( this->mADC[ii] > max ) { 
      index=ii;
      max=this->mADC[ii];
    }
  return sum3x3(index);    
}
Float_t StEEmcMixEvent::htdsm()
{
  Float_t max=0.;
  for ( Int_t ii=0;ii<90;ii++ ) {
    Float_t dsm=mEmcTrigger.highTowerEndcap(ii);
    dsm-=pedEEmcDSM_HT[ii];
    if ( dsm>max ) max=dsm;
  }
  return max;
}
Float_t StEEmcMixEvent::tpdsm()
{
  Float_t max=0.;
  for ( Int_t ii=0;ii<90;ii++ ) {
    Float_t dsm=mEmcTrigger.patchEndcap(ii);
    dsm-=pedEEmcDSM_TP[ii];
    if ( dsm>max ) max=dsm;
  }
  return max;
}
Int_t StEEmcMixEvent::npi0()
{
  Int_t count=0;
  for ( Int_t ii=0;ii<TMath::Min(5,nPairs);ii++ )
    {
      if ( mMass[ii]>0.11 && mMass[ii]<0.17 ) count++;
    }
  return count;
}
Int_t StEEmcMixEvent::deta(Int_t id)
{
  if ( id<0||id>=nPairs)
    return -1;
  Int_t del = ( mTower1[id]%12 - mTower2[id]%12 );
  return TMath::Abs(del);
}
Int_t StEEmcMixEvent::dphi(Int_t id)
{
  if ( id<0||id>=nPairs)
    return -1;
  Int_t del = ( mTower1[id]/12 - mTower2[id]/12 + 60 ) % 60;
  return TMath::Abs(del);
}

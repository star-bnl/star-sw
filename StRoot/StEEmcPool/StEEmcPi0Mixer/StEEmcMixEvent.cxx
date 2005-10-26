#include "StEEmcMixEvent.h"
ClassImp(StEEmcMixEvent);

// ----------------------------------------------------------------------------
StEEmcMixEvent::StEEmcMixEvent()
{
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

}


// ----------------------------------------------------------------------------
void StEEmcMixEvent::Clear(Option_t *o)
{
  nPairs=0;
  for ( Int_t i=0;i<MAX_PAIRS;i++ )
  {
      mMass[i]=0.;
      mPT[i]=0.;
      mEta[i]=-10.;
      mPhi[i]=-10.; 
      mZgg[i]=0.;
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
  } 
}

// ----------------------------------------------------------------------------
void StEEmcMixEvent::addPair ( StEEmcPair  p ) { 
    if ( nPairs >= MAX_PAIRS ) return;  
    mMass[nPairs] = p.mass();
    mPT[nPairs]   = p.pt();
    mEta[nPairs]  = p.momentum().Eta();
    mPhi[nPairs]  = p.momentum().Phi(); 
    mZgg[nPairs]  = p.zgg();
    mEnergy[nPairs] = p.energy();

    Float_t esmdu=0.;
    Float_t esmdv=0.; 
    esmdu += p.point(0).cluster(0).energy();
    esmdu += p.point(1).cluster(0).energy();
    esmdv += p.point(0).cluster(1).energy();
    esmdv += p.point(1).cluster(1).energy();
    Float_t epre1 = 0.;
    epre1 += p.point(0).energy(1);
    epre1 += p.point(1).energy(1);
    Float_t epre2 = 0.;
    epre2 += p.point(0).energy(2);
    epre2 += p.point(1).energy(2);
    Float_t epost = 0.;
    epost += p.point(0).energy(3);
    epost += p.point(1).energy(3);

    mEpre1[nPairs]=epre1;
    mEpre2[nPairs]=epre2;
    mEpost[nPairs]=epost;
    mEsmdu[nPairs]=esmdu;
    mEsmdv[nPairs]=esmdv; 

    mZvertex[nPairs]=p.vertex().Z(); 

    mTower1[nPairs] = p.point(0).tower(0).index();
    mEnergy1[nPairs]= p.point(0).energy();

    mTower2[nPairs] = p.point(1).tower(0).index();
    mEnergy2[nPairs] = p.point(1).energy(); 

    nPairs++; 

}

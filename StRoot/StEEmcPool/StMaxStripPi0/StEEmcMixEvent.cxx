#include "StEEmcMixEvent.h"
ClassImp(StEEmcMixEvent);

// ----------------------------------------------------------------------------
StEEmcMixEvent::StEEmcMixEvent()
{

  mRealPairs  = new TClonesArray("StEEmcPair",500);
  mMixedPairs = new TClonesArray("StEEmcPair",500);
  mPoints     = new TClonesArray("StEEmcPoint",500);
  mTowers     = new TClonesArray("StEEmcTower",720); 

  mClustersU  = new TClonesArray("StEEmcSmdCluster",20);
  mClustersV  = new TClonesArray("StEEmcSmdCluster",20);

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
  mMuTriggerIdCollection = event->triggerIdCollection();
  mL0trigger             = event->l0Trigger();
  mMagneticField         = event->magneticField();

}


// ----------------------------------------------------------------------------
void StEEmcMixEvent::Clear(Option_t *o)
{
  mRealPairs->Delete();
  mMixedPairs->Delete();
  mPoints->Delete();
  mClustersU->Delete();
  mClustersV->Delete();
  
  nReal=0;
  nMixed=0;
  nPoints=0;
  nClustersU=0;
  nClustersV=0;

  mEtotal=0.;
  mEpoints=0.;
}

// ----------------------------------------------------------------------------
void StEEmcMixEvent::addPoint( StEEmcPoint p ) { 
  TClonesArray &points=*mPoints;
  new(points[nPoints++]) StEEmcPoint(p); 
  mEpoints+=p.energy();
}
void StEEmcMixEvent::addPair ( StEEmcPair  p ) { 
  TClonesArray &pairs=*mRealPairs;
  new(pairs[nReal++]) StEEmcPair(p); 
}
void StEEmcMixEvent::addMixed( StEEmcPair  p ) { 
  TClonesArray &mixed=*mMixedPairs;
  new(mixed[nMixed++]) StEEmcPair(p); 
}
void StEEmcMixEvent::addTower( StEEmcTower t ) {
    TClonesArray &towers=*mTowers;
    new(towers[nTowers++]) StEEmcTower(t); 
} 

void StEEmcMixEvent::addCluster( StEEmcSmdCluster c )
{
  if ( c.plane()==0 ) 
    {
      TClonesArray &clusters=*mClustersU;
      new (clusters[nClustersU++]) StEEmcSmdCluster(c);
    }
  else
    {
      TClonesArray &clusters=*mClustersV;
      new (clusters[nClustersV++]) StEEmcSmdCluster(c);
    }
      
}

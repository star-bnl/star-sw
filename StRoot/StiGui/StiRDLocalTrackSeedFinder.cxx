//StiRDLocalTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//11/01

#include "StiGui/StiRootDrawableHits.h"
#include "StiGui/StiRootDisplayManager.h"
#include <math.h>
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StThreeVector.hh"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiDetector.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetectorContainer.h"
#include "StiRootDisplayManager.h"
#include "StiRootDrawableHits.h"
#include "StiRDLocalTrackSeedFinder.h"

ostream& operator<<(ostream&, const StiDetector&);

StiRDLocalTrackSeedFinder::StiRDLocalTrackSeedFinder(const string& name,
						     Factory<StiKalmanTrack>* trackFactory,
						     StiHitContainer* hitContainer,
						     StiDetectorContainer    * detectorContainer)
  : StiLocalTrackSeedFinder(name,trackFactory,hitContainer,detectorContainer),
    mdrawablehits(new StiRootDrawableHits())
{
  cout <<"StiRDLocalTrackSeedFinder::StiRDLocalTrackSeedFinder()"<<endl;
  mdrawablehits->clear();
  mdrawablehits->setColor(3);
  mdrawablehits->setMarkerStyle(3);
  mdrawablehits->setMarkerSize(1.);
  mdrawablehits->setRemoved(false);
  //mdrawablehits->setName("Seed Finder Hits");
  StiRootDisplayManager::instance()->addDrawable(mdrawablehits);
}

StiRDLocalTrackSeedFinder::~StiRDLocalTrackSeedFinder()
{
  cout <<"StiRDLocalTrackSeedFinder::~StiRDLocalTrackSeedFinder()"<<endl;
  //Note, do not call delete on drawable hits, they're owned by root
}

void StiRDLocalTrackSeedFinder::reset()
{
  mdrawablehits->clear();
  //Cleanup the base-class
  StiLocalTrackSeedFinder::reset();
}


StiKalmanTrack* StiRDLocalTrackSeedFinder::makeTrack(StiHit* hit)
{
  _messenger <<"StiRDLocalTrackSeedFinder::makeTrack()"<<endl;
  
  mdrawablehits->clear();
  StiKalmanTrack* track = StiLocalTrackSeedFinder::makeTrack(hit);
  if (!track)return track;
  _messenger<<"\tGet Global positions:\t";
  for (HitVec::const_iterator it=mSeedHitVec.begin(); it!=mSeedHitVec.end(); ++it) {
    const StThreeVectorF& pos = (*it)->globalPosition();
    mdrawablehits->push_back( pos.x() );
    mdrawablehits->push_back( pos.y() );
    mdrawablehits->push_back( pos.z() );
  }
  mdrawablehits->fillHitsForDrawing();
  if (false)
    {
      StiRootDisplayManager::instance()->draw();
      StiRootDisplayManager::instance()->update();
    }
  _messenger <<"\t leaving StiRDLocalTrackSeedFinder::makeTrack()"<<endl;
  return track;
}


//StiRDLocalTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//11/01

//StiGui
#include "StiGui/StiRootDrawableHits.h"
#include "StiGui/StiDisplayManager.h"

//std
#include <math.h>

//scl
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StThreeVector.hh"

//Sti
#include "Sti/StiIOBroker.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiDetector.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetectorContainer.h"

//StiGui
#include "StiDisplayManager.h"
#include "StiRootDrawableHits.h"

#include "StiRDLocalTrackSeedFinder.h"

ostream& operator<<(ostream&, const StiDetector&);

StiRDLocalTrackSeedFinder::StiRDLocalTrackSeedFinder(StiDetectorContainer* det,
						     StiHitContainer* hits)    
    : StiLocalTrackSeedFinder(det, hits), mdrawablehits(new StiRootDrawableHits())
{
    cout <<"StiRDLocalTrackSeedFinder::StiRDLocalTrackSeedFinder()"<<endl;

    mdrawablehits->clear();
    mdrawablehits->setColor(3);
    mdrawablehits->setMarkerStyle(3);
    mdrawablehits->setMarkerSize(1.);
    mdrawablehits->setRemoved(false);
    //mdrawablehits->setName("Seed Finder Hits");
    StiDisplayManager::instance()->addDrawable(mdrawablehits);

    getNewState();
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
#ifdef DEBUG
    mMessenger <<"StiRDLocalTrackSeedFinder::makeTrack()"<<endl;
#endif

    mdrawablehits->clear();
    StiKalmanTrack* track = StiLocalTrackSeedFinder::makeTrack(hit);
    if (!track) {
	return track;
    }

#ifdef DEBUG
    mMessenger<<"\tGet Global positions:\t";
#endif

    for (HitVec::const_iterator it=mSeedHitVec.begin(); it!=mSeedHitVec.end(); ++it) {
	const StThreeVectorF& pos = (*it)->globalPosition();
	mdrawablehits->push_back( pos.x() );
	mdrawablehits->push_back( pos.y() );
	mdrawablehits->push_back( pos.z() );
    }

#ifdef DEBUG
    mMessenger <<"done."<<endl;
#endif
    
    mdrawablehits->fillHitsForDrawing();    
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();

#ifdef DEBUG
    mMessenger <<"\t leaving StiRDLocalTrackSeedFinder::makeTrack()"<<endl;
#endif
    
    return track;
}

void StiRDLocalTrackSeedFinder::getNewState()
{
#ifdef DEBUG
    mMessenger <<"StiRDLocalTrackSeedFinder::getNewState()"<<endl;
#endif
    //const StiIOBroker* broker = StiIOBroker::instance();
    StiLocalTrackSeedFinder::getNewState();
}

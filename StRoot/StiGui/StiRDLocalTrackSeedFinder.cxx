//StiRDLocalTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//11/01

//StiGui
#include "StiGui/StiRootDrawableHits.h"
#include "StiGui/StiRootDisplayManager.h"

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
#include "StiGuiIOBroker.h"
#include "StiRootDisplayManager.h"
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
    StiRootDisplayManager::instance()->addDrawable(mdrawablehits);

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
    mMessenger <<"StiRDLocalTrackSeedFinder::makeTrack()"<<endl;

    mdrawablehits->clear();
    StiKalmanTrack* track = StiLocalTrackSeedFinder::makeTrack(hit);
    if (!track) {
	return track;
    }

    mMessenger<<"\tGet Global positions:\t";

    for (HitVec::const_iterator it=mSeedHitVec.begin(); it!=mSeedHitVec.end(); ++it) {
	const StThreeVectorF& pos = (*it)->globalPosition();
	mdrawablehits->push_back( pos.x() );
	mdrawablehits->push_back( pos.y() );
	mdrawablehits->push_back( pos.z() );
    }

    mMessenger <<"done."<<endl;
    
    mdrawablehits->fillHitsForDrawing();
    if (StiGuiIOBroker::instance()->updateEachTrack()) {
	StiRootDisplayManager::instance()->draw();
	StiRootDisplayManager::instance()->update();
    }

    mMessenger <<"\t leaving StiRDLocalTrackSeedFinder::makeTrack()"<<endl;
    
    return track;
}

void StiRDLocalTrackSeedFinder::getNewState()
{
    mMessenger <<"StiRDLocalTrackSeedFinder::getNewState()"<<endl;
    StiLocalTrackSeedFinder::getNewState();
}

//StiTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01

#include <iostream.h>

//StarClassLibrary
#include "StGetConfigValue.hh"
#include "StThreeVectorF.hh"

//StiGui
#include "StiGui/StiRootDrawableHits.h"
#include "StiGui/StiDisplayManager.h"

//Sti
#include "StiKalmanTrack.h"
#include "StiMapUtilities.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiTrackSeedFinder.h"
#include "StiDetector.h"
#include "StiDetectorContainer.h"

ostream& operator<<(ostream& os, const StiHit& hit);

StiTrackSeedFinder::StiTrackSeedFinder(StiDetectorContainer* det,
				       StiHitContainer* h, Sti2HitComboFilter* filter)
    : StiSeedFinder(),
      mDetStore(det), mhitstore(h), mdrawablehits(new StiRootDrawableHits()),
      mhitcombofilter(filter)
{
    //mMessenger <<"StiTrackSeedFinder::StiTrackSeedFinder()"<<endl;
    if (!mhitcombofilter) {
	mMessenger <<"StiTrackSeedFinder::StiTrackSeedFinder(). ERROR:\t";
	mMessenger <<"hitComboFilter==0.  Undefined Behavior."<<endl;
    }

    //Look at seeds (temp, MLM 8/8/01)
    mdrawablehits->clear();
    mdrawablehits->setColor(3);
    mdrawablehits->setMarkerStyle(3);
    mdrawablehits->setMarkerSize(1.);
    mdrawablehits->setRemoved(false);
    //mdrawablehits->setName("Seed Finder Hits");
    StiDisplayManager::instance()->addDrawable(mdrawablehits);
}

StiTrackSeedFinder::~StiTrackSeedFinder()
{
    //mMessenger <<"StiTrackSeedFinder::~StiTrackSeedFinder()"<<endl;
    //Note, do not call delete on drawable hits, they're owned by root
    
    delete mhitcombofilter;
    mhitcombofilter=0;
}

void StiTrackSeedFinder::reset()
{
    mdrawablehits->clear();
    return;
}

//STL Functors

void StiRectangular2HitComboFilter::build(const string buildPath)
{
    if (buildPath=="empty") {
	*(Messenger::instance(kSeedFinderMessage)) <<"StiRectangular2HitComboFilter::build(). ERROR:\t";
	*(Messenger::instance(kSeedFinderMessage)) <<"buildPath==empty. Abort"<<endl;
	return;
    }
    StGetConfigValue(buildPath.c_str(), "deltaD", deltaD);
    StGetConfigValue(buildPath.c_str(), "deltaZ", deltaZ);
    if (deltaD==-1 || deltaZ==-1) {
	*(Messenger::instance(kSeedFinderMessage)) <<"StiRectangular2HitComboFilter::build(). ERROR:\t";
	*(Messenger::instance(kSeedFinderMessage))  <<"deltaD or deltaZ not set.. Abort"<<endl;
	return;
    }
}

void StiCollinear2HitComboFilter::build(const string buildPath)
{
    if (buildPath=="empty") {
	*(Messenger::instance(kSeedFinderMessage)) <<"StiCollinear2HitComboFilter::build(). ERROR:\t";
	*(Messenger::instance(kSeedFinderMessage)) <<"buildPath==empty. Abort"<<endl;
	return;
    }
    StGetConfigValue(buildPath.c_str(), "deltaPhi", deltaPhi);
    StGetConfigValue(buildPath.c_str(), "deltaTheta", deltaTheta);
    if (deltaPhi==-1 || deltaTheta==-1) {
	*(Messenger::instance(kSeedFinderMessage)) <<"StiCollinear2HitComboFilter::build(). ERROR:\t";
	*(Messenger::instance(kSeedFinderMessage)) <<"deltaPhi or deltaTheta not set.. Abort"<<endl;
	return;
    }
}

bool StiCollinear2HitComboFilter::operator()(const StiHit* hit1, const StiHit* hit2) const
{
    //define collinearity with vertex.  For now use origin, could plug in z-vertex from zdc in future
    return ( ( fabs( hit1->globalPosition().phi()-hit2->globalPosition().phi()) < deltaPhi) &&
	     ( fabs( hit1->globalPosition().theta()-hit2->globalPosition().theta()) < deltaTheta) );
}

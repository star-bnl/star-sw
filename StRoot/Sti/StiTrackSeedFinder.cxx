//StiTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01

#include <iostream>

//StarClassLibrary
#include "StGetConfigValue.hh"
#include "StThreeVectorF.hh"

//StiGui
#include "StiGui/StiRootDrawableHits.h"
#include "StiGui/StiDisplayManager.h"

//Sti
#include "StiKalmanTrack.h"
#include "StiMapUtilities.h"
#include "StiHitContainer.h"
#include "CombinationIterator.h"
#include "StiTrackSeedFinder.h"
#include "StiDetectorContainer.h"


ostream& operator<<(ostream& os, const StiHit& hit);

StiTrackSeedFinder::StiTrackSeedFinder(StiHitContainer* h, Sti2HitComboFilter* filter)
    : mhitstore(h), mtrackfactory(0), mdrawablehits(new StiRootDrawableHits()), mhitcombofilter(filter)
{
    //cout <<"StiTrackSeedFinder::StiTrackSeedFinder()"<<endl;
    if (!mhitcombofilter) {
	cout <<"StiTrackSeedFinder::StiTrackSeedFinder(). ERROR:\thitComboFilter==0.  Undefined Behavior."<<endl;
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
    //cout <<"StiTrackSeedFinder::~StiTrackSeedFinder()"<<endl;
    //Note, do not call delete on drawable hits, they're owned by root
}

StiTrackSeedFinder::StiTrackSeedFinder(const StiTrackSeedFinder& rhs)
    : StiSeedFinder(rhs), mhitstore(0), mtrackfactory(0), mdrawablehits(0), mhitcombofilter(0)
{
    copyToThis(rhs);
}

StiTrackSeedFinder& StiTrackSeedFinder::operator=(const StiTrackSeedFinder& rhs)
{
    if (this==&rhs) return *this; //don't assign to this!
    StiSeedFinder::operator=(rhs);
    copyToThis(rhs);
    return *this;
}

void StiTrackSeedFinder::copyToThis(const StiTrackSeedFinder& rhs)
{
    //shallow copies
    mhitstore = rhs.mhitstore;
    mtrackfactory = rhs.mtrackfactory;
    mnlayers = rhs.mnlayers;
    mdrawablehits = rhs.mdrawablehits; //root owns this object
    miterator = rhs.miterator;
    mhitcombofilter = rhs.mhitcombofilter;

    //StiDisplayManager::instance()->addDrawable(mdrawablehits);
}

void StiTrackSeedFinder::build()
{
    cout <<"StiTrackSeedFinder::build()"<<endl;
}

int StiTrackSeedFinder::numberOfLayers() const 
{
    return mnlayers;
}

void StiTrackSeedFinder::clear()
{
    mnlayers = 0;
    miterator.clear();
    mdrawablehits->clear();
    return;
}

void StiTrackSeedFinder::addLayer(double refangle, double position)
{
    const hitvector& vec = mhitstore->hits(refangle, position);
    miterator.push_back( vec);
    ++mnlayers;
    miterator.init();
    return;
}

void StiTrackSeedFinder::print() const
{
    miterator.print();
    return;
}

bool StiTrackSeedFinder::hasMore()
{
    return ( miterator.current() < miterator.size() );
}

//Loop on combinations until we get a valid one
StiKalmanTrack* StiTrackSeedFinder::next()
{
    if (mhitcombofilter == 0) {
	cout <<"StiTrackSeedFinder::next()\tError!:\t mhitcombofilter==0. ABORT"<<endl;
	return 0;
    }
    else if (mtrackfactory == 0) {
	cout <<"StiTrackSeedFinder::next()\tError!:\t mtrackfactory==0. ABORT"<<endl;
	return 0;
    }
    
    bool go = true;
    StiKalmanTrack* track = 0;
    while (go && hasMore()) {
	//cout <<"\nCombination "<<it.current()<<" -----------"<<endl;
	track = makeTrack( miterator() );
	if (track) {
	    go=false; //We found a good track, return it.  Else, we keep searching combinations
	    //cout <<"Found a good track, return it"<<endl;
	}
	++miterator;
    }
    return track;
}

//check points in a given combination, return track if accepted
StiKalmanTrack* StiTrackSeedFinder::makeTrack(const tvector& vec) const
{
    //cout <<"StiTrackSeedFinder::makeTrack()"<<endl;
    //Construct Track fromt these points
    StiKalmanTrack* track = 0;
    if (vec.size()<3) {
	cout <<"StiTrackSeedFinder::makeTrack()\tError:\tvec.size()<3  Abort"<<endl;
	return track;
    }
    mdrawablehits->clear();

    //This is an ugly loop ,but it is chosen for efficiency to avoid multiple loops over the points
    //and terminate the loop immediately if a hit combination doesn't pass the filter
    bool go=true;
    mdrawablehits->push_back( vec[0] ); //Temp, MLM
    for (unsigned int i=1; i<vec.size() && go; ++i) { //start at begin+1
	go = mhitcombofilter->operator()( vec[i-1], vec[i] );
	mdrawablehits->push_back( vec[i] ); //Temp, MLM
    }
    if (go) { //They're all good
	mdrawablehits->fillHitsForDrawing();
	//Fit points to helix, etc
	track = mtrackfactory->getObject();
	track->reset();
	//Setup DetectorContainer to inner-most point
	//This assumes outside in tracking, can be made to switch on direction
	StiDetectorContainer::instance()->setToDetector( vec.front()->detector() );
    }
    //cout <<"\tLeaving"<<endl;
    return track;
}

void StiRectangular2HitComboFilter::build(const string& buildPath)
{
    if (buildPath=="empty") {
	cout <<"StiRectangular2HitComboFilter::build(). ERROR:\tbuildPath==empty. Abort"<<endl;
	return;
    }
    StGetConfigValue(buildPath.c_str(), "deltaD", deltaD);
    StGetConfigValue(buildPath.c_str(), "deltaZ", deltaZ);
    if (deltaD==-1 || deltaZ==-1) {
	cout <<"StiRectangular2HitComboFilter::build(). ERROR:\tdeltaD or deltaZ not set.. Abort"<<endl;
	return;
    }
}

void StiCollinear2HitComboFilter::build(const string& buildPath)
{
    if (buildPath=="empty") {
	cout <<"StiCollinear2HitComboFilter::build(). ERROR:\tbuildPath==empty. Abort"<<endl;
	return;
    }
    StGetConfigValue(buildPath.c_str(), "deltaPhi", deltaPhi);
    StGetConfigValue(buildPath.c_str(), "deltaTheta", deltaTheta);
    if (deltaPhi==-1 || deltaTheta==-1) {
	cout <<"StiCollinear2HitComboFilter::build(). ERROR:\tdeltaPhi or deltaTheta not set.. Abort"<<endl;
	return;
    }
}

bool StiCollinear2HitComboFilter::operator()(const StiHit* hit1, const StiHit* hit2) const
{
    //define collinearity with vertex.  For now use origin, could plug in z-vertex from zdc in future
    return ( ( fabs( hit1->globalPosition().phi()-hit2->globalPosition().phi()) < deltaPhi) &&
	     ( fabs( hit1->globalPosition().theta()-hit2->globalPosition().theta()) < deltaTheta) );
}

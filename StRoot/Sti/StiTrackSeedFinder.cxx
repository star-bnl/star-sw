//StiTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01

#include <iostream>

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

StiTrackSeedFinder::StiTrackSeedFinder(StiHitContainer* h)
    : mhitstore(h), miterator(new combo_iterator()), mtrackfactory(0),
      mdrawablehits(new StiRootDrawableHits()), mhitcombofilter(0)
{

    //Setup a default 2 hit filter
    StiRectangular2HitComboFilter* temp = new StiRectangular2HitComboFilter();
    temp->deltaD = 2.; //TEMP
    temp->deltaZ = 2.; //TEMP
    mhitcombofilter = temp;

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
    cout <<"StiTrackSeedFinder::~StiTrackSeedFinder()"<<endl;
    delete miterator;
    miterator = 0;
}

int StiTrackSeedFinder::numberOfLayers() const 
{
    return mnlayers;
}

void StiTrackSeedFinder::clear()
{
    mnlayers = 0;
    miterator->clear();
    mdrawablehits->clear();
    return;
}

void StiTrackSeedFinder::addLayer(double refangle, double position)
{
    const hitvector& vec = mhitstore->hits(refangle, position);
    miterator->push_back( vec);
    ++mnlayers;
    miterator->init();
    return;
}

void StiTrackSeedFinder::print() const
{
    miterator->print();
    return;
}

bool StiTrackSeedFinder::hasMore()
{
    return ( miterator->current() < miterator->size() );
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
    
    combo_iterator& it = *miterator;
    bool go = true;
    StiKalmanTrack* track = 0;
    while (go && hasMore()) {
	//cout <<"\nCombination "<<it.current()<<" -----------"<<endl;
	track = makeTrack( it() );
	if (track) {
	    go=false; //We found a good track, return it.  Else, we keep searching combinations
	    //cout <<"Found a good track, return it"<<endl;
	}
	++it;
    }
    return track;
}

//check points in a given combination, return track if accepted
StiKalmanTrack* StiTrackSeedFinder::makeTrack(const tvector& vec) const
{
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
    return track;
}

bool StiTrackSeedFinder::acceptCombination(const tvector& vec) const
{
    //bool go=true;
    return true;
}


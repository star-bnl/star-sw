//StiTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01

#include <iostream>

#include "StiKalmanTrack.h"
#include "StiMapUtilities.h"
#include "StiHitContainer.h"
#include "CombinationIterator.h"
#include "StiTrackSeedFinder.h"

StiTrackSeedFinder::StiTrackSeedFinder(StiHitContainer* h) : mhitstore(h)
{
    miterator = new combo_iterator();
}

StiTrackSeedFinder::~StiTrackSeedFinder()
{
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
    return;
}

void StiTrackSeedFinder::init()
{
    miterator->init();
    return;
}

void StiTrackSeedFinder::addLayer(double refangle, double position)
{
    miterator->push_back( mhitstore->hits(refangle, position) );
    ++mnlayers;
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

StiKalmanTrack* StiTrackSeedFinder::next()
{
    combo_iterator& it = *miterator;
    cout <<"\nCombination "<<it.current()<<" -----------"<<endl;
    StiKalmanTrack* track = makeTrack( it() );
    ++it;
    return track;
}

StiKalmanTrack* StiTrackSeedFinder::makeTrack(const tvector& vec) const
{
    //Construct Track fromt these points
    StiKalmanTrack* track = 0;
    for (tvector::const_iterator cit=vec.begin(); cit!=vec.end(); ++cit) {
	//cout <<"\t"<<*(*cit)<<endl;
    }
    return track;
}

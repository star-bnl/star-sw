//StiHitSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01

#include <iostream>

#include "StiDummyTrack.h"
#include "StiMapUtilities.h"
#include "StiHitContainer.h"
#include "CombinationIterator.h"
#include "StiHitSeedFinder.h"

StiHitSeedFinder::StiHitSeedFinder(StiHitContainer* h) : mhitstore(h)
{
    miterator = new combo_iterator();
    mtrackpair = new sti_track_pair();   
}

StiHitSeedFinder::~StiHitSeedFinder()
{
    delete miterator;
    miterator = 0;
    delete mtrackpair;
    mtrackpair = 0;
}

int StiHitSeedFinder::numberOfLayers() const 
{
    return mnlayers;
}

void StiHitSeedFinder::clear()
{
    mnlayers = 0;
    miterator->clear();
    return;
}

void StiHitSeedFinder::init()
{
    miterator->init();
    return;
}

void StiHitSeedFinder::addLayer(int sector, int padrow)
{
    miterator->push_back( mhitstore->hits(sector, padrow) );
    ++mnlayers;
    return;
}

void StiHitSeedFinder::print() const
{
    miterator->print();
    return;
}

bool StiHitSeedFinder::hasMore()
{
    return ( miterator->current() < miterator->size() );
}

StiSeedFinder::sti_track_pair* StiHitSeedFinder::next()
{
    combo_iterator& it = *miterator;
    cout <<"\nCombination "<<it.current()<<" -----------"<<endl;
    StiDummyTrack* track = makeTrack( it() );
    ++it;
    mtrackpair->first=track;
    mtrackpair->second=0;
    return mtrackpair;
}

StiDummyTrack* StiHitSeedFinder::makeTrack(const tvector& vec) const
{
    //Construct Track fromt these points
    StiDummyTrack* track = 0;
    for (tvector::const_iterator cit=vec.begin(); cit!=vec.end(); ++cit) {
	//cout <<"\t"<<*(*cit)<<endl;
    }
    return track;
}

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


ostream& operator<<(ostream& os, const StiHit& hit);

StiTrackSeedFinder::StiTrackSeedFinder(StiHitContainer* h) : mhitstore(h)
{
    miterator = new combo_iterator();
    
    //Look at seeds (temp, MLM 8/8/01)
    mdrawablehits = new StiRootDrawableHits();
    mdrawablehits->clear();
    mdrawablehits->setColor(3);
    mdrawablehits->setMarkerStyle(3);
    mdrawablehits->setMarkerSize(1.);
    mdrawablehits->setRemoved(false);
    StiDisplayManager::instance()->addDrawable(mdrawablehits);
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
    mdrawablehits->clear();
    return;
}

void StiTrackSeedFinder::init()
{
    miterator->init();
    return;
}

void StiTrackSeedFinder::addLayer(double refangle, double position)
{
    const hitvector& vec = mhitstore->hits(refangle, position);
    //cout <<"StiTrackSeedFinder::addLayer(double, double)"<<endl;
    //cout <<"refangle: "<<refangle<<"\tposition: "<<position<<"\tnhits: "<<vec.size()<<endl;

    miterator->push_back( vec);
    //miterator->push_back( mhitstore->hits(refangle, position) );
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
    mdrawablehits->clear();

    cout <<"StiTrackSeedFinder::makeTrack()\tConstruct seed from"<<endl;
    
    for (tvector::const_iterator cit=vec.begin(); cit!=vec.end(); ++cit) {
	//cout <<"\t"<<*(*cit)<<endl;
	mdrawablehits->push_back( (*cit) );
    }
    mdrawablehits->fillHitsForDrawing();
    return track;
}

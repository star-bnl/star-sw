//StiCompositeSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01

//Std
#include <stdio.h>
#include <iostream.h>
#include <algorithm>
using std::ostream_iterator;
using std::copy;

//Scl
#include "StGetConfigValue.hh"

//StiGui
#include "StiGui/StiRDLocalTrackSeedFinder.h"

//Sti
#include "StiToolkit.h"
#include "StiIOBroker.h"
#include "StiHitContainer.h"
#include "StiDetectorContainer.h"
#include "StiTrackSeedFinder.h"
#include "StiLocalTrackSeedFinder.h"
#include "StiDetector.h"
#include "StiDetectorFinder.h"
#include "StiCompositeSeedFinder.h"
#include "MessageType.h"

StiCompositeSeedFinder::StiCompositeSeedFinder(Factory<StiKalmanTrack>* fact,
					       StiHitContainer* hc)
    : StiSeedFinder(hc)
{
    cout <<"StiCompositeSeedFinder::StiCompositeSeedFinder()"<<endl;
    mFactory = fact;
    build();
}

StiCompositeSeedFinder::~StiCompositeSeedFinder()
{
    cout <<"StiCompositeSeedFinder::~StiCompositeSeedFinder()"<<endl;

    //Destroy seed finders
    for (SeedFinderVec::iterator it=mSeedVec.begin(); it!=mSeedVec.end(); ++it) {
	delete *it;
    }
}

bool StiCompositeSeedFinder::hasMore()
{
    mMessenger <<"StiCompositeSeedFinder::hasMore()"<<endl;
    
    bool val =  mCurrent>=mSeedVec.begin() && mCurrent<mSeedVec.end()
	&& (*mCurrent)->hasMore();
    
    mMessenger <<"\t  returning "<<val<<endl;
    
    return val;
}

StiKalmanTrack* StiCompositeSeedFinder::next()
{
    mMessenger <<"StiCompositeSeedFinder::next()"<<endl;
    
    StiKalmanTrack* track=0;
    //while ((*mCurrent)->hasMore() && track==0) {
    track = (*mCurrent)->next();
    //}
    
    //Check to see if we ran out
    if ( (*mCurrent)->hasMore()==false ) {
	++mCurrent;
    }
    
    mMessenger <<"\t leaving StiCompositeSeedFinder::next()"<<endl;
    
    return track;
}

void StiCompositeSeedFinder::reset()
{
    
    mMessenger <<"StiCompositeSeedFinder::reset()"<<endl;
    
    //reset all!
    for (SeedFinderVec::iterator it=mSeedVec.begin(); it!=mSeedVec.end(); ++it) {
	(*it)->reset();
    }
    mCurrent=mSeedVec.begin();
    
    mMessenger <<"\t leaving StiCompositeSeedFinder::reset()"<<endl;
    
    return;
}

void StiCompositeSeedFinder::build()
{
    cout <<"\nStiCompositeSeedFinder::build()"<<endl;

    //Build each SeedFinder
    
    StiTrackSeedFinder* sf=0;
    if (StiToolkit::instance()->getIOBroker()->useGui()==true) {
	sf = new StiRDLocalTrackSeedFinder( StiToolkit::instance()->getDetectorContainer(),
					    mHitStore);
    }
    else {
	sf = new StiLocalTrackSeedFinder( StiToolkit::instance()->getDetectorContainer(),
					  mHitStore);
    }
    
    if (!mFactory) {
	cout <<"StiCompositeSeedFinder::buidl(). ERROR:\t"
	     <<"factory is null!"<<endl;
    }
    
    sf->setFactory(mFactory);

    //Now add detectors to the container
    const StiIOBroker* broker = StiToolkit::instance()->getIOBroker();
    
    const vector<unsigned int>& thePadrows = broker->ltsfPadrows();
    const vector<unsigned int>& theSectors = broker->ltsfSectors();
    
    for (vector<unsigned int>::const_iterator padrow=thePadrows.begin(); padrow!=thePadrows.end(); ++padrow) {
	for (vector<unsigned int>::const_iterator sector=theSectors.begin(); sector!=theSectors.end();
	     ++sector) {
	    char szBuf[100];
	    sprintf(szBuf, "Tpc/Padrow_%d/Sector_%d",
		    static_cast<int>(*padrow), static_cast<int>(*sector));
	    StiDetector* layer = StiToolkit::instance()->getDetectorFinder()->findDetector(szBuf);
	    if (!layer) {
		cout <<"gTrackSeedFinderBuilder(). ERROR:\t";
		cout <<"No layer: "<<szBuf<<endl;
	    }
	    else {
		sf->addLayer(layer);
	    }
	}
    }
    
    //sf->print();
    mSeedVec.push_back(sf);
    
    return;
}


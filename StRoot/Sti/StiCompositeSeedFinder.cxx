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

//Sti
#include "StiHitContainer.h"
#include "StiDetectorContainer.h"
#include "StiTrackSeedFinder.h"
#include "StiLocalTrackSeedFinder.h"
#include "StiDetector.h"
#include "StiDetectorFinder.h"
#include "StiCompositeSeedFinder.h"
#include "MessageType.h"

StiTrackSeedFinder* gTrackSeedFinderBuilder(const string& buildPath);

StiCompositeSeedFinder::StiCompositeSeedFinder()
{
    mMessenger <<"StiCompositeSeedFinder::StiCompositeSeedFinder()"<<endl;
}

StiCompositeSeedFinder::~StiCompositeSeedFinder()
{
    mMessenger <<"StiCompositeSeedFinder::~StiCompositeSeedFinder()"<<endl;

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
    mMessenger <<"\nStiCompositeSeedFinder::build()"<<endl;
    mMessenger <<"BuildFrom:\t"<<mBuildPath<<endl;

    mMessenger <<"Get vector size"<<endl;
    unsigned int theSize = 0;
    StGetConfigValue(mBuildPath.c_str(), "theStringVecSize", theSize);
    mMessenger <<"Read Size, decide"<<endl;

    if (theSize==0) {
	mMessenger <<"StiCompositeSeedFinder::build(). ERROR:\t";
	mMessenger <<"theStringVecSize==0  Abort"<<endl;
	return;
    }
    mMessenger <<"vector size:\t"<<theSize<<endl;

    mMessenger <<"Get the vector"<<endl;
    vector<string> theStringVec(theSize);
    StGetConfigValue(mBuildPath.c_str(), "theStringVec", theStringVec, theSize);
    if (theStringVec.empty()) {
	mMessenger <<"StiCompositeSeedFinder::build(). ERROR:\t";
	mMessenger <<"theStringVec.empty()==true.  Abort"<<endl;
	return;
    }
    mMessenger <<"Got vector"<<endl;
    
    //Build each SeedFinder
    for (vector<string>::iterator it=theStringVec.begin(); it!=theStringVec.end(); ++it) {
	StiTrackSeedFinder* sf = gTrackSeedFinderBuilder(*it);
	if (sf) {
	    mSeedVec.push_back(sf);
	}
    }
    
    return;
}

// non-members

StiTrackSeedFinder* gTrackSeedFinderBuilder(const string& buildPath)
{
    StiTrackSeedFinder* sf=0;

    Messenger& mMessenger = *(Messenger::instance(MessageType::kSeedFinderMessage));
    
    mMessenger <<" gTrackSeedFinderBuilder().  Build from:\t"<<buildPath<<endl;

    //Get hit filter type
    string filterType="empty";
    StGetConfigValue(buildPath.c_str(), "filterType", filterType);

    Sti2HitComboFilter* filt=0;

    if (filterType=="empty") {
	mMessenger <<"gTrackSeedFinderBuilder() ERROR:\t";
	mMessenger <<"filterType==empty.  Abort"<<endl;
	return 0;
    }
    else if (filterType=="StiRectangular2HitComboFilter") {
	filt = new StiRectangular2HitComboFilter;
	filt->build( buildPath );
    }
    else {
	mMessenger <<"gTrackSeedFinderBuilder() ERROR:\t";
	mMessenger <<"Unknown filter type.  Abort"<<endl;
    }

    //Get Seed-finder type
    string seedFinderType = "empty";
    StGetConfigValue(buildPath.c_str(), "seedFinderType", seedFinderType);
    
    if (seedFinderType=="empty") {
	mMessenger <<"gTrackSeedFinderBuilder() ERROR:\t";
	mMessenger <<"seedFinderType==empty.  Abort"<<endl;
	return 0;
    }
    else if (seedFinderType=="StiLocalTrackSeedFinder") {
	sf = new StiLocalTrackSeedFinder(StiDetectorContainer::instance(),
					 StiHitContainer::instance(), filt);
    }
    else {
	mMessenger <<"gTrackSeedFinderBuilder() ERROR:\t";
	mMessenger <<"Unkown seed finder type.  Abort"<<endl;
	return 0;
    }

    //Now read in the padrows and the sectors
    int nPadrows=0;
    StGetConfigValue(buildPath.c_str(), "nPadrows", nPadrows);

    if (nPadrows==0) {
	mMessenger <<"gTrackSeedFinderBuilder(). ERROR:\t";
	mMessenger <<"nPadrows=0.  Abort"<<endl;
	return 0;
    }
    vector<unsigned int> thePadrows(nPadrows);
    StGetConfigValue(buildPath.c_str(), "thePadrows", thePadrows, nPadrows);
    mMessenger <<"Using Padrows: ";
    copy(thePadrows.begin(), thePadrows.end(), ostream_iterator<unsigned int>(mMessenger, " "));
    mMessenger <<endl;
    
    int nSectors=0;
    StGetConfigValue(buildPath.c_str(), "nSectors",nSectors);
    if (nSectors==0) {
	mMessenger <<"gTrackSeedFinderBuilder(). ERROR:\t";
	mMessenger <<"nSectors=0.  Abort"<<endl;
	return 0;
    }
    vector<unsigned int> theSectors(nSectors);
    StGetConfigValue(buildPath.c_str(), "theSectors", theSectors, nSectors);
    mMessenger <<"Using Sectors: ";
    copy(theSectors.begin(), theSectors.end(), ostream_iterator<unsigned int>(mMessenger, " "));
    mMessenger <<endl;

    sf->setBuildPath(buildPath);
    sf->build();

    //Now add detectors to the container 
    for (vector<unsigned int>::iterator padrow=thePadrows.begin(); padrow!=thePadrows.end(); ++padrow) {
	for (vector<unsigned int>::iterator sector=theSectors.begin(); sector!=theSectors.end();
	     ++sector) {
	    char szBuf[100];
	    sprintf(szBuf, "Tpc/Padrow_%d/Sector_%d", *padrow, *sector);
	    StiDetector* layer = StiDetectorFinder::instance()->findDetector(szBuf);
	    if (!layer) {
		mMessenger <<"gTrackSeedFinderBuilder(). ERROR:\t";
		mMessenger <<"No layer: "<<szBuf<<endl;
	    }
	    else {
		sf->addLayer(layer);
	    }
	}
    }
    sf->print();
    
    return sf;
}


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

StiTrackSeedFinder* gTrackSeedFinderBuilder(const string& buildPath);

StiCompositeSeedFinder::StiCompositeSeedFinder()
{
    cout <<"StiCompositeSeedFinder::StiCompositeSeedFinder()"<<endl;
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
    cout <<"StiCompositeSeedFinder::hasMore()"<<endl;
    bool val =  mCurrent>=mSeedVec.begin() && mCurrent<mSeedVec.end()
	&& (*mCurrent)->hasMore();
    cout <<"\t  returning "<<val<<endl;
    return val;
}

StiKalmanTrack* StiCompositeSeedFinder::next()
{
    cout <<"StiCompositeSeedFinder::next()"<<endl;
    StiKalmanTrack* track=0;
    //while ((*mCurrent)->hasMore() && track==0) {
    track = (*mCurrent)->next();
    //}
    
    //Check to see if we ran out
    if ( (*mCurrent)->hasMore()==false ) {
	++mCurrent;
    }
     
    cout <<"\t leaving StiCompositeSeedFinder::next()"<<endl;
    return track;
}

void StiCompositeSeedFinder::reset()
{
    cout <<"StiCompositeSeedFinder::reset()"<<endl;
    //reset all!
    for (SeedFinderVec::iterator it=mSeedVec.begin(); it!=mSeedVec.end(); ++it) {
	(*it)->reset();
    }
    mCurrent=mSeedVec.begin();
    cout <<"\t leaving StiCompositeSeedFinder::reset()"<<endl;
    
    return;
}

void StiCompositeSeedFinder::build()
{
    cout <<"\nStiCompositeSeedFinder::build()"<<endl;
    cout <<"BuildFrom:\t"<<mBuildPath<<endl;

    cout <<"Get vector size"<<endl;
    unsigned int theSize = 0;
    StGetConfigValue(mBuildPath.c_str(), "theStringVecSize", theSize);
    cout <<"Read Size, decide"<<endl;

    if (theSize==0) {
	cout <<"StiCompositeSeedFinder::build(). ERROR:\t";
	cout <<"theStringVecSize==0  Abort"<<endl;
	return;
    }
    cout <<"vector size:\t"<<theSize<<endl;

    cout <<"Get the vector"<<endl;
    vector<string> theStringVec(theSize);
    StGetConfigValue(mBuildPath.c_str(), "theStringVec", theStringVec, theSize);
    if (theStringVec.empty()) {
	cout <<"StiCompositeSeedFinder::build(). ERROR:\t";
	cout <<"theStringVec.empty()==true.  Abort"<<endl;
	return;
    }
    cout <<"Got vector"<<endl;
    
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

    cout <<" gTrackSeedFinderBuilder().  Build from:\t"<<buildPath<<endl;

    //Get hit filter type
    string filterType="empty";
    StGetConfigValue(buildPath.c_str(), "filterType", filterType);

    Sti2HitComboFilter* filt=0;

    if (filterType=="empty") {
	cout <<"gTrackSeedFinderBuilder() ERROR:\t";
	cout <<"filterType==empty.  Abort"<<endl;
	return 0;
    }
    else if (filterType=="StiRectangular2HitComboFilter") {
	filt = new StiRectangular2HitComboFilter;
	filt->build( buildPath );
    }
    else {
	cout <<"gTrackSeedFinderBuilder() ERROR:\t";
	cout <<"Unknown filter type.  Abort"<<endl;
    }

    //Get Seed-finder type
    string seedFinderType = "empty";
    StGetConfigValue(buildPath.c_str(), "seedFinderType", seedFinderType);
    
    if (seedFinderType=="empty") {
	cout <<"gTrackSeedFinderBuilder() ERROR:\t";
	cout <<"seedFinderType==empty.  Abort"<<endl;
	return 0;
    }
    else if (seedFinderType=="StiLocalTrackSeedFinder") {
	sf = new StiLocalTrackSeedFinder(StiDetectorContainer::instance(),
					 StiHitContainer::instance(), filt);
    }
    else {
	cout <<"gTrackSeedFinderBuilder() ERROR:\t";
	cout <<"Unkown seed finder type.  Abort"<<endl;
	return 0;
    }

    //Now read in the padrows and the sectors
    int nPadrows=0;
    StGetConfigValue(buildPath.c_str(), "nPadrows", nPadrows);

    if (nPadrows==0) {
	cout <<"gTrackSeedFinderBuilder(). ERROR:\t";
	cout <<"nPadrows=0.  Abort"<<endl;
	return 0;
    }
    vector<unsigned int> thePadrows(nPadrows);
    StGetConfigValue(buildPath.c_str(), "thePadrows", thePadrows, nPadrows);
    cout <<"Using Padrows: ";
    copy(thePadrows.begin(), thePadrows.end(), ostream_iterator<unsigned int>(cout, " "));
    cout <<endl;
    
    int nSectors=0;
    StGetConfigValue(buildPath.c_str(), "nSectors",nSectors);
    if (nSectors==0) {
	cout <<"gTrackSeedFinderBuilder(). ERROR:\t";
	cout <<"nSectors=0.  Abort"<<endl;
	return 0;
    }
    vector<unsigned int> theSectors(nSectors);
    StGetConfigValue(buildPath.c_str(), "theSectors", theSectors, nSectors);
    cout <<"Using Sectors: ";
    copy(theSectors.begin(), theSectors.end(), ostream_iterator<unsigned int>(cout, " "));
    cout <<endl;

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
		cout <<"gTrackSeedFinderBuilder(). ERROR:\t";
		cout <<"No layer: "<<szBuf<<endl;
	    }
	    else {
		sf->addLayer(layer);
	    }
	}
    }
    sf->print();
    
    return sf;
}


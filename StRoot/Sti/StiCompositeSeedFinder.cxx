//StiCompositeSeedFinder.cxx
//M.L. Miller (Yale Software)
//8/01

//STD
#include <iostream.h>
#include <stdio.h>

//SCL
#include "StGetConfigValue.hh"

//Sti
#include "StiObjectFactoryInterface.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiHitContainer.h"
#include "StiDetectorContainer.h"
#include "StiDetectorFinder.h"
#include "StiTrackSeedFinder.h"

#include "StiCompositeSeedFinder.h"

ostream& operator<<(ostream&, const StiDetector&);

//Nested class StiSeedFinderRep
StiCompositeSeedFinder::StiSeedFinderRep::StiSeedFinderRep(const string& path,
							   StiObjectFactoryInterface<StiKalmanTrack>* fac)
    : mBuildPath(path), mTrackFactory(fac), mSeedFinder(0), mHitComboFilter(0)
{
    //cout <<"\nStiSeedFidnerRep::StiSeedFinderRep()"<<endl;
    build();
    reset();
}

StiCompositeSeedFinder::StiSeedFinderRep::~StiSeedFinderRep()
{
    //cout <<"\nStiSeedFidnerRep::~StiSeedFinderRep()"<<endl;
    delete mSeedFinder;
    mSeedFinder=0;
    
    delete mHitComboFilter;
    mHitComboFilter=0;
}

StiCompositeSeedFinder::StiSeedFinderRep::StiSeedFinderRep(const StiSeedFinderRep& rhs)
    : mBuildPath("empty"), mTrackFactory(0), mSeedFinder(0), mHitComboFilter(0)
{
    //cout <<"StiSeedFinderRep::StiSeedFinderRep(const StiSeedFinderRep&)"<<endl;
    copyToThis(rhs);
}

StiCompositeSeedFinder::StiSeedFinderRep& //return type
StiCompositeSeedFinder::StiSeedFinderRep::operator=(const StiSeedFinderRep& rhs)
{
    //cout <<"StiSeedFinderRep::opeartor=(const StiSeedFinderRep&)"<<endl;
    if (this == &rhs) return *this;
    copyToThis(rhs);
    return *this;
}

void StiCompositeSeedFinder::StiSeedFinderRep::copyToThis(const StiSeedFinderRep& rhs)
{
    //cout <<"StiSeedFinderRep::copyToThis()"<<endl;
    
    //shallow copies
    mBuildPath = rhs.mBuildPath;
    mTrackFactory = rhs.mTrackFactory;
    mIntDetMap = rhs.mIntDetMap;
    mCurrentStartPoint = rhs.mCurrentStartPoint;

    //deep copy
    if (mHitComboFilter) {
	cout <<"StiSeedFinderRep::copyToThis(). Delete mHitComboFilter"<<endl;
	delete mHitComboFilter;
	mHitComboFilter=0;
    }
    
    if (mSeedFinder) {
	cout <<"StiSeedFinderRep::copyToThis(). Delete mSeedFinder"<<endl;
	delete mSeedFinder;
	mSeedFinder=0;
    }

    //Must copy hit filter before seed finder    
    if (StiRectangular2HitComboFilter* tempCF
	= dynamic_cast<StiRectangular2HitComboFilter*>(rhs.mHitComboFilter) ) {
        mHitComboFilter = new StiRectangular2HitComboFilter(*tempCF);
    }
    else if (StiCollinear2HitComboFilter* tempCF
	     = dynamic_cast<StiCollinear2HitComboFilter*>(rhs.mHitComboFilter) ) {
	mHitComboFilter = new StiCollinear2HitComboFilter(*tempCF);
    }    
    else {
	cout <<"StiSeedFinderRep::copyToThis(). ERROR:\tunknown hit filter type"<<endl;
    }

    const StiTrackSeedFinder& temp = *(rhs.mSeedFinder);
    mSeedFinder = new StiTrackSeedFinder(temp);
    //must reset hit filter (not taken care of by seedFinder copy
    mSeedFinder->setHitComboFilter(mHitComboFilter);

    reset();
}

void StiCompositeSeedFinder::StiSeedFinderRep::build()
{
    //cout <<"\nStiCompositeSeedFinder::StiSeedFinderRep::build()"<<endl;
    
    //find which type of filter to build
    string filterType = "empty";
    StGetConfigValue(mBuildPath.c_str(), "filterType", filterType);

    if (filterType=="StiRectangular2HitComboFilter") {
	mHitComboFilter = new StiRectangular2HitComboFilter();
	mHitComboFilter->build(mBuildPath);
    }
    else if (filterType == "StiCollinear2HitComboFilter") {
	mHitComboFilter = new StiCollinear2HitComboFilter();
	mHitComboFilter->build(mBuildPath);
    }
    else {
	cout <<"StiSeedFinderRep::build(). ERROR:\tHitFilter not initialized.  Abort"<<endl;
	return;
    }

    //Get SeedFinder type
    string seedFinderType = "empty";
    StGetConfigValue(mBuildPath.c_str(), "seedFinderType", seedFinderType);

    if (seedFinderType == "StiTrackSeedFinder") {
	mSeedFinder = new StiTrackSeedFinder( StiHitContainer::instance(), mHitComboFilter );
	mSeedFinder->setFactory(mTrackFactory);
    }
    else {
	cout <<"StiSeedFinderRep::build(). ERROR:\tSeedFinderType not initialized. Abort"<<endl;
	return;
    }

    vector<int> mSectors;
    vector<int> mPadrows;
    
    int nSectors=0;
    StGetConfigValue(mBuildPath.c_str(), "nSectors", nSectors);

    mSectors.resize(nSectors);
    StGetConfigValue(mBuildPath.c_str(), "theSectors", mSectors, nSectors);

    int nPadrows=0;
    StGetConfigValue(mBuildPath.c_str(), "nPadrows", nPadrows);
    
    mPadrows.resize(nPadrows);
    StGetConfigValue(mBuildPath.c_str(), "thePadrows", mPadrows, nPadrows);

    if (mSectors.empty() || mPadrows.empty()) {
	cout <<"StiCompositeSeedFinder::StiSeedFinderRep::build() ERROR.  Not initialized.  Undefined behaviour!"<<endl;
	return;
    }

    //Now add detectors to the container
    for (vector<int>::const_iterator sector=mSectors.begin(); sector!=mSectors.end(); ++sector) {
	StiDetectorVec_t temp;
	for (vector<int>::const_iterator padrow=mPadrows.begin(); padrow!=mPadrows.end(); ++padrow) {
	    char szBuf[100];
	    sprintf(szBuf, "Tpc/Padrow_%d/Sector_%d", *padrow, *sector);
	    StiDetector* layer = StiDetectorFinder::instance()->findDetector(szBuf);
	    if (!layer) {
		cout <<"Error:\tStiCompositeSeedFinder::build()\tNo layer: "<<szBuf<<endl;
	    }
	    else {
		temp.push_back(layer);
	    }
	}
	mIntDetMap.push_back(temp);
    }
}

void StiCompositeSeedFinder::StiSeedFinderRep::reset()
{
    mCurrentStartPoint = mIntDetMap.begin();
    return;
}

bool StiCompositeSeedFinder::StiSeedFinderRep::hasMoreStartPoints() const
{
    return (mCurrentStartPoint>=mIntDetMap.begin()) && (mCurrentStartPoint<mIntDetMap.end());
}

StiSeedFinder* StiCompositeSeedFinder::StiSeedFinderRep::seedFinder()
{
    //cout <<"StiCompositeSeedFinder::StiSeedFinderRep::seedFinder()"<<endl;
    //init for current start point
    init();
    ++mCurrentStartPoint;
    
    return mSeedFinder;
}

void StiCompositeSeedFinder::StiSeedFinderRep::init()
{
    //cout <<"StiCompositeSeedFinder::StiSeedFinderRep::init()"<<endl;
    //Clear
    mSeedFinder->clear();
    StiDetector* layer = 0;
    //Add pointer to hits
    for (StiDetectorVec_t::iterator it=(*mCurrentStartPoint).begin(); it!=(*mCurrentStartPoint).end(); ++it) {
	layer = (*it);
	cout <<"\tAdding layer: "<<*layer<<endl;
	mSeedFinder->addLayer( layer->getPlacement()->getCenterRefAngle(),
			       layer->getPlacement()->getCenterRadius() );	
    }
    //Set DetectorContainer to to last layer (most outside)
    if (layer!=0) {
	StiDetectorContainer::instance()->setToDetector( layer );
    }
    else {
	cout <<"StiSeedFinderRep::init()\tERROR: last layer is null.  "<<endl;
	cout <<"cannot initialize Detector Container. Undefined behavior."<<endl;
    }
    return;
}

StiCompositeSeedFinder::StiCompositeSeedFinder() : mCurrentSeedFinder(0) , mBuildPath("empty")
{
    //cout <<"StiCompositeSeedFinder::StiCompositeSeedFinder()"<<endl;
}

StiCompositeSeedFinder::~StiCompositeSeedFinder()
{
    //cout <<"StiCompositeSeedFinder::~StiCompositeSeedFinder()"<<endl;
}

bool StiCompositeSeedFinder::hasMore()
{
    return (mCurrentRep<mRepVec.end());
}

void StiCompositeSeedFinder::reset()
{
    for (RepVecIterator_t it=mRepVec.begin(); it!=mRepVec.end(); ++it) {
	(*it).reset();
    }
    mCurrentRep = mRepVec.begin();
    mCurrentSeedFinder = (*mCurrentRep).seedFinder(); //Set to first start point of first rep
}

StiKalmanTrack* StiCompositeSeedFinder::next()
{
    StiKalmanTrack* track =0;
    cout <<"StiCompositeSeedFinder::next()"<<endl;
    if (mCurrentSeedFinder->hasMore()) { //Get next track
	//cout <<"\tcurrent has more!"<<endl;
	track = mCurrentSeedFinder->next();
    }
    else {
	//Are we done?
	if (incrementRep()) { //some seeds to be served
	    track = mCurrentSeedFinder->next();
	}
	else {//Nowhere to go
	    //Increment to next seed-finder-rep
	    ++mCurrentRep;
	    if (mCurrentRep<mRepVec.end()) {
		mCurrentSeedFinder = (*mCurrentRep).seedFinder();
	    }
	    else {
		//cout <<"StiCompositeSeedFinder::next():\tOut of Reps"<<endl;
	    }
	}
    }
    return track;
}

bool StiCompositeSeedFinder::incrementRep()
{
    bool go=true;
    while ( (*mCurrentRep).hasMoreStartPoints() && go==true) {
	mCurrentSeedFinder = (*mCurrentRep).seedFinder();
	if (mCurrentSeedFinder->hasMore()) {//There's something to be had here
	    go=false;
	    return true;
	}
    }
    return false;
}

void StiCompositeSeedFinder::build()
{
    //cout <<"StiCompositeSeedFinder::build()"<<endl;

    if (mBuildPath == "empty") {
	cout <<"StiCompositeSeedFinder::build(). ERROR:\tbuildPath==empty.  Abort"<<endl;
	return;
    }

    if (!mTrackFactory) {
	cout <<"StiCompositeSeedFinder::build(). ERROR:\ttrackFactory==0.  Abort"<<endl;
	return;
    }
    
    //cout <<"\tBuild from:\t"<<mBuildPath<<endl;
    int theStringVecSize=0;
    StGetConfigValue(mBuildPath.c_str(), "theStringVecSize", theStringVecSize);
    //cout <<"\tNumber of SeedFinders to build:\t"<<theStringVecSize<<endl;
    
    vector<string> theStringVec(theStringVecSize);
    StGetConfigValue(mBuildPath.c_str(), "theStringVec", theStringVec, theStringVecSize);
    
    for (vector<string>::const_iterator it=theStringVec.begin(); it!=theStringVec.end(); ++it) {
	cout <<"\tadding S.F.Rep from:\t"<<*it<<endl;
	mRepVec.push_back( StiSeedFinderRep(*it, mTrackFactory) );
    }

    mCurrentRep = mRepVec.begin();
    reset();
    cout <<"\n\tStiSeedFinderRep Entries\n"<<endl;
    //copy(mRepVec.begin(), mRepVec.end(), ostream_iterator<StiCompositeSeedFinder::StiSeedFinderRep>(cout, "\n\n"));
    //cout <<endl;
}

ostream& operator<<(ostream& os, const StiCompositeSeedFinder::StiSeedFinderRep& rep)
{
    os <<"\tTrackFactory:\t"<<rep.mTrackFactory<<endl;
    os <<"\tSeedFinder:\t"<<rep.mSeedFinder<<endl;
    os <<"\tHitComboFilter:\t"<<rep.mHitComboFilter<<endl;
    os <<"\tSeedFinder::HitComboFilter:\t"<<rep.mSeedFinder->getHitComboFilter()<<endl;
    os <<"\tDetectors:"<<endl;
    for (vector< vector<StiDetector*> >::const_iterator it=rep.mIntDetMap.begin(); it!=rep.mIntDetMap.end(); ++it) {
	for (vector<StiDetector*>::const_iterator it2=(*it).begin(); it2!=(*it).end(); ++it2) {
	    cout <<"\t\t"<<**it2<<endl;
	}
    }
    return os;
}

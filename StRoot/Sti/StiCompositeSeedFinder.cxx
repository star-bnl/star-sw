//StiCompositeSeedFinder.cxx
//M.L. Miller (Yale Software)
//8/01

//STD
#include <iostream.h>
#include <stdio.h>

//SCL
#include "StGetConfigValue.hh"

//Sti
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiHitContainer.h"
#include "StiDetectorContainer.h"
#include "StiDetectorFinder.h"
#include "StiTrackSeedFinder.h"

#include "StiCompositeSeedFinder.h"

ostream& operator<<(ostream&, const StiDetector&);

//Nested class StiSeedFinderRep
StiCompositeSeedFinder::StiSeedFinderRep::StiSeedFinderRep(StiTrackSeedFinder* sf, const char* path)
    : mSeedFinder(sf), mBuildPath(path)
{
    mMinSector = mMaxSector = mMinPadrow = mMaxPadrow = kNotInitialized;
    build();
    reset();
}

void StiCompositeSeedFinder::StiSeedFinderRep::build()
{
    cout <<"StiCompositeSeedFinder::StiSeedFinderRep::build()"<<endl;
    cout <<"\tBuildPath:\t"<<mBuildPath<<endl;
    StGetConfigValue(mBuildPath, "MinPadrow", mMinPadrow);
    StGetConfigValue(mBuildPath, "MaxPadrow", mMaxPadrow);
    StGetConfigValue(mBuildPath, "MinSector", mMinSector);
    StGetConfigValue(mBuildPath, "MaxSector", mMaxSector);
    cout <<"\tMinPadrow: "<<mMinPadrow;
    cout <<"\tMaxPadrow: "<<mMaxPadrow;
    cout <<"\tMinSector: "<<mMinSector;
    cout <<"\tMaxSector: "<<mMaxSector<<endl;
    
    if (mMinPadrow==kNotInitialized || mMaxPadrow==kNotInitialized
	|| mMinSector==kNotInitialized || mMaxSector==kNotInitialized) {
	cout <<"StiCompositeSeedFinder::StiSeedFinderRep::build() ERROR.  Not initialized.  Undefined behaviour!"<<endl;
	return;
    }

    //Now add detectors to the container
    for (int sector=mMinSector; sector<=mMaxSector; ++sector) {
	StiDetectorVec_t temp;
	for (int padrow=mMinPadrow; padrow<=mMaxPadrow; ++padrow) {
	    cout <<"Building sector: "<<sector<<"\tpadrow: "<<padrow<<endl;
	    char szBuf[100];
	    sprintf(szBuf, "Tpc/Padrow_%d/Sector_%d", padrow, sector);
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

    //check
    /*
      cout <<"Show start points"<<endl;
      for (IntDetectorMapIterator_t st=mIntDetMap.begin(); st!=mIntDetMap.end(); ++st) {
      cout <<"Next Start Points:"<<endl;
      for (StiDetectorVec_t::const_iterator it=(*st).begin(); it!=(*st).end(); ++it) {
      cout <<"\t"<<**it<<endl;
      }
      }
    */
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
    cout <<"StiCompositeSeedFinder::StiSeedFinderRep::seedFinder()"<<endl;
    //init for current start point
    init();
    ++mCurrentStartPoint;
    
    return mSeedFinder;
}

void StiCompositeSeedFinder::StiSeedFinderRep::init()
{
    cout <<"StiCompositeSeedFinder::StiSeedFinderRep::init()"<<endl;
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

StiCompositeSeedFinder::StiCompositeSeedFinder() : mCurrentSeedFinder(0)
{
    cout <<"StiCompositeSeedFinder::StiCompositeSeedFinder()"<<endl;
}

StiCompositeSeedFinder::~StiCompositeSeedFinder()
{
    cout <<"StiCompositeSeedFinder::~StiCompositeSeedFinder()"<<endl;
}

bool StiCompositeSeedFinder::hasMore()
{
    //return ( (mCurrentSeedFinder->hasMore() || (*mCurrentRep).hasMoreStartPoints())
    //    && (mCurrentRep!=mRepVec.end() );
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
	cout <<"\tcurrent has more!"<<endl;
	track = mCurrentSeedFinder->next();
    }
    else {
	//Are we done?
	cout <<"\tcurrent has no more"<<endl;
	if (incrementRep()) { //some seeds to be served
	    track = mCurrentSeedFinder->next();
	}
	else {//Nowhere to go
	    cout <<"StiCompositeSeedFinder::next():\tOut of start points for this rep"<<endl;
	    //Increment to next seed-finder-rep
	    ++mCurrentRep;
	    if (mCurrentRep<mRepVec.end()) {
		mCurrentSeedFinder = (*mCurrentRep).seedFinder();
	    }
	    else {
		cout <<"StiCompositeSeedFinder::next():\tOut of Reps"<<endl;
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

void StiCompositeSeedFinder::buildOuterSeedFinder(StiTrackSeedFinder* sf)
{
    const char* path = "StRoot/StiMaker/RunTimeParameters/OuterSeedFinder.txt";
    mRepVec.push_back( StiSeedFinderRep(sf, path) );
    mCurrentRep = mRepVec.begin();
}

void StiCompositeSeedFinder::buildInnerSeedFinder(StiTrackSeedFinder* sf)
{
    const char* path = "StRoot/StiMaker/RunTimeParameters/InnerSeedFinder.txt";
    mRepVec.push_back( StiSeedFinderRep(sf, path) );
    mCurrentRep = mRepVec.begin();
}

//StiTrackFinder.cxx
//C.Pruneau (Wayne State U)
//05/01

#include <iostream.h>
#include <stdlib.h>

//StDbUtilities
#include "StDbUtilities/StMagUtilities.h"

//Sti
#include "StiSeedFinder.h"
#include "StiTrackFilter.h"
#include "StiDetectorContainer.h"
#include "StiHitContainer.h"
#include "StiTrackContainer.h"

#include "StiTrackFinder.h"

StiTrackFinder::StiTrackFinder()
    : trackFilter(0), trackSeedFinder(0), trackNodeFactory(0), magField(0),
      detectorContainer(0), hitContainer(0), trackContainer(0)
{
    cout <<"StiTrackFinder::StiTrackFinder()"<<endl;

    //Perform safe sets of tracking essentials

    //Track Filter
    setTrackFilter( new StiTrackFilter() );

    //Detector Container
    setDetectorContainer( StiDetectorContainer::instance() );

    //TrackContainer
    setTrackContainer( StiTrackContainer::instance() );
    
}

StiTrackFinder::~StiTrackFinder()
{
    cout <<"StiTrackFinder::~StiTrackFinder()"<<endl;

    delete trackFilter;
    trackFilter=0;
}

bool StiTrackFinder::isValid(bool debug) const
{
    bool val = trackSeedFinder && trackFilter && trackNodeFactory  && detectorContainer &&
	hitContainer && trackContainer; //
	//&& magField;
    
    if (debug) {
	cout <<"\nStiTrackFinder::isValid()\n"<<endl;
	cout <<"trackSeedFinder:\t"<<trackSeedFinder<<endl;
	cout <<"trackFilter:\t\t"<<trackFilter<<endl;
	cout <<"trackNodeFactory:\t"<<trackNodeFactory<<endl;
	cout <<"magField:\t\t"<<magField<<endl;
	cout <<"detectorContainer:\t"<<detectorContainer<<endl;
	cout <<"hitContainer:\t\t"<<hitContainer<<endl;
	cout <<"trackContainer:\t\t"<<trackContainer<<endl;
	cout <<"Returning:\t\t"<< val<< endl;
    }
    return val;
}

//Set Owned Objects-------

void StiTrackFinder::setTrackFilter(StiTrackFilter * filter)
{
  //----------------------------------------------------------------- 
  // Set the seedFilter to be used by this track finder to the given
  // value. The current filter, if any, is destroyed.
  // Nothing is done if the given filter is same as that currently used
  // by this track finder.
  //----------------------------------------------------------------- 
  
  if (filter==0)
    {
      cout << "StiTrackFilter::setTrackFilter() - ERROR - " << endl
	   << "  The given track filter is NULL." << endl
	   << "  A valid, non null value, MUST be supplied" << endl;
      
      return;
    }
  // check whether current filter equals given filter, if so exit
  if (trackFilter==filter)
    {
      cout << "StiTrackFilter::setTrackFilter() - WARNING - " << endl
	   << "  The given track filter is is identical to the one" << endl
	   << "  currently in use. No change will be made" << endl;
      return;
    }

  // check whether a filter currently exist, if so destroy it.
  if (trackFilter!=0)
    delete trackFilter;
 
  // set filter used by this track finder to given value
  trackFilter = filter;  
}

// Set Non-Owned objects ---------
void StiTrackFinder::setTrackSeedFinder(StiSeedFinder * finder)
{
    if (finder==0)    {
	cout << "StiTrackFinder::setTrackSeedFinder() - ERROR - " << endl
	     << "  The given Track Seed Finder is NULL" << endl
	     << "  A valid, non null value, MUST be supplied" << endl;
	return;
    }

    else {
	trackSeedFinder = finder;
    }
}

void StiTrackFinder::setTrackNodeFactory(StiObjectFactoryInterface<StiKalmanTrackNode> * factory)
{
    if (factory!=0) {
	trackNodeFactory = factory;
    }
    else {
	cout <<"StiTrackFinder::setTrackNodeFactory():\tERROR! factory==0"<<endl;
    }
}

void StiTrackFinder::setMagneticField(StMagUtilities * magFieldDesc)
{
    if(magFieldDesc==0)	{
	cout << "StiTrackFinder::setMagneticField() - ERROR - " << endl
	     << "  The given magnetic field descriptor is NULL." << endl
	     << "  A valid, non null value, MUST be supplied" << endl;
	return;
    }
    else {
	magField = magFieldDesc;
    }
}

void StiTrackFinder::setDetectorContainer(StiDetectorContainer* newDetContainer)
{
    if (newDetContainer==0)	{
	cout << "StiTrackFinder::setDetectorContainer() - ERROR - " << endl
	     << "  The detector container is NULL." << endl
	     << "  A valid, non null value, MUST be supplied" << endl;
	return;
    }

    else {
	detectorContainer = newDetContainer;
    }
}

void StiTrackFinder::setHitContainer(StiHitContainer * newHitContainer)
{
    if (newHitContainer==0) {
	cout << "StiTrackFinder::setHitContainer() - ERROR - " << endl
	     << "  The given hit container is NULL." << endl
	     << "  A valid, non null value, MUST be supplied" << endl;
	return;
    }

    else {
	hitContainer = newHitContainer;
    }
}

void StiTrackFinder::setTrackContainer(StiTrackContainer * newTrackContainer)
{
    if (newTrackContainer==0)    {
	cout << "StiTrackFinder::setTrackContainer() - ERROR - " << endl
	     << "  The given track container is NULL." << endl
	     << "  A valid, non null value, MUST be supplied" << endl;
	return;
    }
    else {
	trackContainer = newTrackContainer;
    }
}


//StiTrackFinder.cxx
//C.Pruneau (Wayne State U)
//05/01

#include <iostream.h>
#include <stdlib.h>

//StDbUtilities
#include "StDbUtilities/StMagUtilities.h"

//Sti
#include "StiToolkit.h"
#include "StiSeedFinder.h"
#include "StiTrackFilter.h"
#include "StiDynamicTrackFilter.h"
#include "StiDetectorContainer.h"
#include "StiHitContainer.h"
#include "StiTrackContainer.h"

#include "StiTrackFinder.h"

StiTrackFinder::StiTrackFinder(StiToolkit * userToolkit) :
	trackFilter(0), 
	trackFitter(0),
	trackSeedFinder(0), 
	trackNodeFactory(0), 
	//magField(0),
	detectorContainer(0), 
	hitContainer(0), 
	trackContainer(0)
{
	cout <<"StiTrackFinder::StiTrackFinder()"<<endl;
	
	// none of the following instances are owned by this class.
	toolkit = userToolkit;
	trackFilter = toolkit->getTrackFilter();
	trackFitter = toolkit->getTrackFitter();
	trackSeedFinder = toolkit->getTrackSeedFinder();
	trackNodeFactory = toolkit->getTrackNodeFactory();
	detectorContainer = toolkit->getDetectorContainer();
	hitContainer = toolkit->getHitContainer();
	trackContainer = toolkit->getTrackContainer();
}

StiTrackFinder::~StiTrackFinder()
{
	cout <<"StiTrackFinder::~StiTrackFinder()"<<endl;
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
	//cout <<"magField:\t\t"<<magField<<endl;
	cout <<"detectorContainer:\t"<<detectorContainer<<endl;
	cout <<"hitContainer:\t\t"<<hitContainer<<endl;
	cout <<"trackContainer:\t\t"<<trackContainer<<endl;
	cout <<"Returning:\t\t"<< val<< endl;
    }
    return val;
}

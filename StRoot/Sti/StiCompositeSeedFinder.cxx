//StiCompositeSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01
#include <stdexcept>
#include <stdio.h>
#include <iostream.h>
#include <algorithm>
using std::ostream_iterator;
using std::copy;

//Scl
//#include "StGetConfigValue.hh"
#include "StMeasuredPoint.h"
#include "Sti/Base/MessageType.h"
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Factory.h"
#include "StiToolkit.h"
#include "StiIOBroker.h"
#include "StiHitContainer.h"
#include "StiDetectorContainer.h"
#include "StiTrackSeedFinder.h"
#include "StiLocalTrackSeedFinder.h"
#include "StiDetector.h"
#include "StiDetectorFinder.h"
#include "StiCompositeSeedFinder.h"
#include "StiGui/StiRDLocalTrackSeedFinder.h"
#include "StiMasterDetectorBuilder.h"

StiCompositeSeedFinder::StiCompositeSeedFinder(const string&             name,
					       Factory<StiKalmanTrack> * trackFactory,
					       StiHitContainer         * hitContainer,
					       StiDetectorContainer    * detectorContainer)
  : StiSeedFinder(name,trackFactory,hitContainer,detectorContainer)
{
  build();
}

StiCompositeSeedFinder::~StiCompositeSeedFinder()
{
  cout <<"StiCompositeSeedFinder::~StiCompositeSeedFinder() - INFO - Started"<<endl;
  //Destroy seed finders
  for (SeedFinderVec::iterator it=_trackSeedFinders.begin(); it!=_trackSeedFinders.end(); ++it) 
    delete *it;
}

bool StiCompositeSeedFinder::hasMore()
{
  _messenger <<"StiCompositeSeedFinder::hasMore() - INFO - Started"<<endl;
  bool has_more;
  if (_currentTrackSeedFinder != _trackSeedFinders.end())
    has_more = (*_currentTrackSeedFinder)->hasMore();
  else
    has_more = false;
  return has_more;
}

StiKalmanTrack* StiCompositeSeedFinder::next()
{
  _messenger <<"StiCompositeSeedFinder::next() - INFO - Starting"<<endl;
  
  StiKalmanTrack* track=0;
  track = (*_currentTrackSeedFinder)->next();
  //Check to see if we ran out
  if ( (*_currentTrackSeedFinder)->hasMore()==false ) ++_currentTrackSeedFinder;
  _messenger <<"StiCompositeSeedFinder::next() - INFO - Done"<<endl;
  return track;
}

void StiCompositeSeedFinder::reset()
{
  _messenger <<"StiCompositeSeedFinder::reset() - INFO - Started"<<endl;
  //reset all!
  for (SeedFinderVec::iterator it=_trackSeedFinders.begin(); it!=_trackSeedFinders.end(); ++it)
    (*it)->reset();
  _currentTrackSeedFinder=_trackSeedFinders.begin();
  _messenger <<"StiCompositeSeedFinder::reset() - INFO - Done"<<endl;
}

void StiCompositeSeedFinder::build()
{
  _messenger<<"StiCompositeSeedFinder::build() - INFO - Started"<<endl;
  //Build each SeedFinder
  StiTrackSeedFinder* trackSeedFinder=0;
  if (StiToolkit::instance()->getIOBroker()->useGui()==true)
    trackSeedFinder = new StiRDLocalTrackSeedFinder("RDLocalTrackSeedFinder",
						    _trackFactory,
						    _hitContainer,
						    _detectorContainer);
  else 
    trackSeedFinder = new StiLocalTrackSeedFinder("LocalTrackSeedFinder",
						  _trackFactory,
						  _hitContainer,
						  _detectorContainer);
  //Now add detectors to the container
  //const StiIOBroker* broker = StiToolkit::instance()->getIOBroker();
  //const vector<unsigned int>& thePadrows = broker->ltsfPadrows();
  //const vector<unsigned int>& theSectors = broker->ltsfSectors();
  //char szBuf[100];

  // This will put all known rows in the search loop
  StiMasterDetectorBuilder * builder = StiToolkit::instance()->getDetectorBuilder();
  if (!builder)
    throw runtime_error("StiCompositeSeedFinder::build() - FATAL - builder==0 ");
  for (unsigned int row=0;row<builder->getNRows();row++)
    {
      for (unsigned int sector=0;sector<builder->getNSectors(row);sector++)
	{
	  StiDetector * detector = builder->getDetector(row,sector);
	  if (!detector)
	    {
	      cout << "StiCompositeSeedFinder::build() row:"<<row<<" sector:"<<sector<<" ERROR" << endl;
	      throw runtime_error("StiCompositeSeedFinder::build() - FATAL - detector==0 ");
	    }
	  trackSeedFinder->addLayer(detector);
	}
    }
  /*
  for (vector<unsigned int>::const_iterator padrow=thePadrows.begin(); 
       padrow!=thePadrows.end(); 
       ++padrow) 
    {
      for (vector<unsigned int>::const_iterator sector=theSectors.begin(); 
	   sector!=theSectors.end();
	   ++sector) 
	{
	  sprintf(szBuf, "Tpc/Padrow_%d/Sector_%d",static_cast<int>(*padrow), static_cast<int>(*sector));
	  cout<<szBuf<<endl;
	  StiDetector* layer = StiToolkit::instance()->getDetectorFinder()->findDetector(szBuf);
	  if (!layer) 
	    throw runtime_error("StiCompositeSeedFinder::build() - FATAL - StiDetector *layer==0");
	  trackSeedFinder->addLayer(layer);
	}
	}*/
  _trackSeedFinders.push_back(trackSeedFinder);
  reset();
  _messenger<<"StiCompositeSeedFinder::build() - INFO - Done"<<endl;
}


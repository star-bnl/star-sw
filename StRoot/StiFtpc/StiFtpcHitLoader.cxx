#include "Stiostream.h"
#include <cmath>

#include "StEventTypes.h"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StiFtpc/StiFtpcHitLoader.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDetectorFinder.h"
#include "Sti/StiDetector.h"
#include "StEvent.h"
#include "StMcEvent.hh"

StiFtpcHitLoader::StiFtpcHitLoader()
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("FtpcHitLoader")
{}
    
StiFtpcHitLoader::StiFtpcHitLoader(StiHitContainer* hitContainer,
				   StiHitContainer* mcHitContainer,
				   Factory<StiHit>*hitFactory,
				   StiDetectorBuilder*detector)
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("FtpcHitLoader",hitContainer,mcHitContainer,hitFactory,detector)
{}

StiFtpcHitLoader::~StiFtpcHitLoader()
{}

void StiFtpcHitLoader::loadHits(StEvent* source,
				Filter<StiTrack> * trackFilter, 
				Filter<StiHit> * hitFilter)
{
  double nhit=0;
  double nprint = 10000.;
  unsigned int plane;
  unsigned int sector;
  const StFtpcHitCollection* ftpcHitCollection = source->ftpcHitCollection();
  const StFtpcPlaneHitCollection* ftpcHitPlaneCollection;
  const StFtpcSectorHitCollection*ftpcHitSectorCollection;
  StiHit* stiHit;

  for (plane=0; 
       plane<ftpcHitCollection->numberOfPlanes(); 
       plane++) 
    {
      ftpcHitPlaneCollection = ftpcHitCollection->plane(plane);
      for (sector=0; 
	   sector<ftpcHitPlaneCollection->numberOfSectors(); 
	   sector++) 
	{
	  ftpcHitSectorCollection = ftpcHitPlaneCollection->sector(sector);
	  const StSPtrVecFtpcHit&hitVector = ftpcHitSectorCollection->hits();
//VP	  vector<StFtpcHit*>::const_iterator iter;
          const_StFtpcHitIterator iter;
	  for (iter=hitVector.begin();
	       iter!=hitVector.end();
	       iter++)
	    {
	      stiHit = _hitFactory->getInstance();
	      stiHit->reset();
	      //_transform->operator()( *iter, stiHit);
	      //stihit->setDetector( layer );
	      //_hitContainer->push_back( stiHit );
	      if (fmod(++nhit, nprint)==0.) 
		_messenger <<"Filling FtpcHit:\t"<<nhit<<endl;
	    }
	}
    }
}

void StiFtpcHitLoader::loadMcHits(StMcEvent* source,
				  bool useMcAsRec,
				  Filter<StiTrack> * trackFilter, 
				  Filter<StiHit> * hitFilter)
{
  /* not yet implemented */
}

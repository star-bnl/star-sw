#include "Stiostream.h"
#include <cmath>
#include "StThreeVector.hh"
#include "StEventTypes.h"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StiEmc/StiEmcHitLoader.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDetector.h"
#include "StEvent.h"
#ifndef ST_NO_NAMESPACES
using namespace std;
#endif

StiEmcHitLoader::StiEmcHitLoader()
  : StiHitLoader<StEvent,StiDetectorBuilder>("EmcHitLoader")
{}
    
StiEmcHitLoader::StiEmcHitLoader(StiHitContainer* hitContainer,
				 Factory<StiHit>*hitFactory,
				 StiDetectorBuilder*detector)
  : StiHitLoader<StEvent,StiDetectorBuilder>("EmcHitLoader",hitContainer,hitFactory,detector)
{}

StiEmcHitLoader::~StiEmcHitLoader()
{}

void StiEmcHitLoader::loadHits(StEvent* source,
			       Filter<StiTrack> * trackFilter, 
			       Filter<StiHit> * hitFilter)
{
  StiHit* stiHit;
  StiDetector*detector=0;// kaboum
  double x,y,z;
  unsigned int layer;
  unsigned int sector;
//VP  vector<StEmcPoint*>::const_iterator pointIter;
  const_StEmcPointIterator pointIter;
  const StEmcCollection* emcCollection = source->emcCollection();
  const StSPtrVecEmcPoint & barrelPoints = emcCollection->barrelPoints();
    for (pointIter=barrelPoints.begin();
       pointIter!=barrelPoints.end();
       pointIter++)
    {
      layer = 0;// (*pointIter)->getLayer();
      sector=0;// (*pointIter)->getSector();
      detector = _detector->getDetector(layer,sector);
      stiHit = _hitFactory->getInstance();
      float energy = 0;// must be fixed!!!
      x = (*pointIter)->position().x();
      y = (*pointIter)->position().y();
      z = (*pointIter)->position().z();
      stiHit->setGlobal(detector,*pointIter,x,y,z,energy);
      _hitContainer->add( stiHit );
    }  
}


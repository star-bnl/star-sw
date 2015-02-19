// $Id: StiSsdHitLoader.cxx,v 1.13.4.1 2015/02/19 01:59:07 smirnovd Exp $
// 
// $Log: StiSsdHitLoader.cxx,v $
// Revision 1.13.4.1  2015/02/19 01:59:07  smirnovd
// Squashed Dmitri Smirnov's commits SL15b..02/16/2015 (a3803b3b)
//
// Made all info/warn/error messages consistent across StiDetectorBuilder's
//
// Made all info/warn/error messages consistent across StiDetectorBuilder's
//
// StiSsdHitLoader: Just to be safe got rid of local variable 'ladder' shadowing the loop's counter having the same name
//
// StiXxxDetectorBuilder: Added a check for valid global object of TGeoManager. The detector builder is required one and cannot proceed if one does not exist
//
// Check for valid gGeoManager in buildDetectors instead of constructor
//
// This is a fixup of the change committed on 2015-01-30 16:33:59 (8b14dfaf)
// The detector builder requires a valid TGeoManager object to build detector
// geometries. However in the current StiMaker gGeoManager is not available when
// detector builder is created. It becomes available just before the
// ::buildDetectors() is called in StiMasterDetectorBuilder::build()
//
// StiSstDetectorBuilder: Switched to StiPlacement constructor when positioning active IST sensors
//
// Revision 1.13  2009/03/18 19:55:39  fisyak
// remove StiDetectorFinder class
//
// Revision 1.12  2005/10/26 21:59:12  fisyak
// get rid off dependencies from StMcEvent
//
// Revision 1.11  2005/06/21 15:31:47  lmartin
// CVS tags added
//
/*!
 * \class StiSsdHitLoader
 * \author Christelle Roy
*/

#include "Stiostream.h"
#include <cmath>
#include <stdio.h>

#include "StEventTypes.h"
#include "StEvent.h"
#include "Sti/Base/Factory.h"
#include "StiSsd/StiSsdHitLoader.h"
#include "Sti/StiHit.h"
#include "Sti/StiTrack.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDetector.h"

StiSsdHitLoader::StiSsdHitLoader()
  : StiHitLoader<StEvent,StiDetectorBuilder>("SsdHitLoader")
{}
    
StiSsdHitLoader::StiSsdHitLoader(StiHitContainer* hitContainer,
				 Factory<StiHit>*hitFactory,
				 StiDetectorBuilder*detector)
  : StiHitLoader<StEvent,StiDetectorBuilder>("SsdHitLoader",hitContainer,hitFactory,detector)
{}

StiSsdHitLoader::~StiSsdHitLoader()
{}

void StiSsdHitLoader::loadHits(StEvent* source,
			       Filter<StiTrack> * trackFilter, 
			       Filter<StiHit> * hitFilter)
{
  cout <<"StiSsdHitLoader::loadHits() - Started"<<endl;
  if (!source)
    throw runtime_error("StiSsdHitLoader::loadHits() - FATAL - source==0 ");
  StSsdHitCollection* ssdhits = source->ssdHitCollection();
  if (!ssdhits)
    {
      cout << "StiSsdHitLoader::loadHits(StEvent* source) - WARNING - NO SSD hits"<<endl;
      return;
    }
  int compt = 0;
  int layer = 0;
  StSsdHit* hit;
  StiHit* stiHit;
  StiDetector* detector;
  if (!_hitContainer)
    throw runtime_error("StiSsdHitLoader::loadHits() - FATAL - _hitContainer==0 ");

  
  for (unsigned int ladder = 0; ladder< ssdhits->numberOfLadders(); ++ladder) 
    {
      StSsdLadderHitCollection* ladderhits = ssdhits->ladder(ladder);
      if (!ladderhits) break;
      
      for (unsigned int wafer = 0; wafer< ladderhits->numberOfWafers(); ++wafer) 
	{
	  StSsdWaferHitCollection* waferhits = ladderhits->wafer(wafer);

	  if (!waferhits) break;
	  const StSPtrVecSsdHit& hits = waferhits->hits(); 
	  
	  for (const_StSsdHitIterator it=hits.begin(); it!=hits.end(); ++it) 
	    {
	      if (!*it) throw runtime_error("StiSsdHitLoader::loadHits() - WARNING - *it==0");
	      hit = static_cast<StSsdHit*>(*it);

	      if (!hit) throw runtime_error("StiSsdHitLoader::loadHits() - WARNING - hit==0");

	      detector = _detector->getDetector(layer, hit->ladder() - 1);

	      if (hit && detector) 
		{
		  compt++;
		  stiHit = _hitFactory->getInstance();
		  stiHit->setGlobal(detector,hit,
				    hit->position().x(),
				    hit->position().y(),
				    hit->position().z(),
				    hit->charge() );
		  _hitContainer->add( stiHit );
		}
	    }
	}
    }

  cout <<"StiSsdHitLoader::loadHits() - I - Done <====> Number of SSD Hits = " <<compt<<endl; 
}
	


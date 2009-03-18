// $Id: StiSsdHitLoader.cxx,v 1.13 2009/03/18 19:55:39 fisyak Exp $
// 
// $Log: StiSsdHitLoader.cxx,v $
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

	      int ssdLadder = hit->ladder();
	      int ladder = ssdLadder -1 ;
	      detector = _detector->getDetector(layer,ladder);	      
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
	


#include "Stiostream.h"
#include <cmath>
#include <stdio.h>

#include "StEventTypes.h"
#include "StEvent.h"
#include "StMcEvent.hh"
#include "Sti/Base/Factory.h"
#include "StiSsd/StiSsdHitLoader.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDetectorFinder.h"
#include "Sti/StiDetector.h"

StiSsdHitLoader::StiSsdHitLoader()
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("SsdHitLoader")
{}
    
StiSsdHitLoader::StiSsdHitLoader(StiHitContainer* hitContainer,
				 StiHitContainer* mcHitContainer,
				 Factory<StiHit>*hitFactory,
				 StiDetectorBuilder*detector)
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("SsdHitLoader",hitContainer,mcHitContainer,hitFactory,detector)
{}

StiSsdHitLoader::~StiSsdHitLoader()
{}

void StiSsdHitLoader::loadHits(StEvent* source,
			       Filter<StiTrack> * trackFilter, 
			       Filter<StiHit> * hitFilter)
{
  _messenger <<"StiSsdHitLoader::loadHits() - Started"<<endl;
  if (!source)
    throw runtime_error("StiSsdHitLoader::loadHits() - FATAL - source==0 ");
  StSsdHitCollection* ssdhits = source->ssdHitCollection();
  if (!ssdhits)
    {
      cout << "StiSsdHitLoader::loadHits(StEvent* source) - WARNING - NO SSD hits"<<endl;
      return;
    }
  int compt = 0;
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
	  _messenger <<"Ladder "<<ladder<<"\twafer "<<wafer<<endl;
	  StSsdWaferHitCollection* waferhits = ladderhits->wafer(wafer);
	  _messenger <<" StSsdWaferHitCollection retrieved" << endl;
	  if (!waferhits) break;
	  const StSPtrVecSsdHit& hits = waferhits->hits(); 
	  const_StSsdHitIterator it;
	  for (it=hits.begin(); 
	       it!=hits.end(); 
	       ++it) 
	    {
	      if (!*it) 
		{
		  _messenger <<"StiSsdHitLoader::loadHits() - WARNING - *it==0"<<endl;
		  break;
		}

	      hit = static_cast<StSsdHit*>(*it);
	      if (!hit) 
		{	  
		  _messenger <<"StiSsdHitLoader::loadHits() - WARNING - hit==0"<<endl;
		  break;
		}
	      int layer = 1;
	      int ssdLadder = hit->ladder();
	      int ladder;
	      switch (layer)
		{
		case 1:
		  switch (ssdLadder)
		    {
		    case 1:  layer = 0; ladder =  0;break;
		    case 2:  layer = 0; ladder =  1;break;
		    case 3:  layer = 0; ladder =  2;break;
		    case 4:  layer = 0; ladder =  3;break;
		    case 5:  layer = 0; ladder =  4;break;
		    case 6:  layer = 0; ladder =  5;break;
		    case 7:  layer = 0; ladder =  6;break;
		    case 8:  layer = 0; ladder =  7;break;
		    case 9:  layer = 0; ladder =  8;break;
		    case 10: layer = 0; ladder =  9;break;
		    case 11: layer = 0; ladder = 10;break;
		    case 12: layer = 0; ladder = 11;break;
		    case 13: layer = 0; ladder = 12;break;
		    case 14: layer = 0; ladder = 13;break;
		    case 15: layer = 0; ladder = 14;break;
		    case 16: layer = 0; ladder = 15;break;
		    case 17: layer = 0; ladder = 16;break;
		    case 18: layer = 0; ladder = 17;break;
		    case 19: layer = 0; ladder = 18;break;
		    case 20: layer = 0; ladder = 19;break;
		    default: throw runtime_error("StiSstHitLoader:loadHits() -E- 1");
		    }
		  break;
		}
	      
	      detector = _detector->getDetector(layer,ladder);
	      if (hit && detector && hit->flag()<4) 
		{
		  compt++;
		  stiHit = _hitFactory->getInstance();
		  stiHit->setGlobal(detector,hit,
				    hit->position().x(),
				    hit->position().y(),
				    hit->position().z(),
				    hit->charge() );
		  _hitContainer->add( stiHit );
		  cout << " hit from :"<<*detector<<endl;
		  cout << " stHit-x:"<<hit->position().x()<<" x:"<<stiHit->x()<<endl;
		  cout << " stHit-y:"<<hit->position().y()<<" y:"<<stiHit->y()<<endl;
		  cout << " stHit-z:"<<hit->position().z()<<" z:"<<stiHit->z()<<endl;
		  cout << " _______DIFF:" << stiHit->x()-detector->getPlacement()->getNormalRadius()<<endl;
		}
	    }
	}
    }
  _messenger <<"StiSsdHitLoader::loadHits() - INFO - Done"<<endl;
  cout<< " Number of SSD Hits = " <<compt<<endl; 
 
}
	

void StiSsdHitLoader::loadMcHits(StMcEvent* source,
				 bool useMcAsRec,
				 Filter<StiTrack> * trackFilter, 
				 Filter<StiHit> * hitFilter)
{
  return;
}
	


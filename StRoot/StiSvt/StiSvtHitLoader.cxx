#include <iostream>
#include <cmath>
#include <stdio.h>

#include "StEventTypes.h"
#include "StEvent.h"
#include "StMcEvent.hh"
#include "Sti/Base/Factory.h"
#include "StiSvt/StiSvtHitLoader.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDetectorFinder.h"
#include "Sti/StiDetector.h"

StiSvtHitLoader::StiSvtHitLoader()
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("SvtHitLoader")
{}
    
StiSvtHitLoader::StiSvtHitLoader(StiHitContainer* hitContainer,
				 StiHitContainer* mcHitContainer,
				 Factory<StiHit>*hitFactory,
				 StiDetectorBuilder*detector)
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("SvtHitLoader",hitContainer,mcHitContainer,hitFactory,detector)
{}

StiSvtHitLoader::~StiSvtHitLoader()
{}

void StiSvtHitLoader::loadHits(StEvent* source,
			       Filter<StiTrack> * trackFilter, 
			       Filter<StiHit> * hitFilter)
{
  _messenger <<"StiSvtHitLoader::loadHits() - Started"<<endl;
  if (!source)
    throw runtime_error("StiSvtHitLoader::loadHits() - FATAL - source==0 ");
  StSvtHitCollection* svthits = source->svtHitCollection();
  if (!svthits)
    {
      cout << "StiSvtHitLoader::loadHits(StEvent* source) - WARNING - NO SVT hits"<<endl;
      return;
    }
  StSvtHit* hit;
  StiHit* stiHit;
  StiDetector* detector;
  if (!_hitContainer)
    throw runtime_error("StiSvtHitLoader::loadHits() - FATAL - _hitContainer==0 ");
  for (unsigned int barrel=0; barrel<svthits->numberOfBarrels(); ++barrel) 
    {
      StSvtBarrelHitCollection* barrelhits = svthits->barrel(barrel);
      if (!barrelhits) break;
      for (unsigned int ladder=0; ladder<barrelhits->numberOfLadders(); ++ladder) 
	{
	  StSvtLadderHitCollection* ladderhits = barrelhits->ladder(ladder);
	  if (!ladderhits) break;
	  for (unsigned int wafer=0; wafer<ladderhits->numberOfWafers(); ++wafer) 
	    {
	      _messenger <<"Barrel "<<barrel<<"\tladder "<<ladder<<"\twafer "<<wafer<<endl;
	      StSvtWaferHitCollection* waferhits = ladderhits->wafer(wafer);
	      _messenger <<" StSvtWaferHitCollection retrieved" << endl;
	      if (!waferhits) break;
	      const StSPtrVecSvtHit& hits = waferhits->hits(); 
	      for (vector<StSvtHit*>::const_iterator it=hits.begin(); 
		   it!=hits.end(); 
		   ++it) 
		{
		  cout << "fetch hit"<<endl;
		  if (!*it) 
		    {
		      _messenger <<"StiSvtHitLoader::loadHits() - WARNING - *it==0"<<endl;
		      break;
		    }
		  hit = static_cast<StSvtHit*>(*it);
		  if (!hit) 
		    {
		      _messenger <<"StiSvtHitLoader::loadHits() - WARNING - hit==0"<<endl;
		      break;
		    }
		  int svtLayer = hit->layer();
		  int svtLadder = hit->ladder();
		  int layer,ladder;
		  switch (svtLayer)
		    {
		    case 1:
		      switch (svtLadder)
			{
			case 2: layer = 0; ladder = 0;break;
			case 4: layer = 0; ladder = 1;break;
			case 6: layer = 0; ladder = 2;break;
			case 8: layer = 0; ladder = 3;break;
			default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 1");
			}
		      break;
		    case 2:
		      switch (svtLadder)
			{
			case 1: layer = 1; ladder = 0;break;
			case 3: layer = 1; ladder = 1;break;
			case 5: layer = 1; ladder = 2;break;
			case 7: layer = 1; ladder = 3;break;
			default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 2");
			}
		      break;
		    case 3:
		      switch (svtLadder)
			{
			case  2: layer = 2; ladder = 0;break;
			case  4: layer = 2; ladder = 1;break;
			case  6: layer = 2; ladder = 2;break;
			case  8: layer = 2; ladder = 3;break;
			case 10: layer = 2; ladder = 4;break;
			case 12: layer = 2; ladder = 5;break;
			default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 3");
			}
		      break;
		    case 4:
		      switch (svtLadder)
			{
			case  1: layer = 3; ladder = 0;break;
			case  3: layer = 3; ladder = 1;break;
			case  5: layer = 3; ladder = 2;break;
			case  7: layer = 3; ladder = 3;break;
			case  9: layer = 3; ladder = 4;break;
			case 11: layer = 3; ladder = 5;break;
			default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 4");
			}
		      break;
		    case 5:
		      switch (svtLadder)
			{
			case  2: layer = 4; ladder = 0;break;
			case  4: layer = 4; ladder = 1;break;
			case  6: layer = 4; ladder = 2;break;
			case  8: layer = 4; ladder = 3;break;
			case 10: layer = 4; ladder = 4;break;
			case 12: layer = 4; ladder = 5;break;
			case 14: layer = 4; ladder = 6;break;
			case 16: layer = 4; ladder = 7;break;
			default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 5");
			}
		      break;
		    case 6:
		      switch (svtLadder)
			{
			case  1: layer = 5; ladder = 0;break;
			case  3: layer = 5; ladder = 1;break;
			case  5: layer = 5; ladder = 2;break;
			case  7: layer = 5; ladder = 3;break;
			case  9: layer = 5; ladder = 4;break;
			case 11: layer = 5; ladder = 5;break;
			case 13: layer = 5; ladder = 6;break;
			case 15: layer = 5; ladder = 7;break;
			default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 6");
			}
		      break;
		    }
		  cout << "hit->layer():"<<hit->layer()
		       << "hit->ladder():"<<hit->ladder()
		       << " layer:"<<layer
		       << " ladder:"<<ladder<<endl;
		  
		  //detector = _detector->getDetector(hit->layer(),hit->ladder());
		  detector = _detector->getDetector(layer,ladder);
		  if (hit && detector && hit->flag()<4) 
		    {
		      cout << "StiSvtHitLoader::loadHits() - INFO - set global"<<endl;
		      stiHit = _hitFactory->getInstance();
		      stiHit->setGlobal(detector,hit,
					hit->position().x(),
					hit->position().y(),
					hit->position().z(),
					hit->charge() );
		      cout << "push the hit back"<<endl;
		      _hitContainer->push_back( stiHit );
		      
		    }
		}
	    }
	}
    }
  _messenger <<"StiSvtHitLoader::loadHits() - INFO - Done"<<endl;
}
	

void StiSvtHitLoader::loadMcHits(StMcEvent* source,
				 bool useMcAsRec,
				 Filter<StiTrack> * trackFilter, 
				 Filter<StiHit> * hitFilter)
{
  return;
}
	


#include <iostream>
#include <stdexcept>
#include <cmath>
#include <stdio.h>
#include <stdlib.h>
#include "StEvent.h"
#include "StMcEvent/StMcEvent.hh"
#include "StEventTypes.h"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "Sti/Base/Factory.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorBuilder.h"
//#include "Sti/StiMcTrack.h"
#include "Sti/StiTrackContainer.h"
#include "StiHpdHitLoader.h"
#include "StMcEvent/StMcTrack.hh"
#include "StMcEvent/StMcHpdHit.hh"
#include "StMcEvent/StMcHpdHitCollection.hh"
#include "StMcEvent/StMcHpdLayerHitCollection.hh"

StiHpdHitLoader::StiHpdHitLoader()
    : StiHitLoader<StEvent,StiDetectorBuilder>("HpdHitLoader")
{}

StiHpdHitLoader::StiHpdHitLoader(StiHitContainer* hitContainer,
				 Factory<StiHit>*hitFactory,
				 StiDetectorBuilder * detector)
    : StiHitLoader<StEvent,StiDetectorBuilder>("HpdHitLoader",hitContainer,hitFactory,detector)
{evNum=0;}

StiHpdHitLoader::~StiHpdHitLoader()
{}

void StiHpdHitLoader::loadHits(StEvent* source,
			       Filter<StiTrack> * trackFilter,
			       Filter<StiHit> * hitFilter)
{
    n=0;
    //cout << "  n = " << n << endl;
    cout << "StiHpdHitLoader::loadHits(StEvent*) -I- Started" << endl;
    if (!_detector)
	throw runtime_error("StiHpdHitLoader::loadHits(StEvent*) - FATAL - _detector==0");
    if(!_hitContainer)
	throw runtime_error("StiHpdHitLoader::loadHits(StEvent*) - FATAL - _hitContainer==0");

    StRnDHitCollection *col = source->rndHitCollection();
    if (!col) {
	cout <<"StiHpdHitLoader::loadHits\tERROR:\tcol==0"<<endl;
	cout <<"You must not have pixelFastSim in your chain"<<endl;
	cout <<"will return with no action taken"<<endl;
	return;
    }
    StSPtrVecRnDHit& vec = col->hits();

    cout <<"StiHpdHitLoader: Hpd Hits: "<<vec.size()<<endl;
    
    for(unsigned int j=0; j<vec.size(); j++) {

	StRnDHit* hit = vec[j];
	assert(hit);

	if (hit->detector()!=kHpdId) continue;
	
	cout <<"retrieve detector"<<endl;
	StiDetector* detector = _detector->getDetector(hit->layer()-1, hit->ladder()-1);
	if (!detector) cout <<"no detector found for hit:\t"<<*hit<<endl;
	assert(detector);
	cout <<"add hit to detector:\t"<<detector->getName()<<endl;
	
	StiHit * stiHit = _hitFactory->getInstance();
	if(!stiHit) throw runtime_error("StiHpdHitLoader::loadHits(StEvent*) -E- stiHit==0");
	stiHit->reset();
	stiHit->setGlobal(detector, hit, hit->position().x(),hit->position().y(),hit->position().z(),hit->charge());
	_hitContainer->add( stiHit );
    }
    
    cout << "StiHpdHitLoader::loadHits(StEvent*) -I- Done" << endl;
}

/// Load the MC hits of the Hpd detector associated with the given source, and StMcTrack
/// Call once per MC track - but currently only able to load from source->hpdHitCollection()
/// so... use a cached hit pointer to remember whether an event has already been loaded.
/*
void StiHpdHitLoader::loadMcHits(StMcEvent* source,
				 bool useMcAsRec,
				 Filter<StiTrack> * trackFilter,
				 Filter<StiHit> * hitFilter,
				 StMcTrack & stMcTrack,
				 StiMcTrack & stiMcTrack)
{
  if (evNum==source->eventNumber()) return;
  else evNum=source->eventNumber();

  cout << "StiHpdHitLoader::loadMcHits(StMcEvent*) -I- Started" << endl;
  if (!_detector)  throw runtime_error("StiHpdHitLoader::loadMcHits(StMcEvent*) -F- _detector==0");
  if(!_mcHitContainer) throw runtime_error("StiHpdHitLoader::loadMcHits(StMcEvent*) -F- _mcHitContainer==0");
  if(!_mcTrackFactory) throw runtime_error("StiHpdHitLoader::loadMcHits() -F- _mcTrackFactory==0");
  if (!_mcTrackContainer) throw runtime_error("StiHpdHitLoader::loadMcHitss() -F- _mcTrackContainer==0");
  if(!_hitFactory) throw runtime_error("StiHpdHitLoader::loadMcHits(StMcEvent*) -F- _hitFactory==0");
  cout << "StiHpdHitLoader::loadMcHits() -I- Loading"<<endl;

  if (n>0) return;
  n++;
  StMcHpdHitCollection* allHpdHitCol = source->hpdHitCollection();
  //cout <<   "Hpd: Number of Layers" << allHpdHitCol->numberOfLayers() << endl;
  for(int tiLayer=0; tiLayer<allHpdHitCol->numberOfLayers(); tiLayer++)
    {
      cout << "StiHpdHitLoader::loadMcHits(StMcEvent*) -I- layer " << tiLayer << endl;
      StMcHpdLayerHitCollection* HpdHitCol = allHpdHitCol->layer(tiLayer);
      StSPtrVecMcHpdHit& hits =  HpdHitCol->hits();
      
      int nHitCount = 0;
      StiDetector* detector;
      
      for (vector<StMcHpdHit*>::const_iterator iterHit = hits.begin();iterHit != hits.end();iterHit++)
	{
	  StMcHpdHit *hit = *iterHit;
	  if(!hit){//something's wrong
	    cout<<"null hit"<<endl;
	    continue;
	  }
	  nHitCount++;
	  //cout << "guck: " << tiLayer << "   " << hit->layer()-1 << endl;
	  detector = _detector->getDetector(hit->layer()-1, hit->ladder()-1);
	  StiHit * stiHit = _hitFactory->getInstance();
	  if(!stiHit) throw runtime_error("StiHpdHitLoader::loadHits(StEvent*) -E- stiHit==0");
	  stiHit->reset();
	  stiHit->setGlobal(detector, 0, hit->position().x(),hit->position().y(),hit->position().z(),hit->dE());
	  _hitContainer->add( stiHit );
	  //stiMcTrack.addHit( stiHit );
	}
      cout << "StiHpdHitLoader::loadMcHits(StEvent*) -I-  number of hits uploaded, after: " << _hitContainer->size() << endl;
      cout << "StiHpdHitLoader::loadMcHits(StEvent*) -I- Done" << endl;
      cout<<"nHitCount\t"<<nHitCount<<endl;
    }
}
*/

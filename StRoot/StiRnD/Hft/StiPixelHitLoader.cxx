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
#include "Sti/StiMcTrack.h"
#include "Sti/StiTrackContainer.h"
#include "StiPixelHitLoader.h"
#include "StMcEvent/StMcTrack.hh"
#include "StMcEvent/StMcPixelHit.hh"
#include "StMcEvent/StMcPixelHitCollection.hh"
#include "StMcEvent/StMcPixelLayerHitCollection.hh"

StiPixelHitLoader::StiPixelHitLoader()
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("PixelHitLoader")
{}
    
StiPixelHitLoader::StiPixelHitLoader(StiHitContainer* hitContainer,
				     StiHitContainer* mcHitContainer,
				     Factory<StiHit>*hitFactory,
				     StiDetectorBuilder * detector)
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("PixelHitLoader",hitContainer,mcHitContainer,hitFactory,detector)
{}

StiPixelHitLoader::~StiPixelHitLoader()
{}

void StiPixelHitLoader::loadHits(StEvent* source,
				 Filter<StiTrack> * trackFilter, 
				 Filter<StiHit> * hitFilter)
{
  cout << "StiPixelHitLoader::loadHits(StEvent*) -I- Started" << endl;
  if (!_detector)
    throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) - FATAL - _detector==0");
  if(!_hitContainer)
    throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) - FATAL - _hitContainer==0");
  cout << "StiPixelHitLoader::loadHits(StEvent*) -I- Done" << endl;
}

void StiPixelHitLoader::loadMcHits(StMcEvent* source,
				   bool useMcAsRec,
				   Filter<StiTrack> * trackFilter, 
				   Filter<StiHit> * hitFilter)
{
  cout << "StiPixelHitLoader::loadMcHits(StMcEvent*) -I- Started" << endl;
  cout << "  number of hits uploaded, before: " << _hitContainer->size() << endl;
  if (!_detector)  throw runtime_error("StiPixelHitLoader::loadMcHits(StMcEvent*) -F- _detector==0");
  if(!_mcHitContainer) throw runtime_error("StiPixelHitLoader::loadMcHits(StMcEvent*) -F- _mcHitContainer==0");
  if(!_mcTrackFactory) throw runtime_error("StiPixelHitLoader::loadMcHits() -F- _mcTrackFactory==0");
  if (!_mcTrackContainer) throw runtime_error("StiPixelHitLoader::loadMcHitss() -F- _mcTrackContainer==0");
  if(!_hitFactory) throw runtime_error("StiPixelHitLoader::loadMcHits(StMcEvent*) -F- _hitFactory==0");
  //cout << "StiPixelHitLoader::loadMcHits() -I- Loading"<<endl;
  StMcPixelHitCollection* allPixHitCol = source->pixelHitCollection();

  int nHitCount = 0;
  //cout << endl << endl;
  //cout << "pixellayers: " << allPixHitCol->numberOfLayers() << endl;

  for(int tiLayer=0; tiLayer<allPixHitCol->numberOfLayers(); tiLayer++){
    //cout << "StiPixelHitLoader::loadMcHits(StMcEvent*) -I- layer " << tiLayer << endl;
    StMcPixelLayerHitCollection* pixHitCol = allPixHitCol->layer(tiLayer);
    StSPtrVecMcPixelHit& hits =  pixHitCol->hits();
    for (vector<StMcPixelHit*>::const_iterator iterHit = hits.begin();
	 iterHit != hits.end(); 
	 iterHit++) 
      {
	nHitCount++;
	//cout << "StiPixelHitLoader::loadMcHits(StMcEvent*) -I- HitCount " << nHitCount << endl;
	//cout << "StiPixelHitLoader::loadMcHits(StMcEvent*) -I- loading McHit " << endl;
	StMcPixelHit*hit = *iterHit;       

	StiDetector* detector;
	int row = 1;
	int nLadder = 0;

	unsigned long volId = hit->volumeId();
	unsigned long iModule = volId/1000000;
	unsigned long iLadder = (volId%1000000)/10000;
	unsigned long iLayer = 0;
	unsigned long iSector = 0;
	if (iLadder<4)
	  {
	    iLayer = 1;
	    iSector = (iModule-1)*3 + (iLadder-1);
	  }
	else
	  {
	    iLayer = 0;
	    iSector = (iModule-1);
	  }
	detector = _detector->getDetector(iLayer, iSector);
	StiHit * stiHit = _hitFactory->getInstance();
	if(!stiHit) throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- stiHit==0");
	stiHit->reset();
	//cout << "StiPixelHitLoader::loadMcHits(StMcEvent*) -I- phi, sector " << phi*180./M_PI << "   " << (int) (phi/M_PI*6.) +1 << endl; 
	//cout << "StiPixelHitLoader::loadMcHits(StMcEvent*) -I- McHit, radius, phi " << radius 
	//   << "   " << phi*180./M_PI << "   " << nLadder << "   " << hit->volumeId() << endl;

	// smear MC hits by Gaussian of 4micro-meter width. Poor's man solution of a response simulator...
	double sigma = 0.; // um
	double x = hit->position().x();
	double y = hit->position().y();
	//int RAND_MAX = 1;
	double u1, u2;
	double v1, v2;
	double r = 2.;
	double z1 = 10.;
	double z2 = 10.;
	while(fabs(z1)>2. || fabs(z2)>2.) // sigma
	  {
	    r = 2.;
	    while(r>1.)
	      {
		u1 = rand()/double(RAND_MAX);
		u2 = rand()/double(RAND_MAX);
		v1 = 2*u1 - 1.;
		v2 = 2*u2 - 1.;
		r = pow(v1,2) + pow(v2,2);
	      }
	    z1 = v1*sqrt(-2.*log(r)/r);
	    z2 = v2*sqrt(-2.*log(r)/r);
	  }

	//cout << "StiPixelHitLoader::loadMcHits(StEvent*) -I- random number: " << z1*sigma << "   " << z2*sigma << endl;
	//cout << "setGlobal" << endl;
	stiHit->setGlobal(detector, 
			  0, 
			  x+z1*sigma/1.0e04,
			  y+z2*sigma/1.0e04,
			  hit->position().z(),
			  hit->dE());
	//cout << " HIT:::" << *stiHit << endl;
	_hitContainer->add( stiHit );
      }
  }
  cout << "StiPixelHitLoader::loadMcHits(StEvent*) -I-  number of hits uploaded, after: " << _hitContainer->size() << endl;
  cout << "StiPixelHitLoader::loadMcHits(StEvent*) -I- Done" << endl;
}


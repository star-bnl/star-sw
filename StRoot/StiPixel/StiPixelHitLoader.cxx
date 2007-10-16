/*
 * $Id: StiPixelHitLoader.cxx,v 1.15 2007/10/16 19:57:45 fisyak Exp $
 *
 * $Log: StiPixelHitLoader.cxx,v $
 * Revision 1.15  2007/10/16 19:57:45  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 1.14  2006/02/17 21:37:53  andrewar
 * Removed streaming of all read pixel hits, added version comments log
 *
 */


#include <iostream>
#include <stdexcept>
#include <cmath>
#include <stdio.h>
#include <stdlib.h>
#include "StEvent.h"
#include "StEventTypes.h"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "Sti/Base/Factory.h"
#include "Sti/StiHit.h"
#include "StRnDHit.h"
#include "StRnDHitCollection.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiTrackContainer.h"
#include "StiPixelHitLoader.h"

StiPixelHitLoader::StiPixelHitLoader()
: StiHitLoader<StEvent,StiDetectorBuilder>("PixelHitLoader")
{}

StiPixelHitLoader::StiPixelHitLoader(StiHitContainer* hitContainer,
                                     Factory<StiHit>*hitFactory,
                                     StiDetectorBuilder * detector)
: StiHitLoader<StEvent,StiDetectorBuilder>("PixelHitLoader",hitContainer,hitFactory,detector)
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
    
    StRnDHitCollection *col = source->rndHitCollection();
    if (!col) {
	cout <<"StiPixelHitLoader::loadHits\tERROR:\tcol==0"<<endl;
	cout <<"You must not have pixelFastSim in your chain"<<endl;
	cout <<"will return with no action taken"<<endl;
	return;
    }
    StSPtrVecRnDHit& vec = col->hits();
    
    StiDetector *detector=0;
    int nHit=0;
    for(unsigned int j=0; j<vec.size(); j++)	{
	StRnDHit *pxlH = vec[j];
	if(!pxlH)
	  throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- NULL hit in container");

	if (pxlH->detector()!=kPxlId) continue;
	
	detector= _detector->getDetector(pxlH->layer()-1, pxlH->ladder()-1);
	if(!detector)
	  throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- NULL detector pointer");
	
	StiHit *stiHit=_hitFactory->getInstance();
	if(!stiHit) throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- stiHit==0");
	stiHit->reset();

	stiHit->setGlobal(detector, pxlH,
                          pxlH->position().x(), pxlH->position().y(), pxlH->position().z(),
                          pxlH->charge());
	_hitContainer->add(stiHit);

	//done loop over hits
	nHit++;
      }

    cout <<"StiPixelHitLoader:loadHits -I- Loaded "<<nHit<<" pixel hits."<<endl;


}


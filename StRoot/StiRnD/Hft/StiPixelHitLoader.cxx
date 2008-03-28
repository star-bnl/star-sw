/*
 * $Id: StiPixelHitLoader.cxx,v 1.17 2006/11/30 20:42:46 andrewar Exp $
 *
 * $Log: StiPixelHitLoader.cxx,v $
 * Revision 1.17  2006/11/30 20:42:46  andrewar
 * Fixed sign error in pixel smearing.
 *
 * Revision 1.16  2006/11/29 04:19:23  andrewar
 * Added smearing to hit loader.
 *
 * Revision 1.15  2006/11/17 15:39:03  wleight
 * Changes to make HFT hits work with UPGR05 geometry
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
	StRnDHit *hftH = vec[j];
	if(!hftH)
	  throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- NULL hit in container");

	if (hftH->detector()!=kHftId) continue;
	
	// Because of a screw up with the layout in the pixlgeo3.g file, the detectors are not 
	// sequential in phi. List (geant,ittf): (1,2),(2,1),(3,0),(4,5),(5,4),(6,3),(7,8),(8,7),(9,6)
	// This works to: 
	//    ittfL = 3*n - geantL
	//    n = 1,3,5 (or 2k+1, k=0,1,2)
	//    k= int[( geantL -1 )/3]
	//Resolve the layer and ladder ids.

	//detector= _detector->getDetector(hftH->layer()-1, hftH->ladder()-1);
	int ittfLadder;
	printf("hit layer: %i ladder: %i\n",hftH->layer(), hftH->ladder());
	if(hftH->layer()==1)
	  ittfLadder= ( 2* int( (hftH->ladder()-1.) /3. ) +1)*3 - hftH->ladder();
	else
	  ittfLadder=( 2* int( (hftH->ladder()-1.) /8. ) +1)*8 - hftH->ladder();
	printf("ittfLadder: %i\n",ittfLadder);
	detector= _detector->getDetector(hftH->layer()-1, ittfLadder);



	if(!detector)
	  throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- NULL detector pointer");
	//cout <<"add hit to detector:\t"<<detector->getName()<<endl;
	
	StiHit *stiHit=_hitFactory->getInstance();
	if(!stiHit) throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- stiHit==0");
	stiHit->reset();

	double dCos = cos(detector->getPlacement()->getNormalRefAngle());
	double dSin = sin(detector->getPlacement()->getNormalRefAngle());
        double x = hftH->position().x() * dCos + hftH->position().y() * dSin;
        double y = hftH->position().x() * -1.*dSin + hftH->position().y() * dCos;
	double z = hftH->position().z();

	y = y + hftH->positionError().y();

	double xg = dCos * x - dSin * y;
	double yg = dSin * x + dCos * y;
	double zg = z + hftH->positionError().z();

	const StThreeVectorF newPos(xg,yg,zg);
	hftH->setPosition(newPos);

	
	stiHit->setGlobal(detector, hftH,
                          hftH->position().x(), hftH->position().y(),
			  hftH->position().z(), hftH->charge());
		
	_hitContainer->add(stiHit);

	//done loop over hits
	nHit++;
      }

    cout <<"StiPixelHitLoader:loadHits -I- Loaded "<<nHit<<" pixel hits."<<endl;


}


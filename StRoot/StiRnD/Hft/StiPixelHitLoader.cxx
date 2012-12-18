/*
 * $Id: StiPixelHitLoader.cxx,v 1.24 2012/12/18 20:52:32 bouchet Exp $
 *
 * $Log: StiPixelHitLoader.cxx,v $
 * Revision 1.24  2012/12/18 20:52:32  bouchet
 * update for DEV13 geometry
 *
 * Revision 1.23  2011/04/22 22:00:18  fisyak
 * warn off
 *
 * Revision 1.22  2009/02/09 02:47:19  andrewar
 * UPGR15 update. Will break backward compatibility with older geometries.
 *
 * Revision 1.21  2008/03/25 20:02:28  andrewar
 * Removed hit smearing.
 *
 * Revision 1.20  2007/10/16 19:50:25  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 1.19  2007/05/16 15:03:22  andrewar
 * Removed cout's in favor of LOG_INFO.
 *
 * Revision 1.18  2007/03/28 13:33:23  mmiller
 * Removed cout/printf's.
 *
 * Revision 1.17  2006/11/30 20:42:46  andrewar
 * Fixed sign error in pixel smearing.
 *
 * Revision 1.16  2006/11/29 04:19:23  andrewar
 * Added smearing to hit loader.
 *
 * Revision 1.15  2006/11/17 15:39:03  wleight
 * Changes to make PXL hits work with UPGR05 geometry
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

    LOG_INFO << " -I- Started" << endl;
    if (!_detector)
	throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) - FATAL - _detector==0");
    if(!_hitContainer)
	throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) - FATAL - _hitContainer==0");
    
    StRnDHitCollection *col = source->rndHitCollection();
    if (!col) {
	LOG_ERROR <<"StiPixelHitLoader::loadHits\tERROR:\tcol==0"
		  <<"You must not have pixelFastSim in your chain"
		  <<"will return with no action taken"<<endm;
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
      
      // Because of a screw up with the layout in the pixlgeo3.g file, the detectors are not 
      // sequential in phi. List (geant,ittf): (1,2),(2,1),(3,0),(4,5),(5,4),(6,3),(7,8),(8,7),(9,6)
      // This works to: 
      //    ittfL = 3*n - geantL
      //    n = 1,3,5 (or 2k+1, k=0,1,2)
      //    k= int[( geantL -1 )/3]
      //Resolve the layer and ladder ids.
      
      //detector= _detector->getDetector(pxlH->layer()-1, pxlH->ladder()-1);
      //      int ittfLadder;
      //assert(row<_detectors.size());
      //assert(sector<_detectors[row].size());
      //MLM 
      LOG_DEBUG <<Form("hit layer: %i ladder: %i\n",pxlH->layer(), pxlH->ladder()) << endm;
      /*
	if(pxlH->layer()==1)
	ittfLadder= ( 2* int( (pxlH->ladder()-1.) /3. ) +1)*3 - pxlH->ladder();
	else
	ittfLadder=( 2* int( (pxlH->ladder()-1.) /8. ) +1)*8 - pxlH->ladder();
      */
      //MLM 
      //printf("row<==> pxlH()-1:%i sector<==>ittfLadder: %i\n",pxlH->layer()-1,ittfLadder);
      //detector= _detector->getDetector(pxlH->layer(), ittfLadder);

      detector= _detector->getDetector(pxlH->layer(), pxlH->ladder());
      
      if(!detector)
	throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- NULL detector pointer");
      LOG_DEBUG <<"add hit to detector:\t"<<detector->getName()<<endm;
      //double angle   = detector->getPlacement()->getNormalRefAngle();
      //double radius  = detector->getPlacement()->getNormalRadius();
      //double zcenter = detector->getPlacement()->getZcenter();
      //LOG_DEBUG << " radius = "<< radius << " angle = " << angle << " zCenter = " << zcenter << endm;
      double angle    = detector->getPlacement()->getNormalRefAngle();
      double radius   = detector->getPlacement()->getNormalRadius();
      double zcenter  = detector->getPlacement()->getZcenter();
      double halfDepth = detector->getShape()->getHalfDepth();
      double halfWidth = detector->getShape()->getHalfWidth();
      double thick     = detector->getShape()->getThickness();
      LOG_DEBUG << " detector info " << *detector << endm;
      LOG_DEBUG << " radius = "<< radius << " angle = " << angle << " zCenter = " << zcenter << endm;
      LOG_DEBUG << " depth = " << halfDepth << " Width = " << halfWidth << " thickness= " << thick << endm; 
      LOG_DEBUG << " key 1 : " << detector->getKey(1) <<" key 2 : " << detector->getKey(2) << endm; 
      
      StiHit *stiHit=_hitFactory->getInstance();
	if(!stiHit) throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- stiHit==0");
	stiHit->reset();	
	
	stiHit->setGlobal(detector, pxlH,
                          pxlH->position().x(), pxlH->position().y(),
			  pxlH->position().z(), pxlH->charge());
	
	_hitContainer->add(stiHit);
	LOG_DEBUG <<" nHit = "<<nHit<<" Layer = "<<pxlH->layer()<<" Ladder = "<<pxlH->ladder()<<" x = "<<pxlH->position().x()<<" y = "<<pxlH->position().y()<<" z = "<<pxlH->position().z()<<endm;
	
	//done loop over hits
	nHit++;
      }

    LOG_INFO <<"StiPixelHitLoader:loadHits -I- Loaded "<<nHit<<" pixel hits."<<endm;


}


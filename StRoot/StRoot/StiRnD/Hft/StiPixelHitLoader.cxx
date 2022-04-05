 /*
 * $Id: StiPixelHitLoader.cxx,v 1.28 2014/01/30 16:50:59 didenko Exp $
 *
 * $Log: StiPixelHitLoader.cxx,v $
 * Revision 1.28  2014/01/30 16:50:59  didenko
 * get back to previous revision
 *
 * Revision 1.26  2013/03/12 15:04:00  bouchet
 * StPxlHit navigation to retrieve hits
 *
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
//#include "StRnDHit.h"
#include "StPxlHitCollection.h"
#include "StPxlHit.h"
//#include "StRnDHitCollection.h"
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
    
    StPxlHitCollection *col = source->pxlHitCollection();
    if (!col) {
	LOG_ERROR <<"StiPixelHitLoader::loadHits\tERROR:\tcol==0"
		  <<"You must not have pixelFastSim in your chain"
		  <<"will return with no action taken"<<endm;
	return;
    }
    //Added by Michael Lomnitz (KSU):  Loops over Sector/Ladder/Sensor to obtain the whole hit collection
    StiDetector *detector=0;
    int nHit=0;

    StPxlHitCollection* PxlHitCollection=source->pxlHitCollection(); 
    if(! PxlHitCollection){cout<<"No PXL hit collection"<<endl; return;}
    UInt_t numberOfSectors=PxlHitCollection->numberOfSectors();
    for(UInt_t i=0;i<numberOfSectors;i++){
      StPxlSectorHitCollection* PxlSectorHitCollection=PxlHitCollection->sector(i); 
      if(! PxlSectorHitCollection){cout<<"No PXLSector hit collection"<<endl; return;}
      UInt_t numberOfLadders=PxlSectorHitCollection->numberOfLadders();
      for(UInt_t j=0;j<numberOfLadders;j++){
	StPxlLadderHitCollection* PxlLadderHitCollection=PxlSectorHitCollection->ladder(j); 
	if(! PxlLadderHitCollection){cout<<"No PXLLadder hit collection"<<endl; return;}
	UInt_t numberOfSensors=PxlLadderHitCollection->numberOfSensors();
	for(UInt_t l=0;l<numberOfSensors;l++){
	  StPxlSensorHitCollection* PxlSensorHitCollection=PxlLadderHitCollection->sensor(l);
	  StSPtrVecPxlHit& vec = PxlSensorHitCollection->hits();	 
	  
	  LOG_DEBUG<<"StiPixelHitLoader - collection size: "<<vec.size()<<endm;
	  
	  for(unsigned int j=0; j<vec.size(); j++)	{
	    StPxlHit *pxlH = vec[j];
	    if(!pxlH)
	      throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- NULL hit in container");
	    
	    if (pxlH->detector()!=kPxlId) continue;
	    
	    int LAY=0, LAD=0;
	    
	    if(pxlH->layer()==1)
	      {
		LAY=0;//(int)pxlH->layer()-1;
		LAD=(int)pxlH->sector()-1;
	      }
	    else
	      {
		LAY=1;//(int)pxlH->layer()-1;
		LAD=(((int)pxlH->sector()-1)*3 + (int)pxlH->ladder()-1);
	      }
	    LOG_DEBUG << " layer : "<< LAY <<" ladder :  "<< LAD << endm;
	    LOG_DEBUG << "X/Y/Z    : " << pxlH->position().x()<<"/"<< pxlH->position().y()<<"/"<<pxlH->position().z()<<endm;
	    LOG_DEBUG << "Xl/Yl/Zl : " << pxlH->localPosition(0)<<"/"<< pxlH->localPosition(1)<<"/"<<pxlH->localPosition(2)<<endm;
	    detector= _detector->getDetector(LAY,LAD);
	    
	    if(!detector)
	      throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- NULL detector pointer");
	    LOG_DEBUG <<"add hit to detector:\t"<<detector->getName()<<endm;
	    double angle     = detector->getPlacement()->getNormalRefAngle();
	    double radius    = detector->getPlacement()->getNormalRadius();
	    double zcenter   = detector->getPlacement()->getZcenter();
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
	    LOG_DEBUG <<" nHit = "<<nHit<<" Sector = "<<pxlH->sector()<<" Ladder = "<<pxlH->ladder()<<" x = "<<pxlH->position().x()<<" y = "<<pxlH->position().y()<<" z = "<<pxlH->position().z()<<endm;
	    
	    //done loop over hits
	    nHit++;
	  }
	}
      }
    }
    LOG_INFO <<"StiPixelHitLoader:loadHits -I- Loaded "<<nHit<<" pixel hits."<<endm;
}


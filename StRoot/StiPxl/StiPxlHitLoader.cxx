 /*
 * $Id: StiPxlHitLoader.cxx,v 1.5 2014/03/07 16:27:46 smirnovd Exp $
 *
 * $Log: StiPxlHitLoader.cxx,v $
 * Revision 1.5  2014/03/07 16:27:46  smirnovd
 * Updated assignment and extraction of sti (pixel sensor) detectors by keys
 *
 * Revision 1.4  2014/03/04 15:36:38  smirnovd
 * Remove constraint on sectors used in engeneering run in 2013
 *
 * Revision 1.3  2014/03/03 23:54:35  smirnovd
 * Remove extra pointer validation check
 *
 * Revision 1.2  2014/02/04 16:56:48  smirnovd
 * Clean up and improved readability
 *
 * Revision 1.1  2014/02/01 19:19:35  smirnovd
 * Initial commit: Changed files prefix StiPixel... to StiPxl... according to STAR convention
 *
 * Revision 1.16  2014/02/01 02:48:07  smirnovd
 * Improved style format by running astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
 *
 * Revision 1.15  2014/02/01 02:37:17  smirnovd
 * This commit is intended to sync with what we had in StRoot/StiRnD
 *
 * Revision 1.27  2014/01/23 17:38:18  bouchet
 * *** empty log message ***
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
#include "StPxlHitCollection.h"
#include "StPxlHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiTrackContainer.h"
#include "StiPxlHitLoader.h"
#include "StPxlUtil/StPxlConstants.h"


StiPxlHitLoader::StiPxlHitLoader()
   : StiHitLoader<StEvent, StiDetectorBuilder>("PixelHitLoader")
{}


StiPxlHitLoader::StiPxlHitLoader(StiHitContainer *hitContainer,
                                 Factory<StiHit> *hitFactory,
                                 StiDetectorBuilder *detector)
   : StiHitLoader<StEvent, StiDetectorBuilder>("PixelHitLoader", hitContainer, hitFactory, detector)
{}


void StiPxlHitLoader::loadHits(StEvent *source,
                                 Filter<StiTrack> *trackFilter,
                                 Filter<StiHit> *hitFilter)
{
   LOG_INFO << " -I- Started" << endl;

   if (!_detector)
      throw runtime_error("StiPxlHitLoader::loadHits(StEvent*) - FATAL - _detector==0");

   if (!_hitContainer)
      throw runtime_error("StiPxlHitLoader::loadHits(StEvent*) - FATAL - _hitContainer==0");

   StPxlHitCollection *pxlHitCollection = source->pxlHitCollection();

   if (!pxlHitCollection) {
      LOG_ERROR << "StiPxlHitLoader::loadHits\tERROR:\tcol==0"
                << "You must not have pixelFastSim in your chain"
                << "will return with no action taken" << endm;
      return;
   }

   //Added by Michael Lomnitz (KSU):  Loops over Sector/Ladder/Sensor to obtain the whole hit collection
   int nHit = 0;

   UInt_t numberOfSectors = pxlHitCollection->numberOfSectors();

   for (UInt_t i = 0; i < numberOfSectors; i++)
   {
      StPxlSectorHitCollection *pxlSectorHitCollection = pxlHitCollection->sector(i);
      if ( !pxlSectorHitCollection ) {cout << "No PXLSector hit collection" << endl; return;}

      UInt_t numberOfLadders = pxlSectorHitCollection->numberOfLadders();

      for (UInt_t j = 0; j < numberOfLadders; j++)
      {
         StPxlLadderHitCollection *pxlLadderHitCollection = pxlSectorHitCollection->ladder(j);
         if ( !pxlLadderHitCollection ) {cout << "No PXLLadder hit collection" << endl; return;}

         UInt_t numberOfSensors = pxlLadderHitCollection->numberOfSensors();

         for (UInt_t l = 0; l < numberOfSensors; l++)
         {
            StPxlSensorHitCollection *PxlSensorHitCollection = pxlLadderHitCollection->sensor(l);
            StSPtrVecPxlHit &pxlHits = PxlSensorHitCollection->hits();

            LOG_DEBUG << "StiPxlHitLoader - collection size: " << pxlHits.size() << endm;

            for (unsigned int iPxlHit = 0; iPxlHit < pxlHits.size(); iPxlHit++)
            {
               StPxlHit *pxlHit = pxlHits[iPxlHit];

               if (!pxlHit)
                  throw runtime_error("StiPxlHitLoader::loadHits(StEvent*) -E- NULL hit in container");

               if (pxlHit->detector() != kPxlId) continue;

               // Extract individual sti detector by using the stiRow/stiSensor keys
               int stiRow    = 0;
               int stiSensor = 0;

               if (pxlHit->ladder() == 1) {
                  stiRow = 0 ;
                  stiSensor = (pxlHit->sector()-1) * kNumberOfPxlSensorsPerLadder + (pxlHit->sensor()-1);
               } else {
                  stiRow = 1;
                  stiSensor = (pxlHit->sector()-1) * (kNumberOfPxlLaddersPerSector-1) * kNumberOfPxlSensorsPerLadder
                            + (pxlHit->ladder()-2) * kNumberOfPxlSensorsPerLadder + (pxlHit->sensor()-1);
               }

               LOG_DEBUG << " hit sector : " << (int) pxlHit->sector() << " ladder : " << (int) pxlHit->ladder() << endm;
               LOG_DEBUG << "stiRow: " << stiRow << ", stiSensor: " << stiSensor << endm;
               LOG_DEBUG << "X/Y/Z    : " << pxlHit->position().x() << "/" << pxlHit->position().y() << "/" << pxlHit->position().z() << endm;
               LOG_DEBUG << "Xl/Yl/Zl : " << pxlHit->localPosition(0) << "/" << pxlHit->localPosition(1) << "/" << pxlHit->localPosition(2) << endm;

               StiDetector *detector = _detector->getDetector(stiRow, stiSensor);

               if (!detector)
                  throw runtime_error("StiPxlHitLoader::loadHits(StEvent*) -E- NULL detector pointer");

               LOG_DEBUG << "add hit to detector:\t" << detector->getName() << endm;

               double angle     = detector->getPlacement()->getNormalRefAngle();
               double radius    = detector->getPlacement()->getNormalRadius();
               double zcenter   = detector->getPlacement()->getZcenter();
               double halfDepth = detector->getShape()->getHalfDepth();
               double halfWidth = detector->getShape()->getHalfWidth();
               double thick     = detector->getShape()->getThickness();

               LOG_DEBUG << " detector info " << *detector << endm;
               LOG_DEBUG << " radius = " << radius << " angle = " << angle << " zCenter = " << zcenter << endm;
               LOG_DEBUG << " depth = " << halfDepth << " Width = " << halfWidth << " thickness= " << thick << endm;
               LOG_DEBUG << " key 1 : " << detector->getKey(1) << " key 2 : " << detector->getKey(2) << endm;

               StiHit *stiHit = _hitFactory->getInstance();

               if (!stiHit) throw runtime_error("StiPxlHitLoader::loadHits(StEvent*) -E- stiHit==0");

               stiHit->reset();

               stiHit->setGlobal(detector, pxlHit,
                                 pxlHit->position().x(), pxlHit->position().y(),
                                 pxlHit->position().z(), pxlHit->charge());

               _hitContainer->add(stiHit);
               LOG_DEBUG << " nHit = " << nHit
                  << " Sector = " << (int) pxlHit->sector()
                  << " Ladder = " << (int) pxlHit->ladder()
                  << " x = " << (float) pxlHit->position().x()
                  << " y = " << (float) pxlHit->position().y()
                  << " z = " << (float) pxlHit->position().z() << endm;
               LOG_DEBUG << " " << endm;

               //done loop over hits
               nHit++;
            }
         }
      }
   }

   LOG_INFO << "StiPxlHitLoader:loadHits -I- Loaded " << nHit << " pixel hits." << endm;
}


 /*
 * $Id: StiPxlHitLoader.cxx,v 1.8.2.3 2015/01/07 19:45:34 smirnovd Exp $
 *
 * $Log: StiPxlHitLoader.cxx,v $
 * Revision 1.8.2.3  2015/01/07 19:45:34  smirnovd
 * Squashed commit of changes in StiXxx/ from MAIN CVS branch:
 *
 * Date:   Tue Jan 6 20:57:50 2015 +0000
 *
 *     Reimplemented segmentation of PXL sensor to two halves.
 *
 *     In the sensor's local coordinate system the first half is for x<0 and the second
 *     one is for x>0. The notion of inner and outter halves is not critical and in
 *     fact confusing because it depends on the original rotation around the z axis.
 *     For example, two rotations of phi=5 and phi=-175 give us essentially the same
 *     layer but result in swapped inner and outter halves.
 *
 * Date:   Tue Jan 6 15:48:08 2015 +0000
 *
 *     StiIstHitLoader: Made use of the accessor for sensitive Sti detector/volumes
 *
 * Date:   Tue Jan 6 15:48:04 2015 +0000
 *
 *     StiXxxDetectorBuilder: Added an accessor to access active StiDetectors, i.e. volumes which may have hits associated with them
 *
 * Date:   Tue Jan 6 15:47:58 2015 +0000
 *
 *     StiPxlDetectorBuilder: Switched to method that converts geo sensor id to Sti layer indices
 *
 * Date:   Tue Jan 6 15:47:50 2015 +0000
 *
 *     StiPxlDetectorBuilder: Added a private method to convert natural/geo sensor id to Sti layer indices
 *
 * Date:   Tue Jan 6 15:47:43 2015 +0000
 *
 *     Removed excessive print statements
 *
 * Date:   Tue Jan 6 15:47:34 2015 +0000
 *
 *     Simplified debug output by reusing existing streamers of StHit class and its daughters
 *
 * Date:   Mon Jan 5 15:40:30 2015 +0000
 *
 *     StiIstHitLoader: Cleaned up forward declarations to include only the used classes
 *
 * Date:   Mon Jan 5 15:40:03 2015 +0000
 *
 *     StiXxxHitLoader: Changes in whitespace only
 *
 * Date:   Mon Jan 5 15:39:52 2015 +0000
 *
 *     StiIstHitLoader: Removed useless data members
 *
 * Date:   Fri Dec 19 18:09:01 2014 +0000
 *
 *     Do not set StiDetector members _key1 and _key2 as they are not really used anywhere
 *
 * Date:   Fri Dec 19 18:08:52 2014 +0000
 *
 *     StiXxxDetectorBuilder: Removed output debug messages as they can be easily replaced by a single call to StiDetectorBuilder::Print()
 *
 * Date:   Fri Dec 19 18:08:40 2014 +0000
 *
 *     StiXxxDetectorBuilder: Instead of setting StiDetector parameters in a local private method switched to using new interface provided by StiDetector
 *
 *     The refactoring takes place for both sensitive and inactive volumes
 *
 * Date:   Mon Dec 15 22:18:19 2014 +0000
 *
 *     StiPxlDetectorBuilder: Attempted to make a clear translation between the natural (sector/ladder/sensor) and Sti numbering schemas
 *
 * Date:   Mon Dec 15 22:18:10 2014 +0000
 *
 *     StiIstDetectorBuilder: Increased density of manually constructed IST brackets in Sti.
 *
 *     The effective bracket density have to be muliplied by the number of ladders (24)
 *
 * Revision 1.8.2.2  2014/12/15 21:21:50  qiuh
 * use local x rahter than row number to distribute hits into different half ladders
 *
 * Revision 1.8.2.1  2014/10/30 22:08:11  smirnovd
 * Split PXL sensitive layers in two halves. The change should help to avoid track backward steps in Sti due to ill ordered volumes in r and phi - inspired by Hao Qiu
 *
 * Revision 1.8  2014/06/05 14:28:18  genevb
 * Error about missing hit collection => warning
 *
 * Revision 1.7  2014/04/15 18:46:55  smirnovd
 * Switched to PXL sensitive layer geometry with one Sti volume per ladder centered
 * at z=0. The loop over sensors removed and the indexing of the volumes changed
 * accordingly. Update from Jonathan Bouchet
 *
 * Revision 1.6  2014/03/27 22:48:08  smirnovd
 * Minor improvements in feedback
 *
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
#include "StiPxl/StiPxlDetectorBuilder.h"
#include "StiPxlHitLoader.h"
#include "StPxlUtil/StPxlConstants.h"


StiPxlHitLoader::StiPxlHitLoader()
   : StiHitLoader<StEvent, StiDetectorBuilder>("PixelHitLoader")
{}


StiPxlHitLoader::StiPxlHitLoader(StiHitContainer *hitContainer, Factory<StiHit> *hitFactory,
   StiDetectorBuilder *detector) :
   StiHitLoader<StEvent, StiDetectorBuilder>("PixelHitLoader", hitContainer, hitFactory, detector)
{}


void StiPxlHitLoader::loadHits(StEvent *source, Filter<StiTrack> *trackFilter, Filter<StiHit> *hitFilter)
{
   LOG_INFO << " -I- Started" << endl;

   if (!_detector)
      throw runtime_error("StiPxlHitLoader::loadHits(StEvent*) - FATAL - _detector==0");

   if (!_hitContainer)
      throw runtime_error("StiPxlHitLoader::loadHits(StEvent*) - FATAL - _hitContainer==0");

   StPxlHitCollection *pxlHitCollection = source->pxlHitCollection();

   if (!pxlHitCollection) {
      LOG_WARN << "StiPxlHitLoader::loadHits\tWARN:\t pxlHitCollection not found. "
               << "You may not have StPxlSimMaker or StPxlHitMaker in your chain. "
               << "No pixel hits can be loaded." << endm;
      return;
   }

   //Added by Michael Lomnitz (KSU):  Loops over Sector/Ladder/Sensor to obtain the whole hit collection
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
                  stiRow = 0;

                  if (pxlHit->localPosition(0) > 0)
                     stiRow += 1;

                  stiSensor = (pxlHit->sector() - 1);
               } else {
                  stiRow = 2;

                  if (pxlHit->localPosition(0) < 0)
                     stiRow += 1;

                  stiSensor = (pxlHit->sector() - 1) * (kNumberOfPxlLaddersPerSector - 1) + (pxlHit->ladder() - 1);
               }

               LOG_DEBUG << *pxlHit << "\n"
                         << *static_cast<StMeasuredPoint*>(pxlHit) << endm;

               // The PXL sensitive layers are split into two halves so, access the corresponding
               // sensor half to be later associated with this pxlHit
               int sensorHalf = pxlHit->localPosition(0) < 0 ? 1 : 2;
               const StiDetector *detector = static_cast<StiPxlDetectorBuilder*>(_detector)->getActiveDetector(pxlHit->sector(), pxlHit->ladder(), sensorHalf);

               if (!detector)
                  throw runtime_error("StiPxlHitLoader::loadHits(StEvent*) -E- NULL detector pointer");

               StiHit *stiHit = _hitFactory->getInstance();

               if (!stiHit) throw runtime_error("StiPxlHitLoader::loadHits(StEvent*) -E- stiHit==0");

               stiHit->reset();

               stiHit->setGlobal(detector, pxlHit,
                                 pxlHit->position().x(), pxlHit->position().y(),
                                 pxlHit->position().z(), pxlHit->charge());

               _hitContainer->add(stiHit);
            }
         }
      }
   }
}

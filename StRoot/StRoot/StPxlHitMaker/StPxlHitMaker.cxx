/*!
 * \class StPxlHitMaker
 * \author Qiu Hao, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlHitMaker.cxx,v 1.20 2015/05/14 18:57:52 smirnovd Exp $
 *
 * Author: Qiu Hao, Jan 2013
 **************************************************************************/

#include "StPxlHitMaker.h"
#include "StMessMgr.h"
#include "StEventTypes.h"
#include "TGeoMatrix.h"
#include "StPxlUtil/StThinPlateSpline.h"
#include "StPxlClusterMaker/StPxlCluster.h"
#include "StPxlClusterMaker/StPxlClusterCollection.h"
#include "StPxlUtil/StPxlConstants.h"
#include "StPxlUtil/StPxlDigiHit.h"
#include "tables/St_pxlControl_Table.h"
#include "StPxlDbMaker/StPxlDb.h"

ClassImp(StPxlHitMaker)


StPxlHitMaker::StPxlHitMaker(const Char_t *name) : StMaker(name),
   mPxlDb(0)
{
}


Int_t StPxlHitMaker::InitRun(Int_t runnumber)
{
   TObjectSet *pxlDbDataSet = (TObjectSet*) GetDataSet("pxl_db");

   if (pxlDbDataSet) {
      mPxlDb = (StPxlDb*) pxlDbDataSet->GetObject();
      assert(mPxlDb);
   }
   else {
      LOG_ERROR << "InitRun : not pxlDb" << endm;
      return kStErr;
   }

   return kStOk;
}


/**
 * The input data can be both clusters and pxl hits.
 * If there are already pxl hits, their positions will be recalculated.
 * If there are clusters but no pxl hits collection, a new pxl hit collection will be created.
 * If there are both pxl hits and clusters, new pxl hits from clusters will be added to hits collection.
 * Hit sensor local positions are calculated with the thin plate spline funciton which describe the sensor surface
 * Then global positions are obtained from local positions through rotation + shift by geoHMatrix
 */
Int_t StPxlHitMaker::Make()
{
   Bool_t embeddingShortCut = IAttr("EmbeddingShortCut"); // 1 for embedding, use ideal geometry with no corrections

   StEvent *pEvent = (StEvent*) GetInputDS("StEvent");

   if (!pEvent) {
      LOG_WARN << "StPxlHitMaker::Make(): There is no StEvent " << endm;
      return kStWarn;
   }

   if (!mPxlDb) {
      LOG_WARN << "StPxlHitMaker::Make(): StPxlDb mPxlDb is not initialized" << endm;
      return kStWarn;
   }

   // input pxl cluster collection
   TObjectSet *pxlClusterDataSet = (TObjectSet*) GetDataSet("pxlCluster");
   StPxlClusterCollection *pxlClusterCollection = 0;

   if (pxlClusterDataSet)
      pxlClusterCollection = (StPxlClusterCollection*) pxlClusterDataSet->GetObject();

   // input pxl hit collection
   StPxlHitCollection *pxlHitCollection = pEvent->pxlHitCollection();

   // if no pxl hit collection nor pxl cluster collection, nothing to work on
   if (!pxlClusterCollection && !pxlHitCollection) {
      LOG_WARN << "StPxlHitMaker::Make()  no pxlClusterCollection or pxlHitCollection to work on" << endm;
      return kStWarn;
   }

   // if no pxl hit collection, create one for output
   if (!pxlHitCollection) {
      pxlHitCollection = new StPxlHitCollection();
      pEvent->setPxlHitCollection(pxlHitCollection);
   }

   // loop over the detector
   for (int i = 0; i < kNumberOfPxlSectors; i++)
      for (int j = 0; j < kNumberOfPxlLaddersPerSector; j++)
         for (int k = 0; k < kNumberOfPxlSensorsPerLadder; k++) {
            // add in new hits from clusters
            if (pxlClusterCollection) {
               int vecSize = pxlClusterCollection->numberOfClusters(i + 1, j + 1, k + 1);
               for (int l = 0; l < vecSize; l++) {
                  const StPxlCluster *cluster = pxlClusterCollection->cluster(i + 1, j + 1, k + 1, l);

                  pxlHitCollection->addHit(new StPxlDigiHit(*cluster, i+1, j+1, k+1));
               }
            }

            // Update global coordinates of all hits from sector i, ladder j, and sensor k
            // Also, in case of real data (i.e. not MC or embedding) update the Y coordinate using
            // the thin plane spline correction
            const TGeoHMatrix *geoMSensorOnGlobal = mPxlDb->geoHMatrixSensorOnGlobal(i + 1, j + 1, k + 1);
            int nHitsInSensor = pxlHitCollection->sector(i)->ladder(j)->sensor(k)->hits().size();
            for (int l = 0; l < nHitsInSensor; l++) {
               StPxlHit *pxlHit = pxlHitCollection->sector(i)->ladder(j)->sensor(k)->hits()[l];

               double local[3] = {pxlHit->localPosition()[0], pxlHit->localPosition()[1], pxlHit->localPosition()[2]};

               // apply Tps correction if not embedding
               if (!embeddingShortCut || !pxlHit->idTruth()) {
                  local[1] = mPxlDb->thinPlateSpline(i + 1, j + 1, k + 1)->z(local[2], local[0]); // the Tps x, y, z are sensor local z, x, y respectively
                  pxlHit->setLocalY(local[1]);
               }

               double global[3];
               geoMSensorOnGlobal->LocalToMaster(local, global); // rotation and shift from sensor local to STAR global coordinate
               pxlHit->setPosition(StThreeVectorF(global));
            }
         }

   return kStOK;
}


/***************************************************************************
 *
 * $Log: StPxlHitMaker.cxx,v $
 * Revision 1.20  2015/05/14 18:57:52  smirnovd
 * Squashed commit of the following:
 *
 * StPxlFastSim: Streamlined creation of PXL hits by making use of StPxlUtil/StPxlDigiHit
 *
 * StPxlHitMaker: Updated comments
 *
 * StPxlHitMaker: Streamlined creation of PXL hits by making use of StPxlUtil/StPxlDigiHit
 *
 * StPxlDigiHit: A helper to manipulate local hit position in StPxlHit
 *
 * StPxlConsts: Define constants in namespace
 *
 * For safety reasons, the intentions is to move the constants into the namespace
 * and get rid of those defined in the global space.
 *
 * Revision 1.19  2015/05/14 18:53:50  smirnovd
 * StPxlHitMaker: Removed unused members and local variables
 *
 * These values are set now internaly in the new version of StEvent/StPxlHit
 *
 * Revision 1.18  2015/05/14 18:53:43  smirnovd
 * StPxlHitMaker: Minor stylistic touches to the code
 *
 * Revision 1.17  2015/05/14 18:53:35  smirnovd
 * StPxlHitMaker: Update hit's Y coordinate only when hit is identified as coming from real data otherwise do nothing
 *
 * Do not set the Y coordinate to zero as before. Let the simulation makers decide
 * and set the correct position
 *
 * Revision 1.16  2015/05/14 18:53:28  smirnovd
 * StPxlHitMaker: Only hit's Y coordinate gets updated. The other two are just used as is
 *
 * Revision 1.15  2015/05/14 18:53:18  smirnovd
 * Revert "Bug #3083 fixed. It happened when StPxlHits created in StPxlSimu"
 *
 * This reverts commit 26325ed704a000bd2ff32e237725b3cbbf4f7142.
 *
 * Revision 1.13  2014/05/08 15:10:49  smirnovd
 * PXL DB dataset has been renamed to avoid conflict with StPxlDbMaker's name
 *
 * Revision 1.12  2014/02/07 22:58:30  smirnovd
 * Cosmetic style changes
 *
 * Revision 1.11  2014/02/07 22:58:17  smirnovd
 * Initialize member variables through initialization list
 *
 * Revision 1.10  2014/02/07 22:58:08  smirnovd
 * Added check to validate mPxlDb pointer
 *
 * Revision 1.9  2014/02/07 22:56:39  smirnovd
 * Moved CVS log list to the bottom of file
 *
 * Revision 1.8  2014/02/07 22:38:12  smirnovd
 * Doxygen comments reshuffled
 *
 * Revision 1.7  2014/02/07 14:56:00  smirnovd
 * When a new StPxlHitCollection is created put it in the event right away
 *
 * Revision 1.6  2014/01/28 19:29:40  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

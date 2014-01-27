/*!
 * \class StPxlHitMaker
 * \author Qiu Hao, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlHitMaker.cxx,v 1.5 2014/01/27 02:37:16 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013
 ***************************************************************************
 *
 * Description:
 * Create pxl hits according to clusters and calculate pxl hit global positions.
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlHitMaker.cxx,v $
 * Revision 1.5  2014/01/27 02:37:16  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#include "StPxlHitMaker.h"
#include "StMessMgr.h"
#include "StEventTypes.h"
#include "TGeoMatrix.h"
#include "StPxlUtil/StThinPlateSpline.h"
#include "StPxlClusterMaker/StPxlCluster.h"
#include "StPxlClusterMaker/StPxlClusterCollection.h"
#include "StPxlUtil/StPxlConstants.h"
#include "tables/St_pxlControl_Table.h"
#include "StPxlDbMaker/StPxlDb.h"

ClassImp(StPxlHitMaker)

//________________________________________________________________________________
StPxlHitMaker::StPxlHitMaker(const Char_t *name) : StMaker(name)
{
   mPxlDb = 0;
   mPixelSize = 0;
}
//________________________________________________________________________________
Int_t StPxlHitMaker::InitRun(Int_t runnumber)
{
   TObjectSet *pxlDbDataSet = (TObjectSet *)GetDataSet("pxlDb");
   if (pxlDbDataSet) {
      mPxlDb = (StPxlDb *)pxlDbDataSet->GetObject();
   }
   else {
      LOG_ERROR << "InitRun : not pxlDb" << endm;
      return kStErr;
   }
   mPixelSize = mPxlDb->pxlControl()->pixelSize;

   return kStOk;
}

//________________________________________________________________________________
Int_t StPxlHitMaker::Make()
{
   LOG_INFO << "StPxlHitMaker::Make()" << endm;

   Bool_t embeddingShortCut = IAttr("EmbeddingShortCut"); // 1 for embedding, use ideal geometry with no corrections

   // get StEvent pointer
   StEvent *pEvent = (StEvent *)GetInputDS("StEvent");
   if (! pEvent) {
      LOG_WARN << "StPxlHitMaker::Make there is no StEvent " << endm;
      return kStWarn;
   }

   /// The input data can be both clusters and pxl hits.
   /// If there are already pxl hits, their positions will be recalculated.
   /// If there are clusters but no pxl hits collection, a new pxl hit collection will be created.
   /// If there are both pxl hits and clusters, new pxl hits from clusters will be added to hits collection.

   // input pxl cluster collection
   TObjectSet *pxlClusterDataSet = (TObjectSet *)GetDataSet("pxlCluster");
   StPxlClusterCollection *pxlClusterCollection = 0;
   if (pxlClusterDataSet)
      pxlClusterCollection = (StPxlClusterCollection *)pxlClusterDataSet->GetObject();

   // input pxl hit collection
   StPxlHitCollection *pxlHitCollection = pEvent->pxlHitCollection();

   // if no pxl hit collection nor pxl cluster collection, nothing to work on
   if (!pxlClusterCollection && !pxlHitCollection) {
      LOG_WARN << "StPxlHitMaker::Make()  no pxlClusterCollection or pxlHitCollection to work on" << endm;
      return kStWarn;
   }

   // if no pxl hit collection, create one for output
   if (!pxlHitCollection) pxlHitCollection = new StPxlHitCollection();

   // pixel local x, z at the sensor left lower corner
   double firstPixelZ = -(kNumberOfPxlColumnsOnSensor - 1) * mPixelSize / 2;
   double firstPixelX = (kNumberOfPxlRowsOnSensor - 1) * mPixelSize / 2;

   // loop over the detector
   for (int i = 0; i < kNumberOfPxlSectors; i++)
      for (int j = 0; j < kNumberOfPxlLaddersPerSector; j++)
         for (int k = 0; k < kNumberOfPxlSensorsPerLadder; k++) {
            // add in new hits from clusters
            if (pxlClusterCollection) {
               int vecSize = pxlClusterCollection->numberOfClusters(i + 1, j + 1, k + 1);
               for (int l = 0; l < vecSize; l++) {
                  const StPxlCluster *cluster = pxlClusterCollection->cluster(i + 1, j + 1, k + 1, l);
                  StPxlHit *pxlHit = new StPxlHit();
                  pxlHit->setSector(i + 1);
                  pxlHit->setLadder(j + 1);
                  pxlHit->setSensor(k + 1);
                  pxlHit->setDetectorId(kPxlId);
                  pxlHit->setMeanRow(cluster->rowCenter());
                  pxlHit->setMeanColumn(cluster->columnCenter());
                  pxlHit->setNRawHits(cluster->nRawHits());
                  pxlHit->setIdTruth(cluster->idTruth());

                  pxlHitCollection->addHit(pxlHit);
               }
            }

            // get hit positions
            const TGeoHMatrix *geoMSensorOnGlobal = mPxlDb->geoHMatrixSensorOnGlobal(i + 1, j + 1, k + 1);
            int nHitsInSensor = pxlHitCollection->sector(i)->ladder(j)->sensor(k)->hits().size();
            for (int l = 0; l < nHitsInSensor; l++) {
               StPxlHit *pxlHit = pxlHitCollection->sector(i)->ladder(j)->sensor(k)->hits()[l];
               double local[3];
               double global[3];

               local[2] = firstPixelZ + mPixelSize * pxlHit->meanColumn(); // local z
               local[0] = firstPixelX - mPixelSize * pxlHit->meanRow(); // local x

               // apply Tps correction if not embedding
               if (embeddingShortCut && pxlHit->idTruth())
                  local[1] = 0;
               else
                  local[1] = mPxlDb->thinPlateSpline(i + 1, j + 1, k + 1)->z(local[2], local[0]); // the Tps x, y, z are sensor local z, x, y respectively

               geoMSensorOnGlobal->LocalToMaster(local, global); // rotation and shift from sensor local to STAR global coordinate
               pxlHit->setLocalPosition(local[0], local[1], local[2]);
               StThreeVectorF vecGlobal(global);
               pxlHit->setPosition(vecGlobal);
            }
         }

   if (!pEvent->pxlHitCollection())
      pEvent->setPxlHitCollection(pxlHitCollection);
   return kStOK;
}


/* $Id: StIstHitMaker.cxx,v 1.29 2018/01/04 17:34:38 smirnovd Exp $ */

#include "Stypes.h"
#include "TNamed.h"
#include "TGeoMatrix.h"

#include "StIstHitMaker.h"
#include "StIstUtil/StIstCollection.h"
#include "StIstUtil/StIstCluster.h"
#include "StIstUtil/StIstClusterCollection.h"
#include "StIstHit.h"
#include "StIstHitCollection.h"

#include "St_base/StMessMgr.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StContainers.h"
#include "StEvent/StEnumerations.h"
#include "StIstUtil/StIstConsts.h"

#include "StIstDbMaker/StIstDb.h"
#include "tables/St_istControl_Table.h"

ClassImp(StIstHitMaker);


StIstHitMaker::StIstHitMaker( const char *name ) : StMaker(name), mSensorTransforms(0)
{
}


Int_t StIstHitMaker::InitRun(Int_t runnumber)
{
   TObjectSet *istDbDataSet = (TObjectSet *) GetDataSet("ist_db");
   StIstDb    *istDb = 0;

   if (istDbDataSet) {
      istDb = (StIstDb *) istDbDataSet->GetObject();
      assert(istDb);
   }
   else {
      LOG_ERROR << "InitRun : no istDb" << endm;
      return kStErr;
   }

   // geometry Db tables
   mSensorTransforms = istDb->getRotations();

   return kStOk;
}


/**
 * Takes the StIstClusterCollection from StIstCollection that is normally
 * created by the StIstClusterMaker and fills StEvent's StIstHitCollection which
 * is used in tracking.
 */
Int_t StIstHitMaker::Make()
{
   // Obtain hit collection
   StEvent *eventPtr = (StEvent *) GetDataSet("StEvent");

   if (!eventPtr) {
      LOG_ERROR << "Make() - No StEvent found in the chain. Cannot proceed" << endm;
      return kStErr;
   }

   //input clusters info.
   TObjectSet *istDataSet = (TObjectSet *) GetDataSet("istRawHitAndCluster");

   if (!istDataSet) {
      LOG_WARN << "Make() - istRawHitAndCluster dataset not found. No IST hits will be available for tracking" << endm;
      return kStWarn;
   }

   StIstCollection *istCollectionPtr = (StIstCollection *) istDataSet->GetObject();

   if ( !istCollectionPtr ) {
      LOG_WARN << "Make() - StIstCollection not found. No IST hits will be available for tracking" << endm;
      return kStWarn;
   }

   // Get pointer to an existing StIstHitCollection if any
   StIstHitCollection *istHitCollection = eventPtr->istHitCollection();

   // If no ist hit collection, create one
   if (!istHitCollection) {
      istHitCollection = new StIstHitCollection();
      eventPtr->setIstHitCollection(istHitCollection);
      LOG_DEBUG << "Make() - Added new StIstHitCollection to this StEvent" << endm;
   }

   unsigned char  nClusteringType = -1;

   for (unsigned char ladderIdx = 0; ladderIdx < kIstNumLadders; ++ladderIdx) {
      //add new hits from clusters

      StIstClusterCollection *clusterCollectionPtr = istCollectionPtr->getClusterCollection(ladderIdx );

      if ( clusterCollectionPtr ) {
         unsigned int numClusters = clusterCollectionPtr->getNumClusters();
         LOG_DEBUG << "Make() - Number of clusters found in ladder " << (int)(ladderIdx + 1) << ": " << numClusters << endm;

         unsigned short idTruth = 0;
         unsigned char  nRawHits = -1, nRawHitsZ = -1, nRawHitsRPhi = -1;
         unsigned char  ladder = -1, sensor = -1;
         float  meanRow = 0., meanColumn = 0., charge = 0., chargeErr = 0.;
         unsigned char  maxTb = -1;
         int	 key = -1;

         for (std::vector< StIstCluster * >::iterator clusterIter = clusterCollectionPtr->getClusterVec().begin(); clusterIter != clusterCollectionPtr->getClusterVec().end(); ++clusterIter) {
            idTruth         = (*clusterIter)->getIdTruth();
            key             = (*clusterIter)->getKey();
            ladder          = (*clusterIter)->getLadder();
            sensor          = (*clusterIter)->getSensor();
            meanRow         = (*clusterIter)->getMeanRow();
            meanColumn      = (*clusterIter)->getMeanColumn();
            maxTb           = (*clusterIter)->getMaxTimeBin();
            charge          = (*clusterIter)->getTotCharge();
            chargeErr       = (*clusterIter)->getTotChargeErr();
            nRawHits        = (*clusterIter)->getNRawHits();
            nRawHitsZ       = (*clusterIter)->getNRawHitsZ();
            nRawHitsRPhi    = (*clusterIter)->getNRawHitsRPhi();
            nClusteringType = (*clusterIter)->getClusteringType();

            StIstHit *newHit = new StIstHit(ladder, sensor, charge, chargeErr, maxTb, nRawHits, nRawHitsZ, nRawHitsRPhi);
            newHit->setId(key);
            newHit->setIdTruth(idTruth);

            double local[3];
            local[0] = 0.5 * kIstSensorActiveSizeRPhi - (meanRow - 0.5) * kIstPadPitchRow; //unit: cm
            local[1] = 0.;
            local[2] = (meanColumn - 0.5) * kIstPadPitchColumn - 0.5 * kIstSensorActiveSizeZ; //unit: cm
            newHit->setLocalPosition(local[0], local[1], local[2]); //set local position on sensor

            istHitCollection->addHit(newHit);
         } //cluster loop over
      }//end clusterCollectionPtr

      //set global position
      StIstLadderHitCollection *ladderHitCollection = istHitCollection->ladder(ladderIdx);

      for (int sensorIdx = 0; sensorIdx < kIstNumSensorsPerLadder; sensorIdx++) {
         StIstSensorHitCollection *sensorHitCollection = ladderHitCollection->sensor(sensorIdx);

         for (int idx = 0; idx < (int) sensorHitCollection->hits().size(); idx++ ) {
            StIstHit *newHit = sensorHitCollection->hits()[idx];
            double local[3];
            double global[3];
            local[0] = newHit->localPosition(0);
            local[1] = newHit->localPosition(1);
            local[2] = newHit->localPosition(2);

            int sensorId = 1000 + ((int)newHit->getLadder() - 1) * kIstNumSensorsPerLadder + (int)newHit->getSensor();
            TGeoHMatrix *geoMSensorOnGlobal = (TGeoHMatrix *) mSensorTransforms->FindObject(Form("R%04i", sensorId));
            geoMSensorOnGlobal->LocalToMaster(local, global);
            StThreeVectorF vecGlobal(global);
            newHit->setPosition(vecGlobal); //set global position
         }//end sensor hit collection
      }//end ladder hit collection
   } //ladder loop over

   istHitCollection->setClusteringType(nClusteringType);

   return kStOk;
}


/***************************************************************************
*
* $Log: StIstHitMaker.cxx,v $
* Revision 1.29  2018/01/04 17:34:38  smirnovd
* [Cosmetic] Remove StRoot/ from include path
*
* $STAR/StRoot is already in the default path search
*
* Revision 1.28  2015/01/14 02:29:10  ypwang
* minor update in Make() back to version 1.24
*
* Revision 1.27  2014/10/14 21:06:40  smirnovd
* Updated debug and log messages, added doxygen comments. Also other minor whitespace and style changes
*
* Revision 1.26  2014/10/14 21:06:34  smirnovd
* StIstHitMaker: Arranged checks for required containers and objects in a more logical way. Added corresponding log messages
*
* Revision 1.25  2014/10/14 21:06:29  smirnovd
* StIstHitMaker: No point to proceed with cluster-to-tracking-hit conversion if there are no cluster container for this ladder
*
* Revision 1.24  2014/10/14 21:06:22  smirnovd
* StIstHitMaker: No need to check for valid pointer to StIstCollection for each ladder as it has been already verified at the begining of this Make()
*
* Revision 1.23  2014/10/14 21:05:54  smirnovd
* Don't wait until the end of routine to return error codes. Leads to somewhat cleaner code and eliminates if statements
*
* Revision 1.22  2014/10/13 22:33:04  smirnovd
* Minor adjustments to the code and comments
*
* Revision 1.21  2014/10/13 22:32:57  smirnovd
* StIstHitMaker: Renamed data member to more meaningful name conforming with STAR style
*
* Revision 1.20  2014/10/13 22:28:24  smirnovd
* StIstHitMaker: Use local pointer to StIstDb. No need to have a data member
*
* Revision 1.19  2014/10/13 22:28:11  smirnovd
* Removed pointless methods. ::Init() and ::Finish() do not do much. Data members initialized in constructor
*
* Revision 1.18  2014/10/13 22:28:03  smirnovd
* StIstHitMaker: Corrected style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.17  2014/10/13 22:21:56  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.16  2014/08/14 00:49:10  smirnovd
* StIstHitMaker: Changed the logic in Make(). Don't create a new StEvent if it is not found in the chain, issue an error instead
*
* Revision 1.15  2014/08/12 23:08:09  ypwang
* remove the cluster number cut per ladder, due to chip occupancy cut was added in raw hit maker which can do the bad column rejection
*
* Revision 1.14  2014/08/06 18:56:53  ypwang
* minor update due to coding style update of the StIstDb method
*
* Revision 1.13  2014/07/29 20:13:31  ypwang
* update the IST DB obtain method
*
* Revision 1.12  2014/06/27 21:31:40  ypwang
* remove data member istHitCollection and related Clear() function
*
* Revision 1.11  2014/03/25 03:06:53  ypwang
* updates on Db table accessory method
*
* Revision 1.10  2014/03/24 15:55:08  ypwang
* minor updates due to returned const pointers in StIstDbMaker
*
* Revision 1.9  2014/03/18 02:30:25  ypwang
* minor typo error correction
*
* Revision 1.8  2014/03/17 21:41:49  ypwang
* update to process hit from cluster collection or existed hit collection
*
* Revision 1.7  2014/02/26 01:39:27  ypwang
* minor updates on hit local position setting: meanColumn/meanRow transform to local position here
*
* Revision 1.6  2014/02/25 17:10:55  ypwang
* minor update due to mClusteringType moved to StIstHitCollection
*
* Revision 1.5  2014/02/14 14:47:20  ypwang
* update due to removal of getNumLadders() member function from StIstCollection
*
* Revision 1.4  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstHitMaker.cxx,v 1.0
* Revision 1.0 2013/11/04 16:05:30 Yaping
* Initial version
****************************************************************************/

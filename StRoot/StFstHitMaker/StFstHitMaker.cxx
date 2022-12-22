#include "Stypes.h"
#include "TNamed.h"
#include "TGeoMatrix.h"

#include "StFstHitMaker.h"
#include "StFstUtil/StFstCollection.h"
#include "StFstUtil/StFstCluster.h"
#include "StFstUtil/StFstClusterCollection.h"
#include "StFstHit.h"
#include "StFstHitCollection.h"

#include "St_base/StMessMgr.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StContainers.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StFstConsts.h"

#include "StFstDbMaker/StFstDb.h"
#include "tables/St_fstControl_Table.h"

ClassImp(StFstHitMaker);


StFstHitMaker::StFstHitMaker( const char *name ) : StMaker(name), mSensorTransforms(0)
{}


Int_t StFstHitMaker::InitRun(Int_t runnumber)
{
	TObjectSet *fstDbDataSet = (TObjectSet *) GetDataSet("fst_db");
	StFstDb    *fstDb = 0;

	if (fstDbDataSet) {
		fstDb = (StFstDb *) fstDbDataSet->GetObject();
		assert(fstDb);
	}
	else {
		LOG_ERROR << "InitRun : no fstDb" << endm;
		return kStErr;
	}

	// geometry Db tables
	mSensorTransforms = fstDb->getRotations();

	return kStOk;
}


/**
 * Takes the StFstClusterCollection from StFstCollection that is normally
 * created by the StFstClusterMaker and fills StEvent's StFstHitCollection which
 * is used in tracking.
 */
Int_t StFstHitMaker::Make()
{
	// Obtain hit collection
	StEvent *eventPtr = (StEvent *) GetDataSet("StEvent");

	if (!eventPtr) {
		LOG_ERROR << "Make() - No StEvent found in the chain. Cannot proceed" << endm;
		return kStErr;
	}

	//input clusters info.
	TObjectSet *fstDataSet = (TObjectSet *) GetDataSet("fstRawHitAndCluster");

	if (!fstDataSet) {
		LOG_WARN << "Make() - fstRawHitAndCluster dataset not found. No FST hits will be available for tracking" << endm;
		return kStWarn;
	}

	StFstCollection *fstCollectionPtr = (StFstCollection *) fstDataSet->GetObject();

	if ( !fstCollectionPtr ) {
		LOG_WARN << "Make() - StFstCollection not found. No FST hits will be available for tracking" << endm;
		return kStWarn;
	}

	// Get pointer to an existing StFstHitCollection if any
	StFstHitCollection *fstHitCollection = eventPtr->fstHitCollection();

	// If no fst hit collection, create one
	if (!fstHitCollection) {
		fstHitCollection = new StFstHitCollection();
		eventPtr->setFstHitCollection(fstHitCollection);
		LOG_DEBUG << "Make() - Added new StFstHitCollection to this StEvent" << endm;
	}

	unsigned char  nClusteringType = -1;

	for (unsigned char wedgeIdx = 0; wedgeIdx < kFstNumWedges; ++wedgeIdx) {
		//add new hits from clusters

		StFstClusterCollection *clusterCollectionPtr = fstCollectionPtr->getClusterCollection(wedgeIdx );

		if ( clusterCollectionPtr ) {
			unsigned int numClusters = clusterCollectionPtr->getNumClusters();
			LOG_DEBUG << "Make() - Number of clusters found in wedge " << (int)(wedgeIdx + 1) << ": " << numClusters << endm;

			unsigned short idTruth = 0;
			unsigned char  nRawHits = -1, nRawHitsR = -1, nRawHitsPhi = -1;
			unsigned char  disk = -1, wedge = -1, sensor = -1, apv = -1;
			int  meanRStrip = -1, meanPhiStrip = -1;
                        float  charge = 0., chargeErr = 0.;
			unsigned char  maxTb = -1;
			int	 key = -1;
			float phiInner = -999.9, phiOuter = -999.9;

			for (std::vector< StFstCluster * >::iterator clusterIter = clusterCollectionPtr->getClusterVec().begin(); clusterIter != clusterCollectionPtr->getClusterVec().end(); ++clusterIter) {
				idTruth         = (*clusterIter)->getIdTruth();
				key             = (*clusterIter)->getKey();
				disk            = (*clusterIter)->getDisk();
				wedge           = (*clusterIter)->getWedge();
				sensor          = (*clusterIter)->getSensor();
				apv             = (*clusterIter)->getApv();
				meanRStrip      = (*clusterIter)->getMeanRStrip();
				meanPhiStrip    = (*clusterIter)->getMeanPhiStrip();
				maxTb           = (*clusterIter)->getMaxTimeBin();
				charge          = (*clusterIter)->getTotCharge();
				chargeErr       = (*clusterIter)->getTotChargeErr();
				nRawHits        = (*clusterIter)->getNRawHits();
				nRawHitsR       = (*clusterIter)->getNRawHitsR();
				nRawHitsPhi     = (*clusterIter)->getNRawHitsPhi();
				nClusteringType = (*clusterIter)->getClusteringType();

				StFstHit *newHit = new StFstHit(disk, wedge, sensor, apv, charge, chargeErr, maxTb, meanRStrip, meanPhiStrip, nRawHits, nRawHitsR, nRawHitsPhi);
				newHit->setId(key);
				newHit->setIdTruth(idTruth);

                                int moduleIdx;
				if(disk == 1) // Disk 1
					moduleIdx = wedge;
				else if(disk == 2)// Disk 2
					moduleIdx = wedge-12;
				else if(disk == 3)// Disk 3
					moduleIdx = wedge-24;
				// The simple transformation will be updated with the geomtry table in database later
				if(disk == 1 || disk == 3)
				{// Disk 1 & 3
					phiInner = kFstphiStart[moduleIdx-1]*TMath::Pi()/6.0 + 0.5*kFstzDirct[moduleIdx-1]*kFstStripPitchPhi;
					phiOuter = kFstphiStop[moduleIdx-1]*TMath::Pi()/6.0  - 0.5*kFstzDirct[moduleIdx-1]*kFstStripPitchPhi;
				}
				else if(disk == 2)
				{ // Disk 2
					phiInner = kFstphiStop[moduleIdx-1]*TMath::Pi()/6.0  - 0.5*kFstzDirct[moduleIdx-1]*kFstStripPitchPhi;
					phiOuter = kFstphiStart[moduleIdx-1]*TMath::Pi()/6.0 + 0.5*kFstzDirct[moduleIdx-1]*kFstStripPitchPhi;
				}
				double local[3] = {0};
				if(meanRStrip < kFstNumRStripsPerWedge/2)
				{ // inner
					local[0] = kFstrStart[meanRStrip] + 0.5*kFstStripPitchR;
					local[1] = phiInner + kFstzFilp[disk-1]*kFstzDirct[moduleIdx-1]*meanPhiStrip*kFstStripPitchPhi;
				}
				else
				{// outer
					if(sensor == 1){
						local[0] = kFstrStart[meanRStrip] + 0.5*kFstStripPitchR;
						local[1] = phiOuter - kFstzFilp[disk-1]*kFstzDirct[moduleIdx-1]*meanPhiStrip*kFstStripPitchPhi - kFstzFilp[disk-1]*kFstzDirct[moduleIdx-1]*0.5*kFstStripGapPhi;
					}
					if(sensor == 2){
						local[0] = kFstrStart[meanRStrip] + 0.5*kFstStripPitchR;
						local[1] = phiOuter - kFstzFilp[disk-1]*kFstzDirct[moduleIdx-1]*meanPhiStrip*kFstStripPitchPhi + kFstzFilp[disk-1]*kFstzDirct[moduleIdx-1]*0.5*kFstStripGapPhi;
					}
				}

				if(disk == 1) local[2] = 151.750; //unit: cm
				else if(disk == 2) local[2] = 165.248; //unit: cm
				else if(disk == 3) local[2] = 178.781; //unit: cm
				newHit->setLocalPosition(local[0], local[1], local[2]); //set local position on sensor

				fstHitCollection->addHit(newHit);
			} //cluster loop over
		}//end clusterCollectionPtr

		//set global position
		StFstWedgeHitCollection *wedgeHitCollection = fstHitCollection->wedge(wedgeIdx);
		for (int sensorIdx = 0; sensorIdx < kFstNumSensorsPerWedge; sensorIdx++) {
			StFstSensorHitCollection *sensorHitCollection = wedgeHitCollection->sensor(sensorIdx);

			for (int idx = 0; idx < (int) sensorHitCollection->hits().size(); idx++ ) {
				StFstHit *newHit = sensorHitCollection->hits()[idx];
				double local[3];
				double global[3];
				local[0] = newHit->localPosition(0);
				local[1] = newHit->localPosition(1);
				local[2] = newHit->localPosition(2);

                //Ye: simple transformation. Need to revisit
		// The simple transformation will be updated with the geomtry table in database later
				int sensorId = 1000 + ((int)newHit->getWedge() - 1) * kFstNumSensorsPerWedge + (int)newHit->getSensor();
				TGeoHMatrix *geoMSensorOnGlobal = (TGeoHMatrix *) mSensorTransforms->FindObject(Form("R%04i", sensorId));
				geoMSensorOnGlobal->LocalToMaster(local, global);

                                global[0] = local[0]*cos(local[1]);
                                global[1] = local[0]*sin(local[1]);
                                global[2] = local[2];

				StThreeVectorF vecGlobal(global);
				newHit->setPosition(vecGlobal); //set global position
			}//end sensor hit collection
		}//end wedge hit collection
	} //wedge loop over

	fstHitCollection->setClusteringType(nClusteringType);

	return kStOk;
}

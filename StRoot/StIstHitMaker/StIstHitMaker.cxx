/***************************************************************************
*
* $Id: StIstHitMaker.cxx,v 1.9 2014/03/18 02:30:25 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* See header file.
****************************************************************************
*
* $Log: StIstHitMaker.cxx,v $
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

#include "Stypes.h"
#include "TNamed.h"
#include "TGeoMatrix.h"

#include "StIstHitMaker.h"
#include "StRoot/StIstUtil/StIstCollection.h"
#include "StRoot/StIstUtil/StIstCluster.h"
#include "StRoot/StIstUtil/StIstClusterCollection.h"
#include "StIstHit.h"
#include "StIstHitCollection.h"

#include "StRoot/St_base/StMessMgr.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StContainers.h"
#include "StEvent/StEnumerations.h"
#include "StRoot/StIstUtil/StIstConsts.h"

#include "StRoot/StIstDbMaker/StIstDbMaker.h"
#include "tables/St_istControl_Table.h"

void StIstHitMaker::Clear(Option_t *opts)
{
    if( istHitCollection )
     {
	for(int ladderIdx=0; ladderIdx<kIstNumLadders; ladderIdx++ ) {
          StIstLadderHitCollection* ladderHitCollection = istHitCollection->ladder(ladderIdx);
          for(int sensorIdx=0; sensorIdx<kIstNumSensorsPerLadder; sensorIdx++)   {
              StIstSensorHitCollection* sensorHitCollection = ladderHitCollection->sensor(sensorIdx);
              sensorHitCollection->hits().clear();
	  }
	}
     }      
};

Int_t StIstHitMaker::Make()
{
    Int_t ierr = kStOk;

    //obtain hit collection
    StEvent* eventPtr=0;
    eventPtr= (StEvent*)GetInputDS("StEvent");

    istHitCollection = NULL;
    //input ist hit collection
    if(eventPtr) {
       istHitCollection = eventPtr->istHitCollection();
    } else {
       eventPtr=new StEvent();
       AddData(eventPtr); 
       istHitCollection = eventPtr->istHitCollection();
    }
    
    //input clusters info.
    TObjectSet* istDataSet = (TObjectSet*)GetDataSet("istRawHitAndCluster");
    StIstCollection* istCollectionPtr = 0;
    if(istDataSet) {
	istCollectionPtr = (StIstCollection*)istDataSet->GetObject();
    }

    if( !istCollectionPtr && !istHitCollection ) {
        LOG_WARN << " StIstHitMaker::Make() - no istCollection nor istHitCollection to work on " << endm;
        ierr = kStWarn;
    }

    //if no ist hit collection, create one
    if(!istHitCollection) {
       istHitCollection = new StIstHitCollection();
       eventPtr->setIstHitCollection(istHitCollection);
       LOG_DEBUG <<"StIstHitMaker::Make() has added a non existing StIstHitCollection()"<<endm;
    }

    if(!ierr)   {
	unsigned char  nClusteringType = -1;   
        for(unsigned char ladderIdx=0; ladderIdx<kIstNumLadders; ++ladderIdx)   { 
	    //add new hits from clusters
            StIstClusterCollection *clusterCollectionPtr = 0;
	    if( istCollectionPtr)
		clusterCollectionPtr = istCollectionPtr->getClusterCollection(ladderIdx );

            if( clusterCollectionPtr ){
		unsigned int numClusters = clusterCollectionPtr->getNumClusters();
                if(numClusters<mMinNumOfRawHits)   { // Here mMinNumOfRawHits indicate minimum number of found clusters
                    LOG_WARN <<"no cluster found in ladder " << ladderIdx+1 << "! " <<endl;
                    continue;
                }
                if(numClusters>mMaxNumOfRawHits)   { // Here mMaxNumOfRawHits indicate maximum number of found clusters
                    LOG_WARN <<"too large number of clusters found in ladder " << ladderIdx+1 << "! " <<endl;
                    continue;
                }

                unsigned short idTruth = 0;
		unsigned char  nRawHits = -1, nRawHitsZ = -1, nRawHitsRPhi = -1;
                unsigned char  ladder = -1, sensor = -1;
		float  meanRow = 0., meanColumn = 0., charge = 0., chargeErr = 0.;
		unsigned char  maxTb = -1;
		int	 key = -1;
 
                for(std::vector< StIstCluster* >::iterator clusterIter = clusterCollectionPtr->getClusterVec().begin(); clusterIter != clusterCollectionPtr->getClusterVec().end(); ++clusterIter)   {
                    idTruth 	= (*clusterIter)->getIdTruth();
		    key		= (*clusterIter)->getKey();
                    ladder  	= (*clusterIter)->getLadder();
                    sensor  	= (*clusterIter)->getSensor();
                    meanRow 	= (*clusterIter)->getMeanRow();
                    meanColumn 	= (*clusterIter)->getMeanColumn();
                    maxTb 	= (*clusterIter)->getMaxTimeBin();
		    charge      = (*clusterIter)->getTotCharge();
                    chargeErr	= (*clusterIter)->getTotChargeErr();
                    nRawHits 	= (*clusterIter)->getNRawHits();
                    nRawHitsZ 	= (*clusterIter)->getNRawHitsZ();
                    nRawHitsRPhi= (*clusterIter)->getNRawHitsRPhi();
		    nClusteringType = (*clusterIter)->getClusteringType();

		    StIstHit *newHit = new StIstHit(ladder, sensor, charge, chargeErr, maxTb, nRawHits, nRawHitsZ, nRawHitsRPhi);
                    newHit->setId(key);
		    newHit->setIdTruth(idTruth);
 
		    double local[3];
		    local[0] = 0.5*kIstSensorActiveSizeRPhi - (meanRow-0.5)*kIstPadPitchRow;//unit: cm
                    local[1] = 0.;
		    local[2] = (meanColumn-0.5)*kIstPadPitchColumn - 0.5*kIstSensorActiveSizeZ;//unit: cm
		    newHit->setLocalPosition(local[0], local[1], local[2]); //set local position on sensor

                    istHitCollection->addHit(newHit);
                } //cluster loop over                
            } //end clusterCollectionPtr

	    //set global position
	    StIstLadderHitCollection* ladderHitCollection = istHitCollection->ladder(ladderIdx);
	    for(int sensorIdx=0; sensorIdx<kIstNumSensorsPerLadder; sensorIdx++) {
               	StIstSensorHitCollection* sensorHitCollection = ladderHitCollection->sensor(sensorIdx);
		for(int idx=0; idx<(int)sensorHitCollection->hits().size(); idx++ ){
		    StIstHit *newHit = sensorHitCollection->hits()[idx];
		    double local[3];
		    double global[3];
		    local[0] = newHit->localPosition(0);
		    local[1] = newHit->localPosition(1);
		    local[2] = newHit->localPosition(2);		

	    	    int sensorId = 1000 + ((int)newHit->getLadder()-1) * kIstNumSensorsPerLadder + (int)newHit->getSensor();
            	    TGeoHMatrix *geoMSensorOnGlobal = (TGeoHMatrix*)listGeoMSensorOnGlobal->FindObject(Form("R%04i", sensorId));
	    	    geoMSensorOnGlobal->LocalToMaster(local, global);
		    StThreeVectorF vecGlobal(global);
		    newHit->setPosition(vecGlobal); //set global position
		}//end sensor hit collection
	   }//end ladder hit collection
        } //ladder loop over
	istHitCollection->setClusteringType(nClusteringType);
    } //ierr
 
    return ierr;
};

Int_t StIstHitMaker::Init() {
    Int_t ierr = kStOk;

    mIstDbMaker = (StIstDbMaker*)GetMaker("istDb");

    if(!mIstDbMaker) {
      LOG_WARN << "Error getting IST Db maker handler" << endm;
      ierr = kStWarn;
    }

    return ierr;
}

Int_t StIstHitMaker::InitRun(Int_t runnumber)
{
   Int_t ierr = kStOk;

   // geometry Db tables
   listGeoMSensorOnGlobal = mIstDbMaker->GetRotations();

   // control parameters
   St_istControl *istControl = mIstDbMaker->GetControl();
   if (!istControl)  LOG_WARN << " no istControl table " << endm;
   istControl_st *istControlTable = istControl->GetTable();

   mMinNumOfRawHits = istControlTable[0].kIstMinNumOfRawHits;
   mMaxNumOfRawHits = istControlTable[0].kIstMaxNumOfRawHits;

   return ierr;
};
 
StIstHitMaker::StIstHitMaker( const char* name ) : StMaker(name), listGeoMSensorOnGlobal(0), mIstDbMaker(0), istHitCollection(0)
{
   /* no op */
};

ClassImp(StIstHitMaker);

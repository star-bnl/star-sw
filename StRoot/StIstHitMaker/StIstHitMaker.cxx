/***************************************************************************
*
* $Id: StIstHitMaker.cxx,v 1.1 2014/01/23 20:11:30 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* See header file.
****************************************************************************
*
* $Log: StIstHitMaker.cxx,v $
* Revision 1.1  2014/01/23 20:11:30  ypwang
* adding scripts
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
#include "StRoot/StIstUtil/StIstConsts.h"
#include "StRoot/StIstDbMaker/StIstDbMaker.h"

void StIstHitMaker::Clear(Option_t *opts)
{
    if( istHitCollection )
     {
	for(unsigned int ladderIdx=0; ladderIdx<istHitCollection->numberOfLadders() && ladderIdx<kIstNumLadders; ladderIdx++ ) {
          StIstLadderHitCollection* ladderHitCollection = istHitCollection->ladder(ladderIdx);
          for(unsigned int sensorIdx=0; sensorIdx<ladderHitCollection->numberOfSensors() && sensorIdx<kIstNumSensorsPerLadder; sensorIdx++)   {
              StIstSensorHitCollection* sensorHitCollection = ladderHitCollection->sensor(sensorIdx);
              sensorHitCollection->hits().clear();
	  }
	}
     }      
};

Int_t StIstHitMaker::Make()
{
    Int_t ierr = kStOk;

    //obtain raw hits or clusters info.
    TObjectSet* istDataSet = (TObjectSet*)GetDataSet("istRawHitAndCluster");
    StIstCollection* istCollectionPtr = 0;
    if(istDataSet) {
	istCollectionPtr = (StIstCollection*)istDataSet->GetObject();
    }

    if( !istCollectionPtr) {
        LOG_WARN << " StIstHitMaker::Make() - no istCollection to work on " << endm;
        ierr = kStWarn;
    }

    if(!ierr)   {   
        for(unsigned char ladderIdx=0; ladderIdx<istCollectionPtr->getNumLadders() && ladderIdx<kIstNumLadders; ++ladderIdx)   { 
            StIstClusterCollection *clusterCollectionPtr = istCollectionPtr->getClusterCollection(ladderIdx );
    
            if( clusterCollectionPtr ){
                StIstHit *newHit = 0;
                
                unsigned short idTruth = 0;
		unsigned char  nClusteringType = -1, nRawHits = -1, nRawHitsZ = -1, nRawHitsRPhi = -1;
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
                    charge 	= (*clusterIter)->getTotCharge();
                    chargeErr	= (*clusterIter)->getTotChargeErr();
		    nClusteringType = (*clusterIter)->getClusteringType();
                    nRawHits 	= (*clusterIter)->getNRawHits();
                    nRawHitsZ 	= (*clusterIter)->getNRawHitsZ();
                    nRawHitsRPhi= (*clusterIter)->getNRawHitsRPhi();

		    newHit = new StIstHit(ladder, sensor, charge, chargeErr, maxTb, nRawHits, nRawHitsZ, nRawHitsRPhi, meanColumn, meanRow);
		    newHit->setClusteringType(nClusteringType);
                    newHit->setId(key);
		    newHit->setIdTruth(idTruth);
 
		    int sensorId = 1000 + (ladder-1) * 6 + sensor;
		    TGeoHMatrix *geoMSensorOnGlobal = (TGeoHMatrix*)listGeoMSensorOnGlobal->FindObject(Form("R%04i", sensorId));

		    double local[3];
		    double global[3];
		    local[0] = newHit->localPosition(0); //unit: cm
                    local[1] = -0.; 			 //to be defined later
		    local[2] = newHit->localPosition(2); //unit: cm

		    geoMSensorOnGlobal->LocalToMaster(local, global);
		    StThreeVectorF vecGlobal(global);
		    newHit->setPosition(vecGlobal); //set global position
		    
                    istHitCollection->addHit(newHit);
                } //cluster loop over                
            } //end clusterCollectionPtr
        } //ladder loop over
    } //ierr
 
    return ierr;
};

Int_t StIstHitMaker::Init() {
    Int_t ierr = kStOk;

    StEvent* eventPtr=0;
    eventPtr= (StEvent*)GetInputDS("StEvent");

    istHitCollection = NULL;
    if(eventPtr) {
       istHitCollection = eventPtr->istHitCollection();
    } else {
       eventPtr=new StEvent();
       AddData(eventPtr); 
       istHitCollection = eventPtr->istHitCollection();
    }
    
    if(!istHitCollection) {
       istHitCollection = new StIstHitCollection();
       eventPtr->setIstHitCollection(istHitCollection);
       LOG_DEBUG <<"StIstHitMaker::Make() has added a non existing StIstHitCollection()"<<endm;
    }

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

   listGeoMSensorOnGlobal = mIstDbMaker->GetRotations();

   return ierr;
};
 
StIstHitMaker::StIstHitMaker( const char* name ) : StMaker(name), listGeoMSensorOnGlobal(0), mIstDbMaker(0), istHitCollection(0)
{
   /* no op */
};

StIstHitMaker::~StIstHitMaker()
{
  /* no op */	
};
 
ClassImp(StIstHitMaker);

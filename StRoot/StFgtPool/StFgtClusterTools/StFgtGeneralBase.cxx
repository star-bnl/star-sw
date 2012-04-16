#include "StFgtGeneralBase.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"

#include "StRoot/StMuDSTMaker/COMMON/StMuDst.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtStrip.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtCluster.h"



StFgtGeneralBase::StFgtGeneralBase(const Char_t* name): StMaker( name )
{
  pClusters=new vector<generalCluster>*[6];
  pClusters[0]=&clustersD1;
  pClusters[1]=&clustersD2;
  pClusters[2]=&clustersD3;
  pClusters[3]=&clustersD4;
  pClusters[4]=&clustersD5;
  pClusters[5]=&clustersD6;

  pStrips=new vector<generalStrip>[50]; //for each quad so we don't have to lookup so much...
  StFgtDbMaker *fgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );
  if( !fgtDbMkr ){
    LOG_FATAL << "StFgtDb not provided and error finding StFgtDbMaker" << endm;
  }
    mDb = fgtDbMkr->getDbTables();
    if( !mDb ){
      LOG_FATAL << "StFgtDb not provided and error retrieving pointer from StFgtDbMaker '"
		<< fgtDbMkr->GetName() << endm;
    }
}

Int_t StFgtGeneralBase::Make()
{
  Int_t ierr=kStOk;

  clustersD1.clear();
  clustersD2.clear();
  clustersD3.clear();
  clustersD4.clear();
  clustersD5.clear();
  clustersD6.clear();
  cout << " clearing strips..." <<endl;
  for(int i=0;i<50;i++)
    {
      pStrips[i].clear();
    }
  cout << " done" <<endl;

  //  fillFromStEvent();
  fillFromMuDst();

  return ierr;
}


Int_t StFgtGeneralBase::fillFromStEvent()
{
   Int_t ierr = kStFatal;

   StEvent* eventPtr = 0;
   StFgtCollection *fgtCollectionPtr = 0;
   StFgtHitCollection *fgtHitColPtr = 0;

   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( eventPtr ) {
      fgtCollectionPtr = eventPtr->fgtCollection();

      if( fgtCollectionPtr ){
         // got this far, so flag that this is the right input.
         ierr = kStOk;

         // loop over discs
         for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
            fgtHitColPtr = fgtCollectionPtr->getHitCollection( disc );

            if( fgtHitColPtr ){
               const StSPtrVecFgtHit& hitVec = fgtHitColPtr->getHitVec();
               StSPtrVecFgtHitConstIterator hitIter;

               Int_t idx = 0;
               for( hitIter = hitVec.begin(); hitIter != hitVec.end(); ++hitIter, ++idx )
		 {
		   Short_t quad, disc, strip;
		   Char_t layer; 
		   Double_t posR=(*hitIter)->getPositionR();
		   Double_t posPhi=(*hitIter)->getPositionPhi();
		   Int_t geoId=(*hitIter)->getCentralStripGeoId();
		   Double_t discZ=StFgtGeom::getDiscZ(disc);
		   StFgtGeom::decodeGeoId((*hitIter)->getCentralStripGeoId(),disc, quad, layer, strip);
		   //				  Int_t clusterSizePhi=(*hitIter)->getStripWeightMap().size();
		   Int_t clusterSize=(*hitIter)->getStripWeightMap().size();
		   Double_t clusterCharge=(*hitIter)->charge();
		   pClusters[disc]->push_back(generalCluster(geoId,layer,discZ,posPhi,posR,quad,disc,strip, clusterSize, clusterCharge));
		   mapGeoId2Cluster[geoId]=((pClusters[disc]->size()-1));
		 }
            };
         };
      };
   };

   return ierr;

}

Int_t StFgtGeneralBase::fillFromMuDst()
{
    cout <<"general base, muDST filler" <<endl;
   Int_t ierr = kStFatal;

   // get pointer to input
   const StMuDst* muDst = (const StMuDst*)GetInputDS("MuDst");

   if( muDst ){
      TClonesArray *fgtClusters = muDst->fgtArray( muFgtClusters );
      TClonesArray *fgtStrips=muDst->fgtArray(muFgtStrips);
      if( fgtClusters ){
         // flag this is the correct input
         ierr = kStOk;
	 cout <<" got fgt clust" <<endl;
         Int_t nClusters = fgtClusters->GetEntriesFast();

         for( Int_t i = 0; i < nClusters; ++i ){
            StMuFgtCluster* clus = static_cast< StMuFgtCluster* >( (*fgtClusters)[i] );
            if( clus )
	      {
			cout<<"found cluster in muDST " << endl;
		   Int_t geoId=clus->getCentralStripGeoId();
		   Short_t quad, disc, strip;
		   Char_t layer;
		   StFgtGeom::decodeGeoId(geoId,disc, quad, layer, strip);
		   Double_t posR=clus->getR();
		   Double_t posPhi=clus->getPhi();
		   Double_t discZ=StFgtGeom::getDiscZ(disc);
		   StFgtGeom::decodeGeoId(geoId,disc, quad, layer, strip);
		   Int_t clusterSize=clus->getNumStrips();
		   Double_t clusterCharge=clus->getCharge();
		   //		   cout <<"looking at geoID: " << geoId <<" r: " << posR <<" phi: " << posPhi <<" charge: " << clusterCharge <<" size: " << clusterSize <<endl;
		   //		   cout <<" disc: " << disc <<" quad: " << quad << " layer: " << layer <<endl;
		   pClusters[disc]->push_back(generalCluster(geoId,layer,discZ,posPhi,posR,quad,disc,strip,clusterSize,clusterCharge));
		   mapGeoId2Cluster[geoId]=((pClusters[disc]->size()-1));
	      }
		     //               addClus( i, clus->getCentralStripGeoId(), clus->getR(), clus->getPhi() );
         };
      }
            if(fgtStrips)
      //      if(false)
	{
	  cout <<"got strip " <<endl;
	  ierr = kStOk;
         Int_t nStrips = fgtStrips->GetEntriesFast();
         for( Int_t i = 0; i < nStrips; ++i ){
            StMuFgtStrip* strip = static_cast< StMuFgtStrip* >( (*fgtStrips)[i] );
	    cout <<"got strip" <<endl;
            if( strip )
	      {
		cout <<"got strip again" <<endl;
		Int_t geoId=strip->getGeoId();
		Int_t cSeedType=strip->getClusterSeedType();
		Double_t charge=strip->getCharge();
		Double_t chargeUncert=strip->getChargeUncert();
		Short_t quad, disc, stripI;
		Char_t layer;
		StFgtGeom::decodeGeoId(geoId,disc, quad, layer, stripI);
		Double_t ped=0.0; //get from DB
		Double_t pedErr=0.0; 
		Int_t rdo, arm, apv, chan; 
		mDb->getElecCoordFromGeoId(geoId, rdo,arm,apv,chan);
		Int_t elecId = StFgtGeom::encodeElectronicId( rdo, arm, apv, chan );
		ped = mDb->getPedestalFromElecId( elecId );
		pedErr = mDb->getPedestalSigmaFromElecId( elecId );
		if(quad<2)
		  {
		    pStrips[disc*2+quad].push_back(generalStrip(geoId,ped,pedErr,cSeedType,charge, chargeUncert));
		    for(int j=0;j<7;j++)
		      {
			pStrips[disc*2+quad].back().adc[j]=strip->getAdc(j);
		      }

		    if(mapGeoId2Cluster.find(geoId)!=mapGeoId2Cluster.end())
		      {
			(*pClusters[disc])[mapGeoId2Cluster[geoId] ].centerStripIdx=(pStrips[disc*2+quad].size()-1);

		      }
		  }
		
	      }
	 }
	}
   }
   return ierr;
};

ClassImp(StFgtGeneralBase);

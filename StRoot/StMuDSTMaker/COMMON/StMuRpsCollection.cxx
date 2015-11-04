#include "StMuRpsCollection.h"
#include "StEvent/StRpsCollection.h"
#include "StEvent/StRpsRomanPot.h"
#include "StEvent/StRpsPlane.h"
#include "StEvent/StRpsCluster.h"
#include <algorithm>


ClassImp(StMuRpsCollection)

StMuRpsCollection::StMuRpsCollection(const StRpsCollection & rps){

	// mTrackPoints 	= new TClonesArray( "StMuRpsTrackPoint", 0 );
	// mTracks 		= new TClonesArray( "StMuRpsTrack", 0 );


    mSiliconBunch = rps.siliconBunch();

	for(int i=0;i<mNumberOfRomanPot;i++){

		mNumberPlanesWithClusters[i]  = rps.romanPot(i)->numberOfPlanesWithClusters();
		mStatusRomanPot[i]   = rps.romanPot(i)->status();
		mADC[i][0] = rps.romanPot(i)->adc(0);
		mADC[i][1] = rps.romanPot(i)->adc(1);
		mTAC[i][0] = rps.romanPot(i)->tac(0);
		mTAC[i][1] = rps.romanPot(i)->tac(1);
	 												
		for(int j=0;j<mNumberOfPlanes;j++){

			mOffsetPlane[i][j]  = rps.romanPot(i)->plane(j)->offset();
			mzPlane[i][j]  = rps.romanPot(i)->plane(j)->z();
			mAnglePlane[i][j]  = rps.romanPot(i)->plane(j)->angle();
			mOrientationPlane[i][j]  = rps.romanPot(i)->plane(j)->orientation();		
			mStatusPlane[i][j]  = rps.romanPot(i)->plane(j)->status();	
			mNumberOfClusters[i][j]  = rps.romanPot(i)->plane(j)->numberOfClusters();
			
			for(int k=0;k<mNumberOfClusters[i][j];k++){
			
				mPositionCluster[i][j].push_back(rps.romanPot(i)->plane(j)->cluster(k)->position());
				mPositionRMSCluster[i][j].push_back(rps.romanPot(i)->plane(j)->cluster(k)->positionRMS());
				mLengthCluster[i][j].push_back(rps.romanPot(i)->plane(j)->cluster(k)->length());
				mEnergyCluster[i][j].push_back(rps.romanPot(i)->plane(j)->cluster(k)->energy());				
				mXYCluster[i][j].push_back(rps.romanPot(i)->plane(j)->cluster(k)->xy());				
				mQualityCluster[i][j].push_back(rps.romanPot(i)->plane(j)->cluster(k)->quality());		

			}
		}
	}

	// Need to mirror over the StRpsTrackPoint
	int nTracks = rps.tracks().size();
	for ( int iTrack = 0; iTrack < nTracks; iTrack++ ){
		StMuRpsTrack * muRpsTrack = new StMuRpsTrack();

		// The track points are taken from the StRpsTrack
		for ( int iStation = 0; iStation < StMuRpsTrack::mNumberOfStationsInBranch; iStation++ ){
			if ( rps.tracks()[iTrack] && rps.tracks()[iTrack]->trackPoint( iStation ) ){
				StMuRpsTrackPoint * ptp = addTrackPoint( rps.tracks()[iTrack]->trackPoint( iStation ) );
				muRpsTrack->setTrackPoint( ptp, iStation );
			}
		}
		muRpsTrack->setP( TVector3( rps.tracks()[iTrack]->pVec().x(), rps.tracks()[iTrack]->pVec().y(), rps.tracks()[iTrack]->pVec().z() ) );
		muRpsTrack->setBranch( rps.tracks()[iTrack]->branch() );
		muRpsTrack->setType( (StMuRpsTrack::StMuRpsTrackType)rps.tracks()[iTrack]->type() );

		mTracks.push_back( muRpsTrack );
	}
}



StMuRpsTrackPoint* StMuRpsCollection::addTrackPoint( StRpsTrackPoint * rpsTP ){

	StMuRpsTrackPoint * muRpsTrackPoint = new StMuRpsTrackPoint();


	muRpsTrackPoint->setPosition( TVector3( rpsTP->x(), rpsTP->y(), rpsTP->z() ) );
	muRpsTrackPoint->setQuality( (StMuRpsTrackPoint::StMuRpsTrackPointQuality)rpsTP->quality() );
	muRpsTrackPoint->setRpId( rpsTP->rpId() );

	for ( int iPlane = 0; iPlane < StMuRpsTrackPoint::mNumberOfPlanesInRp; iPlane++ ){
		muRpsTrackPoint->setClusterId( rpsTP->clusterId( iPlane ), iPlane );
	}

	for ( int iPmt = 0; iPmt < StMuRpsTrackPoint::mNumberOfPmtsInRp; iPmt++ ){
		muRpsTrackPoint->setTime( rpsTP->time( iPmt ), iPmt );
	}

	mTrackPoints.push_back( muRpsTrackPoint );
	return muRpsTrackPoint;
}

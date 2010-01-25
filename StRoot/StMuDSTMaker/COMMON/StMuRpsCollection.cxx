#include "StMuRpsCollection.h"
#include "StEvent/StRpsCollection.h"
#include "StEvent/StRpsRomanPot.h"
#include "StEvent/StRpsPlane.h"
#include "StEvent/StRpsCluster.h"

ClassImp(StMuRpsCollection)

StMuRpsCollection::StMuRpsCollection(const StRpsCollection & rps){


/*	memset( mNumberPlanes, 0 ,  sizeof(mNumberPlanes));
	memset( mNumberPlanesWithClusters , 0,  sizeof(mNumberPlanesWithClusters));
	memset( mStatusRomanPot , 0,  sizeof(mStatusRomanPot));
	memset( mADC , 0,  sizeof(mADC));
	memset( mTAC, 0	,  sizeof(mTAC));

	memset( mOffsetPlane, 0 , sizeof(mOffsetPlane));
	memset( mzPlane, 0 , sizeof(mzPlane));
	memset( mAnglePlane, 0, sizeof(mAnglePlane));
	memset( mOrientationPlane, 0,  sizeof(mOrientationPlane));
	memset( mStatusPlane, 0, sizeof(mStatusPlane));
	memset( mNumberOfClusters, 0, sizeof(mNumberOfClusters));

	memset( mPositionCluster, 0, sizeof(mPositionCluster));
	memset( mLengthCluster, 0, sizeof(mLengthCluster));
	memset( mEnergyCluster, 0, sizeof(mEnergyCluster));
	memset( mXYCluster, 0, sizeof(mXYCluster));
	memset(mQualityCluster,0 , sizeof(mQualityCluster));*/

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
				mLengthCluster[i][j].push_back(rps.romanPot(i)->plane(j)->cluster(k)->length());
				mEnergyCluster[i][j].push_back(rps.romanPot(i)->plane(j)->cluster(k)->energy());				
				mXYCluster[i][j].push_back(rps.romanPot(i)->plane(j)->cluster(k)->xy());				
				mQualityCluster[i][j].push_back(rps.romanPot(i)->plane(j)->cluster(k)->quality());		

	/*			mPositionCluster[i][j][k] = rps.romanPot(i)->plane(j)->cluster(k)->position();
				mLengthCluster[i][j][k] = rps.romanPot(i)->plane(j)->cluster(k)->length();
				mEnergyCluster[i][j][k] = rps.romanPot(i)->plane(j)->cluster(k)->energy();				
				mXYCluster[i][j][k] =  rps.romanPot(i)->plane(j)->cluster(k)->xy();				
				mQualityCluster[i][j][k] = rps.romanPot(i)->plane(j)->cluster(k)->quality();*/		
				
//				cout<<"Cluster position "<<rps.romanPot(i)->plane(j)->cluster(k)->position()<<endl;
				
			}
		}
	}

}

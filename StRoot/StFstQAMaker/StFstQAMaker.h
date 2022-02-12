/***************************************************************************
*
* $Id: StFstQAMaker.h,v$
*
* Author: Shenghui Zhang, Nov. 2021
****************************************************************************
* Description: 
* Generate several histograms and trees for FST raw hits and hits.
****************************************************************************
* StFstQAMaker.h,v 1.0
* Revision 1.0 2021/11/1 Shenghui Zhang
* Initial version
****************************************************************************/

#ifndef StFstQAMaker_hh     
#define StFstQAMaker_hh

#include <string>

#include "StMaker.h"
#include "StEvent/StFstConsts.h"

class TH2F;
class TH2S;
class TProfile;
class TTree;

class StFstQAMaker : public StMaker {
public:
  StFstQAMaker(const char *name="fst_Qa");     // constructor
  Int_t Init();
  Int_t Make();                      		// invoked for every event
  Int_t Finish();                    		// called once at the end

  struct rawHitInfo {
	int channelId, geoId, wedge, sensor, phistrip, rstrip, maxTimeBin, rdo, arm, apv, channel, idTruth, seedHitFlag, EventId;
	float charge[kFstNumTimeBins], chargeErr[kFstNumTimeBins];
  } fstRawHit;
  
  struct hitInfo {
	int hitId, wedge, sensor, apv, idTruth, EventId, maxTimeBin, clusteringType, nRawHits, nRawHitsR, nRawHitsPhi;
	float meanPhiStrip, meanRStrip, localR, localPhi, localZ, x, y, z, charge, chargeErr;
  } fstHit;

protected:
  // TFile* myRootFile;
  TTree *fstRawHitTree;
  TTree *fstHitTree;

  //position
  TH2S* rawHitMap[kFstNumSensors];  // raw hit phistrip vs. rstrip per sensor
  TH2S* hitMap[kFstNumSensors];     // hit mean phistrip vs. mean rstrip per sensor
  TH2S* hitMapOfFST[kFstNumDisk];   // hit map in r vs. phi per disk
  TH2S* hitMapOfAPV[kFstNumDisk];   // hit map in APV geometry Id vs. module geometry Id per disk
  TH2F* hitGlobalXY[kFstNumDisk];   // hit global x vs. y per disk
  TH2F* hitGlobalRPhi[kFstNumDisk]; // hit global r vs. phi per disk
  //Charge
  TH2S* rawHitCharge_TimeBin[kFstNumTimeBins]; // Charge (ADC) vs channel ID over all time bins
  TH2S* rawHitChargeErr;           // RMS noise vs channel ID
  TH2S* rawHitMaxTimeBin_APV;      // Raw hit max ADC time bin vs APV electronics ID [48*(ARC-1)+16*ARM+APV]
  TH2S* hitCharge_SensorId;        // Charge vs sensorID
  TH2S* hitChargeErr_SensorId;     // Charge uncertainty vs sensorID
  TH2S* maxTimeBin_SensorId;	   // hit max ADC time bin vs sensorID
  //hit or raw hit number
  TH2S* numOfRawHits_SensorId;     // number of raw hits vs sensor Id
  TH2S* numOfHits_SensorId;	   // number of hits vs sensor Id
  TProfile* numOfRawHits_EventId[kFstNumSensors];
  //cluster size
  TH2S* clusterSize_SensorId;	   // hit cluster size
  TH2S* clusterSizeR_SensorId;	   // hit cluster size in R direction
  TH2S* clusterSizePhi_SensorId;   // hit cluster size in Phi direction

  Int_t  mEventCounter;            // Event countter
  Bool_t mDoTreeOutput;

private:
  ClassDef(StFstQAMaker,0);
};
#endif

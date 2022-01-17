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
#include "StIOMaker/StIOMaker.h"
#include "StEvent/StEnumerations.h"
#include "StFstUtil/StFstConsts.h"
#include "TString.h"
#include "TNtuple.h"
#include "TH2F.h"
#include "TProfile.h"

//using namespace StFstConsts;

class StIOMaker;
class StEvent;
class StTrack;

class StFstQAMaker : public StMaker {
public:
  StFstQAMaker(const char *name="fst_Qa");     // constructor
  Int_t Init();
  Int_t  Make();                      		// invoked for every event
  Int_t  Finish();                    		// called once at the end
  /*virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StFstQAMaker.h,v 1.8 2014/03/18 02:19:37 ypwang Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  };*/

protected:
  TFile* myRootFile;
  TTree *fstRawHitTree;
  TTree *fstHitTree;

  struct rawHitInfo {
	int channelId, geoId, wedge, sensor, phistrip, rstrip, maxTimeBin, rdo, arm, apv, channel, idTruth, EventId;
	float charge[kFstNumTimeBins], chargeErr[kFstNumTimeBins];
  } fstRawHit;
  
  struct hitInfo {
	int hitId, wedge, sensor, apv, idTruth, EventId, maxTimeBin, clusteringType, nRawHits, nRawHitsR, nRawHitsPhi;
	float meanPhiStrip, meanRStrip, localR, localPhi, localZ, x, y, z, charge, chargeErr;
  } fstHit;

  //position
  TH2S* rawHitMap[kFstNumSensors]; // raw hit phistrip vs rstrip per sensor
  TH2S* hitMap[kFstNumSensors];    // hit mean phistrip vs mean rstrip per sensor
  TH2S* hitMapOfFST;		   // hit map of FST layer in r-phi vs. Z area
  TH2S* hitMapOfAPV;		   // hit map in wedge vs. APV geometry Id
  TH2F* hitGlobalXY;		   // hit global x vs y
  TH2F* hitGlobalPhiZ;		   // hit global z
  //Charge
  TH2S* rawHitCharge_TimeBin[kFstNumTimeBins]; // Charge (ADC) vs channel ID over all time bins
  TH2S* rawHitChargeErr;  // RMS noise vs channel ID
  TH2S* rawHitMaxTimeBin_APV;       // Raw hit max ADC time bin vs APV electronics ID [48*(ARC-1)+16*ARM+APV]
  TH2S* hitCharge_SensorId;         // Charge vs sensorID
  TH2S* hitChargeErr_SensorId;      // Charge uncertainty vs sensorID
  TH2S* maxTimeBin_SensorId;	    // hit max ADC time bin vs sensorID
  //hit or raw hit number
  TH2S* numOfRawHits_SensorId;      // number of raw hits vs sensor Id
  TH2S* numOfHits_SensorId;	    // number of hits vs sensor Id
  TProfile* numOfRawHits_EventId[kFstNumSensors];
  //cluster size
  TH2S* clusterSize_SensorId;	    // hit cluster size
  TH2S* clusterSizeR_SensorId;	    // hit cluster size in R direction
  TH2S* clusterSizePhi_SensorId;    // hit cluster size in Phi direction

  Int_t  mEventCounter;             // Event countter

private:
  ClassDef(StFstQAMaker,1);
};
#endif

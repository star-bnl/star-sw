/***************************************************************************
*
* $Id: StFstQAMaker.cxx,v $
*
* Author: Shenghui Zhang, Nov. 2021
****************************************************************************
* Description: 
* See header file.
****************************************************************************
* StFstQAMaker.cxx,v 1.0
* Revision 1.0 2021/11/01 Shenghui Zhang
* Initial version
****************************************************************************/

#include "StFstQAMaker.h"
#include "StFstHitCollection.h"
#include "StFstHit.h"
#include "StRoot/StFstUtil/StFstCollection.h"
#include "StRoot/StFstUtil/StFstRawHitCollection.h"
#include "StEvent/StFstRawHit.h"
#include "StIOMaker/StIOMaker.h"
#include "StEvent.h"
#include "StTrack.h"

#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StDcaGeometry.h"
#if ROOT_VERSION_CODE < 334081
#include "TArrayL.h"
#else
#include "TArrayL64.h"
#endif
#include "TClassTable.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TTree.h"
#include "TFile.h"
#include "TString.h"
#include "StThreeVectorF.hh"
#include "StDetectorName.h"

#include <string>

// constructor
StFstQAMaker::StFstQAMaker( const char* name ) :
   StMaker(name), mEventCounter(0), mDoTreeOutput(false) {
   for(int iSensor=0; iSensor<kFstNumSensors; iSensor++) {
	rawHitMap[iSensor]            = NULL;
	hitMap[iSensor]               = NULL;
	numOfRawHits_EventId[iSensor] = NULL;
   }

   for(int iDisk = 0; iDisk < kFstNumDisk; iDisk++) {
     hitMapOfFST[iDisk]   = NULL;
     hitMapOfAPV[iDisk]   = NULL;
     hitGlobalXY[iDisk]   = NULL;
     hitGlobalRPhi[iDisk] = NULL;
   }

   for(unsigned char iTimeBin=0; iTimeBin<kFstNumTimeBins; iTimeBin++)
    	rawHitCharge_TimeBin[iTimeBin] = NULL;

   rawHitChargeErr         = NULL;
   rawHitMaxTimeBin_APV    = NULL;
   hitCharge_SensorId      = NULL;
   hitChargeErr_SensorId   = NULL;
   maxTimeBin_SensorId     = NULL;
   numOfRawHits_SensorId   = NULL;
   numOfHits_SensorId      = NULL;
   clusterSize_SensorId    = NULL;
   clusterSizeR_SensorId   = NULL;
   clusterSizePhi_SensorId = NULL;
}

// initialize
Int_t StFstQAMaker::Init()
{
    Int_t ierr = kStOk;

    fstRawHitTree = new TTree("fstRawHits", "fstRawHits_QA");
    fstRawHitTree->Branch("rawHits", &fstRawHit, "channelId/I:geoId:wedge:sensor:phistrip:rstrip:maxTimeBin:rdo:arm:apv:channel:idTruth:seedHitFlag:EventId:charge[9]/F:chargeErr[9]/F");

    fstHitTree = new TTree("fstHits", "fstHits_QA");
    fstHitTree->Branch("hits", &fstHit, "hitId/I:wedge/I:sensor/I:apv/I:idTruth/I:EventId/I:maxTimeBin/I:clusteringType/I:nRawHits/I:nRawHitsR/I:nRawHitsPhi/I:meanPhiStrip/F:meanRStrip/F:localR/F:localPhi/F:localZ/F:x/F:y/F:z/F:charge/F:chargeErr/F");

    numOfRawHits_SensorId = new TH2S("numOfRawHits_SensorId", "The number of RawHits vs. sensor ID", 108, 0, 108, 128, 0, 128);
    numOfRawHits_SensorId->GetXaxis()->SetTitle("Sensor ID");
    numOfRawHits_SensorId->GetYaxis()->SetTitle("Number of Raw Hits");

    rawHitMaxTimeBin_APV = new TH2S("rawHitMaxTimeBin_APV", "Max time bin of raw hits vs APV ID", 288, 0, 288, kFstNumTimeBins, 0, kFstNumTimeBins);
    rawHitMaxTimeBin_APV->GetXaxis()->SetTitle("APV ID [48*(RDO-1)+16*ARM+APV]");
    rawHitMaxTimeBin_APV->GetYaxis()->SetTitle("Max Time Bin Index");

    char buffer[100];
    for(int iTimeBin=0; iTimeBin<kFstNumTimeBins; iTimeBin++) {
	sprintf(buffer, "rawHitCharge_TimeBin%d", iTimeBin);
	rawHitCharge_TimeBin[iTimeBin] = new TH2S(buffer, Form("ADC of raw hits at time bin %d vs. channel geometry ID",iTimeBin), 288, 0, 36864, 512, 0, kFstMaxAdc);
	rawHitCharge_TimeBin[iTimeBin]->GetXaxis()->SetTitle("Channel ID");
	rawHitCharge_TimeBin[iTimeBin]->GetYaxis()->SetTitle("ADC of Raw Hits");
    }

    rawHitChargeErr = new TH2S("rawHitChargeErr", "RMS noise of raw hits vs. channel geometry ID", 288, 0, 36864, 128, 0, 64);
    rawHitChargeErr->GetXaxis()->SetTitle("Channel ID");
    rawHitChargeErr->GetYaxis()->SetTitle("RMS noise of Raw Hits");

    numOfHits_SensorId = new TH2S("numOfHits_SensorId", "The number of hits vs. sensor ID", 108, 0, 108, 128, 0, 128);
    numOfHits_SensorId->GetXaxis()->SetTitle("Sensor ID");
    numOfHits_SensorId->GetYaxis()->SetTitle("Number of Hits");

    hitCharge_SensorId = new TH2S("hitCharge_SensorId", "ADC of hits vs. sensor ID", 108, 0, 108, 512, 0, kFstMaxAdc);
    hitCharge_SensorId->GetXaxis()->SetTitle("Sensor ID");
    hitCharge_SensorId->GetYaxis()->SetTitle("ADC of Hits");

    hitChargeErr_SensorId = new TH2S("hitChargeErr_SensorId", "RMS noise of hits vs. sensor ID", 108, 0, 108, 128, 0, 64);
    hitChargeErr_SensorId->GetXaxis()->SetTitle("Sensor ID");
    hitChargeErr_SensorId->GetYaxis()->SetTitle("RMS noise of Hits");

    maxTimeBin_SensorId = new TH2S("maxTimeBin_SensorId", "Max time bin of hits vs. sensor ID", 108, 0, 108, kFstNumTimeBins, 0, kFstNumTimeBins);
    maxTimeBin_SensorId->GetXaxis()->SetTitle("Sensor ID");
    maxTimeBin_SensorId->GetYaxis()->SetTitle("Max Time Bin Index");

    clusterSize_SensorId = new TH2S("clusterSize_SensorId", "Cluster size of hits vs. sensor ID", 108, 0, 108, 20, 0, 20);
    clusterSize_SensorId->GetXaxis()->SetTitle("Sensor ID");
    clusterSize_SensorId->GetYaxis()->SetTitle("Cluster Size of Hits");

    clusterSizeR_SensorId = new TH2S("clusterSizeR_SensorId", "Cluster size in R of hits vs. sensor ID", 108, 0, 108, 20, 0, 20);
    clusterSizeR_SensorId->GetXaxis()->SetTitle("Sensor ID");
    clusterSizeR_SensorId->GetYaxis()->SetTitle("Cluster Size in R of Hits");

    clusterSizePhi_SensorId = new TH2S("clusterSizePhi_SensorId", "Cluster size in #phi of hits vs. sensor ID", 108, 0, 108, 20, 0, 20);
    clusterSizePhi_SensorId->GetXaxis()->SetTitle("Sensor ID");
    clusterSizePhi_SensorId->GetYaxis()->SetTitle("Cluster Size in #phi of hits");

    for(int iDisk = 0; iDisk < kFstNumDisk; ++iDisk)
    {
      TString HistName;
      TString HistTitle;

      HistName = Form("hitMapOfFST_Disk%d",iDisk);
      HistTitle = Form("FST hit map in r-phi for Disk%d",iDisk+1);
      hitMapOfFST[iDisk] = new TH2S(HistName.Data(), HistTitle.Data(), kFstNumPhiSegPerWedge*kFstNumWedgePerDisk, 0, kFstNumPhiSegPerWedge*kFstNumWedgePerDisk, kFstNumRStripsPerWedge, 0, kFstNumRStripsPerWedge);
      hitMapOfFST[iDisk]->GetXaxis()->SetTitle("PhiStrip");
      hitMapOfFST[iDisk]->GetYaxis()->SetTitle("RStrip");

      HistName = Form("hitMapOfAPV_Disk%d",iDisk);
      HistTitle = Form("FST hit map in APV geometry Id vs. wedge for Disk%d",iDisk+1);
      hitMapOfAPV[iDisk] = new TH2S(HistName.Data(), HistTitle.Data(), kFstNumWedgePerDisk, 1, kFstNumWedgePerDisk+1, kFstApvsPerWedge, 0, kFstApvsPerWedge);
      hitMapOfAPV[iDisk]->GetXaxis()->SetTitle("Wedge ID");
      hitMapOfAPV[iDisk]->GetYaxis()->SetTitle("APV geometry ID");

      HistName = Form("hitGlobalXY_Disk%d",iDisk);
      HistTitle = Form("Global X vs. Global Y for Disk%d",iDisk+1);
      hitGlobalXY[iDisk] = new TH2F(HistName.Data(), HistTitle.Data(), 140, -35, 35, 140, -35, 35);
      hitGlobalXY[iDisk]->GetXaxis()->SetTitle("Global X [cm]");
      hitGlobalXY[iDisk]->GetYaxis()->SetTitle("Global Y [cm]");

      HistName = Form("hitGlobalRPhi_Disk%d",iDisk);
      HistTitle = Form("Global #phi vs. Global r for Disk%d",iDisk+1);
      hitGlobalRPhi[iDisk] = new TH2F(HistName.Data(), HistTitle.Data(), kFstNumPhiSegPerWedge*kFstNumWedgePerDisk/8, -TMath::Pi(), TMath::Pi(), kFstNumRStripsPerWedge, 5, 28);
      hitGlobalRPhi[iDisk]->GetXaxis()->SetTitle("Global #phi [rad.]");
      hitGlobalRPhi[iDisk]->GetYaxis()->SetTitle("Global r [cm]");
    }

    char histtitle[128];
    for(int iWedge=0; iWedge<kFstNumWedges; iWedge++) {
	for(int iSensor=0; iSensor<kFstNumSensorsPerWedge; iSensor++) {
	    sprintf(histtitle, "Raw Hit phistrip vs. rstrip: Wedge %d Sensor %d", iWedge+1, iSensor);
	    sprintf(buffer,"rawHitMap_Sensor%d", iWedge*3+iSensor+1);
	    rawHitMap[iWedge*3+iSensor] = new TH2S(buffer, histtitle, 128, 0, 128, 8, 0, 8);
	    rawHitMap[iWedge*3+iSensor]->GetXaxis()->SetTitle("PhiStrip");
	    rawHitMap[iWedge*3+iSensor]->GetYaxis()->SetTitle("RStrip");

	    sprintf(histtitle, "Number of raw hits vs. EventID: Wedge %d Sensor %d", iWedge+1, iSensor);
            sprintf(buffer,"numOfRawHitsVsEventId_Sensor%d", iWedge*3+iSensor+1);
	    numOfRawHits_EventId[iWedge*3+iSensor] = new TProfile(buffer, histtitle, 10000, 0, 10000);
            numOfRawHits_EventId[iWedge*3+iSensor]->GetXaxis()->SetTitle("EventID (Time)");
            numOfRawHits_EventId[iWedge*3+iSensor]->GetYaxis()->SetTitle("<rawhits>");

	    sprintf(histtitle, "Hit mean phistrip vs. mean rstrip: Wedge %d Sensor %d", iWedge+1, iSensor);
            sprintf(buffer,"hitMap_Sensor%d", iWedge*3+iSensor+1);
            hitMap[iWedge*3+iSensor] = new TH2S(buffer, histtitle, 128, 0, 128, 8, 0, 8);
            hitMap[iWedge*3+iSensor]->GetXaxis()->SetTitle("Mean PhiStrip");
            hitMap[iWedge*3+iSensor]->GetYaxis()->SetTitle("Mean RStrip");
	}		
    }
    return ierr;
}

Int_t StFstQAMaker::Make(){
   Int_t ierr = kStOk;

   //hit data input
   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( !eventPtr ) {
      LOG_WARN << "StFstQAMaker::Make : Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
      ierr = kStWarn;
   }

   StFstHitCollection* fstHitCollection = 0;
   if( eventPtr ) {
      fstHitCollection = eventPtr->fstHitCollection();
   }

   if( !fstHitCollection) {
      LOG_WARN << "StFstQAMaker::Make : Error getting pointer to StFstHitCollection from '" << ClassName() << "'" << endm;
      ierr = kStWarn;
   }

   //raw hit data input
    TObjectSet* fstDataSet = (TObjectSet*)GetDataSet("fstRawHitAndCluster");
    if (!fstDataSet) {
        LOG_WARN << "StFstQAMaker::Make() - there is no fstDataSet (raw hit) " << endm;
        ierr = kStWarn;
    }        
          
    StFstCollection* fstCollectionPtr = (StFstCollection*)fstDataSet->GetObject();
    if(!fstCollectionPtr) {
        LOG_WARN << "StFstQAMaker::Make() - no fstCollection."<<endm;
        ierr = kStWarn;
    }

   //*******Initialization of the raw hit and hit level variables********** 
   fstRawHit.channelId = fstRawHit.geoId = fstRawHit.wedge = fstRawHit.sensor = fstRawHit.phistrip = fstRawHit.rstrip = fstRawHit.maxTimeBin = fstRawHit.rdo = fstRawHit.arm = fstRawHit.apv = fstRawHit.channel = fstRawHit.idTruth = fstRawHit.seedHitFlag = fstRawHit.EventId = -1;
   for(int iTB=0; iTB<kFstNumTimeBins; iTB++) {
	fstRawHit.charge[iTB] = 0.;
	fstRawHit.chargeErr[iTB] = 0.;
   }
   fstHit.hitId = fstHit.wedge = fstHit.sensor = fstHit.apv = fstHit.idTruth = fstHit.EventId = fstHit.maxTimeBin = fstHit.clusteringType = fstHit.nRawHits = fstHit.nRawHitsR = fstHit.nRawHitsPhi = fstHit.meanPhiStrip = fstHit.meanRStrip = fstHit.localZ = -1;
   fstHit.x = fstHit.y = fstHit.z = fstHit.charge = fstHit.chargeErr = 0.; 

   //loop
   if( !ierr ){
      StPrimaryVertex *primaryVertex = eventPtr->primaryVertex();
      if(primaryVertex) {
          const StThreeVectorF &primXYZ = primaryVertex->position();
          LOG_DEBUG << "primaryVertex \t" << primXYZ.x() << "\t" << primXYZ.y() << "\t" << primXYZ.z() << endm;
      }
      else{
          LOG_DEBUG << "no primaryVertex found" << endm;
      }
      
      if(mEventCounter%100 == 0)
      	  LOG_DEBUG << "event index: " << mEventCounter << endm; 

      if(fstHitCollection->numberOfHits() > 0) {
         for(int wedgeIdx=0; wedgeIdx<kFstNumWedges; wedgeIdx++ )	{
	    StFstWedgeHitCollection* wedgeHitCollection = fstHitCollection->wedge(wedgeIdx);
	    unsigned char nClusteringType = fstHitCollection->getClusteringType();
	    for(int sensorIdx=0; sensorIdx<kFstNumSensorsPerWedge; sensorIdx++)	{
	       StFstSensorHitCollection* sensorHitCollection = wedgeHitCollection->sensor(sensorIdx);
	       int sensorIdxTemp = 0;
               for(int idx=0; idx<(int)sensorHitCollection->hits().size(); idx++ ){
		  StFstHit* hit = sensorHitCollection->hits()[idx];
		  if(hit)	{
			const StThreeVectorF &P = hit->position();

			fstHit.hitId 		= (int)hit->id();
			fstHit.wedge		= (int)hit->getWedge();
			fstHit.sensor		= (int)hit->getSensor();
			fstHit.maxTimeBin	= (int)hit->getMaxTimeBin();
			fstHit.clusteringType	= (int)nClusteringType;
			fstHit.nRawHits		= (int)hit->getNRawHits();
			fstHit.nRawHitsR	= (int)hit->getNRawHitsR();
			fstHit.nRawHitsPhi	= (int)hit->getNRawHitsPhi();
			fstHit.idTruth		= (int)hit->idTruth();
			fstHit.EventId		= (int)eventPtr->id();
			fstHit.localR          	= (float)hit->localPosition(0);
			fstHit.localPhi		= (float)hit->localPosition(1);
			fstHit.localZ		= (float)hit->localPosition(2);
			fstHit.x		= (float)P.x();
			fstHit.y		= (float)P.y();
			fstHit.z		= (float)P.z();
			fstHit.charge		= (float)hit->charge();
			fstHit.chargeErr	= (float)hit->getChargeErr();
			fstHit.apv		= (int)hit->getApv();
			fstHit.meanPhiStrip	= (float)hit->getMeanPhiStrip();
			fstHit.meanRStrip	= (float)hit->getMeanRStrip();

			fstHitTree->Fill();

			sensorIdxTemp = ((int)hit->getWedge()-1)*kFstNumSensorsPerWedge + (int)hit->getSensor(); // 0-107
			int diskIdxTemp = ((int)hit->getWedge()-1)/kFstNumWedgePerDisk + 1; // 1-3
			int wedgeIdxTemp = (int)hit->getWedge() - (diskIdxTemp-1)*kFstNumWedgePerDisk; // 1-12
			int phiIdxTemp = (wedgeIdxTemp-1)*kFstNumPhiSegPerWedge+(int)(hit->getMeanPhiStrip()+0.5);
			int rIdxTemp = (int)(hit->getMeanRStrip()+0.5);

			hitMap[sensorIdxTemp]->Fill((int)(hit->getMeanPhiStrip()+0.5), (int)(hit->getMeanRStrip()+0.5));
			hitMapOfFST[diskIdxTemp-1]->Fill(phiIdxTemp, rIdxTemp);
			hitMapOfAPV[diskIdxTemp-1]->Fill(wedgeIdxTemp, (int)hit->getApv()+(wedgeIdxTemp%2-1)*kFstApvsPerWedge);
			hitGlobalXY[diskIdxTemp-1]->Fill((float)P.x(), (float)P.y());
			hitGlobalRPhi[diskIdxTemp-1]->Fill((float)P.phi(), (float)P.perp());

			hitCharge_SensorId->Fill(sensorIdxTemp, (int)hit->charge());
			hitChargeErr_SensorId->Fill(sensorIdxTemp, (int)(hit->getChargeErr()+0.5));
			maxTimeBin_SensorId->Fill(sensorIdxTemp, (int)hit->getMaxTimeBin());
			clusterSize_SensorId->Fill(sensorIdxTemp, (int)hit->getNRawHits());
			clusterSizeR_SensorId->Fill(sensorIdxTemp, (int)hit->getNRawHitsR());
			clusterSizePhi_SensorId->Fill(sensorIdxTemp, (int)hit->getNRawHitsPhi());
		 }//hit cut
              }//end loop over hits
	      numOfHits_SensorId->Fill(sensorIdxTemp, sensorHitCollection->hits().size());
	      sensorHitCollection->hits().clear();
	   }//loop over sensors
        }//loop over wedges 
      }//end of hits cut

      if(fstCollectionPtr->getNumRawHits() > 0) {
	 int counter[kFstNumSensors]; //raw hit multiplicity per sensor per event
         for(int iS=0; iS<kFstNumSensors; iS++)
             counter[iS] = 0;

         for(int wedgeIdx=0; wedgeIdx<kFstNumWedges; ++wedgeIdx ){
            StFstRawHitCollection *rawHitCollectionPtr = fstCollectionPtr->getRawHitCollection( wedgeIdx );
            if( rawHitCollectionPtr ){
               std::vector<StFstRawHit*>& rawHitVec = rawHitCollectionPtr->getRawHitVec();
               std::vector< StFstRawHit* >::iterator rawHitIter;

               for( rawHitIter = rawHitVec.begin(); rawHitIter != rawHitVec.end(); ++rawHitIter ){
		   StFstRawHit* rawHit = *rawHitIter;

		   unsigned char wedge = rawHit->getWedge();
		   unsigned char sensor = rawHit->getSensor();
		   unsigned char maxTimeBin = rawHit->getMaxTimeBin();
		   int sensorId = (wedge-1)*kFstNumSensorsPerWedge + sensor;
		   counter[sensorId]++;

                   for( unsigned char timeBin = 0; timeBin < kFstNumTimeBins; ++timeBin ) {
		       rawHitCharge_TimeBin[timeBin]->Fill(rawHit->getGeoId(), (int)rawHit->getCharge( timeBin ));
		       fstRawHit.charge[timeBin] = rawHit->getCharge(timeBin);
		       fstRawHit.chargeErr[timeBin]  = rawHit->getChargeErr(timeBin);
		   }
		   rawHitChargeErr->Fill(rawHit->getGeoId(), (int)(rawHit->getChargeErr( maxTimeBin )+0.5));
		   rawHitMap[sensorId]->Fill((int)rawHit->getPhiStrip(), (int)rawHit->getRStrip());
		   rawHitMaxTimeBin_APV->Fill(((int)rawHit->getRdo()-1)*48+(int)rawHit->getArm()*16+(int)rawHit->getApv(), (int)maxTimeBin);

		   fstRawHit.channelId   = (int)rawHit->getChannelId();
		   fstRawHit.geoId       = (int)rawHit->getGeoId();
		   fstRawHit.wedge       = (int)rawHit->getWedge();
		   fstRawHit.sensor      = (int)rawHit->getSensor();
		   fstRawHit.phistrip    = (int)rawHit->getPhiStrip();
		   fstRawHit.rstrip      = (int)rawHit->getRStrip();
		   fstRawHit.maxTimeBin  = (int)maxTimeBin;
		   fstRawHit.rdo         = (int)rawHit->getRdo();
		   fstRawHit.arm         = (int)rawHit->getArm();
		   fstRawHit.apv         = (int)rawHit->getApv();
		   fstRawHit.channel     = (int)rawHit->getChannel();
		   fstRawHit.idTruth     = (int)rawHit->getIdTruth();
		   fstRawHit.seedHitFlag = (int)rawHit->getSeedhitflag();
		   fstRawHit.EventId     = (int)eventPtr->id();

		   fstRawHitTree->Fill();
                }//loop over raw hits
		while (!rawHitVec.empty()) delete rawHitVec.back(), rawHitVec.pop_back();
             }//end raw hit collection
          }//loop over wedges

	  for(int iS=0; iS<kFstNumSensors; iS++) {
              numOfRawHits_SensorId->Fill(iS+1, counter[iS]);
              numOfRawHits_EventId[iS]->Fill((int)eventPtr->id()/100+1, counter[iS]);
          }
       }//end number of raw hits cut
   }
  
   mEventCounter++;

   return ierr;
}

Int_t StFstQAMaker::Finish(){
   Int_t ierr = kStOk;

   if(mDoTreeOutput)
   {
     //create output file
     StIOMaker *ioMaker = (StIOMaker * )GetMaker("inputStream");
     if (!ioMaker) {
       LOG_WARN << "StFstQAMaker::Init(): No StIOMaker" << endm;
     }

     TString mRootFilename = TString(ioMaker->GetFile());
     int found = mRootFilename.Last('/');
     if(found >= 0){
       mRootFilename.Replace(0, found + 1, "");
     }
     found = mRootFilename.First(".");
     if(found == 0) found = mRootFilename.Length();
     mRootFilename.Replace(found, mRootFilename.Length() - found, ".fstQa.root");
     LOG_INFO << "FST QA File Name: " << mRootFilename << endm;

     cout << "QA file name: " << mRootFilename << endl;     
     TFile *myRootFile = new TFile(mRootFilename.Data(),"RECREATE");
     if( !myRootFile ) {
       LOG_WARN << "Error recreating file '" << mRootFilename << "'" << endl;
       ierr = kStWarn;
     }

     //saving histograms 
     myRootFile->WriteTObject(fstRawHitTree);
     myRootFile->WriteTObject(fstHitTree);

     myRootFile->WriteTObject(numOfRawHits_SensorId);
     myRootFile->WriteTObject(numOfHits_SensorId);
     for(int iTimeBin=0; iTimeBin<kFstNumTimeBins; iTimeBin++)
       myRootFile->WriteTObject(rawHitCharge_TimeBin[iTimeBin]);
     myRootFile->WriteTObject(rawHitChargeErr);
     myRootFile->WriteTObject(hitCharge_SensorId);
     myRootFile->WriteTObject(hitChargeErr_SensorId);
     myRootFile->WriteTObject(rawHitMaxTimeBin_APV);
     myRootFile->WriteTObject(maxTimeBin_SensorId);
     myRootFile->WriteTObject(clusterSize_SensorId);
     myRootFile->WriteTObject(clusterSizeR_SensorId);
     myRootFile->WriteTObject(clusterSizePhi_SensorId);
     for(int iWedge=0; iWedge<kFstNumWedges; iWedge++) {
       for(int iSensor=0; iSensor<kFstNumSensorsPerWedge; iSensor++) {
	 myRootFile->WriteTObject(rawHitMap[iWedge*3+iSensor]);
	 myRootFile->WriteTObject(hitMap[iWedge*3+iSensor]);
	 myRootFile->WriteTObject(numOfRawHits_EventId[iWedge*3+iSensor]);
       }
     }
     for(int iDisk = 0; iDisk < kFstNumDisk; iDisk++)
     {
       myRootFile->WriteTObject(hitMapOfFST[iDisk]);
       myRootFile->WriteTObject(hitMapOfAPV[iDisk]);
       myRootFile->WriteTObject(hitGlobalXY[iDisk]);
       myRootFile->WriteTObject(hitGlobalRPhi[iDisk]);
     }

     myRootFile->Close();
   }

    return ierr;
}

ClassImp( StFstQAMaker );

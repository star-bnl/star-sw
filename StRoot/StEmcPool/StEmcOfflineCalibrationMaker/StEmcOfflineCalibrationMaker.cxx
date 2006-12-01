//StEmcOfflineCalibrationMaker.cxx

#include "TFile.h"
#include "TTree.h"
#include "TH2.h"
#include "TChain.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "StEvent/StTriggerId.h"
#include "StEvent/StEvent.h"
#include "StEventTypes.h"

#include "StDetectorDbMaker/StDetectorDbTriggerID.h"
#include "StEvent/StL0Trigger.h"

#include "StDaqLib/TRG/trgStructures2005.h"

//StEmc
#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcTriggerMaker/StEmcTriggerMaker.h"

//logger
#include "StMessMgr.h"

//my files
#include "StEmcOfflineCalibrationEvent.h"
#include "StEmcOfflineCalibrationMaker.h"

//use this to get kBarrelEmcTowerId defined
#include "StEnumerations.h"

ClassImp(StEmcOfflineCalibrationMaker)

StEmcOfflineCalibrationMaker::StEmcOfflineCalibrationMaker(const char* name, const char* file)
{
	filename = file;
	
	muDstMaker = NULL;
	mEmcPosition = new StEmcPosition();
	
	towerSlopes[0] = NULL;
	towerSlopes[1] = NULL;
	preshowerSlopes[0] = NULL;
	preshowerSlopes[1] = NULL;
	preshowerSlopes[2] = NULL;
	
	mbTriggers.clear();
	htTriggers.clear();
}

StEmcOfflineCalibrationMaker::~StEmcOfflineCalibrationMaker() { }

Int_t StEmcOfflineCalibrationMaker::Init()
{
	myFile = new TFile(filename,"RECREATE");
	calibTree = new TTree("calibTree","BTOW calibration tree");
	myEvent = new StEmcOfflineCalibrationEvent();
	myTrack = new StEmcOfflineCalibrationTrack();
	calibTree->Branch("event_branch","StEmcOfflineCalibrationEvent",&myEvent);
	calibTree->SetAutoSave(1000000000);
	
	muDstMaker		= dynamic_cast<StMuDstMaker*>(GetMaker("MuDst")); assert(muDstMaker);
	mADCtoEMaker	= dynamic_cast<StEmcADCtoEMaker*>(GetMaker("Eread")); assert(mADCtoEMaker);
	emcTrigMaker	= dynamic_cast<StEmcTriggerMaker*>(GetMaker("bemctrigger")); assert(emcTrigMaker);

	
//	mTables = new StBemcTables(kTRUE);
	mTables = mADCtoEMaker->getBemcData()->getTables();
	mEmcGeom = StEmcGeom::instance("bemc");

	LOG_INFO << "StEmcOfflineCalibrationMaker::Init() == kStOk" << endm;
	return StMaker::Init();
}

Int_t StEmcOfflineCalibrationMaker::InitRun(int run)
{
	//switch histograms
	if(towerSlopes[0]){
		myFile->cd();
		towerSlopes[0]->Write();
		towerSlopes[1]->Write();
		preshowerSlopes[0]->Write();
		preshowerSlopes[1]->Write();
		preshowerSlopes[2]->Write();
	}
	
	//look for histograms from this run in the current file and switch to them if found, otherwise create them
	char name[200];
	sprintf(name,"towerSlopes_R%i",run);
	towerSlopes[0] = NULL;
	myFile->GetObject(name,towerSlopes[0]);
	if(towerSlopes[0]){
		sprintf(name,"towerSlopes_HT_R%i",run);
		myFile->GetObject(name,towerSlopes[1]);
		
		sprintf(name,"preshowerSlopes_0_R%i",run);
		myFile->GetObject(name,preshowerSlopes[0]);
		
		sprintf(name,"preshowerSlopes_124_R%i",run);
		myFile->GetObject(name,preshowerSlopes[1]);

		sprintf(name,"preshowerSlopes_125_R%i",run);
		myFile->GetObject(name,preshowerSlopes[2]);
	}
	else{
		towerSlopes[0] = new TH2D(name,"ADC vs. towerID",4800,0.5,4800.5,250,-49.5,200.5);
		
		sprintf(name,"towerSlopes_HT_R%i",run);
		towerSlopes[1] = new TH2D(name,"ADC vs. towerID",4800,0.5,4800.5,250,-49.5,200.5);
		
		sprintf(name,"preshowerSlopes_0_R%i",run);		
		preshowerSlopes[0] = new TH2D(name,"ADC vs. towerID",4800,0.5,4800.5,500,-49.5,450.5);

		sprintf(name,"preshowerSlopes_124_R%i",run);
		preshowerSlopes[1] = new TH2D(name,"ADC vs. towerID",4800,0.5,4800.5,500,-49.5,450.5);

		sprintf(name,"preshowerSlopes_125_R%i",run);
		preshowerSlopes[2] = new TH2D(name,"ADC vs. towerID",4800,0.5,4800.5,500,-49.5,450.5);	
	}
	
	return StMaker::InitRun(run);
}

Int_t StEmcOfflineCalibrationMaker::Make()
{		
	//get pointers to useful objects
	TChain* chain				= muDstMaker->chain(); assert(chain);
	StMuDst* muDst				= muDstMaker->muDst(); assert(muDst);
	StMuEvent* event			= muDst->event(); assert(event);
	StRunInfo* runInfo			= &(event->runInfo()); assert(runInfo);
//	TrgDataType2005* trgData	= (TrgDataType2005*)event->triggerData()->getTriggerStructure(); assert(trgData);
	
	LOG_DEBUG << "general pointer assertions OK" << endm;
	
	//pedestals, rms, status	
	for (int id=1; id<=4800; id++) {
		float pedestal, rms;
		int status;
		
		mTables->getPedestal(BTOW, id, 0, pedestal, rms);
		mTables->getStatus(BTOW, id, status);
		mPedestal[BTOW-1][id-1] = pedestal;
		mPedRMS[BTOW-1][id-1]	= rms;
		mStatus[BTOW-1][id-1]	= status;
		
		mTables->getPedestal(BPRS, id, 0, pedestal, rms);
		mTables->getStatus(BPRS, id, status);
		mPedestal[BPRS-1][id-1] = pedestal;
		mPedRMS[BPRS-1][id-1]	= rms;
		mStatus[BPRS-1][id-1]	= status;		
	}

	LOG_DEBUG << "got peds and status in Make()" << endm;
	
	//triggers
	const StTriggerId& trigs = event->triggerIdCollection().nominal();

	//toss out fpd1-tpcdead-fast triggers immediately
	if(trigs.isTrigger(2) || trigs.isTrigger(117460) || trigs.isTrigger(137460) || trigs.isTrigger(147461)) return kStOK;
	
	//record if event satisfies trigger in a run-independent way
	myEvent->mbTrigger = 0;
	for(vector<unsigned int>::const_iterator it=mbTriggers.begin(); it!=mbTriggers.end(); it++){
		if(trigs.isTrigger(*it)){
			myEvent->mbTrigger = 1;
			break;
		}
	}
	
	myEvent->htTrigger = 0;
	for(vector<unsigned int>::const_iterator it=htTriggers.begin(); it!=htTriggers.end(); it++){
		if(trigs.isTrigger(*it)){
			myEvent->htTrigger = 1;
			break;
		}
	}
	
//	myEvent->rawTriggerBlock = trgData->TrgSum.L2Sum[0];
	
//	myEvent->mbTrigger	= trigs.isTrigger(117001) || trigs.isTrigger(147001);
//	myEvent->htTrigger	= trigs.isTrigger(117211) || trigs.isTrigger(117212) || trigs.isTrigger(127212) || trigs.isTrigger(127213) || trigs.isTrigger(137213);
//	myEvent->jp0Trigger = trigs.isTrigger(117501) || trigs.isTrigger(127501) || trigs.isTrigger(137501);
//	myEvent->jp1Trigger	= trigs.isTrigger(117221) || trigs.isTrigger(127221) || trigs.isTrigger(137221) || trigs.isTrigger(137222);
//	myEvent->httpTrigger= trigs.isTrigger(117201) || trigs.isTrigger(117821) || trigs.isTrigger(127821) || trigs.isTrigger(137821); 	
	
	//basic event info
	bool fillQA			= (runInfo->beamFillNumber(blue)==runInfo->beamFillNumber(yellow));
	myEvent->fill		= (fillQA) ? (unsigned short)runInfo->beamFillNumber(blue):0;
	myEvent->run		= event->runNumber();
	myEvent->event		= event->eventNumber();
	myEvent->date		= GetDate();
	myEvent->time		= GetTime();
	
	//filename manipulations
	TString inputfile(chain->GetFile()->GetName());
	int index1 = inputfile.Index(".MuDst");
	TString string1(inputfile(index1-19,7));
	TString string2(inputfile(index1-7,7));
	myEvent->fileid1 = string1.Atoi();
	myEvent->fileid2 = string2.Atoi();	
	
	//store all vertices from this event	
	myEvent->nVertices	= muDst->numberOfPrimaryVertices();
	for(unsigned int i=0; i<myEvent->nVertices; i++){
		if(i>9)
		{
			LOG_WARN << "found more than 10 vertices for R"<<myEvent->run<< ", event "<<myEvent->event<<endm;
			continue;
		}
		
		assert(muDst->primaryVertex(i));
		StThreeVectorF stvertex = muDst->primaryVertex(i)->position();
		myEvent->vx[i] = stvertex.x();
		myEvent->vy[i] = stvertex.y();
		myEvent->vz[i] = stvertex.z();
		myEvent->ranking[i] = muDst->primaryVertex(i)->ranking();
	}
	
	LOG_DEBUG << "old code seems fine" << endm;
	
	//fill ADC values from StEmcCollection obtained from MuDst
	mEmcCollection = muDst->emcCollection();
	getADCs(BTOW);
	getADCs(BPRS);
	for(int id=1; id<=4800; id++){
		if(mADC[BTOW-1][id-1] != 0){
			if(myEvent->mbTrigger) towerSlopes[0]->Fill(id,mADC[BTOW-1][id-1]-mPedestal[BTOW-1][id-1]);
			if(myEvent->htTrigger) towerSlopes[1]->Fill(id,mADC[BTOW-1][id-1]-mPedestal[BTOW-1][id-1]);
		}
		if(mADC[BPRS-1][id-1] != 0){		
			switch(mCapacitor[id-1]){
				case(125):	preshowerSlopes[2]->Fill(id,mADC[BPRS-1][id-1]-mPedestal[BPRS-1][id-1]);
				case(124):	preshowerSlopes[1]->Fill(id,mADC[BPRS-1][id-1]-mPedestal[BPRS-1][id-1]);
				default:	preshowerSlopes[0]->Fill(id,mADC[BPRS-1][id-1]-mPedestal[BPRS-1][id-1]);
			}
		}
	}
	
	LOG_DEBUG << "got ADCs" << endm;
	
	
	//trigger maker
	myEvent->htTrigMaker[0] = emcTrigMaker->is2006HT2();
	myEvent->htTrigMaker[1] = emcTrigMaker->get2006HT2_ID();
	myEvent->htTrigMaker[2] = emcTrigMaker->get2006HT2_ADC();
	
	
	//now for the tracks
	for(unsigned int vertex_index=0; vertex_index<myEvent->nVertices; vertex_index++){
		muDst->setVertexIndex(vertex_index);
		TObjArray* primaryTracks = muDst->primaryTracks();
		StMuTrack* track;
		int nentries = muDst->numberOfPrimaryTracks();
		assert(nentries==primaryTracks->GetEntries());
		for(int i=0; i<nentries; i++){
			track = muDst->primaryTracks(i);
			
			//project track to BEMC
			pair<unsigned int, pair<float,float> > center_tower = getTrackTower(track);
			int id = center_tower.first;
			
			double p = track->p().mag();
			if(id > 0 && p > 1.0){
				
				//find neighboring towers
				int softid[9];
				softid[0] = id;
				softid[1] = mEmcPosition->getNextTowerId(id,-1,-1);
				softid[2] = mEmcPosition->getNextTowerId(id,0,-1);
				softid[3] = mEmcPosition->getNextTowerId(id,1,-1);
				softid[4] = mEmcPosition->getNextTowerId(id,-1,0);
				softid[5] = mEmcPosition->getNextTowerId(id,1,0);
				softid[6] = mEmcPosition->getNextTowerId(id,-1,1);
				softid[7] = mEmcPosition->getNextTowerId(id,0,1);
				softid[8] = mEmcPosition->getNextTowerId(id,1,1);
				
				//save EMC info for 3x3 tower block around track
				for(int tower=0; tower<9; tower++){
					myTrack->tower_id[tower]			= softid[tower];
					myTrack->tower_adc[tower]			= mADC[BTOW-1][softid[tower]-1];
					myTrack->tower_pedestal[tower]		= mPedestal[BTOW-1][softid[tower]-1];
					myTrack->tower_pedestal_rms[tower]	= mPedRMS[BTOW-1][softid[tower]-1];
					myTrack->tower_status[tower]		= mStatus[BTOW-1][softid[tower]-1];
					
					myTrack->preshower_adc[tower]			= mADC[BPRS-1][softid[tower]-1];
					myTrack->preshower_pedestal[tower]		= mPedestal[BPRS-1][softid[tower]-1];
					myTrack->preshower_pedestal_rms[tower]	= mPedRMS[BPRS-1][softid[tower]-1];
					myTrack->preshower_status[tower]		= mStatus[BPRS-1][softid[tower]-1];
					myTrack->preshower_cap[tower]			= mCapacitor[softid[tower]-1];	
				}
					
				myTrack->p					= p;
				myTrack->deta				= center_tower.second.first;
				myTrack->dphi				= center_tower.second.second;
				myTrack->tower_id_exit		= (getTrackTower(track, true)).first;
				myTrack->highest_neighbor	= highestNeighbor(myTrack->tower_id[0]);
				
	//			myTrack->tower_id			= id;
	//			myTrack->dR					= center_tower.second;
	//			myTrack->tower_adc			= mADC[BTOW-1][id-1];
	//			myTrack->tower_pedestal		= mPedestal[BTOW-1][id-1];
	//			myTrack->tower_pedestal_rms = mPedRMS[BTOW-1][id-1];
	//			myTrack->tower_status		= mStatus[BTOW-1][id-1];
				
				myTrack->nSigmaElectron		= track->nSigmaElectron();
				myTrack->nHits				= track->nHits();
				myTrack->nFitPoints			= track->nHitsFit();
				myTrack->nDedxPoints		= track->nHitsDedx();
				myTrack->nHitsPossible		= track->nHitsPoss();
				myTrack->dEdx				= track->dEdx();
				
				LOG_DEBUG<<"adding track for tower "<<myTrack->tower_id<<" with ADC "<<myTrack->tower_adc<<endm;
				myEvent->addTrack(myTrack);
			}
			
			myTrack->Clear();
		}
	}
	calibTree->Fill();
	
	LOG_DEBUG << "StEmcOfflineCalibrationMaker::Make() == kStOk" << endm;
	return kStOK;
}

void StEmcOfflineCalibrationMaker::Clear(Option_t* option)
{	
	myEvent->Clear();
	for(int i=0; i<2; i++){
		for(int j=0; j<4800; j++){
			mADC[i][j]		= 0;
		}
	}
}

Int_t StEmcOfflineCalibrationMaker::Finish()
{
	myFile->cd();
	towerSlopes[0]->Write();
	towerSlopes[1]->Write();
	preshowerSlopes[0]->Write();
	preshowerSlopes[1]->Write();
	preshowerSlopes[2]->Write();
	calibTree->Write();
	myFile->Close();
	LOG_INFO << "StEmcOfflineCalibrationMaker::Finish() == kStOk"<<endm;
	return kStOk;
}

void StEmcOfflineCalibrationMaker::getADCs(int det) //1==BTOW, 2==BPRS
{
	det--;
	StDetectorId detectorid = static_cast<StDetectorId>(kBarrelEmcTowerId + det);
	StEmcDetector* detector=mEmcCollection->detector(detectorid);
	if(detector){
		for(int m=1;m<=120;m++){
			StEmcModule* module = detector->module(m);
			if(module)
			{
				StSPtrVecEmcRawHit& rawHit=module->hits();
				for(unsigned int k=0;k<rawHit.size();k++){
					if(rawHit[k]){
						int module = rawHit[k]->module();
						int eta = rawHit[k]->eta(); 
						int submodule = abs(rawHit[k]->sub());
						int ADC = rawHit[k]->adc();
						int hitid;
						int stat = mEmcGeom->getId(module,eta,submodule,hitid);
						if(stat==0) mADC[det][hitid-1] = ADC;
						if(det==1){
							unsigned char CAP =  rawHit[k]->calibrationType();
							if(CAP > 127) CAP -= 128;
							mCapacitor[hitid-1] = CAP;
						}
					}
				}
			}
			else { LOG_WARN<<"couldn't find StEmcModule "<<m<<" for detector "<<det<<endm; }
		}
	}
	else { if(det==0) { LOG_WARN<<"couldn't find StEmcDetector for detector "<<det<<endm; } }
}

//returns (tower, (deta,dphi))
pair<unsigned short, pair<float,float> > StEmcOfflineCalibrationMaker::getTrackTower(StMuTrack* track, bool useExitRadius){
	pair<unsigned short, pair<float,float> > tower;
	tower.first = 0;
	tower.second.first = 1000.;
	tower.second.second = 1000.;

	StThreeVectorD momentum,position;
	Double_t radius = mEmcGeom->Radius();
		
	const StEventSummary& evtSummary = muDstMaker->muDst()->event()->eventSummary();
	Double_t mField = evtSummary.magneticField()/10;
		
	//add 30 cm to radius to find out if track left same tower
	if(useExitRadius) radius += 30.0;
	
	bool goodProjection = mEmcPosition->trackOnEmc(&position,&momentum,track,mField,radius);
	if(goodProjection){
		int m,e,s,id=0;
		float eta=position.pseudoRapidity();
		float phi=position.phi();
		mEmcGeom->getBin(phi,eta,m,e,s);
		
		if(mEmcGeom->getId(m,e,s,id)==0){
			tower.first = id;
			tower.second = getTrackDetaDphi(eta, phi, id);
		}
	}
	
	return tower;
}

pair<float,float> StEmcOfflineCalibrationMaker::getTrackDetaDphi(float track_eta, float track_phi, int id){
	float eta_tower, phi_tower;
	eta_tower = phi_tower = -999.;
	mEmcGeom->getEtaPhi(id,eta_tower,phi_tower);
					
	//now calculate distance from center of tower:
	float dphi = phi_tower - track_phi;
	while(dphi >  M_PI)		{dphi -= 2.0 * M_PI;}
	while(dphi < -1.*M_PI)	{dphi += 2.0 * M_PI;}

	pair<float, float> dtow;
	dtow.first = eta_tower - track_eta;
	dtow.second = dphi;
	
	return dtow;
}

double StEmcOfflineCalibrationMaker::highestNeighbor(int id){
	double nSigma=0.;
	
	for(int deta=-1; deta<=1; deta++){
		for(int dphi=-1; dphi<=1; dphi++){
			if(deta==0 && dphi==0) continue;
			int nextId = mEmcPosition->getNextTowerId(id,deta,dphi);
			if(nextId<1 || nextId>4800) continue;
			if((mADC[BTOW-1][nextId-1]-mPedestal[BTOW-1][nextId-1]) > nSigma*mPedRMS[BTOW-1][nextId-1]){
				nSigma = (mADC[BTOW-1][nextId-1] - mPedestal[BTOW-1][nextId-1])/mPedRMS[BTOW-1][nextId-1];
			}
		}
	}
	
	return nSigma;
}

void StEmcOfflineCalibrationMaker::addMinBiasTrigger(unsigned int trigId){ 
	mbTriggers.push_back(trigId);
}

void StEmcOfflineCalibrationMaker::addHighTowerTrigger(unsigned int trigId){ 
	htTriggers.push_back(trigId);
}

//StEmcOfflineCalibrationMaker.cxx

#include <map>

#include "TFile.h"
#include "TTree.h"
#include "TH2.h"
#include "TH3.h"
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
	preshowerSlopes = NULL;
	
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
		preshowerSlopes->Write();
	}
	
	//look for histograms from this run in the current file and switch to them if found, otherwise create them
	char name[200];
	sprintf(name,"towerSlopes_R%i",run);
	towerSlopes[0] = NULL;
	myFile->GetObject(name,towerSlopes[0]);
	if(towerSlopes[0]){
		sprintf(name,"towerSlopes_HT_R%i",run);
		myFile->GetObject(name,towerSlopes[1]);
		
		sprintf(name,"preshowerSlopes_R%i",run);
		myFile->GetObject(name,preshowerSlopes);
	}
	else{
		towerSlopes[0] = new TH2D(name,"ADC vs. towerID",4800,0.5,4800.5,250,-49.5,200.5);
		
		sprintf(name,"towerSlopes_HT_R%i",run);
		towerSlopes[1] = new TH2D(name,"ADC vs. towerID",4800,0.5,4800.5,250,-49.5,200.5);
		
		sprintf(name,"preshowerSlopes_R%i",run);		
		preshowerSlopes = new TH3F(name,"ADC vs. cap vs. towerID",4800,0.5,4800.5, 2,-0.5,127.5, 500,-49.5,450.5);
	}
	
	LOG_DEBUG << "finished init run for " << run << endm;
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
	
	myEvent->triggerIds = trigs.triggerIds();
	myEvent->l2Result = event->L2Result();
	
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
		
	//fill ADC values from StEmcCollection obtained from MuDst
	mEmcCollection = muDst->emcCollection();
	getADCs(BTOW);
	getADCs(BPRS);
	for(int id=1; id<=4800; id++){
		if((mADC[BTOW-1][id-1] != 0) && (mStatus[BTOW-1][id-1] == 1)){
			//if(myEvent->mbTrigger)
			towerSlopes[0]->Fill(id,mADC[BTOW-1][id-1]-mPedestal[BTOW-1][id-1]);
			//if(myEvent->htTrigger) towerSlopes[1]->Fill(id,mADC[BTOW-1][id-1]-mPedestal[BTOW-1][id-1]); 
		}
		if(mADC[BPRS-1][id-1] != 0){		
	//		LOG_DEBUG << "filling preshower slopes histogram" << endm;
			preshowerSlopes->Fill(id, mCapacitor[id-1], mADC[BPRS-1][id-1]-mPedestal[BPRS-1][id-1]);
		}
	}
	
	LOG_DEBUG << "got ADCs" << endm;
	
	//trigger maker
	myEvent->htTrigMaker[0] = emcTrigMaker->isTrigger(137213);
    std::map<int,int>::const_iterator p = (emcTrigMaker->barrelTowersAboveThreshold(137213)).begin();
    myEvent->htTrigMaker[1] = p->first;
    myEvent->htTrigMaker[2] = p->second;
	
	
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
				}
				
				//recenter the array to test new PRS mapping hypotheses
				softid[0] = getCorrectSignalForPRS(id);
				softid[1] = mEmcPosition->getNextTowerId(softid[0],-1,-1);
				softid[2] = mEmcPosition->getNextTowerId(softid[0],0,-1);
				softid[3] = mEmcPosition->getNextTowerId(softid[0],1,-1);
				softid[4] = mEmcPosition->getNextTowerId(softid[0],-1,0);
				softid[5] = mEmcPosition->getNextTowerId(softid[0],1,0);
				softid[6] = mEmcPosition->getNextTowerId(softid[0],-1,1);
				softid[7] = mEmcPosition->getNextTowerId(softid[0],0,1);
				softid[8] = mEmcPosition->getNextTowerId(softid[0],1,1);
				
				LOG_DEBUG << "BTOW id = " << id << " and BPRS id = " << softid[0] << endm;
				
				for(int tower=0; tower<9; tower++){
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
				
				myTrack->nSigmaElectron		= track->nSigmaElectron();
				myTrack->nHits				= track->nHits();
				myTrack->nFitPoints			= track->nHitsFit();
				myTrack->nDedxPoints		= track->nHitsDedx();
				myTrack->nHitsPossible		= track->nHitsPoss();
				myTrack->dEdx				= track->dEdx();
				
//				LOG_DEBUG<<"adding track for tower "<<myTrack->tower_id<<" with ADC "<<myTrack->tower_adc<<endm;
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
			mADC[i][j] = 0;
		}
	}
}

Int_t StEmcOfflineCalibrationMaker::Finish()
{
	LOG_INFO << "cd to output file" << endm;
	myFile->cd();
	LOG_INFO << "write calibration tree" << calibTree->Write() << endm;
	LOG_INFO << "write tower histogram" << towerSlopes[0]->Write() << endm;
	towerSlopes[1]->Write();
	LOG_INFO << "write preshower histogram" << preshowerSlopes->Write() << endm;
	LOG_INFO << "close the output file" << endm;
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

//this is a debugging function used to test possible swaps in the preshower
//if a track projects to softId, we will take the preshower data from prsSignalId
int StEmcOfflineCalibrationMaker::getCorrectSignalForPRS(int softId)
{
	int prsSignalId = softId;
	
	switch(softId){
		//http://www4.rcf.bnl.gov/~rfc/web_20_chan/bad_map_20.txt
		case(635):case(636):/*case(709):case(710):case(712):case(715):*/case(823):case(825):case(826):case(828):case(831):
		case(834):case(839):case(840):case(1021):case(1022):case(1023):case(1024):case(1025):case(1026):case(1027):
		case(1028):case(1061):case(1062):case(1064):case(1065):case(1066):case(1074):case(1220):case(1301):case(1303):
		case(1313):case(1381):case(1382):case(1383):case(1384):case(1385):case(1386):case(1387):case(1388):case(1513):
		case(1751):case(1752):case(1753):case(1754):case(1861):case(1862):case(1863):case(1864):case(1865):case(1866):
		case(1867):case(1868):case(1869):case(1870):case(1871):case(1872):case(1873):case(1874):case(1875):case(1876):
		case(1877):case(1878):case(1879):case(1880):case(2023):case(2024):case(2077):case(2078):case(2209):case(2773):
		case(2906):case(3065):case(3066):case(3067):case(3070):case(3121):/*case(3178):case(3237):case(3279):case(3480):
		case(3537):case(3538):case(3618):case(3657):case(3718):case(3720):case(3838):case(4195):case(4206):case(4217):
		case(4547):case(4548):case(4550):*/
			prsSignalId += 20; break;
		case(655):case(656):/*case(729):case(730):case(732):case(735):*/case(843):case(845):case(846):case(848):case(851):
		case(854):case(859):case(860):case(1041):case(1042):case(1043):case(1044):case(1045):case(1046):case(1047):
		case(1048):case(1081):case(1082):case(1084):case(1085):case(1086):case(1094):case(1240):case(1321):case(1323):
		case(1333):case(1401):case(1402):case(1403):case(1404):case(1405):case(1406):case(1407):case(1408):case(1533):
		case(1771):case(1772):case(1773):case(1774):case(1881):case(1882):case(1883):case(1884):case(1885):case(1886):
		case(1887):case(1888):case(1889):case(1890):case(1891):case(1892):case(1893):case(1894):case(1895):case(1896):
		case(1897):case(1898):case(1899):case(1900):case(2043):case(2044):case(2097):case(2098):case(2229):case(2793):
		case(2926):case(3085):case(3086):case(3087):case(3090):case(3141):/*case(3198):case(3257):case(3299):case(3500):
		case(3557):case(3558):case(3638):case(3677):case(3738):case(3740):case(3848):case(4215):case(4226):case(4237):
		case(4567):case(4568):case(4570):*/
			prsSignalId -= 20; break;
			
			
		//http://www.star.bnl.gov/protected/emc/wwjacobs/tmp/bprs_zoom_probs_west.txt
		case(389): prsSignalId = 412; break;
		case(390): prsSignalId = 411; break;
		case(391): prsSignalId = 410; break;
		case(392): prsSignalId = 409; break;
			
		case(409): prsSignalId = 392; break;
		case(410): prsSignalId = 391; break;
		case(411): prsSignalId = 390; break;
		case(412): prsSignalId = 389; break;
			
		case(661):case(662):case(663):case(664):case(665):case(666):case(667):case(668):case(669):case(670):
		case(671):case(672):case(673):case(674):case(675):case(676):case(677):case(678):case(679):case(680):
		case(681):case(682):case(683):case(684):case(685):case(686):case(687):case(688):case(689):case(690):
		case(691):case(692):case(693):case(694):case(695):case(696):case(697):case(698):case(699):case(700):
			prsSignalId += 40; break;
		case(701):case(702):case(703):case(704):case(705):case(706):case(707):case(708):case(709):case(710):
		case(711):case(712):case(713):case(714):case(715):case(716):case(717):case(718):case(719):case(720):
		case(721):case(722):case(723):case(724):case(725):case(726):case(727):case(728):case(729):case(730):
		case(731):case(732):case(733):case(734):case(735):case(736):case(737):case(738):case(739):case(740):
			prsSignalId -= 40; break;
			
		case(757):case(758):case(759):case(760):case(777):case(778):case(779):case(780):
			prsSignalId += 40; break;
		case(797):case(798):case(799):case(800):case(817):case(818):case(819):case(820):
			prsSignalId -= 40; break;			
			
		case(1199):case(1219): prsSignalId += 40;
		case(1239):case(1259): prsSignalId -= 40;
			
		//http://www.star.bnl.gov/protected/emc/wwjacobs/tmp/bprs_zoom_probs_east.txt
		case(3157):case(3158):case(3159):case(3160):
			prsSignalId += 40; break;
		case(3197):case(3198):case(3199):case(3200):
			prsSignalId -= 40; break;

		case(3177):case(3178):case(3179):case(3180):
			prsSignalId += 40; break;
		case(3217):case(3218):case(3219):case(3220):
			prsSignalId -= 40; break;
			
		case(3181): prsSignalId = 3208; break;
		case(3182): prsSignalId = 3207; break;
		case(3183): prsSignalId = 3206; break;
		case(3184): prsSignalId = 3205; break;
		case(3185): prsSignalId = 3204; break;
		case(3186): prsSignalId = 3203; break;
		case(3187): prsSignalId = 3202; break;
		case(3188): prsSignalId = 3201; break;
			
		case(3237):case(3238):case(3239):case(3240):
			prsSignalId += 40; break;
		case(3277):case(3278):case(3279):case(3280):
			prsSignalId -= 40; break;
			
		case(3257):case(3258):case(3259):case(3260):
			prsSignalId += 40; break;
		case(3297):case(3298):case(3299):case(3300):
			prsSignalId -= 40; break;
			
		case(3397):case(3398):case(3399):case(3400):
			prsSignalId += 40; break;
		case(3437):case(3438):case(3439):case(3440):
			prsSignalId -= 40; break;
	
		case(3417):case(3418):case(3419):case(3420):
			prsSignalId += 40; break;
		case(3457):case(3458):case(3459):case(3460):
			prsSignalId -= 40; break;
			
		case(3477):case(3478):case(3479):case(3480):
			prsSignalId += 40; break;
		case(3517):case(3518):case(3519):case(3520):
			prsSignalId -= 40; break;
			
		case(3497):case(3498):case(3499):case(3500):
			prsSignalId += 40; break;
		case(3537):case(3538):case(3539):case(3540):
			prsSignalId -= 40; break;
			
		case(3557):case(3558):case(3559):case(3560):
			prsSignalId += 40; break;
		case(3597):case(3598):case(3599):case(3600):
			prsSignalId -= 40; break;
			
		case(3577):case(3578):case(3579):case(3580):
			prsSignalId += 40; break;
		case(3617):case(3618):case(3619):case(3620):
			prsSignalId -= 40; break;
			
		case(3637):case(3638):case(3639):case(3640):
			prsSignalId += 40; break;
		case(3677):case(3678):case(3679):case(3680):
			prsSignalId -= 40; break;
			
		case(3657):case(3658):case(3659):case(3660):
			prsSignalId += 40; break;
		case(3697):case(3698):case(3699):case(3700):
			prsSignalId -= 40; break;
			
		case(3717):case(3718):case(3719):case(3720):
			prsSignalId += 40; break;
		case(3757):case(3758):case(3759):case(3760):
			prsSignalId -= 40; break;
			
		case(3737):case(3738):case(3739):case(3740):
			prsSignalId += 40; break;
		case(3777):case(3778):case(3779):case(3780):
			prsSignalId -= 40; break;
			
		case(3797):case(3798):case(3799):case(3800):
			prsSignalId += 40; break;
		case(3837):case(3838):case(3839):case(3840):
			prsSignalId -= 40; break;
			
		case(3817):case(3818):case(3819):case(3820):
			prsSignalId += 40; break;
		case(3857):case(3858):case(3859):case(3860):
			prsSignalId -= 40; break;
			
		case(4181):case(4182):case(4183):case(4184):case(4185):case(4186):case(4187):case(4188):case(4189):case(4190):
		case(4191):case(4192):case(4193):case(4194):case(4195):case(4196):case(4197):case(4198):case(4199):case(4200):
		case(4201):case(4202):case(4203):case(4204):case(4205):case(4206):case(4207):case(4208):case(4209):case(4210):
		case(4211):case(4212):case(4213):case(4214):case(4215):case(4216):case(4217):case(4218):case(4219):case(4220):			
			prsSignalId += 40; break;
		case(4221):case(4222):case(4223):case(4224):case(4225):case(4226):case(4227):case(4228):case(4229):case(4230):			
		case(4231):case(4232):case(4233):case(4234):case(4235):case(4236):case(4237):case(4238):case(4239):case(4240):			
		case(4241):case(4242):case(4243):case(4244):case(4245):case(4246):case(4247):case(4248):case(4249):case(4250):			
		case(4251):case(4252):case(4253):case(4254):case(4255):case(4256):case(4257):case(4258):case(4259):case(4260):			
			prsSignalId -= 40; break;

		case(4545):case(4546):case(4547):case(4548):
			prsSignalId -= 20; break;

		case(4565):case(4566):case(4567):case(4568):
			prsSignalId -= 20; break;
	}
	
	return prsSignalId;
}

//StChargedPionAnalysisMaker.cxx

#include "TFile.h"
#include "TTree.h"
#include "TChain.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "StEvent/StTriggerId.h"
#include "StEvent/StEvent.h"
#include "StEventTypes.h"

#include "StDetectorDbMaker/StDetectorDbTriggerID.h"
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
#include "StEvent/StL0Trigger.h"
#include "StEmcTriggerMaker/StEmcTriggerMaker.h"

#include "StBbcTriggerDetector.h"

//old-style PYTHIA tables
#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"

//StMcEvent
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEventTypes.hh"
#include "StMcEvent.hh"

//logger
#include "StMessMgr.h"

//my files
#include "TChargedPionEvent.h"
#include "StChargedPionAnalysisMaker.h"

ClassImp(StChargedPionAnalysisMaker)

StChargedPionAnalysisMaker::StChargedPionAnalysisMaker(const char* name, const char* file)
{
	isRealData = true;	
	filename = file;
	
	muDstMaker = NULL;
	emcTrigMaker = NULL;
	spDbMaker = NULL;
}

StChargedPionAnalysisMaker::~StChargedPionAnalysisMaker() { }

Int_t StChargedPionAnalysisMaker::Init()
{
	myFile = new TFile(filename,"RECREATE");
	pionTree = new TTree("pionTree","pion event tree");
	myEvent = new TChargedPionEvent();
	pion = new TChargedPion();
	pionTree->Branch("event_branch","TChargedPionEvent",&myEvent);
	pionTree->SetAutoSave(1000000);
	
	muDstMaker		= dynamic_cast<StMuDstMaker*>(GetMaker("MuDst")); assert(muDstMaker);
	emcTrigMaker	= dynamic_cast<StEmcTriggerMaker*>(GetMaker("bemctrigger")); assert(emcTrigMaker);
	spDbMaker		= (isRealData) ? dynamic_cast<StSpinDbMaker*>(GetMaker("spinDb")) : NULL; 
	
	LOG_INFO << "StChargedPionAnalysisMaker::Init() == kStOk" << endm;
	return StMaker::Init();
}

Int_t StChargedPionAnalysisMaker::InitRun(int run)
{
	return StMaker::InitRun(run);
}

Int_t StChargedPionAnalysisMaker::Make()
{	
	if(isRealData==false) makeSimulatedData();
	
	//get pointers to useful objects
	TChain* chain				= muDstMaker->chain(); assert(chain);
	StMuDst* muDst				= muDstMaker->muDst(); assert(muDst);
	StMuEvent* event			= muDst->event(); assert(event);
	StBbcTriggerDetector* bbc	= &(event->bbcTriggerDetector()); assert(bbc);
	StRunInfo* runInfo			= &(event->runInfo()); assert(runInfo);
	
	LOG_DEBUG << "general pointer assertions OK" << endm;
	
	//basic event info
	bool fillQA			= (runInfo->beamFillNumber(blue)==runInfo->beamFillNumber(yellow));
	myEvent->fill		= (fillQA) ? (unsigned short)runInfo->beamFillNumber(blue):0;
	myEvent->run		= event->runNumber();
	myEvent->event		= event->eventNumber();
	

	//filename manipulations
	TString inputfile(chain->GetFile()->GetName());
	if(isRealData){
		int index1 = inputfile.Index(".MuDst");
		TString string1(inputfile(index1-19,7));
		TString string2(inputfile(index1-7,7));
		myEvent->fileid1 = string1.Atoi();
		myEvent->fileid2 = string2.Atoi();
	}
	else{
		int index1 = inputfile.Index("rcf");
		TString string1(inputfile(index1+3,4));
		TString string2(inputfile(index1+8,2));
		myEvent->fileid1 = string1.Atoi();
		myEvent->fileid2 = string2.Atoi();
	}
	
	
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
	
	
	if(isRealData){
		//more useful pointers; these are valid only for real data
		StL0Trigger* trig			= &(event->l0Trigger()); assert(trig);
		StDetectorDbTriggerID* v	= StDetectorDbTriggerID::instance(); assert(v);
		
		LOG_DEBUG << "data-only pointer assertions OK" << endm;
		
		myEvent->x48		= trig->bunchCrossingId();
		myEvent->x7			= trig->bunchCrossingId7bit(myEvent->run);
		myEvent->trigspin	= trig->spinBits(myEvent->run);
		
		myEvent->bbcTimeBin	= event->bbcTriggerDetector().onlineTimeDifference();
		
		const StTriggerId& trigs = event->triggerIdCollection().nominal();
		myEvent->mbTrigger	= trigs.isTrigger(96011);
		myEvent->ht1Trigger	= trigs.isTrigger(96201);
		myEvent->ht2Trigger	= trigs.isTrigger(96211);
		myEvent->jp1Trigger	= trigs.isTrigger(96221);
		myEvent->jp2Trigger	= trigs.isTrigger(96233);
		
		myEvent->mbPrescale  = v->getTotalPrescaleByTrgId(96011);
		myEvent->ht1Prescale = v->getTotalPrescaleByTrgId(96201);
		myEvent->ht2Prescale = v->getTotalPrescaleByTrgId(96211);
		myEvent->jp1Prescale = v->getTotalPrescaleByTrgId(96221);
		myEvent->jp2Prescale = v->getTotalPrescaleByTrgId(96233);
				
		//get spin info (Yellow,Blue): Up,Up = 5; Down,Up = 6; Up,Down = 9; Down,Down = 10;
		myEvent->spinQA[0] = spDbMaker->isValid();
		myEvent->spinQA[1] = spDbMaker->isPolDirLong();
		myEvent->spinQA[2] = !spDbMaker->isPolDirTrans();
		myEvent->spinQA[3] = !spDbMaker->isMaskedUsingBX48(myEvent->x48);
		myEvent->spinQA[4] = (spDbMaker->offsetBX48minusBX7(myEvent->x48, myEvent->x7) == 0);
		
		myEvent->dbspin	= spDbMaker->spin4usingBX48(myEvent->x48);
	}
	
	
	//get EMC software trigger info
	myEvent->ht1TrigMaker[0] = emcTrigMaker->is2005HT1();
	myEvent->ht1TrigMaker[1] = emcTrigMaker->get2005HT1_ID();
	myEvent->ht1TrigMaker[2] = emcTrigMaker->get2005HT1_ADC();
	
	myEvent->ht2TrigMaker[0] = emcTrigMaker->is2005HT2();
	myEvent->ht2TrigMaker[1] = emcTrigMaker->get2005HT2_ID();
	myEvent->ht2TrigMaker[2] = emcTrigMaker->get2005HT2_ADC();
	
	myEvent->jp1TrigMaker[0] = emcTrigMaker->is2005JP1();
	myEvent->jp1TrigMaker[1] = emcTrigMaker->get2005JP1_ID();
	myEvent->jp1TrigMaker[2] = emcTrigMaker->get2005JP1_ADC();
	
	myEvent->jp2TrigMaker[0] = emcTrigMaker->is2005JP2();
	myEvent->jp2TrigMaker[1] = emcTrigMaker->get2005JP2_ID();
	myEvent->jp2TrigMaker[2] = emcTrigMaker->get2005JP2_ADC();
	
	
	//get BBC trigger info
	int Npmt=bbc->numberOfPMTs();
	bool Wbbc=false;
	bool Ebbc=false;
	for (int pmt=0;pmt<Npmt;pmt++){    
		if(bbc->adc(pmt) > 5)
		{
			if(pmt<16) Ebbc=true;
			if(pmt>23 && pmt<40) Wbbc=true;
		}
	}
	myEvent->bbcTrigger = (Ebbc && Wbbc);
	
	
	//now for the tracks
	for(unsigned int vertex_index=0; vertex_index<myEvent->nVertices; vertex_index++){
		muDst->setVertexIndex(vertex_index);
		TObjArray* primaryTracks = muDst->primaryTracks();
		StMuTrack* track;
		StMuTrack* global;
		int nentries = muDst->numberOfPrimaryTracks();
		assert(nentries==primaryTracks->GetEntries());
		for(int i=0; i<nentries; i++){
			track = muDst->primaryTracks(i);
			global = track->globalTrack();
			if(!global){
				LOG_WARN << "no global found for R"<<myEvent->run<< ", event "<<myEvent->event<<", key "<<track->id()<<", so skip it"<<endm;
				continue;
			}
			
			if(track->pt()<1.) continue;
			
			//basics
			pion->pt			= track->pt();
			pion->eta			= track->eta();
			pion->phi			= track->phi();
			pion->nHits			= track->nHits();
			pion->nFitPoints	= track->nHitsFit();
			pion->nDedxPoints	= track->nHitsDedx();
			pion->nHitsPossible = track->nHitsPoss();
			pion->charge		= track->charge();
			pion->chi2			= track->chi2();
			pion->key			= track->id();
			pion->flag			= track->flag();
			
			//global info
			pion->globalPt		= global->pt();
			pion->globalEta		= global->eta();
			pion->globalPhi		= global->phi();
			
			//dedx info
			pion->dEdx			= track->dEdx();
			pion->nSigmaPion	= track->nSigmaPion();
			pion->nSigmaElectron= track->nSigmaElectron();
			pion->nSigmaProton	= track->nSigmaProton();
			pion->nSigmaKaon	= track->nSigmaKaon();
			
			pion->vertexIndex = vertex_index;

			//TVector3 stuff (vertex,dca,first,last)
			StThreeVectorF stdca = track->dcaGlobal(pion->vertexIndex);
			pion->dca.SetXYZ(stdca.x(),stdca.y(),stdca.z());
			
			StThreeVectorF stfp = global->firstPoint();
			pion->firstPoint.SetXYZ(stfp.x(),stfp.y(),stfp.z());
			
			StThreeVectorF stlp = global->lastPoint();
			pion->lastPoint.SetXYZ(stlp.x(),stlp.y(),stlp.z());
			
			LOG_DEBUG<<"adding track with pt "<<pion->pt<<endm;
			myEvent->addTrack(pion);
			pion->Clear();
		}
	}
	pionTree->Fill();
	
	LOG_DEBUG << "StChargedPionAnalysisMaker::Make() == kStOk" << endm;
	return kStOK;
}

void StChargedPionAnalysisMaker::Clear(Option_t* option)
{	
	myEvent->Clear();
}

Int_t StChargedPionAnalysisMaker::Finish()
{
	myFile->cd();
	pionTree->Write();
	myFile->Close();
	LOG_INFO << "StChargedPionAnalysisMaker::Finish() == kStOk"<<endm;
	return kStOk;
}

int StChargedPionAnalysisMaker::makeSimulatedData()
{	
	//get ~useful pointers (StMcEvent is only used to check quantities already available from geant!)
	StMcEvent* mcEvent			= dynamic_cast<StMcEvent*>(GetDataSet("StMcEvent")); assert(mcEvent);
	
	TDataSet* geantDst = GetDataSet("geant");
	TDataSetIter geantDstIter(geantDst);
	
	St_particle* particleTabPtr	= dynamic_cast<St_particle*>(geantDstIter("particle"));
	particle_st* particleTable	= particleTabPtr->GetTable();
	
	St_g2t_event* Pg2t_event	= dynamic_cast<St_g2t_event*>(geantDstIter("g2t_event")); assert(Pg2t_event);
	g2t_event_st* g2t_event1	= Pg2t_event->GetTable(); assert(g2t_event1);
	
	St_g2t_pythia* Pg2t_pythia	= dynamic_cast<St_g2t_pythia*>(geantDstIter("g2t_pythia")); assert(Pg2t_pythia);
	g2t_pythia_st* g2t_pythia1	= Pg2t_pythia->GetTable(); assert(g2t_pythia1);
	
	unsigned short pid			= mcEvent->subProcessId();
	unsigned int eventNumber	= mcEvent->eventNumber();
	
	unsigned short geantPID		= g2t_event1->subprocess_id;
	unsigned int geantID		= g2t_event1->n_event; 
	
	//basic PYTHIA kinematic quantities
	float s				= g2t_pythia1->mand_s;
	float t				= g2t_pythia1->mand_t;
	float u				= g2t_pythia1->mand_u;
	float partonic_pt	= g2t_pythia1->hard_p;
	float cos_theta		= g2t_pythia1->cos_th;
	float x1			= g2t_pythia1->bjor_1;
	float x2			= g2t_pythia1->bjor_2;
	double Q2			= partonic_pt*partonic_pt;
	
	
	LOG_DEBUG << Form("s=%f,t=%f,u=%f,partonic_pt=%f,cos_theta=%f,x1=%f,x2=%f",s,t,u,partonic_pt,cos_theta,x1,x2) << endm;
	
	//get flavor of partons after intial radiation before scattering and then after scattering
	int flavor[4];
	flavor[0] = particleTable[4].idhep;
	flavor[1] = particleTable[5].idhep;
	flavor[2] = particleTable[6].idhep;
	flavor[3] = particleTable[7].idhep;
	
	LOG_DEBUG << Form("PID/evtid from McEvent = %d,%d; PID/evtid from Table = %d,%d:",pid,eventNumber,geantPID,geantID) << endm;	
	LOG_DEBUG << Form("row |   id   |   px   |   py   |   pz   |   E   |   m   | status | moth1 | moth2 | daught1 | daught2 |") << endm;
	for (int i=0; i<particleTabPtr->GetNRows(); i++) {
		LOG_DEBUG << Form("  %d,  %d,  %f,   %f,   %f,   %f,   %f,   %d,   %d,   %d,   %d,   %d",i,particleTable[i].idhep, particleTable[i].phep[0], particleTable[i].phep[1], particleTable[i].phep[2] , particleTable[i].phep[3], particleTable[i].phep[4], particleTable[i].isthep , particleTable[i].jmohep[0], particleTable[i].jmohep[1], particleTable[i].jdahep[0], particleTable[i].jdahep[1]) << endm;
	}
		
	myEvent->subProcessID = (pid==geantPID) ? pid:0;
	myEvent->event2 = (eventNumber==geantID) ? eventNumber:0;
	
	myEvent->parton1[0]		= particleTable[6].phep[0]; //px
	myEvent->parton1[1]		= particleTable[6].phep[1]; //py
	myEvent->parton1[2]		= particleTable[6].phep[2]; //pz
	myEvent->parton1[3]		= particleTable[6].phep[3]; //E
	myEvent->parton1[4]		= particleTable[6].phep[4]; //m
	myEvent->parton1[5]		= particleTable[6].isthep; //status
	myEvent->parton1[6]		= particleTable[6].jmohep[0]; //moth1
	myEvent->parton1[7]		= particleTable[6].jmohep[1]; //moth2
	myEvent->parton1[8]		= particleTable[6].jdahep[0]; //daughter1
	myEvent->parton1[9]		= particleTable[6].jdahep[1]; //daughter2
	
	myEvent->parton2[0]		= particleTable[7].phep[0]; //px
	myEvent->parton2[1]		= particleTable[7].phep[1]; //py
	myEvent->parton2[2]		= particleTable[7].phep[2]; //pz
	myEvent->parton2[3]		= particleTable[7].phep[3]; //E
	myEvent->parton2[4]		= particleTable[7].phep[4]; //m
	myEvent->parton2[5]		= particleTable[7].isthep; //status
	myEvent->parton2[6]		= particleTable[7].jmohep[0]; //moth1
	myEvent->parton2[7]		= particleTable[7].jmohep[1]; //moth2
	myEvent->parton2[8]		= particleTable[7].jdahep[0]; //daughter1
	myEvent->parton2[9]		= particleTable[7].jdahep[1]; //daughter2	
	
	myEvent->flavor[0]		= flavor[0];
	myEvent->flavor[1]		= flavor[1];
	myEvent->flavor[2]		= flavor[2];
	myEvent->flavor[3]		= flavor[3];
	
	myEvent->s				= s;
	myEvent->t				= t;
	myEvent->u				= u;
	myEvent->partonic_pt	= partonic_pt;
	myEvent->cos_theta		= cos_theta;
	myEvent->x1				= x1;
	myEvent->x2				= x2;
	myEvent->Q2				= Q2;

	//Get partonic a_LL, polarized/unpolarized pdfs using Q2 = partonic_pT^2
	myEvent->partonic_all	= getPartonicALL(s,t,u,pid,flavor[0],flavor[1],flavor[2],flavor[3]);
	
	myEvent->df1[0]			= getPolPDF(flavor[0],x1,Q2,101); //LO std
	myEvent->df1[1]			= getPolPDF(flavor[0],x1,Q2,102); //NLO std
	myEvent->df1[2]			= getPolPDF(flavor[0],x1,Q2,103); //NLO g0
	myEvent->df1[3]			= getPolPDF(flavor[0],x1,Q2,104); //NLO max
	myEvent->df1[4]			= getPolPDF(flavor[0],x1,Q2,105); //NLO min
	
	myEvent->df2[0]			= getPolPDF(flavor[1],x2,Q2,101);
	myEvent->df2[1]			= getPolPDF(flavor[1],x2,Q2,102);
	myEvent->df2[2]			= getPolPDF(flavor[1],x2,Q2,103);
	myEvent->df2[3]			= getPolPDF(flavor[1],x2,Q2,104);
	myEvent->df2[4]			= getPolPDF(flavor[1],x2,Q2,105);
	
	myEvent->f1[0]			= getUnPolPDF(flavor[0],x1,Q2,3); //LO
	myEvent->f1[1]			= getUnPolPDF(flavor[0],x1,Q2,1); //NLO Msbar
	
	myEvent->f2[0]			= getUnPolPDF(flavor[1],x2,Q2,3);
	myEvent->f2[1]			= getUnPolPDF(flavor[1],x2,Q2,1);
	
	return 0;
}

double StChargedPionAnalysisMaker::getPartonicALL(double s, double t, double u, int sub, int inA, int inB, int outA, int outB){
	
	//Werner definitions:
	//1: qq'->qq' (qqbar'->qqbar') 2: qq->qq   3: qqbar->q'qbar'  4: qqbar->qqbar 5: qqbar->gg   6: gg->qqbar  7: qg->qg   8: gg->gg
	//PYTHIA definitions:
	//1: 11a                       2:11b       3: 12a             4:12b           5: 13          6: 53         7: 28       8: 68
	//NOTES:
	// 3==5==6==-1  1==7  1!=2 and 1!=4
	
	double N1,N2,N3,N4,N5,N6,N7,N8;
	double D1,D2,D3,D4,D5,D6,D7,D8;
	double all=-10;
	
	num_(&s,&t,&u,&N1,&N2,&N3,&N4,&N5,&N6,&N7,&N8);
	denom_(&s,&t,&u,&D1,&D2,&D3,&D4,&D5,&D6,&D7,&D8);
	LOG_DEBUG<<"s="<<s<<" t="<<t<<" u="<<u<<" sub="<<sub<<" inA="<<inA<<" inB="<<inB<<" outA="<<outA<<" outB="<<outB<<endm;
	LOG_DEBUG<<" 1="<<N1<<" "<<D1<<endm;
	LOG_DEBUG<<" 2="<<N2<<" "<<D2<<endm;
	LOG_DEBUG<<" 3="<<N3<<" "<<D3<<endm;
	LOG_DEBUG<<" 4="<<N4<<" "<<D4<<endm;
	LOG_DEBUG<<" 5="<<N5<<" "<<D5<<endm;
	LOG_DEBUG<<" 6="<<N6<<" "<<D6<<endm;
	LOG_DEBUG<<" 7="<<N7<<" "<<D7<<endm;
	LOG_DEBUG<<" 8="<<N8<<" "<<D8<<endm;
	
	
	if ((sub==11)&&(abs(inA)!=abs(inB))) all=N1/D1;
	if ((sub==11)&&(abs(inA)==abs(inB))) all=N2/D2;
	if ((sub==12)&&(abs(inA)!=abs(outA))) all=N3/D3;
	if ((sub==12)&&(abs(inA)==abs(outA))) all=N4/D4;
	if (sub==13) all=N5/D5;
	if (sub==53) all=N6/D6;
	if (sub==28) all=N7/D7;
	if (sub==68) all=N8/D8;
	   
	return all;
}

double StChargedPionAnalysisMaker::getPolPDF(int flavor, double x, double Q2, int set){
	LOG_DEBUG<<"getPolPDF: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" polset="<<set<<endm;
	
	double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
	double pdf=1000;
	int polset=set;
	int polid=0;
	
	polar_(&polset, &x, &Q2, parpol, &polid);
	LOG_DEBUG <<"getPolPDF: U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endm;
	
	if (flavor==1) pdf=parpol[1];      //dv + dsea quark
	if (flavor==2) pdf=parpol[0];      //uv + usea quark
	if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
	if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
	if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
	if (flavor==21) pdf=parpol[5];     //gluon
	if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];
	
	return pdf;
}

double StChargedPionAnalysisMaker::getUnPolPDF(int flavor, double x, double Q2, int set){
	LOG_DEBUG<<"getUnPolPDF: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" iset="<<set<<endm;
	
	double pdf=0.0;
	int iset=set;
	int er=0;
	int fl=10;
	
	if (flavor==1) fl=2;
	if (flavor==2) fl=1;
	if (flavor==-1) fl=-2;
	if (flavor==-2) fl=-1;
	if (flavor==21) fl=0;
	if (flavor==3) fl=3;
	if (flavor==-3) fl=-3;
	if (flavor==4) fl=4;
	if (flavor==-4) fl=-4;
	if (flavor==5) fl=5;
	if (flavor==-5) fl=-5;
	
	double Q=pow(Q2,0.5);
	pdf=ctq5pd_(&iset,&fl,&x,&Q,&er);
	if (er!=0) pdf=0.0;
	
	return pdf;
}

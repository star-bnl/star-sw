#include <string>
#include "Riostream.h"
#include <fstream>
#include <math>
//#define BFC
class StMuDstMaker;


void spinJet_short(  
		//const char *dir = "/star/data14/reco/ppTrans-1/FullField/P03ie/2003/123/", 
	     //const char *outname = "/star/u/dthein/test2/ppLong146_tpc_22",

const char* file="/star/data22/reco/ppLong-1/FullField/P03ie/2003/137/st_physics_4137012_raw_0020024.MuDst.root",
	 //    const char *file = "/star/data22/reco/ppLong-1/FullField/P03ie/2003/137/st_physics_4137012_raw_0010001.MuDst.root", 
	     const char* runnum= "",
	     const char *outname = "/star/u/dthein/test5/emc_jets/",
	     const char *filter = ".MuDst.root",
	     const int nFiles=1,
	     const int nevents=3900
	     ) {
	if (gClassTable->GetID("TTable") < 0) {
		gSystem->Load("libStar");
		gSystem->Load("libPhysics");
	}     
#ifdef BFC
	gROOT->LoadMacro("bfc.C");
	TString Chain("db CMuDst Tree McEvent");
	bfc(-1,Chain.Data(),file,0,0);
#else
	gSystem->Load("St_base");
	gSystem->Load("StChain");
	gSystem->Load("St_Tables");
	gSystem->Load("StMagF");
	gSystem->Load("StUtilities");  // new addition 22jul99
	gSystem->Load("StTreeMaker");
	gSystem->Load("StIOMaker");
	gSystem->Load("StarClassLibrary");
	gSystem->Load("StTpcDb");
	gSystem->Load("StDbUtilities");
	gSystem->Load("StEvent");
	gSystem->Load("StEventUtilities"); 
	gSystem->Load("StMcEvent"); 
	gSystem->Load("StMcEventMaker"); 
	gSystem->Load("StAssociationMaker");
	gSystem->Load("StBFChain");
	gSystem->Load("StMcAnalysisMaker");
	gSystem->Load("StStrangeMuDstMaker");
	gSystem->Load("StEmcUtil");
	gSystem->Load("StMuDSTMaker");
	gSystem->Load("StAnalysisMaker");

	gSystem->Load("StJetFinder");
	gSystem->Load("StSpinMaker");

	// libraries to get hit energies
	gSystem->Load("StDaqLib");
	gSystem->Load("StDbLib");
	gSystem->Load("StDbBroker");
	gSystem->Load("St_db_Maker");

	gSystem->Load("StEmcADCtoEMaker");
	gSystem->Load("StPreEclMaker");
	gSystem->Load("StEpcMaker");
#endif

	///////////////////////////////////////////////////////////////////

	ifstream in; 
	in.open("luminosity.text"); 

	struct frisbee {
		int fill;
		int run;
		int lum;
	} disk[500];
	struct mouse {
		int fill;
		int run;
		float Bpol;
		float Ypol;
	} catcat[500];

	int a_counter=0;
	int b_counter=0;
	while (1){
		if (!in.good()) break;
		in >> disk[a_counter].fill >> disk[a_counter].run >>disk[a_counter].lum;
		a_counter++; 
	}
	a_counter--;

	in.close();
	in.open("dogdog"); 

	while (1){
		if (!in.good()) break;
		in >> catcat[b_counter].fill >> catcat[b_counter].run >>catcat[b_counter].Bpol>>catcat[b_counter].Ypol;
		b_counter++; 
	}
	b_counter--;
	in.close();


	//////////////////////////////////////////////////////////////////

	int dA_min1_trigger=2001;
	int dA_min3_trigger=2003;
	int dA_ht1_trigger=2201;
	int dA_ht2_trigger=2202;

	int pp_min1_trigger=1000;
	int pp_min3_trigger=1000;
	int pp_ht1_trigger=1101;
	int pp_ht2_trigger=1102;
	int pp_jp1_trigger=1201;
	int pp_jp2_trigger=1202;

	const int nTrigs=10;
	int our_triggers[nTrigs];
	int t_prescale[nTrigs];
	int t_hold[nTrigs];

	our_triggers[0]=dA_min1_trigger;
	our_triggers[1]=dA_min3_trigger;
	our_triggers[2]=dA_ht1_trigger;
	our_triggers[3]=dA_ht2_trigger;
	our_triggers[4]=pp_min1_trigger;
	our_triggers[5]=pp_min3_trigger;
	our_triggers[6]=pp_ht1_trigger;
	our_triggers[7]=pp_ht2_trigger;
	our_triggers[8]=pp_jp1_trigger;
	our_triggers[9]=pp_jp2_trigger;

	TNtuple* trigger_counter=new TNtuple ("trigger_counter","trigger_counter","min:ht1:ht2:jt1:jt2:z:run:bXing:time:nJets:eventId");

	TNtuple* calor=new TNtuple ("calor","calor","eP:etP:eJ:etJ:nP:nJ:etPJ:ht1:nTracks:eventId:fill");
	TNtuple* points=new TNtuple ("points","points","e:phi:eta:eventId:fill:ephi:eeta:run:time:bXing");

	TNtuple* min_Jets= new TNtuple ("min_Jets","min_Jets","Pt:phi:eta:run:z:nJets:nTracks:Et:emcTrig:emcEt:nPoints:fill:eventId:bXing:time");
	TNtuple* ht1_Jets= new TNtuple ("ht1_Jets","ht1_Jets","Pt:phi:eta:run:z:nJets:nTracks:Et:emcTrig:emcEt:nPoints:fill:eventId:bXing:time");
	TNtuple* ht2_Jets= new TNtuple ("ht2_Jets","ht2_Jets","Pt:phi:eta:run:z:nJets:nTracks:Et:emcTrig:emcEt:nPoints:fill:eventId:bXing:time");
	TNtuple* jp1_Jets= new TNtuple ("jp1_Jets","jp1_Jets","Pt:phi:eta:run:z:nJets:nTracks:Et:emcTrig:emcEt:nPoints:fill:eventId:bXing:time");
	TNtuple* jp2_Jets= new TNtuple ("jp2_Jets","jp2_Jets","Pt:phi:eta:run:z:nJets:nTracks:Et:emcTrig:emcEt:nPoints:fill:eventId:bXing:time");
	/*
	TNtuple* min_eJets= new TNtuple ("min_eJets","min_eJets","Pt:phi:eta:spin:run:z:nJets:nCells:Et:emcTrig:emcEt:nPoints:Bpol:Ypol:bXing");
	TNtuple* ht1_eJets= new TNtuple ("ht1_eJets","ht1_eJets","Pt:phi:eta:spin:run:z:nJets:nCells:Et:emcTrig:emcEt:nPoints:Bpol:Ypol:bXing");
	TNtuple* ht2_eJets= new TNtuple ("ht2_eJets","ht2_eJets","Pt:phi:eta:spin:run:z:nJets:nCells:Et:emcTrig:emcEt:nPoints:Bpol:Ypol:bXing");
	*/

	double pi = atan(1.0)*4.0;
	double twoPi = 2.0*pi;

	cout << " loading done " << endl;
#ifndef BFC
	StChain* chain = new StChain("StChain"); 
#endif
	chain->SetDebug(1);

	//TString dir="/star/u/dthein/test4/runs_short/";
	//dir+=runnum;
	//dir+="/";
	TString file_name(file);
	int where_File=file_name.Index("st_physics");
	char* thing1=file_name;
	TString thing2;

for(int i=where_File +11;i<where_File+30;i++){
	thing2+=thing1[i];
}

cout<<"Root blows: "<<thing2<<endl;
	
/*	thing2=file_name.substr(22,8);
	thing1+=thing2;
	thing1+=".root";
*/	
	StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",file,filter,nFiles,"MuDst");

        StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");
	St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");
	StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
        adc->setPrint(kFALSE);
	preEcl = new StPreEclMaker("preEcl");
	preEcl->setPrint(kFALSE);
	StEpcMaker *epc = new StEpcMaker();
	//StAnalysisMaker *analysisMaker = new StAnalysisMaker( runnum );

	StEmcTpcFourPMaker* emcFourPMaker = new StEmcTpcFourPMaker("EmcTpcFourPMaker", muDstMaker, 30, 30, .3, .3, .04);
	StJetMaker* jetMaker = new StJetMaker("emcJetMaker", emcFourPMaker, muDstMaker, "/dev/null");
	jetMaker->SetSaveEventWithNoJets(false);
	jetMaker->SetStoreEMC(false);
	emcFourPMaker->SetDepRatios(0.0,0.0,0.0,1.0,0.3);
	//emcFourPMaker->setUseType(StEmcTpcFourPMaker::Clusters);
	emcFourPMaker->setUseType(StEmcTpcFourPMaker::Hits);
	//emcFourPMaker->setUseType(StEmcTpcFourPMaker::Points);

	/*
	StTpcFourPMaker* fourPMaker = new StTpcFourPMaker("FourPMaker", muDstMaker);
	StJetMaker* jetMaker = new StJetMaker("JetMaker", fourPMaker, muDstMaker, "/dev/null");
	jetMaker->SetSaveEventWithNoJets(false);
	jetMaker->SetStoreEMC(false);
	*/

	StppMikeConeJetAnalyzer* mcanalyzer = new StppMikeConeJetAnalyzer
		(56, -1.6, 1.6, 120, -atan(1.0)*4.0, atan(1.0)*4.0);
	mcanalyzer->setConeR(0.7);
	mcanalyzer->setConeSeedEtMin(0.5);
	mcanalyzer->setConeAssocEtMin(0.35);
	mcanalyzer->setPerformMinimization(true);
	mcanalyzer->setAddMidpoints(true);
	mcanalyzer->setDoSplitMerge(false);
	//mcanalyzer->setDoSplitMerge(true);
	//mcanalyzer->setDoMidpointFix(true);
	//mcanalyzer->setRequireStableMidpoints(true);
	mcanalyzer->setNhits(15);
	mcanalyzer->setDebug(false);
	mcanalyzer->setCutPtMin(0.35);
	mcanalyzer->setAbsEtaMax(1.6);
	mcanalyzer->setSeedEtMin(1.0);
	mcanalyzer->setAssocEtMin(.35);
	mcanalyzer->setEmcAccepted(false);
	mcanalyzer->setTpcAccepted(false);
	mcanalyzer->setFpdAccepted(false);
	mcanalyzer->setJetPtMin(3.0); // GeV/c2
	mcanalyzer->setJetEtaMax(100.0);
	mcanalyzer->setJetEtaMin(0);
	mcanalyzer->setJetNmin(0);

     	jetMaker->addAnalyzer(mcanalyzer, "MkConeJets");

	chain->Init(); // This should call the Init() method in ALL makers
	chain->PrintInfo();

int temp_counter=0;


	for (Int_t iev=0;iev<nevents; iev++) {
		cout << "Working on eventNumber " << iev << endl;
		chain->Clear();
		int iret = chain->Make(iev); // This should call the Make() method in ALL makers    
		if (iret) {
			cout<<"broke"<<endl;
			break;
		}


		////////////////////////////////////////////////////////////////////
/*
		if(iev>0 && iev%25000==0){

	temp_counter++;
	
		  TString ishmael(outname);
		  
		  ishmael+="temps/";
		  ishmael+=runnum;		  
		  ishmael+="_temp";
		  ishmael+=temp_counter;
		  ishmael+=".root";

		  TFile *insurance = new TFile(ishmael, "RECREATE");


		points->Write();
		calor->Write();
		trigger_counter->Write();
		min_Jets->Write();
		ht1_Jets->Write();
		ht2_Jets->Write();
		jp1_Jets->Write();
		jp2_Jets->Write();
		//min_eJets->Write();
		//ht1_eJets->Write();
		//ht2_eJets->Write();

		insurance->Write();
		insurance->Close();
		}
		////////////////////////////////////////////


*/
		cout<<"entered fine"<<endl;


		StTriggerId& trigger = muDstMaker->muDst()->event()->triggerIdCollection().nominal();


		for(int i=0;i<nTrigs;i++){
			t_hold[i]=0;
			t_prescale[i]=0;
			if(trigger.isTrigger(our_triggers[i]) && ( muDstMaker->muDst()->numberOfPrimaryTracks() > 0 )){
			  t_hold[i]=1;
			  t_prescale[i]=trigger.prescaleVersion(our_triggers[i]);
			  //cout<<"Prescale["<<i<<"]: "<<t_prescale[i]<<endl;
			}
		}

		int run=muDstMaker->muDst()->event()->runId();
		int eventId=muDstMaker->muDst()->event()->eventId();
		int runAkio=muDstMaker->muDst()->event()->eventInfo().runId();
		int time=muDstMaker->muDst()->event()->eventInfo().time();
		StL0Trigger& spin_trigger = muDstMaker->muDst()->event()->l0Trigger();
		int bXing=spin_trigger.bunchCrossingId7bit(runAkio);		
		StThreeVectorF vertex=muDstMaker->muDst()->event()->primaryVertexPosition();

		StJets* jetSet = jetMaker->firstanalyzer()->getmuDstJets();
		int numberJets = jetSet->nJets();
		cout << endl <<"Number Jets found: " << numberJets << endl;

		trigger_counter->Fill(t_hold[0]+t_hold[1]+t_hold[4]+t_hold[5],t_hold[2]+t_hold[6],t_hold[3]+t_hold[7],t_hold[8],t_hold[9],vertex.z(),run,bXing,time,numberJets,eventId);
		

		if(numberJets ==0 ) continue;


		int nTracks=muDstMaker->muDst()->numberOfPrimaryTracks();
		if(nTracks <= 0) continue;
	
	        //StAnalysisMaker *analysisMaker = new StAnalysisMaker( runnum );
	        //int reTrigger=analysisMaker->checkTrigger();	
		
	        int reTrigger=adc->isTowerTriggered;	
		int wasTriggered=adc->wasTowerTriggered;
		int adcCount=adc->adcCount;

		if(adcCount>10){ continue;}
		
		StMuEmcCollection* Points=muDstMaker->muDst()->emcCollection();
		//int checkTrigger=0;
		/*
		int adcCount=0;	
		for(int k=1;k<=2400;k++){
		       	int tower_adc=Points->getTowerADC(k);
			//if( tower_adc>277 ){ checkTrigger++;}	
		for (int n=1;n<16;n++){
			if(tower_adc==n*256){adcCount++;break;}
		}}
		if(adcCount>50){continue;}
		*/
		/////////////////////////////////////////////////////////////////


		int fill=0;


		int lum=0;
		for(int j=0;j<a_counter;j++){
			if (run==disk[j].run){
				lum=disk[j].lum;
				fill=disk[j].fill;
				break;
			}
		}



		//so polarization only works if the luminosity was found....

		float Bpol=0;
		float Ypol=0;
		for(int j=0;j<b_counter;j++){
			if (fill==catcat[j].fill){
				Bpol=catcat[j].Bpol;
				Ypol=catcat[j].Ypol;
				break;
			}
		}
		if( (Bpol+Ypol)==0 ){cout<<"!!!!!No Polarization found for Fill!!!!!!"<<endl;}

		/////////////////////////////////////////////////////////////////

//		StMuEmcCollection* Points=muDstMaker->muDst()->emcCollection();
		/*
     		for(int k=0;k<2400;k++){
		  int tower_adc=Points->getTowerADC(k);
		  if(tower_adc<50) continue;
		  adcs.Fill(k,tower_adc,eventId);
		}
		*/

		StMuEmcPoint *p;

		int numberPoints=Points->getNPoints();
		cout<<"Number of Points: "<<numberPoints<<endl;

		const int total=1000;
		Float_t e[total],phi[total],r[total],sintheta[total],eta[total];
		float e_cal,et_cal,e_cluster_eta,e_cluster_phi;
		int nClusters[total];

		e_cal=0;
		et_cal=0;

		for (int count=0; count<numberPoints; count++) {

		  p=Points->getPoint(count);
		  e[count]=p->getEnergy();
		  phi[count]=p->getPhi();
		  eta[count]=p->getEta();
		  
		  e_cluster_eta=0;e_cluster_phi=0;
		  if (p->getCluster(3)) e_cluster_eta=p->getCluster(3)->getEnergy();
		  if (p->getCluster(4)) e_cluster_phi=p->getCluster(4)->getEnergy();
		  
		  e_cal+=e[count];
		  sintheta[count]=sqrt(1-tanh(eta[count])*tanh(eta[count]));
		  et_cal+=e[count]*sintheta[count];
		  
		  points->Fill(e[count],phi[count],eta[count],eventId,fill,e_cluster_phi,e_cluster_eta,run,time,bXing);		
		}
		////////////////////////////////////////////////////////





		//////////////////////////////////////////////////////////////////


		float eta_dif,phi_dif,ep_dist;
		float total_fake_emc_jet_Et=0;
		double total_et0=0;
		double total_e_et0=0;
		
		
		for(int k=0;k<numberJets;k++){
			double et0=jetSet->et(k);
			double pt0=jetSet->pt(k);
			int nCells=jetSet->nCell(k);
			double phi0=jetSet->phi(k);
			double eta0=jetSet->eta(k);
			int towerTriggered=0;
			float fake_emc_jet_Et=0;
			int points_in_jet=0;
      			total_et0+=et0;

			cout<<endl<<"Et-Pt of Jet: "<<(et0-pt0)<<endl;


			//points can be used in several jets.

			for (int count=0; count<numberPoints; count++) {
				eta_dif=(eta0-eta[count])*(eta0-eta[count]);
				phi_dif=(phi0-phi[count])*(phi0-phi[count]);
				ep_dist=sqrt(eta_dif+phi_dif);
				if (ep_dist<0.7){
					points_in_jet+=1;
					fake_emc_jet_Et+=e[count]*sintheta[count];
					if ((e[count]*sintheta[count])>2.4) towerTriggered+=1;
				}

			}

			total_fake_emc_jet_Et+=fake_emc_jet_Et;
			
			//towerTriggered=reTrigger;
			towerTriggered=wasTriggered;
			nCells=reTrigger;
			
			int exclusive=0;//just another way to write else if's
			if(t_hold[0] || t_hold[1] || t_hold[4] || t_hold[5] ){
				exclusive=1;
				min_Jets->Fill(pt0,phi0,eta0,run,vertex.z(),numberJets,nCells,et0,towerTriggered,fake_emc_jet_Et,points_in_jet,fill,eventId,bXing,time);}
			if((t_hold[2] || t_hold[6]) && exclusive==0){
				exclusive=1;
				ht1_Jets->Fill(pt0,phi0,eta0,run,vertex.z(),numberJets,nCells,et0,towerTriggered,fake_emc_jet_Et,points_in_jet,fill,eventId,bXing,time);}
				
			if((t_hold[3] || t_hold[7]) && exclusive==0){
				ht2_Jets->Fill(pt0,phi0,eta0,run,vertex.z(),numberJets,nCells,et0,towerTriggered,fake_emc_jet_Et,points_in_jet,fill,eventId,bXing,time);
				exclusive=1;}
			if(t_hold[8] && exclusive==0){
			  jp1_Jets->Fill(pt0,phi0,eta0,run,vertex.z(),numberJets,nCells,et0,towerTriggered,fake_emc_jet_Et,points_in_jet,fill,eventId,bXing,time);
			  exclusive=1;}
			if(t_hold[9] && exclusive==0){
				exclusive=1;
			  jp2_Jets->Fill(pt0,phi0,eta0,run,vertex.z(),numberJets,nCells,et0,towerTriggered,fake_emc_jet_Et,points_in_jet,fill,eventId,bXing,time);
			}
			
		}//end jet loop
		

//////////////////////////////////////////////////////////////////////////////////////////////////////////





		calor->Fill(e_cal,et_cal,0,total_et0,numberPoints,numberJets,total_fake_emc_jet_Et,t_hold[2]+t_hold[6],nTracks,eventId,fill);



	} // Event Loop


	chain->Finish(); // This should call the Finish() method in ALL makers


	{
		TString oName(outname);
		//oName += runnum;
		oName +=thing2;
		oName += ".root";
		TFile *outfile = new TFile(oName, "RECREATE");

		points->Write();
		calor->Write();
		trigger_counter->Write();
		min_Jets->Write();
		ht1_Jets->Write();
		ht2_Jets->Write();
		jp1_Jets->Write();
		jp2_Jets->Write();
		//min_eJets->Write();
		//ht1_eJets->Write();
		//ht2_eJets->Write();

		//outfile->Write();
		outfile->Close();
	}

}


int spinBitter(int fill,int bXing)
{

  if(bXing==20 || bXing==60) return 0;//kicked bunches
  //board 5 corrupts 0,2,4

        int pat1[]={  
	  51, 0, 83, 0, 53, 0, 85, 0, 
	  51, 0, 83, 0, 53, 0, 85, 0, 
	  51, 0, 83, 0, 53, 0, 85, 0, 
	  51, 0, 83, 0, 53, 0,  5, 0, 
	  03, 0,  3, 0,  5, 0,  5, 0,
	  51, 0, 83, 0, 53, 0, 85, 0, 
	  51, 0, 83, 0, 53, 0, 85, 0, 
	  51, 0, 83, 0, 53, 0, 85, 0, 
	  51, 0, 83, 0, 53, 0, 85, 0, 
	  51, 0, 83, 0, 53, 0, 85, 0, 
	  51, 0, 83, 0, 53, 0, 85, 0, 
	  51, 0, 83, 0, 53, 0, 85, 0, 
	  51, 0, 83, 0, 53, 0, 85, 0, 
	  51, 0, 83, 0, 53, 0, 80, 0, 
	  48, 0, 80, 0, 48, 0, 80, 0};
       
        int pat2[]={
	  51, 0, 85, 0, 51, 0, 85, 0,
	  51, 0, 85, 0, 51, 0, 85, 0,
	  51, 0, 85, 0, 51, 0, 85, 0,
	  51, 0, 85, 0, 51, 0,  5, 0,
	  3, 0,  5, 0,  3, 0,  5, 0,
	  51, 0, 85, 0, 51, 0, 85, 0,
	  51, 0, 85, 0, 51, 0, 85, 0,
	  51, 0, 85, 0, 51, 0, 85, 0,
	  51, 0, 85, 0, 51, 0, 85, 0,
	  51, 0, 85, 0, 51, 0, 85, 0,
	  51, 0, 85, 0, 51, 0, 85, 0,
	  51, 0, 85, 0, 51, 0, 85, 0,
	  51, 0, 85, 0, 51, 0, 85, 0,
	  51, 0, 85, 0, 51, 0, 80, 0, 
	  48, 0, 80, 0, 48, 0, 80, 0};
	int pat3[]={
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  51, 0, 53, 0, 51, 0,  5, 0, 
	  3, 0,  5, 0,  3, 0,  5, 0,
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  51, 0, 53, 0, 83, 0, 80, 0, 
	  48, 0, 48, 0, 80, 0, 80, 0};
	int pat4[]={
	  53, 0, 51, 0, 85, 0, 83, 0, 
	  53, 0, 51, 0, 85, 0, 83, 0, 
	  53, 0, 51, 0, 85, 0, 83, 0, 
	  53, 0, 51, 0, 53, 0,  3, 0, 
	  5, 0,  3, 0,  5, 0,  3, 0,
	  53, 0, 51, 0, 85, 0, 83, 0, 
	  53, 0, 51, 0, 85, 0, 83, 0, 
	  53, 0, 51, 0, 85, 0, 83, 0, 
	  53, 0, 51, 0, 85, 0, 83, 0, 
	  53, 0, 51, 0, 85, 0, 83, 0, 
	  53, 0, 51, 0, 85, 0, 83, 0, 
	  53, 0, 51, 0, 85, 0, 83, 0, 
	  53, 0, 51, 0, 85, 0, 83, 0, 
	  53, 0, 51, 0, 85, 0, 80, 0, 
	  48, 0, 48, 0, 80, 0, 80, 0};
	int pat5[]={
	  85, 0, 83, 0, 53, 0, 51, 0, 
	  153, 0, 85, 0, 83, 0, 53, 0, 
	  51, 0, 85, 0, 83, 0, 53, 0, 
	  51, 0, 85, 0, 83, 0,  5, 0, 
	  3, 0,  5, 0,  3, 0,  5, 0,
	  51, 0, 53, 0, 83, 0, 85, 0, 
	  153, 0, 51, 0, 53, 0, 83, 0, 
	  85, 0, 51, 0, 53, 0, 83, 0, 
	  85, 0, 51, 0, 53, 0, 83, 0, 
	  85, 0, 51, 0, 53, 0, 83, 0, 
	  85, 0, 51, 0, 53, 0, 83, 0, 
	  153, 0, 85, 0, 51, 0, 53, 0, 
	  83, 0, 51, 0, 85, 0, 83, 0, 
	  83, 0, 51, 0, 85, 0, 48, 0, 
	  80, 0, 80, 0, 48, 0, 48, 0};
	


	if (fill>=3612 && fill<=3614) { return pat1[bXing];}
	else if (fill>=3615 && fill<=3627) { return pat3[bXing];}
	else if (fill>=3634 && fill<=3793) { return pat1[bXing];}
	else if (fill>=3796 && fill<=3801) { return pat4[bXing];}
	else if (fill>=3803 && fill<=3810) { return pat5[bXing];}
	else {return -1;}

}


int Yeller(int Xspin){
	if (Xspin==51 || Xspin==83) return 1;
	if (Xspin==53 || Xspin==85) return 0;
	return -10;
}

int Bluer(int Xspin){
	if (Xspin==51 || Xspin==53) return 1;
	if (Xspin==83 || Xspin==85) return 0;
	return -10;
}





/*
   int numberTracks0 = jetSet->numTracks(0, jetMaker->event());
   StJet* jet0 = (StJet*) jetSet->jets()->At(0);
   StJet* jet1 = NULL;
   if(numberJets > 1)
   jet1 = (StJet*) jetSet->jets()->At(1);
   if((numberJets > 1) && (jet1->Pt() > jet0->Pt()))
   if((numberJets > 1) && (jet1->Pt() > jet0->Pt()))
   {
   StJet* temp = jet0;
   jet0 = jet1;
   jet1 = temp;
   }
 */

/* old code for single jet
   double pt0 = jet0->Pt();
   cout << "Pt0 : " << pt0 << endl;
   cout << "Multiplicity: " << jet0->nCell << endl;
   double phi0 = jet0->Phi();
   while(phi0 < 0) phi0 += twoPi;
   while(phi0 > twoPi) phi0 -= twoPi;
   double eta0 = jet0->Eta();
 */


/*    double phi1, eta1, dphi, deta;
      if(numberJets > 1)
      {
      phi1 = jet1->Phi();
      while(phi0 < 0) phi0 += twoPi;
      while(phi0 > twoPi) phi0 -= twoPi;
      dphi = phi1 - phi0;
      eta1 = jet1->Eta();
      deta = eta1 - eta0;
      }
 */








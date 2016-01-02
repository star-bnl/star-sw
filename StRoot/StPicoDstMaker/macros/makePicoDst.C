/* 
   root.exe 'lMuDst.C(-1,"./*MuDst.root","StEvent,RMuDst,mysql,tpcDb,magF,nodefault,CorrX,TRGDef,mtdMatch,mtdCalib,eemcD,emcAtoE,PreEcl,Epc")' makePicoDst.C+
 */
#include "TSystem.h"
#include "Riostream.h"

#include "StChain/StMaker.h"
#include "StBFChain/StBFChain.h" 
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h" 
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StPreEclMaker/StPreEclMaker.h"
#include "StEpcMaker/StEpcMaker.h"
StBFChain *chain;
void makePicoDst(const bool creatingPhiWgt = kFALSE, const int prodMod = 0, const int emcMode=1) {
  Int_t nEvents = 10000000;
#if 0
  	gSystem->Load("libTable");
  	gSystem->Load("libPhysics");
  	gSystem->Load("St_base");
  	gSystem->Load("StChain");
  	gSystem->Load("St_Tables");
  	gSystem->Load("StUtilities");        // new addition 22jul99
  	gSystem->Load("StTreeMaker");
  	gSystem->Load("StIOMaker");
  	gSystem->Load("StarClassLibrary");
  	gSystem->Load("StTriggerDataMaker"); // new starting from April 2003
  	gSystem->Load("StBichsel");
  	gSystem->Load("StEvent");
  	gSystem->Load("StEventUtilities");
  	gSystem->Load("StDbLib");
  	gSystem->Load("StEmcUtil");
  	gSystem->Load("StTofUtil");
  	gSystem->Load("StPmdUtil");
  	gSystem->Load("StPreEclMaker");
  	gSystem->Load("StStrangeMuDstMaker");
  	gSystem->Load("StMuDSTMaker");
#endif
	if(!creatingPhiWgt&&emcMode) {
	  //		gSystem->Load("StTpcDb");
#if 0
		gSystem->Load("StMcEvent");
		gSystem->Load("StMcEventMaker");
		gSystem->Load("StDaqLib");
		gSystem->Load("libgen_Tables");
 		gSystem->Load("libsim_Tables");
  		gSystem->Load("libglobal_Tables");
		gSystem->Load("StEmcTriggerMaker");
		//		gSystem->Load("StEmcUtil");//mine
		gSystem->Load("StEmcRawMaker");
		gSystem->Load("StEmcADCtoEMaker");
		gSystem->Load("StPreEclMaker");
		gSystem->Load("StEpcMaker");
		gSystem->Load("StEmcSimulatorMaker");
		//		gSystem->Load("StEmcUtil");
		//		gSystem->Load("StDbBroker");
		//		gSystem->Load("StDetectorDbMaker");
		//		gSystem->Load("StDbUtilities");
                gSystem->Load("StEEmcUtil");
                gSystem->Load("StEEmcDbMaker");
		//		gSystem->Load("St_db_Maker");
                gSystem->Load("StTriggerUtilities");
#endif
	}
        gSystem->Load("StPicoDstMaker");
	StMuDstMaker *MuDstMaker = (StMuDstMaker *) chain->Maker("MuDst");
        MuDstMaker->SetStatus("*",0);
        MuDstMaker->SetStatus("MuEvent",1);
        MuDstMaker->SetStatus("PrimaryVertices",1);
        MuDstMaker->SetStatus("PrimaryTracks",1);
        MuDstMaker->SetStatus("GlobalTracks",1);
        MuDstMaker->SetStatus("CovGlobTrack",1);
        MuDstMaker->SetStatus("BTof*",1);
        MuDstMaker->SetStatus("Emc*",1);
        MuDstMaker->SetStatus("MTD*",1);
	
	if(!creatingPhiWgt&&emcMode) {
	  
	  //		St_db_Maker *dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
		StEmcADCtoEMaker *adc2e = (StEmcADCtoEMaker *) chain->Maker("bemcA2E");
		//		adc2e->setPrint(false);
		adc2e->saveAllStEvent(true);
		StPreEclMaker *pre_ecl = (StPreEclMaker *) chain->Maker("preecl");
		pre_ecl->setPrint(kFALSE);
		StEpcMaker *epc= (StEpcMaker *)  chain->Maker("epc");
		epc->setPrint(kFALSE);
#if 0
    // Trigger simulator
    StTriggerSimuMaker* trigSimu = new StTriggerSimuMaker;
    trigSimu->setMC(false);
    trigSimu->useBemc();
    trigSimu->useEemc();
    trigSimu->useOnlineDB();
    trigSimu->bemc->setConfig(StBemcTriggerSimu::kOffline);
#endif
    
	}
	const Char_t *inputfile = chain->GetFileIn().Data();
	StPicoDstMaker *picoMaker = new StPicoDstMaker(1,inputfile,"picoDst");
	//        picoMaker->setRunNumber(runnumber);
        picoMaker->setProdMode(prodMod); // 0-mb, 1-central, 2-ht
        picoMaker->setEmcMode(emcMode); // 0-No EMC, 1-EMC ON
//        picoMaker->SetDebug(1);
	StMaker::lsMakers(chain);
	chain->Init();
	cout<<"chain->Init();"<<endl;
#if 0
	int total = 0;
	for (Int_t i=0; i<nEvents; i++){
	  if(i%100==0)
		cout << "Working on eventNumber " << i << endl;
		
	  chain->Clear();
	  int iret = chain->Make(i);
		
	  if (iret) { cout << "Bad return code!" << iret << endl; break;}

	  total++;
		
	}
	
	cout << "****************************************** " << endl;
	cout << "Work done... now its time to close up shop!"<< endl;
	cout << "****************************************** " << endl;
	chain->Finish();
	cout << "****************************************** " << endl;
	cout << "total number of events  " << total << endl;
	cout << "****************************************** " << endl;
	
	delete chain;
	
#endif	
}

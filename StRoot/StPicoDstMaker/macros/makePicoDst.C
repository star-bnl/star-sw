
#include <TSystem>

class StMaker;
class StChain;
class StPicoDstMaker;
class StMuDstMaker;


StChain *chain;
//void makePicoDst(const Int_t runnumber=11037016, const Char_t *inputFile="st_physics_11037016_raw_5010002.MuDst.root", const bool creatingPhiWgt = kFALSE, const int prodMod = 0)
void makePicoDst(const Int_t runnumber=15140004,
//    const Char_t *inputFile="/star/data54/reco/AuAu200_production_2011/FullField/P11id/2011/169/12169026/st_physics_adc_12169026_raw_4510001.MuDst.root",
//    const Char_t *inputFile="/star/data78/reco/pp200_production_2012/ReversedFullField/P12id/2012/040/13040016/st_physics_13040016_raw_1010001.MuDst.root",
//    const Char_t *inputFile="/star/data43/reco/pp500_production_2013/ReversedFullField/P14ia/2013/115/14115072/st_physics_14115072_raw_3690004.MuDst.root",
//    const Char_t *inputFile="root://xrdstar.rcf.bnl.gov:1095//home/starlib/home/starreco/reco/pp500_production_2011/ReversedFullField/P11id/2011/042/12042026/st_physics_adc_12042026_raw_2500001.MuDst.root",
//    const Char_t *inputFile="/star/u/xgn1992/work/14PicoDst/PdsfData/15087019/0/st_physics_15087019_raw_0000050.MuDst.root",
//    const Char_t *inputFile="st_physics_15166010_raw_3000054.MuDst.root",
//    const Char_t *inputFile="/star/data79/reco/AuAu_200_production_low_2014/ReversedFullField/P15ic/2014/145/15145024/st_physics_15145024_raw_1000048.MuDst.root",
//    const Char_t *inputFile="root://xrdstar.rcf.bnl.gov:1095//home/starlib/home/starreco/reco/AuAu_200_production_low_2014/ReversedFullField/P15ic/2014/166/15166010/st_physics_15166010_raw_4500060.MuDst.root",
    const Char_t *inputFile="root://xrdstar.rcf.bnl.gov:1095//home/starlib/home/starreco/reco/AuAu_200_production_low_2014/ReversedFullField/P15ic/2014/140/15140004/st_physics_15140004_raw_1000016.MuDst.root",
    const bool creatingPhiWgt = kFALSE, const int prodMod = 0, const int emcMode=1
){
        Int_t nEvents = 10000000;
//	Int_t nEvents = 500;	
//Load all the System libraries
	
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

	if(!creatingPhiWgt&&emcMode) {
		gSystem->Load("StTpcDb");
		gSystem->Load("StMcEvent");
		gSystem->Load("StMcEventMaker");
		gSystem->Load("StDaqLib");
		gSystem->Load("libgen_Tables");
 		gSystem->Load("libsim_Tables");
  		gSystem->Load("libglobal_Tables");
		gSystem->Load("StEmcTriggerMaker");
		gSystem->Load("StEmcUtil");//mine
		gSystem->Load("StEmcRawMaker");
		gSystem->Load("StEmcADCtoEMaker");
		gSystem->Load("StPreEclMaker");
		gSystem->Load("StEpcMaker");
		gSystem->Load("StEmcSimulatorMaker");
		gSystem->Load("StEmcUtil");
		gSystem->Load("StDbBroker");
		gSystem->Load("StDetectorDbMaker");
		gSystem->Load("StDbUtilities");
                gSystem->Load("StEEmcUtil");
                gSystem->Load("StEEmcDbMaker");
		gSystem->Load("St_db_Maker");
                gSystem->Load("StTriggerUtilities");
	}

// New additions for MTD fixes in Run14
  gSystem->Load("StMagF");
  gSystem->Load("StMtdUtil");
  gSystem->Load("StMtdMatchMaker");
  gSystem->Load("StMtdCalibMaker");

        gSystem->Load("StPicoDstMaker");

	chain = new StChain();

	StMuDstMaker *MuDstMaker = new StMuDstMaker(0,0,"",inputFile,"MuDst",100);
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
		St_db_Maker *dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");

                // Endcap database
                StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

		StEmcADCtoEMaker *adc2e = new StEmcADCtoEMaker();
  		adc2e->setPrint(false);
  		//adc2e->setFillHisto(false);
  		//adc2e->setDebug(false); //more histograms
  		//adc2e->setSMDRmsCut(0,0);
  		adc2e->saveAllStEvent(true);
  		//adc2e->setRemoveGhostEvent(false);
  		//adc2e->setTallyHist(mTally);
  		//adc2e->setDbName("Calibrations/emc/y3");

 	 	StPreEclMaker *pre_ecl=new StPreEclMaker();
  		pre_ecl->setPrint(kFALSE);
  		StEpcMaker *epc=new StEpcMaker();
  		epc->setPrint(kFALSE);

#if 1
    // Trigger simulator
    StTriggerSimuMaker* trigSimu = new StTriggerSimuMaker;
    trigSimu->setMC(false);
    trigSimu->useBemc();
    trigSimu->useEemc();
    trigSimu->useOnlineDB();
    trigSimu->bemc->setConfig(StBemcTriggerSimu::kOffline);
#endif

	}

  StMagFMaker *magfMk = new StMagFMaker; 
  StMtdMatchMaker *mtdMatchMaker = new StMtdMatchMaker();
  StMtdCalibMaker *mtdCalibMaker = new StMtdCalibMaker("mtdcalib"); 
	
	StPicoDstMaker *picoMaker = new StPicoDstMaker(1,inputFile,"picoDst");
        picoMaker->setRunNumber(runnumber);
        picoMaker->setProdMode(prodMod); // 0-mb, 1-central, 2-ht
        picoMaker->setEmcMode(emcMode); // 0-No EMC, 1-EMC ON
//        picoMaker->SetDebug(1);

	chain->Init();
	cout<<"chain->Init();"<<endl;
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
	
	
}

void RunTriggerSimu(int nevents = 1000000,
//const char* indir = "root://xrdstar.rcf.bnl.gov:1095//home/starreco/reco/pp200_production_2012/ReversedFullField/P12id/2012/049/13049101/st_physics_13049101_raw_2020005.MuDst.root",			 
const char* indir = "root://xrdstar.rcf.bnl.gov:1095//home/starreco/reco/pp500_production_2012/ReversedFullField/P13ib/2012/078/13078014/st_physics_13078014_raw_1020001.MuDst.root"			 
			 )
{
//  cout<<"MuDst file is "<<MuDst<<endl;

  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");

  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StTriggerFilterMaker");
  
   StChain *chain = new StChain;

   StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,indir, "","",1000);

   StMuDbReader* muDstDb = StMuDbReader::instance();

   St_db_Maker* starDb = new St_db_Maker("StarDb","MySQL:StarDb");

   StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

   //   StSpinDbMaker* spinDb = new StSpinDbMaker;

   StEmcADCtoEMaker* adc = new StEmcADCtoEMaker;
   adc->saveAllStEvent(true);

   StTriggerSimuMaker* simuTrig = new StTriggerSimuMaker;
   //use online or offline option
   //simuTrig->useOnlineDB();
   simuTrig->useOfflineDB();
   simuTrig->setMC(false);

   //bbc is not used in run12 analysis
   //   simuTrig->useBbc();
   simuTrig->useBemc();
   simuTrig->useEemc();
   //use online or offline bemc tower pedestals and statuses
   //simuTrig->bemc->setConfig(StBemcTriggerSimu::kOffline);
   simuTrig->bemc->setConfig(StBemcTriggerSimu::kOnline);
   //define triggers with trigger index, trigger name, offline Id, onbits, offbits, onbits1, onbits2, onbits3, offbits1, offbits2, offbits3
   //this method need to be called only when the trigger definitions in online and offline are not correct
   //simuTrig->emc->defineTrigger(8, "JP0", 380401, 0x0, 0x0, 0x80000000, 0x0, 0x0, 0x0, 0x0, 0x1);
   //set jet patch thresholds only used when the jet patch thresholds in online and offline database not correct
   //jp-th0 set to 28
   //simuTrig->setBarrelJetPatchTh(0,28);
   //simuTrig->setOverlapJetPatchTh(0,28);
   //simuTrig->setEndcapJetPatchTh(0,28);
   //set high tower thresholds only used when the jet patch thresholds in online and offline database not correct
   //simuTrig->setBarrelHighTowerTh(0, 11);
   //simuTrig->setEndcapHighTowerTh(0, 25);
   // Run
   chain->Init();
   for(int iEvent = 0; iEvent < nevents; iEvent++)
     {
       chain->Clear();
       int status = chain->Make(iEvent);
       if(status == kStSkip) continue;
       if(status % 10 == kStEOF || status % 10 == kStFatal) break;
       std::cout<<"trigger simulator: JP0 fired = "<< simuTrig->isTrigger(380401) <<endl;
     }
   //chain->EventLoop(nevents);
}

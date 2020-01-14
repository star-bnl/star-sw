void RunTriggerSimu2015(
//int nevents = 10,
//data test file:
const char* indir = "root://xrdstar.rcf.bnl.gov:1095//home/starlib/home/starreco/reco/production_pp200trans_2015/ReversedFullField/P16id/2015/085/16085026/st_physics_16085026_raw_1000019.MuDst.root"
//const char* indir = "root://xrdstar.rcf.bnl.gov:1095//home/starlib/home/starreco/reco/dAu200_production_2016/ReversedFullField/P17ib/2016/136/17136038/st_upc_17136038_raw_4000004.MuDst.root"
//embedding test file:
//const char* indir = "/gpfs01/star/pwg/ztu/STARlight/starsim/output/slight16dAu_zerobias_incohVM_100k_Emb_v6/0004/out/output.MuDst.root"			 
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

   StTriggerSimuMaker* simuTrig = new StTriggerSimuMaker("StarTrigSimu");
   //use online or offline option
   //simuTrig->useOnlineDB();
   simuTrig->useOfflineDB();
   simuTrig->setMC(0);

   //bbc is not used in run12 analysis
   //simuTrig->useBbc();
   simuTrig->useBemc();
   simuTrig->useEemc();
   //use online or offline bemc tower pedestals and statuses
   simuTrig->bemc->setConfig(StBemcTriggerSimu::kOnline);
   //un-comment out the following two lines to test DSM algorithm with raw MuDst files
   //simuTrig->bemc->mTestMode = true;
   //simuTrig->eemc->mTestMode = true;
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
   TFile *fout = new TFile("out.simu.2015.root", "recreate");
   TH1F *hh = new TH1F("hh", "; fired", 2, 0, 2);
   TH2F *h2d =new TH2F("h2d", "; data ;simu", 2, 0, 2, 2, 0, 2);
   TH2F *hhtdiff = new TH2F("hhtdiff", "; patch; ht diff", 300, 0, 300, 253, -126, 127);
   TH2F *hht = new TH2F("hht", "; patch; ht", 300,0,300, 64, 0, 64);
   TH2F *hpadiff = new TH2F("hpadiff", "; patch; pa diff", 300, 0, 300, 253, -126, 127);
   TH2F *hpa = new TH2F("hpa", "; patch; pa", 300,0,300, 64, 0, 64);

   //TH2F *hlayer1 = new TH2F("hlayer1", ";channel; bit", 48, 0, 48, 16,0, 16);
   int nEvents = muDstMaker->chain()->GetEntries();
   int trigId_1 = 480411;
   int trigId_2 = 480411;
   cout << "number of events ~ " << nEvents << endl;
   //nEvents = 1000;
   for(int iEvent = 0; iEvent < nEvents; iEvent++)
     {
       chain->Clear();
       int status = chain->Make(iEvent);
       if(status == kStSkip) continue;
       if(status % 10 == kStEOF || status % 10 == kStFatal) break;
       //std::cout<<"trigger simulator: UPC-Jpsi fired = "<< simuTrig->isTrigger(trigId) <<endl;
       bool did_1 = StMuDst::event()->triggerIdCollection().nominal().isTrigger(trigId_1);
       bool did_2 = StMuDst::event()->triggerIdCollection().nominal().isTrigger(trigId_2);
       //bool did_2 = 0;
       bool did = did_1 || did_2;
       //bool fire = simuTrig->emc->EHT0();
       bool fire = simuTrig->isTrigger(trigId_1);
       hh->Fill(fire);
       h2d->Fill(did, fire);
       if(did && !fire){
         Printf("iEvent=%d", iEvent);
       
	 StEmcTriggerDetector emc = StMuDst::event()->emcTriggerDetector();
	 int *stpht = simuTrig->bemc->getBEMC_FEE_HT_ADC();
	 int *stppa = simuTrig->bemc->getBEMC_FEE_TP_ADC();
	 for(int ip = 0; ip < 300; ip++){
	   int ht = emc.highTower(ip);
	   int sht = stpht[ip];
	   int diffht = ht - sht;
	   //if(diffht > 0){
               hhtdiff->Fill(ip, diffht);
               hht->Fill(ip, ht);
           //}

	   int pa = emc.patch(ip);
	   int spa = stppa[ip];
           int diffpa = pa -spa;
	   //if(diffpa > 0){
               hpadiff->Fill(ip, diffpa);
               hpa->Fill(ip, ht);
           //}
	 }
       }
     }
   //chain->EventLoop(nevents);
   fout->Write();
   fout->Close();
}

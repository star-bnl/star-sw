/* How to use this macro.
To run w/o jets

root4star -b -q 'rdMuWana2011.C(2e3,"st_W_12037041_raw_1400001.MuDst.root",0,0,5,7,"/star/data13/Magellan/reco/pp500_production_2011/ReversedFullField/P10k/2011/12037041/")'

To  produce jets
root4star -b -q 'rdMuWana2011.C(2e3,"st_W_12037041_raw_1400001.MuDst.root",0,1,5,7,"/star/data13/Magellan/reco/pp500_production_2011/ReversedFullField/P10k/2011/12037041/","/star/institutions/anl/balewski/jetTemp/")'

*/



class StChain;
StChain *chain = 0;

int  spinSort = false;
bool isZ      = false;
int  geant    = false;

int rdMuWana2011(
   int   nEve         = 1e6,
   char* file         = "st_W_12037063_raw_1380001_1201.MuDst.root",
   int   isMC         = 0, // 0           = run9-data  200 = new MC w/ EEss in BFC
   int   useJetFinder = 0, // 0 - no jets = badWalgo; 1 generate jet trees; 2 read jet trees
   int   idL2BWtrg    = 0, //  offline Ids  needed for real data
   int   idL2EWtrg    = 0, // run 9 L2EW
   // make those below  empty for scheduler
   char* muDir        = "",
   char* jetDir       = "",
   char* histDir      = "",
   char* wtreeDir     = ""
)
{
   char *eemcSetupPath = "/afs/rhic.bnl.gov/star/users/kocolosk/public/StarTrigSimuSetup/";

   if (isMC && useJetFinder == 2) geant = true;

   if (isMC) spinSort = false;

   string  inputPathFile(file);

   size_t iLastSlash = inputPathFile.find_last_of("/");

   string inputPath("");

   if (iLastSlash != string::npos)
      inputPath = inputPathFile.substr(0, iLastSlash);
   else inputPath = "";

   string inputFile  = inputPathFile.substr(iLastSlash + 1);

   printf("Input path: %s\n", inputPath.c_str());
   printf("Input file: %s\n", inputFile.c_str());

   TString outF = inputFile;
   outF = outF.ReplaceAll(".MuDst.root", "");
   outF = outF.ReplaceAll(".lis", "");

   if (!isMC) {
      //outF = file;
      //outF = outF;
   }
   else { //  new  MC w/ working time stamp
      //assert(2==5); M-C not unpacking not implemented
      char *file1 = strstr(file, "cn100");
      assert(file1);
      file1--;
      printf("file1: %s\n", file1);
      outF = file1;
      //outF.ReplaceAll(".MuDst.root","");
      TString fileG = file;
      fileG.ReplaceAll("MuDst", "geant");
   }

   printf("Output file: %s\n", outF.Data());

   printf("TRIG ID: L2BW=%d, L2EW=%d   isMC=%d  useJetFinder=%d\n", idL2BWtrg, idL2EWtrg, isMC, useJetFinder );

   gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
   loadSharedLibraries();
   gROOT->LoadMacro("LoadLogger.C");
   gMessMgr->SwitchOff("D");
   gMessMgr->SwitchOn("I");

   assert( !gSystem->Load("StDaqLib"));

   // libraries below are needed for DB interface
   assert( !gSystem->Load("StDetectorDbMaker")); // for St_tpcGasC
   assert( !gSystem->Load("StTpcDb"));
   assert( !gSystem->Load("StDbUtilities")); //for trigger simu
   assert( !gSystem->Load("StDbBroker"));
   assert( !gSystem->Load("St_db_Maker"));
   assert( !gSystem->Load("StEEmcUtil"));
   assert( !gSystem->Load("StEEmcDbMaker"));
   assert( !gSystem->Load("StTriggerFilterMaker"));
   assert( !gSystem->Load("StWalgo2011"));
   assert( !gSystem->Load("StTriggerUtilities"));
   assert( !gSystem->Load("StSpinDbMaker"));

   if (useJetFinder == 1 || useJetFinder == 2) { // jetfinder/jetreader libraries
      cout << "BEGIN: loading jetfinder libs" << endl;
      gSystem->Load("StEmcRawMaker");
      gSystem->Load("StEmcADCtoEMaker");
      gSystem->Load("StJetSkimEvent");
      gSystem->Load("StJets");
      gSystem->Load("StSpinDbMaker");
      gSystem->Load("StEmcTriggerMaker");
      gSystem->Load("StTriggerUtilities");
      gSystem->Load("StMCAsymMaker");
      gSystem->Load("StRandomSelector");
      gSystem->Load("StJetEvent");
      gSystem->Load("StJetFinder");
      gSystem->Load("StJetMaker");
      cout << "END: loading jetfinder libs" << endl;
   }
   else  {
      cout << "\nWARN: Jet are NOT read in, W-algo will not wrk properly\n " << endl;
   }

   if (geant) {
      // libraries for access to MC record
      assert( !gSystem->Load("StMcEvent"));
      assert( !gSystem->Load("StMcEventMaker"));

      // libraries for trigger simulator
      assert( !gSystem->Load("StEmcSimulatorMaker"));
      assert( !gSystem->Load("StEEmcSimulatorMaker"));
      assert( !gSystem->Load("StEpcMaker"));
   }

   cout << " loading done " << endl;

   // create chain
   chain = new StChain("StChain");

   // create histogram storage array  (everybody needs it):
   TObjArray* HList = new TObjArray;

   if (geant) {
      // get geant file
      StIOMaker* ioMaker = new StIOMaker();
      printf("\n %s \n\n", fileG.Data());
      ioMaker->SetFile(fileG.Data());

      ioMaker->SetIOMode("r");
      ioMaker->SetBranch("*", 0, "1"); //deactivate all branches
      ioMaker->SetBranch("geantBranch", 0, "r"); //activate geant Branch
      ioMaker->SetBranch("minimcBranch", 0, "r"); //activate geant Branch
   }

   // Now we add Makers to the chain...
   int maxFiles = 1000;

   StMuDstMaker *stMuDstMaker = new StMuDstMaker(0, 0, muDir, file, "MuDst.root", maxFiles);

   stMuDstMaker->SetStatus("*", 0);
   stMuDstMaker->SetStatus("MuEvent", 1);
   stMuDstMaker->SetStatus("EmcTow", 1);
   stMuDstMaker->SetStatus("EmcSmde", 1);
   stMuDstMaker->SetStatus("EmcSmdp", 1);
   stMuDstMaker->SetStatus("PrimaryVertices", 1);
   stMuDstMaker->SetStatus("GlobalTracks", 1);
   stMuDstMaker->SetStatus("PrimaryTracks", 1);

   TChain* stMuDstMakerChain = stMuDstMaker->chain();

   assert(stMuDstMakerChain);

   int nEntries = (int) stMuDstMakerChain->GetEntries();

   if (nEntries < 0) {
      Error("rdMuWana2011", "Invalid number of events %d", nEntries)
      return -1;
   }

   printf("Total number of events in muDst chain = %d\n", nEntries);

   //for EEMC, need full db access:
   St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb", "MySQL:StarDb", "$STAR/StarDb");

   if (isMC == 0) {
      // run 11  data
      dbMk->SetFlavor("Wbose2", "bsmdeCalib"); // Willie's abs gains E-plane, run 9
      dbMk->SetFlavor("Wbose2", "bsmdpCalib"); // P-plane
      dbMk->SetFlavor("sim", "bemcCalib"); // use ideal gains for real data
      dbMk->SetFlavor("sim", "eemcPMTcal"); // use ideal gains for 2011 real data as well
   }
   else {
      printf("???? unforeseen MC flag, ABORT\n");
      assert(1 == 2);
   }


   //.... load EEMC database
   StEEmcDbMaker*  mEEmcDatabase = new StEEmcDbMaker("eemcDb");

#if 0 // drop abs lumi for now   
   if (!isMC && strstr(file, "fillListPhys")) {
      StTriggerFilterMaker *filterMaker = new StTriggerFilterMaker;
      filterMaker->addTrigger(230420); // AJP
      filterMaker->addTrigger(230411); // JP2
      filterMaker->addTrigger(bht3ID); // regular W -> e+ analysis
   }
#endif

   if (geant) {
      StMcEventMaker *mcEventMaker = new StMcEventMaker();
      mcEventMaker->doPrintEventInfo = false;
      mcEventMaker->doPrintMemoryInfo = false;
   }

   if (geant && useJetFinder != 1) { // only use trigger simulator in W algo
      //BEMC simulator:
      StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker(); //use this instead to "redo" converstion from geant->adc
      emcSim->setCalibSpread(kBarrelEmcTowerId, 0.15); //spread gains by 15%
      StEmcADCtoEMaker *bemcAdc = new StEmcADCtoEMaker();//for real data this sets calibration and status

      //EEMC simulator:
      new StEEmcDbMaker("eemcDb");
      StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");
      //slowSim->setSamplingFraction(0.0384); // effectively scales all Tower energies with a factor of 1.3 (for old private filtered simu only!)
      slowSim->setAddPed(true);
      slowSim->setSmearPed(true);

      //Get TriggerMaker
      StTriggerSimuMaker *simuTrig = new StTriggerSimuMaker("StarTrigSimu");
      simuTrig->setHList(HList);
      simuTrig->setMC(isMC); // must be before individual detectors, to be passed
      simuTrig->useBbc();
      simuTrig->useEemc(0);//default=0:just process ADC, 1,2:comp w/trgData,see .
      simuTrig->eemc->setSetupPath(eemcSetupPath);
      simuTrig->useBemc();
      simuTrig->bemc->setConfig(2);
   }

   TString jetFile = jetDir;

   //.... Jet finder code ....
   if (useJetFinder > 0)  {
      //ds TString jetFile = jetDir;
      //ds jetFile+="jets_"+outF+".root";
      jetFile += "jets_";
      jetFile += outF + ".root";
      cout << "BEGIN: running jet finder/reader =" << jetFile << "=" << endl;
   }


   if (useJetFinder == 1)
   { //{{{ // run jet finder
      double pi = atan(1.0) * 4.0;
      // Makers for clusterfinding
      StSpinDbMaker    *uspDbMaker = new StSpinDbMaker("spinDb");
      StEmcADCtoEMaker *adc        = new StEmcADCtoEMaker();

      //here we also tag whether or not to do the swap:
      bool doTowerSwapFix   = true;
      bool use2003TowerCuts = false;
      bool use2006TowerCuts = true;

      //4p maker using 100% tower energy correction
      StBET4pMaker* bet4pMakerFrac100 = new StBET4pMaker("BET4pMakerFrac100", stMuDstMaker, doTowerSwapFix, new StjTowerEnergyCorrectionForTracksFraction(1.0));
      bet4pMakerFrac100->setUse2003Cuts(use2003TowerCuts);
      bet4pMakerFrac100->setUseEndcap(true);
      bet4pMakerFrac100->setUse2006Cuts(use2006TowerCuts);

      //4p maker using 100% tower energy correction (no endcap)
      StBET4pMaker* bet4pMakerFrac100_noEEMC = new StBET4pMaker("BET4pMakerFrac100_noEEMC", stMuDstMaker, doTowerSwapFix, new StjTowerEnergyCorrectionForTracksFraction(1.0));
      bet4pMakerFrac100_noEEMC->setUse2003Cuts(use2003TowerCuts);
      bet4pMakerFrac100_noEEMC->setUseEndcap(false);
      bet4pMakerFrac100_noEEMC->setUse2006Cuts(use2006TowerCuts);

      //Instantiate the JetMaker and SkimEventMaker
      StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", stMuDstMaker, jetFile);
      //StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker", stMuDstMaker,outSkimFile);

      //set the analysis cuts: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
      StppAnaPars* anapars = new StppAnaPars();
      anapars->setFlagMin(0); //track->flag() > 0
      anapars->setNhits(12); //track->nHitsFit()>12
      anapars->setCutPtMin(0.2); //track->pt() > 0.2
      anapars->setAbsEtaMax(2.0); //abs(track->eta())<1.6
      anapars->setJetPtMin(3.5);
      anapars->setJetEtaMax(100.0);
      anapars->setJetEtaMin(0);
      anapars->setJetNmin(0);

      //Setup the cone finder (See StJetFinder/StConeJetFinder.h -> class StConePars)
      StConePars* cpars = new StConePars();
      cpars->setGridSpacing(105, -3.0, 3.0, 120, -pi, pi);  //include EEMC
      cpars->setConeRadius(0.7); // default=0.7
      cpars->setSeedEtMin(0.5);
      cpars->setAssocEtMin(0.1);
      cpars->setSplitFraction(0.5); //default=0.5. if 0.3 less split?
      cpars->setPerformMinimization(true);
      cpars->setAddMidpoints(true);
      cpars->setRequireStableMidpoints(true);
      cpars->setDoSplitMerge(true);

      cpars->setDebug(false);

      emcJetMaker->addAnalyzer(anapars, cpars, bet4pMakerFrac100, "ConeJets12_100"); //100% subtraction
      emcJetMaker->addAnalyzer(anapars, cpars, bet4pMakerFrac100_noEEMC, "ConeJets12_100_noEEMC"); //100% subtraction (no Endcap)

      chain->Init();
      chain->ls(3);

      char txt[1000];
      //---------------------------------------------------
      int eventCounter = 0;
      int t1 = time(0);
      TStopwatch tt;

      for (Int_t iev = 0; iev < nEntries; iev++) {
         if (eventCounter >= nEve) break;
         chain->Clear();
         int stat = chain->Make();
         if (stat != kStOk && stat != kStSkip) break; // EOF or input error
         eventCounter++;
      }

      cout << "run " << file << " nEve=" << eventCounter << " total ";
      tt.Print();
      printf("****************************************** \n");

      int t2 = time(0);
      if (t2 == t1) t2 = t1 + 1;
      float tMnt = (t2 - t1) / 60.;
      float rate = 1.*eventCounter / (t2 - t1);

      printf("jets sorting done %d of   nEve= %d, CPU rate= %.1f Hz, total time %.1f minute(s) \n\n", eventCounter, nEntries, rate, tMnt);

      cout << "END: jet finder " << endl;

      return 1;
   } //}}}

   if (useJetFinder == 2) {
      cout << "Configure to read jet trees " << endl;
      StJetReader *jetReader = new StJetReader;
      jetReader->InitFile(jetFile);
   }

   //.... W reconstruction code ....
   St2011WMaker *WmuMk = new St2011WMaker();

   if (isMC) { // MC specific
      WmuMk->setMC(isMC); //pass "version" of MC to maker
      //WmuMk->setJetNeutScaleMC(1.0);
      //WmuMk->setJetChrgScaleMC(1.0);
   }
   else {  // real data
      WmuMk->setTrigID(idL2BWtrg, idL2EWtrg);
   }

   TString treeFileName = wtreeDir;
   treeFileName += outF;
   treeFileName += ".Wtree.root";

   WmuMk->setTreeName(treeFileName);

   if (useJetFinder == 2)
      WmuMk->setJetTreeBranch("ConeJets12_100", "ConeJets12_100_noEEMC"); //select jet tree braches used

   WmuMk->setMaxDisplayEve(100); // only first N events will get displayed
   //set energy scale (works for data and MC - be careful!)
   //WmuMk->setBtowScale(1.0);
   //WmuMk->setEtowScale(1.0);

   /* evaluation of result, has full acess to W-algo internal data
      including overwrite - be careful */

   St2011pubWanaMaker *WpubMk = new St2011pubWanaMaker("pubJan");
   WpubMk->attachWalgoMaker(WmuMk);

   //Collect all output histograms
   //already defined this above:  TObjArray* HList=new TObjArray;
   WmuMk->setHList(HList);
   WpubMk->setHList(HList);

   StSpinDbMaker *spDb = 0;
   if (spinSort) {
      spDb = new StSpinDbMaker("spinDb");
      enum {mxSM = 5}; // to study eta-cuts, drop Q/PT cut
      St2011pubSpinMaker *spinMkA[mxSM];

      for (int kk = 0; kk < mxSM; kk++) {
         char ttx[100];
         sprintf(ttx, "%cspin", 'A' + kk);
         printf("add spinMaker %s %d \n", ttx, kk);
         spinMkA[kk] = new St2011pubSpinMaker(ttx);
         spinMkA[kk]->attachWalgoMaker(WmuMk);
         spinMkA[kk]->attachSpinDb(spDb);
         spinMkA[kk]->setHList(HList);
         if (kk == 1) spinMkA[kk]->setEta(-1., 0.);
         if (kk == 2) spinMkA[kk]->setEta(0, 1.);
         if (kk == 3) spinMkA[kk]->setQPT(-1); // disable Q/PT cut
         if (kk == 4) spinMkA[kk]->setNoEEMC();
      }
   }

   if (geant) {
      pubMcMk = new St2009pubMcMaker("pubMc");
      pubMcMk->attachWalgoMaker(WmuMk);
      pubMcMk->setHList(HList);
   }

   if (isZ) {
      ZMk = new St2011ZMaker("Z");
      ZMk->attachWalgoMaker(WmuMk);
      ZMk->setHList(HList);
      ZMk->setNearEtFrac(0.88);
      ZMk->setClusterMinEt(15);
      ZMk->setPhi12Min(3.1416 / 2.);
      ZMk->setMinZMass(73.); // Zmass -20%
      ZMk->setMaxZMass(114.);// Zmass +20%
   }

   chain->Init();
   chain->ls(3);

   char txt[1000];
   int  eventCounter = 0;
   int  t1 = time(0);

   TStopwatch tt;

   for (Int_t iev = 0; iev < nEntries; iev++) {
      Info("rdMuWana2011", "Analyzing event %d", iev);
      if (eventCounter >= nEve) break;
      chain->Clear();
      int stat = chain->Make();
      if (stat != kStOk && stat != kStSkip) break; // EOF or input error
      eventCounter++;
   }

   //chain->Finish();

   cout << "run " << file << " nEve=" << eventCounter << " total ";
   tt.Print();

   printf("****************************************** \n");

   int t2 = time(0);
   if (t2 == t1) t2 = t1 + 1;
   float tMnt = (t2 - t1) / 60.;
   float rate = 1.*eventCounter / (t2 - t1);

   printf("#sorting %s done %d of   nEve= %d, CPU rate= %.1f Hz, total time %.1f minute(s) \n\n", file, eventCounter, nEntries, rate, tMnt);


   TString histFileName = histDir;
   histFileName += outF;
   histFileName += ".wana.hist.root";

   cout << "Output histo file " << histFileName << endl;

   hf = new TFile(histFileName, "recreate");

   if (hf->IsOpen()) {
      //HList->ls();
      HList->Write();
      printf("\n Histo saved -->%s<\n", outFh.Data());
   }
   else {
      printf("\n Failed to open Histo-file -->%s<, continue\n", outFh.Data());
   }

   //WmuMk->Finish();

   return 2;
}


// $Log: rdMuWana2011.C,v $
// Revision 1.5  2012/08/21 17:40:15  stevens4
// Revert to previous version
//
// Revision 1.3  2012/03/19 23:45:23  smirnovd
// Major clean up. Removed hardcoded references etc.
//
// Revision 1.2  2012/03/12 23:11:30  smirnovd
// *** empty log message ***
//
// Revision 1.1  2011/02/10 20:33:35  balewski
// start
//

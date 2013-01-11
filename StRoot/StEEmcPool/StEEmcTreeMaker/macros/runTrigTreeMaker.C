// Based on the macro StRoot/StTriggerUtilities/macros/rdMu2TrigSimu.C as of August 20th, 2012

// Before running macro create an output directory "outL2" if you want a copy of the L2 output histos
// This macro runs on real data (flagMC=0) and MC files (flagMC=1)
// Set flag ==1 for those detectors you want included in the trigger decisions 
// Set configuration for BEMC (online, offline or custom).  EEMC is the same for offline and online
// Choose the correct configuration year if you want to use L2

// designed for 2006

int total=0;
class StChain *chain=0; 

// make sure the eemcSetupPath ends with a '/' and has a subdirectory for the
// year of interest

void runTrigTreeMaker( const Char_t *file="rcf10063_2_6000evts.MuDst.root", const Char_t* fileOut = "rcf10063_2_6000evts.TrigTree-A.root", Char_t trigVer = 'g', Int_t nevents = 10000000,
                       Int_t flagMC = 1, const Char_t *eemcSetupPath="/star/u/sgliske/Share/StarTrigSimuSetup/" ){
  
   //int flagMC=0;  // 0/1 == Using Real/Simulation data files 
  int useEemc=2; // 0/1 == Exclude/Include EEMC in Trigger Decisions 
  int useBemc=0; // 0/1 == Exclude/Include BEMC in Trigger Decisions 
  int useL2=1;   // 0/1 == Exclude/Include L2 in Trigger Decisions 
  int L2ConfigYear=2006; // possible: 2006, 2008
  int bemcConfig=1; // Online==1, Offline==2, Expert==3
  int playConfig=0; // jan:100_199
  int emcEveDump=0; // extrating raw EMC data in a custom format
  int outputL2Histo=1;//output L2 histos to directory outL2
  TString outDir="./outL2/"; 

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  assert( !gSystem->Load("StDetectorDbMaker"));
  assert( !gSystem->Load("StTpcDb"));
  assert( !gSystem->Load("StDbUtilities"));
  assert( !gSystem->Load("StDbBroker"));
  assert( !gSystem->Load("St_db_Maker"));
  assert( !gSystem->Load("StEEmcUtil")); // needed by eemcDb
  assert( !gSystem->Load("StEEmcDbMaker"));
  assert( !gSystem->Load("StDaqLib")); // needed by bemcDb
  assert( !gSystem->Load("StEmcRawMaker"));
  assert( !gSystem->Load("StEmcADCtoEMaker"));
  if (flagMC) {
    assert( !gSystem->Load("StMcEvent"));
    assert( !gSystem->Load("StMcEventMaker"));
    assert( !gSystem->Load("StEmcSimulatorMaker"));
    assert( !gSystem->Load("StEEmcSimulatorMaker"));
    assert( !gSystem->Load("StEpcMaker"));
  }
  assert( !gSystem->Load("StTriggerUtilities"));
   assert( !gSystem->Load("StSpinDbMaker") );
  assert( !gSystem->Load("StEEmcPoolEEmcTreeContainers") );
  assert( !gSystem->Load("StEEmcTreeMaker") );

  gROOT->Macro("LoadLogger.C");
  cout << " loading done " << endl;
  
  chain= new StChain("StChain"); 

  if (flagMC){
    TString geantFile;
    geantFile += file;
    geantFile.ReplaceAll("MuDst.root", "geant.root");
    printf("geantFile=%s\n", geantFile.Data());
    StIOMaker* ioMaker = new StIOMaker();
    ioMaker->SetFile(geantFile);
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");             //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
    StMcEventMaker *evtMaker = new StMcEventMaker();
  }

  //Need MuDstMaker to get data
  printf(" Analyzing file=%s\n",file);  
  StMuDstMaker* muDstMaker =new StMuDstMaker(0,0,"",file,"",1000);

   // just to make sure StEvent is found for the trig simu
   StMuDst2StEventMaker *muDst2StEvent = new StMuDst2StEventMaker();

  //Database -- get a real calibration from the database
  St_db_Maker* dbMk =0;
  if(useEemc || useL2) // full DB access
    dbMk = new St_db_Maker("StarDb","MySQL:StarDb","MySQL:StarDb","$STAR/StarDb");
  else // only Barrel is uploaded, is faster 
    dbMk  = new St_db_Maker("Calibrations","MySQL:Calibrations_emc");
   dbMk->SetAttr("blacklist", "fgt");
   dbMk->SetAttr("blacklist", "svt");
   dbMk->SetAttr("blacklist", "tpc");
   dbMk->SetAttr("blacklist", "ftpc");

    
  // CORRECTION: do not always need to set date and time for MC
  //If MC then must set database time and date
  //If Endcap fast simu is used tower gains in DB do not matter,JB
  //if (flagMC) dbMk->SetDateTime(20060522, 55000);//timestamp R7142018
  
  //Collect all output histograms 
  TObjArray* HList=new TObjArray; 
  
  //Endcap DB
  if(useEemc || useL2) new StEEmcDbMaker("eemcDb");
  

  //Get BEMC adc values
  if (flagMC && useBemc) {
    StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker(); //use this instead to "redo" converstion from geant->adc
    if (bemcConfig == 1) {
        emcSim->setCheckStatus(kBarrelEmcTowerId,false); //this returns hits regardless of offline tower status
    }
    emcSim->setCalibSpread(kBarrelEmcTowerId,0.15);//spread gains by 15%
  }
  if (flagMC==0 && useBemc){
    StEmcADCtoEMaker *bemcAdc = new StEmcADCtoEMaker();//for real data this sets calibration and status
    if (bemcConfig == 1) {
        bemcAdc->setCheckStatus(kBarrelEmcTowerId,false);
    }
  }

  // original comment: must use slow simulator to get pedestals
  // correct for L2.  New comment: I think this is needed if slow simulator was
  // used in the MC production.
  if (0 && flagMC==1 && useEemc){
    StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");

    // note: changing the sampling fraction does not effect the
    // L0 trigger decision.  Also, it is probably better to use the value
    // in the slow simulator, so that it matches data. (S. Gliske,
    // Sept 2012)

    // This 1.3 comes from the iron/air MC, where the sampling
    // fraction was believed to be near 4%, but slow/fast simulator
    // used 5%.

    // Now that the slow/fast simulator uses 0.048, it is a 1.25 scaling factor

    // slowSim->setSamplingFraction(0.0384); // effectively scale all Tower energies with a factor of 1.3 (added by: Ilya Selyuzhenkov; April 11, 2008)

    slowSim->setAddPed(true);
    slowSim->setSmearPed(true);
  }
 
  //Get TriggerMaker
  StTriggerSimuMaker *simuTrig = new StTriggerSimuMaker("StarTrigSimu");
  simuTrig->setHList(HList);
  simuTrig->setMC(flagMC); // must be before individual detectors, to be passed
  simuTrig->useBbc();
  if(useEemc) {
    simuTrig->useEemc(0);//default=0:just process ADC, 1,2:comp w/trgData,see .
    simuTrig->eemc->setSetupPath(eemcSetupPath);
  } 
  if(useBemc){
    simuTrig->useBemc();
    simuTrig->bemc->setConfig(bemcConfig);
  }

  if(flagMC && useEemc==2){
     // pass one argument to M-C as generic switch    
     // Endcap specific params -- ok Jan you need to change this to a default "online" setup
     int eemcDsmSetup[20]; // see StEemcTriggerSimu::initRun() for definition
     memset(eemcDsmSetup, 0,sizeof(eemcDsmSetup));// clear all, may be a bad default

     if( trigVer == 'd' ){
        // valid runs 7130037-7132029
        eemcDsmSetup[0]=6;  // HTthr0
        eemcDsmSetup[1]=12; // HTthr1
        eemcDsmSetup[2]=22; // HTthr2
        eemcDsmSetup[3]=1;  // TPthr0
        eemcDsmSetup[4]=20; // TPthr1
        eemcDsmSetup[5]=31; // TPthr2
     } else if ( trigVer == 'e' ){
        // valid runs 7132045-7133051
        eemcDsmSetup[0]=6;  // HTthr0
        eemcDsmSetup[1]=17; // HTthr1
        eemcDsmSetup[2]=22; // HTthr2
        eemcDsmSetup[3]=1;  // TPthr0
        eemcDsmSetup[4]=20; // TPthr1
        eemcDsmSetup[5]=31; // TPthr2
     } else if ( trigVer == 'f' ){
        // valid runs 7133052-7156040
        eemcDsmSetup[0]=6;  // HTthr0
        eemcDsmSetup[1]=16; // HTthr1
        eemcDsmSetup[2]=22; // HTthr2
        eemcDsmSetup[3]=1;  // TPthr0
        eemcDsmSetup[4]=20; // TPthr1
        eemcDsmSetup[5]=31; // TPthr2
     } else if ( trigVer == 'g' ){
        // test trigger, ht1 & tp2 are 1 DSM (roughly 5%) higher than 'e'
        eemcDsmSetup[0]=6;  // HTthr0
        eemcDsmSetup[1]=18; // HTthr1
        eemcDsmSetup[2]=22; // HTthr2
        eemcDsmSetup[3]=1;  // TPthr0
        eemcDsmSetup[4]=21; // TPthr1
        eemcDsmSetup[5]=31; // TPthr2
   } else if ( trigVer == 'h' || trigVer == 'i' ){
      // test trigger, ht1 & tp2 are 2 DSM (roughly 10%) higher than 'e'
      eemcDsmSetup[0]=6;  // HTthr0
      eemcDsmSetup[1]=19; // HTthr1
      eemcDsmSetup[2]=22; // HTthr2
      eemcDsmSetup[3]=1;  // TPthr0
      eemcDsmSetup[4]=22; // TPthr1
      eemcDsmSetup[5]=31; // TPthr2
     } else {
        cerr << "Invalid trigger version" << endl;
        return;
     };
    eemcDsmSetup[10]=2; //HTTPthrSelc, 2=use_thres_#1

    simuTrig->eemc->setDsmSetup(eemcDsmSetup);    
  }


  StGenericL2Emulator* simL2Mk=0;
  if(useL2) {
    /* 
       NOTE: the following comment was in the rdMu2TrigSimu.C macro,
       but does not seem applicable.  Just use the given
       StarTrigSimuSetup directory, and make sure the given outDir
       exists, all will be well.

       reads all input/setup files from  L2setup-yyyymmdd/
       writes all output files to L2out-yyyymmdd 
       depending on the DB time stamp 
       both dierectiorie MUST exist, setup must be reasonable
    */
    if(L2ConfigYear==2006) simL2Mk= new StL2_2006EmulatorMaker;
    else if(L2ConfigYear==2008) simL2Mk= new StL2_2008EmulatorMaker;
    assert(simL2Mk);
    simL2Mk->setSetupPath(eemcSetupPath);

    simL2Mk->setOutPath(outDir.Data());
    if (flagMC) simL2Mk->setMC();
    simuTrig->useL2(simL2Mk);
  }

  // now the maker to write the tree
  StTrigTreeMaker_t *treeMkr = new StTrigTreeMaker_t( "trigTreeMaker", fileOut, 1 );
  treeMkr->addTrigger( -999 ); // i.e. no trigger cut now

  chain->ls(3);

  cout << "******* Initialize *******" << endl ;
  chain->Init();
  cout << "******* done with initializing *******" << endl;
 
  for (Int_t iev=0;iev<nevents; iev++) {
     if( iev % 100 == 1  )
        cout << "Working on eventNumber:\t" << iev <<"\tof:\t"<<nevents<<endl;

    chain->Clear();
    int iret = chain->Make(iev);
    total++;   
    if (iret % 10 == kStEOF || iret % 10 == kStFatal)  {
      cout << iev << " Bad return code!" << endl;
      break;
    }

    //struct L2gammaResult2006* result = dynamic_cast< L2gammaResult2006* >( simL2Mk->result() );
    //cout << "ZZZ " << result->trigger << endl;

    if( simL2Mk ){
//        const unsigned int* result = simL2Mk->result();
//        cout << "ZZZ " << std::hex << result[0] << result[1] << result[2] << result[3] << result[4] << result[5] << result[6] << result[7] << std::dec
//             << " " << simL2Mk->mAcceptTriggerList.size() << ' ' << simL2Mk->mVetoTriggerList.size() << endl;

//        cout << "ZZZ accept: ";
//        for( Int_t i=0; i<simL2Mk->mAcceptTriggerList.size(); ++i )
//           cout << simL2Mk->mAcceptTriggerList.at(i) << ' ';
//        cout << endl;

//        cout << "ZZZ veto: ";
//        for( Int_t i=0; i<simL2Mk->mVetoTriggerList.size(); ++i )
//           cout << simL2Mk->mVetoTriggerList.at(i) << ' ';
//        cout << endl;

//       cout << "ZZZ " << simuTrig->isTrigger( 137641 ) << endl;
    }
  }

 
  chain->Finish();
  cout << "****************************************** " << endl;
  cout << "total number of events  " << total << endl;
  cout << "****************************************** " << endl;

  if (outputL2Histo==1) {
  
    TString fileMu=file;
    printf("=%s=\n",fileMu.Data());
    if(fileMu.Contains(".lis")) fileMu.ReplaceAll(".lis",".trgSim");
    if(fileMu.Contains(".MuDst.root")) fileMu.ReplaceAll(".MuDst.root",".trgSim");
    TString outF=outDir+fileMu;
    outF+=".hist.root";
    printf("=%s=\n",outF.Data());
    hf=new TFile(outF,"recreate");
    if(hf->IsOpen()) {
       cout << "HList not saved, or seg faults" << endl;
       //HList->Write();
       //printf("\n Histo saved -->%s<\n",outF.Data());
    } 
    else {
      printf("\n Failed to open Histo-file -->%s<, continue\n",outF.Data());
    }
  }
 
}

/*
 * $Id: runTrigTreeMaker.C,v 1.2 2013/01/11 17:32:29 sgliske Exp $
 * $Log: runTrigTreeMaker.C,v $
 * Revision 1.2  2013/01/11 17:32:29  sgliske
 * bug fix
 *
 * Revision 1.1  2013/01/11 16:14:21  sgliske
 * copied from users/sgliske CVS area
 *
 *
 */

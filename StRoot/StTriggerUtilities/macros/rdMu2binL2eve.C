/* This macro is set up to convert muDst to ETOW & BTOW data blocks
   in the format 'StJanEvent'
   needed by stand-alne L2-algos, run by multiTest.c
   This macro runs on real data (flagMC=0) and MC files (flagMC=1)
*/

int total=0;

void rdMu2binL2eve( int nevents = 100,
		    int flagMC=0,  // 0== data, 1==M-C 
		    char *file="R9067013.lis")
{ 
		   
  if (flagMC)  file="mit0029.lis";
 

  int nFiles = 100; // make this big if you want to read all events from a run
  
  char *eemcSetupPath="/star/institutions/mit/balewski/StarTrigSimuSetup2008/";  
  TString outDir="./out2/"; 
  char *dirIn ="./";

   
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  assert( !gSystem->Load("StDetectorDbMaker"));
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
    assert( !gSystem->Load("StEpcMaker"));
  }
  assert( !gSystem->Load("StTriggerUtilities"));

  gROOT->Macro("LoadLogger.C");
  cout << " loading done " << endl;
  
  StChain *chain= new StChain("StChain"); 
 
  //Need MuDstMaker to get data
  printf(" dirIn=%s=  file=%s=\n",dirIn,file);  
  StMuDstMaker* muDstMaker =new StMuDstMaker(0,0,dirIn,file,"MuDst.root",nFiles);

  TChain* tree=muDstMaker->chain(); assert(tree); int nEntries=(int) tree->GetEntries();
  cout << "Avaliable number of events  " << nEntries << endl;  
  
  //Database -- get a real calibration from the database
  St_db_Maker* dbMk = new St_db_Maker("StarDb","MySQL:StarDb","MySQL:StarDb","$STAR/StarDb");
    
  //If MC then must set database time and date
  // if Endcap fast simu is used tower gains in DB do not matter,JB
  if(flagMC) {
    dbMk->SetDateTime(20080307, 83000);//timestamp R9067013
 }

  //Endcap DB
  new StEEmcDbMaker("eemcDb");

  //Get BEMC adc values
  StEmcADCtoEMaker *a2eMk = new StEmcADCtoEMaker();
  a2eMk->saveAllStEvent(true);
 
 /* 
     reads all input/setup files from  L2setup-yyyymmdd/
     writes all output files to L2out-yyyymmdd 
     depending on the DB time stamp 
     both dierectiorie MUST exist, setup must be reasonable
  */
  StGenericL2Emulator* simL2Mk=new StL2_2008EmulatorMaker;
  assert(simL2Mk);
  simL2Mk->setSetupPath(eemcSetupPath);
  simL2Mk->setOutPath(outDir.Data());
  if (flagMC) simL2Mk->setMC();
  //simL2Mk->useStEvent(); // default : use muDst


  new StJanEventMaker; // produces binary event file - the main goal of this macro
  
  chain->ls(3);
  chain->Init();
 
  int t1=time(0);

  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "\n****************************************** " << endl;
    cout << "Working on eventNumber:\t" << iev <<"\tof:\t"<<nevents<<endl;
    cout << "****************************************** " << endl;
    chain->Clear();
    int iret = chain->Make(iev);
    total++;   
    if (iret % 10 == kStEOF || iret % 10 == kStFatal)  {
      cout << "Bad return code!" << endl;
      break;
    }
  }
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*total/(t2-t1);
  
  chain->Finish();
  cout << "****************************************** " << endl;
  cout << "total number of events  " << total << endl;
  cout << "****************************************** " << endl;


  TString fileMu=file;
  printf("=%s=\n",fileMu.Data());
  if(fileMu.Contains(".lis")) fileMu.ReplaceAll(".lis",".trgSim");
  if(fileMu.Contains(".MuDst.root")) fileMu.ReplaceAll(".MuDst.root",".trgSim");
  cout <<Form("sorting done %d of   nEve=%d, CPU rate=%.1f Hz, total time %.1f minute(s) \n\n",total,nEntries,rate,tMnt)<<endl;

}


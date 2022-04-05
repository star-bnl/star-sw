 
/* This macro is set up to calibrate barrel 
   This macro runs on real data (flagMC=0) and MC files (flagMC=1)
*/

int total=0;

void rdMu2BarCal(
		 char *file="R9060016f.lis",//"Day68a.lis", //"Ry.lis",//"R9067013f.lis",  //"R9064019f.lis", //
		 int calibPass=0x3, //bits: 0x0=raw,0x1=pedSubtr, 0x2=capIDfix
		 int nEvents = 1000,
		 int dbType=1, // 0=STAR only,  1=overwriteBPRSw128cap
		 int  isSched=0,
		 TString fileHi="aa", // is provided in scheduler mode
		 TString outDir="out1d/",
		 int bprsHist=1, // 1=commCap histo, 2=128capHisto
		 int dumm=0
		 ) 
{  
  char *dirIn ="runList/";		   
  int nFiles = 1000; // make this big if you want to read all events from a run

  if(isSched) { // setup changes for mass data processing with scheduler 
    dirIn ="";
    outDir="";
    nFiles=1000;
    //   nEvents =1e6;
  }

 int flagMC=0;  // 0== data, 1==M-C 
  if (flagMC)  file="mit0029.lis";

 
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  assert( !gSystem->Load("StDetectorDbMaker"));
  assert( !gSystem->Load("StDbUtilities"));
  assert( !gSystem->Load("StDbBroker"));
  assert( !gSystem->Load("St_db_Maker"));
  assert( !gSystem->Load("StDaqLib")); // needed by bemcDb
  assert( !gSystem->Load("StEmcRawMaker"));
  assert( !gSystem->Load("StEmcADCtoEMaker"));
  if (flagMC) {
    assert( !gSystem->Load("StMcEvent"));
    assert( !gSystem->Load("StMcEventMaker"));
    assert( !gSystem->Load("StEmcSimulatorMaker"));
    assert( !gSystem->Load("StEpcMaker"));
  }
  assert( !gSystem->Load("StBarrelCalib"));

  gROOT->Macro("LoadLogger.C");
  cout << " loading done " << endl;
  
  StChain *chain= new StChain("StChain"); 
 
  //....Needs MuDstMaker to get data
  printf(" dirIn=%s=  file=%s=\n",dirIn,file);  
  StMuDstMaker* muDstMaker =new StMuDstMaker(0,0,dirIn,file,"MuDst.root",nFiles);

  TChain* tree=muDstMaker->chain(); assert(tree); 
  int nEntries=(int) tree->GetEntries();
  cout << "Avaliable number of events  " << nEntries << endl;  
  
  if(nEvents>nEntries){
    nEvents=nEntries;
    cout<<" WARN: # of requested events reduced to "<< nEvents<<endl;
  }

  //Database -- get a real calibration from the database
  St_db_Maker* dbMk = new St_db_Maker("StarDb","MySQL:StarDb","MySQL:StarDb","$STAR/StarDb");
    
  //If MC then must set database time and date
  // if Endcap fast simu is used tower gains in DB do not matter,JB
  if(flagMC) {
    dbMk->SetDateTime(20080307, 83000);//timestamp R9067013
 }

  //Get BEMC adc values
  StEmcADCtoEMaker *a2eMk = new StEmcADCtoEMaker();
  a2eMk->setCheckStatus(kBarrelEmcTowerId,0); // force to ignore status tables in the offline DB
  a2eMk->saveAllStEvent(true); // preserve 1:1 copy of muDst content

  // if (flagMC) simL2Mk->setMC();

 //Collect all output histograms   
  TObjArray* HList=new TObjArray; 

  StJanBarrelDbMaker *jdbMk=new StJanBarrelDbMaker;
  jdbMk->setDbType(dbType);
  jdbMk->setHList(HList);

  StBarrelMonitorMaker *bmMk= new StBarrelMonitorMaker; 
  bmMk->setHList(HList);
  bmMk->setCalibPass(calibPass); 
  bmMk->setBprsHisto(bprsHist);

  chain->ls(3);
  chain->Init();
 
  int t1=time(0);

  for (Int_t iev=0;iev<nEvents; iev++) {
    cout << "\n****************************************** " << endl;
    cout << "Working on eventNumber:\t" << iev <<"\tof:\t"<<nEvents<<endl;
    cout << "****************************************** " << endl;
    chain->Clear();
    int iret = chain->Make(iev);
    total++;   
    if (iret % 10 == kStEOF || iret % 10 == kStFatal)  {
      cout << " my.C macro: Bad return code!" << endl;
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


  if(!isSched) {
	fileHi=file;	
  //"=%s=\n",fileMu.Data());
  if(fileHi.Contains(".lis")) fileHi.ReplaceAll(".lis",".barCal");
  if(fileHi.Contains(".MuDst.root")) fileHi.ReplaceAll(".MuDst.root",".barCal");
 }
 TString outF=outDir+fileHi;
  outF+="";
  outF+=".hist.root";
  cout<<"Output histo file "<<outF<<endl;
  hf=new TFile(outF,"recreate");
  if(hf->IsOpen()) {
    HList->ls();
    HList->Write();
    printf("\n Histo saved -->%s<\n",outF.Data());
  } else {
    printf("\n Failed to open Histo-file -->%s<, continue\n",outF.Data());
  }

  cout <<Form("sorting done %d of   nEve=%d, CPU rate=%.1f Hz, total time %.1f minute(s) \n\n",total,nEntries,rate,tMnt)<<endl;

}


class StChain;
StChain *chain=0;
int spinSort=false;
int useEtow=2;// 0=don't use; 1=only in event-display, 2=in away sum,3=in away&near sum
int isJustin=true; 
int geant=false;

int rdMuWana(
	     int nEve=1e3,
	     char* inDir   = "",// make it empty for scheduler 
	     char* file    = "/star/institutions/mit/balewski/freezer/2009-W-algoVer4.1/fillListA/R10097000_230531_230601.lis",// full fill F10505
	     int nFiles  = 1000, // max # of muDst files
	     int isMC=0 // 0=run9-data, 1=Weve, 2=QCDeve, 3=Zeve
 ) { 


  if(isMC==1) file  = "/star/institutions/mit/balewski/freezer/2009-W-algoVer4.1/fillListA/mcSetD1_ppWprod.lis";
  if(isMC==2) file  = "/star/institutions/mit/balewski/freezer/2009-W-algoVer4.1/fillListA/mcSetD2_ppQCD10_inf_filter.lis";
  if(isMC==3) file  = "/star/institutions/mit/balewski/freezer/2009-W-algoVer4.1/fillListA/mcSetD1_ppZprod.lis";
  if(isMC==4) file  = "fillListA/mcSetD1_ppWplusProd.lis";
  if(isMC==5) file  = "fillListA/mcSetD1_ppWminusProd.lis";
  if(isMC==6) file  = "fillListA/mcSetD1_ppWdec.lis";
  if(isMC==7) file  = "fillListA/mcSetD1_ppZdec.lis";
  if(isMC==8) geant=true; //uses geant files 

  if(isMC) spinSort=false;

  TString outF=file;
  int runNo=0,bht3ID=0,l2wID=0,fillNo=0;
  if(isMC==0) {//real data: runNum, trigID, L2ID from list name
    printf("Unpack file=%s=\n",file); 
    char *file1=strstr(file,"/R")+1;
    printf("file1=%s=%s=\n",file1);
    fillNo=atoi(file1-6);
    outF=file1; file1=outF.Data();
    char *p1=strstr(file1,"_");
    char *p2=strstr(p1+1,"_");
    runNo=atoi(file1+1);
    bht3ID=atoi(p1+1);
    l2wID=atoi(p2+1);
    p1[0]=0;
    outF=file1;
    outF=outF;
    printf("OutF=%s=\n",outF.Data());
  } 
  else if(geant){ //include geant.root files             
    char *file1=strstr(file,"/pp")+1;                      
    printf("file1=%s=%s=\n",file1);                        
    outF=file1; outF.ReplaceAll(".MuDst.root","");          
    TString fileG=file; fileG.ReplaceAll("MuDst","geant"); 
    fileG.ReplaceAll("/mu/","/geant/");                     
  }                                                        
  else { // MC events
    char *file1=strstr(file,"/mc")+1;
    printf("file1=%s=%s=\n",file1);
    outF=file1; file1=outF.Data();
    outF.ReplaceAll(".lis",""); 
  }
  printf("TRIG ID: bht3=%d l2w=%d isMC=%d\n",bht3ID,l2wID,isMC);
  
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gROOT->LoadMacro("LoadLogger.C");
  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOn("I");
  
  assert( !gSystem->Load("StDaqLib"));

  // libraries below are needed for DB interface
  assert( !gSystem->Load("StDbBroker"));
  assert( !gSystem->Load("St_db_Maker"));
  assert( !gSystem->Load("StDetectorDbMaker")); // for St_tpcGasC
  assert( !gSystem->Load("StEEmcUtil"));
  assert( !gSystem->Load("StEEmcDbMaker"));
  cout << " loading done " << endl;
  assert( !gSystem->Load("St2009Wana"));
  if(spinSort)  assert( !gSystem->Load("StSpinDbMaker"));
  
  // libraries for access to MC record         
  if(geant){                                  
    assert( !gSystem->Load("StMcEvent"));      
    assert( !gSystem->Load("StMcEventMaker")); 
  }

  // create chain    
  chain = new StChain("StChain"); 
  
  if(geant){                          
    // get geant file                    
    StIOMaker* ioMaker = new StIOMaker();
    printf("\n %s \n\n",fileG.Data());   
    ioMaker->SetFile(fileG.Data());      
    
    ioMaker->SetIOMode("r");             
    ioMaker->SetBranch("*",0,"1");   //deactivate all branches 
    ioMaker->SetBranch("geantBranch",0,"r");//activate geant Branch
    ioMaker->SetBranch("minimcBranch",0,"r");//activate geant Branch 
  }

  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  muMk->SetStatus("*",0);
  muMk->SetStatus("MuEvent",1);
  muMk->SetStatus("EmcTow",1);
  muMk->SetStatus("EmcSmde",1);
  muMk->SetStatus("EmcSmdp",1);
  muMk->SetStatus("PrimaryVertices",1);
  muMk->SetStatus("GlobalTracks",1);
  muMk->SetStatus("PrimaryTracks",1);

  if(geant){                                            
    StMcEventMaker *mcEventMaker = new StMcEventMaker();  
    mcEventMaker->doPrintEventInfo = false;               
    mcEventMaker->doPrintMemoryInfo = false;              
  }

  St_db_Maker   *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");
  if(isMC) { // use different DB tables for M-C data
    // .... Barrel....
    dbMk->SetFlavor("sim","bemcPed"); // set all ped=0
    dbMk->SetFlavor("sim","bemcStatus");  // ideal, all=on
    
    dbMk->SetFlavor("sim","bsmdePed"); // set all ped=0
    dbMk->SetFlavor("sim","bsmdeCalib"); // load those, not used in algo
    dbMk->SetFlavor("sim","bsmdeStatus"); // ideal, all=on
    
    dbMk->SetFlavor("sim","bsmdpPed");   // set all ped=0
    dbMk->SetFlavor("sim","bsmdpCalib"); // load those, not used in algo
    dbMk->SetFlavor("sim","bsmdpStatus"); // ideal, all=on

    //.... Endcap.....
    dbMk->SetFlavor("sim","eemcPMTcal");
    dbMk->SetFlavor("sim","eemcPMTstat");
    dbMk->SetFlavor("sim","eemcPMTped");
  } 
  else { // run 9 data
    dbMk->SetFlavor("Wbose","bsmdeCalib"); // Willie's relative gains E-plane
    dbMk->SetFlavor("Wbose","bsmdpCalib"); // P-plane
    dbMk->SetFlavor("missetTCD","eemcPMTcal");  // ETOW gains , not-standard
  }

  dbMk->SetFlavor("sim","bemcCalib"); // use ideal gains for 2009 real data as well
    
  //.... load EEMC database
  StEEmcDbMaker*  mEEmcDatabase = new StEEmcDbMaker("eemcDb");
  
  //.... W reconstruction code ....
  WmuMk=new St2009WMaker();
  if(isMC) { // MC specific
    WmuMk->setMC(isMC); //pass "version" of MC to maker
    WmuMk->setEtowScaleMC(1.3);
  }else {// real data specific
    WmuMk->setTrigID(bht3ID,l2wID);
  }
  
  
  /* to change default cuts activate any of the lines below */
  //WmuMk->setVertexCuts(101.,3);// vertexZ (cm), minPileupVert
  //WmuMk->setEleTrackCuts(16,0.52,91.,169.,10.1); //nFitP, fitFrac, Rin,Rout, elePt  
  //WmuMk->setEmcCuts(3,200.,15.,0.9,0.97);// kSigPed, maxAdc, clET, fr2/4
  //WmuMk->setWbosonCuts(28.,0.8,8.0); // 2x2ET, 2x2/nearJet, awayJet
  
  WmuMk->setMaxDisplayEve(10); // only first N events will get displayed 
  WmuMk->useEtow(useEtow);// 0=don't use; 1=only in event-display, 2=in away sum,3=in away&near sum

  /* evaluation of result, has full acess to W-algo internal data
     including overwrite - be careful */
  
  WpubMk=new St2009pubWanaMaker("pubJan"); 
  WpubMk->attachWalgoMaker(WmuMk); // 

  //Collect all output histograms   
  TObjArray* HList=new TObjArray;
  WmuMk->setHList(HList);
  WpubMk->setHList(HList);
  
  if(spinSort){
    spDb=new StSpinDbMaker("spinDb");
    pubSpinMk=new St2009pubSpinMaker("pubSpin"); 
    pubSpinMk->attachWalgoMaker(WmuMk);
    pubSpinMk->attachSpinDb(spDb);
    pubSpinMk->setHList(HList); 
  }  
  
  if(geant){
    pubMcMk=new St2009pubMcMaker("pubMc");
    pubMcMk->attachWalgoMaker(WmuMk);
    pubMcMk->setHList(HList);
  }
    
  // Justin's code
  if(isJustin){
    pubJSMk=new St2009pubJSMaker("pubJS");
    pubJSMk->attachWalgoMaker(WmuMk); 
    pubJSMk->setHList(HList); 
  }
  
  TChain* tree=muMk->chain(); assert(tree);
  int nEntries=(int) tree->GetEntries();
  printf("total eve in muDst chain =%d\n",nEntries);  // return ;
  if(nEntries<0) return;

  chain->Init();
  chain->ls(3);

  char txt[1000];
  //---------------------------------------------------
  int eventCounter=0;
  int t1=time(0);  
  TStopwatch tt;
  for (Int_t iev=0;iev<nEntries; iev++) {
    if(eventCounter>=nEve) break;
    chain->Clear();
    int stat = chain->Make();
    if(stat) break; // EOF or input error
    eventCounter++;    
  }

  cout<<"R"<<runNo<<" nEve="<<eventCounter<<" total "; tt.Print();
  printf("****************************************** \n");

  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*eventCounter/(t2-t1);
  printf("#sorting R%d done %d of   nEve= %d, CPU rate= %.1f Hz, total time %.1f minute(s) \n\n",runNo,eventCounter,nEntries,rate,tMnt);


  TString outFh=outF+".wana.hist.root";

  cout<<"Output histo file "<<outFh<<endl;
  hf=new TFile(outFh,"recreate");
  if(hf->IsOpen()) {
    //HList->ls();
    HList->Write();
    printf("\n Histo saved -->%s<\n",outFh.Data());
  } else {
    printf("\n Failed to open Histo-file -->%s<, continue\n",outFh.Data());
  }

  return;  
}


// $Log: rdMuWana.C,v $
// Revision 1.1  2009/11/23 23:00:20  balewski
// code moved spin-pool
//

TObjArray  *HList;
class MuEzPanitkinMaker;
MuEzPanitkinMaker  *myMk3;
//pp200, 2005, EJP1=96294, 

int rdEztPanitkin( int trigID=0,//96294,
 int nEve=100, 
 Int_t nFiles  =20,
 char* file="runList/R7101015.lis", //R6121034.lis,
 char* inDir   = "./"   
 ){ 

  //inDir="/star/data05/scratch/eemcdb/muDst/2005/048/";
  // file="st_physics_6048024_raw_2010006.MuDst.root";
  inDir=" myMuDst/";
  file="st_physics_7166007_raw_1040002.MuDst.root";

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  assert( !gSystem->Load("StEEmcPoolmuEztPanitkin")); 
  assert( !gSystem->Load("StEEmcUtil")); 

  // libraries below are needed for DB interface
  assert( !gSystem->Load("StDbBroker")); 
  assert( !gSystem->Load("St_db_Maker")); 
  assert( !gSystem->Load("StEEmcDbMaker")); 

  // create chain    
  chain = new StChain("StChain"); 

  printf("adding muDst from '%s' ....\n",file);

  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=(int) tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);
  printf("in=%s%s=\n",inDir,file);
  //return;

  St_db_Maker *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  stDb->SetFlavor("onlPed","eemcPMTped");
  
  new StEEmcDbMaker("EEmcDbMaker");
  
  HList=new  TObjArray;
  myMk3=new MuEzPanitkinMaker("myPanitkin","MuDst");
  myMk3->SetHList(HList);
  myMk3->SetTrigIdFilter(trigID);
  //myMk3->SetTrigIdFilter(66300); //zeroB

  //if(trigID==66007)
  // myMk3->SetHistoPixels();
  //  myMk3->SetSpy();
 


  {
   // Endcap specific DSM params
    int eemcDsmSetup[20]; // see StEemcTriggerSimu::initRun() for definition
    memset(eemcDsmSetup, 0,sizeof(eemcDsmSetup));// clear all, may be a bad default
    eemcDsmSetup[0]=3;  // HTthr0
    eemcDsmSetup[1]=12; // HTthr1
    eemcDsmSetup[2]=22; // HTthr2
    eemcDsmSetup[3]=1;  // TPthr0
    eemcDsmSetup[4]=17; // TPthr1
    eemcDsmSetup[5]=31; // TPthr2
    eemcDsmSetup[10]=2; //HTTPthrSelc, 2=use_thres_#1
    // myMk3->SetDsmSetup(eemcDsmSetup);
}


  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOn("I");
 
  muMk->SetStatus("*",0);
  muMk->SetStatus("EztAll",1);
  muMk->SetStatus("MuEvent",1);
  chain->Init();
  chain->ls(3);
  // muMk->printArrays();

  printf("All Ezt-branches set\n");
  int eventCounter=0;
  int stat=0;
  int t1=time(0);
  StMuTimer timer;
  timer.start();

  //---------------------------------------------------
  while ( 1) {// loop over events
    if(eventCounter>=nEve) break;
    eventCounter++;
    chain->Clear();
    stat = chain->Make();
    if(stat) break;
    if(eventCounter%200!=0)continue;

    printf("\n\n ====================%d  processing  ==============\n", eventCounter);
    
  }
  printf("sorting done, nEve=%d of %d\n",nEve, nEntries);
  int t2=time(0);
  if(t1==t2) t2++;
  float rate=1.*eventCounter/(t2-t1);
  float nMnts=(t2-t1)/60.;
  printf("sorting done %d of   nEve=%d, elapsed rate=%.1f Hz, tot %.1f minutes\n",eventCounter,nEntries,rate,nMnts);
  
  if (eventCounter) {
    cout << "CPU time/event= " << timer.elapsedTime()/eventCounter << " sec  "
	 << " rate= " << eventCounter/timer.elapsedTime() <<  " Hz" << endl;
  }
  
  myMk3->saveHisto("eemcQA");
  //  TString txt="R6035106-"; txt+=trigID;  myMk3->saveHisto(txt);
  
}

TObjArray  *HList;
class MuEzPanitkinMaker;
MuEzPanitkinMaker  *myMk3;
//pp200, 2005, EJP1=96294, 

int rdEztPanitkin( int trigID=96294,
 int nEve=2000, 
 Int_t nFiles  =10,
 char* file="lis/R6117003.lis", 
 char* inDir   = "./"   
 ){ 

  //inDir="/star/data05/scratch/eemcdb/muDst/2005/048/";
  // file="st_physics_6048024_raw_2010006.MuDst.root";

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
  int nEntries=tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);
  printf("in=%s%s=\n",inDir,file);
  //return;

  St_db_Maker *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  stDb->SetFlavor("onlPed","eemcPMTped");
  
  myDb=new StEEmcDbMaker("eemcDb");
  //myDb->setSectors(11,12);
  
  HList=new  TObjArray;
  myMk3=new MuEzPanitkinMaker("myPanitkin","MuDst");
  myMk3->SetHList(HList);
  myMk3->SetTrigIdFilter(trigID);
  //myMk3->SetTrigIdFilter(66300); //zeroB
  //myMk3->SetTrigIdFilter(66007); //minB
  //   myMk3->SetTrigIdFilter(66212); //EHT19
  // myMk3->SetTrigIdFilter(66210); //EHT10
  //if(trigID==66007) myMk3->SetHistoPixels();
  // myMk3->SetSpy();
 
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
    myDb->print();
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

class StChain;
class StMuEmcCollection;

class StEEmcDb;
class StMuDstMaker;
class TChain;

StEEmcDb *myDb;
StMuDstMaker* muMk;
StChain *chain=0;

int rdMu2histoExample( int nEve=5000 ){

  Int_t nFiles  = 1; 
  char* file="st_physics_12108020_raw_1020001.MuDst.root";
  char* inDir   = "/star/data05/scratch/balewski/mu2011/";
  TString fullName=file;

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");  
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEzExample");

  // create chain    
  chain = new StChain("StChain"); 

  printf("adding muDst from run '%s' ....\n",fullName.Data());

  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,fullName,".MuDst.root",nFiles);
  //switch on only ETOW branch I need
  muMk->SetStatus("*",0);
  muMk->SetStatus("MuEvent",1);
  muMk->SetStatus("EmcTow",1);

  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);



  St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");
  new StEEmcDbMaker("eemcDb");
  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOff("I");

  TObjArray  HList;

  myMk3=new StEEtowerExampleMaker("eeExample","MuDst");
  myMk3->Set(&HList);

  chain->ls();
  chain->Init();
  //  return;

  // read one event first to init DB, excluded from timing measurement

  chain->Clear();
  stat = chain->Make();

  int eventCounter=0;
  int stat=0;
  int t1=time(0);
  //---------------------------------------------------
  while ( stat==0 ) {// loop over events
    if(eventCounter>=nEve) break;
    eventCounter++;
    chain->Clear();
    stat = chain->Make();
    if(eventCounter%500==0)
      printf("\n ====================%d  processing  ============\n", eventCounter);
    
  }
  printf("sorting done, nEve=%d of %d\n",nEve, nEntries);
  //  return;
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float rate=1.*nEve/(t2-t1);
  printf("sorting done %d of   nEve=%d, CPU rate=%.1f Hz\n",eventCounter,nEntries,rate);

   chain->Finish();

   // save output histograms

   HList.ls();
   myMk3->saveHisto("aaa");

}

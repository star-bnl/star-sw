TObjArray  *HList;
class StEEstaleMaker;

int rdEztCorrupt(
 int nEve=3609,
 Int_t nFiles  = 20,
 char* file="R5135068ezB.lis", 
 char* inDir   = "./"
){ 
  char* inDir= "/star/data05/scratch/rfatemi/";
   file="LOCAL_st_physics_5313164_raw_1030001.MuDst.root";

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  assert( !gSystem->Load("StEEmcPoolEzCorruption")); 
  assert( !gSystem->Load("StEEmcUtil")); 

  // create chain    
  chain = new StChain("StChain"); 

  printf("adding muDst from '%s' ....\n",file);
  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);
  printf("in=%s=r=%s=\n",inDir,file);
  //return;

  HList=new  TObjArray;

  myMk3=new MuEzCorrMaker("muTurtle","MuDst");
  myMk3->SetHList(HList);
  myMk3->DoPrintout();

  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOn("I");
 

  muMk->SetStatus("*",0);
  muMk->SetStatus("EztAll",1);
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
    // printf ("stat=%d\n",stat);
   /*
    cout << "muEvtHeader array: " << muMk->muDst()->eztArray(0) << endl;
    cout << muMk->muDst()->eztArray(0)->GetEntries() << " entries" << endl;
    cout << "header at: " << muMk->muDst()->eztHeader() << endl;
    */
    if(eventCounter%1000!=0)continue;

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

  myMk3->saveHisto("muEztRace");

}

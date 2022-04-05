TObjArray  *HList;

int rdMu2Ped( 
 int nEve=300, 
 Int_t nFiles  =20,
 char* file="Rnnn.lis"
 ){ 
  char* inDir   = "./"   ;
  inDir="/star/data08/reco/ppProductionMinBias/FullField/dev/2005/174/";
  file="st_physics_6174067_raw_2040010.MuDst.root";

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  assert( !gSystem->Load("StEEmcUtil")); 
  assert( !gSystem->Load("StEEmcPoolmuDst")); 

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


  St_db_Maker *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  stDb->SetFlavor("onlPed","eemcPMTped");
  stDb->SetFlavor("sim","eemcPMTstat");

  myDb=new StEEmcDbMaker("eemcDb");
  
  HList=new  TObjArray;
  StAdcPedHistoMaker* myMk3=new StAdcPedHistoMaker("myPanitkin",muMk);
  myMk3->SetHList(HList);
  myMk3->SetTrigId(96011); // minB-trig for pp200
  //  myMk3->DoPedSubtraction();
  // myMk3->SetKillMask(0x0001 | 0x0002); // activates kill-byts as well

  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOn("I");

  chain->Init();
  chain->ls(3);

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
  
   TString out="outPed/";
   out+="Rnnn";
   out+="ped.hist.root";
   TFile f( out,"recreate");
   assert(f.IsOpen());
   printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),out.Data());
   HList->Write();
   f.Close();
   
  
}

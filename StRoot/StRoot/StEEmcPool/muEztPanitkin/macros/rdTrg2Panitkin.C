int rdTrg2Panitkin(int nEve=1000000,
		   char* file="R7072002",
		   char *pathIn="//star/data05/scratch/balewski/2006-L2algo-events/2006-data/072/",
		   char *pathOut="./wrk/hist/"
		   ){ 
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  // assert( !gSystem->Load("StEEmcPoolmuEztPanitkin")); // DEV version
  assert( !gSystem->Load(".sl305_gcc323/obj/StRoot/StEEmcPool/muEztPanitkin/StEEmcPoolmuEztPanitkin.so")); // local version
  assert( !gSystem->Load("StEEmcUtil")); 
  assert( !gSystem->Load("StEEmcPoolHanksTriggerDataReader")); 
  
  // create chain    
  chain = new StChain("StChain"); 
  
  TString iFile=file; iFile+=".dat";
  iFile=pathIn+iFile;

  trgRd = new StTriggerDataReader("TRGRD");
  if (trgRd->OpenFile(iFile.Data())==kStErr){
    printf("Error opening data file %s\n",file);
    return(kStErr);
  }else{
    printf("Opening data file %s\n",iFile.Data());
  }
  
  HList=new  TObjArray;
  myMk3=new StTrigOnlyPanitkinMaker("trig2Panitkin","MuDst");
  myMk3->SetHList(HList);

  chain->Init();
  chain->ls(3);
  
  int eventCounter=0;
  int stat=0;
  int t1=time(0);
  StMuTimer timer;
  timer.start();
  
  while ( 1) {// loop over events
    if(eventCounter>=nEve) break;
    eventCounter++;
    chain->Clear();        
    stat = chain->Make();
    if(stat) break;
    if(eventCounter%2000!=0)continue;
    
    printf("\n\n ====================%d  processing  ==============\n", eventCounter);
    
  }
 
  printf("sorting done, nEve=%d \n",nEve);
  int t2=time(0);
  if(t1==t2) t2++;
  float rate=1.*eventCounter/(t2-t1);
  float nMnts=(t2-t1)/60.;
  printf("sorting done nEve=%d , elapsed rate=%.1f Hz, tot %.1f minutes\n",eventCounter,rate,nMnts);
  
  if (eventCounter) {
    cout << "CPU time/event= " << timer.elapsedTime()/eventCounter << " sec  "
	 << " rate= " << eventCounter/timer.elapsedTime() <<  " Hz" << endl;
  }

  TString hFile=file;
  hFile=pathOut+hFile;
  myMk3->saveHisto(hFile);
  trgRd->CloseFile();
 }

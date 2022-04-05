void TDFReader(int run, int file=1, int nEvents=1000000, int debug=0, const char* dir="trgdata"){
  //void TDFReader(int run, const char* dir=".", int nEvents=10000){

  gROOT->Macro("Load.C");
  gSystem->Load("StIOMaker");
  gSystem->Load("StTriggerDataMaker");
  gSystem->Load("StRccCounterMonitor");
  
  cout << "Setting up chain" << endl;
  StChain* chain = new StChain;

  StIOMaker* iomaker = 0;
  char name[200]; 
  if(file>0){
    sprintf(name,"trgdata/run%d.%d.dat",run,file); 
    cout << "Reading up to "<<nEvents<<" events from "<<name<<endl;
    iomaker = new StIOMaker("IO","r", name);
  }else if(file>-999){ 
    StFile* files = new StFile();
    int id=run/1000;
    for(int i=0; i<-file; i++){
      int f=i*20+1;
      char* cmd[200];
      sprintf(cmd,"mkdir -p %d; cd %d; ln -s ../trgdata/run%d.%d.dat run%d_%d.dat",id,id,run,f,run,f);
      printf("%s\n",cmd);
      system(cmd);
      sprintf(name,"%d/run%d_%d.dat",id,run,f);
      printf("Adding %s\n",name);
      files->AddFile(name);
    }
    files->ls();
    iomaker = new StIOMaker("IO","r",files);
  }else{      
    StFile* files = new StFile();
    int id=run/1000;
    for(int i=0; i<2; i++){
      int f=i+1;
      char* cmd[200];
      sprintf(cmd,"mkdir -p %d; cd %d; ln -s ../trgdata/run%d.%d.dat run%d_%d.dat",id,id,run,f,run,f);
      printf("%s\n",cmd);
      system(cmd);
      sprintf(name,"%d/run%d_%d.dat",id,run,f);
      printf("Adding %s\n",name);
      files->AddFile(name);
    }
    files->ls();
    iomaker = new StIOMaker("IO","r",files);
  }

  StTriggerDataMaker* trgmaker = new StTriggerDataMaker();
  StRccCounterMonitor* rccmaker = new StRccCounterMonitor(run);
  rccmaker->setDebug(debug);
  
  cout << "Init Chain" << endl;
  chain->Init();

  cout << "Event Loop  nEvents=" << nEvents << endl;
  chain->EventLoop(0,nEvents);

  cout << "Finish Chain" << endl;
  chain->Finish();
  delete chain;
}

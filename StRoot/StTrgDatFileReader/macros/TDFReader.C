void TDFReader(const char *files="*.dat", int nEvents=3){
  cout << "Reading up to "<<nEvents<<" events\n";
  gROOT->Macro("Load.C");
  gSystem->Load("StIOMaker");
  gSystem->Load("StTriggerDataMaker");

  cout << "Setting up chain" << endl;
  StChain* chain = new StChain;
  StIOMaker* iomaker = new StIOMaker("IO","r", files);
  StTriggerDataMaker* trgmaker = new StTriggerDataMaker();
  trgmaker->setDebug(1);

  cout << "Init Chain" << endl;
  chain->Init();

  cout << "Event Loop  nEvents=" << nEvents << endl;
  chain->EventLoop(0,nEvents);

  cout << "Finish Chain" << endl;
  chain->Finish();
  delete chain;
}

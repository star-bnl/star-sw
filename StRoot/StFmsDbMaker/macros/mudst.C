void mudst(char * file="/star/data13/reco/production_dAu2008/ReversedFullField/P08ie/2008/021/9021048/st_fast_9021048_raw_1110017.MuDst.root", 
	   int nEvents=2){

  gROOT->Macro("loadMuDst.C");
  gSystem->Load("StDbBroker.so");
  gSystem->Load("St_db_Maker.so");
  gSystem->Load("StFmsDbMaker.so");

  cout << "Setting up chain" << endl;
  StChain* chain = new StChain;

  cout << "Setting up MuDstMaker with file=" << file << endl;
  StMuDstMaker* mudstmaker = new StMuDstMaker(0,0,"",file,"MuDst",1);

  cout << "Setting up St_db_Maker" << endl;
  St_db_Maker* dbMaker = new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
  dbMaker->SetDEBUG();
  dbMaker->SetDateTime(20090601, 0);  //set time stamp for the data base
  
  cout << "Setting up StFmsDbMaker" << endl;
  StFmsDbMaker* fmsdb = new StFmsDbMaker("fmsdbmaker");   
  fmsdb->setDebug(1);  //debug>0 to dump tables to text files
  
  cout << "Init Chain" << endl;
  chain->Init();  
  cout << "Event Loop  nEvents=" << nEvents << endl;
  chain->EventLoop(0,nEvents);

  cout << "Finish Chain" << endl;
  chain->Finish();
  delete chain;
}
 

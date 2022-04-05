void DbBroker(){

  // Baseline shared libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");

  // DB-specific libs

  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 

  StDbBroker* myBroker = new StDbBroker();
  int numRows; 

  myBroker->setVerbose(1);
  dbConfig_st* config=myBroker->InitConfig("StarDb",numRows);
  cout << "Number of rows returned " << numRows << endl;
  
  myBroker->SetDateTime(20000101,120000);

  // I happen to know in this test that tabID=60 for this.
  tpcDedxPidAmpDb_st* dedx=myBroker->Use(60,57);
  cout << "Dedx Gas Calibration "<<dedx->gasCalib << endl;

}



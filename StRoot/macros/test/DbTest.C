void DbTest(){

  // Baseline shared libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");

  // DB-specific libs
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
  gSystem->Load("St_db_Maker");

  // create makers connecting to Star databases 

  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");

  dbMk->Init();

  // Make reaquests for data
  // choose timestamp 
  dbMk->SetDateTime(20001001,10000);

  TDataSet *p = dbMk->GetDataBase("Calibrations/tpc");
//  p->ls(99);
  TTable *tb = p->Find("tpc/tpcDriftVelocity");
  printf("NRows=%d\n",tb->GetNRows());
  tb->Print(0,1);  

  TBrowser* b = new TBrowser("TestBrowser",p);

}








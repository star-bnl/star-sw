void DbTest(){

  // Baseline shared libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("St_Tables");

  // DB-specific libs

  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
  gSystem->Load("St_db_Maker");

  // create makers connecting to databases RunParams & Geometry

  St_db_Maker *dbMk = new St_db_Maker("db","MySQL:StarDb","StarDb","$STAR/StarDb");
  dbMk->SetFlavor("sim","tpcEffectiveGeom");
 
  dbMk->Init();

  // Make reaquests for data

  // choose timestamp 
  dbMk->SetDateTime(20010501,10000);

  TDataSet *p = dbMk->GetDataBase("Calibrations/tpc");
//  p->ls(99);
  TTable *tb = p->Find("tpc/tpcEffectiveGeom");
  printf("NRows=%d\n",tb->GetNRows());
  tb->Print(0,1);  


  // choose timestamp 
  dbMk->SetDateTime(20010501,10000);




 TBrowser* b = new TBrowser("TestBrowser",p);
}








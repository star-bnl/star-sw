void DbTest(){

  // Baseline shared libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");

  // DB-specific libs

  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
  gSystem->Load("St_db_Maker");

  // create makers connecting to databases RunParams & Geometry

  St_db_Maker *dbMk = new St_db_Maker("RunParams","MySQL:RunParams");
  St_db_Maker *dbMk2 = new St_db_Maker("Geometry","MySQL:Geometry");

  // Initial with [database]_hierarchy structure known to St_db_Maker

  dbMk->Init();
  dbMk2->Init();

  // Make reaquests for data

  p = dbMk->GetDataBase("RunParams/tpc");
  p2 = dbMk2->GetDataBase("Geometry/tpc");

  // Browse these requests

  TBrowser *b = new TBrowser("RunParams",p);
  TBrowser *b2 = new TBrowser("Geometry",p2);
}








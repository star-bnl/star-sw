void DbSave(){

  // Baseline shared libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");

  // DB-specific libs

  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
  gSystem->Load("St_db_Maker");

  // create makers connecting to databases RunParams & Geometry

  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");

 
  dbMk->Init();

  // Make reaquests for data

  // choose timestamp 
  dbMk->SetDateTime(19990101,10000);

  p = dbMk->GetDataBase("Calibrations/tpc");
  p->ls(99);
  dbMk->Save("Calibrations/tpc");
  

}








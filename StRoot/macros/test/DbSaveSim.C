void DbSaveSim(cons char* simTag){
  /*
   * possible tags 
   * 
   * static const char *aliases[]={
   * "sd97",   "sd98",   "year_1a","year_1b","year_1c",
   * "es99",   "er99",   "dc99"   ,"year_1d","year_1e",
   * "year_1h","year_2a", "year_2b", 0};   
   *
   */

  // Baseline shared libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");

  // DB-specific libs

  gSystem->Load("StUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
  gSystem->Load("St_db_Maker");

  // create makers connecting to databases RunParams & Geometry

  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");
  dbMk->Init();

  // choose timestamp from simulation tag.
  dbMk->SetDateTime(simTag);

  // Make requests for data per database Type
  p0 = dbMk->GetDataBase("Calibrations");
  p1 = dbMk->GetDataBase("Geometry");
  p2 = dbMk->GetDataBase("Conditions");
  p3 = dbMk->GetDataBase("RunLog");

  // save data a local copy in files per database type
  dbMk->Save("Calibrations");
  dbMk->Save("Geometry");
  dbMk->Save("Conditions");
  dbMk->Save("RunLog");
}








#include <iostream.h> 
#include <fstream.h> 

void fpsConstant_db(char* opt="", char* year="15sim") {
  TString option(opt), yr(year);  
  TString storeTime; // storetime is begin time for validity range for WRITING DB
  int date,time;     // time for READING DB
  std::cout << "year = " << year << "\n";
  if(yr.Contains("15ofl")){
    storeTime="2014-12-20 00:00:01";
    date = 20141220; time = 1;
  }else if(yr.Contains("15sim")){
    storeTime="2014-12-10 00:00:02";
    date = 20141210; time = 2;
  }else if(yr.Contains("17ofl")){
    storeTime="2016-12-20 00:00:00";
    date = 20161220; time = 0;
  }else if(yr.Contains("17sim")){
    storeTime="2016-12-10 00:00:00";
    date = 20161210; time = 0;
  }else{
    std::cout << "Please specify valid year tag\n"; exit;
  }
  std::cout << "Opt =" << opt << "\n";
  std::cout << "writedb = " << option.Contains("write")   << "\n";
  std::cout << "storetime =" << storeTime << "\n";
  std::cout << "date,time =" << date <<" "<< time << "\n";
  
  gROOT->Macro("loadlib.C");
  
  fpsConstant_st table;
  if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Geometry_fps");
    StDbTable* dbtable = node->addDbTable("fpsConstant");
    mgr->setStoreTime(storeTime.Data());

    table.nQuad = 4; 
    table.nLayer = 3; 
    table.maxSlat = 21; 
    table.maxQTaddr = 8; 
    table.maxQTch = 32; 

    dbtable->SetTable((char*)&table, 1);
    if(yr.Contains("sim")) dbtable->setFlavor("sim");
    mgr->storeDbTable(dbtable);    
    std::cout << "INFO: table saved to database" << std::endl;
  }

  std::cout << "INFO: Reading database" << std::endl;  
  gSystem->Unsetenv("DB_ACCESS_MODE");
  //gSystem->Unsetenv("DB_SERVER_LOCAL_CONFIG");
  St_db_Maker *dbMk=new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
  dbMk->SetDebug();
  dbMk->SetDateTime(date,time); // event or run start time, set to your liking
  if(yr.Contains("ofl"))      {dbMk->SetFlavor("ofl");}
  else if(yr.Contains("sim")) {dbMk->SetFlavor("sim");}
  dbMk->Init();
  dbMk->Make();
  TDataSet *DB = 0;
  DB = dbMk->GetDataBase("Geometry/fps/fpsConstant");
  if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
  St_fpsConstant *dataset = 0;
  dataset = (St_fpsConstant*) DB->Find("fpsConstant");
  if (!dataset) {
    td::cout << "ERROR: dataset does not contain requested table" << std::endl;
    return;
  }
  Int_t rows = dataset->GetNRows();
  if (rows > 1) {
    std::cout << "INFO: found INDEXED table with " << rows << " rows" << std::endl;
  }
  TDatime val[2];
  dbMk->GetValidity((TTable*)dataset,val);
  std::cout << "Dataset validity range: [ " << val[0].GetDate() << "." << val[0].GetTime() << " - " 
	    << val[1].GetDate() << "." << val[1].GetTime() << " ] "
	    << std::endl;    
  fpsConstant_st *tbl = dataset->GetTable();
  for (Int_t i = 0; i < rows; i++) {
    std::cout << Form("Row=%d nQuad=%1d nLayer=%1d maxSlat=%2d maxQTaddr=%1d maxQTch=%2d\n",i,
		      tbl[i].nQuad,tbl[i].nLayer,tbl[i].maxSlat,
		      tbl[i].maxQTaddr,tbl[i].maxQTch);
  }  
}

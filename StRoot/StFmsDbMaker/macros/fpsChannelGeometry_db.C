#include <iostream.h> 
#include <fstream.h> 

void fpsChannelGeometry_db(char* opt="", char* year="15sim") {
  TString option(opt), yr(year);  
  TString storeTime; // storetime is begin time for validity range for WRITING DB
  int date,time;     // time for READING DB
  std::cout << "year = " << year << "\n";
  if(yr.Contains("15ofl")){
    storeTime="2014-12-20 00:00:01";
    date = 20141220; time = 1;
  }else if(yr.Contains("15sim")){
    storeTime="2014-12-10 00:00:01";
    date = 20141210; time = 1;
  }else if (yr.Contains("17ofl")){      
      storeTime="2016-12-20 00:00:00";
      date = 20161220; time = 0;
  }else if(yr.Contains("17sim")){
      storeTime="2016-12-10 00:00:00";
      date = 20161210; time = 0;      
  }else{
    std::cout << "Please specify valid year tag\n"; exit;
  }
  std::cout << "Opt =" << opt << "\n";
  std::cout << "write = " << option.Contains("writedb")   << "\n";
  std::cout << "storetime =" << storeTime << "\n";
  std::cout << "date,time =" << date <<" "<< time << "\n";
  
  gROOT->Macro("loadlib.C");
  
  const Int_t MAX_DB_INDEX = 12;
  fpsChannelGeometry_st table[MAX_DB_INDEX];

  if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Geometry_fps");
    StDbTable* dbtable = node->addDbTable("fpsChannelGeometry");
    mgr->setStoreTime(storeTime.Data());

    table[ 0].quad = 1; table[ 0].layer = 1; table[ 0].nslat = 21; 
    table[ 1].quad = 1; table[ 1].layer = 2; table[ 1].nslat = 21; 
    table[ 2].quad = 1; table[ 2].layer = 3; table[ 2].nslat = 21; 
    table[ 3].quad = 2; table[ 3].layer = 1; table[ 3].nslat = 19; 
    table[ 4].quad = 2; table[ 4].layer = 2; table[ 4].nslat = 19; 
    table[ 5].quad = 2; table[ 5].layer = 3; table[ 5].nslat = 19; 
    table[ 6].quad = 3; table[ 6].layer = 1; table[ 6].nslat = 21; 
    table[ 7].quad = 3; table[ 7].layer = 2; table[ 7].nslat = 21; 
    table[ 8].quad = 3; table[ 8].layer = 3; table[ 8].nslat = 21; 
    table[ 9].quad = 4; table[ 9].layer = 1; table[ 9].nslat = 19; 
    table[10].quad = 4; table[10].layer = 2; table[10].nslat = 19; 
    table[11].quad = 4; table[11].layer = 3; table[11].nslat = 19; 

    dbtable->SetTable((char*)&table, MAX_DB_INDEX);
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
  DB = dbMk->GetDataBase("Geometry/fps/fpsChannelGeometry");
  if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
  St_fpsChannelGeometry *dataset = 0;
  dataset = (St_fpsChannelGeometry*) DB->Find("fpsChannelGeometry");
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
  fpsChannelGeometry_st *tbl = dataset->GetTable();
  for (Int_t i = 0; i < rows; i++) {
    std::cout << Form("Row=%4d quad=%1d layer=%1d nslat=%2d\n",i,
		      tbl[i].quad,tbl[i].layer,tbl[i].nslat);
  }
}

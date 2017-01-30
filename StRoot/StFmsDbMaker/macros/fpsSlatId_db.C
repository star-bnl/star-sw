#include <iostream.h> 
#include <fstream.h> 

void fpsSlatId_db(char* opt="", char* year="15sim") {
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
  std::cout << "write = " << option.Contains("writedb")   << "\n";
  std::cout << "storetime =" << storeTime << "\n";
  std::cout << "date,time =" << date <<" "<< time << "\n";
  
  gROOT->Macro("loadlib.C");
  
  const Int_t MAX_DB_INDEX = 252;
  fpsSlatId_st table[MAX_DB_INDEX];

  if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Geometry_fps");
    StDbTable* dbtable = node->addDbTable("fpsSlatId");
    mgr->setStoreTime(storeTime.Data());
    
    int id=0;
    for (Int_t q = 1; q <= 4; q++) {
      for (Int_t l = 1; l <= 3; l++) {
	for (Int_t s = 1; s <= 21; s++) {
	  table[id].slatid = id; 
	  int flag=0;
	  if(q==2 || q==4){
	    if(s>=20) flag=1;
	  }	    
	  if(flag==0){table[id].quad = q; table[id].layer = l; table[id].slat = s;}
	  else       {table[id].quad = 0; table[id].layer = 0; table[id].slat = 0;}
	  id++;
	}
      }
    }
    
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
  DB = dbMk->GetDataBase("Geometry/fps/fpsSlatId");
  if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
  St_fpsSlatId *dataset = 0;
  dataset = (St_fpsSlatId*) DB->Find("fpsSlatId");
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
  fpsSlatId_st *tbl = dataset->GetTable();
  for (Int_t i = 0; i < rows; i++) {
    std::cout << Form("Row=%4d slatid=%3d quad=%2d layer=%2d slat=%2d\n",i,
		      tbl[i].slatid,tbl[i].quad,tbl[i].layer,tbl[i].slat);
  }
}

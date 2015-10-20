#include <iostream.h> 
#include <fstream.h> 

void fpsGain_db(char* opt="", char* year="15sim") {
  TString option(opt), yr(year);  
  TString storeTime; // storetime is begin time for validity range for WRITING DB
  int date,time;     // time for READING DB
  std::cout << "year = " << year << "\n";
  if(yr.Contains("15ofl")){
    storeTime="2014-12-20 00:00:01";
    date = 20141225; time = 0;
  }else if(yr.Contains("15sim")){
    storeTime="2014-12-10 00:00:02";
    date = 20141215; time = 0;
  }else{
    std::cout << "Please specify valid year tag\n"; exit;
  }
  std::cout << "Opt =" << opt << "\n";
  std::cout << "write = " << option.Contains("writedb")   << "\n";
  std::cout << "storetime =" << storeTime << "\n";
  std::cout << "date,time =" << date <<" "<< time << "\n";
  
  gROOT->Macro("loadlib.C");
  
  const Int_t MAX_DB_INDEX = 252;
  fpsGain_st table[MAX_DB_INDEX];

  if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_fps");
    StDbTable* dbtable = node->addDbTable("fpsGain");
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
	  float mip=100.0;
	  if(l==3) mip=50.0;
	  if(flag==0){table[id].slatid = id; table[id].MIP=mip;} 
	  else       {table[id].slatid = id; table[id].MIP=0;} 
	  printf("id=%3d Q%1dL%1dS%2d mip=%f\n",id,q,l,s,table[id].MIP);
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
  DB = dbMk->GetDataBase("Calibrations/fps/fpsGain");
  if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
  St_fpsGain *dataset = 0;
  dataset = (St_fpsGain*) DB->Find("fpsGain");
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
  fpsGain_st *tbl = dataset->GetTable();
  for (Int_t i = 0; i < rows; i++) {
    std::cout << Form("Row=%4d slatid=%3d MIP=%8.4f\n",i,
		      tbl[i].slatid,tbl[i].MIP);
  }
}

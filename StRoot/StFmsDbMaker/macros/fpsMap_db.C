#include <iostream.h> 
#include <fstream.h> 

void fpsMap_db(char* opt="", char* year="15sim", char* input="fpsmap.txt") {
  TString option(opt), yr(year);  
  TString storeTime; // storetime is begin time for validity range for WRITING DB
  int date,time;     // time for READING DB
  std::cout << "year = " << year << "\n";
  if(yr.Contains("15ofl")){
    storeTime="2014-12-20 00:00:3";
    date = 20141221; time = 0;
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
  
  gROOT->Macro("./loadlib.C");
  
  const Int_t MAX_DB_INDEX = 252;
  fpsMap_st in[MAX_DB_INDEX];
  for(int i=0; i<MAX_DB_INDEX; i++){
    in[i].slatid=i;
    in[i].QTaddr=-1;
    in[i].QTch=-1;
  }
  
  FILE *FP = fopen(input,"r");
  if(!FP) { printf("Could not open %s\n",input); exit;}
  printf("Reading %s\n",input);
  char line[1000], nw[10], dc[10], posi[10];
  int id,q,l,s,sipm,sipmbd,feebd,tbox,tgrp,qtaddr,qtch,cbl;
  int n=0;
  while(fgets(line,1000,FP)!=NULL){
    sscanf(line,"%d %d %d %d %d %s %d %d %d %d",
	   &id,&q,&l,&s,
	   &sipm,nw,
	   &tbox,&tgrp,
	   &qtaddr,&qtch);
    //printf("Id=%3d Q=%3d L=%3d S=%3d QTaddr=%1d QTch=%2d\n",id,q,l,s,qtaddr,qtch);
    in[id].QTaddr=qtaddr;
    in[id].QTch=qtch;
    n++;
  }    
  for(int i=0; i<MAX_DB_INDEX; i++){
    printf("Id=%3d QTaddr=%1d QTch=%2d\n",in[i].slatid,in[i].QTaddr,in[i].QTch);
  }
  printf("Found %d entries\n",n);
  
  if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Geometry_fps");
    StDbTable* dbtable = node->addDbTable("fpsMap");
    mgr->setStoreTime(storeTime.Data());    
    dbtable->SetTable((char*)&in, MAX_DB_INDEX);
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
  DB = dbMk->GetDataBase("Geometry/fps/fpsMap");
  if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
  St_fpsMap *dataset = 0;
  dataset = (St_fpsMap*) DB->Find("fpsMap");
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
  fpsMap_st *table = dataset->GetTable();
  for (Int_t i = 0; i < rows; i++) {
    std::cout << Form("Row=%4d slatid=%3d QTaddr=%2d QTch=%2d\n",i,
		      table[i].slatid,
		      table[i].QTaddr,table[i].QTch);
  }
}

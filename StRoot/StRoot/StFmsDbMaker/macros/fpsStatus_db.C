#include <iostream.h> 
#include <fstream.h> 

void fpsStatus_db(char* opt="", char* year="15ofl", int badch=0, int readdate=0, int readtime=0) {
  TString option(opt), yr(year);  
  TString storeTime; // storetime is begin time for validity range for WRITING DB
  int date,time;     // time for READING DB
  std::cout << "year = " << year << "\n";
  if(yr.Contains("15ofl")){
    storeTime="2014-12-20 00:00:00";
    date = 20141220; time = 1;
  }else if(yr.Contains("15sim")){
    storeTime="2014-12-10 00:00:00";
    date = 20141210; time = 1;
  }else{
    std::cout << "Please specify valid year tag\n"; exit;
  }
  if(readdate>0) date=readdate;
  if(readtime>0) time=readtime;
  std::cout << "Opt =" << opt << "\n";
  std::cout << "write = " << option.Contains("writedb")   << "\n";
  std::cout << "storetime =" << storeTime << "\n";
  std::cout << "read date,time =" << date <<" "<< time << "\n";
  
  TString storeTime1, storeTime2; 
  int badq,badl,bads,status1,status2;  
  if(badch==1){
    badq=3; badl=3; bads=16;
    storeTime1="2015-02-20 00:00:00"; status1=1;
    storeTime2="2015-07-04 00:00:00"; status2=0;
  }else if(badch==2){
    badq=1; badl=1; bads=1;
    storeTime1="2015-04-15 12:00:00"; status1=1;
    storeTime2="2015-04-27 12:00:00"; status2=0;
  }else if(badch==3){
    badq=4; badl=1; bads=14;
    storeTime1="2015-04-15 12:00:01"; status1=1;
    storeTime2="2015-04-22 12:00:00"; status2=0;
  }

  gROOT->Macro("loadlib.C");
  
  const Int_t MAX_DB_INDEX = 252;
  fpsStatus_st table[MAX_DB_INDEX];
  
  int id=0;
  if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_fps");
    StDbTable* dbtable = node->addDbTable("fpsStatus"); 
    if(yr.Contains("sim")) dbtable->setFlavor("sim");
   
    if(badch==0){
      mgr->setStoreTime(storeTime.Data());
      id=0;
      for (Int_t q = 1; q <= 4; q++) {
	for (Int_t l = 1; l <= 3; l++) {
	  for (Int_t s = 1; s <= 21; s++) {
	    table[id].slatid = id; 
	    int flag=0;
	    if(q==2 || q==4){
	      if(s>=20) flag=1;
	    }	    
	    if(flag==0){table[id].slatid = id; table[id].status=0;} 
	    else       {table[id].slatid = id; table[id].status=9;} 
	    id++;
	  }
	}
      }
      dbtable->SetTable((char*)&table, MAX_DB_INDEX);
      mgr->storeDbTable(dbtable);    
      std::cout << "INFO: table saved to database" << std::endl;
    }else{
      int idList[1];
      id=(badq-1)*3*21+(badl-1)*21+(bads-1);
      idList[0]=id+1;
      table[0].slatid = id; 
      
      table[0].status= status1;
      mgr->setStoreTime(storeTime1.Data());
      dbtable->SetTable((char*)&table,1,idList);
      mgr->storeDbTable(dbtable);
      printf("Q%1dL%1dS%02d SlatId=%3d Row=%3d Stat=%d saved on %s\n",
	     badq,badl,bads,id,idList[0],table[0].status,storeTime1.Data());

      table[0].status= status2;
      mgr->setStoreTime(storeTime2.Data());
      dbtable->SetTable((char*)&table,1,idList);
      mgr->storeDbTable(dbtable);
      printf("Q%1dL%1dS%02d SlatId=%3d Row=%3d Stat=%d saved on %s\n",
	     badq,badl,bads,id,idList[0],table[0].status,storeTime2.Data());
    }
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
  DB = dbMk->GetDataBase("Calibrations/fps/fpsStatus");
  if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
  St_fpsStatus *dataset = 0;
  dataset = (St_fpsStatus*) DB->Find("fpsStatus");
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
  fpsStatus_st *tbl = dataset->GetTable();
  id=(badq-1)*3*21+(badl-1)*21+(bads-1);
  for (Int_t i = 0; i < rows; i++) {
    if(badch>0 && i!=id) continue;
    std::cout << Form("Row=%4d slatid=%3d status=%d\n",i,
		      tbl[i].slatid,tbl[i].status);
  }
}

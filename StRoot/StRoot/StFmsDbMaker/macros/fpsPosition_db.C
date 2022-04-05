#include <iostream.h> 
#include <fstream.h> 

void fpsPosition_db(char* opt="", char* year="15ofl", char* input="fpsgeom_run15_20170925.txt") {
  TString option(opt), yr(year);  
  TString storeTime; // storetime is begin time for validity range for WRITING DB
  int date,time;     // time for READING DB
  std::cout << "year = " << year << "\n";
  if(yr.Contains("15ofl")){
    storeTime="2014-12-20 00:00:05";
    date = 20141225; time = 1;
  }else if(yr.Contains("15sim")){
    storeTime="2014-12-10 00:00:00";
    date = 20141215; time = 0;
  }else if(yr.Contains("17ofl")){
      storeTime="2016-12-20 00:00:02";
      date = 20161225; time = 0;
  }else if(yr.Contains("17sim")){
      storeTime="2016-12-10 00:00:01";
      date = 20161210; time = 1;
  }else{
    std::cout << "Please specify valid year tag\n"; exit;
  }
  std::cout << "Opt =" << opt << "\n";
  std::cout << "write = " << option.Contains("writedb")   << "\n";
  std::cout << "storetime =" << storeTime << "\n";
  std::cout << "date,time =" << date <<" "<< time << "\n";
  
  gROOT->Macro("./loadlib.C");
  
  const Int_t MAX_DB_INDEX = 252;
  fpsPosition_st in[MAX_DB_INDEX];
  
  FILE *FP = fopen(input,"r");
  if(!FP) { printf("Could not open %s\n",input); exit;}
  printf("Reading %s\n",input);
  char line[1000];  
  int q,l,s,n=0;
  while(fgets(line,1000,FP)!=NULL){
    sscanf(line,"%d %d %d %d %f %f %f %f %f %f",
	   &in[n].slatid,&q,&l,&s,
	   &in[n].xwidth, &in[n].ywidth, &in[n].zwidth,
	   &in[n].xoffset, &in[n].yoffset, &in[n].zoffset);
    printf("Id=%3d Q=%3d L=%3d S=%3d D=%9.4f %9.4f %9.4f X=%9.4f %9.4f %9.4f\n",
           in[n].slatid,q,l,s,
           in[n].xwidth, in[n].ywidth, in[n].zwidth,
           in[n].xoffset, in[n].yoffset, in[n].zoffset);
    n++;
  }  
  printf("Found %d entries\n",n);
  
  if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Geometry_fps");
    StDbTable* dbtable = node->addDbTable("fpsPosition");
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
  DB = dbMk->GetDataBase("Geometry/fps/fpsPosition");
  if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
  St_fpsPosition *dataset = 0;
  dataset = (St_fpsPosition*) DB->Find("fpsPosition");
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
  fpsPosition_st *table = dataset->GetTable();
  for (Int_t i = 0; i < rows; i++) {
    std::cout << Form("Row=%4d slatid=%3d x=%9.4f %9.4f %9.4f d=%9.4f %9.4f %9.4f\n",i,
		      table[i].slatid,
		      table[i].xwidth,table[i].ywidth,table[i].zwidth,
		      table[i].xoffset,table[i].yoffset,table[i].zoffset);
  }
}

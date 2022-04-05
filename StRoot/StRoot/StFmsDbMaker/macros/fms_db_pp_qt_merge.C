#include <iostream.h> 
#include <fstream.h> 

void fms_db_pp_qt_merge(char* opt, int year) {

  TString option(opt);
  std::cout << "Opt =" << opt << "\n";
  std::cout << "merge (read fmsPatchPanelmap and fmsQTmap from DB and merge to create fmsMap) = " << option.Contains("merge") << "\n";
  std::cout << "readdb (read fmsMap from DB)     = " << option.Contains("readdb")    << "\n";
  std::cout << "writedb (write fmsMap from DB)   = " << option.Contains("writedb")   << "\n";
  std::cout << "writetext (write fmsMap from DB )= " << option.Contains("writetext") << "\n";
  std::cout << "\n";  

  TString filename;  // output filename
  TString storeTime; // storetime is begin time for validity range for WRITING DB
  int date,time;     // time for READING DB
  std::cout << "year = " << year << "\n";  
  if(year==8){
    storeTime="2007-11-09 00:00:01";
    date = 20080301;
    time = 0;
  }elseif(year==9){
    storeTime="2008-11-09 00:00:00";
    date = 20090101;
    time = 0;
  }else{
    std::cout << "Please specify year\n";
    exit;
  }
  std::cout << "storetime =" << storeTime << "\n";
  std::cout << "date,time =" << date <<" "<< time << "\n";
  if(option.Contains("merge")) filename="fms_db_pp_qt_merge.txt";
  if(option.Contains("readdb")) filename="fms_db_pp_qt_merge.txt_dbout";
				 
  
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("St_base.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");

  // max index dimensions
  int maxch[4]  = {578,578,288,288};
  int nsmap[4]  = {  0,  1,  0,  1};
  const Int_t MAX_PP_MAP = 4;
  const Int_t MAX_QT_MAP = 2;
  const Int_t MAX_MAP = 578 + 578 + 288 + 288;
  int nmap = 0;

  // structure to fill up
  fmsPatchPanelMap_st ppmap[MAX_PP_MAP];
  fmsQTMap_st qtmap[MAX_QT_MAP];
  fmsMap_st map[MAX_MAP];

  gSystem->Load("StChain");
  gSystem->Load("StBFChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_Tables");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  
  St_db_Maker *dbMk=new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
  dbMk->SetDebug();
  dbMk->SetDateTime(date,time);
  dbMk->SetFlavor("ofl"); // for offline calibrations/mapping
  // dbMk->SetFlavor("simu"); // for simulations
  dbMk->Init();
  dbMk->Make();
  
  // this is done inside ::Make method
  TDataSet *DB = 0;
  // "dbMk->" will NOT be needed. 
  //  if done inside your FmsDbMaker. Simply use DB = GetInputDb("Calibrations/fms/mapping")
  DB = dbMk->GetInputDB("Calibrations/fms/mapping");
  if (!DB) { std::cout << "ERROR: no db maker?" << std::endl; return 0;}
  
  if(option.Contains("merge")){
    gSystem->Unsetenv("DB_ACCESS_MODE");
    gSystem->Unsetenv("DB_SERVER_LOCAL_CONFIG");
    
    // fetch ROOT fmsPatchPanelMap descriptor of db table
    St_fmsPatchPanelMap *dbppmap = 0;
    dbppmap = (St_fmsPatchPanelMap*) DB->Find("fmsPatchPanelMap");
    // fetch data and place it to appropriate structure
    if (dbppmap) {
      std::cout << "Reading fmsPatchPanelMap table\n";
      fmsPatchPanelMap_st *pptable = dbppmap->GetTable();
      memcpy(ppmap,pptable,sizeof(ppmap));
    } else {
      std::cout << "WARNING: No data in fmsPatchPanelMap table (wrong timestamp?). Nothing to return, then.\n";
    }
    
    // fetch ROOT fmsQTmap descriptor of db table
    St_fmsQTMap *dbqtmap = 0;
    dbqtmap = (St_fmsQTMap*) DB->Find("fmsQTMap");
    // fetch data and place it to appropriate structure
    if (dbqtmap){
      std::cout << "Reading fmsQTMap table\n";
      fmsQTMap_st *qttable = dbqtmap->GetTable();
      memcpy(qtmap,qttable,sizeof(qtmap));
    } else {
      std::cout << "WARNING: No data in fmsQTMap table (wrong timestamp?). Nothing to return, then.\n";
    }
    
    // merge 2 tables
    nmap=0;
    for(int mod=0; mod<4; mod++){
      int detid = mod+8;
      int ns=nsmap[mod];
      printf("mod=%d detid=%d ns=%d maxch=%d\n",mod,detid,ns,maxch[mod]);
      for(int ch=0; ch<maxch[mod]; ch++){
	int pp   = ppmap[mod].ppPanel[ch];
	int row  = ppmap[mod].ppRow[ch];
	int col  = ppmap[mod].ppColumn[ch];
	map[nmap].detectorId = detid;
	map[nmap].ch         = ch+1;
	if(pp==0 && row==0 && col==0){
	  map[nmap].qtCrate    = 0;
	  map[nmap].qtSlot     = 0;
	  map[nmap].qtChannel  = 0;
	}else{
	  map[nmap].qtCrate    = qtmap[ns].qtCrate[pp-1][row-1][col-1];
	  map[nmap].qtSlot     = qtmap[ns].qtSlot[pp-1][row-1][col-1];
	  map[nmap].qtChannel  = qtmap[ns].qtChannel[pp-1][row-1][col-1];
	}
	nmap++;
	//printf("%3d %3d %3d %3d %3d %3d %3d %3d %3d %3d\n",
	//       mod,detid,ns,ch,pp,row,col,map[nmap].qtCrate,map[nmap].qtSlot,map[nmap].qtChannel);
      }
    }
  }
  
  if(option.Contains("readdb")){
    gSystem->Unsetenv("DB_ACCESS_MODE");
    gSystem->Unsetenv("DB_SERVER_LOCAL_CONFIG");
    nmap=0;
    // fetch ROOT fmsMap descriptor of db table
    St_fmsMap *dbmap = 0;
    dbmap = (St_fmsMap*) DB->Find("fmsMap");
    // fetch data and place it to appropriate structure
    if (dbmap) {
      fmsMap_st *table = dbmap->GetTable();
      nmap = dbmap->GetNRows();
      //      nmap = MAX_MAP;
      std::cout << "Reading fmsMap table with nrow = "<<nmap<<"\n";
      memcpy(map,table,sizeof(fmsMap_st)*nmap);
    } else {
      std::cout << "WARNING: No data in fmsMap table (wrong timestamp?). Nothing to return, then.\n";
    }
  }
  
  if(option.Contains("writetext")){
    char* file=filename.Data();
    FILE* fp;
    int crt,slt,ch;
    cout << "Writing "<<file<<"\n";
    if(fp=fopen(file,"w")){
      fprintf(fp,"%d\n",nmap);
      for(int i=0; i<nmap; i++){
	fprintf(fp,"%3d %4d %3d %3d %3d\n",map[i].detectorId,map[i].ch,
		map[i].qtCrate, map[i].qtSlot, map[i].qtChannel);
      }
    }
  }
  
  if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    cout << "DB_ACCESS_MODE="<<gSystem->Getenv("DB_ACCESS_MODE")<<endl;
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_fms");
    StDbTable* table = node->addDbTable("fmsMap");	  
    mgr->setStoreTime(storeTime.Data());
    // store data in the table
    table->SetTable((char*)&map,nmap);
    // store table in dBase
    mgr->storeDbTable(table); 
    gSystem->Unsetenv("DB_ACCESS_MODE");
    std::cout << "Done with database upload \n";
  }
}


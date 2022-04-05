#include <iostream.h> 
#include <fstream.h> 

void fms_db_ChannelGeometry(char* opt="readdb", int year = 15) {

  TString option(opt);
  std::cout << "Opt =" << opt << "\n";
  std::cout << "readdb = " << option.Contains("readdb")    << "\n";
  std::cout << "writedb = " << option.Contains("writedb")   << "\n";
  std::cout << "\n";  

  TString storeTime; // storetime is begin time for validity range for WRITING DB
  int date,time;     // time for READING DB
  std::cout << "year = " << year << "\n";
  if(year==8){
    storeTime="2007-11-09 00:00:02";
    date = 20080301;
    time = 0;
  }else if(year==9){
    storeTime="2008-11-09 00:00:01";
    date = 20090301;
    time = 0;
  }else if(year=15){
    storeTime="2014-12-20 00:00:00";
    date = 20141220;
//    date = 20140222;
    time = 0;
  }
  else{
    std::cout << "Please specify year\n";
    exit;
  }
  std::cout << "storetime =" << storeTime << "\n";
  std::cout << "date,time =" << date <<" "<< time << "\n";
  
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("St_base.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");

  // max index dimensions
  const Int_t MAXINDEX = 12;
  int n=0;

  // structure to fill up
  fmsChannelGeometry_st g[MAXINDEX];
  memset(g,0,sizeof(g));

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
  //  if done inside your FmsDbMaker. Simply use DB = GetInputDb("Geometry/fms")
  DB = dbMk->GetInputDB("Geometry/fms/");
  if (!DB) { std::cout << "ERROR: no db maker?" << std::endl; }
  
  if(option.Contains("readdb")){
    gSystem->Unsetenv("DB_ACCESS_MODE");
    gSystem->Unsetenv("DB_SERVER_LOCAL_CONFIG");
    St_fmsChannelGeometry *db = 0;
    db = (St_fmsChannelGeometry*) DB->Find("fmsChannelGeometry");
    // fetch data and place it to appropriate structure
    if (db) {
      std::cout << "Reading fmsMap table\n";
      fmsChannelGeometry_st *table = db->GetTable();
      n = db->GetNRows();
      memcpy(g,table,sizeof(fmsChannelGeometry_st)*n);
    } else {
      std::cout << "WARNING: No data in fmsChannelGeometry table (wrong timestamp?). Nothing to return, then.\n";
    }
  }
 
 if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    cout << "DB_ACCESS_MODE="<<gSystem->Getenv("DB_ACCESS_MODE")<<endl;
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Geometry_fms");
    StDbTable* wtable = node->addDbTable("fmsChannelGeometry");	  
    mgr->setStoreTime(storeTime.Data());

    // input data
    /* Detector Name   detectorId  ew  ns  type   nX   nY  */
    /* FPD-North            0       0   0   0      7    7  */
    /* FPD-South            1       0   1   0      7    7  */
    /* FPD-North-Pres       2       0   0   1      7    1  */
    /* FPD-South-Pres       3       0   1   1      7    1  */
    /* FPD-North-SMDV       4       0   0   2     48    1  */
    /* FPD-South-SMDV       5       0   1   2     48    1  */
    /* FPD-North-SMDH       6       0   0   3      1   48  */
    /* FPD-South-SMDH       7       0   1   3      1   48  */
    /* FMS-North-Large      8       1   0   4     17   34  */
    /* FMS-South-Large      9       1   1   4     17   34  */
    /* FMS-North-Small     10       1   0   0     12   24  */
    /* FMS-South-Small     11       1   1   0     12   24  */
    /* FHC-North           12       1   0   5      9   12  */
    /* FHC-South           13       1   1   5      9   12  */

    int n=0;
    g[n].detectorId = 0; g[n].type=0; g[n].ew=0; g[n].ns=0; g[n].nX= 7; g[n].nY= 7;    n++;
    g[n].detectorId = 1; g[n].type=0; g[n].ew=0; g[n].ns=1; g[n].nX= 7; g[n].nY= 7;    n++;
    g[n].detectorId = 2; g[n].type=1; g[n].ew=0; g[n].ns=0; g[n].nX= 7; g[n].nY= 1;    n++;
    g[n].detectorId = 3; g[n].type=1; g[n].ew=0; g[n].ns=1; g[n].nX= 7; g[n].nY= 1;    n++;
    if(year>=9){								       	   
      g[n].detectorId = 4; g[n].type=2; g[n].ew=0; g[n].ns=0; g[n].nX=48; g[n].nY= 1;  n++;
      g[n].detectorId = 5; g[n].type=2; g[n].ew=0; g[n].ns=1; g[n].nX=48; g[n].nY= 1;  n++;
      g[n].detectorId = 6; g[n].type=3; g[n].ew=0; g[n].ns=0; g[n].nX= 1; g[n].nY=48;  n++;
      g[n].detectorId = 7; g[n].type=3; g[n].ew=0; g[n].ns=1; g[n].nX= 1; g[n].nY=48;  n++;
    }										       	   
    g[n].detectorId = 8; g[n].type=4; g[n].ew=1; g[n].ns=0; g[n].nX=17; g[n].nY=34;    n++;
    g[n].detectorId = 9; g[n].type=4; g[n].ew=1; g[n].ns=1; g[n].nX=17; g[n].nY=34;    n++;
    g[n].detectorId =10; g[n].type=0; g[n].ew=1; g[n].ns=0; g[n].nX=12; g[n].nY=24;    n++;
    g[n].detectorId =11; g[n].type=0; g[n].ew=1; g[n].ns=1; g[n].nX=12; g[n].nY=24;    n++;
    
    // store data in the table
    wtable->SetTable((char*)&g,n);
    // store table in dBase
    mgr->storeDbTable(wtable); 
    gSystem->Unsetenv("DB_ACCESS_MODE");
    std::cout << "Done with database upload \n";
 }
 
 printf("n = %d\n",n);
 printf(" Id Typ  ew  ns  nX  nY\n");
 for(int i=0; i<n; i++){
   printf("%3d %3d %3d %3d %3d %3d\n",g[i].detectorId,g[i].type,g[i].ew,g[i].ns,g[i].nX,g[i].nY);
 }
}


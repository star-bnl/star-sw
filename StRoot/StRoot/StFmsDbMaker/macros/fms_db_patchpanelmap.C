#include <iostream.h> 
#include <fstream.h> 

void fms_db_patchpanelmap(char* opt = "readdb writetext") {

  // that's begin time for validity range for WRITING DB
  // your data will be available from storeTime till 
  // next entry or 2037 for last entry
  TString storeTime = "2007-11-09 00:00:01";

  // that's time for READING DB  /
  int date = 20141220;
  int time = 0;
 
  TString option(opt);
  std::cout << "Opt =" << opt << "\n";
  std::cout << "testinput = " << option.Contains("testinput")  << "\n";
  std::cout << "readtext  = " << option.Contains("readtext")  << "\n";
  std::cout << "readdb    = " << option.Contains("readdb")    << "\n";
  std::cout << "writedb   = " << option.Contains("writedb")   << "\n";
  std::cout << "writetext = " << option.Contains("writetext") << "\n";
  
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("St_base.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");

  // max index dimensions
  const Int_t MAX_PP_MAP = 4;	

  // structure to fill up
  fmsPatchPanelMap_st ppmap[MAX_PP_MAP];
  
  if(option.Contains("testinput")){
    // fill only first entry out of 548 for testing purposes:
    ppmap[0].ppPanel[0] = 2; // 1-2
    ppmap[0].ppColumn[0] = 7; // 1-20
    ppmap[0].ppRow[0] = 15; // 1-16
    
    ppmap[1].ppPanel[0] = 1; // 1-2
    ppmap[1].ppColumn[0] = 3; // 1-20
    ppmap[1].ppRow[0] = 7; // 1-16
    
    ppmap[2].ppPanel[0] = 2; // 1-2
    ppmap[2].ppColumn[0] = 20; // 1-20
    ppmap[2].ppRow[0] = 15; // 1-16
    
    ppmap[3].ppPanel[0] = 1; // 1-2
    ppmap[3].ppColumn[0] = 13; // 1-20
    ppmap[3].ppRow[0] = 9; // 1-16
  }
  
  int module[4] = {1,3,2,4};
  int maxch[4]  = {578,578,288,288};

  if(option.Contains("readtext")){
    char* file="qtmap2pp.txt";
    FILE* fp;
    int rmod,rch,pp,col,row;
    cout << "Reading "<<file<<"\n";
    if(fp=fopen(file,"r")){
      for(int mod=0; mod<4; mod++){
	for(int ch=0; ch<maxch[module[mod]-1]; ch++){
	  if(!feof(fp)) fscanf(fp,"%d %d %d %d %d",&rmod,&rch,&pp,&row,&col);
	  //printf("%5d %5d %5d %5d %5d\n",rmod,rch,pp,row,col);
	  if(rmod!=module[mod]) cout << "Error mod# ="<<rmod<<" "<<module[mod]<<"\n";
	  if(rch !=ch +1) cout << "Error ch#  ="<<rch <<" "<<ch+1<<"\n";
	  ppmap[rmod-1].ppPanel[ch]=pp; 
	  ppmap[rmod-1].ppRow[ch]=row; 
	  ppmap[rmod-1].ppColumn[ch]=col;
	}
      }
    }
    fclose(fp);
  }

  if(option.Contains("readdb")){
    gSystem->Unsetenv("DB_ACCESS_MODE");
    gSystem->Unsetenv("DB_SERVER_LOCAL_CONFIG");

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
    if (!DB) { std::cout << "ERROR: no db maker?" << std::endl; }

    // fetch ROOT descriptor of db table
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
  }

  if(option.Contains("writetext")){
    char* file="qtmap2pp.txt_dbout";
    FILE* fp;
    cout << "Writing "<<file<<"\n";
    if(fp=fopen(file,"w")){
      for(int mod=0; mod<4; mod++){
	for(int ch=0; ch<maxch[module[mod]-1]; ch++){
	  fprintf(fp,"%3d %3d %3d %3d %3d\n",module[mod],ch+1,
		  ppmap[module[mod]-1].ppPanel[ch],
		  ppmap[module[mod]-1].ppRow[ch],
		  ppmap[module[mod]-1].ppColumn[ch]);
	}
      }
    }
    fclose(fp);
  }

  if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    cout << "DB_ACCESS_MODE="<<gSystem->Getenv("DB_ACCESS_MODE")<<endl;
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_fms");
    StDbTable* table = node->addDbTable("fmsPatchPanelMap");	  
    // set store time
    mgr->setStoreTime(storeTime.Data());
    // store data in the table
    table->SetTable((char*)&ppmap,MAX_PP_MAP);
    // store table in dBase
    mgr->storeDbTable(table); 
    gSystem->Unsetenv("DB_ACCESS_MODE");
    std::cout << "Done with database upload \n";
  }
}


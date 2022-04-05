#include <iostream.h> 
#include <fstream.h> 

void fms_db_map(char* opt = "readdb") {

  // that's begin time for validity range for WRITING TO DB 
  // your data will be available from storeTime till 2037 
  TString storeTime = "2007-11-09 00:00:00";
  
  // this is data and time for READING FROM DB
  int readDate = 20141220;
  //int readDate = 20140222;
  int readTime = 0;

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
  const Int_t MAX = 2000;	

  // structure to fill up
  fmsMap_st ppmap[MAX];
  
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
	  ppmap[mod].ppPanel[ch]=pp; 
	  ppmap[mod].ppRow[ch]=row; 
	  ppmap[mod].ppColumn[ch]=col;
	}
      }
    }
    fclose(fp);
  }

  if(option.Contains("readdb")){
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
    dbMk->SetDateTime(readDate,readTime);
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
      for(int mod=0; mod<4; mod++){
	for(int ch=0; ch<maxch[module[mod]-1]; ch++){
          printf("%3d %3d %3d %3d %3d\n",module[mod],ch+1,
		 int(pptable[mod].ppPanel[ch]),
		 int(pptable[mod].ppRow[ch]),
		 int(pptable[mod].ppColumn[ch]) );
        } 
      }
      memcpy(ppmap,pptable,sizeof(ppmap));
    } else {
      std::cout << "WARNING: No data in fmsPatchPanelMap table (wrong timestamp?). Nothing to return, then.\n";
    }
  }

  if(option.Contains("writetext")){
    char* file="qtmap2pp_out.txt";
    FILE* fp;
    cout << "Writing "<<file<<"\n";
    if(fp=fopen(file,"w")){
      for(int mod=0; mod<4; mod++){
	for(int ch=0; ch<maxch[module[mod]-1]; ch++){
	  fprintf(fp,"%3d %3d %3d %3d %3d\n",module[mod],ch+1,
		  ppmap[mod].ppPanel[ch],
		  ppmap[mod].ppRow[ch],
		  ppmap[mod].ppColumn[ch]);
	}
      }
    }
    fclose(fp);
  }

  if(option.Contains("writedb")) {
    //putenv("DB_ACCESS_MODE=write");
    //char* env = getenv("DB_ACCESS_MODE");
    //cout <<  "Setting DB_ACCESS_MODE " << env << endl;
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_fms");
    StDbTable* table = node->addDbTable("fmsPatchPanelMap");	  
    mgr->setStoreTime(storeTime.Data());
    // store data in the table
    table->SetTable((char*)&ppmap,MAX_PP_MAP);
    // set store time
    // store table in dBase
    mgr->storeDbTable(table); 
    //unsetenv("DB_ACCESS_MODE");
    std::cout << "Done with database upload \n";
  }
}


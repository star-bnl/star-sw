#include <iostream.h> 
#include <fstream.h> 

void fms_db_qtmap(char* opt = "readdb writetext",int year = 15) {

  TString option(opt);
  std::cout << "Opt =" << opt << "\n";
  std::cout << "testinput = " << option.Contains("testinput") << "\n";
  std::cout << "readtext  = " << option.Contains("readtext")  << "\n";
  std::cout << "readdb    = " << option.Contains("readdb")    << "\n";
  std::cout << "writedb   = " << option.Contains("writedb")   << "\n";
  std::cout << "writetext = " << option.Contains("writetext") << "\n";
  std::cout << "\n";  

  TString filename;  // file name for READING TEXT FILE
  TString storeTime; // storetime is begin time for validity range for WRITING DB
  int date,time;     // time for READING DB
  std::cout << "year = " << year << "\n";
  if(year==8){
    filename="qtmap_run8.txt";
    storeTime="2007-11-09 00:00:00";
    date = 20080301;
    time = 0;
  }else if(year==9){
    filename="qtmap2009V1.txt";
    storeTime="2008-11-09 00:00:00";
    date = 20090301;
    time = 0;
  }else if(year==15){
    filename="qtmap2009V1.txt";
    storeTime="2014-12-20 00:00:00";
    //date = 20140222;
    date = 20141220;
    time = 0;
  }
  else{
    std::cout << "Please specify year\n";
    exit;
  }
  std::cout << "filename  =" << filename << "\n";
  std::cout << "storetime =" << storeTime << "\n";
  std::cout << "date,time =" << date <<" "<< time << "\n";
  
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("St_base.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");

  // max index dimensions
  const Int_t MAX_QT_MAP = 2;

  // structure to fill up
  fmsQTMap_st qtmap[MAX_QT_MAP];
  
  if(option.Contains("testinput")){
    // fill only first entry out of 548 for testing purposes:
    qtmap[0].qtCrate[0][0][0] = 15;
    qtmap[0].qtSlot[0][0][0] = 15;
    qtmap[0].qtChannel[0][0][0] = 15;
    
    qtmap[1].qtCrate[0][0][0] = 17;
    qtmap[1].qtSlot[0][0][0] = 17;
    qtmap[1].qtChannel[0][0][0] = 17;
  }
  
  if(option.Contains("readtext")){
    char* file=filename.Data();
    FILE* fp;
    int rns,rpp,rrow,rcol,rqt,rcrt,rch;
    cout << "Reading "<<file<<"\n";
    if(fp=fopen(file,"r")){
      for(int ns=0; ns<2; ns++){
	for(int pp=0; pp<2; pp++){
	  for(int row=0; row<20; row++){
	    for(int col=0; col<16; col++){
	      if(!feof(fp)) fscanf(fp,"%d %d %d %d %d %d %d",&rns,&rpp,&rrow,&rcol,&rqt,&rcrt,&rch);
	      //printf("%d %d %d %d %d %d %d\n",rns,rpp,rrow,rcol,rqt,rcrt,rch);
	      if(rns == -1 && rpp == -1 && rrow == -1 && rcol == -1){
		qtmap[ns].qtCrate[pp][row][col]   = 0;
		qtmap[ns].qtSlot[pp][row][col]    = 0;
		qtmap[ns].qtChannel[pp][row][col] = 0;
	      }else{
		if(rns !=ns +1) cout << "Error ns#  ="<<rns <<" "<<ns +1<<"\n";
		if(rpp !=pp +1) cout << "Error pp#  ="<<rpp <<" "<<pp +1<<"\n";
		if(rrow!=row+1) cout << "Error row# ="<<rrow<<" "<<row+1<<"\n";
		if(rcol!=col+1) cout << "Error col# ="<<rcol<<" "<<col+1<<"\n";
		qtmap[ns].qtCrate[pp][row][col]   = rqt;
		qtmap[ns].qtSlot[pp][row][col]    = rcrt;
		qtmap[ns].qtChannel[pp][row][col] = rch;
	      }
	    }
	  }
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
    St_fmsQTMap *dbmap = 0;
    dbmap = (St_fmsQTMap*) DB->Find("fmsQTMap");
    // fetch data and place it to appropriate structure
    if (dbmap){
      std::cout << "Reading fmsQTMap table\n";
      fmsQTMap_st *qttable = dbmap->GetTable();
      memcpy(qtmap,qttable,sizeof(qtmap));
    } else {
      std::cout << "WARNING: No data in fmsQTMap table (wrong timestamp?). Nothing to return, then.\n";
    }
  }

  if(option.Contains("writetext")){
    filename+="_dbout";
    char* file=filename.Data();
    FILE* fp;
    int crt,slt,ch;
    cout << "Writing "<<file<<"\n";
    if(fp=fopen(file,"w")){
      for(int ns=0; ns<2; ns++){
        for(int pp=0; pp<2; pp++){
          for(int row=0; row<20; row++){
            for(int col=0; col<16; col++){
	      if(qtmap[ns].qtCrate[pp][row][col] ==0 &&
		 qtmap[ns].qtSlot[pp][row][col] ==0 &&
		 qtmap[ns].qtChannel[pp][row][col] ==0 ){
		if(year==8) fprintf(fp,"%3d %2d %3d %3d %3d %3d %3d\n",-1,-1,-1,-1,-1,-1,-1);
		if(year==9) fprintf(fp,"%3d %3d %4d %4d %4d %4d %4d\n",-1,-1,-1,-1,-1,-1,-1);
	      }else{
		if(year==8) fprintf(fp,"%3d %2d %3d %3d %3d %3d %3d\n",ns+1,pp+1,row+1,col+1,
				    qtmap[ns].qtCrate[pp][row][col],
				    qtmap[ns].qtSlot[pp][row][col],
				    qtmap[ns].qtChannel[pp][row][col]);
		if(year==9||year==15) fprintf(fp,"%3d %3d %4d %4d %4d %4d %4d\n",ns+1,pp+1,row+1,col+1,
				    qtmap[ns].qtCrate[pp][row][col],
				    qtmap[ns].qtSlot[pp][row][col],
				    qtmap[ns].qtChannel[pp][row][col]);
	      }
            }
          }
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
    StDbTable* table = node->addDbTable("fmsQTMap");	  
    mgr->setStoreTime(storeTime.Data());
    // store data in the table
    table->SetTable((char*)&qtmap,MAX_QT_MAP);
    // set store time
    // store table in dBase
    mgr->storeDbTable(table); 
    gSystem->Unsetenv("DB_ACCESS_MODE");
    std::cout << "Done with database upload \n";
  }
}


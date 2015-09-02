#include <iostream.h> 
#include <fstream.h>

void fms_db_detectorposition(char* opt = "readdb", char* dataspec) {
  // that's begin time for validity range for WRITING TO DB 
  // your data will be available from storeTime till 2037
  TString data(dataspec);
  if(data.Contains("run8") && data.Contains("dAu200"))
    TString storeTime = "2007-11-09 12:00:00";
  else
    if(data.Contains("run8") && data.Contains("pp200"))
      TString storeTime = "2008-01-28 12:00:00";
    else
      if(data.Contains("run9") && data.Contains("pp200"))  
	TString storeTime = "2009-01-16 00:00:00";
      else std::cout<<"Invalid year range"<<std::endl;

  TString storeTime = "2014-12-20 00:00:00";
  // this is data and time for READING FROM DB
  //int readDate = 20140202;
  int readDate = 20141220;
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
   const Int_t MAX = 6;

  // structure to fill up
  fmsDetectorPosition_st detposition[MAX];

  if(option.Contains("testinput")){
    // fill only first entry for testing purposes:
    detposition[0].detectorId = 2;
    detposition[0].xwidth = 7;
    detposition[0].ywidth = .15;
    detposition[1].detectorId = 2;
    detposition[1].xwidth = 7;
    detposition[1].ywidth = .15;
  }


  if(option.Contains("readtext")){
    FILE* fp;
    int rdetid;
    float rzoffset, rxoffset, ryoffset, rxwidth, rywidth;
    cout << "Reading "<<dataspec<<"\n";
    if(fp=fopen(dataspec,"r")){
      for(int i=0; i<MAX; i++){
	    if(!feof(fp)) fscanf(fp,"%d %f %f %f %f %f",&rdetid,&rzoffset, &rxoffset, &ryoffset, &rxwidth, &rywidth);
	    detposition[i].detectorId=rdetid;
	    detposition[i].xwidth=rxwidth;
	    detposition[i].ywidth=rywidth;
	    detposition[i].xoffset=rxoffset;
	    detposition[i].yoffset=ryoffset;
	    detposition[i].zoffset=rzoffset;
	    //cout << rdetid<<" "<<rzoffset<<" "<<rxoffset<<" "<<ryoffset<<" "<<rxwidth<<" "<<rywidth<<endl;
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
    DB = dbMk->GetInputDB("Geometry/fms");
    if (!DB) { std::cout << "ERROR: no db maker?" << std::endl; }

    // fetch ROOT descriptor of db table
    St_fmsDetectorPosition *dbppmap = 0;
    dbppmap = (St_fmsDetectorPosition*) DB->Find("fmsDetectorPosition");
    // fetch data and place it to appropriate structure
    if (dbppmap) {
      std::cout << "Reading fmsDetectorPosition table\n";
      fmsDetectorPosition_st *pptable = dbppmap->GetTable();
      for(int i=0; i<MAX; i++)
	printf("%5d%8.1f%6.1f%5.1f%6.3f%6.3f\n",pptable[i].detectorId,pptable[i].zoffset,pptable[i].xoffset,
	       pptable[i].yoffset,pptable[i].xwidth,pptable[i].ywidth);
      memcpy(detposition,pptable,sizeof(detposition));
    }
    else
      std::cout << "WARNING: No data in fmsDetposition table (wrong timestamp?). Nothing to return, then.\n";
  }


  if(option.Contains("writetext")){
    int ii = data.Length();
    data.Insert(ii-4,"_out");
    char* file=data.Data();
    FILE* fp;
    cout << "Writing "<<file<<"\n";
    if(fp=fopen(file,"w"))
      for(int i=0; i<MAX; i++){
	fprintf(fp,"%8d%10.1f%10.2f%8.1f%10.3f%10.3f\n", detposition[i].detectorId, detposition[i].zoffset, detposition[i].xoffset,
	       detposition[i].yoffset, detposition[i].xwidth, detposition[i].ywidth);
    }
    fclose(fp);
  }

  if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    //putenv("DB_ACCESS_MODE=write");
    //char* env = getenv("DB_ACCESS_MODE");
    //cout <<  "Setting DB_ACCESS_MODE " << env << endl;
    cout << "DB_ACCESS_MODE="<<gSystem->Getenv("DB_ACCESS_MODE")<<endl;
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Geometry_fms");
    StDbTable* table = node->addDbTable("fmsDetectorPosition");
    mgr->setStoreTime(storeTime.Data());
    // store data in the table
    table->SetTable((char*)&detposition,MAX);
    // set store time
    // store table in dBase
    mgr->storeDbTable(table);
    //StDbTable* table = node->findTable("fmsDetposition");
    //node->removeTable(table);
    gSystem->Unsetenv("DB_ACCESS_MODE");
    //unsetenv("DB_ACCESS_MODE");
    std::cout << "Done with database upload \n";
  }
}


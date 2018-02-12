#include <iostream.h> 
#include <fstream.h>

// max index dimensions
static const Int_t MAX = 2500;
static const Int_t MAXD = 14;
static const Int_t MAXCH= 578;

int getDetectorId(int ew, int nstb) {return (ew-1)*8+nstb-1;}
int getEW(int detid) {return detid/8 + 1;}
int getNSTB(int detid) {return detid%8 + 1;}

void fms_db_fmsgainB(char* option = "readtext writetext 15sim", char* dataspec="FmsPtGainR16.txt") {
  // that's begin time for validity range for WRITING TO DB 
  // your data will be available from storeTime till 2037    
  TString opt(option);
  TString data(dataspec);
  int readDatte=20071210, readTime=0, isSim=0;
  if(opt.Contains("run8") && opt.Contains("dAu200")){
      TString storeTime = "2007-11-09 12:00:00";
  }else if(opt.Contains("run8") && opt.Contains("pp200")){
      TString storeTime = "2008-01-28 12:00:00";
  }else if(opt.Contains("run9") && opt.Contains("pp200")){
      TString storeTime = "2009-01-16 00:00:00";
  }else if(opt.Contains("15sim")){
      TString storeTime = "2014-12-10 00:00:01"; readDate = 20141215; readTime = 0; isSim=1;
  }else if(opt.Contains("15ofl")){
      TString storeTime = "2014-12-20 00:00:00"; readDate = 20141225; readTime = 0;
  }else if(opt.Contains("17sim")){
      TString storeTime = "2016-12-10 00:00:01"; readDate = 20161215; readTime = 0; isSim=1;
  }else if(opt.Contains("17ofl")){
      TString storeTime = "2016-12-20 00:00:00"; readDate = 20161225; readTime = 0;
  }      
  else std::cout<<"Invalid year range"<<std::endl;
  
  std::cout << "Opt =" << option << "\n";
  std::cout << "testinput = " << opt.Contains("testinput")  << "\n";
  std::cout << "readtext  = " << opt.Contains("readtext")  << "\n";
  std::cout << "readdb    = " << opt.Contains("readdb")    << "\n";
  std::cout << "writedb   = " << opt.Contains("writedb")   << "\n";
  std::cout << "writetext = " << opt.Contains("writetext") << "\n";
  
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("St_base.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");

  // structure to fill up
  fmsGainB_st gain;
  memset(&gain,0,sizeof(gain));
  
  if(opt.Contains("readtext")){
      FILE* fp;
      int rew,rnstb,rch;
      float rgain;
      int nread = 0;
      cout << "Reading "<<dataspec<<"\n";
      if(fp=fopen(dataspec,"r")){
	  while(fscanf(fp,"%d %d %d %f",&rew,&rnstb,&rch,&rgain) != EOF){
	      int detid=getDetectorId(rew,rnstb);
	      gain.detectorId[nread]=detid;
	      gain.ch[nread]        =rch;
	      gain.gain[nread]      =rgain;
	      //printf("%4d %2d %3d %8.4f\n",nread,detid,rch,rgain);
	      nread++;
	  }
      }
      fclose(fp);
      printf("read %d channels of gain\n",nread);
  }
  
  if(opt.Contains("writedb")) {
      gSystem->Setenv("DB_ACCESS_MODE","write");
      //putenv("DB_ACCESS_MODE=write");
      //char* env = getenv("DB_ACCESS_MODE");
      //cout <<  "Setting DB_ACCESS_MODE " << env << endl;
      StDbManager* mgr = StDbManager::Instance();
      StDbConfigNode* node = mgr->initConfig("Calibrations_fms");
      StDbTable* table = node->addDbTable("fmsGainB");
      mgr->setStoreTime(storeTime.Data());
      // store data in the table
      table->SetTable((char*)&gain,1);
      // set store time
      // store table in dBase
      if(isSim) table->setFlavor("sim");
      mgr->storeDbTable(table);
      //StDbTable* table = node->findTable("fmsGain");
      //node->removeTable(table);
      gSystem->Unsetenv("DB_ACCESS_MODE");
      //unsetenv("DB_ACCESS_MODE");
      std::cout << "Done with database upload \n";
  }
  
  if(opt.Contains("readdb")){
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
      if(isSim) dbMk->SetFlavor("sim"); // for simulations
      dbMk->Init();
      dbMk->Make();
      
      // this is done inside ::Make method
      TDataSet *DB = 0;
      // "dbMk->" will NOT be needed. 
      //  if done inside your FmsDbMaker. Simply use DB = GetInputDb("Calibrations/fms/mapping")
      DB = dbMk->GetInputDB("Calibrations/fms");
      if (!DB) { std::cout << "ERROR: no db maker?" << std::endl; }

      // fetch ROOT descriptor of db table
      St_fmsGainB *dbppmap = 0;
      dbppmap = (St_fmsGainB*) DB->Find("fmsGainB");
      // fetch data and place it to appropriate structure
      int ngain = 0;
      if (dbppmap) {
	  std::cout << "Reading fmsGainB table\n";
	  fmsGainB_st *pptable = dbppmap->GetTable();
	  for(int i=0; i<MAX; i++){
	      if(pptable.ch[i]>0){
		  printf("%3d%8d%8d%8.4f\n",
			 int(pptable.detectorId[i]/8+1),int(pptable.detectorId[i]%8+1),
			 int(pptable.ch[i]),float(pptable.gain[i]) );
		  ngain++;
	      }
	  }
	  memcpy(gain,pptable,sizeof(gain));
      }else {
	  std::cout << "WARNING: No data in fmsGain table (wrong timestamp?). Nothing to return, then.\n";
      }
  }
  
  if(opt.Contains("writetext")){
      int ii = data.Length();
      data.Insert(ii-4,"_out");
      char* file=data.Data();
      FILE* fp;
      cout << "Writing "<<file<<"\n";
      int ngain = 0;
      if(fp=fopen(file,"w")){
	  for(int i=0; i<MAX; i++){
	      if(gain.ch[i]>0){
		  int detid=gain.detectorId[i];
		  int iew=int(gain.detectorId[i]/8+1);
		  int instb=int(gain.detectorId[i]%8+1);
		      fprintf(fp,"%d %d %d %5.3f\n",iew,instb,
			      gain.ch[i],gain.gain[i]);
		  ngain++;
	      }
	  }
      }
      fclose(fp);
      printf("Write %d channels\n",ngain);
  }
}


